#!/usr/bin/env Rscript
# =============================================================================
# 02_classify_cookies.R
#
# Stream the parsed tracker shards, preserve cookie source, recover valid
# host-only Set-Cookie domains, classify first- versus third-party cookies by
# registrable domain, and write date-partitioned FST files for 03_build_panel.R.
#
# Important definitions
#   source == "request": chrome.cookies.getAll() cookie-stock snapshot rows.
#   source == "set"    : response Set-Cookie action rows.
#   source == "none"   : a captured tracker record with no parsed cookie row.
#
# This script never mixes those sources. It keeps source=="none" so the next
# stage can distinguish a captured zero-cookie tracker record from an absent
# tracker measurement.
#
# INPUT
#   ../data/processed_data/parsed_trackers/{06_22,07_03,09_16}/parsed_*.csv
#   ../data/final_extension_data/trackers.csv
#
# OUTPUT (atomically replaced only after a complete successful build)
#   ../data/processed_data/cookies_classified_by_date/YYYY-MM-DD/part_*.fst
#   ../data/processed_data/cookies_classified_by_date/construction_audit.csv
#   ../data/processed_data/cookies_classified_by_date/manifest.csv
# =============================================================================

setwd("~/Dropbox/spring2025experiment/code_github")

suppressMessages({
  library(data.table)
  library(fst)
})

setDTthreads(4L)
options(datatable.verbose = FALSE)
options(scipen = 999)

source("replication_files/utils/time_usage_helpers.R")


# =============================================================================
# 0. Paths and configuration
# =============================================================================

PARSED_DIRS <- file.path(
  "../data/processed_data/parsed_trackers",
  c("06_22", "07_03", "09_16")
)
TRACKERS_PATH <- "../data/final_extension_data/trackers.csv"

OUT_DIR <- "../data/processed_data/cookies_classified_by_date"
STAGING_DIR <- paste0(OUT_DIR, "_building")
BACKUP_DIR <- paste0(OUT_DIR, "_previous")

PARSED_PATTERN <- "parsed_.*[.]csv$"
BUFFER_SHARDS <- 2L
READ_THREADS <- 4L

NEEDED_PARSED_COLUMNS <- c(
  "experiment_id", "tracker_id", "tstamp", "source",
  "domain", "name", "path", "storeId"
)

pr <- function(...) cat(sprintf(...), "\n")
comma <- function(x) format(x, big.mark = ",", scientific = FALSE)

normalize_host <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x == ""] <- NA_character_
  x <- sub("^[a-z][a-z0-9+.-]*://", "", x)
  x <- sub("[/?#].*$", "", x)
  x <- sub(":([0-9]+)$", "", x)
  x <- sub("^\\.+", "", x)
  x <- sub("\\.+$", "", x)
  x[x == ""] <- NA_character_
  x
}

registrable_lookup <- function(hosts) {
  normalized <- normalize_host(hosts)
  unique_hosts <- unique(normalized[!is.na(normalized)])
  if (length(unique_hosts) == 0L) {
    return(rep(NA_character_, length(normalized)))
  }
  lookup <- data.table(
    host = unique_hosts,
    registrable_domain = clean_site(unique_hosts)
  )
  lookup$registrable_domain[match(normalized, lookup$host)]
}


# =============================================================================
# 1. Resolve inputs and tracker metadata
# =============================================================================

missing_dirs <- PARSED_DIRS[!dir.exists(PARSED_DIRS)]
if (length(missing_dirs) > 0L) {
  stop("Missing parsed directories:\n", paste(missing_dirs, collapse = "\n"))
}
if (!file.exists(TRACKERS_PATH)) stop("Missing: ", TRACKERS_PATH)

parsed_files <- sort(unique(unlist(lapply(PARSED_DIRS, function(path) {
  list.files(path, pattern = PARSED_PATTERN, full.names = TRUE)
}), use.names = FALSE)))
if (length(parsed_files) == 0L) {
  stop("No parsed files matched: ", PARSED_PATTERN)
}

tracker_columns <- c("id", "domain", "third_party_domain")
tracker_header <- names(fread(
  TRACKERS_PATH,
  nrows = 0L,
  showProgress = FALSE
))
missing_tracker_columns <- setdiff(tracker_columns, tracker_header)
if (length(missing_tracker_columns) > 0L) {
  stop(
    "trackers.csv is missing: ",
    paste(missing_tracker_columns, collapse = ", ")
  )
}

pr("Reading tracker metadata...")
tracker_map <- fread(
  TRACKERS_PATH,
  select = tracker_columns,
  colClasses = list(character = tracker_columns),
  nThread = READ_THREADS,
  showProgress = FALSE
)
setnames(
  tracker_map,
  c("id", "domain", "third_party_domain"),
  c("tracker_id", "website", "request_host")
)
if (anyDuplicated(tracker_map$tracker_id)) {
  stop("trackers.csv contains duplicated tracker_id values")
}
tracker_map[, website := normalize_host(website)]
tracker_map[, request_host := normalize_host(request_host)]

tracker_ids <- tracker_map$tracker_id
tracker_websites <- tracker_map$website
tracker_request_hosts <- tracker_map$request_host


# =============================================================================
# 2. Prepare staging output
# =============================================================================

# Recover the last successful output if a previous run was interrupted between
# moving OUT_DIR aside and promoting the completed staging directory.
if (dir.exists(BACKUP_DIR) && !dir.exists(OUT_DIR)) {
  if (!file.rename(BACKUP_DIR, OUT_DIR)) {
    stop("Could not restore the previous successful output: ", BACKUP_DIR)
  }
}
if (dir.exists(BACKUP_DIR) && dir.exists(OUT_DIR)) {
  unlink(BACKUP_DIR, recursive = TRUE, force = TRUE)
}

# STAGING_DIR is a task-specific generated directory. Removing it is safe: it
# can contain only an incomplete earlier build of this script.
if (dir.exists(STAGING_DIR)) {
  unlink(STAGING_DIR, recursive = TRUE, force = TRUE)
}
dir.create(STAGING_DIR, recursive = TRUE, showWarnings = FALSE)

classified_buffer <- vector("list", BUFFER_SHARDS)
buffer_count <- 0L
part_index <- 0L
manifest_parts <- list()
audit_parts <- vector("list", length(parsed_files))

output_columns <- c(
  "experiment_id", "tracker_id", "tstamp", "date", "website",
  "source", "is_cookie_row", "cookie_domain", "cookie_name",
  "cookie_path", "store_id", "domain_was_imputed", "is_third_party"
)

source_record_identity_key <- c(
  "experiment_id", "tracker_id", "tstamp", "website", "source",
  "cookie_domain", "cookie_name", "cookie_path", "store_id"
)

flush_buffer <- function() {
  if (buffer_count == 0L) return(invisible(NULL))
  
  part_index <<- part_index + 1L
  combined <- rbindlist(
    classified_buffer[seq_len(buffer_count)],
    use.names = TRUE,
    fill = TRUE
  )
  
  rows_before <- nrow(combined)
  combined <- unique(combined, by = source_record_identity_key)
  rows_after <- nrow(combined)
  
  date_values <- sort(unique(combined$date))
  if (length(date_values) == 0L || anyNA(date_values)) {
    stop("A classified buffer contains missing dates")
  }
  
  current_manifest <- vector("list", length(date_values))
  for (date_index in seq_along(date_values)) {
    current_date <- date_values[date_index]
    date_string <- format(current_date, "%Y-%m-%d")
    date_dir <- file.path(STAGING_DIR, date_string)
    dir.create(date_dir, recursive = TRUE, showWarnings = FALSE)
    
    part_path <- file.path(
      date_dir,
      sprintf("part_%04d.fst", part_index)
    )
    current_part <- combined[date == current_date, ..output_columns]
    write_fst(current_part, part_path, compress = 50)
    
    current_manifest[[date_index]] <- data.table(
      date = current_date,
      part = basename(part_path),
      rows = nrow(current_part)
    )
  }
  
  manifest_parts[[part_index]] <<- rbindlist(current_manifest)
  pr(
    "Flushed part %d: %s rows -> %s after source-aware record dedup",
    part_index,
    comma(rows_before),
    comma(rows_after)
  )
  
  classified_buffer <<- vector("list", BUFFER_SHARDS)
  buffer_count <<- 0L
  rm(combined, current_manifest)
  invisible(gc(full = TRUE))
}


# =============================================================================
# 3. Stream, classify, and partition parsed shards
# =============================================================================

build_start <- Sys.time()

for (file_index in seq_along(parsed_files)) {
  current_file <- parsed_files[file_index]
  current_header <- names(fread(
    current_file,
    nrows = 0L,
    showProgress = FALSE
  ))
  missing_columns <- setdiff(NEEDED_PARSED_COLUMNS, current_header)
  if (length(missing_columns) > 0L) {
    stop(
      "Parsed file is missing required columns: ", current_file,
      "\nMissing: ", paste(missing_columns, collapse = ", ")
    )
  }
  
  current <- fread(
    current_file,
    select = NEEDED_PARSED_COLUMNS,
    colClasses = list(character = NEEDED_PARSED_COLUMNS),
    nThread = READ_THREADS,
    showProgress = FALSE
  )
  parsed_rows <- nrow(current)
  
  unexpected_sources <- setdiff(
    unique(current$source[!is.na(current$source)]),
    c("none", "request", "set")
  )
  if (length(unexpected_sources) > 0L || anyNA(current$source)) {
    stop(
      "Unexpected or missing source in ", current_file, ": ",
      paste(unexpected_sources, collapse = ", ")
    )
  }
  
  tracker_position <- match(current$tracker_id, tracker_ids)
  if (anyNA(tracker_position)) {
    stop(
      "Parsed rows without tracker metadata in ", current_file, ": ",
      sum(is.na(tracker_position))
    )
  }
  current[, website := tracker_websites[tracker_position]]
  current[, request_host := tracker_request_hosts[tracker_position]]
  if (anyNA(current$website) || any(current$website == "")) {
    stop("Missing website mapping in ", current_file)
  }
  
  current[, is_cookie_row := source %chin% c("request", "set")]
  current[, domain_was_imputed :=
            source == "set" & (is.na(domain) | trimws(domain) == "")]
  if (current[domain_was_imputed == TRUE, anyNA(request_host)]) {
    stop("A missing-Domain Set-Cookie row lacks request_host in ", current_file)
  }
  current[domain_was_imputed == TRUE, domain := request_host]
  
  current[, cookie_domain := normalize_host(domain)]
  current[, cookie_name := fifelse(is.na(name), "", as.character(name))]
  current[, cookie_path := fifelse(is.na(path), "", as.character(path))]
  current[, store_id := fifelse(is.na(storeId), "", as.character(storeId))]
  
  current[, tstamp := suppressWarnings(as.numeric(tstamp))]
  current[, date := as.Date(
    as.POSIXct(
      tstamp,
      origin = "1970-01-01",
      tz = "America/Los_Angeles"
    ),
    tz = "America/Los_Angeles"
  )]
  if (anyNA(current$date)) {
    stop("Missing/unparseable tstamp rows in ", current_file, ": ", sum(is.na(current$date)))
  }
  
  all_hosts <- c(current$cookie_domain, current$website)
  all_regs <- registrable_lookup(all_hosts)
  n_current <- nrow(current)
  current[, cookie_reg := all_regs[seq_len(n_current)]]
  current[, website_reg := all_regs[n_current + seq_len(n_current)]]
  rm(all_hosts, all_regs, n_current)
  
  current[, is_third_party := as.logical(NA)]
  current[is_cookie_row == TRUE, is_third_party := cookie_reg != website_reg]
  unresolved_cookie_rows <- current[
    is_cookie_row == TRUE & is.na(is_third_party),
    .N
  ]
  # Conservative rule retained from the corrected v2 construction: malformed
  # cookie or website domains are kept and treated as third party.
  current[is_cookie_row == TRUE & is.na(is_third_party), is_third_party := TRUE]
  
  # source==none is a record marker, not a cookie. Force its cookie fields to
  # neutral values and leave its party classification missing.
  current[source == "none", `:=`(
    cookie_domain = NA_character_,
    cookie_name = "",
    cookie_path = "",
    store_id = "",
    domain_was_imputed = FALSE,
    cookie_reg = NA_character_,
    is_third_party = NA
  )]
  
  rows_before_shard_dedup <- nrow(current)
  current <- unique(current, by = source_record_identity_key)
  rows_after_shard_dedup <- nrow(current)
  
  audit_parts[[file_index]] <- data.table(
    pull_batch = basename(dirname(current_file)),
    parsed_file = basename(current_file),
    parsed_rows = parsed_rows,
    rows_after_shard_source_dedup = rows_after_shard_dedup,
    within_shard_duplicates_removed =
      rows_before_shard_dedup - rows_after_shard_dedup,
    none_rows = current[source == "none", .N],
    request_rows = current[source == "request", .N],
    set_rows = current[source == "set", .N],
    set_domain_rows_imputed = current[
      source == "set" & domain_was_imputed == TRUE,
      .N
    ],
    cookie_rows_missing_or_blank_name = current[
      is_cookie_row == TRUE & cookie_name == "",
      .N
    ],
    unresolved_cookie_rows_classified_third_party = unresolved_cookie_rows
  )
  
  buffer_count <- buffer_count + 1L
  classified_buffer[[buffer_count]] <- current[, ..output_columns]
  
  if (buffer_count == BUFFER_SHARDS) flush_buffer()
  
  if (
    file_index == 1L || file_index %% 5L == 0L ||
    file_index == length(parsed_files)
  ) {
    elapsed_minutes <- as.numeric(
      difftime(Sys.time(), build_start, units = "mins")
    )
    pr(
      "Shard %d/%d | elapsed %.1f min | input rows %s",
      file_index,
      length(parsed_files),
      elapsed_minutes,
      comma(parsed_rows)
    )
  }
  
  rm(current, tracker_position)
}

flush_buffer()


# =============================================================================
# 4. Audit, manifest, and atomic replacement
# =============================================================================

audit <- rbindlist(audit_parts, use.names = TRUE, fill = TRUE)
manifest <- rbindlist(manifest_parts, use.names = TRUE, fill = TRUE)
setorder(manifest, date, part)

fwrite(audit, file.path(STAGING_DIR, "construction_audit.csv"))
fwrite(manifest, file.path(STAGING_DIR, "manifest.csv"))

summary_table <- audit[, .(
  parsed_shards = .N,
  parsed_rows = sum(parsed_rows),
  rows_after_shard_source_dedup = sum(rows_after_shard_source_dedup),
  within_shard_duplicates_removed = sum(within_shard_duplicates_removed),
  none_rows = sum(none_rows),
  request_rows = sum(request_rows),
  set_rows = sum(set_rows),
  set_domain_rows_imputed = sum(set_domain_rows_imputed),
  cookie_rows_missing_or_blank_name =
    sum(cookie_rows_missing_or_blank_name),
  unresolved_cookie_rows_classified_third_party =
    sum(unresolved_cookie_rows_classified_third_party),
  date_part_files = nrow(manifest),
  dates = uniqueN(manifest$date)
)]
fwrite(summary_table, file.path(STAGING_DIR, "construction_summary.csv"))

if (dir.exists(BACKUP_DIR)) {
  unlink(BACKUP_DIR, recursive = TRUE, force = TRUE)
}
if (dir.exists(OUT_DIR) && !file.rename(OUT_DIR, BACKUP_DIR)) {
  stop("Could not temporarily move the previous output: ", OUT_DIR)
}
if (!file.rename(STAGING_DIR, OUT_DIR)) {
  if (dir.exists(BACKUP_DIR)) file.rename(BACKUP_DIR, OUT_DIR)
  stop("Could not move completed staging directory to: ", OUT_DIR)
}
if (dir.exists(BACKUP_DIR)) {
  unlink(BACKUP_DIR, recursive = TRUE, force = TRUE)
}

pr("=== 02 CLASSIFICATION COMPLETE ===")
print(summary_table)
pr("Wrote date-partitioned classified data to: %s", OUT_DIR)