#!/usr/bin/env Rscript
# =============================================================================
# 03_build_panel.R
#
# Build the canonical participant-website-day cookie panel from the
# date-partitioned output of 02_classify_cookies.R.
#
# The panel deliberately separates two objects:
#   1. request-only cookie-stock snapshots from chrome.cookies.getAll();
#   2. set-only distinct cookie-identity actions from response headers.
#
# It also retains tracker-record coverage. Therefore downstream code can tell
# the difference between:
#   - a captured tracker day with zero cookies of a source; and
#   - a browsing day with no captured tracker measurement after a later join.
#
# INPUT
#   ../data/processed_data/cookies_classified_by_date/YYYY-MM-DD/part_*.fst
#
# OUTPUT (atomically replaced)
#   ../data/processed_data/panel_cookies.fst
#   ../data/processed_data/panel_cookies_construction_audit.csv
#   ../data/processed_data/panel_cookies_construction_summary.csv
# =============================================================================

setwd("~/Dropbox/spring2025experiment/code_github")

suppressMessages({
  library(data.table)
  library(fst)
})

setDTthreads(4L)
options(datatable.verbose = FALSE)
options(scipen = 999)


# =============================================================================
# 0. Paths and schema
# =============================================================================

IN_DIR <- "../data/processed_data/cookies_classified_by_date"
OUT_FST <- "../data/processed_data/panel_cookies.fst"
OUT_FST_STAGING <- paste0(OUT_FST, ".building")
OUT_FST_BACKUP <- paste0(OUT_FST, ".previous")
OUT_AUDIT <- "../data/processed_data/panel_cookies_construction_audit.csv"
OUT_SUMMARY <- "../data/processed_data/panel_cookies_construction_summary.csv"

pr <- function(...) cat(sprintf(...), "\n")
comma <- function(x) format(x, big.mark = ",", scientific = FALSE)

required_columns <- c(
  "experiment_id", "tracker_id", "tstamp", "date", "website",
  "source", "is_cookie_row", "cookie_domain", "cookie_name",
  "cookie_path", "store_id", "domain_was_imputed", "is_third_party"
)

daily_key <- c("experiment_id", "website", "date")
record_key <- c(
  "experiment_id", "website", "date", "tracker_id", "tstamp"
)
source_record_identity_key <- c(
  "experiment_id", "tracker_id", "tstamp", "website", "source",
  "cookie_domain", "cookie_name", "cookie_path", "store_id"
)
snapshot_identity_key <- c(
  daily_key,
  "cookie_domain", "cookie_name", "cookie_path", "store_id"
)

panel_columns <- c(
  daily_key,
  "tracker_record_observed",
  "n_tracker_records",
  "n_tracker_records_with_request_cookie_rows",
  "n_tracker_records_with_set_cookie_actions",
  "n_tracker_records_with_no_cookie_rows",
  "unique_snapshot_cookies_3rd_p",
  "unique_snapshot_cookies_1st_p",
  "unique_snapshot_cookies_total",
  "set_cookie_actions_3rd_p",
  "set_cookie_actions_1st_p",
  "set_cookie_actions_total",
  "set_cookie_actions_imputed_domain"
)

zero_columns <- setdiff(
  panel_columns,
  c(daily_key, "tracker_record_observed", "n_tracker_records")
)


# =============================================================================
# 1. Resolve date partitions
# =============================================================================

if (!dir.exists(IN_DIR)) {
  stop(
    "Missing classified input directory: ", IN_DIR,
    "\nRun replication_files/data_construction/02_classify_cookies.R first."
  )
}

date_dirs <- list.dirs(IN_DIR, recursive = FALSE, full.names = TRUE)
date_dirs <- date_dirs[grepl(
  "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
  basename(date_dirs)
)]
date_dirs <- date_dirs[order(basename(date_dirs))]
if (length(date_dirs) == 0L) {
  stop("No YYYY-MM-DD partitions found under: ", IN_DIR)
}

# Recover the last successful panel if an earlier run was interrupted during
# the final atomic file swap.
if (file.exists(OUT_FST_BACKUP) && !file.exists(OUT_FST)) {
  if (!file.rename(OUT_FST_BACKUP, OUT_FST)) {
    stop("Could not restore the previous successful panel: ", OUT_FST_BACKUP)
  }
}
if (file.exists(OUT_FST_BACKUP) && file.exists(OUT_FST)) {
  file.remove(OUT_FST_BACKUP)
}


# =============================================================================
# 2. Build one daily panel partition at a time
# =============================================================================

daily_panels <- vector("list", length(date_dirs))
audit_parts <- vector("list", length(date_dirs))
build_start <- Sys.time()

for (date_index in seq_along(date_dirs)) {
  date_dir <- date_dirs[date_index]
  date_string <- basename(date_dir)
  part_files <- sort(list.files(
    date_dir,
    pattern = "[.]fst$",
    full.names = TRUE
  ))
  if (length(part_files) == 0L) {
    stop("Date partition has no FST parts: ", date_dir)
  }
  
  part_tables <- lapply(part_files, function(path) {
    current <- read_fst(path, as.data.table = TRUE)
    missing_columns <- setdiff(required_columns, names(current))
    if (length(missing_columns) > 0L) {
      stop(
        "Classified part is missing columns: ", path,
        "\nMissing: ", paste(missing_columns, collapse = ", ")
      )
    }
    current[, ..required_columns]
  })
  day_rows <- rbindlist(part_tables, use.names = TRUE, fill = TRUE)
  rm(part_tables)
  day_rows[, date := as.Date(date)]
  
  rows_before_global_dedup <- nrow(day_rows)
  day_rows <- unique(day_rows, by = source_record_identity_key)
  rows_after_global_dedup <- nrow(day_rows)
  
  observed_dates <- unique(day_rows$date)
  if (
    length(observed_dates) != 1L || is.na(observed_dates) ||
    format(observed_dates, "%Y-%m-%d") != date_string
  ) {
    stop("Date partition contents do not match directory: ", date_dir)
  }
  
  # One row per captured tracker record, including source==none records.
  record_sources <- day_rows[, .(
    has_request_cookie_rows = any(source == "request"),
    has_set_cookie_actions = any(source == "set"),
    has_none_marker = any(source == "none")
  ), by = record_key]
  
  record_panel <- record_sources[, .(
    n_tracker_records = .N,
    n_tracker_records_with_request_cookie_rows =
      sum(has_request_cookie_rows),
    n_tracker_records_with_set_cookie_actions =
      sum(has_set_cookie_actions),
    n_tracker_records_with_no_cookie_rows = sum(
      has_none_marker &
        !has_request_cookie_rows &
        !has_set_cookie_actions
    )
  ), by = daily_key]
  record_panel[, tracker_record_observed := 1L]
  
  # Daily request-only cookie identities. Repeated getAll snapshots of the
  # same browser cookie count once per participant-website-day.
  snapshot_rows <- day_rows[
    source == "request" &
      is_cookie_row == TRUE &
      cookie_name != "" &
      !is.na(is_third_party)
  ]
  snapshot_identities <- unique(
    snapshot_rows,
    by = snapshot_identity_key
  )
  snapshot_panel <- snapshot_identities[, .(
    unique_snapshot_cookies_3rd_p = sum(is_third_party == TRUE),
    unique_snapshot_cookies_1st_p = sum(is_third_party == FALSE)
  ), by = daily_key]
  snapshot_panel[, unique_snapshot_cookies_total :=
                   unique_snapshot_cookies_3rd_p + unique_snapshot_cookies_1st_p]
  
  # Each remaining source==set row is one distinct cookie identity set within
  # a captured tracker record after cross-pull deduplication. Cookie value is
  # deliberately excluded: updating a value does not create a new identity.
  set_rows <- day_rows[
    source == "set" &
      is_cookie_row == TRUE &
      cookie_name != "" &
      !is.na(is_third_party)
  ]
  set_panel <- set_rows[, .(
    set_cookie_actions_3rd_p = sum(is_third_party == TRUE),
    set_cookie_actions_1st_p = sum(is_third_party == FALSE),
    set_cookie_actions_imputed_domain = sum(domain_was_imputed == TRUE)
  ), by = daily_key]
  set_panel[, set_cookie_actions_total :=
              set_cookie_actions_3rd_p + set_cookie_actions_1st_p]
  
  current_panel <- merge(
    record_panel,
    snapshot_panel,
    by = daily_key,
    all.x = TRUE,
    sort = FALSE
  )
  current_panel <- merge(
    current_panel,
    set_panel,
    by = daily_key,
    all.x = TRUE,
    sort = FALSE
  )
  for (column in zero_columns) {
    set(current_panel, which(is.na(current_panel[[column]])), column, 0L)
  }
  
  setcolorder(current_panel, panel_columns)
  if (anyDuplicated(current_panel, by = daily_key)) {
    stop("Duplicate daily panel keys for date: ", date_string)
  }
  if (any(current_panel$n_tracker_records <= 0L)) {
    stop("Nonpositive n_tracker_records for date: ", date_string)
  }
  
  daily_panels[[date_index]] <- current_panel
  audit_parts[[date_index]] <- data.table(
    date = observed_dates,
    input_part_files = length(part_files),
    rows_before_global_source_dedup = rows_before_global_dedup,
    rows_after_global_source_dedup = rows_after_global_dedup,
    source_duplicate_rows_removed =
      rows_before_global_dedup - rows_after_global_dedup,
    tracker_records = nrow(record_sources),
    participant_website_day_cells = nrow(current_panel),
    unique_snapshot_cookies_3rd_p =
      sum(current_panel$unique_snapshot_cookies_3rd_p),
    unique_snapshot_cookies_1st_p =
      sum(current_panel$unique_snapshot_cookies_1st_p),
    set_cookie_actions_3rd_p =
      sum(current_panel$set_cookie_actions_3rd_p),
    set_cookie_actions_1st_p =
      sum(current_panel$set_cookie_actions_1st_p)
  )
  
  if (
    date_index == 1L || date_index %% 5L == 0L ||
    date_index == length(date_dirs)
  ) {
    elapsed_minutes <- as.numeric(
      difftime(Sys.time(), build_start, units = "mins")
    )
    pr(
      "Date %d/%d (%s) | %s rows -> %s panel cells | elapsed %.1f min",
      date_index,
      length(date_dirs),
      date_string,
      comma(rows_after_global_dedup),
      comma(nrow(current_panel)),
      elapsed_minutes
    )
  }
  
  rm(
    day_rows,
    record_sources,
    record_panel,
    snapshot_rows,
    snapshot_identities,
    snapshot_panel,
    set_rows,
    set_panel,
    current_panel
  )
  invisible(gc(full = TRUE))
}


# =============================================================================
# 3. Final invariants and atomic output
# =============================================================================

panel <- rbindlist(daily_panels, use.names = TRUE, fill = TRUE)
audit <- rbindlist(audit_parts, use.names = TRUE, fill = TRUE)
setorder(panel, experiment_id, website, date)
setorder(audit, date)

if (anyDuplicated(panel, by = daily_key)) {
  stop("Final panel has duplicate participant-website-day keys")
}
if (any(vapply(panel[, ..daily_key], anyNA, logical(1L)))) {
  stop("Final panel has missing key values")
}
if (!all(panel$tracker_record_observed == 1L)) {
  stop("tracker_record_observed invariant failed")
}

count_columns <- setdiff(panel_columns, c(daily_key, "tracker_record_observed"))
if (any(vapply(panel[, ..count_columns], function(x) {
  any(is.na(x) | x < 0)
}, logical(1L)))) {
  stop("Final panel contains missing or negative count values")
}

summary_table <- data.table(
  statistic = c(
    "Panel rows",
    "Participants",
    "Websites",
    "Dates",
    "Tracker records",
    "Tracker records with request-cookie rows",
    "Tracker records with Set-Cookie actions",
    "Tracker records with no cookie rows",
    "Unique request-snapshot third-party cookies",
    "Unique request-snapshot first-party cookies",
    "Recorded third-party Set-Cookie actions",
    "Recorded first-party Set-Cookie actions",
    "Set-Cookie actions with recovered Domain",
    "Source-identity duplicate rows removed across date parts"
  ),
  value = c(
    nrow(panel),
    uniqueN(panel$experiment_id),
    uniqueN(panel$website),
    uniqueN(panel$date),
    sum(panel$n_tracker_records),
    sum(panel$n_tracker_records_with_request_cookie_rows),
    sum(panel$n_tracker_records_with_set_cookie_actions),
    sum(panel$n_tracker_records_with_no_cookie_rows),
    sum(panel$unique_snapshot_cookies_3rd_p),
    sum(panel$unique_snapshot_cookies_1st_p),
    sum(panel$set_cookie_actions_3rd_p),
    sum(panel$set_cookie_actions_1st_p),
    sum(panel$set_cookie_actions_imputed_domain),
    sum(audit$source_duplicate_rows_removed)
  )
)

if (file.exists(OUT_FST_STAGING)) file.remove(OUT_FST_STAGING)
write_fst(panel, OUT_FST_STAGING, compress = 50)
if (file.exists(OUT_FST_BACKUP)) file.remove(OUT_FST_BACKUP)
if (file.exists(OUT_FST) && !file.rename(OUT_FST, OUT_FST_BACKUP)) {
  stop("Could not temporarily move the previous panel: ", OUT_FST)
}
if (!file.rename(OUT_FST_STAGING, OUT_FST)) {
  if (file.exists(OUT_FST_BACKUP)) {
    file.rename(OUT_FST_BACKUP, OUT_FST)
  }
  stop("Could not move completed panel to: ", OUT_FST)
}
if (file.exists(OUT_FST_BACKUP)) file.remove(OUT_FST_BACKUP)
fwrite(audit, OUT_AUDIT)
fwrite(summary_table, OUT_SUMMARY)

pr("=== 03 PANEL BUILD COMPLETE ===")
print(summary_table)
pr("Wrote: %s", OUT_FST)
pr("Wrote: %s", OUT_AUDIT)
pr("Wrote: %s", OUT_SUMMARY)