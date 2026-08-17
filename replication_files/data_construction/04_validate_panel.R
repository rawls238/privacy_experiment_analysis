#!/usr/bin/env Rscript
# =============================================================================
# 04_validate_panel.R
#
# Validation gate for the canonical cookie construction:
#   cookies_classified_by_date/ -> panel_cookies.fst
#
# This script validates construction only. It deliberately does NOT:
#   - treat an old cookie panel as ground truth;
#   - run a treatment-effect regression;
#   - mix request snapshots with Set-Cookie actions; or
#   - use log(1 + x) outcomes.
#
# OUTPUT
#   output/diagnostics/cookie_panel_validation/
#     04_VALIDATE_PANEL_REPORT.txt
#
# Run from the repository root:
#   Rscript replication_files/data_construction/04_validate_panel.R
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
# 0. Paths, schema, and reporting helpers
# =============================================================================

CLASSIFIED_DIR <- "../data/processed_data/cookies_classified_by_date"
PANEL_PATH <- "../data/processed_data/panel_cookies.fst"
AUDIT_03_PATH <-
  "../data/processed_data/panel_cookies_construction_audit.csv"
SUMMARY_03_PATH <-
  "../data/processed_data/panel_cookies_construction_summary.csv"

SUMMARY_02_PATH <- file.path(CLASSIFIED_DIR, "construction_summary.csv")
MANIFEST_02_PATH <- file.path(CLASSIFIED_DIR, "manifest.csv")

REPORT_DIR <- "output/diagnostics/cookie_panel_validation"
REPORT_PATH <- file.path(REPORT_DIR, "04_VALIDATE_PANEL_REPORT.txt")

daily_key <- c("experiment_id", "website", "date")
record_key <- c(
  "experiment_id", "website", "date", "tracker_id", "tstamp"
)
source_record_identity_key <- c(
  "experiment_id", "tracker_id", "tstamp", "website", "source",
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

classified_columns <- c(
  "experiment_id", "tracker_id", "tstamp", "date", "website",
  "source", "is_cookie_row", "cookie_domain", "cookie_name",
  "cookie_path", "store_id", "domain_was_imputed", "is_third_party"
)

count_columns <- setdiff(
  panel_columns,
  c(daily_key, "tracker_record_observed")
)

comma <- function(x) format(x, big.mark = ",", scientific = FALSE)
pr <- function(...) cat(sprintf(...), "\n")
hr <- function(title) {
  cat("\n", strrep("=", 76L), "\n", title, "\n",
      strrep("=", 76L), "\n", sep = "")
}


# =============================================================================
# 1. Run validation with a simultaneous console and text report
# =============================================================================

run_validation <- function() {
  dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)
  report_connection <- file(REPORT_PATH, open = "wt")
  sink(report_connection, split = TRUE)
  on.exit({
    sink()
    close(report_connection)
  }, add = TRUE)
  
  all_ok <- TRUE
  
  PASS <- function(ok, message) {
    ok <- isTRUE(ok)
    cat(if (ok) "  PASS  " else "  FAIL  ", message, "\n", sep = "")
    all_ok <<- all_ok && ok
    invisible(ok)
  }
  
  require_columns <- function(data, required, label) {
    missing_columns <- setdiff(required, names(data))
    PASS(
      length(missing_columns) == 0L,
      sprintf("%s contains every required column", label)
    )
    if (length(missing_columns) > 0L) {
      stop(
        label, " is missing: ",
        paste(missing_columns, collapse = ", ")
      )
    }
  }
  
  required_files <- c(
    PANEL_PATH,
    SUMMARY_02_PATH,
    MANIFEST_02_PATH,
    AUDIT_03_PATH,
    SUMMARY_03_PATH
  )
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0L) {
    stop("Missing required inputs:\n", paste(missing_files, collapse = "\n"))
  }
  
  cat("COOKIE PANEL CONSTRUCTION VALIDATION\n")
  cat("====================================\n\n")
  pr("Working directory: %s", getwd())
  pr("Panel: %s", PANEL_PATH)
  pr("Classified partitions: %s", CLASSIFIED_DIR)
  
  
  # ---------------------------------------------------------------------------
  hr("V1: PANEL SCHEMA, KEYS, AND ACCOUNTING IDENTITIES")
  # ---------------------------------------------------------------------------
  
  panel <- read_fst(PANEL_PATH, as.data.table = TRUE)
  require_columns(panel, panel_columns, "panel_cookies.fst")
  panel[, date := as.Date(date)]
  
  pr(
    "Panel: %s cells | %s participants | %s websites | %s dates",
    comma(nrow(panel)),
    comma(uniqueN(panel$experiment_id)),
    comma(uniqueN(panel$website)),
    comma(uniqueN(panel$date))
  )
  pr(
    "Date range: %s to %s",
    as.character(min(panel$date)),
    as.character(max(panel$date))
  )
  
  PASS(nrow(panel) > 0L, "panel is nonempty")
  PASS(
    !any(vapply(panel[, ..daily_key], anyNA, logical(1L))),
    "participant-website-date keys contain no missing values"
  )
  PASS(
    anyDuplicated(panel, by = daily_key) == 0L,
    "participant-website-date key is unique"
  )
  PASS(
    all(vapply(panel[, ..count_columns], is.numeric, logical(1L))),
    "all count measures are numeric"
  )
  PASS(
    all(vapply(panel[, ..count_columns], function(x) {
      all(is.finite(x) & x >= 0 & x == floor(x))
    }, logical(1L))),
    "all count measures are finite, nonnegative integers"
  )
  PASS(
    all(panel$tracker_record_observed == 1L),
    "every panel row represents at least one captured tracker record"
  )
  PASS(
    all(panel$n_tracker_records >= 1L),
    "every panel row has a positive tracker-record count"
  )
  PASS(
    all(
      panel$n_tracker_records_with_request_cookie_rows <=
        panel$n_tracker_records
    ),
    "request-cookie record count never exceeds total tracker records"
  )
  PASS(
    all(
      panel$n_tracker_records_with_set_cookie_actions <=
        panel$n_tracker_records
    ),
    "Set-Cookie record count never exceeds total tracker records"
  )
  PASS(
    all(
      panel$n_tracker_records_with_no_cookie_rows <=
        panel$n_tracker_records
    ),
    "no-cookie record count never exceeds total tracker records"
  )
  PASS(
    all(
      panel$unique_snapshot_cookies_total ==
        panel$unique_snapshot_cookies_3rd_p +
        panel$unique_snapshot_cookies_1st_p
    ),
    "snapshot total equals first-party plus third-party snapshot identities"
  )
  PASS(
    all(
      panel$set_cookie_actions_total ==
        panel$set_cookie_actions_3rd_p +
        panel$set_cookie_actions_1st_p
    ),
    "Set-Cookie total equals first-party plus third-party actions"
  )
  
  
  # ---------------------------------------------------------------------------
  hr("V2: RECONCILE THE 02 AND 03 CONSTRUCTION AUDITS")
  # ---------------------------------------------------------------------------
  
  summary_02 <- fread(SUMMARY_02_PATH)
  manifest_02 <- fread(MANIFEST_02_PATH)
  audit_03 <- fread(AUDIT_03_PATH)
  summary_03 <- fread(SUMMARY_03_PATH)
  
  required_summary_02 <- c(
    "rows_after_shard_source_dedup", "none_rows", "request_rows",
    "set_rows", "set_domain_rows_imputed", "date_part_files", "dates"
  )
  required_audit_03 <- c(
    "date", "rows_before_global_source_dedup",
    "rows_after_global_source_dedup", "source_duplicate_rows_removed",
    "tracker_records", "participant_website_day_cells",
    "unique_snapshot_cookies_3rd_p",
    "unique_snapshot_cookies_1st_p",
    "set_cookie_actions_3rd_p", "set_cookie_actions_1st_p"
  )
  require_columns(summary_02, required_summary_02, "02 construction summary")
  require_columns(manifest_02, c("date", "part", "rows"), "02 manifest")
  require_columns(audit_03, required_audit_03, "03 construction audit")
  require_columns(summary_03, c("statistic", "value"), "03 summary")
  
  rows_02_after_shard <- as.numeric(
    summary_02$rows_after_shard_source_dedup[1L]
  )
  rows_02_written <- sum(as.numeric(manifest_02$rows))
  rows_03_before_global <- sum(
    as.numeric(audit_03$rows_before_global_source_dedup)
  )
  rows_03_after_global <- sum(
    as.numeric(audit_03$rows_after_global_source_dedup)
  )
  duplicates_03_global <- sum(
    as.numeric(audit_03$source_duplicate_rows_removed)
  )
  
  reconciliation <- data.table(
    stage = c(
      "02 rows after within-shard identity dedup",
      "02 rows written after buffer identity dedup",
      "03 rows before cross-part identity dedup",
      "03 rows after cross-part identity dedup"
    ),
    rows = c(
      rows_02_after_shard,
      rows_02_written,
      rows_03_before_global,
      rows_03_after_global
    )
  )
  print(reconciliation)
  
  PASS(
    rows_02_after_shard >= rows_02_written,
    "buffer-level deduplication cannot increase classified rows"
  )
  PASS(
    rows_02_written == rows_03_before_global,
    "02 manifest rows equal the rows read by 03 before cross-part dedup"
  )
  PASS(
    rows_03_before_global - rows_03_after_global == duplicates_03_global,
    "03 before-after row difference equals reported cross-part duplicates"
  )
  PASS(
    nrow(manifest_02) == summary_02$date_part_files[1L],
    "manifest part count equals the 02 summary"
  )
  PASS(
    uniqueN(as.character(manifest_02$date)) == summary_02$dates[1L],
    "manifest date count equals the 02 summary"
  )
  PASS(
    uniqueN(panel$date) == summary_02$dates[1L],
    "panel date count equals the classified date count"
  )
  PASS(
    sum(audit_03$participant_website_day_cells) == nrow(panel),
    "daily audit panel-cell total equals final panel rows"
  )
  PASS(
    sum(audit_03$tracker_records) == sum(panel$n_tracker_records),
    "daily audit tracker-record total equals final panel"
  )
  PASS(
    sum(audit_03$unique_snapshot_cookies_3rd_p) ==
      sum(panel$unique_snapshot_cookies_3rd_p),
    "daily audit third-party snapshot total equals final panel"
  )
  PASS(
    sum(audit_03$unique_snapshot_cookies_1st_p) ==
      sum(panel$unique_snapshot_cookies_1st_p),
    "daily audit first-party snapshot total equals final panel"
  )
  PASS(
    sum(audit_03$set_cookie_actions_3rd_p) ==
      sum(panel$set_cookie_actions_3rd_p),
    "daily audit third-party Set-Cookie total equals final panel"
  )
  PASS(
    sum(audit_03$set_cookie_actions_1st_p) ==
      sum(panel$set_cookie_actions_1st_p),
    "daily audit first-party Set-Cookie total equals final panel"
  )
  
  imputed_02 <- as.numeric(summary_02$set_domain_rows_imputed[1L])
  imputed_panel <- sum(panel$set_cookie_actions_imputed_domain)
  imputed_removed_cross_part <- imputed_02 - imputed_panel
  pr(
    "Recovered Set-Cookie Domain rows: 02=%s | panel=%s | removed later=%s",
    comma(imputed_02),
    comma(imputed_panel),
    comma(imputed_removed_cross_part)
  )
  PASS(
    imputed_removed_cross_part >= 0 &
      imputed_removed_cross_part <=
      (rows_02_after_shard - rows_03_after_global),
    "recovered-Domain loss is fully bounded by later identity deduplication"
  )
  
  summary_03[, value := as.numeric(value)]
  summary_03_values <- setNames(summary_03$value, summary_03$statistic)
  expected_summary_statistics <- c(
    "Panel rows",
    "Tracker records",
    "Unique request-snapshot third-party cookies",
    "Unique request-snapshot first-party cookies",
    "Recorded third-party Set-Cookie actions",
    "Recorded first-party Set-Cookie actions",
    "Set-Cookie actions with recovered Domain",
    "Source-identity duplicate rows removed across date parts"
  )
  PASS(
    all(expected_summary_statistics %chin% names(summary_03_values)),
    "03 summary contains every headline construction statistic"
  )
  if (all(expected_summary_statistics %chin% names(summary_03_values))) {
    PASS(
      summary_03_values[["Panel rows"]] == nrow(panel) &
        summary_03_values[["Tracker records"]] ==
        sum(panel$n_tracker_records) &
        summary_03_values[[
          "Unique request-snapshot third-party cookies"
        ]] == sum(panel$unique_snapshot_cookies_3rd_p) &
        summary_03_values[[
          "Unique request-snapshot first-party cookies"
        ]] == sum(panel$unique_snapshot_cookies_1st_p) &
        summary_03_values[[
          "Recorded third-party Set-Cookie actions"
        ]] == sum(panel$set_cookie_actions_3rd_p) &
        summary_03_values[[
          "Recorded first-party Set-Cookie actions"
        ]] == sum(panel$set_cookie_actions_1st_p) &
        summary_03_values[[
          "Set-Cookie actions with recovered Domain"
        ]] == imputed_panel &
        summary_03_values[[
          "Source-identity duplicate rows removed across date parts"
        ]] == duplicates_03_global,
      "03 summary exactly reproduces final panel and audit totals"
    )
  }
  
  
  # ---------------------------------------------------------------------------
  hr("V3: TRACKER-RECORD SOURCE DECOMPOSITION")
  # ---------------------------------------------------------------------------
  
  total_records <- sum(panel$n_tracker_records)
  request_records <- sum(
    panel$n_tracker_records_with_request_cookie_rows
  )
  set_records <- sum(
    panel$n_tracker_records_with_set_cookie_actions
  )
  none_records <- sum(
    panel$n_tracker_records_with_no_cookie_rows
  )
  
  request_or_set_records <- total_records - none_records
  both_records <- request_records + set_records - request_or_set_records
  request_only_records <- request_records - both_records
  set_only_records <- set_records - both_records
  
  source_decomposition <- data.table(
    source_profile = c(
      "request rows only",
      "Set-Cookie actions only",
      "both request rows and Set-Cookie actions",
      "no cookie rows"
    ),
    tracker_records = c(
      request_only_records,
      set_only_records,
      both_records,
      none_records
    )
  )
  source_decomposition[, percent :=
                         100 * tracker_records / total_records]
  print(source_decomposition)
  
  PASS(
    all(source_decomposition$tracker_records >= 0),
    "all mutually exclusive source-profile counts are nonnegative"
  )
  PASS(
    sum(source_decomposition$tracker_records) == total_records,
    "source profiles sum exactly to total tracker records"
  )
  PASS(
    none_records <= as.numeric(summary_02$none_rows[1L]),
    "panel no-cookie records do not exceed classified none rows"
  )
  
  pr(
    "Snapshot identities: third-party %s | first-party %s",
    comma(sum(panel$unique_snapshot_cookies_3rd_p)),
    comma(sum(panel$unique_snapshot_cookies_1st_p))
  )
  pr(
    "Set-Cookie actions: third-party %s | first-party %s",
    comma(sum(panel$set_cookie_actions_3rd_p)),
    comma(sum(panel$set_cookie_actions_1st_p))
  )
  
  
  # ---------------------------------------------------------------------------
  hr("V4: INDEPENDENT RECOMPUTATION OF THREE PANEL CELLS")
  # ---------------------------------------------------------------------------
  
  eligible <- panel[
    unique_snapshot_cookies_total > 0 |
      set_cookie_actions_total > 0
  ]
  eligible_dates <- sort(unique(eligible$date))
  PASS(
    length(eligible_dates) >= 3L,
    "at least three dates contain positive cookie measures"
  )
  
  set.seed(42L)
  sampled_date_indices <- sample.int(
    length(eligible_dates),
    size = min(3L, length(eligible_dates)),
    replace = FALSE
  )
  sampled_dates <- eligible_dates[sampled_date_indices]
  sampled_cells <- rbindlist(lapply(
    as.character(sampled_dates),
    function(sampled_date_string) {
      candidates <- eligible[date == as.Date(sampled_date_string)]
      candidates[sample.int(nrow(candidates), 1L)]
    }))
  
  measures_to_check <- c(
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
  
  recompute_ok <- TRUE
  
  for (cell_index in seq_len(nrow(sampled_cells))) {
    cell <- sampled_cells[cell_index]
    date_string <- format(cell$date, "%Y-%m-%d")
    date_dir <- file.path(CLASSIFIED_DIR, date_string)
    part_files <- sort(list.files(
      date_dir,
      pattern = "[.]fst$",
      full.names = TRUE
    ))
    
    if (length(part_files) == 0L) {
      PASS(FALSE, paste("classified parts exist for", date_string))
      recompute_ok <- FALSE
      next
    }
    
    matched_parts <- vector("list", length(part_files))
    matched_count <- 0L
    
    for (part_path in part_files) {
      current <- read_fst(
        part_path,
        columns = classified_columns,
        as.data.table = TRUE
      )
      current[, date := as.Date(date)]
      current <- current[
        experiment_id == cell$experiment_id &
          website == cell$website &
          date == cell$date
      ]
      if (nrow(current) > 0L) {
        matched_count <- matched_count + 1L
        matched_parts[[matched_count]] <- current
      }
    }
    
    if (matched_count == 0L) {
      PASS(
        FALSE,
        sprintf(
          "classified rows found for sampled cell %s | %s | %s",
          cell$experiment_id,
          cell$website,
          date_string
        )
      )
      recompute_ok <- FALSE
      next
    }
    
    raw <- rbindlist(
      matched_parts[seq_len(matched_count)],
      use.names = TRUE,
      fill = TRUE
    )
    raw <- unique(raw, by = source_record_identity_key)
    
    record_sources <- raw[, .(
      has_request_cookie_rows = any(source == "request"),
      has_set_cookie_actions = any(source == "set"),
      has_none_marker = any(source == "none")
    ), by = record_key]
    
    snapshot_identities <- unique(
      raw[
        source == "request" &
          is_cookie_row == TRUE &
          cookie_name != "" &
          !is.na(is_third_party)
      ],
      by = c(
        "cookie_domain", "cookie_name", "cookie_path", "store_id"
      )
    )
    
    set_rows <- raw[
      source == "set" &
        is_cookie_row == TRUE &
        cookie_name != "" &
        !is.na(is_third_party)
    ]
    
    manual_values <- c(
      tracker_record_observed = as.integer(nrow(record_sources) > 0L),
      n_tracker_records = nrow(record_sources),
      n_tracker_records_with_request_cookie_rows =
        sum(record_sources$has_request_cookie_rows),
      n_tracker_records_with_set_cookie_actions =
        sum(record_sources$has_set_cookie_actions),
      n_tracker_records_with_no_cookie_rows = sum(
        record_sources$has_none_marker &
          !record_sources$has_request_cookie_rows &
          !record_sources$has_set_cookie_actions
      ),
      unique_snapshot_cookies_3rd_p =
        snapshot_identities[is_third_party == TRUE, .N],
      unique_snapshot_cookies_1st_p =
        snapshot_identities[is_third_party == FALSE, .N],
      unique_snapshot_cookies_total = nrow(snapshot_identities),
      set_cookie_actions_3rd_p = set_rows[is_third_party == TRUE, .N],
      set_cookie_actions_1st_p = set_rows[is_third_party == FALSE, .N],
      set_cookie_actions_total = nrow(set_rows),
      set_cookie_actions_imputed_domain =
        set_rows[domain_was_imputed == TRUE, .N]
    )
    
    panel_values <- as.numeric(unlist(
      cell[, ..measures_to_check],
      use.names = FALSE
    ))
    names(panel_values) <- measures_to_check
    cell_matches <- manual_values[measures_to_check] == panel_values
    
    pr(
      paste0(
        "Cell %d: %s | %s | %s | snapshot 3p %s/%s | ",
        "Set-Cookie 3p %s/%s | %s"
      ),
      cell_index,
      cell$experiment_id,
      cell$website,
      date_string,
      comma(manual_values[["unique_snapshot_cookies_3rd_p"]]),
      comma(panel_values[["unique_snapshot_cookies_3rd_p"]]),
      comma(manual_values[["set_cookie_actions_3rd_p"]]),
      comma(panel_values[["set_cookie_actions_3rd_p"]]),
      if (all(cell_matches)) "MATCH" else "MISMATCH"
    )
    
    if (!all(cell_matches)) {
      print(data.table(
        measure = measures_to_check,
        recomputed = as.numeric(manual_values[measures_to_check]),
        panel = panel_values,
        match = as.logical(cell_matches)
      )[match == FALSE])
    }
    
    recompute_ok <- recompute_ok && all(cell_matches)
    rm(
      raw,
      record_sources,
      snapshot_identities,
      set_rows,
      matched_parts
    )
    invisible(gc(full = TRUE))
  }
  
  PASS(
    recompute_ok && nrow(sampled_cells) == 3L,
    "independent classified-row recomputation matches all three panel cells"
  )
  
  
  # ---------------------------------------------------------------------------
  hr(sprintf(
    "GATE RESULT: %s",
    if (all_ok) {
      "ALL PASS -- CONSTRUCTION CLEARED FOR THE NEXT COVERAGE TEST"
    } else {
      "FAILURES -- DO NOT USE THE PANEL DOWNSTREAM"
    }
  ))
  pr("Copy/paste report: %s", REPORT_PATH)
  cat("=== DONE 04 ===\n")
  
  all_ok
}


gate_ok <- run_validation()
if (!gate_ok) {
  stop("04 validation failed. See: ", REPORT_PATH)
}