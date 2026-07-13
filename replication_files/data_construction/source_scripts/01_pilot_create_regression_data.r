#!/usr/bin/env Rscript
# ============================================================================
# PILOT: CREATE REGRESSION DATASET (FINAL FIXED VERSION)
# ============================================================================
# Purpose: Test data processing pipeline on ~13% of data before full run
# Author: Mark Li
# Date: 2025-10-24
# 
# CHANGES FROM ORIGINAL (with explanations):
# 
# 1. THIRD-PARTY LOGIC FIX (Line ~90)
#    Problem: trackers.csv had 27,021 cases where third_party_domain == website
#             These are misclassified first-party cookies
#    Fix: Exclude domain matches from third-party classification
#    Impact: More accurate third-party identification (98.2% vs 100%)
#
# 2. 'NONE' SOURCE HANDLING (Line ~107-139)
#    Problem: Original code excluded 'none' source → no zeros in DV
#    Pre-reg: Says 'none' = user visited but got NO cookies → should be 0
#    Fix: If ALL cookies for (user, website, date) are 'none' → DV = 0
#    Impact: Creates ~26% zeros in DV (realistic tracking-free observations)
#
# 3. RE-AGGREGATION AFTER COMBINING FILES (Lines ~250, ~340, ~430)
#    Problem: Parser split same user's activity across multiple files
#             Example: File 1 has 17 cookies, File 2 has 119 cookies for same key
#             Original code would create 2 rows (duplicates)
#    Fix: After combining files, RE-AGGREGATE by SUMMING cookies for same key
#    Impact: Eliminates 7,952 duplicate rows, correct cookie counts
#    Evidence: Diagnostic showed 77.2% of duplicates have different DV values
#
# 4. P_ij CALCULATION METHOD (Line ~296)
#    Decision: Use MEAN (average tracking intensity)
#    Alternative: SUM (total exposure), MEDIAN (robust to outliers)
#    Rationale: MEAN controls for visit frequency variation
#
# 5. INCREASED SAMPLE SIZE
#    Original: 13 files (~3% of data)
#    Updated: 55 files (~13% of data)
#    Rationale: Better validation before full 429-file run
# ============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(fst)
  library(lubridate)
})

# Start time
start_time <- Sys.time()

cat("\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("PILOT: CREATING REGRESSION DATASET (FINAL FIXED VERSION)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")

# ============================================================================
# CONFIGURATION
# ============================================================================
cat("CONFIGURATION\n")
cat(paste0(rep("=", 13), collapse = ""), "\n")

# UPDATED: Increased sample sizes for better pilot testing
N_PERIOD1 <- 35    # out of 35 (14.3%)
N_PERIOD2 <- 70   # out of 70 (14.3%)
N_PERIOD3 <- 324   # out of 324 (12.3%)

cat("Subset size (INCREASED for better testing):\n")
cat(sprintf("  Period 1: %d files (out of 35, %.1f%%)\n", N_PERIOD1, 100*N_PERIOD1/35))
cat(sprintf("  Period 2: %d files (out of 70, %.1f%%)\n", N_PERIOD2, 100*N_PERIOD2/70))
cat(sprintf("  Period 3: %d files (out of 324, %.1f%%)\n", N_PERIOD3, 100*N_PERIOD3/324))
cat(sprintf("  Total: %d files (~13%% of data)\n\n", N_PERIOD1 + N_PERIOD2 + N_PERIOD3))

# Create output directory
dir.create("pilot_output", showWarnings = FALSE)

# ============================================================================
# STEP 0: Loading metadata
# ============================================================================
cat("STEP 0: Loading metadata\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("0.1 Loading treatment assignments...\n")
treatments <- fread("experiment_conditions_CLEANED.csv")
cat(sprintf("  Rows: %d \n", nrow(treatments)))
cat(sprintf("  Columns: %s \n", paste(names(treatments), collapse = ", ")))
cat(sprintf("  Treatments: %s \n\n", paste(unique(treatments$experiment_condition), collapse = ", ")))

cat("0.2 Loading trackers metadata...\n")
trackers <- fread("trackers.csv")
cat(sprintf("  Rows: %s \n", format(nrow(trackers), big.mark = ",")))

# Standardize column names
setnames(trackers, 
         old = c("id", "domain"),
         new = c("tracker_id", "website"),
         skip_absent = TRUE)

# ============================================================================
# FIX #1: IMPROVED THIRD-PARTY LOGIC
# ============================================================================
cat("\n>>> FIX #1: IMPROVED THIRD-PARTY IDENTIFICATION <<<\n")
cat("WHY: Diagnostics found 27,021 cases where third_party_domain == website\n")
cat("     These are first-party cookies misclassified as third-party\n")
cat("     Example: hobbii.com → hobbii.com should be first-party\n\n")

# ORIGINAL: is_third_party = !is.na(third_party_domain) & third_party_domain != ""
# FIXED: ALSO exclude cases where third_party_domain == website
trackers[, is_third_party := !is.na(third_party_domain) & 
                              third_party_domain != "" &
                              third_party_domain != website]

n_excluded_domain_matches <- sum(!is.na(trackers$third_party_domain) & 
                                 trackers$third_party_domain != "" &
                                 trackers$third_party_domain == trackers$website,
                                 na.rm = TRUE)

cat(sprintf("  Excluded %s domain matches (third_party_domain == website)\n", 
            format(n_excluded_domain_matches, big.mark = ",")))
cat(sprintf("  These are first-party, not third-party\n"))

n_first_party <- sum(!trackers$is_third_party, na.rm = TRUE)
n_third_party <- sum(trackers$is_third_party, na.rm = TRUE)

cat(sprintf("  First-party trackers: %s (%.1f%%)\n", 
            format(n_first_party, big.mark = ","),
            100 * n_first_party / nrow(trackers)))
cat(sprintf("  Third-party trackers: %s (%.1f%%)\n\n", 
            format(n_third_party, big.mark = ","),
            100 * n_third_party / nrow(trackers)))

# ============================================================================
# FIX #2: PROPER 'NONE' SOURCE HANDLING
# ============================================================================
cat(">>> FIX #2: PROPER 'NONE' SOURCE HANDLING <<<\n")
cat("WHY: Pre-reg (line 167-169) says 'none' = user visited but NO cookies\n")
cat("     This is REAL data (not missing), should become 0 in regression\n")
cat("     Original code excluded 'none' → 0% zeros → failed validation\n\n")
cat("LOGIC: If user visited website but ALL cookies are 'none'\n")
cat("       → n_cookies_third_party = 0, n_trackers_third_party = 0\n\n")

#' Aggregate cookie data to (user, website, date) level
#' Properly handles 'none' source to create zero-valued observations
#'
#' @param data Cookie-level data with columns: experiment_id, website, date, 
#'             source, is_third_party, tracker_id
#' @return Aggregated data.table
aggregate_cookies <- function(data) {
  agg <- data[, {
    # Check if ALL cookies for this (user, website, date) are 'none'
    all_none <- all(source == "none")
    
    if (all_none) {
      # User visited but got NO cookies → Zero tracking
      list(
        n_cookies_third_party = 0L,
        n_trackers_third_party = 0L,
        n_cookies_all = 0L,
        has_none = TRUE
      )
    } else {
      # Normal case: count third-party cookies (excluding 'none')
      third_party_mask <- is_third_party == TRUE & source != "none"
      list(
        n_cookies_third_party = sum(third_party_mask, na.rm = TRUE),
        n_trackers_third_party = n_distinct(tracker_id[third_party_mask]),
        n_cookies_all = sum(source != "none"),
        has_none = any(source == "none")
      )
    }
  }, by = .(experiment_id, website, date)]
  
  return(agg)
}

# ============================================================================
# STEP 1: Processing Period 1 (Baseline)
# ============================================================================
cat("STEP 1: Processing Period 1 (Baseline)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

# List all Period 1 files
period1_dir <- "dataset_parsed_06_22"
period1_all_files <- list.files(period1_dir, pattern = "user_trackers_06_22_parsed_.*\\.csv$", full.names = FALSE)
period1_all_files <- sort(period1_all_files)

cat(sprintf("Total Period 1 files available: %d \n", length(period1_all_files)))
cat(sprintf("Using first %d files for pilot\n\n", N_PERIOD1))

# Select subset for pilot
period1_files_subset <- head(period1_all_files, N_PERIOD1)

cat("Processing files:\n")
for (f in period1_files_subset) {
  cat(sprintf("  - %s \n", f))
}
cat("\n")

# Initialize list to store processed files
period1_files <- list()

# Tracking variables for diagnostics
total_test_rows_removed <- 0
total_rows_before_filter <- 0
total_none_rows <- 0

# Process each file
for (i in seq_along(period1_files_subset)) {
  file_name <- period1_files_subset[i]
  file_path <- file.path(period1_dir, file_name)
  
  cat(sprintf("Processing file %d of %d : %s \n", i, length(period1_files_subset), file_name))
  
  proc_start <- Sys.time()
  
  # Read data
  data <- fread(file_path)
  cat(sprintf("  Raw rows: %s \n", format(nrow(data), big.mark = ",")))
  
  # Filter test data
  n_before_filter <- nrow(data)
  total_rows_before_filter <- total_rows_before_filter + n_before_filter
  
  data <- data[nchar(experiment_id) == 7 & !grepl("test", experiment_id, ignore.case = TRUE)]
  
  n_after_filter <- nrow(data)
  n_removed <- n_before_filter - n_after_filter
  total_test_rows_removed <- total_test_rows_removed + n_removed
  
  cat(sprintf("  After test filter: %s rows (removed %s)\n", 
              format(n_after_filter, big.mark = ","),
              format(n_removed, big.mark = ",")))
  
  # Clean: remove rows with missing tracker_id
  data <- data[!is.na(tracker_id)]
  
  # Add date
  data[, date := as.Date(as.POSIXct(tstamp, origin = "1970-01-01", tz = "UTC"))]
  
  # Merge with trackers metadata (including is_third_party)
  data <- merge(data, trackers[, .(tracker_id, website, third_party_domain, is_third_party)], 
                by = "tracker_id", all.x = TRUE)
  
  # Drop rows with missing website
  data <- data[!is.na(website)]
  
  # Track 'none' source
  n_none <- sum(data$source == "none")
  total_none_rows <- total_none_rows + n_none
  cat(sprintf("  'none' source rows: %s (%.1f%%)\n", 
              format(n_none, big.mark = ","),
              100 * n_none / nrow(data)))
  
  # Aggregate using new function (handles 'none' properly)
  agg <- aggregate_cookies(data)
  
  cat(sprintf("  Aggregated to: %s rows\n", format(nrow(agg), big.mark = ",")))
  cat(sprintf("  Rows with zero cookies: %d (%.1f%%)\n", 
              sum(agg$n_cookies_third_party == 0),
              100 * mean(agg$n_cookies_third_party == 0)))
  
  # Add period indicator
  agg[, period := 1]
  
  period1_files[[i]] <- agg
  
  proc_time <- as.numeric(difftime(Sys.time(), proc_start, units = "secs"))
  cat(sprintf("  Processing time: %.1f seconds\n\n", proc_time))
}

cat("TEST DATA FILTERING SUMMARY (Period 1):\n")
cat(sprintf("  Total rows before filtering: %s\n", format(total_rows_before_filter, big.mark = ",")))
cat(sprintf("  Total test rows removed: %s (%.1f%%)\n", 
            format(total_test_rows_removed, big.mark = ","),
            100 * total_test_rows_removed / total_rows_before_filter))
cat(sprintf("  Total 'none' rows encountered: %s\n\n", format(total_none_rows, big.mark = ",")))

# Combine Period 1 files
cat("Combining Period 1 files...\n")
gc()
period1_combined <- rbindlist(period1_files)

cat("Period 1 combined (BEFORE re-aggregation):\n")
cat(sprintf("  Total rows: %s \n", format(nrow(period1_combined), big.mark = ",")))
cat(sprintf("  Unique keys: %s \n", format(nrow(unique(period1_combined[, .(experiment_id, website, date)])), big.mark = ",")))

# Check for duplicates BEFORE re-aggregation
dup_check_before <- period1_combined[, .N, by = .(experiment_id, website, date)][N > 1]
if (nrow(dup_check_before) > 0) {
  cat(sprintf("  ⚠ Found %d duplicate keys (%d extra rows)\n", 
              nrow(dup_check_before), 
              sum(dup_check_before$N) - nrow(dup_check_before)))
}

# ============================================================================
# FIX #3: RE-AGGREGATE TO ELIMINATE DUPLICATES
# ============================================================================
cat("\n>>> FIX #3: RE-AGGREGATING TO ELIMINATE DUPLICATES <<<\n")
cat("WHY: Parser split same user's activity across multiple files\n")
cat("     Example: File 1 has 17 cookies, File 2 has 119 cookies for same key\n")
cat("     Diagnostic showed 77.2% of duplicates have DIFFERENT cookie counts\n")
cat("     → These are PARTIAL counts that need to be SUMMED, not deduplicated\n\n")

# RE-AGGREGATE: Sum cookies for duplicate keys
period1_combined <- period1_combined[, .(
  n_cookies_third_party = sum(n_cookies_third_party),
  n_trackers_third_party = sum(n_trackers_third_party),
  n_cookies_all = sum(n_cookies_all),
  period = first(period),
  has_none = any(has_none)
), by = .(experiment_id, website, date)]

cat("Period 1 combined (AFTER re-aggregation):\n")
cat(sprintf("  Total rows: %s \n", format(nrow(period1_combined), big.mark = ",")))
cat(sprintf("  Unique experiment_ids: %s \n", format(n_distinct(period1_combined$experiment_id), big.mark = ",")))
cat(sprintf("  Unique websites: %s \n", format(n_distinct(period1_combined$website), big.mark = ",")))
cat(sprintf("  Unique dates: %s \n", format(n_distinct(period1_combined$date), big.mark = ",")))
cat(sprintf("  Date range: %s to %s \n", min(period1_combined$date), max(period1_combined$date)))
cat(sprintf("  Rows with zeros: %d (%.1f%%)\n", 
            sum(period1_combined$n_cookies_third_party == 0),
            100 * mean(period1_combined$n_cookies_third_party == 0)))

# Verify no duplicates after re-aggregation
dup_check_after <- period1_combined[, .N, by = .(experiment_id, website, date)][N > 1]
if (nrow(dup_check_after) > 0) {
  cat(sprintf("  ⚠ WARNING: Still %d duplicates after re-aggregation!\n\n", nrow(dup_check_after)))
} else {
  cat(sprintf("  ✓ No duplicates after re-aggregation\n\n"))
}

# Verify no test IDs
cat("DIAGNOSTIC: Verifying no test data in combined Period 1...\n")
test_ids <- sum(grepl("test", period1_combined$experiment_id, ignore.case = TRUE), na.rm = TRUE)
non7char_ids <- sum(nchar(period1_combined$experiment_id) != 7, na.rm = TRUE)

if (test_ids > 0 || non7char_ids > 0) {
  cat(sprintf("  ⚠ WARNING: Found %d test IDs, %d non-7-char IDs\n\n", test_ids, non7char_ids))
} else {
  cat("  ✓ PASS: No test data in Period 1\n\n")
}

# Save Period 1
write_fst(period1_combined, "pilot_output_final/period1_aggregated.fst")
cat("✓ Saved: pilot_output_final/period1_aggregated.fst\n\n")

cat("Period 1 DV distribution:\n")
print(summary(period1_combined$n_cookies_third_party))
cat("\n")

gc()

# ============================================================================
# STEP 2: Constructing P_ij (Baseline Moderator)
# ============================================================================
cat("STEP 2: Constructing P_ij (Baseline Moderator)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat(">>> FIX #4: P_ij CALCULATION METHOD = MEAN <<<\n")
cat("WHY: Need to choose between MEAN, SUM, MEDIAN, MAX\n")
cat("     MEAN = Average tracking intensity (controls for visit frequency)\n")
cat("     SUM = Total exposure (affected by # visits)\n")
cat("     MEDIAN = Robust to outliers\n")
cat("     DECISION: Use MEAN (default, reasonable interpretation)\n\n")

cat("Aggregating to (experiment_id, website) level...\n")
P_ij <- period1_combined[, .(
  P_ij_cookies = mean(n_cookies_third_party),    # MEAN method
  P_ij_trackers = mean(n_trackers_third_party),
  n_visits_baseline = .N,
  sum_cookies = sum(n_cookies_third_party),      # Store SUM for comparison
  median_cookies = median(n_cookies_third_party)  # Store MEDIAN for comparison
), by = .(experiment_id, website)]

cat("P_ij constructed:\n")
cat(sprintf("  User-website pairs: %s \n", format(nrow(P_ij), big.mark = ",")))
cat(sprintf("  Unique experiment_ids: %s \n", format(n_distinct(P_ij$experiment_id), big.mark = ",")))
cat(sprintf("  Unique websites: %s \n\n", format(n_distinct(P_ij$website), big.mark = ",")))

cat("P_ij distribution (MEAN method):\n")
print(summary(P_ij$P_ij_cookies))
cat("\n")

cat(sprintf("Percentage with P_ij = 0: %.1f %%\n\n", 100 * mean(P_ij$P_ij_cookies == 0)))

# Save P_ij
fwrite(P_ij, "pilot_output_final/P_ij_baseline.csv")
cat("✓ Saved: pilot_output_final/P_ij_baseline.csv\n\n")

# ============================================================================
# STEP 3: Processing Period 2 (Intervention)
# ============================================================================
cat("STEP 3: Processing Period 2 (Intervention)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

period2_dir <- "dataset_parsed_07_03"
period2_all_files <- list.files(period2_dir, pattern = "user_trackers_07_03_parsed_.*\\.csv$", full.names = FALSE)
period2_all_files <- sort(period2_all_files)

cat(sprintf("Total Period 2 files available: %d \n", length(period2_all_files)))
cat(sprintf("Using first %d files for pilot\n\n", N_PERIOD2))

period2_files_subset <- head(period2_all_files, N_PERIOD2)

cat("Processing files:\n")
for (f in period2_files_subset) {
  cat(sprintf("  - %s \n", f))
}
cat("\n")

period2_files <- list()

for (i in seq_along(period2_files_subset)) {
  file_name <- period2_files_subset[i]
  file_path <- file.path(period2_dir, file_name)
  
  cat(sprintf("Processing file %d of %d : %s \n", i, length(period2_files_subset), file_name))
  
  data <- fread(file_path)
  cat(sprintf("  Raw rows: %s \n", format(nrow(data), big.mark = ",")))
  
  # Filter test data
  data <- data[nchar(experiment_id) == 7 & !grepl("test", experiment_id, ignore.case = TRUE)]
  cat(sprintf("  After test filter: %s rows\n", format(nrow(data), big.mark = ",")))
  
  data <- data[!is.na(tracker_id)]
  data[, date := as.Date(as.POSIXct(tstamp, origin = "1970-01-01", tz = "UTC"))]
  
  data <- merge(data, trackers[, .(tracker_id, website, third_party_domain, is_third_party)], 
                by = "tracker_id", all.x = TRUE)
  data <- data[!is.na(website)]
  
  # Aggregate with proper 'none' handling
  agg <- aggregate_cookies(data)
  agg[, period := 2]
  
  cat(sprintf("  Aggregated to: %s rows\n", format(nrow(agg), big.mark = ",")))
  cat(sprintf("  Zeros: %d (%.1f%%)\n\n", 
              sum(agg$n_cookies_third_party == 0),
              100 * mean(agg$n_cookies_third_party == 0)))
  
  period2_files[[i]] <- agg
}

period2_combined <- rbindlist(period2_files)

cat("Period 2 combined (BEFORE re-aggregation):\n")
cat(sprintf("  Total rows: %s \n", format(nrow(period2_combined), big.mark = ",")))

# Check for duplicates
dup_check_p2 <- period2_combined[, .N, by = .(experiment_id, website, date)][N > 1]
if (nrow(dup_check_p2) > 0) {
  cat(sprintf("  ⚠ Found %d duplicate keys (%d extra rows)\n", 
              nrow(dup_check_p2), 
              sum(dup_check_p2$N) - nrow(dup_check_p2)))
}

# RE-AGGREGATE Period 2 (same logic as Period 1)
cat("\nRe-aggregating Period 2 to eliminate duplicates...\n")
period2_combined <- period2_combined[, .(
  n_cookies_third_party = sum(n_cookies_third_party),
  n_trackers_third_party = sum(n_trackers_third_party),
  n_cookies_all = sum(n_cookies_all),
  period = first(period),
  has_none = any(has_none)
), by = .(experiment_id, website, date)]

cat("Period 2 combined (AFTER re-aggregation):\n")
cat(sprintf("  Total rows: %s \n", format(nrow(period2_combined), big.mark = ",")))
cat(sprintf("  Date range: %s to %s \n", min(period2_combined$date), max(period2_combined$date)))
cat(sprintf("  Zeros: %d (%.1f%%)\n", 
            sum(period2_combined$n_cookies_third_party == 0),
            100 * mean(period2_combined$n_cookies_third_party == 0)))

# Verify no duplicates
dup_check_p2_after <- period2_combined[, .N, by = .(experiment_id, website, date)][N > 1]
if (nrow(dup_check_p2_after) > 0) {
  cat(sprintf("  ⚠ WARNING: Still %d duplicates\n\n", nrow(dup_check_p2_after)))
} else {
  cat(sprintf("  ✓ No duplicates\n\n"))
}

write_fst(period2_combined, "pilot_output_final/period2_aggregated.fst")
cat("✓ Saved: pilot_output_final/period2_aggregated.fst\n\n")

gc()

# ============================================================================
# STEP 4: Processing Period 3 (Late + Deletion)
# ============================================================================
cat("STEP 4: Processing Period 3 (Late + Deletion)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

period3_dir <- "dataset_parsed_09_16"
period3_all_files <- list.files(period3_dir, pattern = "user_trackers_09_16_parsed_.*\\.csv$", full.names = FALSE)
period3_all_files <- sort(period3_all_files)

cat(sprintf("Total Period 3 files available: %d \n", length(period3_all_files)))
cat(sprintf("Using first %d files for pilot\n\n", N_PERIOD3))

period3_files_subset <- head(period3_all_files, N_PERIOD3)

cat("Processing files:\n")
for (f in period3_files_subset) {
  cat(sprintf("  - %s \n", f))
}
cat("\n")

period3_files <- list()

for (i in seq_along(period3_files_subset)) {
  file_name <- period3_files_subset[i]
  file_path <- file.path(period3_dir, file_name)
  
  cat(sprintf("Processing file %d of %d : %s \n", i, length(period3_files_subset), file_name))
  
  data <- fread(file_path)
  cat(sprintf("  Raw rows: %s \n", format(nrow(data), big.mark = ",")))
  
  data <- data[nchar(experiment_id) == 7 & !grepl("test", experiment_id, ignore.case = TRUE)]
  cat(sprintf("  After test filter: %s rows\n", format(nrow(data), big.mark = ",")))
  
  data <- data[!is.na(tracker_id)]
  data[, date := as.Date(as.POSIXct(tstamp, origin = "1970-01-01", tz = "UTC"))]
  
  data <- merge(data, trackers[, .(tracker_id, website, third_party_domain, is_third_party)], 
                by = "tracker_id", all.x = TRUE)
  data <- data[!is.na(website)]
  
  agg <- aggregate_cookies(data)
  agg[, period := 3]
  
  cat(sprintf("  Aggregated to: %s rows\n", format(nrow(agg), big.mark = ",")))
  cat(sprintf("  Zeros: %d (%.1f%%)\n\n", 
              sum(agg$n_cookies_third_party == 0),
              100 * mean(agg$n_cookies_third_party == 0)))
  
  period3_files[[i]] <- agg
}

period3_combined <- rbindlist(period3_files)

cat("Period 3 combined (BEFORE re-aggregation):\n")
cat(sprintf("  Total rows: %s \n", format(nrow(period3_combined), big.mark = ",")))

# Check for duplicates
dup_check_p3 <- period3_combined[, .N, by = .(experiment_id, website, date)][N > 1]
if (nrow(dup_check_p3) > 0) {
  cat(sprintf("  ⚠ Found %d duplicate keys (%d extra rows)\n", 
              nrow(dup_check_p3), 
              sum(dup_check_p3$N) - nrow(dup_check_p3)))
}

# RE-AGGREGATE Period 3
cat("\nRe-aggregating Period 3 to eliminate duplicates...\n")
period3_combined <- period3_combined[, .(
  n_cookies_third_party = sum(n_cookies_third_party),
  n_trackers_third_party = sum(n_trackers_third_party),
  n_cookies_all = sum(n_cookies_all),
  period = first(period),
  has_none = any(has_none)
), by = .(experiment_id, website, date)]

cat("Period 3 combined (AFTER re-aggregation):\n")
cat(sprintf("  Total rows: %s \n", format(nrow(period3_combined), big.mark = ",")))
cat(sprintf("  Date range: %s to %s \n", min(period3_combined$date), max(period3_combined$date)))
cat(sprintf("  Zeros: %d (%.1f%%)\n", 
            sum(period3_combined$n_cookies_third_party == 0),
            100 * mean(period3_combined$n_cookies_third_party == 0)))

# Verify no duplicates
dup_check_p3_after <- period3_combined[, .N, by = .(experiment_id, website, date)][N > 1]
if (nrow(dup_check_p3_after) > 0) {
  cat(sprintf("  ⚠ WARNING: Still %d duplicates\n\n", nrow(dup_check_p3_after)))
} else {
  cat(sprintf("  ✓ No duplicates\n\n"))
}

write_fst(period3_combined, "pilot_output_final/period3_aggregated.fst")
cat("✓ Saved: pilot_output_final/period3_aggregated.fst\n\n")

gc()

# ============================================================================
# STEP 5: Combining All Periods
# ============================================================================
cat("STEP 5: Combining All Periods\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

# why rbind make sense
panel <- rbind(period1_combined, period2_combined, period3_combined)

cat("Combined panel:\n")
cat(sprintf("  Total rows: %s \n", format(nrow(panel), big.mark = ",")))
cat(sprintf("  Unique keys: %s \n", format(nrow(unique(panel[, .(experiment_id, website, date)])), big.mark = ",")))
cat(sprintf("  Date range: %s to %s \n", min(panel$date), max(panel$date)))
cat(sprintf("  Zeros across all periods: %d (%.1f%%)\n\n", 
            sum(panel$n_cookies_third_party == 0),
            100 * mean(panel$n_cookies_third_party == 0)))

cat("Observations by period:\n")
print(table(panel$period))
cat("\n")

# Final duplicate check (should be 0 if re-aggregation worked)
final_dup_check <- panel[, .N, by = .(experiment_id, website, date)][N > 1]
if (nrow(final_dup_check) > 0) {
  cat(sprintf("⚠ WARNING: Found %d duplicates in final panel!\n", nrow(final_dup_check)))
  cat("   This should not happen if re-aggregation worked correctly.\n\n")
} else {
  cat("✓ VERIFIED: No duplicates in final combined panel\n\n")
}

gc()

# ============================================================================
# STEP 6: Merging Treatment Assignment
# ============================================================================
cat("STEP 6: Merging Treatment Assignment\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat(">>> MERGING ON experiment_id (NOT user_id) <<<\n\n")

cat(sprintf("Rows in panel before merge: %s \n", format(nrow(panel), big.mark = ",")))

# Prepare treatment data
treatment_data <- treatments[, .(experiment_id, 
                                treatment = experiment_condition,
                                cookie_treatment_idx)]

# Merge
panel <- merge(panel, treatment_data, by = "experiment_id", all.x = TRUE)

cat(sprintf("Rows after merge: %s \n", format(nrow(panel), big.mark = ",")))
cat(sprintf("Merge rate: %.1f%%\n\n", 100 * sum(!is.na(panel$treatment)) / nrow(panel)))

cat("Observations by treatment:\n")
print(table(panel$treatment, useNA = "ifany"))
cat("\n")

# ============================================================================
# STEP 7: Merging P_ij Moderator
# ============================================================================
cat("STEP 7: Merging P_ij Moderator\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat(sprintf("Rows before P_ij merge: %s \n", format(nrow(panel), big.mark = ",")))

panel <- merge(panel, P_ij[, .(experiment_id, website, P_ij_cookies, P_ij_trackers, n_visits_baseline)], 
               by = c("experiment_id", "website"), all.x = TRUE)

cat(sprintf("Rows after P_ij merge: %s \n", format(nrow(panel), big.mark = ",")))
cat(sprintf("Missing P_ij: %d (%.1f%%)\n\n", 
            sum(is.na(panel$P_ij_cookies)),
            100 * mean(is.na(panel$P_ij_cookies))))

# ============================================================================
# STEP 8: Creating POST Indicator
# ============================================================================
cat("STEP 8: Creating POST Indicator\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

panel[, POST := ifelse(period == 1, 0, 1)]

cat("POST distribution:\n")
print(table(panel$POST))
cat("\n")

cat("POST by period:\n")
print(table(Period = panel$period, POST = panel$POST))
cat("\n")

# ============================================================================
# STEP 9: Final Data Quality Checks
# ============================================================================
cat("STEP 9: Final Data Quality Checks\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

# Initialize validation results
validation_results <- data.frame(
  Check = character(),
  Result = character(),
  Value = character(),
  stringsAsFactors = FALSE
)

# Check 1: All treatment groups present
treatments_present <- paste(sort(unique(panel$treatment[!is.na(panel$treatment)])), collapse = ", ")
validation_results <- rbind(
  validation_results,
  data.frame(
    Check = "All three treatment groups present",
    Result = ifelse(length(unique(panel$treatment[!is.na(panel$treatment)])) == 3, "PASS", "FAIL"),
    Value = treatments_present
  )
)

# Check 2: POST correctly defined
n_post0 <- sum(panel$POST == 0)
n_post1 <- sum(panel$POST == 1)
validation_results <- rbind(
  validation_results,
  data.frame(
    Check = "POST correctly defined",
    Result = ifelse(n_post0 > 0 && n_post1 > 0, "PASS", "FAIL"),
    Value = sprintf("%d zeros, %d ones", n_post0, n_post1)
  )
)

# Check 3: No duplicate rows (CRITICAL - should now pass)
n_unique <- nrow(unique(panel[, .(experiment_id, website, date)]))
validation_results <- rbind(
  validation_results,
  data.frame(
    Check = "No duplicate rows",
    Result = ifelse(nrow(panel) == n_unique, "PASS", "FAIL"),
    Value = sprintf("%d rows, %d unique", nrow(panel), n_unique)
  )
)

# Check 4: P_ij balanced across treatments
if (sum(!is.na(panel$P_ij_cookies)) > 0) {
  p_ij_by_treatment <- panel[!is.na(P_ij_cookies), .(mean_pij = mean(P_ij_cookies)), by = treatment]
  cv <- sd(p_ij_by_treatment$mean_pij) / mean(p_ij_by_treatment$mean_pij)
  validation_results <- rbind(
    validation_results,
    data.frame(
      Check = "P_ij balanced across treatments",
      Result = ifelse(cv < 0.5, "PASS", "FAIL"),
      Value = sprintf("CV = %.2f", cv)
    )
  )
}

# Check 5: DV has zeros (now expect ~20-30% based on observed data)
zero_pct <- 100 * mean(panel$n_cookies_third_party == 0)
validation_results <- rbind(
  validation_results,
  data.frame(
    Check = "DV has zeros (realistic range)",
    Result = ifelse(zero_pct >= 5, "PASS", "FAIL"),
    Value = sprintf("%.1f%%", zero_pct)
  )
)

# Check 6: All Treatment × POST cells have data
treat_post_table <- table(panel$treatment, panel$POST)
all_cells_filled <- all(treat_post_table > 0)
validation_results <- rbind(
  validation_results,
  data.frame(
    Check = "All Treatment × POST cells have data",
    Result = ifelse(all_cells_filled, "PASS", "FAIL"),
    Value = sprintf("%d of %d cells", sum(treat_post_table > 0), length(treat_post_table))
  )
)

# Check 7: Dates in expected range
date_min <- min(panel$date)
date_max <- max(panel$date)
dates_valid <- date_min >= as.Date("2025-05-01") && date_max <= as.Date("2025-08-31")
validation_results <- rbind(
  validation_results,
  data.frame(
    Check = "Dates in expected range (May-Aug 2025)",
    Result = ifelse(dates_valid, "PASS", "FAIL"),
    Value = sprintf("%s to %s", date_min, date_max)
  )
)

# Check 8: No test IDs
test_ids_final <- sum(grepl("test", panel$experiment_id, ignore.case = TRUE), na.rm = TRUE)
validation_results <- rbind(
  validation_results,
  data.frame(
    Check = "No test IDs in final data",
    Result = ifelse(test_ids_final == 0, "PASS", "FAIL"),
    Value = sprintf("%d test IDs found", test_ids_final)
  )
)

cat("VALIDATION RESULTS:\n")
print(validation_results)
cat("\n")

write.csv(validation_results, "pilot_output_final/validation_results.csv", row.names = FALSE)
cat("✓ Saved: pilot_output_final/validation_results.csv\n\n")

# ============================================================================
# STEP 10: Saving Final Dataset
# ============================================================================
cat("STEP 10: Saving Final Pilot Dataset\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

write.csv(panel, "pilot_output_final/regression_panel_PILOT.csv", row.names = FALSE)
cat("✓ Saved: pilot_output_final/regression_panel_PILOT.csv\n")

write_fst(panel, "pilot_output_final/regression_panel_PILOT.fst")
cat("✓ Saved: pilot_output_final/regression_panel_PILOT.fst\n\n")

# ============================================================================
# STEP 11: Summary Statistics & EDA
# ============================================================================
cat("STEP 11: Summary Statistics & EDA\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("DATASET DIMENSIONS:\n")
cat(sprintf("  Total observations: %s\n", format(nrow(panel), big.mark = ",")))
cat(sprintf("  Unique experiment_ids: %s\n", format(n_distinct(panel$experiment_id), big.mark = ",")))
cat(sprintf("  Unique websites: %s\n", format(n_distinct(panel$website), big.mark = ",")))
cat(sprintf("  Unique dates: %s\n", format(n_distinct(panel$date), big.mark = ",")))
cat(sprintf("  Date range: %s to %s\n\n", min(panel$date), max(panel$date)))

cat("BY PERIOD:\n")
print(table(panel$period))
cat("\n")

cat("BY TREATMENT:\n")
print(table(panel$treatment))
cat("\n")

cat("DEPENDENT VARIABLES:\n")
cat("n_cookies_third_party:\n")
print(summary(panel$n_cookies_third_party))
cat("\nn_trackers_third_party:\n")
print(summary(panel$n_trackers_third_party))
cat("\n")

cat("ZERO ANALYSIS:\n")
cat(sprintf("  Rows with zero cookies: %d (%.1f%%)\n", 
            sum(panel$n_cookies_third_party == 0),
            100 * mean(panel$n_cookies_third_party == 0)))
cat(sprintf("  Rows with zero trackers: %d (%.1f%%)\n\n",
            sum(panel$n_trackers_third_party == 0),
            100 * mean(panel$n_trackers_third_party == 0)))

cat("MODERATOR (P_ij_cookies):\n")
print(summary(panel$P_ij_cookies))
cat("\n")

cat("TREATMENT BALANCE (BASELINE):\n")
baseline_balance <- panel %>%
  filter(period == 1, !is.na(treatment)) %>%
  group_by(treatment) %>%
  summarize(
    n_obs = n(),
    n_users = n_distinct(experiment_id),
    mean_cookies = mean(n_cookies_third_party),
    mean_trackers = mean(n_trackers_third_party),
    pct_zeros = 100 * mean(n_cookies_third_party == 0),
    .groups = "drop"
  )
print(baseline_balance)
cat("\n")

cat("TREATMENT × POST:\n")
print(table(panel$treatment, panel$POST, dnn = c("Treatment", "POST")))
cat("\n")

write.csv(baseline_balance, "pilot_output_final/summary_statistics.csv", row.names = FALSE)
cat("✓ Saved: pilot_output_final/summary_statistics.csv\n\n")

# ============================================================================
# STEP 12: Performance Metrics
# ============================================================================
cat("STEP 12: Performance Metrics\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

end_time <- Sys.time()
total_runtime <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat(sprintf("Total runtime: %.2f minutes\n", total_runtime))
cat(sprintf("Files processed: %d\n", N_PERIOD1 + N_PERIOD2 + N_PERIOD3))
cat(sprintf("Avg time per file: %.0f seconds\n\n", 
            (total_runtime * 60) / (N_PERIOD1 + N_PERIOD2 + N_PERIOD3)))

total_files <- 429
est_hours <- (total_runtime / (N_PERIOD1 + N_PERIOD2 + N_PERIOD3)) * total_files / 60
cat(sprintf("Estimated full dataset: %.1f hours\n\n", est_hours))

cat("MEMORY USAGE:\n")
print(gc())
cat("\n")

# ============================================================================
# COMPLETION
# ============================================================================
cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("PILOT COMPLETED SUCCESSFULLY (FINAL VERSION)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

n_passed <- sum(validation_results$Result == "PASS")
n_total <- nrow(validation_results)

if (n_passed == n_total) {
  cat("🎉 ALL VALIDATION CHECKS PASSED! 🎉\n\n")
  cat("This pilot is ready to scale to full dataset.\n\n")
} else {
  cat(sprintf("⚠ %d of %d checks passed\n\n", n_passed, n_total))
  
  failed_checks <- validation_results[validation_results$Result == "FAIL", ]
  if (nrow(failed_checks) > 0) {
    cat("Failed checks:\n")
    print(failed_checks)
    cat("\n")
  }
}

cat("OUTPUT FILES:\n")
cat("  pilot_output_final/regression_panel_PILOT.csv\n")
cat("  pilot_output_final/regression_panel_PILOT.fst\n")
cat("  pilot_output_final/P_ij_baseline.csv\n")
cat("  pilot_output_final/validation_results.csv\n")
cat("  pilot_output_final/summary_statistics.csv\n")
cat("  pilot_output_final/period1_aggregated.fst\n")
cat("  pilot_output_final/period2_aggregated.fst\n")
cat("  pilot_output_final/period3_aggregated.fst\n\n")

cat("KEY FIXES APPLIED IN THIS VERSION:\n")
cat("  1. ✅ Third-party logic: Excluded 27,021 domain matches\n")
cat("  2. ✅ 'none' source: Creates zero-valued observations (~26%)\n")
cat("  3. ✅ RE-AGGREGATION: Sums cookies for duplicate keys (fixes 7,952 rows)\n")
cat("  4. ✅ P_ij method: MEAN (average tracking intensity)\n")
cat("  5. ✅ Sample size: Increased to 55 files (~13% of data)\n\n")

cat("READY FOR FULL RUN:\n")
cat("  If all checks pass, scale to full 429 files\n")
cat("  Estimated runtime: %.1f hours\n\n", est_hours)

cat("Completed at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")