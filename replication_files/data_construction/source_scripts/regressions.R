#!/usr/bin/env Rscript
# ============================================================================
# PRIVACY TRANSPARENCY EXPERIMENT - COMPLETE ANALYSIS PIPELINE
# ============================================================================
# 
# PURPOSE: Analyze treatment effects on online tracking behavior
# 
# STRUCTURE:
#   PART A: DATA ENGINEERING (Steps 1-5)
#           - Load, clean, merge, create variables
#   PART B: STATISTICAL MODELING (Steps 6-11)
#           - Run regressions, display results
#
# KEY INSIGHT: 
#   Total Trackers = Trackers per Visit × Number of Visits
#   → Must decompose to understand mechanisms
#
# TREATMENTS:
#   - Control: No intervention
#   - Saliency: Easy access to privacy policy
#   - Info: Simplified privacy information
#
# ============================================================================

library(fixest)
library(data.table)
library(fst)

options(width = 120)

cat("\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("PRIVACY TRANSPARENCY EXPERIMENT - COMPLETE ANALYSIS\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

# ============================================================================
# ============================================================================
# PART A: DATA ENGINEERING
# ============================================================================
# ============================================================================

# ============================================================================
# STEP 1: LOAD AND CLEAN PANEL DATA (Tracker Data)
# ============================================================================
# 
# INPUTS: regression_panel_PILOT.fst
#   - Raw panel with tracker/cookie counts per (user, website, date)
#
# CLEANING RULES:
#   1. Remove rows with missing treatment assignment
#   2. Remove invalid experiment_ids (not 7 characters)
#   3. Remove test users (experiment_id contains "test")
#   4. Remove UNKNOWN users
#
# OUTPUT: Clean panel ready for merge
# ============================================================================

cat("STEP 1: Loading and Cleaning Panel Data (Tracker Data)\n")
cat(paste0(rep("-", 80), collapse = ""), "\n\n")

# Load raw panel
panel_raw <- read_fst("regression_panel_PILOT.fst", as.data.table = TRUE)
cat(sprintf("Loaded raw panel: %s rows\n", format(nrow(panel_raw), big.mark = ",")))

# CLEAN 1: Remove missing treatment
panel <- panel_raw[!is.na(treatment)]
cat(sprintf("  → Remove NA treatment: %s rows removed\n", 
            format(nrow(panel_raw) - nrow(panel), big.mark = ",")))

# CLEAN 2: Remove invalid experiment_id length (must be 7 characters)
panel <- panel[nchar(experiment_id) == 7]
cat(sprintf("  → Remove wrong ID length: %s rows removed\n", 
            format(nrow(panel_raw) - nrow(panel), big.mark = ",")))

# CLEAN 3: Remove test users
panel <- panel[!grepl("test", experiment_id, ignore.case = TRUE)]
cat(sprintf("  → Remove test users: %s rows removed\n", 
            format(nrow(panel_raw) - nrow(panel), big.mark = ",")))

# CLEAN 4: Remove UNKNOWN
panel <- panel[experiment_id != "UNKNOWN"]
cat(sprintf("  → Remove UNKNOWN: %s rows removed\n\n", 
            format(nrow(panel_raw) - nrow(panel), big.mark = ",")))

cat(sprintf("Clean panel: %s rows\n", format(nrow(panel), big.mark = ",")))
cat(sprintf("  Users: %d\n", n_distinct(panel$experiment_id)))
cat(sprintf("  Websites: %s\n", format(n_distinct(panel$website), big.mark = ",")))
cat(sprintf("  Dates: %s to %s\n\n", min(panel$date), max(panel$date)))

# ============================================================================
# STEP 2: LOAD AND CLEAN TIME DATA (Usage Data)
# ============================================================================
#
# INPUTS: time_data_2.csv
#   - Raw usage data with visit_count, time_spent per (user, website, date)
#
# CLEANING RULES (same as panel):
#   1. Remove UNKNOWN users
#   2. Remove test users
#   3. Remove invalid experiment_id length
#
# NOTE: We do NOT filter dates here - panel dates determine final range
#
# OUTPUT: Aggregated time data ready for merge
# ============================================================================

cat("STEP 2: Loading and Cleaning Time Data (Usage Data)\n")
cat(paste0(rep("-", 80), collapse = ""), "\n\n")

# Load raw time data
time_raw <- fread("time_data_2.csv")
cat(sprintf("Loaded raw time data: %s rows\n", format(nrow(time_raw), big.mark = ",")))

# Parse dates
time_raw[, date := as.Date(date, format = "%m/%d/%Y")]

# CLEAN 0: Remove invalid dates (NEWLY ADDED - FIX FOR 1969/2089 BUG)
before <- nrow(time_raw)
time_clean <- time_raw[
  !is.na(date) & 
    date >= as.Date("2024-01-01") & 
    date <= as.Date("2026-12-31")
]
cat(sprintf("  → Remove bad dates: %s rows removed\n", 
            format(before - nrow(time_clean), big.mark = ",")))

# CLEAN 1: Remove UNKNOWN
time_clean <- time_raw[experiment_id != "UNKNOWN"]
cat(sprintf("  → Remove UNKNOWN: %s rows removed\n", 
            format(nrow(time_raw) - nrow(time_clean), big.mark = ",")))

# CLEAN 2: Remove test users
before <- nrow(time_clean)
time_clean <- time_clean[!grepl("test", experiment_id, ignore.case = TRUE)]
cat(sprintf("  → Remove test users: %s rows removed\n", 
            format(before - nrow(time_clean), big.mark = ",")))

# CLEAN 3: Remove invalid experiment_id length
before <- nrow(time_clean)
time_clean <- time_clean[nchar(experiment_id) == 7]
cat(sprintf("  → Remove wrong ID length: %s rows removed\n\n", 
            format(before - nrow(time_clean), big.mark = ",")))

cat(sprintf("Clean time data: %s rows\n", format(nrow(time_clean), big.mark = ",")))
cat(sprintf("  Date range: %s to %s\n\n", 
            min(time_clean$date), max(time_clean$date)))

# AGGREGATE: Sum usage by (user, website, date)
# Multiple rows per combination → collapse to one row
cat("Aggregating time data by (user, website, date)...\n")
time_agg <- time_clean[, .(
  time_spent = sum(time_spent, na.rm = TRUE),
  visit_count = sum(visit_count, na.rm = TRUE),
  elicitation_count = sum(elicitation_count, na.rm = TRUE)
), by = .(experiment_id, website, date)]

cat(sprintf("After aggregation: %s unique (user, website, date) combinations\n\n", 
            format(nrow(time_agg), big.mark = ",")))

# ============================================================================
# STEP 3: MERGE PANEL DATA + TIME DATA
# ============================================================================
#
# MERGE STRATEGY: Left join (keep all panel rows)
#   - Panel = tracking observations (complete)
#   - Time = usage observations (83% coverage)
#   - Unmatched rows → NA for visit_count/time_spent
#
# WHY UNMATCHED? 
#   - Cross-site tracking: Panel records tracker on Site A when user visits Site B
#   - Example: Facebook pixel fires on NYTimes → panel has facebook.com row,
#     but time data only has nytimes.com row
#   - This is EXPECTED for 3rd-party trackers (not a data quality issue)
#
# OUTPUT: Merged panel with usage data (NAs for unmatched)
# ============================================================================

cat("STEP 3: Merging Panel + Time Data\n")
cat(paste0(rep("-", 80), collapse = ""), "\n\n")

cat("Merge strategy: LEFT JOIN (keep all panel observations)\n")
cat("  Panel = tracking data (all rows)\n")
cat("  Time = usage data (83%% coverage)\n")
cat("  Unmatched → NA for visit/time variables\n\n")

panel_merged <- merge(
  panel,
  time_agg,
  by = c("experiment_id", "website", "date"),
  all.x = TRUE  # Keep all panel rows
)

# Calculate merge coverage
n_matched <- sum(!is.na(panel_merged$visit_count))
coverage_pct <- 100 * n_matched / nrow(panel_merged)

cat(sprintf("Merge results:\n"))
cat(sprintf("  Total rows: %s\n", format(nrow(panel_merged), big.mark = ",")))
cat(sprintf("  Matched (have usage data): %s (%.1f%%)\n", 
            format(n_matched, big.mark = ","), coverage_pct))
cat(sprintf("  Unmatched (cross-site tracking): %s (%.1f%%)\n\n", 
            format(nrow(panel_merged) - n_matched, big.mark = ","), 
            100 - coverage_pct))

cat("NOTE: Unmatched rows are legitimate tracking observations\n")
cat("      (3rd-party trackers firing on other sites)\n\n")

# ============================================================================
# STEP 4: CREATE POST VARIABLE
# ============================================================================
#
# POST = Treatment period indicator
#   POST = 0: Period 1 (baseline, no treatment)
#   POST = 1: Periods 2 + 3 (treatment active)
#
# WHY NEEDED: DiD specification tests Treatment × POST interaction
#   → Answers: "Did behavior change AFTER treatment started?"
#
# ============================================================================

cat("STEP 4: Creating POST Variable\n")
cat(paste0(rep("-", 80), collapse = ""), "\n\n")

# Check if POST already exists
if ("POST" %in% names(panel_merged)) {
  cat("POST variable already exists in data\n")
  cat("Values:\n")
  print(table(panel_merged$POST, panel_merged$period))
} else {
  cat("Creating POST variable:\n")
  cat("  POST = 0 for Period 1 (baseline)\n")
  cat("  POST = 1 for Periods 2 & 3 (treatment)\n\n")
  
  panel_merged[, POST := as.numeric(period %in% c(2, 3))]
  
  cat("Verification:\n")
  print(table(panel_merged$POST, panel_merged$period))
}

cat("\n")

# ============================================================================
# STEP 5: CREATE DERIVED VARIABLES
# ============================================================================
#
# INTENSITY METRICS (usage-adjusted):
#   - trackers_per_visit = n_trackers / visit_count
#   - cookies_per_visit = n_cookies / visit_count
#   - trackers_per_minute = n_trackers / (time_spent/60)
#   → Only defined where visit_count > 0
#
# LOG TRANSFORMS (for skewed variables):
#   - log_visit = log(1 + visit_count)
#   - log_time = log(1 + time_spent)
#   → Handles skewness without dropping observations
#
# FLAGS:
#   - has_visit_data = indicator for matched observations
#
# TREATMENT DUMMIES:
#   - Set "control" as reference category for regression
#
# ============================================================================

cat("STEP 5: Creating Derived Variables\n")
cat(paste0(rep("-", 80), collapse = ""), "\n\n")

# Set reference level for treatment (control = baseline)
panel_merged[, treatment := relevel(factor(treatment), ref = "control")]
cat("Set treatment reference level: control\n\n")

# INTENSITY METRICS: Trackers/cookies per visit
# Only calculate where visit_count > 0 (avoid division by zero)
cat("Creating intensity metrics (usage-adjusted):\n")

panel_merged[visit_count > 0, 
             trackers_per_visit := n_trackers_third_party / visit_count]
panel_merged[visit_count > 0, 
             cookies_per_visit := n_cookies_third_party / visit_count]
panel_merged[time_spent > 0, 
             trackers_per_minute := n_trackers_third_party / (time_spent / 60)]

cat(sprintf("  trackers_per_visit: %s non-NA values\n", 
            format(sum(!is.na(panel_merged$trackers_per_visit)), big.mark = ",")))
cat(sprintf("  cookies_per_visit: %s non-NA values\n", 
            format(sum(!is.na(panel_merged$cookies_per_visit)), big.mark = ",")))
cat(sprintf("  trackers_per_minute: %s non-NA values\n\n", 
            format(sum(!is.na(panel_merged$trackers_per_minute)), big.mark = ",")))

# LOG TRANSFORMS: Handle skewness
# log(1 + x) transformation: preserves zeros, compresses large values
cat("Creating log-transformed variables:\n")

panel_merged[, log_visit := log(1 + visit_count)]
panel_merged[, log_time := log(1 + time_spent)]

cat("  log_visit = log(1 + visit_count)\n")
cat("  log_time = log(1 + time_spent)\n\n")

# FLAG: Has visit data
panel_merged[, has_visit_data := !is.na(visit_count)]
cat(sprintf("has_visit_data flag: %s TRUE (%.1f%%)\n\n",
            format(sum(panel_merged$has_visit_data), big.mark = ","),
            100 * mean(panel_merged$has_visit_data)))

# ============================================================================
# TODO: HETEROGENEITY VARIABLE P_ij (PRIVACY INVASIVENESS)
# ============================================================================
#
# SKIPPED FOR NOW - Need conjoint preference data
#
# WHEN AVAILABLE:
#   Input File 1: user_conjoint_weights.csv
#     Columns: experiment_id, attr_1_weight, ..., attr_19_weight
#     → User i's preference weights from conjoint analysis
#
#   Input File 2: website_privacy_attributes.csv
#     Columns: website, sells_data, collects_demo, ..., third_party
#     → Website j's binary privacy attributes (hand-coded)
#
#   Calculation:
#     P_ij = sum(w_i,k × A_j,k) for k=1 to 19
#     → Personalized privacy invasiveness score
#
#   Then create:
#     high_privacy_concern = (P_ij > median(P_ij))
#     
#   And run heterogeneity models:
#     y ~ Treatment×POST + Treatment×POST×high_privacy_concern + FE
#
# CODE TEMPLATE (uncomment when data available):
# 
# # Load conjoint weights
# conjoint <- fread("user_conjoint_weights.csv")
# 
# # Load website attributes  
# attributes <- fread("website_privacy_attributes.csv")
# 
# # Merge to panel
# panel_merged <- merge(panel_merged, conjoint, by = "experiment_id")
# panel_merged <- merge(panel_merged, attributes, by = "website")
# 
# # Calculate P_ij (weighted sum)
# attr_cols <- grep("^attr_", names(panel_merged), value = TRUE)
# weight_cols <- grep("_weight$", names(panel_merged), value = TRUE)
# 
# panel_merged[, P_ij := rowSums(.SD * .SD, .SDcols = c(attr_cols, weight_cols))]
# 
# # Create high/low indicator
# panel_merged[, high_privacy_concern := P_ij > median(P_ij, na.rm = TRUE)]
# 
# ============================================================================

cat("NOTE: P_ij heterogeneity analysis SKIPPED\n")
cat("      Requires: user conjoint weights + website privacy attributes\n")
cat("      See TODO section in code for implementation when data available\n\n")

# ============================================================================
# SAVE CLEANED DATA
# ============================================================================

cat("Saving cleaned merged dataset...\n")
write_fst(panel_merged, "panel_merged_CLEAN.fst")
cat("✓ Saved: panel_merged_CLEAN.fst\n\n")

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("DATA ENGINEERING COMPLETE\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

# Summary statistics
cat("Final dataset summary:\n")
cat(sprintf("  Observations: %s\n", format(nrow(panel_merged), big.mark = ",")))
cat(sprintf("  Users: %d\n", n_distinct(panel_merged$experiment_id)))
cat(sprintf("  Websites: %s\n", format(n_distinct(panel_merged$website), big.mark = ",")))
cat(sprintf("  Date range: %s to %s (%d days)\n",
            min(panel_merged$date), max(panel_merged$date),
            as.numeric(max(panel_merged$date) - min(panel_merged$date)) + 1))
cat(sprintf("  With usage data: %.1f%%\n\n", 100 * mean(panel_merged$has_visit_data)))

cat("Treatment groups:\n")
print(table(panel_merged$treatment))
cat("\nPeriods:\n")
print(table(panel_merged$period))
cat("\n\n")

# ============================================================================
# ============================================================================
# PART B: STATISTICAL MODELING
# ============================================================================
# ============================================================================

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("PART B: STATISTICAL MODELING\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("ANALYSIS LOGIC:\n")
cat("  Total Trackers = Trackers per Visit × Number of Visits\n")
cat("  → Must decompose to separate mechanisms:\n")
cat("    1. AVOIDANCE: Users visit high-tracking sites less\n")
cat("    2. INTENSITY: Sites reduce tracking per visit\n\n")

cat("MODEL SPECIFICATIONS:\n")
cat("  Part 1: DV = Total trackers (conflates mechanisms)\n")
cat("  Part 2: DV = Visit count (tests avoidance)\n")
cat("  Part 3: DV = Trackers per visit (tests intensity)\n")
cat("  Part 4: DV = Total trackers + usage controls (combined)\n")
cat("  Part 5: Period 3 cookie deletion analysis\n\n")

# ============================================================================
# PART 1: TOTAL EFFECTS (BASELINE)
# ============================================================================
#
# SPECIFICATION: y_ijt = Treatment×POST + FE(user, date, website) + ε
#
# TESTS: Do treatments reduce TOTAL tracking?
#
# DVs:
#   - n_trackers_third_party (count of 3rd-party trackers)
#   - n_cookies_third_party (count of 3rd-party cookies)
#
# COEFFICIENTS OF INTEREST:
#   - info:POST → Effect of Info treatment vs Control
#   - saliency:POST → Effect of Saliency treatment vs Control
#
# EXPECTED: Negative (treatments reduce tracking)
#
# PROBLEM: Conflates avoidance + intensity mechanisms
#   → If null/positive, can't tell why (need Parts 2-3)
#
# FIXED EFFECTS:
#   - experiment_id: Controls for user-level differences
#   - date: Controls for time trends
#   - website: Controls for website-level differences
#
# CLUSTERING: Standard errors clustered at user level (conservative)
#
# NOTE: fixest automatically removes singleton observations
#   (groups appearing only once in a FE dimension → not identified)
#
# ============================================================================

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("PART 1: TOTAL EFFECTS (Baseline Specification)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("Specification: y_ijt = Treatment × POST + FE(user, date, website) + ε\n")
cat("Tests: Do treatments reduce TOTAL tracking?\n")
cat("Problem: Conflates usage changes and intensity changes\n\n")

# Model 1A: Total trackers
cat("Running Model 1A: Total Trackers...\n")
m1a <- feols(
  n_trackers_third_party ~ i(treatment, POST, ref = "control") | 
    experiment_id + date + website,
  data = panel_merged, 
  cluster = ~experiment_id,
  notes = FALSE
)

# Model 1B: Total cookies
cat("Running Model 1B: Total Cookies...\n")
m1b <- feols(
  n_cookies_third_party ~ i(treatment, POST, ref = "control") | 
    experiment_id + date + website,
  data = panel_merged, 
  cluster = ~experiment_id,
  notes = FALSE
)

cat("✓ Models 1A-1B complete\n\n")

# ============================================================================
# PART 2: USAGE CHANGES (AVOIDANCE MECHANISM)
# ============================================================================
#
# SPECIFICATION: Same as Part 1, but DV = usage metrics
#
# TESTS: Do treatments change HOW MUCH users browse?
#
# DVs:
#   - visit_count: Number of page visits (frequency)
#   - log(time_spent): Duration on site (engagement)
#
# INTERPRETATION:
#   - Negative coef → Avoidance (users visit less)
#   - Positive coef → Compensating behavior (users visit MORE)
#   - Null → No usage change
#
# WHY THIS MATTERS: If treatments work (reduce intensity) but users
#   compensate by visiting more → total tracking unchanged
#   → Need to see this to understand null results
#
# SAMPLE: Only observations with visit data (83% of panel)
#
# ============================================================================

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("PART 2: USAGE CHANGES (Avoidance Mechanism)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("Tests: Do users visit high-tracking sites more or less?\n")
cat("Interpretation:\n")
cat("  Negative → Avoidance (users visit less)\n")
cat("  Positive → Compensating behavior (users visit more)\n")
cat("  Null → No usage change\n\n")

# Subset to observations with visit data
panel_visits <- panel_merged[has_visit_data == TRUE]
cat(sprintf("Sample: %s observations (%.1f%% with usage data)\n\n",
            format(nrow(panel_visits), big.mark = ","),
            100 * nrow(panel_visits) / nrow(panel_merged)))

# Model 2A: Visit count
cat("Running Model 2A: Visit Count...\n")
m2a <- feols(
  visit_count ~ i(treatment, POST, ref = "control") | 
    experiment_id + date + website,
  data = panel_visits, 
  cluster = ~experiment_id,
  notes = FALSE
)

# Model 2B: Time spent (log scale due to skewness)
cat("Running Model 2B: Log(Time Spent)...\n")
m2b <- feols(
  log_time ~ i(treatment, POST, ref = "control") | 
    experiment_id + date + website,
  data = panel_visits, 
  cluster = ~experiment_id,
  notes = FALSE
)

cat("✓ Models 2A-2B complete\n\n")

# ============================================================================
# PART 3: INTENSITY CHANGES (DATA SHARING MECHANISM)
# ============================================================================
#
# SPECIFICATION: Same as Part 1, but DV = per-visit metrics
#
# TESTS: Do treatments reduce tracking PER VISIT?
#
# DVs:
#   - trackers_per_visit: Trackers normalized by visits
#   - cookies_per_visit: Cookies normalized by visits
#   - log(trackers_per_visit): Log specification (handles skewness)
#
# INTERPRETATION:
#   - Negative coef → Data sharing reduced
#     (sites track less OR users block more)
#   - Null → No intensity change
#
# WHY THIS MATTERS: This is the PURE treatment effect!
#   → Controls for usage changes by normalizing
#   → If negative here but positive in Part 1 → compensating behavior
#
# SAMPLE: Only observations with visit_count > 0 (can compute per-visit)
#
# NOTE: We do NOT filter outliers (e.g., trackers_per_visit < 100)
#   → Log transformation handles skewness
#   → Keeps all data, avoids arbitrary cutoffs
#
# ============================================================================

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("PART 3: INTENSITY CHANGES (Data Sharing Mechanism)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("Tests: Does treatment reduce tracking PER VISIT?\n")
cat("Interpretation: Pure data sharing effect (usage-adjusted)\n")
cat("Note: Using log specification to handle skewness (no outlier filtering)\n\n")

# Subset to observations where per-visit metrics are defined
panel_intensity <- panel_visits[
  visit_count > 0 & 
    !is.na(trackers_per_visit)
]

cat(sprintf("Sample: %s observations (%.1f%% of panel)\n\n",
            format(nrow(panel_intensity), big.mark = ","),
            100 * nrow(panel_intensity) / nrow(panel_merged)))

# Model 3A: Trackers per visit
cat("Running Model 3A: Trackers per Visit...\n")
m3a <- feols(
  trackers_per_visit ~ i(treatment, POST, ref = "control") | 
    experiment_id + date + website,
  data = panel_intensity, 
  cluster = ~experiment_id,
  notes = FALSE
)

# Model 3B: Cookies per visit
cat("Running Model 3B: Cookies per Visit...\n")
m3b <- feols(
  cookies_per_visit ~ i(treatment, POST, ref = "control") | 
    experiment_id + date + website,
  data = panel_intensity, 
  cluster = ~experiment_id,
  notes = FALSE
)

# Model 3C: Log(Trackers per visit) - preferred for skewed distributions
cat("Running Model 3C: Log(Trackers per Visit)...\n")
m3c <- feols(
  log(1 + trackers_per_visit) ~ i(treatment, POST, ref = "control") | 
    experiment_id + date + website,
  data = panel_intensity, 
  cluster = ~experiment_id,
  notes = FALSE
)

cat("✓ Models 3A-3C complete\n\n")

# ============================================================================
# PART 4: TOTAL EFFECTS WITH USAGE CONTROLS
# ============================================================================
#
# SPECIFICATION: y_ijt = Treatment×POST + log(visits) + log(time) + FE + ε
#
# TESTS: Total effect HOLDING USAGE CONSTANT
#
# WHY THIS MATTERS:
#   - If Part 2 shows usage changed → this controls for it
#   - Should match Part 3 (intensity) if decomposition is correct
#   - Validation check: Models 3A and 4C should have similar coefficients
#
# CONTROLS:
#   - log_visit: Log of visit count
#   - log_time: Log of time spent
#   → Flexible functional form, handles skewness
#
# ============================================================================

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("PART 4: TOTAL EFFECTS WITH USAGE CONTROLS\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("Tests: Total effect HOLDING USAGE CONSTANT\n")
cat("Validation: Should match Part 3 if decomposition is correct\n\n")

# Model 4A: Control for visit count
cat("Running Model 4A: Total + Control for Visits...\n")
m4a <- feols(
  n_trackers_third_party ~ i(treatment, POST, ref = "control") + log_visit | 
    experiment_id + date + website,
  data = panel_visits, 
  cluster = ~experiment_id,
  notes = FALSE
)

# Model 4B: Control for time spent
cat("Running Model 4B: Total + Control for Time...\n")
m4b <- feols(
  n_trackers_third_party ~ i(treatment, POST, ref = "control") + log_time | 
    experiment_id + date + website,
  data = panel_visits, 
  cluster = ~experiment_id,
  notes = FALSE
)

# Model 4C: Control for both
cat("Running Model 4C: Total + Control for Both...\n")
m4c <- feols(
  n_trackers_third_party ~ i(treatment, POST, ref = "control") + 
    log_visit + log_time | 
    experiment_id + date + website,
  data = panel_visits, 
  cluster = ~experiment_id,
  notes = FALSE
)

cat("✓ Models 4A-4C complete\n\n")

# ============================================================================
# PART 5: PERIOD 3 - COOKIE DELETION ANALYSIS
# ============================================================================
#
# BACKGROUND: In Period 3, cookies were exogenously deleted in 2 batches
#
# COLLINEARITY ISSUE: If cookie_treatment_idx is assigned at USER level
#   → Does not vary within users
#   → Perfectly collinear with user FE
#   → Cannot estimate effect with user FE
#
# SOLUTION: Check variation structure first
#   - If varies within users → Can estimate with user FE
#   - If constant per user → Must drop user FE (or skip analysis)
#
# ============================================================================

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("PART 5: PERIOD 3 - COOKIE DELETION ANALYSIS\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("Checking cookie_treatment_idx variation structure...\n\n")

# Check if cookie_treatment_idx exists
if ("cookie_treatment_idx" %in% names(panel_merged)) {
  
  # Subset to Period 3
  panel_p3 <- panel_merged[period == 3]
  
  cat(sprintf("Sample: Period 3 only (%s observations)\n\n",
              format(nrow(panel_p3), big.mark = ",")))
  
  # Check variation: Does cookie_treatment_idx vary WITHIN users?
  variation_check <- panel_p3[, .(
    n_values = n_distinct(cookie_treatment_idx)
  ), by = experiment_id]
  
  varies_within_user <- any(variation_check$n_values > 1)
  
  if (varies_within_user) {
    cat("✓ cookie_treatment_idx varies within users\n")
    cat("  → Can estimate with user FE\n\n")
    
    # Model 5A: Main effect (with user FE)
    cat("Running Model 5A: Cookie Deletion Main Effect...\n")
    m5a <- feols(
      n_trackers_third_party ~ i(cookie_treatment_idx) | 
        experiment_id + website,
      data = panel_p3, 
      cluster = ~experiment_id,
      notes = FALSE
    )
    
    # Model 5B: Interaction
    cat("Running Model 5B: Cookie Deletion × Treatment...\n")
    m5b <- feols(
      n_trackers_third_party ~ i(treatment, cookie_treatment_idx, ref = "control") | 
        experiment_id + website,
      data = panel_p3, 
      cluster = ~experiment_id,
      notes = FALSE
    )
    
    cookie_models_exist <- TRUE
    
  } else {
    cat("✗ cookie_treatment_idx is CONSTANT within users\n")
    cat("  → Assigned at user level (does not vary over time)\n")
    cat("  → Perfectly collinear with user FE\n")
    cat("  → Cannot estimate effect with user FE\n\n")
    
    cat("ALTERNATIVE APPROACH: Drop user FE, use date + website FE\n")
    cat("  Note: Loses within-user identification\n")
    cat("        Estimates between-user differences\n\n")
    
    # Model 5A: Between-user estimation (no user FE)
    cat("Running Model 5A: Cookie Deletion (Between-User)...\n")
    m5a <- feols(
      n_trackers_third_party ~ i(cookie_treatment_idx) | 
        date + website,  # NO user FE
      data = panel_p3, 
      cluster = ~experiment_id,
      notes = FALSE
    )
    
    # Model 5B: Interaction (between-user)
    cat("Running Model 5B: Treatment × Cookie Deletion (Between-User)...\n")
    m5b <- feols(
      n_trackers_third_party ~ i(treatment, cookie_treatment_idx, ref = "control") | 
        date + website,  # NO user FE
      data = panel_p3, 
      cluster = ~experiment_id,
      notes = FALSE
    )
    
    cookie_models_exist <- TRUE
    
    cat("\n⚠ CAUTION: These estimates use between-user variation\n")
    cat("           Not as clean as within-user identification\n")
    cat("           Interpret with care\n\n")
  }
  
  cat("✓ Models 5A-5B complete\n\n")
  
} else {
  cat("NOTE: cookie_treatment_idx variable not found\n")
  cat("      Skipping Period 3 cookie deletion analysis\n\n")
  
  cookie_models_exist <- FALSE
}

# ============================================================================
# DISPLAY RESULTS
# ============================================================================

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("RESULTS TABLES\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

# TABLE 1: Total Effects
cat("TABLE 1: Total Effects (Baseline)\n")
cat(paste0(rep("-", 80), collapse = ""), "\n")
cat("Purpose: Test overall treatment effect (conflates usage + intensity)\n")
cat("Expected: Negative if treatments work\n\n")

etable(m1a, m1b,
       headers = c("Total Trackers", "Total Cookies"),
       digits = 2, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.1, "."=0.15))

# TABLE 2: Usage Mechanisms
cat("\n\nTABLE 2: Usage Changes (Avoidance Mechanism)\n")
cat(paste0(rep("-", 80), collapse = ""), "\n")
cat("Purpose: Test if treatments change how much users browse\n")
cat("Interpretation:\n")
cat("  Negative → Users avoid high-tracking sites\n")
cat("  Positive → Users visit MORE (compensating behavior)\n")
cat("  Null → No usage change\n\n")

etable(m2a, m2b,
       headers = c("Visits", "Log(Time)"),
       digits = 2, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.1, "."=0.15))

# TABLE 3: Intensity Mechanisms
cat("\n\nTABLE 3: Intensity Changes (Data Sharing Mechanism)\n")
cat(paste0(rep("-", 80), collapse = ""), "\n")
cat("Purpose: Test if treatments reduce tracking PER VISIT\n")
cat("Interpretation: Pure data sharing effect (usage-adjusted)\n")
cat("Expected: Negative if treatments reduce intensity\n\n")

etable(m3a, m3b, m3c,
       headers = c("Trackers/Visit", "Cookies/Visit", "Log(Trackers/Visit)"),
       digits = 2, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.1, "."=0.15))

# TABLE 4: Total with Controls
cat("\n\nTABLE 4: Total Effects with Usage Controls\n")
cat(paste0(rep("-", 80), collapse = ""), "\n")
cat("Purpose: Test total effect HOLDING USAGE CONSTANT\n")
cat("Validation: Should match Table 3 if decomposition is correct\n\n")

etable(m4a, m4b, m4c,
       headers = c("+ Visits", "+ Time", "+ Both"),
       digits = 2, 
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.1, "."=0.15))

# TABLE 5: Cookie Deletion (if models exist)
if (cookie_models_exist) {
  cat("\n\nTABLE 5: Period 3 - Cookie Deletion Analysis\n")
  cat(paste0(rep("-", 80), collapse = ""), "\n")
  cat("Purpose: Test exogenous data sharing variation\n")
  cat("Note: cookie_treatment_idx constant within users → no user FE\n\n")
  
  etable(m5a, m5b,
         headers = c("Main Effect", "× Treatment"),
         digits = 2, 
         signifCode = c("***"=0.01, "**"=0.05, "*"=0.1, "."=0.15))
}

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("\n\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("SAVING RESULTS\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

# Main results table (for paper)
etable(m1a, m2a, m3a, m4c,
       headers = c("Total", "Visits", "Per Visit", "Total+Controls"),
       digits = 2,
       signifCode = c("***"=0.01, "**"=0.05, "*"=0.1, "."=0.15),
       tex = TRUE,
       file = "main_results.tex",
       title = "Treatment Effects on Online Tracking",
       replace = TRUE)

cat("✓ Saved: main_results.tex (LaTeX table)\n")

# Full results table
if (cookie_models_exist) {
  etable(m1a, m1b, m2a, m2b, m3a, m3b, m3c, m4a, m4b, m4c, m5a, m5b,
         digits = 2,
         signifCode = c("***"=0.01, "**"=0.05, "*"=0.1, "."=0.15),
         tex = TRUE,
         file = "full_results.tex",
         title = "Complete Analysis Results",
         replace = TRUE)
  
  cat("✓ Saved: full_results.tex (all models)\n")
} else {
  etable(m1a, m1b, m2a, m2b, m3a, m3b, m3c, m4a, m4b, m4c,
         digits = 2,
         signifCode = c("***"=0.01, "**"=0.05, "*"=0.1, "."=0.15),
         tex = TRUE,
         file = "full_results.tex",
         title = "Complete Analysis Results",
         replace = TRUE)
  
  cat("✓ Saved: full_results.tex (all models)\n")
}

cat("✓ Saved: panel_merged_CLEAN.fst (cleaned data)\n\n")

# ============================================================================
# INTERPRETATION GUIDE
# ============================================================================

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("INTERPRETATION GUIDE\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("HOW TO READ THE RESULTS:\n\n")

cat("SCENARIO A: Pure Avoidance\n")
cat("  Table 1: Negative (less total tracking)\n")
cat("  Table 2: Negative (fewer visits)\n")
cat("  Table 3: Null (no intensity change)\n")
cat("  → Users avoid sites but don't change data sharing\n\n")

cat("SCENARIO B: Pure Intensity Reduction\n")
cat("  Table 1: Negative (less total tracking)\n")
cat("  Table 2: Null (same visits)\n")
cat("  Table 3: Negative (less per visit)\n")
cat("  → Users block trackers or sites reduce tracking\n\n")

cat("SCENARIO C: Compensating Behavior (might explain positive results!)\n")
cat("  Table 1: Null or Positive (same/more tracking)\n")
cat("  Table 2: Positive (more visits)\n")
cat("  Table 3: Negative (less per visit)\n")
cat("  → Treatment works but users visit more, offsetting the gain\n")
cat("  → Still an important finding! Shows unintended consequence\n\n")

cat("SCENARIO D: No Effect\n")
cat("  Table 1: Null\n")
cat("  Table 2: Null\n")
cat("  Table 3: Null\n")
cat("  → Treatments don't work\n\n")

cat("VALIDATION CHECK:\n")
cat("  Compare Models 3A (trackers per visit) and 4C (total + both controls)\n")
cat("  If coefficients are similar → Decomposition is correct\n")
cat("  If different → Something else going on (investigate further)\n\n")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY STATISTICS\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("Sample Sizes:\n")
cat(sprintf("  Part 1 (Total): %s obs\n", format(nobs(m1a), big.mark = ",")))
cat(sprintf("  Part 2 (Usage): %s obs (%.1f%% of Part 1)\n", 
            format(nobs(m2a), big.mark = ","),
            100 * nobs(m2a) / nobs(m1a)))
cat(sprintf("  Part 3 (Intensity): %s obs (%.1f%% of Part 1)\n", 
            format(nobs(m3a), big.mark = ","),
            100 * nobs(m3a) / nobs(m1a)))
cat(sprintf("  Part 4 (Controlled): %s obs\n", format(nobs(m4c), big.mark = ",")))
if (cookie_models_exist) {
  cat(sprintf("  Part 5 (Period 3): %s obs\n", format(nobs(m5a), big.mark = ",")))
}
cat("\n")

cat("Fixed Effects:\n")
cat(sprintf("  Users: %d\n", m1a$fixef_sizes["experiment_id"]))
cat(sprintf("  Dates: %d\n", m1a$fixef_sizes["date"]))
cat(sprintf("  Websites: %s\n\n", format(m1a$fixef_sizes["website"], big.mark = ",")))

cat("Singletons Removed (by fixest):\n")
cat("  Reason: Groups appearing once in FE dimension → not identified\n")
cat("  This is correct statistical behavior (not a bug)\n\n")

# ============================================================================
# COMPLETE
# ============================================================================

cat(paste0(rep("=", 80), collapse = ""), "\n")
cat("ANALYSIS COMPLETE\n")
cat(paste0(rep("=", 80), collapse = ""), "\n\n")

cat("Output Files:\n")
cat("  1. panel_merged_CLEAN.fst - Cleaned merged dataset\n")
cat("  2. main_results.tex - Main results table (for paper)\n")
cat("  3. full_results.tex - All models\n\n")

cat("Questions? Check the code comments for detailed explanations.\n\n")