# =============================================================================
# EXPERIMENTAL BALANCE TABLE (balance_table)
# =============================================================================
#
# Produces:
#   Appendix C [tab:exp_balance_table, "Experimental Balance Tables"]:
#     output/tables/exp_balance_table.tex
#       Full 5-column latex tabular (Control mean / Saliency mean / Info mean
#       / p(S vs C) / p(I vs C)) for 21 baseline characteristics. Paper
#       loads this directly via \input{./output/tables/exp_balance_table}.
#
# Scope:
#   R port of the balance check from
#   code_github/replication_files/balance_table/assign_groups_wave_two.py.
#   Reads the existing final treatment assignments from experiment_conditions
#   (we do NOT redo randomization), pools wave 1 and wave 2 participants,
#   and computes per-treatment means + ANOVA p-values for the 21 rows that
#   make up Appendix C.
#
# Rows reported (21):
#   Time-use (from enriched_time_data_2_wave_2.csv):
#     1. Total Hours on Sites with Privacy Info
#     2. Daily Hours on Sites with Privacy Info
#     3. Total Hours Spent Online
#   Demographics (from survey_full_eligible.csv):
#     4. Male
#     5. College Graduate
#     6. Age
#     7. Income
#     8. No Income Disclosure
#     9. Is White
#   Privacy attitudes and stated preferences:
#     10. Privacy Attitude Index
#     11. Privacy Knowledge
#     12. PEW Extensive Margin
#     13. WTA
#   Mean beliefs (computed in pipeline upstream):
#     14. Mean Beliefs (Use)
#     15. Mean Beliefs (Collect)
#     16. Mean Beliefs (Control)
#     17. Mean Beliefs (Quality)
#   Conjoint category indicators:
#     18. Conjoint category (News)
#     19. Conjoint category (Entertainment)
#     20. Conjoint category (E-commerce)
#     21. Conjoint category (Social)
#
# Sample:
#   1596 treatment-assigned participants:
#     - 532 control + 532 info + 532 saliency
#     - filter: experiment_condition != ""
#     - exclude: in_experiment %in% c("testing", "Removed due to grounding")
#   Paper reports 1597. Off-by-one is the one info-arm participant with
#   in_experiment == "Removed due to grounding" that we drop. This affects
#   the info column N by 1 but is invisible at the rounded precision of the
#   table.
#
# Data sources:
#   ../data/processed_data/enriched_time_data_2_wave_2.csv
#     CUMULATIVE time-use file (already contains all wave_1 data; do NOT also
#     read enriched_time_data_2_wave_1.csv or rows are double-counted -- this
#     was the source of a 27% overshoot in an earlier iteration).
#
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#     Final treatment assignments. Read experiment_condition directly; do
#     NOT redo the iterative balance check from assign_groups_wave_two.py.
#
#   ../onboarding_sequence_scripts/qualtrics_csvs/survey_full_eligible.csv
#     Pre-cleaned survey table with cleaned_* fields and conjoint category
#     dummies. Joined on tolower(email).
#
# Cleaning rules (matched to Python except where noted):
#   Time data:
#     - Sort by (experiment_id, date)
#     - Drop each user's earliest date (extension install / config day)
#     - Drop survey/recruitment websites by splitting the website on "."
#       and checking any component against SURVEY_WEBSITES
#     - Inner-merge participants on experiment_id (drops time-use for
#       non-assigned users)
#     - Wave-specific tstamp filter:
#         wave_id == 1 : (1749873600, 1751083200)  -- Jun 13 - Jun 27 2025
#         wave_id != 1 : (1751083200, 1752282000)  -- Jun 27 - Jul 11 2025
#       wave 3 participants use the wave 2 timestamp window (Python script
#       applies wave_id != 1, which sweeps wave 3 in).
#
#   Survey:
#     - tolower(trimws(email)) on both sides
#     - Drop duplicates on email (keep first)
#
#   Variable bugs preserved from Python (Stage 1 replication):
#     - is_male uses the buggy cleaned column where PNA -> 0 (counts as
#       not-male). Paper's reported 0.378 / 0.385 / 0.402 already reflect
#       this bug.
#     - cleaned_WTA is NOT trimmed for outliers (paper WTA means ~19, which
#       matches the un-trimmed column).
#
# avg_daily_time formula:
#   Paper / Python:
#     avg_daily_time = total_time_spent_privacy / ((today - first_active) + 1)
#   This depends on the date the script was run (today()) -- not reproducible.
#   We replace with the deterministic baseline-period length:
#     avg_daily_time = total_time_spent_privacy / 14
#   This makes our Daily Hours row read 1.279 / 1.268 / 1.293 instead of the
#   paper's 1.150 / 1.148 / 1.155. The 11% gap is entirely due to today()
#   in Python evaluating to a date about 15.6 days after each user's first
#   active day on average, rather than the intended 14-day baseline window.
#
# Numbers vs. paper (writeup_v3.tex Appendix C):
#   18 of 21 rows match paper to 0.001 and ANOVA p-values match to 0.001.
#   3 time-use rows differ:
#     Total Hours on Sites w/ Privacy Info  17.91 vs paper 17.906   match
#     Total Hours Spent Online              28.31 vs paper 28.311   match
#     Daily Hours on Sites w/ Privacy Info  1.279 vs paper 1.150    fixed (today bug)
#   social_conjoint_cat p(S-C) = 1.000 in our run vs paper "nan"
#     Both reflect identical means in S and C (0.237 vs 0.237). Python's
#     f_oneway returns NaN when between-group variance is 0; R's
#     oneway.test returns p = 1.0. The R value is technically correct;
#     the Python NaN is an edge-case artifact.
#
# ANOVA:
#   oneway.test(y ~ group, var.equal = TRUE) is numerically equivalent to
#   scipy.stats.f_oneway. Verified against Python output to 0.001.
#
# Inputs:
#   ../data/processed_data/enriched_time_data_2_wave_2.csv
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#   ../onboarding_sequence_scripts/qualtrics_csvs/survey_full_eligible.csv
#
# Dependencies:
#   tidyverse, data.table, lubridate, fixest (for esttex-like output)
#
# Outputs:
#   output/tables/exp_balance_table.tex
#
# Required edits to writeup_v3.tex (one-time):
#   1. Remove the \begin{comment} / \end{comment} surrounding the
#      tab:exp_balance_table block in Appendix C.
#   2. Replace the hand-written tabular with a single line:
#        \input{./output/tables/exp_balance_table}
#      keeping the surrounding \begin{table}, \caption{}, \label{}, and
#      \caption*{Notes} environment intact.
# =============================================================================

library(tidyverse)
library(data.table)
library(lubridate)

setwd("~/Dropbox/spring2025experiment/code_github")

DATA_DIR    <- "../data/"
PROC_DIR    <- paste0(DATA_DIR, "processed_data/")
EXT_DIR     <- paste0(DATA_DIR, "final_extension_data/")
SURVEY_PATH <- "../onboarding_sequence_scripts/qualtrics_csvs/survey_full_eligible.csv"
TABLES_DIR  <- "output/tables/"

# Survey websites to drop (Python list, verbatim)
SURVEY_WEBSITES <- c(
  "qualtrics", "vercel", "respondent", "cmix", "raterproject", "questionpro",
  "usertesting", "netlify", "primeopinion", "paidviewpoint", "surveymonkey",
  "surveymeasure", "alchemer", "yougov", "prolific", "mturk", "cloudresearch",
  "guyaridor", "chromewebstore"
)

# Python wave timestamps (verbatim from assign_groups_wave_two.py)
START_TSTAMP_WAVE_ONE <- 1749873600
END_TSTAMP_WAVE_ONE   <- 1751083200
START_TSTAMP_WAVE_TWO <- 1751083200
END_TSTAMP_WAVE_TWO   <- 1752282000

# Python-equivalent is_survey_site: split URL on "." and check parts
is_survey_site <- function(website) {
  if (is.na(website) || website == "") return(FALSE)
  parts <- strsplit(tolower(website), ".", fixed = TRUE)[[1]]
  any(parts %in% SURVEY_WEBSITES)
}

# =============================================================================
# Load experiment_conditions, build participant list (1596 all-assigned)
# =============================================================================
exp_cond <- fread(paste0(EXT_DIR, "experiment_conditions_pilot_july_2024.csv"))

participants <- exp_cond %>%
  filter(experiment_condition != "" &
           !in_experiment %in% c("testing", "Removed due to grounding")) %>%
  mutate(email_lower = tolower(trimws(email)),
         experiment_id = sprintf("%07s", as.character(experiment_id))) %>%
  select(experiment_id, email_lower, experiment_condition, wave_id)

cat("=== Participants ===\n")
print(table(participants$experiment_condition))
cat("Total N:", nrow(participants),
    "  (paper: 1597; we drop 1 'Removed due to grounding' info user)\n\n")

# =============================================================================
# Load time data (wave_2.csv is cumulative -- do NOT rbind with wave_1.csv)
# =============================================================================
time_data <- fread(paste0(PROC_DIR, "enriched_time_data_2_wave_2.csv"))

cat("=== Time data pipeline ===\n")
cat("Raw rows:", nrow(time_data), "\n")

# Zero-pad experiment_id, parse date as m/d/Y
time_data[, experiment_id := sprintf("%07s", as.character(experiment_id))]
time_data[, date := lubridate::mdy(date)]
time_data <- time_data[order(experiment_id, date)]

# Drop each user's earliest date (Python: transform('min') + filter date != min)
time_data[, min_date := min(date, na.rm = TRUE), by = experiment_id]
time_data <- time_data[date != min_date]
cat("After drop min date:", nrow(time_data), "\n")

# Drop SURVEY_WEBSITES via Python-equivalent dot-split check
time_data[, is_survey := sapply(website, is_survey_site)]
time_data <- time_data[is_survey == FALSE]
cat("After drop survey websites:", nrow(time_data), "\n")

# Inner merge with participants (drops rows for non-assigned users)
time_data <- merge(time_data,
                   participants[, .(experiment_id, wave_id_p = wave_id)],
                   by = "experiment_id", all = FALSE)
cat("After merge with participants:", nrow(time_data), "\n")

# Wave-specific tstamp filter (wave 3 sweeps in under "wave_id != 1")
time_data[, tstamp := as.numeric(tstamp)]
time_data <- time_data[
  (wave_id_p == 1 & tstamp > START_TSTAMP_WAVE_ONE & tstamp < END_TSTAMP_WAVE_ONE) |
    (wave_id_p != 1 & tstamp > START_TSTAMP_WAVE_TWO & tstamp < END_TSTAMP_WAVE_TWO)
]
cat("After wave-specific tstamp filter:", nrow(time_data), "\n\n")

# =============================================================================
# Compute 3 time-use vars per participant
# =============================================================================
total_overall <- time_data[, .(total_time_spent_overall = sum(time_spent) / 3600),
                           by = experiment_id]

privacy_data <- time_data[privacy_exist == TRUE]
total_privacy <- privacy_data[, .(total_time_spent = sum(time_spent) / 3600),
                              by = experiment_id]

# Deterministic baseline-period denominator (fixes Python's today() bug)
total_privacy[, avg_daily_time := total_time_spent / 14]

participants_w_time <- participants %>%
  left_join(total_overall,  by = "experiment_id") %>%
  left_join(total_privacy,  by = "experiment_id") %>%
  mutate(total_time_spent_overall = replace_na(total_time_spent_overall, 0),
         total_time_spent         = replace_na(total_time_spent, 0),
         avg_daily_time           = replace_na(avg_daily_time, 0))

cat("=== Time-use coverage ===\n")
cat("  Users with positive total_time_spent:",
    sum(participants_w_time$total_time_spent > 0), "/",
    nrow(participants_w_time), "\n\n")

# =============================================================================
# Merge with survey_full_eligible.csv on tolower(email)
# =============================================================================
elig <- fread(SURVEY_PATH)
elig[, email_lower := tolower(trimws(email))]
elig_dedup <- elig[!duplicated(elig$email_lower)]

merged <- participants_w_time %>%
  left_join(elig_dedup, by = "email_lower") %>%
  filter(!is.na(total_time_spent) & !is.na(experiment_condition))

cat("=== Final merged sample ===\n")
print(table(merged$experiment_condition))
cat("\n")

# =============================================================================
# 21 balance rows
# =============================================================================
# Internal variable name -> paper display label
row_specs <- tibble::tribble(
  ~var,                          ~label,
  "total_time_spent",            "Total Hours on Sites with Privacy Info",
  "avg_daily_time",              "Daily Hours on Sites with Privacy Info",
  "total_time_spent_overall",    "Total Hours Spent Online",
  "is_male",                     "Male",
  "college_graduate",            "College Graduate",
  "cleaned_age",                 "Age",
  "cleaned_income",              "Income",
  "not_disclosing_income",       "No Income Disclosure",
  "is_white",                    "Is White",
  "privacy_attitude_index",      "Privacy Attitude Index",
  "privacy_knowledge",           "Privacy Knowledge",
  "privacy_extensive_margin",    "PEW Extensive Margin",
  "cleaned_WTA",                 "WTA",
  "beliefs_use",                 "Mean Beliefs (Use)",
  "beliefs_collection",          "Mean Beliefs (Collect)",
  "beliefs_control",             "Mean Beliefs (Control)",
  "beliefs_quality",             "Mean Beliefs (Quality)",
  "news_conjoint_cat",           "Conjoint category (News)",
  "entertainment_conjoint_cat",  "Conjoint category (Entertainment)",
  "ecom_conjoint_cat",           "Conjoint category (E-commerce)",
  "social_conjoint_cat",         "Conjoint category (Social)"
)

# Compute means + 2 ANOVA p-values per row
compute_row <- function(v) {
  if (!v %in% names(merged)) {
    return(c(ctrl = NA, sal = NA, info = NA, p_sc = NA, p_ic = NA))
  }
  ctrl <- as.numeric(merged %>% filter(experiment_condition == "control")  %>% pull(!!v))
  sal  <- as.numeric(merged %>% filter(experiment_condition == "saliency") %>% pull(!!v))
  info <- as.numeric(merged %>% filter(experiment_condition == "info")     %>% pull(!!v))
  
  ctrl <- ctrl[!is.na(ctrl)]
  sal  <- sal[!is.na(sal)]
  info <- info[!is.na(info)]
  
  m_ctrl <- if (length(ctrl) > 0) mean(ctrl) else NA
  m_sal  <- if (length(sal)  > 0) mean(sal)  else NA
  m_info <- if (length(info) > 0) mean(info) else NA
  
  p_sc <- tryCatch({
    df <- data.frame(y = c(ctrl, sal),
                     g = c(rep("c", length(ctrl)), rep("s", length(sal))))
    oneway.test(y ~ g, data = df, var.equal = TRUE)$p.value
  }, error = function(e) NA)
  
  p_ic <- tryCatch({
    df <- data.frame(y = c(ctrl, info),
                     g = c(rep("c", length(ctrl)), rep("i", length(info))))
    oneway.test(y ~ g, data = df, var.equal = TRUE)$p.value
  }, error = function(e) NA)
  
  c(ctrl = m_ctrl, sal = m_sal, info = m_info, p_sc = p_sc, p_ic = p_ic)
}

results <- t(sapply(row_specs$var, compute_row))
results_df <- as.data.frame(results) %>%
  mutate(label = row_specs$label) %>%
  select(label, ctrl, sal, info, p_sc, p_ic)

cat("=== Computed balance table ===\n")
print(results_df, digits = 4)
cat("\n")

# =============================================================================
# Build LaTeX tabular and write to output/tables/exp_balance_table.tex
# =============================================================================
# Formatting rules per row -- match Python's "{:.3f}" everywhere except Income.
fmt_cell <- function(label, val) {
  if (is.na(val)) return("--")
  # Income gets one decimal; everything else 3 decimals like Python's :.3f.
  if (label == "Income") return(formatC(val, format = "f", digits = 3))
  formatC(val, format = "f", digits = 3)
}

build_row <- function(i) {
  r <- results_df[i, ]
  paste(
    r$label, "&",
    fmt_cell(r$label, r$ctrl), "&",
    fmt_cell(r$label, r$sal),  "&",
    fmt_cell(r$label, r$info), "&",
    fmt_cell(r$label, r$p_sc), "&",
    fmt_cell(r$label, r$p_ic), "\\\\\n\\hline"
  )
}

body_rows <- sapply(seq_len(nrow(results_df)), build_row)
body <- paste(body_rows, collapse = "\n")

latex <- paste0(
  "\\scalebox{0.8}{\n",
  "\\begin{tabular}{|l|c|c|c|c|c|}\n",
  "\\hline\n",
  " & Control Mean & Saliency Mean & Information Mean & p (S vs C) & p (I vs C) \\\\\n",
  "\\hline\n",
  body, "\n",
  "\\end{tabular}\n",
  "}\n"
)

# Make sure tables/ exists
if (!dir.exists(TABLES_DIR)) dir.create(TABLES_DIR, recursive = TRUE)

out_path <- file.path(TABLES_DIR, "exp_balance_table.tex")
writeLines(latex, out_path)
cat("Saved:", out_path, "\n\n")

cat("Required edits to writeup_v3.tex (Appendix C):\n")
cat("  1. Remove \\begin{comment} / \\end{comment} around tab:exp_balance_table\n")
cat("  2. Replace hand-written tabular with:\n")
cat("       \\input{./output/tables/exp_balance_table}\n")
cat("  3. Keep surrounding \\begin{table}, \\caption{}, \\label{}, and notes\n")