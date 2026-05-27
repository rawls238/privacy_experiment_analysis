# =============================================================================
# SURVEY DESCRIPTIVE SCALARS (survey_analysis)
# =============================================================================
#
# Produces:
#   Section 4 inline scalars [main paper prose, sample descriptives + WTA]:
#     output/values/survey_descriptive_values.tex
#       10 \newcommand macros:
#
#       Demographics (writeup_v3.tex Section 4.5 paragraph):
#         \surveyMeanAge              -- mean age of the survey sample
#         \surveyFemalePct            -- pct female (denom = full survey N)
#         \surveyMalePct              -- pct male   (denom = full survey N)
#         \surveyPnaPct               -- pct "Prefer not to answer" for gender
#         \surveyCollegePct           -- pct with at least a college education
#
#       WTA descriptives (writeup_v3.tex Section 4.6 selection paragraph):
#         \wtaInvitedMedian           -- median WTA, invited (eligible) sample
#         \wtaInvitedMean             -- mean   WTA, invited (eligible) sample
#         \wtaFullMedian              -- median WTA, full survey population
#         \wtaFullMean                -- mean   WTA, full survey population
#         \invitedSampleCoveragePct   -- pct of full survey with WTA <= $40
#
# Sample definitions:
#
#   Baseline completers (denom for demographics + WTA full + coverage):
#     elig %>% filter(!is.na(cleaned_WTA))  -- N = 8168
#
#     survey_full_eligible.csv has 8187 rows but 19 of those are mid-survey
#     dropouts who completed demographics + conjoint but did NOT finish the
#     WTA module. Paper defines "baseline completion" as finishing the full
#     baseline survey including the WTA elicitation, so we filter to the 8168
#     users with non-NA cleaned_WTA. This matches \baselineSurveyN = 8168 in
#     participant_flow_values.tex.
#
#   Invited (eligible) sample (denom for WTA invited):
#     exp %>% filter(presented_offer >= cutoff_wta, in_experiment != "testing")
#     -- N = 2874 (paper "2,874 were eligible for the extension phase")
#
#     Paper Section 4.6 "invited sample" refers to this eligible sample
#     (users whose stated WTA was at or below the random BDM offer, so they
#     were invited to install the extension). NOT the installed (1717),
#     randomized (1597), or finished (1510) sub-samples.
#
# Cleaning rules:
#
#   WTA censor ($1000, applied to mean/median only -- per Guy 2026-05-25):
#     The upstream cleaned_WTA column is type-converted but NOT outlier-
#     trimmed: any string in the Other free-text field (WTA_11_other) is
#     parsed numerically, so raw cleaned_WTA contains protest values up to
#     1e23 from entries like "100000000000000000000000". Censor cleaned_WTA
#     > $1000 -> NA for the mean/median computation. Per Guy's guidance:
#     values like $100, $500, $1000 are plausible high-WTA responses
#     indicating genuine privacy sensitivity, not data entry errors. The
#     > $1000 threshold removes protest entries while preserving informative
#     high-WTA responses. 52 users trimmed from full sample; 19 from invited.
#
#   Coverage (NOT trimmed -- intentional, per Guy 2026-05-25):
#     The invited_sample_coverage_pct reports "fraction of survey takers
#     with WTA <= $40 (the BDM offer cap)". Users with WTA > $1000 are
#     informative here: they represent the most privacy-sensitive tail of
#     the population that our experimental sample fails to cover. Trimming
#     them would understate the selection problem the macro is meant to
#     characterize. Computed on raw cleaned_WTA without trim.
#
#   Gender shares:
#     Use the raw Gender field (character: "Female" / "Male" / "Prefer not to
#     answer") directly. Denominator for all three shares is the full survey
#     N (not the non-PNA N), so the three pcts sum to 100.
#
#     NB. The upstream is_male field (analysis_full.py line 319) treats PNA
#     as is_male = 0, conflating it with female. This script does not use
#     is_male; it reads raw Gender instead.
#
# Numbers vs. paper (writeup_v3.tex Section 4.5 + 4.6):
#   match paper:
#     \surveyMeanAge              39.4 vs paper 39.4
#     \surveyFemalePct            59.9 vs paper 59.9
#     \surveyCollegePct           58.5 vs paper 58.6 (banker's rounding)
#     \wtaInvitedMedian           20   vs paper 20
#     \wtaInvitedMean             20   vs paper 19 (rounding; raw 19.97)
#     \wtaFullMedian              25   vs paper 25
#   new (not in paper):
#     \surveyMalePct              38.8
#     \surveyPnaPct               1.3
#   differ from paper (paper text must be updated):
#     \wtaFullMean                41   vs paper 52
#     \invitedSampleCoveragePct   90.8 vs paper 89
#
# Number formatting:
#   All numeric output is pre-formatted via number_format_helpers.R helpers
#   (format_pct, format_age, format_dollar, format_count). See that file for
#   the full rules. We pre-format because savetexvalue's accuracy parameter
#   is broken in our package version.
#
# Inputs:
#   ../onboarding_sequence_scripts/qualtrics_csvs/survey_full_eligible.csv
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#
# Dependencies:
#   tidyverse
#   savetexvalue (devtools::install_github("Ori-Shoham/savetexvalue"))
#   replication_files/utils/number_format_helpers.R
#
# Outputs:
#   output/values/survey_descriptive_values.tex
# =============================================================================

library(tidyverse)
library(savetexvalue)

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/number_format_helpers.R")

VALUES_DIR <- "output/values/"

# =============================================================================
# Load eligible survey CSV; filter to baseline completers
# =============================================================================

elig <- read.csv("../onboarding_sequence_scripts/qualtrics_csvs/survey_full_eligible.csv",
                 stringsAsFactors = FALSE)
cat("Loaded survey_full_eligible.csv:", nrow(elig), "rows (raw)\n")

# Filter to baseline completers: those who finished the full baseline survey
# including the WTA module. 19 users have cleaned_WTA = NA (mid-survey
# dropout); they are excluded so the denominator matches paper's notion of
# "baseline survey completion" (8168 = \baselineSurveyN).
elig <- elig %>% filter(!is.na(cleaned_WTA))
cat("After filter to baseline completers:", nrow(elig), "rows\n\n")

# =============================================================================
# Demographics (denom = full baseline completers, N = 8168)
# =============================================================================

n_total <- nrow(elig)

survey_mean_age    <- mean(elig$cleaned_age, na.rm = TRUE)
survey_female_pct  <- sum(elig$Gender == "Female",               na.rm = TRUE) / n_total * 100
survey_male_pct    <- sum(elig$Gender == "Male",                 na.rm = TRUE) / n_total * 100
survey_pna_pct     <- sum(elig$Gender == "Prefer not to answer", na.rm = TRUE) / n_total * 100
survey_college_pct <- mean(elig$college_graduate, na.rm = TRUE) * 100

cat("--- Demographics (N =", n_total, ") ---\n")
cat("  Mean age      :", round(survey_mean_age, 2),    " (paper: 39.4)\n")
cat("  Female %      :", round(survey_female_pct, 2),  " (paper: 59.9)\n")
cat("  Male %        :", round(survey_male_pct, 2),    " (paper: not reported)\n")
cat("  PNA %         :", round(survey_pna_pct, 2),     " (paper: not reported)\n")
cat("  College %     :", round(survey_college_pct, 2), " (paper: 58.6)\n")
cat("  Sum of gender shares:",
    round(survey_female_pct + survey_male_pct + survey_pna_pct, 2),
    "(should be ~100)\n\n")

# =============================================================================
# WTA cleaning: censor > $1000 -> NA (for mean/median; NOT for coverage)
# =============================================================================

WTA_CENSOR_THRESHOLD <- 1000

elig <- elig %>%
  mutate(wta_dollars = ifelse(cleaned_WTA > WTA_CENSOR_THRESHOLD,
                              NA_real_, cleaned_WTA))

n_wta_trimmed <- sum(elig$cleaned_WTA > WTA_CENSOR_THRESHOLD, na.rm = TRUE)
cat("WTA censor rule: cleaned_WTA > $", WTA_CENSOR_THRESHOLD,
    " -> NA (mean/median only)\n", sep = "")
cat("  N censored      :", n_wta_trimmed, "\n")
cat("  N retained      :", sum(!is.na(elig$wta_dollars)), "\n\n")

# =============================================================================
# WTA: full survey population
#   Median/mean use wta_dollars (censored).
#   Coverage uses raw cleaned_WTA (untrimmed, per Guy's guidance).
# =============================================================================

wta_full_median             <- median(elig$wta_dollars, na.rm = TRUE)
wta_full_mean               <- mean(  elig$wta_dollars, na.rm = TRUE)
invited_sample_coverage_pct <- sum(elig$cleaned_WTA <= 40, na.rm = TRUE) /
  sum(!is.na(elig$cleaned_WTA)) * 100

cat("--- WTA Full sample ---\n")
cat("  Median (censored)      :", wta_full_median,         " (paper: 25)\n")
cat("  Mean   (censored)      :", round(wta_full_mean, 2), " (paper: 52)\n")
cat("  Coverage (untrimmed)   :", round(invited_sample_coverage_pct, 2),
    " (paper: 89)\n\n")

# =============================================================================
# WTA: invited sample (eligible = presented_offer >= cutoff_wta, drop testing)
# =============================================================================

exp <- read.csv("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv",
                stringsAsFactors = FALSE)

# Invited sample = eligible for extension phase (paper Section 4.5 "2,874
# were eligible"). NOT the 1717 installed / 1597 randomized / 1510 finished
# sub-samples.
elig_sample <- exp %>%
  filter(presented_offer >= cutoff_wta,
         in_experiment != "testing")

# Join WTA from elig (baseline completers, with censored wta_dollars) onto
# invited sample
invited <- elig %>%
  filter(contact %in% elig_sample$email)

cat("--- WTA Invited sample (eligible) ---\n")
cat("  N (eligible in exp)    :", nrow(elig_sample), "(paper: 2874)\n")
cat("  N (with valid WTA)     :", sum(!is.na(invited$cleaned_WTA)), "\n")
cat("  N (after censor)       :", sum(!is.na(invited$wta_dollars)), "\n")

wta_invited_median <- median(invited$wta_dollars, na.rm = TRUE)
wta_invited_mean   <- mean(  invited$wta_dollars, na.rm = TRUE)

cat("  Median (censored)      :", wta_invited_median,         " (paper: 20)\n")
cat("  Mean   (censored)      :", round(wta_invited_mean, 2), " (paper: 19)\n\n")

# =============================================================================
# Write 10 macros via savetexvalue (append-only -> wipe first)
# =============================================================================

values_file <- "survey_descriptive_values"
values_full <- file.path(VALUES_DIR, paste0(values_file, ".tex"))
if (file.exists(values_full)) file.remove(values_full)

# Demographics
save_tex_value(values = format_age(survey_mean_age),
               names = "surveyMeanAge",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_pct(survey_female_pct),
               names = "surveyFemalePct",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_pct(survey_male_pct),
               names = "surveyMalePct",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_pct(survey_pna_pct),
               names = "surveyPnaPct",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_pct(survey_college_pct),
               names = "surveyCollegePct",
               file_name = values_file, path = VALUES_DIR)

# WTA invited: median is a ladder integer ($5, $10, $15, ..., $40), so the
# integer formatter is fine for both.
save_tex_value(values = format_dollar(wta_invited_median),
               names = "wtaInvitedMedian",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_dollar(wta_invited_mean),
               names = "wtaInvitedMean",
               file_name = values_file, path = VALUES_DIR)

# WTA full
save_tex_value(values = format_dollar(wta_full_median),
               names = "wtaFullMedian",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_dollar(wta_full_mean),
               names = "wtaFullMean",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_pct(invited_sample_coverage_pct),
               names = "invitedSampleCoveragePct",
               file_name = values_file, path = VALUES_DIR)

cat("Saved:", values_full, "\n")