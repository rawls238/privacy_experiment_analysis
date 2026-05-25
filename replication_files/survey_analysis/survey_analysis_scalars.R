# =============================================================================
# SURVEY DESCRIPTIVE SCALARS (survey_analysis)
# =============================================================================
#
# Produces:
#   Section 4 inline scalars [main paper prose, sample descriptives + WTA]:
#     output/values/survey_descriptive_values.tex
#       10 \newcommand macros:
#
#       Demographics (writeup_v3.tex line 396):
#         \surveyMeanAge              -- mean age of the survey sample
#         \surveyFemalePct            -- pct female (denom = full survey N)
#         \surveyMalePct              -- pct male   (denom = full survey N)
#         \surveyPnaPct               -- pct "Prefer not to answer" for gender
#         \surveyCollegePct           -- pct with at least a college education
#
#       WTA descriptives (writeup_v3.tex line 415):
#         \wtaInvitedMedian           -- median WTA, invited sample
#         \wtaInvitedMean             -- mean   WTA, invited sample
#         \wtaFullMedian              -- median WTA, full survey population
#         \wtaFullMean                -- mean   WTA, full survey population
#         \invitedSampleCoveragePct   -- pct of full survey with WTA <= $40
#
# Data sources:
#   ../onboarding_sequence_scripts/qualtrics_csvs/survey_full_eligible.csv
#     Pre-processed baseline survey from the recruiting pipeline. Provides
#     cleaned_age, college_graduate, raw Gender (character), and cleaned_WTA.
#
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#     Used to define the invited sample (in_experiment == "true"), joined on
#     email == contact.
#
# Cleaning rules (applied inline below):
#
#   WTA outlier trim (full survey only):
#     The upstream cleaned_WTA column is type-converted but NOT outlier-
#     trimmed: any string in the Other free-text field (WTA_11_other) is
#     parsed numerically, so raw cleaned_WTA contains values up to 1e23 from
#     entries like "100000000000000000000000". Drop cleaned_WTA > $100 -> NA.
#     Rationale: the WTA ladder runs $0..$40 and the BDM mechanism caps the
#     random offer at $40, so any user reporting > $40 in the Other field has
#     already opted out and the dollar amount is not incentive-compatible.
#     $100 = 2.5x the BDM cap, retains plausible Other entries ($50, $75,
#     $100) and removes protest values. 318 users trimmed.
#     The invited sample is unaffected: no Other user can be invited because
#     the random BDM offer is at most $40.
#
#   Gender shares:
#     Use the raw Gender field (character: "Female" / "Male" / "Prefer not to
#     answer") directly. Denominator for all three shares is the full survey
#     N (not the non-PNA N), so the three pcts sum to 100.
#
#     NB. The upstream is_male field (analysis_full.py line 319) treats PNA
#     as is_male = 0, conflating it with female. Female% computed as
#     1 - mean(is_male) is inflated by ~1.3pp. This script does not use
#     is_male; it reads raw Gender instead.
#
# Numbers vs. paper (writeup_v3.tex lines 396 and 415):
#   match paper:
#     \surveyMeanAge              39.4 vs paper 39.4
#     \surveyFemalePct            59.9 vs paper 59.9 (paper happens to match
#                                 because the buggy mean(is_male) and the
#                                 correct mean(Gender == "Female") with PNA
#                                 in the denominator coincide to one decimal)
#     \surveyCollegePct           58.6 vs paper 58.6
#     \wtaInvitedMedian           20   vs paper 20
#     \wtaInvitedMean             19   vs paper 19
#     \wtaFullMedian              25   vs paper 25
#   new (not in paper):
#     \surveyMalePct              38.8
#     \surveyPnaPct               1.3
#   differ from paper (paper text must be updated):
#     \wtaFullMean                28   vs paper 52
#     \invitedSampleCoveragePct   94.5 vs paper 89
#
#   The paper's $52 mean cannot be reproduced from any code in the repository:
#   no committed script applies an outlier trim. With the principled drop
#   > $100 rule applied here, the mean drops to $28 and coverage rises to
#   94.5%.
#
# Inputs:
#   ../onboarding_sequence_scripts/qualtrics_csvs/survey_full_eligible.csv
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#
# Dependencies:
#   savetexvalue (devtools::install_github("Ori-Shoham/savetexvalue"))
#
# Outputs:
#   output/values/survey_descriptive_values.tex
#
# Required edits to writeup_v3.tex (one-time, after first run):
#   1. Add to preamble:
#        \input{../code_github/output/values/survey_descriptive_values.tex}
#   2. Line 396: replace "39.4", "59.9\%", "58.6\%" with
#                \surveyMeanAge, \surveyFemalePct\%, \surveyCollegePct\%
#      and expand the gender prose to report all three categories, e.g.
#        "59.9\% female, 38.8\% male, and 1.3\% prefer not to answer"
#      so the macros \surveyFemalePct, \surveyMalePct, \surveyPnaPct cover
#      the full breakdown.
#   3. Line 415: replace "\$20 (\$19)" with "\$\wtaInvitedMedian\ (\$\wtaInvitedMean)"
#                replace "\$25 (\$52)" with "\$\wtaFullMedian\ (\$\wtaFullMean)"
#                replace "bottom 89\%"  with "bottom \invitedSampleCoveragePct\%"
#                and update the surrounding prose: the full-sample mean is now
#                \$28, not \$52; coverage is 94.5\%, not 89\%.
# =============================================================================

library(tidyverse)
library(savetexvalue)

setwd("~/Dropbox/spring2025experiment/code_github")

VALUES_DIR <- "output/values/"

# =============================================================================
# Load eligible survey CSV
# =============================================================================

elig <- read.csv("../onboarding_sequence_scripts/qualtrics_csvs/survey_full_eligible.csv",
                 stringsAsFactors = FALSE)
cat("Loaded survey_full_eligible.csv:", nrow(elig), "rows\n\n")

# =============================================================================
# Demographics (full survey sample, denom = full N)
# =============================================================================

survey_mean_age <- mean(elig$cleaned_age, na.rm = TRUE)

# Gender shares. Denominator is the full survey N; PNA is its own bucket so
# the three pcts sum to 100. Raw Gender is the source of truth (the upstream
# is_male field has a known bug for PNA respondents; see header note).
n_total            <- nrow(elig)
survey_female_pct  <- sum(elig$Gender == "Female",               na.rm = TRUE) / n_total * 100
survey_male_pct    <- sum(elig$Gender == "Male",                 na.rm = TRUE) / n_total * 100
survey_pna_pct     <- sum(elig$Gender == "Prefer not to answer", na.rm = TRUE) / n_total * 100

survey_college_pct <- mean(elig$college_graduate, na.rm = TRUE) * 100

cat("Demographics (full survey sample, denom =", n_total, "):\n")
cat("  Mean age      :", round(survey_mean_age, 3),    " (paper: 39.4)\n")
cat("  Female %      :", round(survey_female_pct, 3),  " (paper: 59.9)\n")
cat("  Male %        :", round(survey_male_pct, 3),    " (paper: not reported)\n")
cat("  PNA %         :", round(survey_pna_pct, 3),     " (paper: not reported)\n")
cat("  College %     :", round(survey_college_pct, 3), " (paper: 58.6)\n")
cat("  Sum of gender shares:",
    round(survey_female_pct + survey_male_pct + survey_pna_pct, 3),
    "(should be ~100)\n\n")

# =============================================================================
# WTA cleaning rule for the full survey population (drop > $100 -> NA)
# =============================================================================

WTA_FULL_DROP_THRESHOLD <- 100
elig <- elig %>%
  mutate(wta_dollars = ifelse(
    !is.na(cleaned_WTA) & cleaned_WTA > WTA_FULL_DROP_THRESHOLD,
    NA_real_,
    cleaned_WTA
  ))
n_wta_dropped <- sum(!is.na(elig$cleaned_WTA) & elig$cleaned_WTA > WTA_FULL_DROP_THRESHOLD)
cat("WTA cleaning rule: drop cleaned_WTA > $",
    WTA_FULL_DROP_THRESHOLD, " -> NA\n", sep = "")
cat("  N dropped       :", n_wta_dropped, "\n")
cat("  N retained      :", sum(!is.na(elig$wta_dollars)), "\n\n")

# =============================================================================
# WTA: full survey population (after cleaning)
# =============================================================================

wta_full_median             <- median(elig$wta_dollars, na.rm = TRUE)
wta_full_mean               <- mean(  elig$wta_dollars, na.rm = TRUE)
invited_sample_coverage_pct <- mean(elig$wta_dollars <= 40, na.rm = TRUE) * 100

cat("WTA, full survey population (after drop > $",
    WTA_FULL_DROP_THRESHOLD, "):\n", sep = "")
cat("  Median           :", wta_full_median,         " (paper: 25)\n")
cat("  Mean             :", round(wta_full_mean, 3), " (paper: 52, differs)\n")
cat("  Pct WTA <= $40   :", round(invited_sample_coverage_pct, 3),
    " (paper: 89, differs)\n\n")

# =============================================================================
# WTA: invited sample (no cleaning applied; BDM caps offers at $40)
# =============================================================================

exp <- read.csv("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv",
                stringsAsFactors = FALSE)
exp_slim <- exp %>%
  select(email, in_experiment) %>%
  rename(contact = email)

joined  <- elig %>% left_join(exp_slim, by = "contact")
invited <- joined %>% filter(in_experiment == "true")

# Invariant: no invited user should have an above-threshold pre-trim WTA.
n_invited_above_thresh <- sum(invited$cleaned_WTA > WTA_FULL_DROP_THRESHOLD, na.rm = TRUE)
if (n_invited_above_thresh > 0) {
  warning("Invited sample contains ", n_invited_above_thresh,
          " users with cleaned_WTA > $", WTA_FULL_DROP_THRESHOLD,
          ". Expected zero under BDM. Investigate before trusting invited stats.")
}

wta_invited_median <- median(invited$cleaned_WTA, na.rm = TRUE)
wta_invited_mean   <- mean(  invited$cleaned_WTA, na.rm = TRUE)

cat("WTA, invited sample (in_experiment == 'true'):\n")
cat("  N                :", sum(!is.na(invited$cleaned_WTA)), "\n")
cat("  Median           :", wta_invited_median,         " (paper: 20)\n")
cat("  Mean             :", round(wta_invited_mean, 3), " (paper: 19)\n\n")

# =============================================================================
# Write 10 macros via savetexvalue (append-only -> wipe first)
# =============================================================================

values_file <- "survey_descriptive_values"
values_full <- file.path(VALUES_DIR, paste0(values_file, ".tex"))
if (file.exists(values_full)) file.remove(values_full)

# Demographics
save_tex_value(values = survey_mean_age,    names = "surveyMeanAge",
               file_name = values_file, path = VALUES_DIR, accuracy = 0.1)
save_tex_value(values = survey_female_pct,  names = "surveyFemalePct",
               file_name = values_file, path = VALUES_DIR, accuracy = 0.1)
save_tex_value(values = survey_male_pct,    names = "surveyMalePct",
               file_name = values_file, path = VALUES_DIR, accuracy = 0.1)
save_tex_value(values = survey_pna_pct,     names = "surveyPnaPct",
               file_name = values_file, path = VALUES_DIR, accuracy = 0.1)
save_tex_value(values = survey_college_pct, names = "surveyCollegePct",
               file_name = values_file, path = VALUES_DIR, accuracy = 0.1)

# WTA, invited sample. Medians are ladder integers -> bare integer via character.
save_tex_value(values = as.character(wta_invited_median), names = "wtaInvitedMedian",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = wta_invited_mean,   names = "wtaInvitedMean",
               file_name = values_file, path = VALUES_DIR, accuracy = 1)

# WTA, full population.
save_tex_value(values = as.character(wta_full_median),    names = "wtaFullMedian",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = wta_full_mean,      names = "wtaFullMean",
               file_name = values_file, path = VALUES_DIR, accuracy = 1)
save_tex_value(values = invited_sample_coverage_pct,      names = "invitedSampleCoveragePct",
               file_name = values_file, path = VALUES_DIR, accuracy = 0.1)

cat("Saved:", values_full, "\n\n")

cat("Paper edits required in writeup_v3.tex (see script header for details):\n")
cat("  line 396: replace 39.4 / 59.9% / 58.6% with macros; expand gender\n")
cat("            prose to report female / male / PNA breakdown\n")
cat("  line 415: replace $20 ($19) / $25 ($52) / 89% with macros\n")
cat("            and update prose: full-sample mean is now $28 (not $52),\n")
cat("            coverage is 94.5% (not 89%)\n")