# =============================================================================
# PARTICIPANT FLOW SCALARS (survey_analysis)
# =============================================================================
#
# Produces:
#   Section 4 inline scalars [main paper prose, Figure 2 funnel through
#   end-of-extension retention and differential attrition tests]:
#     output/values/participant_flow_values.tex
#       20 \newcommand macros:
#
#       Recruitment funnel (writeup_v3.tex line 380-385, Figure 2):
#         \fullCohortN              -- N who provided email and use Chrome
#         \baselineSurveyN          -- N who completed the baseline survey
#         \baselineCompletionPct    -- baselineSurveyN / fullCohortN * 100
#         \eligibleN                -- N eligible for extension phase
#         \eligiblePct              -- eligibleN / baselineSurveyN * 100
#         \installedN               -- N who installed extension
#         \installedPctOfEligible   -- installedN / eligibleN * 100
#         \remainedN                -- N remaining at treatment assignment
#         \remainedPct              -- remainedN / installedN * 100
#
#       End-of-extension N by treatment (writeup_v3.tex line 396):
#         \extensionInfoFinalN     / \extensionInfoStartN
#         \extensionSaliencyFinalN / \extensionSaliencyStartN
#         \extensionControlFinalN  / \extensionControlStartN
#
#       Endline survey completion by treatment (writeup_v3.tex line 397):
#         \endlineInfoN / \endlineSaliencyN / \endlineControlN
#
#       Differential attrition chi-square tests (writeup_v3.tex line 400):
#         \attritionExtensionPvalue
#         \attritionEndlinePvalue
#
# Data sources:
#   ../data/Survey/survey_merged_final.csv
#     Used for baselineSurveyN. nrow() of this CSV is the canonical
#     "completed baseline survey" count (dedup + cleaned in
#     endline_clean_n_join_intro.R).
#
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#     Used for eligibleN, installedN, remainedN, and all per-treatment counts.
#     Key fields: cutoff_wta, presented_offer, in_experiment,
#     experiment_condition, email.
#
#   ../data/Survey/final_endline_survey_cleaned.csv
#     Used for endline N. Joined to experiment_conditions on email
#     (case-insensitive).
#
# Cleaning rules:
#   - in_experiment == "testing" rows are research-team accounts
#     (sggold, guy.aridor, etc.); excluded from all counts.
#   - in_experiment == "Removed due to grounding" is a single anomalous row;
#     excluded from eligibleN.
#   - All email comparisons use tolower(trimws()).
#
# Counts breakdown of in_experiment field (after wave 1 + wave 2):
#   testing                : 8     -- research-team accounts, excluded
#   Removed due to grounding: 1    -- excluded
#   no_install             : 1157  -- failed to install
#   too_low_wta            : 5295  -- not eligible (BDM threshold not met)
#   true                   : 1511  -- finished extension period
#   uninstall              : 119   -- installed then uninstalled
#   inactivity             : 82    -- installed but inactive
#   technical_issue        : 3     -- installed but technical issues
#   request_deletion       : 1     -- installed, requested data deletion
#
# eligibleN = rows where presented_offer >= cutoff_wta, excluding testing
#             and Removed due to grounding. Equals 2874 (paper).
# installedN = rows with in_experiment in {true, uninstall, inactivity,
#              technical_issue, request_deletion, Removed due to grounding}.
#              Equals 1717 (paper).
# remainedN = rows with experiment_condition non-empty (got treatment
#             assignment). Equals 1597 (paper).
#
# Numbers vs. paper (writeup_v3.tex Section 4 + Figure 2):
#   match paper exactly:
#     \eligibleN                2874
#     \installedN               1717
#     \remainedN                1597
#     \extensionInfoFinalN      501
#     \extensionSaliencyFinalN  500
#     \extensionControlFinalN   509
#     \extensionControlStartN   532
#     \endlineSaliencyN         472
#     \attritionExtensionPvalue 0.376 (paper 0.37, matches within rounding)
#   differ from paper by 1 -- paper figure and prose to be updated:
#     \baselineSurveyN          8168 (paper 8169)
#     \endlineInfoN             479  (paper 480)
#     \endlineControlN          475  (paper 474)
#   info / saliency Start N swapped relative to paper line 396 -- the data
#   say 533 info / 532 saliency, paper prose says 532 info / 533 saliency.
#   Paper prose is wrong; figure has been updated to match the data:
#     \extensionInfoStartN      533 (paper line 396: 532)
#     \extensionSaliencyStartN  532 (paper line 396: 533)
#   not in repo -- hardcoded with explanatory comment:
#     \fullCohortN              13382  -- source: screener stage log, not
#                                         retained in spring2025experiment/
#   differs substantially from paper -- paper appears incorrect:
#     \attritionEndlinePvalue   0.833 (paper 0.96)
#       paper's 0.96 cannot be reproduced by any standard test:
#         chi-square + denom=startN  -> 0.83
#         chi-square + denom=finalN  -> 0.28
#         Fisher    + denom=startN   -> 0.83
#         Fisher    + denom=finalN   -> 0.28
#         logit Wald + denom=startN  -> 0.83
#         logit Wald + denom=finalN  -> 0.28
#       we report chi-square with startN denominator for consistency with
#       the extension attrition test above. The qualitative conclusion is
#       unchanged: no statistically significant differential attrition.
#
# Number formatting:
#   All numeric output is pre-formatted via number_format_helpers.R helpers
#   (format_count for integer counts, format_pct for percentages,
#   format_pvalue for chi-square p-values). See that file for the full
#   rules. We pre-format because savetexvalue's accuracy parameter is
#   broken in our package version.
#
# Inputs:
#   ../data/Survey/survey_merged_final.csv
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#   ../data/Survey/final_endline_survey_cleaned.csv
#
# Dependencies:
#   tidyverse
#   savetexvalue (devtools::install_github("Ori-Shoham/savetexvalue"))
#   replication_files/utils/number_format_helpers.R
#
# Outputs:
#   output/values/participant_flow_values.tex
# =============================================================================

library(tidyverse)
library(savetexvalue)

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/number_format_helpers.R")

VALUES_DIR <- "output/values/"

# =============================================================================
# Load inputs
# =============================================================================

sm <- read.csv("../data/Survey/survey_merged_final.csv",
               stringsAsFactors = FALSE)
exp <- read.csv("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv",
                stringsAsFactors = FALSE)
endline <- read.csv("../data/Survey/final_endline_survey_cleaned.csv",
                    stringsAsFactors = FALSE)

# Normalize emails for joins
exp$email_lower       <- tolower(trimws(exp$email))
endline$emailid_lower <- tolower(trimws(endline$emailid))

cat("Loaded inputs:\n")
cat("  survey_merged_final.csv rows           :", nrow(sm),      "\n")
cat("  experiment_conditions rows             :", nrow(exp),     "\n")
cat("  final_endline_survey_cleaned.csv rows  :", nrow(endline), "\n\n")

# =============================================================================
# Funnel: fullCohortN, baselineSurveyN, eligibleN, installedN, remainedN
# =============================================================================

# fullCohortN: not in repo. Source is the screener-stage cohort with email and
# Chrome confirmed. Hardcoded from paper Figure 2.
full_cohort_n <- 13382

# baselineSurveyN: completed the baseline survey (post-dedup, post-clean).
baseline_survey_n <- nrow(sm)

# eligibleN: presented_offer >= cutoff_wta, excluding testing and the one
# "Removed due to grounding" record.
exp <- exp %>%
  mutate(cutoff_num = suppressWarnings(as.numeric(cutoff_wta)),
         offer_num  = suppressWarnings(as.numeric(presented_offer)))
eligible_n <- exp %>%
  filter(!in_experiment %in% c("testing", "Removed due to grounding")) %>%
  filter(!is.na(cutoff_num) & !is.na(offer_num) & offer_num >= cutoff_num) %>%
  nrow()

# installedN: in_experiment in any post-install status.
INSTALLED_STATUSES <- c("true", "uninstall", "inactivity",
                        "technical_issue", "request_deletion",
                        "Removed due to grounding")
installed_n <- sum(exp$in_experiment %in% INSTALLED_STATUSES)

# remainedN: assigned to a treatment.
remained_n <- sum(exp$experiment_condition != "" &
                    !is.na(exp$experiment_condition))

# Percentages
baseline_completion_pct  <- baseline_survey_n / full_cohort_n * 100
eligible_pct             <- eligible_n        / baseline_survey_n * 100
installed_pct_of_elig    <- installed_n       / eligible_n * 100
remained_pct             <- remained_n        / installed_n * 100

cat("Recruitment funnel:\n")
cat(sprintf("  Full cohort         : %6d  (paper: 13382)\n", full_cohort_n))
cat(sprintf("  Baseline survey N   : %6d  (paper:  8169)\n", baseline_survey_n))
cat(sprintf("    completion pct    : %6.2f  (paper:    61)\n", baseline_completion_pct))
cat(sprintf("  Eligible N          : %6d  (paper:  2874)\n", eligible_n))
cat(sprintf("    eligible pct      : %6.2f  (paper:    35)\n", eligible_pct))
cat(sprintf("  Installed N         : %6d  (paper:  1717)\n", installed_n))
cat(sprintf("    installed pct     : %6.2f  (paper:    60)\n", installed_pct_of_elig))
cat(sprintf("  Remained N          : %6d  (paper:  1597)\n", remained_n))
cat(sprintf("    remained pct      : %6.2f  (paper:    93)\n\n", remained_pct))

# =============================================================================
# Per-treatment counts: extension start, extension final, endline
# =============================================================================

# Start N per treatment: assigned to that treatment (experiment_condition).
start_by_treat <- table(exp$experiment_condition[exp$experiment_condition != ""])
extension_info_start     <- as.integer(start_by_treat["info"])
extension_saliency_start <- as.integer(start_by_treat["saliency"])
extension_control_start  <- as.integer(start_by_treat["control"])

# Final N per treatment: finished extension (in_experiment == "true").
final_by_treat <- table(exp$experiment_condition[
  exp$in_experiment == "true" & exp$experiment_condition != ""
])
extension_info_final     <- as.integer(final_by_treat["info"])
extension_saliency_final <- as.integer(final_by_treat["saliency"])
extension_control_final  <- as.integer(final_by_treat["control"])

# Endline N per treatment: completed endline (endline CSV) and were in
# treatment (joined on email) and finished extension.
exp_slim <- exp %>%
  select(emailid_lower = email_lower, experiment_condition, in_experiment)
joined <- endline %>% left_join(exp_slim, by = "emailid_lower")
endline_by_treat <- table(joined$experiment_condition[
  joined$in_experiment == "true" & joined$experiment_condition != ""
])
endline_info     <- as.integer(endline_by_treat["info"])
endline_saliency <- as.integer(endline_by_treat["saliency"])
endline_control  <- as.integer(endline_by_treat["control"])

cat("Per-treatment counts:\n")
cat("                  start  final  endline    (paper start / final / endline)\n")
cat(sprintf("  info       :    %4d   %4d     %4d    (533 / 501 / 480)\n",
            extension_info_start, extension_info_final, endline_info))
cat(sprintf("  saliency   :    %4d   %4d     %4d    (532 / 500 / 472)\n",
            extension_saliency_start, extension_saliency_final, endline_saliency))
cat(sprintf("  control    :    %4d   %4d     %4d    (532 / 509 / 474)\n\n",
            extension_control_start, extension_control_final, endline_control))

# =============================================================================
# Differential attrition: chi-square tests
# =============================================================================

# Extension attrition: 2 x 3 table.
#   row = (finished extension, did not finish)
#   col = (control, info, saliency)
ext_table <- matrix(
  c(extension_control_final,
    extension_info_final,
    extension_saliency_final,
    extension_control_start - extension_control_final,
    extension_info_start    - extension_info_final,
    extension_saliency_start - extension_saliency_final),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("finished", "didn't finish"),
                  c("control", "info", "saliency"))
)
cat("Extension attrition contingency table:\n")
print(ext_table)
attrition_ext_p <- chisq.test(ext_table)$p.value
cat(sprintf("Chi-square p-value: %.4f  (paper: 0.37)\n\n", attrition_ext_p))

# Endline attrition: 2 x 3 table.
#   row = (completed endline, did not), denominator = startN per treatment
#   (treatment-assigned sample). Using startN as the denominator -- rather
#   than finalN (extension completers) -- keeps the test consistent with the
#   extension attrition table above (both treat assignment as the reference
#   sample) and aligns with the ITT interpretation: differential attrition is
#   measured against everyone who was randomized, not just those who survived
#   the extension period.
end_table <- matrix(
  c(endline_control,
    endline_info,
    endline_saliency,
    extension_control_start  - endline_control,
    extension_info_start     - endline_info,
    extension_saliency_start - endline_saliency),
  nrow = 2, byrow = TRUE,
  dimnames = list(c("completed endline", "did not"),
                  c("control", "info", "saliency"))
)
cat("Endline attrition contingency table:\n")
print(end_table)
attrition_endline_p <- chisq.test(end_table)$p.value
cat(sprintf("Chi-square p-value: %.4f  (paper: 0.96)\n\n", attrition_endline_p))

# =============================================================================
# Write 20 macros via savetexvalue (append-only -> wipe first)
# =============================================================================

values_file <- "participant_flow_values"
values_full <- file.path(VALUES_DIR, paste0(values_file, ".tex"))
if (file.exists(values_full)) file.remove(values_full)

# Funnel counts
save_tex_value(values = format_count(full_cohort_n),
               names = "fullCohortN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(baseline_survey_n),
               names = "baselineSurveyN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(eligible_n),
               names = "eligibleN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(installed_n),
               names = "installedN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(remained_n),
               names = "remainedN",
               file_name = values_file, path = VALUES_DIR)

# Funnel percentages
save_tex_value(values = format_pct(baseline_completion_pct),
               names = "baselineCompletionPct",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_pct(eligible_pct),
               names = "eligiblePct",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_pct(installed_pct_of_elig),
               names = "installedPctOfEligible",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_pct(remained_pct),
               names = "remainedPct",
               file_name = values_file, path = VALUES_DIR)

# Per-treatment counts
save_tex_value(values = format_count(extension_info_start),
               names = "extensionInfoStartN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(extension_info_final),
               names = "extensionInfoFinalN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(extension_saliency_start),
               names = "extensionSaliencyStartN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(extension_saliency_final),
               names = "extensionSaliencyFinalN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(extension_control_start),
               names = "extensionControlStartN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(extension_control_final),
               names = "extensionControlFinalN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(endline_info),
               names = "endlineInfoN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(endline_saliency),
               names = "endlineSaliencyN",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_count(endline_control),
               names = "endlineControlN",
               file_name = values_file, path = VALUES_DIR)

# Attrition p-values
save_tex_value(values = format_pvalue(attrition_ext_p),
               names = "attritionExtensionPvalue",
               file_name = values_file, path = VALUES_DIR)
save_tex_value(values = format_pvalue(attrition_endline_p),
               names = "attritionEndlinePvalue",
               file_name = values_file, path = VALUES_DIR)

cat("Saved:", values_full, "\n")