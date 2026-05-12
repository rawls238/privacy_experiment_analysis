# =============================================================================
# OTHER SURVEY REGRESSIONS
# =============================================================================
#
# Produces:
#   Main paper:
#     Table C.3 [tab:experimenter_demand, "Changes in Experimental Behavior"]:
#       output/tables/experiment_modified_behavior[_suffix].tex
#     Table C.8 [tab:data_sharing_purpose, "Data Sharing Purposes"]:
#       output/values/data_sharing_purpose_values.tex
#       (22 macros: 11 \dataPurpose<X>N counts + 11 \dataPurpose<X>Pct percents)
#     Table C.9 [tab:treatment_effect_data_sharing,
#                "Treatment Effect on Self-Reported Data Sharing Behavior"]:
#       output/tables/data_sharing_treatment_effects[_suffix].tex
#
#   `_suffix` is empty for unweighted and `_{weight_spec}` otherwise. Set
#   WEIGHT_SPEC at the top of the script: "unweighted", "weight_census",
#   "weight_pew", "weight_combined", or "all" to run all four.
#
# Inputs:
#   ../data/Survey/survey_merged_final.csv
#   ../data/Survey/final_endline_survey_cleaned.csv
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#   ../data/Survey/individual_level_weights.csv  (only if WEIGHT_SPEC != "unweighted")
#
# Dependencies:
#   replication_files/utils/values.R
#   replication_files/utils/time_usage_helpers.R
#   replication_files/utils/info_acq_helpers.R
#
# Outputs:
#   output/tables/data_sharing_treatment_effects[_suffix].tex
#   output/tables/experiment_modified_behavior[_suffix].tex
#   output/values/data_sharing_purpose_values.tex
#
# Note: Previous version of this script also produced (now removed as dead code):
#   - category_summary: aggregate Positive/Negative totals (console-only, not in paper)
#   - n_consequences / n_positive / n_negative per-respondent means (console-only)
#   - valence_table + prop.table(valence_table): "Mixed/Positive only/Negative only"
#     distribution across respondents (console-only)
#   - summary_by_treatment: raw data-sharing rates by treatment group, supplanted
#     by the Table C.9 regression (console-only diagnostic)
# Removed dead library() calls (verified zero use in script):
#   - library(quantreg)
#   - library(acs)
# =============================================================================

# Set working directory to code_github root so all relative paths resolve.
setwd("~/Dropbox/spring2025experiment/code_github")

# Source utility scripts
source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/info_acq_helpers.R")

# Load required libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(fixest)
library(savetexvalue)

# Output directories
TABLES_DIR <- "output/tables/"
VALUES_DIR <- "output/values/"

# =============================================================================
# Table C.8 [tab:data_sharing_purpose]: DATA SHARING PURPOSES (savetexvalue)
# =============================================================================
# Lifted outside the weight loop because the consequence frequencies come from
# a single multi-select endline question and are unweighted descriptives.

endline_clean <- read.csv("../data/Survey/final_endline_survey_cleaned.csv",
                          stringsAsFactors = FALSE)
cat(sprintf("Loaded endline_clean: %d rows\n", nrow(endline_clean)))

cat("Creating binary indicators for each consequence type...\n")
consequences_processed <- endline_clean %>%
  filter(!is.na(consequences) & consequences != "") %>%
  mutate(
    cons_interesting_ads   = grepl("More interesting ads",             consequences),
    cons_annoying_ads      = grepl("More annoying ads",                consequences),
    cons_personalized      = grepl("Personalized content",             consequences),
    cons_easier_sharing    = grepl("Easier content sharing & log-ins", consequences),
    cons_loss_control      = grepl("Loss of data control",             consequences),
    cons_hacking           = grepl("Hacking",                          consequences),
    cons_identity_theft    = grepl("Identity theft",                   consequences),
    cons_cyberstalking     = grepl("Cyberstalking",                    consequences),
    cons_coupons           = grepl("Coupons & price discounts",        consequences),
    cons_better_search     = grepl("Better search results",            consequences),
    cons_improved_services = grepl("Improved online services",         consequences)
  )

n_resp <- nrow(consequences_processed)
cat(sprintf("  Sample size: %d respondents\n", n_resp))

# Build the per-row Category (Positive / Negative) label used in the paper's
# Table C.8 fourth column. The aggregate summary by category has been removed
# (see header note), but the per-row label is still required.
consequence_summary <- consequences_processed %>%
  summarise(
    `More interesting ads` = sum(cons_interesting_ads),
    `More annoying ads`    = sum(cons_annoying_ads),
    `Personalized content` = sum(cons_personalized),
    `Easier sharing/logins`= sum(cons_easier_sharing),
    `Loss of data control` = sum(cons_loss_control),
    `Hacking`              = sum(cons_hacking),
    `Identity theft`       = sum(cons_identity_theft),
    `Cyberstalking`        = sum(cons_cyberstalking),
    `Coupons/discounts`    = sum(cons_coupons),
    `Better search results`= sum(cons_better_search),
    `Improved services`    = sum(cons_improved_services)
  ) %>%
  pivot_longer(everything(), names_to = "Consequence", values_to = "Count") %>%
  mutate(
    Percentage = round(100 * Count / n_resp, 2),
    Category = case_when(
      Consequence %in% c("More annoying ads", "Loss of data control",
                         "Hacking", "Identity theft", "Cyberstalking") ~ "Negative",
      TRUE ~ "Positive"
    )
  ) %>%
  arrange(desc(Count))

cat("\nConsequences ranked by frequency:\n")
print(consequence_summary)

# Save 22 macros to data_sharing_purpose_values.tex. The ShortName ordering
# below must match the paper's Table C.8 row order (paper rows are sorted by
# Count descending), so the LaTeX cell -> macro mapping is unambiguous.
consequence_short_names <- c(
  "LossControl",       # Loss of data control     (Negative)
  "Personalized",      # Personalized content     (Positive)
  "AnnoyingAds",       # More annoying ads        (Negative)
  "IdentityTheft",     # Identity theft           (Negative)
  "Hacking",           # Hacking                  (Negative)
  "BetterSearch",      # Better search results    (Positive)
  "EasierSharing",     # Easier sharing/logins    (Positive)
  "Cyberstalking",     # Cyberstalking            (Negative)
  "ImprovedServices",  # Improved services        (Positive)
  "Coupons",           # Coupons/discounts        (Positive)
  "InterestingAds"     # More interesting ads     (Positive)
)
consequence_long_names <- c(
  "Loss of data control", "Personalized content", "More annoying ads",
  "Identity theft", "Hacking", "Better search results",
  "Easier sharing/logins", "Cyberstalking", "Improved services",
  "Coupons/discounts", "More interesting ads"
)

# Re-order consequence_summary rows to match consequence_long_names (paper
# row order), so values line up with consequence_short_names.
ordered_summary <- consequence_summary[match(consequence_long_names,
                                             consequence_summary$Consequence), ]
stopifnot(!any(is.na(ordered_summary$Count)))  # guard against name mismatch

# Save 11 count macros (integer, accuracy = 1).
save_tex_value(
  values    = ordered_summary$Count,
  names     = paste0("dataPurpose", consequence_short_names, "N"),
  file_name = "data_sharing_purpose_values",
  path      = VALUES_DIR,
  accuracy  = 1
)

# Save 11 percentage macros. percent = FALSE because the paper's Table C.8
# column header is "Percentage" -- the unit is implied, and the original
# hard-coded cells were bare numbers like "62.20" without `\%`. accuracy =
# 0.01 reproduces the existing two-decimal format.
save_tex_value(
  values    = ordered_summary$Percentage,
  names     = paste0("dataPurpose", consequence_short_names, "Pct"),
  file_name = "data_sharing_purpose_values",
  path      = VALUES_DIR,
  percent   = FALSE,
  accuracy  = 0.01
)

cat(sprintf("Saved 22 macros to %sdata_sharing_purpose_values.tex\n", VALUES_DIR))

# =============================================================================
# WEIGHT LOOP: Table C.9 and Table C.3 (weight-dependent regressions)
# =============================================================================
## [WEIGHT MODIFICATION] ======================================================
## Options: "unweighted", "weight_census", "weight_pew", "weight_combined", "all"
WEIGHT_SPEC <- "all"

.ALL_SPECS <- c("unweighted", "weight_census", "weight_pew", "weight_combined")
if (WEIGHT_SPEC == "all") {
  .specs_to_run <- .ALL_SPECS
} else {
  .specs_to_run <- WEIGHT_SPEC
}

for (WEIGHT_SPEC in .specs_to_run) {
  
  cat("\n\n############################################################\n")
  cat(sprintf("## Running other_survey_regressions.R with: %s\n", WEIGHT_SPEC))
  cat("############################################################\n\n")
  
  OUTPUT_SUFFIX <- if (WEIGHT_SPEC == "unweighted") "" else paste0("_", WEIGHT_SPEC)
  
  # ---- weight helpers ----
  run_weighted_feols <- function(fml, data, cluster_var = NULL, wt_spec = WEIGHT_SPEC) {
    if (wt_spec == "unweighted") {
      feols(fml, cluster = cluster_var, data = data)
    } else {
      feols(fml, cluster = cluster_var, weights = ~wt, data = data)
    }
  }
  
  join_weights <- function(df, wt_spec = WEIGHT_SPEC) {
    if (wt_spec == "unweighted") {
      df$wt <- 1
      return(df)
    }
    weights_df  <- read.csv("../data/Survey/individual_level_weights.csv",
                            stringsAsFactors = FALSE)
    weights_ext <- weights_df %>%
      filter(sample == "extension") %>%
      select(experiment_id, all_of(wt_spec)) %>%
      rename(wt = !!wt_spec)
    df <- df %>%
      mutate(experiment_id_chr = as.character(experiment_id)) %>%
      left_join(weights_ext %>% mutate(experiment_id = as.character(experiment_id)),
                by = c("experiment_id_chr" = "experiment_id")) %>%
      select(-experiment_id_chr) %>%
      mutate(wt = ifelse(is.na(wt), 1, wt))
    cat(sprintf("  Weight join: %d / %d rows\n", sum(!is.na(df$wt)), nrow(df)))
    cat(sprintf("  Weight summary: min=%.3f max=%.3f mean=%.3f\n",
                min(df$wt, na.rm = TRUE),
                max(df$wt, na.rm = TRUE),
                mean(df$wt, na.rm = TRUE)))
    return(df)
  }
  
  # ---- load data ----
  survey_merged <- read.csv("../data/Survey/survey_merged_final.csv",
                            stringsAsFactors = FALSE)
  cat(sprintf("Loaded survey_merged: %d rows, %d columns\n",
              nrow(survey_merged), ncol(survey_merged)))
  
  experiment_user_info <- read.csv(
    "../data/final_extension_data/experiment_conditions_pilot_july_2024.csv",
    stringsAsFactors = FALSE
  )
  
  survey_merged <- survey_merged %>%
    left_join(
      experiment_user_info %>%
        select(email, experiment_id_aux = experiment_id, experiment_condition,
               requested_information_survey_aux  = requested_information_survey,
               requested_information_conjoint_aux = requested_information_conjoint,
               in_experiment, cutoff_wta, wave_id, block_idx),
      by = c("emailid" = "email")
    )
  
  # ===========================================================================
  # Table C.9 [tab:treatment_effect_data_sharing]
  # ===========================================================================
  
  cat("\n=== Table C.9: Data Sharing ===\n")
  
  data_sharing_analysis <- survey_merged %>%
    filter(completed_both == TRUE & !is.na(data_sharing_1) &
             !is.na(experiment_condition) & experiment_condition != "") %>%
    mutate(
      data_sharing_1_binary = ifelse(data_sharing_1 != "No", 1, 0),
      data_sharing_2_binary = ifelse(data_sharing_2 != "No" & !is.na(data_sharing_2), 1, 0),
      data_sharing_3_binary = ifelse(data_sharing_3 != "No", 1, 0),
      data_sharing_4_binary = ifelse(data_sharing_4 != "No" & !is.na(data_sharing_4), 1, 0),
      total_shared = data_sharing_1_binary + data_sharing_2_binary +
        data_sharing_3_binary + data_sharing_4_binary,
      any_shared   = ifelse(total_shared > 0, 1, 0)
    )
  
  cat(sprintf("  Analysis sample: %d participants\n", nrow(data_sharing_analysis)))
  cat(sprintf("  Treatment groups: Control=%d, Info=%d, Saliency=%d\n",
              sum(data_sharing_analysis$experiment_condition == "control"),
              sum(data_sharing_analysis$experiment_condition == "info"),
              sum(data_sharing_analysis$experiment_condition == "saliency")))
  
  # Set factor levels: saliency before information (matches paper)
  data_sharing_analysis <- data_sharing_analysis %>%
    mutate(experiment_condition = factor(experiment_condition,
                                         levels = c("control", "saliency", "info"))) %>%
    mutate(wave_id = ifelse(wave_id == 3, 2, wave_id)) %>%
    mutate(block_by_wave = paste(wave_id, block_idx, sep = "_")) %>%
    rename(experiment_id = experiment_id_aux)
  
  data_sharing_analysis <- join_weights(data_sharing_analysis)
  
  model_share1 <- run_weighted_feols(
    data_sharing_1_binary ~ experiment_condition | block_by_wave,
    cluster = ~experiment_id, data = data_sharing_analysis)
  model_share2 <- run_weighted_feols(
    data_sharing_2_binary ~ experiment_condition | block_by_wave,
    cluster = ~experiment_id, data = data_sharing_analysis)
  model_share3 <- run_weighted_feols(
    data_sharing_3_binary ~ experiment_condition | block_by_wave,
    cluster = ~experiment_id, data = data_sharing_analysis)
  model_share4 <- run_weighted_feols(
    data_sharing_4_binary ~ experiment_condition | block_by_wave,
    cluster = ~experiment_id, data = data_sharing_analysis)
  
  dict <- c(
    "experiment_conditioninfo"     = "Information Treatment",
    "experiment_conditionsaliency" = "Saliency Treatment",
    "(Intercept)" = "Constant",
    "Intercept"   = "Constant"
  )
  
  etable(model_share1, model_share2, model_share3, model_share4,
         headers = c("Deleted Cookies", "Opted out",
                     "Withheld data", "Changed privacy settings"),
         dict   = dict,
         tex    = TRUE,
         replace = TRUE,                                          
         title  = "Treatment Effect on Self-Reported Data Sharing Behavior",
         signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
         digits = 3,
         file   = paste0(TABLES_DIR, "data_sharing_treatment_effects",
                         OUTPUT_SUFFIX, ".tex"))
  
  # ===========================================================================
  # Table C.3 [tab:experimenter_demand]
  # ===========================================================================
  
  cat("\n=== Table C.3: Experimenter Demand ===\n")
  
  survey_merged <- survey_merged %>%
    mutate(
      annoying_1_numeric = case_when(
        annoying_1 == "Not at all" ~ 0,
        annoying_1 == "2"          ~ 0.25,
        annoying_1 == "3"          ~ 0.5,
        annoying_1 == "4"          ~ 0.75,
        annoying_1 == "Always"     ~ 1,
        TRUE ~ as.numeric(annoying_1)
      ),
      annoying_2_numeric = case_when(
        annoying_2 == "Not at all" ~ 0,
        annoying_2 == "2"          ~ 0.25,
        annoying_2 == "3"          ~ 0.5,
        annoying_2 == "4"          ~ 0.75,
        annoying_2 == "Always"     ~ 1,
        TRUE ~ as.numeric(annoying_2)
      )
    ) %>%
    mutate(experiment_condition = factor(experiment_condition,
                                         levels = c("control", "saliency", "info")))
  
  experimenter_demand_data <- survey_merged %>%
    filter(experiment_condition %in% c("info", "control", "saliency")) %>%
    rename(experiment_id = experiment_id_aux)
  experimenter_demand_data <- join_weights(experimenter_demand_data)
  
  t1 <- run_weighted_feols(annoying_1_numeric ~ experiment_condition,
                           data = experimenter_demand_data)
  t2 <- run_weighted_feols(annoying_2_numeric ~ experiment_condition,
                           data = experimenter_demand_data)
  
  dict <- c(
    "experiment_conditioninfo"     = "Information Treatment",
    "experiment_conditionsaliency" = "Saliency Treatment",
    "(Intercept)" = "Constant"
  )
  
  etable(t1, t2,
         headers = c("Disrupted Browsing", "Evaded Extension/Chrome"),
         dict    = dict,
         tex     = TRUE,
         replace = TRUE,
         depvar  = FALSE,
         signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
         digits = 3,
         file    = paste0(TABLES_DIR, "experiment_modified_behavior",
                          OUTPUT_SUFFIX, ".tex"))
  
} # end weight spec loop