## [WEIGHT MODIFICATION] ======================================================
## Options: "unweighted", "weight_census", "weight_pew", "weight_combined", "all"
WEIGHT_SPEC <- "all" # change flag here!

.ALL_SPECS <- c("unweighted", "weight_census", "weight_pew", "weight_combined")
if (WEIGHT_SPEC == "all") {
  .specs_to_run <- .ALL_SPECS
} else {
  .specs_to_run <- WEIGHT_SPEC
}

for (WEIGHT_SPEC in .specs_to_run) {
  
  cat("\n\n############################################################\n")
  cat("## Running other_survey_regressions.R with:", WEIGHT_SPEC, "\n")
  cat("############################################################\n\n")
  
  rm(list = setdiff(ls(), c("WEIGHT_SPEC", ".specs_to_run", ".ALL_SPECS")))
  ## =============================================================================
  
  if (Sys.info()[['nodename']] == 'GSB-P4FVDL7QF6'){
    WD <- "/Users/sggold/Library/CloudStorage/Dropbox/Shared-Project-Folders/Privacy-Experiment/spring2025experiment"
  } else if (grepl("marklee", Sys.info()[['user']])) {
    WD <- "/Users/marklee/Dropbox/spring2025experiment"
  } else {
    WD <- "/Users/guyaridor/Dropbox/Privacy-Experiment/spring2025experiment"
  }
  
  setwd(WD)
  
  source("code/utils/values.R")
  source("code/utils/time_usage_helpers.R")
  source("code/utils/info_acq_helpers.R")
  
  # Load required libraries
  library(tidyverse)
  library(quantreg)
  library(lubridate)
  library(stringr)
  library(acs)
  library(fixest)
  
  # Set up directories
  FIGURES_DIR <- "results/baseline_survey_descriptives/"
  TABLES_DIR <- "results/baseline_survey_descriptives/"
  
  ## [WEIGHT MODIFICATION] ======================================================
  OUTPUT_SUFFIX <- if (WEIGHT_SPEC == "unweighted") "" else paste0("_", WEIGHT_SPEC)
  
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
    
    weights_df <- read.csv("data/Survey/individual_level_weights.csv", stringsAsFactors = FALSE)
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
    
    cat("  Weight join:", sum(!is.na(df$wt)), "/", nrow(df), "rows\n")
    cat("  Weight summary: min=", round(min(df$wt, na.rm=TRUE), 3),
        " max=", round(max(df$wt, na.rm=TRUE), 3),
        " mean=", round(mean(df$wt, na.rm=TRUE), 3), "\n")
    return(df)
  }
  ## =============================================================================
  
  survey_merged <- read.csv("data/Survey/survey_merged_final.csv", stringsAsFactors = FALSE)
  cat("Loaded survey_merged:", nrow(survey_merged), "rows,", ncol(survey_merged), "columns\n")
  
  # Load the cleaned endline
  endline_clean <- read.csv("data/Survey/final_endline_survey_cleaned.csv", stringsAsFactors = FALSE)
  cat("Loaded endline_clean:", nrow(endline_clean), "rows\n")
  
  
  cat("\n=== LOADING AUXILIARY DATASETS ===\n")
  
  # Load experiment conditions
  experiment_user_info <- read.csv("data/final_extension_data/experiment_conditions_pilot_july_2024.csv", 
                                   stringsAsFactors = FALSE)
  
  
  survey_merged <- survey_merged %>%
    left_join(
      experiment_user_info %>% 
        select(email, experiment_id_aux = experiment_id, experiment_condition, 
               requested_information_survey_aux = requested_information_survey, 
               requested_information_conjoint_aux = requested_information_conjoint,
               in_experiment, cutoff_wta, wave_id, block_idx),
      by = c("emailid" = "email")
    )
  
  # ============================================================================
  # CONSEQUENCES ANALYSIS (descriptive, not in EC regression tables)
  # ============================================================================
  
  cat("Step 1: Creating binary indicators for each consequence type...\n")
  
  consequences_processed <- endline_clean %>%
    filter(!is.na(consequences) & consequences != "") %>%
    mutate(
      cons_interesting_ads = grepl("More interesting ads", consequences),
      cons_annoying_ads = grepl("More annoying ads", consequences),
      cons_personalized = grepl("Personalized content", consequences),
      cons_easier_sharing = grepl("Easier content sharing & log-ins", consequences),
      cons_loss_control = grepl("Loss of data control", consequences),
      cons_hacking = grepl("Hacking", consequences),
      cons_identity_theft = grepl("Identity theft", consequences),
      cons_cyberstalking = grepl("Cyberstalking", consequences),
      cons_coupons = grepl("Coupons & price discounts", consequences),
      cons_better_search = grepl("Better search results", consequences),
      cons_improved_services = grepl("Improved online services", consequences)
    )
  
  cat("  Created binary indicators for", ncol(consequences_processed) - ncol(endline_clean), "consequence types\n")
  cat("  Sample size:", nrow(consequences_processed), "\n")
  
  cat("\nStep 2: Calculating frequency and percentage for each consequence...\n")
  
  consequence_summary <- consequences_processed %>%
    summarise(
      `More interesting ads` = sum(cons_interesting_ads),
      `More annoying ads` = sum(cons_annoying_ads),
      `Personalized content` = sum(cons_personalized),
      `Easier sharing/logins` = sum(cons_easier_sharing),
      `Loss of data control` = sum(cons_loss_control),
      `Hacking` = sum(cons_hacking),
      `Identity theft` = sum(cons_identity_theft),
      `Cyberstalking` = sum(cons_cyberstalking),
      `Coupons/discounts` = sum(cons_coupons),
      `Better search results` = sum(cons_better_search),
      `Improved services` = sum(cons_improved_services)
    ) %>%
    pivot_longer(everything(), names_to = "Consequence", values_to = "Count") %>%
    mutate(
      Percentage = round(100 * Count / nrow(consequences_processed), 1)
    ) %>%
    arrange(desc(Count))
  
  cat("\nConsequences ranked by frequency:\n")
  print(consequence_summary)
  
  cat("\nStep 3: Categorizing consequences as positive or negative...\n")
  
  consequence_summary <- consequence_summary %>%
    mutate(
      Category = case_when(
        Consequence %in% c("More annoying ads", "Loss of data control", 
                           "Hacking", "Identity theft", "Cyberstalking") ~ "Negative",
        TRUE ~ "Positive"
      )
    )
  
  category_summary <- consequence_summary %>%
    group_by(Category) %>%
    summarise(
      Total_Mentions = sum(Count),
      Avg_Percentage = mean(Percentage),
      .groups = 'drop'
    )
  
  cat("\nSummary by category:\n")
  print(category_summary)
  
  consequences_processed <- consequences_processed %>%
    mutate(
      n_consequences = str_count(consequences, ",") + 1,
      n_positive = cons_interesting_ads + cons_easier_sharing + cons_coupons + 
        cons_better_search + cons_improved_services + cons_personalized,
      n_negative = cons_annoying_ads + cons_loss_control + cons_hacking + 
        cons_identity_theft + cons_cyberstalking
    )
  
  cat("\nDistribution of consequences selected:\n")
  cat("  Mean total consequences:", mean(consequences_processed$n_consequences), "\n")
  cat("  Mean positive consequences:", mean(consequences_processed$n_positive), "\n")
  cat("  Mean negative consequences:", mean(consequences_processed$n_negative), "\n")
  
  cat("\nStep 4: Analyzing valence patterns...\n")
  
  consequences_processed <- consequences_processed %>%
    mutate(
      consequence_valence = case_when(
        n_positive > 0 & n_negative == 0 ~ "Positive only",
        n_positive == 0 & n_negative > 0 ~ "Negative only", 
        n_positive > 0 & n_negative > 0 ~ "Mixed",
        TRUE ~ "Unknown"
      )
    )
  
  valence_table <- table(consequences_processed$consequence_valence)
  cat("\nValence distribution:\n")
  print(valence_table)
  
  cat("\nValence proportions:\n")
  print(prop.table(valence_table))
  
  
  # ============================================================================
  # TABLE 5: DATA SHARING TREATMENT EFFECTS
  # ============================================================================
  
  cat("\n=== Table 5: Data Sharing ===\n")
  
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
      any_shared = ifelse(total_shared > 0, 1, 0)
    )
  
  cat("  Analysis sample:", nrow(data_sharing_analysis), "participants\n")
  cat("  Treatment groups: Control =", sum(data_sharing_analysis$experiment_condition == "control"),
      ", Info =", sum(data_sharing_analysis$experiment_condition == "info"),
      ", Saliency =", sum(data_sharing_analysis$experiment_condition == "saliency"), "\n")
  
  # Summary statistics by treatment group
  summary_by_treatment <- data_sharing_analysis %>%
    group_by(experiment_condition) %>%
    summarise(
      n = n(),
      share_1_pct = round(mean(data_sharing_1_binary) * 100, 1),
      share_2_pct = round(mean(data_sharing_2_binary) * 100, 1),
      share_3_pct = round(mean(data_sharing_3_binary) * 100, 1),
      share_4_pct = round(mean(data_sharing_4_binary) * 100, 1),
      any_share_pct = round(mean(any_shared) * 100, 1),
      mean_fields_shared = round(mean(total_shared), 2),
      .groups = 'drop'
    )
  
  cat("\nSharing rates by treatment (% who shared):\n")
  print(summary_by_treatment)
  
  # Set factor levels: saliency before information (matches paper)
  data_sharing_analysis <- data_sharing_analysis %>%
    mutate(experiment_condition = factor(experiment_condition, 
                                         levels = c("control", "saliency", "info")))
  data_sharing_analysis <- data_sharing_analysis %>%
    mutate(wave_id = ifelse(wave_id == 3, 2, wave_id)) %>%
    mutate(block_by_wave = paste(wave_id, block_idx, sep = '_'))
  
  ## [WEIGHT MODIFICATION] join weights
  ## Use experiment_id_aux since that's the column from experiment_conditions join
  data_sharing_analysis <- data_sharing_analysis %>%
    rename(experiment_id = experiment_id_aux)
  data_sharing_analysis <- join_weights(data_sharing_analysis)
  
  ## [WEIGHT MODIFICATION] Table 5 regressions: felm -> run_weighted_feols with cluster SE
  model_share1 <- run_weighted_feols(
    data_sharing_1_binary ~ experiment_condition | block_by_wave,
    cluster = ~experiment_id,
    data = data_sharing_analysis)
  model_share2 <- run_weighted_feols(
    data_sharing_2_binary ~ experiment_condition | block_by_wave,
    cluster = ~experiment_id,
    data = data_sharing_analysis)
  model_share3 <- run_weighted_feols(
    data_sharing_3_binary ~ experiment_condition | block_by_wave,
    cluster = ~experiment_id,
    data = data_sharing_analysis)
  model_share4 <- run_weighted_feols(
    data_sharing_4_binary ~ experiment_condition | block_by_wave,
    cluster = ~experiment_id,
    data = data_sharing_analysis)
  
  dict <- c(
    "experiment_conditioninfo"      = "Information Treatment",
    "experiment_conditionsaliency"  = "Saliency Treatment",
    "(Intercept)"     = "Constant",
    "Intercept"       = "Constant"
  )
  
  ## [WEIGHT MODIFICATION] add OUTPUT_SUFFIX to filename
  etable(model_share1, model_share2, model_share3, model_share4,
         headers = c("Deleted Cookies", "Opted out", "Withheld data", "Changed privacy settings"),
         dict = dict,
         tex = TRUE,
         title = "Treatment Effect on Self-Reported Data Sharing Behavior",
         file = paste0(TABLES_DIR, "data_sharing_treatment_effects", OUTPUT_SUFFIX, ".tex"))
  
  
  # ============================================================================
  # TABLE A4: EXPERIMENTER DEMAND (Changes in Experimental Behavior)
  # ============================================================================
  
  cat("\n=== Table A4: Experimenter Demand ===\n")
  
  survey_merged <- survey_merged %>%
    mutate(
      annoying_1_numeric = case_when(
        annoying_1 == "Not at all" ~ 0,
        annoying_1 == "2" ~ 0.25,
        annoying_1 == "3" ~ 0.5,
        annoying_1 == "4" ~ 0.75,
        annoying_1 == "Always" ~ 1,
        TRUE ~ as.numeric(annoying_1)
      ),
      annoying_2_numeric = case_when(
        annoying_2 == "Not at all" ~ 0,
        annoying_2 == "2" ~ 0.25,
        annoying_2 == "3" ~ 0.5,
        annoying_2 == "4" ~ 0.75,
        annoying_2 == "Always" ~ 1,
        TRUE ~ as.numeric(annoying_2)
      ),
    )
  
  # Set factor levels: saliency before information (matches paper)
  survey_merged <- survey_merged %>%
    mutate(experiment_condition = factor(experiment_condition, 
                                         levels = c("control", "saliency", "info")))
  
  ## [WEIGHT MODIFICATION] join weights for experimenter demand
  ## Need experiment_id column; use experiment_id_aux from the join
  experimenter_demand_data <- survey_merged %>%
    filter(experiment_condition %in% c("info", "control", "saliency")) %>%
    rename(experiment_id = experiment_id_aux)
  experimenter_demand_data <- join_weights(experimenter_demand_data)
  
  ## [WEIGHT MODIFICATION] Table A4 regressions
  t1 <- run_weighted_feols(
    annoying_1_numeric ~ experiment_condition,
    data = experimenter_demand_data)
  t2 <- run_weighted_feols(
    annoying_2_numeric ~ experiment_condition,
    data = experimenter_demand_data)
  
  dict <- c(
    "experiment_conditioninfo"      = "Information Treatment",
    "experiment_conditionsaliency"  = "Saliency Treatment",
    "(Intercept)"     = "Constant"
  )
  
  ## [WEIGHT MODIFICATION] add OUTPUT_SUFFIX to filename
  etable(t1, t2,
         headers = c("Disrupted Browsing", "Evaded Extension/Chrome"),
         dict = dict,
         tex = TRUE,
         depvar = FALSE,
         file = paste0(TABLES_DIR, "experiment_modified_behavior", OUTPUT_SUFFIX, ".tex"))
  
} # end weight spec loop