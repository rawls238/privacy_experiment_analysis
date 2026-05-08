rm(list = ls())
# Alternative path for Guy
#WD <- "/Users/guyaridor/Dropbox/Privacy-Experiment/spring2025experiment"

if (Sys.info()[['nodename']] == 'GSB-P4FVDL7QF6'){
  WD <- "/Users/sggold/Library/CloudStorage/Dropbox/Shared-Project-Folders/Privacy-Experiment/spring2025experiment"
} else {
  WD <- "/Users/guyaridor/Dropbox/Privacy-Experiment/spring2025experiment"
}
#setwd(WD)

# Set working directory and load required libraries
WD <- "/Users/markli/Library/CloudStorage/Dropbox/spring2025experiment"
setwd(WD)

# Load required libraries
library(tidyverse)
library(lubridate)
library(lfe)
library(stargazer)
library(xtable)
library(stringr)
library(acs)

source("code/utils/plot_rules.R")

# ============================================================================
# PEW BENCHMARK CONFIGURATION
# Note: These values appear to be from a PEW Research privacy survey
# Ideally these should be loaded from PEW's data directly
# ============================================================================

# PEW_BENCHMARKS <- list(
#   immediate_accept = list(
#     always_often = 56/96,
#     sometimes = 22/96,
#     rarely_never = 18/96,
#     n = 5101 * 0.96
#   ),
#   policy_effective = list(
#     extremely_very = 7/95,
#     somewhat = 27/95,
#     not_effective = 61/95,
#     n = 5101 * 0.95
#   ),
#   extensive_margin = list(
#     yes = 51/100,
#     no = 49/100,
#     n = 5101
#   )
# )

PEW_BENCHMARKS <- list(
  immediate_accept = list(
    always_often = 57/97,
    sometimes = 22/97,
    rarely_never = 18/97,
    n = 5101 * 0.97
  ),
  policy_effective = list(
    extremely_very = 8/96,
    somewhat = 27/96,
    not_effective = 61/96,
    n = 5101 * 0.96
  ),
  extensive_margin = list(
    yes = 49/96,
    no = 47/96,
    n = 5101 * 0.96
  )
)

# Set up directories (relative to spring2025experiment)
FIGURES_DIR <- "results/baseline_survey_descriptives/"
TABLES_DIR <- "results/baseline_survey_descriptives/"

# Create directory structure INCLUDING balance subdirectory AND tables subdirectory
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(FIGURES_DIR, "balance/"), showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(TABLES_DIR, "tables/"), showWarnings = FALSE, recursive = TRUE)

# Write PEW benchmarks to TeX for use in papers
sink(paste0(TABLES_DIR, "tables/pew_benchmarks.tex"))
cat("% PEW Benchmark Values from PEW Research Privacy Survey\n")
cat("% Note: These should ideally be loaded from PEW data directly\n\n")
cat("% Immediately Accept Terms and Conditions\n")
cat(sprintf("\\newcommand{\\PEWImmediateAlwaysOften}{%.1f}\n", PEW_BENCHMARKS$immediate_accept$always_often * 100))
cat(sprintf("\\newcommand{\\PEWImmediateSometimes}{%.1f}\n", PEW_BENCHMARKS$immediate_accept$sometimes * 100))
cat(sprintf("\\newcommand{\\PEWImmediateRarelyNever}{%.1f}\n", PEW_BENCHMARKS$immediate_accept$rarely_never * 100))
cat(sprintf("\\newcommand{\\PEWImmediateN}{%.0f}\n", PEW_BENCHMARKS$immediate_accept$n))
cat("\n% Privacy Policies Effectiveness\n")
cat(sprintf("\\newcommand{\\PEWEffectiveExtremelyVery}{%.1f}\n", PEW_BENCHMARKS$policy_effective$extremely_very * 100))
cat(sprintf("\\newcommand{\\PEWEffectiveSomewhat}{%.1f}\n", PEW_BENCHMARKS$policy_effective$somewhat * 100))
cat(sprintf("\\newcommand{\\PEWEffectiveNotEffective}{%.1f}\n", PEW_BENCHMARKS$policy_effective$not_effective * 100))
cat(sprintf("\\newcommand{\\PEWEffectiveN}{%.0f}\n", PEW_BENCHMARKS$policy_effective$n))
cat("\n% Extensive Margin\n")
cat(sprintf("\\newcommand{\\PEWExtensiveYes}{%.1f}\n", PEW_BENCHMARKS$extensive_margin$yes * 100))
cat(sprintf("\\newcommand{\\PEWExtensiveNo}{%.1f}\n", PEW_BENCHMARKS$extensive_margin$no * 100))
cat(sprintf("\\newcommand{\\PEWExtensiveN}{%.0f}\n", PEW_BENCHMARKS$extensive_margin$n))
sink()

# ============================================================================
# DEFINE ALL HELPER FUNCTIONS
# ============================================================================

belief_to_internal <- c(
  # beliefscollection → collect
  beliefscollection_r1 = "collect-log",
  beliefscollection_r2 = "collect-bio",
  beliefscollection_r3 = "collect-sensitive",
  beliefscollection_r4 = "collect-financial",
  beliefscollection_r5 = "collect-offsite",
  beliefscollection_r6 = "collect-location",
  beliefscollection_r7 = "collect-social",
  
  # beliefsuse → share / related
  beliefsuse_r1 = "anonymized-anonymized",
  beliefsuse_r2 = "share-social",
  beliefsuse_r3 = "share-finance",
  beliefsuse_r4 = "share-ads",
  beliefsuse_r5 = "share-law",
  beliefsuse_r6 = "share-service",
  beliefsuse_r7 = "share-partners",
  beliefsuse_r8 = "personalization-personalization",
  
  # beliefscontrol → control-related
  beliefscontrol_r1 = "change-change",
  beliefscontrol_r2 = "automated-automated",
  beliefscontrol_r3 = "deletion-deletion",
  beliefscontrol_r4 = "storage-storage",
  
  # beliefsquality
  beliefsquality_r1 = "quality-content",
  beliefsquality_r2 = "quality-ads",
  beliefsquality_r3 = "quality-navigation"
)

labels <- c(
  beliefscollection_r1 = "Collects Browsing Behavior on Site",
  beliefscollection_r2 = "Collects Demographic Data",
  beliefscollection_r3 = "Collects Sensitive Personal Information",
  beliefscollection_r4 = "Collects Financial Data",
  beliefscollection_r5 = "Collects Browsing Behavior on Other Sites",
  beliefscollection_r6 = "Collects Location Data",
  beliefscollection_r7 = "Collects Social Media Profiles",
  
  beliefsuse_r1 = "Share Data Only After Anonymized",
  beliefsuse_r2 = "Share Data with Social Media",
  beliefsuse_r3 = "Share Data with Financial Providers",
  beliefsuse_r4 = "Share Data with Advertisers",
  beliefsuse_r5 = "Share Data with Law Enforcement",
  beliefsuse_r6 = "Share Data with Service Providers",
  beliefsuse_r7 = "Share Data with Non-Service Providers",
  beliefsuse_r8 = "Use Data to Personalize Experience",
  
  beliefscontrol_r1 = "Notified of Policy Changes",
  beliefscontrol_r2 = "Data Auto-Deleted After Time Period",
  beliefscontrol_r3 = "Can Delete Data on Request",
  beliefscontrol_r4 = "Data Stored Securely and Anonymized",
  
  beliefsquality_r1 = "High Quality Content",
  beliefsquality_r2 = "Obstructive or Annoying Advertising",
  beliefsquality_r3 = "Easy to Use and Navigate"
)

# Census data function
get_census_data <- function() {
  api.key.install("5a55e425c21ed4b512cdc03fab445dcb00f32892")
  
  geo <- geo.make(state = "*")
  
  age_gender_data <- acs.fetch(geography = geo, 
                               table.number = "B01001", 
                               endyear = 2021, 
                               span = 5)
  age_gender_df <- as.data.frame(age_gender_data@estimate)
  age_gender_df <- age_gender_df %>% 
    rownames_to_column("state") %>% 
    pivot_longer(cols = -state, names_to = "variable", values_to = "value")
  
  income_data <- acs.fetch(geography = geo, 
                           table.number = "B19001", 
                           endyear = 2021, 
                           span = 5)
  income_df <- as.data.frame(income_data@estimate)
  income_df <- income_df %>% 
    rownames_to_column("state") %>% 
    pivot_longer(cols = -state, names_to = "variable", values_to = "value")
  
  ethnicity_data <- acs.fetch(geography = geo, 
                              table.number = "B03002", 
                              endyear = 2021, 
                              span = 5)
  ethnicity_df <- as.data.frame(ethnicity_data@estimate)
  ethnicity_df <- ethnicity_df %>% 
    rownames_to_column("state") %>% 
    pivot_longer(cols = -state, names_to = "variable", values_to = "value")
  
  age_gender_df <- age_gender_df %>%
    mutate(original_age_group = case_when(
      variable == "B01001_003" ~ "Under 5",
      variable == "B01001_004" ~ "5 to 9",
      variable == "B01001_005" ~ "10 to 14",
      variable == "B01001_006" ~ "15 to 17",
      variable == "B01001_007" ~ "18 to 19",
      variable == "B01001_008" ~ "20",
      variable == "B01001_009" ~ "21",
      variable == "B01001_010" ~ "22 to 24",
      variable == "B01001_011" ~ "25 to 29",
      variable == "B01001_012" ~ "30 to 34",
      variable == "B01001_013" ~ "35 to 39",
      variable == "B01001_014" ~ "40 to 44",
      variable == "B01001_015" ~ "45 to 49",
      variable == "B01001_016" ~ "50 to 54",
      variable == "B01001_017" ~ "55 to 59",
      variable == "B01001_018" ~ "60 to 61",
      variable == "B01001_019" ~ "62 to 64",
      variable == "B01001_020" ~ "65 to 66",
      variable == "B01001_021" ~ "67 to 69",
      variable == "B01001_022" ~ "70 to 74",
      variable == "B01001_023" ~ "75 to 79",
      variable == "B01001_024" ~ "80 to 84",
      variable == "B01001_025" ~ "85 and over"
    ),
    
    mapped_age_group = case_when(
      # Male 18-24: 007-010, Female 18-24: 031-034
      variable %in% c("B01001_007", "B01001_008", "B01001_009", "B01001_010",
                      "B01001_031", "B01001_032", "B01001_033", "B01001_034") ~ "18-24",
      # Male 25-29: 011, Female 25-29: 035
      variable %in% c("B01001_011", "B01001_035") ~ "25-29",
      # Male 30-34: 012, Female 30-34: 036
      variable %in% c("B01001_012", "B01001_036") ~ "30-34",
      # Male 35-39: 013, Female 35-39: 037
      variable %in% c("B01001_013", "B01001_037") ~ "35-39",
      # Male 40-44: 014, Female 40-44: 038
      variable %in% c("B01001_014", "B01001_038") ~ "40-44",
      # Male 45-49: 015, Female 45-49: 039
      variable %in% c("B01001_015", "B01001_039") ~ "45-49",
      # Male 50-54: 016, Female 50-54: 040
      variable %in% c("B01001_016", "B01001_040") ~ "50-54",
      # Male 55-59: 017, Female 55-59: 041
      variable %in% c("B01001_017", "B01001_041") ~ "55-59",
      # Male 60-64: 018-019, Female 60-64: 042-043
      variable %in% c("B01001_018", "B01001_019",
                      "B01001_042", "B01001_043") ~ "60-64",
      # Male 65-70: 020-021, Female 65-70: 044-045
      variable %in% c("B01001_020", "B01001_021",
                      "B01001_044", "B01001_045") ~ "65-70",
      # Male 70-74: 022, Female 70-74: 046
      variable %in% c("B01001_022", "B01001_046") ~ "70-74",
      # Male 75+: 023-025, Female 75+: 047-049
      variable %in% c("B01001_023", "B01001_024", "B01001_025",
                      "B01001_047", "B01001_048", "B01001_049") ~ "75+"
    )) %>%
    filter(!is.na(mapped_age_group) & !is.na(original_age_group)) %>%
    group_by(mapped_age_group, original_age_group) %>%
    summarise(estimate = sum(value), .groups = 'drop') %>%
    mutate(proportion = estimate / sum(estimate))
  
  income_df <- income_df %>%
    mutate(census_income = case_when(
      variable == "B19001_002" ~ "< $10,000",
      variable == "B19001_003" ~ "$10,001 - $14,999",
      variable == "B19001_004" ~ "$15,000 - $19,999",
      variable == "B19001_005" ~ "$20,000 - $24,999",
      variable == "B19001_006" ~ "$25,000 - $29,999",
      variable == "B19001_007" ~ "$30,000 - $34,999",
      variable == "B19001_008" ~ "$35,000 - $39,999",
      variable == "B19001_009" ~ "$40,000 - $44,999",
      variable == "B19001_010" ~ "$45,000 - $49,999",
      variable == "B19001_011" ~ "$50,000 - $59,999",
      variable == "B19001_012" ~ "$60,000 - $74,999",
      variable == "B19001_013" ~ "$75,000 - $99,999",
      variable == "B19001_014" ~ "$100,000 - $124,999",
      variable == "B19001_015" ~ "$125,000 - $149,999",
      variable == "B19001_016" ~ "$150,000 - $199,999",
      variable == "B19001_017" ~ "$200,000+",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(census_income)) %>%
    group_by(census_income) %>%
    summarise(estimate = sum(value), .groups = 'drop') %>%
    mutate(proportion = estimate / sum(estimate))
  
  # fixed (whilelist for better transparent)
  ethnicity_df <- ethnicity_df %>%
    filter(variable %in% c("B03002_003", "B03002_004", "B03002_005", "B03002_006",
                           "B03002_007", "B03002_008", "B03002_009",
                           "B03002_013", "B03002_014", "B03002_015", "B03002_016",
                           "B03002_017", "B03002_018", "B03002_019")) %>%
    mutate(variable = case_when(
      variable == "B03002_003" ~ "White",
      variable == "B03002_004" ~ "Black or African American",
      variable == "B03002_005" ~ "American Indian or Alaska Native",
      variable == "B03002_006" ~ "Asian",
      TRUE ~ "Other (please specify)"
    )) %>%
    group_by(variable) %>%
    summarise(estimate = sum(value), .groups = 'drop') %>%
    mutate(proportion = estimate / sum(estimate))
  
  education_data <- acs.fetch(geography = geo, 
                              table.number = "B15003", 
                              endyear = 2021, 
                              span = 5)
  education_df <- as.data.frame(education_data@estimate)
  education_df <- education_df %>% 
    rownames_to_column("state") %>% 
    pivot_longer(cols = -state, names_to = "variable", values_to = "value")
  
  education_df <- education_df %>%
    mutate(variable = case_when(
      variable %in% c("B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018") ~ "Less than College",
      variable %in% c("B15003_019", "B15003_020", "B15003_021") ~ "Some College",
      variable %in% c("B15003_022", "B15003_023", "B15003_024", "B15003_025") ~ "Bachelor's Degree or Higher",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(variable))
  
  education_df <- education_df %>%
    group_by(variable) %>%
    summarise(estimate = sum(value), .groups = 'drop') %>%
    mutate(proportion = estimate / sum(estimate))
  
  gender_data <- acs.fetch(geography = geo, 
                           table.number = "B01001", 
                           endyear = 2021, 
                           span = 5)
  gender_df <- as.data.frame(gender_data@estimate)
  gender_df <- gender_df %>% 
    rownames_to_column("state") %>% 
    pivot_longer(cols = -state, names_to = "variable", values_to = "value")
  
  gender_df <- gender_df %>%
    filter(variable %in% c("B01001_002", "B01001_026") & !is.na(variable)) %>%
    mutate(variable = case_when(
      variable == "B01001_002" ~ "Male",
      variable == "B01001_026" ~ "Female",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(variable))
  
  gender_df <- gender_df %>%
    group_by(variable) %>%
    summarise(estimate = sum(value), .groups = 'drop') %>%
    mutate(proportion = estimate / sum(estimate))
  
  return(list(age_data = age_gender_df,
              gender_data = gender_df,
              income_data = income_df,
              education_data = education_df,
              ethnicity_data = ethnicity_df))
}

# PEW attitudes analysis function - now uses PEW_BENCHMARKS configuration
get_privacy_attitude_pew_df <- function(df) {
  
  get_pew_agg_immediate <- function(agg_df, condition = FALSE) {
    if (condition) {
      agg_df <- agg_df %>% filter(experiment_condition != "")
    }
    privacy_policy_immediate <- agg_df %>% 
      filter(privacy.policy.pew != "") %>% 
      mutate(total = n()) %>% 
      group_by(privacy.policy.pew) %>% 
      summarise(proportion = n() / first(total)) %>% 
      ungroup()
    
    privacy_policy_immediate <- privacy_policy_immediate %>%
      mutate(
        privacy.policy.pew = case_when(
          privacy.policy.pew %in% c("Always or almost always", "Often") ~ "Always/Often",
          privacy.policy.pew %in% c("Rarely", "Never") ~ "Rarely/Never",
          TRUE ~ privacy.policy.pew
        )
      ) %>%
      group_by(privacy.policy.pew) %>%
      summarise(proportion = sum(proportion)) %>% ungroup()
    return(privacy_policy_immediate)
  }
  privacy_policy_immediate_entry_sample <- get_pew_agg_immediate(df, FALSE)
  privacy_policy_immediate_entry_sample <- privacy_policy_immediate_entry_sample %>% rename(proportion_entry = proportion)
  privacy_policy_immediate_exp_sample <- get_pew_agg_immediate(df, TRUE)
  privacy_policy_immediate_exp_sample <- privacy_policy_immediate_exp_sample %>% rename(proportion_exp= proportion)
  privacy_policy_immediate <- privacy_policy_immediate_exp_sample %>% left_join(privacy_policy_immediate_entry_sample, by="privacy.policy.pew")
  
  # Use PEW_BENCHMARKS instead of hardcoded values
  privacy_policy_immediate$pew <- c(
    PEW_BENCHMARKS$immediate_accept$always_often, 
    PEW_BENCHMARKS$immediate_accept$rarely_never, 
    PEW_BENCHMARKS$immediate_accept$sometimes
  )
  privacy_policy_immediate$score <- c(0.0, 1.0, 0.5)
  privacy_policy_immediate$label <- "Immediately Accept Terms"
  privacy_policy_immediate$npew <- PEW_BENCHMARKS$immediate_accept$n
  privacy_policy_immediate$nsurvey_overall <- nrow(df %>% filter(privacy.policy.pew != ""))
  privacy_policy_immediate$nsurvey_exp <- nrow(df %>% filter(privacy.policy.pew != "" & experiment_condition != ""))
  privacy_policy_immediate <- privacy_policy_immediate %>% rename(raw_label = privacy.policy.pew)
  
  ## privacy policies are effective
  get_pew_effective <- function(agg_df, condition = FALSE) {
    if (condition) {
      agg_df <- agg_df %>% filter(experiment_condition != "")
    }
    privacy_policy_effective <- agg_df %>% 
      filter(privacy.pol.eff.pew != "") %>% 
      mutate(total = n()) %>% 
      group_by(privacy.pol.eff.pew) %>% 
      summarise(proportion = n() / first(total)) %>% 
      ungroup()
    
    privacy_policy_effective <- privacy_policy_effective %>%
      mutate(
        privacy.pol.eff.pew = case_when(
          privacy.pol.eff.pew %in% c("Extremely effective", "Very effective") ~ "Extremely/Very effective",
          privacy.pol.eff.pew %in% c("Not at all effective", "Not too effective") ~ "Not too/Not at all effective",
          TRUE ~ privacy.pol.eff.pew
        )
      ) %>%
      group_by(privacy.pol.eff.pew) %>%
      summarise(proportion = sum(proportion)) %>% ungroup()
    return(privacy_policy_effective)
  }
  privacy_policy_effective_entry_sample <- get_pew_effective(df, FALSE)
  privacy_policy_effective_entry_sample <- privacy_policy_effective_entry_sample %>% rename(proportion_entry = proportion)
  privacy_policy_effective_exp_sample <- get_pew_effective(df, TRUE)
  privacy_policy_effective_exp_sample <- privacy_policy_effective_exp_sample  %>% rename(proportion_exp= proportion)
  privacy_policy_effective <- privacy_policy_effective_exp_sample %>% left_join(privacy_policy_effective_entry_sample, by="privacy.pol.eff.pew")
  
  # Use PEW_BENCHMARKS instead of hardcoded values
  privacy_policy_effective$pew <- c(
    PEW_BENCHMARKS$policy_effective$extremely_very,
    PEW_BENCHMARKS$policy_effective$not_effective,
    PEW_BENCHMARKS$policy_effective$somewhat
  )
  privacy_policy_effective$score <- c(0.0, 1.0, 0.5)
  privacy_policy_effective$label <- "Privacy Policies Effective"
  privacy_policy_effective$npew <- PEW_BENCHMARKS$policy_effective$n
  privacy_policy_effective$nsurvey_overall <- nrow(df %>% filter(privacy.pol.eff.pew != ""))
  privacy_policy_effective$nsurvey_exp <- nrow(df %>% filter(privacy.pol.eff.pew != "" & experiment_condition != ""))
  privacy_policy_effective <- privacy_policy_effective %>% rename(raw_label = privacy.pol.eff.pew)
  
  ### extensive margin question
  get_pew_extensive <- function(agg_df, condition = FALSE) {
    if (condition) {
      agg_df <- agg_df %>% filter(experiment_condition != "")
    }
    extensive_margin_pew <- agg_df %>% 
      filter(extensive.margin.pew != "") %>% 
      mutate(total = n()) %>% 
      group_by(extensive.margin.pew) %>% 
      summarise(proportion = n() / first(total)) %>% 
      ungroup()
    return(extensive_margin_pew)
  }
  
  extensive_margin_pew_entry_sample <- get_pew_extensive(df, FALSE)
  extensive_margin_pew_entry_sample <- extensive_margin_pew_entry_sample %>% rename(proportion_entry = proportion)
  privacy_policy_extensive_exp_sample <- get_pew_extensive(df, TRUE)
  privacy_policy_extensive_exp_sample <- privacy_policy_extensive_exp_sample  %>% rename(proportion_exp= proportion)
  extensive_margin_pew <- privacy_policy_extensive_exp_sample %>% left_join(extensive_margin_pew_entry_sample, by="extensive.margin.pew")
  
  # Use PEW_BENCHMARKS instead of hardcoded values
  extensive_margin_pew$pew <- c(
    PEW_BENCHMARKS$extensive_margin$yes,
    PEW_BENCHMARKS$extensive_margin$no
  )
  extensive_margin_pew$score <- c(0.0, 1.0)
  extensive_margin_pew$npew <- PEW_BENCHMARKS$extensive_margin$n
  extensive_margin_pew$nsurvey_overall <- nrow(df %>% filter(extensive.margin.pew != ""))
  extensive_margin_pew$nsurvey_exp <- nrow(df %>% filter(extensive.margin.pew != "" & experiment_condition != ""))
  extensive_margin_pew$label <- "Extensive Margin Privacy Policies"
  extensive_margin_pew <- extensive_margin_pew %>% rename(raw_label = extensive.margin.pew)
  privacy_attitudes_pew <- rbind(extensive_margin_pew, privacy_policy_effective, privacy_policy_immediate)
  return(privacy_attitudes_pew)
}

# Compare survey to ACS function
compare_survey_to_acs <- function(demographic_values, acs_data, demographic_variable, acs_variable_name, demographic_variable_str, order_levels = NULL, experiment_data = NULL) {
  survey_proportion <- demographic_values %>%
    filter(!!sym(demographic_variable) != "" & 
             !!sym(demographic_variable) != "Prefer not to say" &
             !!sym(demographic_variable) != "Prefer not to answer") %>%
    mutate(total_entries = n()) %>%
    group_by(!!sym(demographic_variable)) %>%
    summarise(survey_proportion = n() / first(total_entries)) %>%
    ungroup()
  
  acs_proportion <- acs_data %>%
    rename(acs_proportion = proportion, category = !!sym(acs_variable_name)) %>%
    group_by(category) %>%
    summarise(acs_proportion = sum(acs_proportion)) %>%
    ungroup()
  
  comparison_data <- full_join(acs_proportion, survey_proportion, by = c("category" = demographic_variable))
  
  # If experiment_data provided, compute experiment proportions
  if (!is.null(experiment_data)) {
    exp_proportion <- experiment_data %>%
      filter(!!sym(demographic_variable) != "" & 
               !!sym(demographic_variable) != "Prefer not to say" &
               !!sym(demographic_variable) != "Prefer not to answer") %>%
      mutate(total_entries = n()) %>%
      group_by(!!sym(demographic_variable)) %>%
      summarise(experiment_proportion = n() / first(total_entries)) %>%
      ungroup()
    comparison_data <- full_join(comparison_data, exp_proportion, by = c("category" = demographic_variable))
  }
  
  comparison_data[is.na(comparison_data)] <- 0
  
  if (!is.null(order_levels)) {
    comparison_data$category <- factor(comparison_data$category, levels = order_levels, ordered = TRUE)
  }
  
  # Pivot columns depending on whether experiment_data is present
  # Color scheme: Green=ACS (external benchmark), Blue=Survey, Red=Extension
  if (!is.null(experiment_data)) {
    comparison_data_long <- comparison_data %>%
      pivot_longer(cols = c(acs_proportion, survey_proportion, experiment_proportion), names_to = "source", values_to = "proportion") %>%
      mutate(source = factor(recode(source, acs_proportion = "ACS", survey_proportion = "Survey", experiment_proportion = "Extension"),
                             levels = c("Survey", "Extension", "ACS")))
    fill_values <- BENCHMARK_COLORS
  } else {
    comparison_data_long <- comparison_data %>%
      pivot_longer(cols = c(acs_proportion, survey_proportion), names_to = "source", values_to = "proportion") %>%
      mutate(source = factor(recode(source, acs_proportion = "ACS", survey_proportion = "Survey"),
                             levels = c("Survey", "ACS")))
    fill_values <- BENCHMARK_COLORS
  }
  
  p <- ggplot(comparison_data_long, aes(x = category, y = proportion, fill = source)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.85) +
    labs(x = demographic_variable_str,
         y = "Proportion",
         fill = "Source") +
    scale_fill_manual(values = fill_values) +
    theme_privacy_experiment(show_grid_x = FALSE) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0.01))
  
  return(p)
}

compute_ground_truth_privacy_distribution <- function() {
  privacy_info <- read.csv("data/final_extension_data/privacy_info.csv")
  privacy_info <- privacy_info %>% mutate(rating_numeric = ifelse(rating == "Yes", 1, 0))
  t <- privacy_info %>% group_by(feature, field) %>% summarise(mean_rating = mean(rating_numeric), .groups="drop")
  return(t)
}

# ============================================================================
# LOAD MAIN DATASETS
# ============================================================================

# Load the merged dataset
survey_merged <- read.csv("data/Survey/survey_merged_final.csv", stringsAsFactors = FALSE)

#### SECTION 1: Compute representativeness on demographics ####
# Demographics mapping
if("Age" %in% names(survey_merged)) {
  survey_merged <- survey_merged %>%
    mutate(
      age_range = case_when(
        Age >= 18 & Age <= 24 ~ "18-24",
        Age >= 25 & Age <= 29 ~ "25-29",
        Age >= 30 & Age <= 34 ~ "30-34",
        Age >= 35 & Age <= 39 ~ "35-39",
        Age >= 40 & Age <= 44 ~ "40-44",
        Age >= 45 & Age <= 49 ~ "45-49",
        Age >= 50 & Age <= 54 ~ "50-54",
        Age >= 55 & Age <= 59 ~ "55-59",
        Age >= 60 & Age <= 64 ~ "60-64",
        Age >= 65 & Age <= 70 ~ "65-70",
        Age >= 70 & Age <= 74 ~ "70-74",
        Age >= 75 ~ "75+",
        TRUE ~ NA_character_
      )
    )
}

if("Education" %in% names(survey_merged)) {
  survey_merged <- survey_merged %>%
    mutate(
      formatted_education = case_when(
        Education %in% c(1, 2, 3) ~ "Less than College",
        Education == 4 | Education == 5 | Education == 6 ~ "Some College",
        Education >= 7 ~ "Bachelor's Degree or Higher",
        TRUE ~ NA_character_
      )
    )
}

if("Gender" %in% names(survey_merged)) {
  survey_merged <- survey_merged %>%
    mutate(
      gender = case_when(
        Gender == 1 ~ "Male",
        Gender == 2 ~ "Female",
        TRUE ~ NA_character_
      )
    )
}

if("Income" %in% names(survey_merged)) {
  survey_merged <- survey_merged %>%
    mutate(
      income_full = case_when(
        Income == 1  ~ "< $10,000",
        Income == 2  ~ "$10,000 - $19,999",
        Income == 3  ~ "$20,000 - $29,999",
        Income == 4  ~ "$30,000 - $49,999",
        Income == 5  ~ "$50,000 - $59,999",
        Income == 6  ~ "$60,000 - $74,999",
        Income == 7  ~ "$75,000 - $99,999",
        Income == 8  ~ "$100,000 - $124,999",
        Income == 9  ~ "$125,000 - $149,999",
        Income == 10 ~ "$150,000 - $199,999",
        Income == 11 ~ "Greater than $200,000",
        Income == 12 ~ "Prefer not to answer",
        TRUE ~ NA_character_
      )
    )
}

demo_summary <- survey_merged %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE),
    pct_female = mean(Gender == 2, na.rm = TRUE) * 100,
    pct_college = mean(Education >= 7, na.rm = TRUE) * 100,
    n_respondents = n()
  )

# Write demo_summary to TeX
sink(paste0(TABLES_DIR, "tables/demo_summary.tex"))
stargazer(demo_summary, type = "latex", title = "Demographic Summary Statistics",
          label = "tab:demo_summary", summary = FALSE,
          digits = 2, header = FALSE)
sink()

acs_data <- get_census_data()
age_data <- acs_data$age_data
gender_data <- acs_data$gender_data
education_data <- acs_data$education_data
income_data <- acs_data$income_data
ethnicity_data <- acs_data$ethnicity_data

income_data <- income_data %>%
  mutate(
    census_income_buckets = case_when(
      census_income %in% c("< $10,000") ~ "< $10,000",
      census_income %in% c("$10,001 - $14,999", "$15,000 - $19,999") ~ "$10,000 - $19,999",
      census_income %in% c("$20,000 - $24,999", "$25,000 - $29,999") ~ "$20,000 - $29,999",
      census_income %in% c("$30,000 - $34,999", "$35,000 - $39,999",
                           "$40,000 - $44,999", "$45,000 - $49,999") ~ "$30,000 - $49,999",
      census_income %in% c("$50,000 - $59,999") ~ "$50,000 - $59,999",
      census_income %in% c("$60,000 - $74,999") ~ "$60,000 - $74,999",
      census_income %in% c("$75,000 - $99,999") ~ "$75,000 - $99,999",
      census_income %in% c("$100,000 - $124,999") ~ "$100,000 - $124,999",
      census_income %in% c("$125,000 - $149,999") ~ "$125,000 - $149,999",
      census_income %in% c("$150,000 - $199,999") ~ "$150,000 - $199,999",
      census_income %in% c("$200,000+") ~ "Greater than $200,000",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(census_income_buckets) %>%
  summarise(
    estimate = sum(estimate, na.rm = TRUE),
    proportion = sum(proportion, na.rm = TRUE),
    .groups = "drop"
  ) 


demographic_cols <- c("Age", "Gender", "Education", "gender", "formatted_education", "age_range", "income_full")

# Load experiment_users early so we can show experiment bar in demographic balance plots
experiment_users <- read.csv("data/final_extension_data/experiment_conditions_pilot_july_2024.csv", 
                             stringsAsFactors = FALSE)
experiment_users <- experiment_users %>% select(email, experiment_id, experiment_condition, in_experiment, cutoff_wta) %>% rename(emailid = email)
survey_merged <- survey_merged %>% left_join(experiment_users, by="emailid")

demographic_values <- survey_merged[,demographic_cols]
# Experiment subsample for third bar in demographic balance plots
demographic_values_exp <- survey_merged %>% filter(!is.na(experiment_condition) & experiment_condition != "") %>% select(all_of(demographic_cols))

age_comparison_plot <- compare_survey_to_acs(demographic_values, age_data, "age_range", "mapped_age_group", "Age", experiment_data = demographic_values_exp)
ggsave(paste(FIGURES_DIR, "balance/age.pdf", sep=""), device="pdf", plot=age_comparison_plot, width=8.14, height=7.6)

income_order <- c("< $10,000",  "$10,000 - $19,999", "$20,000 - $29,999", 
                  "$30,000 - $49,999", "$50,000 - $59,999", "$60,000 - $74,999", 
                  "$75,000 - $99,999", "$100,000 - $124,999", "$125,000 - $149,999", 
                  "$150,000 - $199,999", "Greater than $200,000")
income_comparison_plot <- compare_survey_to_acs(demographic_values %>% filter(income_full != "Prefer not to answer"), income_data, "income_full", "census_income_buckets", "Income", order_levels = income_order, experiment_data = demographic_values_exp %>% filter(income_full != "Prefer not to answer"))
ggsave(paste(FIGURES_DIR, "balance/income.pdf", sep=""), device="pdf", plot=income_comparison_plot, width=8.14, height=7.6)


gender_comparison_plot <- compare_survey_to_acs(demographic_values, gender_data, "gender", "variable", "Gender", experiment_data = demographic_values_exp)
ggsave(paste(FIGURES_DIR, "balance/gender.pdf", sep=""), device="pdf", plot=gender_comparison_plot, width=8.14, height=7.6)


education_order <- c("Bachelor's Degree or Higher", "Some College", "Less than College")
education_comparison_plot <- compare_survey_to_acs(demographic_values, education_data, "formatted_education", "variable", "Education Levels", order_levels = education_order, experiment_data = demographic_values_exp)
ggsave(paste(FIGURES_DIR, "balance/education.pdf", sep=""), device="pdf", plot=education_comparison_plot, width=8.14, height=7.6)


#### SECTION 2: Compare on privacy attitudes and measure selection into study ####

# NOTE: experiment_users already loaded and joined in SECTION 1 above

survey_merged <- survey_merged %>%
  mutate(
    privacy.policy.pew = case_when(
      pewq1 == 1 ~ "Always or almost always",
      pewq1 == 2 ~ "Often",
      pewq1 == 3 ~ "Sometimes",
      pewq1 == 4 ~ "Rarely",
      pewq1 == 5 ~ "Never",
      TRUE ~ ""
    ),
    privacy.pol.eff.pew = case_when(
      pewq2 == 1 ~ "Extremely effective",
      pewq2 == 2 ~ "Very effective",
      pewq2 == 3 ~ "Somewhat effective",
      pewq2 == 4 ~ "Not too effective",
      pewq2 == 5 ~ "Not at all effective",
      TRUE ~ ""
    ),
    extensive.margin.pew = case_when(
      pewq3 == 1 ~ "Yes, I have",
      pewq3 == 2 ~ "No",
      TRUE ~ ""
    ),
    cookies_question. = case_when(
      pewq4 == 1 ~ "True",
      pewq4 == 2 ~ "False",
      pewq4 == 3 ~ "I don't know",
      TRUE ~ ""
    )
  )

privacy_attitudes_pew <- get_privacy_attitude_pew_df(survey_merged)

# Write our survey's PEW comparison results to TeX
sink(paste0(TABLES_DIR, "tables/survey_pew_comparison.tex"))
cat("% Survey Results for PEW Comparison Questions\n\n")
for(i in 1:nrow(privacy_attitudes_pew)) {
  row <- privacy_attitudes_pew[i,]
  label_clean <- gsub(" ", "", row$label)
  raw_label_clean <- gsub("[/ ,]", "", row$raw_label)
  
  cat(sprintf("\\newcommand{\\Survey%s%sEntry}{%.1f}\n", 
              label_clean, raw_label_clean, row$proportion_entry * 100))
  cat(sprintf("\\newcommand{\\Survey%s%sExp}{%.1f}\n", 
              label_clean, raw_label_clean, row$proportion_exp * 100))
}
sink()

plot_df <- privacy_attitudes_pew %>%
  select(raw_label, label, proportion_entry, proportion_exp, pew) %>%
  tidyr::pivot_longer(cols = c(proportion_entry, proportion_exp, pew),
                      names_to = "source", values_to = "value") %>%
  mutate(
    source = recode(source,
                    proportion_entry = "Survey",
                    proportion_exp   = "Extension",
                    pew              = "Pew benchmark"),
    source = factor(source, levels = c("Survey", "Extension", "Pew benchmark")),
    raw_label = factor(raw_label, levels = c(
      # Immediately Accept Terms (least → most privacy conscious)
      "Always/Often", "Sometimes", "Rarely/Never",
      # Privacy Policies Effective
      "Not too/Not at all effective", "Somewhat effective", "Extremely/Very effective",
      # Extensive Margin — keep only the "Yes" answer
      "Yes, I have"
    ))
  ) %>%
  filter(!(label == "Extensive Margin Privacy Policies" & raw_label == "No"))

g <- ggplot(plot_df, aes(x = raw_label, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.85) +
  facet_wrap(~label, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0.01)) +
  scale_fill_benchmark() +
  labs(x = "Response category", y = "Share", fill = NULL) +
  theme_privacy_experiment(show_grid_x = FALSE) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    panel.spacing = unit(1.5, "lines")
  )
ggsave(paste(FIGURES_DIR, "pew_comparison.pdf", sep=""), device="pdf", plot=g, width=10, height=7.6)

##### WTA COMPARISONS ####

dfp <- survey_merged %>%
  filter(!is.na(cutoff_wta)) %>%
  mutate(
    # Winsorize at 1st and 99th percentile
    cutoff_wta_trim = pmin(
      pmax(cutoff_wta, quantile(cutoff_wta, 0.01, na.rm = TRUE)),
      quantile(cutoff_wta, 0.99, na.rm = TRUE)
    )
  )

# Calculate WTA statistics for both samples
wta_stats_full <- dfp %>%
  summarise(
    mean_wta = mean(cutoff_wta_trim, na.rm = TRUE),
    median_wta = median(cutoff_wta_trim, na.rm = TRUE),
    sd_wta = sd(cutoff_wta_trim, na.rm = TRUE),
    p25_wta = quantile(cutoff_wta_trim, 0.25, na.rm = TRUE),
    p75_wta = quantile(cutoff_wta_trim, 0.75, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(sample = "Full")

wta_stats_exp <- dfp %>%
  filter(experiment_condition != "") %>%
  summarise(
    mean_wta = mean(cutoff_wta_trim, na.rm = TRUE),
    median_wta = median(cutoff_wta_trim, na.rm = TRUE),
    sd_wta = sd(cutoff_wta_trim, na.rm = TRUE),
    p25_wta = quantile(cutoff_wta_trim, 0.25, na.rm = TRUE),
    p75_wta = quantile(cutoff_wta_trim, 0.75, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(sample = "Extension")

wta_stats <- bind_rows(wta_stats_full, wta_stats_exp)

experiment_users <- read.csv("data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
experiment_users <- experiment_users %>% filter(experiment_condition != "")

# Write WTA statistics to TeX
sink(paste0(TABLES_DIR, "tables/wta_statistics.tex"))
cat("% WTA Statistics for Full Sample and Extension Subsample\n\n")
cat(sprintf("\\newcommand{\\WTAFullMean}{%.2f}\n", wta_stats_full$mean_wta))
cat(sprintf("\\newcommand{\\WTAFullMedian}{%.2f}\n", wta_stats_full$median_wta))
cat(sprintf("\\newcommand{\\WTAFullSD}{%.2f}\n", wta_stats_full$sd_wta))
cat(sprintf("\\newcommand{\\WTAFullN}{%.0f}\n", wta_stats_full$n))
cat(sprintf("\\newcommand{\\WTAExtMean}{%.2f}\n", wta_stats_exp$mean_wta))
cat(sprintf("\\newcommand{\\WTAExtMedian}{%.2f}\n", wta_stats_exp$median_wta))
cat(sprintf("\\newcommand{\\WTAExtSD}{%.2f}\n", wta_stats_exp$sd_wta))
cat(sprintf("\\newcommand{\\WTAExtN}{%.0f}\n", wta_stats_exp$n))
cat(sprintf("\\newcommand{\\WTAExtPresentedOfferMean}{%.0f}\n", mean(experiment_users$presented_offer)))
cat(sprintf("\\newcommand{\\WTAExtPresentedOfferMedian}{%.0f}\n", median(experiment_users$presented_offer)))

sink()

g <- ggplot() +
  # Unconditional
  geom_density(
    data = dfp,
    aes(x = cutoff_wta_trim, color = "Full sample"),
    adjust = 3,
    size = 1.2,
    alpha = 0.8
  ) +
  # Conditional
  geom_density(
    data = filter(dfp, experiment_condition != ""),
    aes(x = cutoff_wta_trim, color = "Extension subsample"),
    adjust = 3,
    size = 1.2,
    alpha = 0.8
  ) +
  scale_x_log10() +
  scale_color_manual(values = c(
    "Full sample" = "#5B9BD5",           # Blue
    "Extension subsample" = "#FF6B6B"     # Coral red
  )) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "WTA to Install Extension",
    y = "Density",
    color = "Sample"
  )
ggsave(paste(FIGURES_DIR, "wta_comparison.pdf", sep=""), device="pdf", plot=g, width=8.14, height=7.6)

####  Section 3: Compare across offer groups #####
connect <- read.csv("data/final_extension_data/connect.csv")
connect <- connect %>% select(email, compensation) %>% rename(emailid = email)

survey_merged <- survey_merged %>% left_join(connect, by="emailid")

df.belief <- survey_merged %>% 
  select(emailid, compensation, starts_with("beliefs")) %>%
  select(-matches('_post')) %>%
  pivot_longer(
    cols = starts_with("beliefs"),
    names_to = "name",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  mutate(value = case_when(value == 5 ~ 1,
                           value == 4 ~ .75,
                           value == 3 ~ .5,
                           value == 2 ~ .25,
                           value == 1 ~ 0)) 

# map belief var -> internal q_html_key
belief_map <- enframe(belief_to_internal, name = "name", value = "q_html_key")

df.belief <- df.belief %>%
  left_join(belief_map, by = "name") %>%
  select(-name) %>%
  filter(!is.na(q_html_key)) 

df.belief <- df.belief %>%
  left_join(
    compute_ground_truth_privacy_distribution() %>%
      rename(universe_rating = mean_rating) %>%
      mutate(
        q_html_key = paste(feature, field, sep = "-")
      ) %>%
      select(q_html_key, universe_rating),
    by = "q_html_key"
  ) %>%
  mutate(
    # (a) Ground truth
    abs_distance_overall_u    = abs(value - universe_rating),
  ) 

df.belief <- df.belief %>%
  select(-value, -universe_rating) %>%
  filter(!is.na(abs_distance_overall_u)) %>%
  pivot_wider(
    id_cols   = c(emailid, compensation),
    names_from  = q_html_key,
    values_fn   = list(abs_distance_overall_u = ~ first(.x)), 
    values_from = abs_distance_overall_u,
    names_sort  = TRUE
  ) %>%
  left_join(
    survey_merged %>%
      select(emailid, matches("pewq"),"WTA")
  )

df.belief <- df.belief %>%
  mutate(WTA = log(as.numeric(WTA)))


sum_long <- df.belief %>%
  filter(!is.na(compensation)) %>%
  select(-emailid) %>%
  mutate(
    across(
      where(is.numeric) & !c(compensation),
      ~ .x - mean(.x, na.rm = TRUE)
    )
  ) %>%
  group_by(compensation) %>%
  summarise(
    across(
      where(is.numeric),   # or: -c(emailid, compensation)
      list(
        mean = ~mean(.x, na.rm = TRUE),
        se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    -c(compensation),                # keep ids; gather var__mean/var__se
    names_to  = c("variable", ".value"),
    names_sep = "__"
  )

sum_long <- sum_long %>%
  mutate(
    var_group = case_when(
      str_detect(variable, "collect") ~ "Beliefs: Collection",
      str_detect(variable, "share")    ~ "Beliefs: Control",
      str_detect(variable, "(?i)pew")                    ~ "PEW",
      toupper(variable) == "WTA"                         ~ "WTA",
      TRUE                                               ~ "Beliefs: Use"
    )
  ) %>%
  filter(var_group %in% c("PEW", "Beliefs: Collection", "Beliefs: Control", "Beliefs: Use", "WTA")) %>%
  group_by(var_group) %>%
  mutate(variable = factor(variable, levels = sort(unique(variable)))) %>%
  ungroup()

# ----- 5) Plot: side-by-side facets with error bars -----
ggplot(sum_long, aes(x = variable, y = mean, color = as.factor(compensation))) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    width = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  facet_wrap(~ var_group, scales = "free_x", ncol = 5) +
  labs(x = NULL, y = "Centered Mean (± 1 SE)", color = "Compensation") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor.x = element_blank(),
    leged.position = 'bottom'
  ) + 
  scale_shape_manual(values = c("3" = 21, "4" = 22, "5" = 23)) +
  scale_fill_manual(values  = c("3" = "grey", "4" = "#2ca25f", "5" = "#3182bd" )) 

ggsave('./results/baseline_survey_descriptives/disagg_selection_lots.pdf' , plot = last_plot())

#---- Aggregated version
df_demeaned <- df.belief %>%
  filter(!is.na(compensation)) %>%
  mutate(
    across(
      where(is.numeric) & !any_of("compensation"),
      ~ .x - mean(.x, na.rm = TRUE)
    )
  )

# 2) Go long: one row per obs × variable
long <- df_demeaned %>%
  pivot_longer(
    cols = where(is.numeric) & !any_of("compensation"),
    names_to  = "variable",
    values_to = "value"
  ) %>%
  # 3) Tag variable groups
  mutate(
    var_group = case_when(
      str_detect(variable, "collect")          ~ "Beliefs: Collection",
      str_detect(variable, "share")            ~ "Beliefs: Control",
      str_detect(variable, "(?i)pew")          ~ "PEW",
      toupper(variable) == "WTA"               ~ "WTA",
      TRUE                                     ~ "Beliefs: Use"
    )
  )

# 4) Aggregate by group(compensation, var_group)
sum_by_group <- long %>%
  group_by(compensation, var_group) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    se   = sd(value,  na.rm = TRUE) / sqrt(sum(!is.na(value))),
    n    = sum(!is.na(value)),
    .groups = "drop"
  )

sum_by_group <- sum_by_group %>%
  mutate(compensation_label = paste0("$", compensation))

ggplot(sum_by_group,
       aes(x = var_group,
           y = mean,
           color = compensation_label,
           group = compensation_label)) +
  geom_hline_zero(linetype = "dashed", color = "gray50") +
  geom_errorbar(
    aes(ymin = mean - 1.96*se, ymax = mean + 1.96*se),
    width = ERRORBAR_WIDTH,
    linewidth = LINE_WIDTH,
    position = position_dodge(width = DODGE_WIDTH_3)
  ) +
  geom_point(
    size = POINT_SIZE,
    position = position_dodge(width = DODGE_WIDTH_3)
  ) +
  labs(
    x = NULL, y = "Mean (demeaned)",
    color = "Survey compensation"
  ) +
  theme_privacy_experiment() +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

ggsave('./results/baseline_survey_descriptives/agg_selection_lots.pdf' , plot = last_plot(), units = 'in', height = 6, width = 10)

##### Aggregates

beliefs_long <- survey_merged %>%
  filter(!is.na(compensation)) %>%
  select(-matches('_post')) %>%
  pivot_longer(
    cols = matches("beliefs(collection|control|use)"),
    names_to = "item",
    values_to = "value"
  ) %>%
  mutate(
    belief_type = case_when(
      str_detect(item, "(?i)collection") ~ "Collection",
      str_detect(item, "(?i)control")    ~ "Control",
      str_detect(item, "(?i)use")        ~ "Use",
      TRUE                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(belief_type))

anova_comp <- aov(value ~ item * compensation, data = beliefs_long)
summary(anova_comp)


## Section 4: Descriptives about consequences

endline_clean <- read.csv("data/Survey/final_endline_survey_cleaned.csv", stringsAsFactors = FALSE)
cat("✅ Loaded endline_clean:", nrow(endline_clean), "rows\n")
consequences_processed <- endline_clean %>%
  filter(!is.na(consequences) & consequences != "") %>%
  mutate(
    # Binary indicators for each consequence type
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

# Step 2: Calculate frequency and percentage for each consequence
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


tab <- xtable(consequence_summary, caption = "Consequences of Data Sharing")

print(tab,
      include.rownames = FALSE,
      file = paste(TABLES_DIR, "consequences_data_sharing.tex"),
      sanitize.text.function = identity)


# Step 3: Categorize consequences as positive or negative
cat("\nStep 3: Categorizing consequences as positive or negative...\n")

# Add category to the summary
consequence_summary <- consequence_summary %>%
  mutate(
    Category = case_when(
      Consequence %in% c("More annoying ads", "Loss of data control", 
                         "Hacking", "Identity theft", "Cyberstalking") ~ "Negative",
      TRUE ~ "Positive"
    )
  )

# Summary by category
category_summary <- consequence_summary %>%
  group_by(Category) %>%
  summarise(
    Total_Mentions = sum(Count),
    Avg_Percentage = mean(Percentage),
    .groups = 'drop'
  )

cat("\nSummary by category:\n")
print(category_summary)

# Count how many consequences each person selected
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


# Step 4: Analyze valence patterns (positive only, negative only, or mixed)
cat("\nStep 4: Analyzing valence patterns...\n")

# Categorize each person's overall valence
consequences_processed <- consequences_processed %>%
  mutate(
    consequence_valence = case_when(
      n_positive > 0 & n_negative == 0 ~ "Positive only",
      n_positive == 0 & n_negative > 0 ~ "Negative only", 
      n_positive > 0 & n_negative > 0 ~ "Mixed",
      TRUE ~ "Unknown"
    )
  )

# Valence distribution
valence_table <- table(consequences_processed$consequence_valence)
cat("\nValence distribution:\n")
print(valence_table)

cat("\nValence proportions:\n")
print(prop.table(valence_table))

survey_merged <- survey_merged %>%
  mutate(annoying_1 = case_when(
    annoying_1 == "Not at all" ~ 0.0,
    annoying_1 == "2" ~ 0.25,
    annoying_1 == "3" ~ 0.5,
    annoying_1 == "4" ~ 0.75,
    annoying_1 == "Always" ~ 1.0,
    TRUE ~ NA_real_
  ), annoying_2 = case_when(
    annoying_2 == "Not at all" ~ 0.0,
    annoying_2 == "2" ~ 0.25,
    annoying_2 == "3" ~ 0.5,
    annoying_2 == "4" ~ 0.75,
    annoying_2 == "Always" ~ 1.0,
    TRUE ~ NA_real_
  ))

t1 <- felm(annoying_1 ~ experiment_condition, survey_merged %>% filter(experiment_condition != ""))
t2 <- felm(annoying_2 ~ experiment_condition, survey_merged %>% filter(experiment_condition != ""))

stargazer(t1, t2)