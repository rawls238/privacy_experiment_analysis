# =============================================================================
# SELECTION INTO STUDY
# =============================================================================
#
# Produces:
#   Appendix C.1 "Figures on Experimental Sample"
#     Fig C.1 [fig:dem_balance, "Demographic Composition of Experimental Sample"]
#       Fig C.1(a) [fig:dem_balance_age]:       output/figures/balance/age.pdf
#       Fig C.1(b) [fig:dem_balance_education]: output/figures/balance/education.pdf
#       Fig C.1(c) [fig:dem_balance_gender]:    output/figures/balance/gender.pdf
#       Fig C.1(d) [fig:dem_balance_income]:    output/figures/balance/income.pdf
#     Fig C.2 [fig:selection_on_survey_payments,
#              "Privacy Preferences and Beliefs across (Exogenous) Survey Payment"]:
#       output/figures/agg_selection_lots.pdf
#     Fig C.3 [fig:pew_comparison, "PEW Comparison"]:
#       output/figures/pew_comparison.pdf
#
# Inputs:
#   ../data/Survey/survey_merged_final.csv
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#   ../data/final_extension_data/privacy_info.csv
#   ../data/final_extension_data/connect.csv
#   ACS via tidycensus / acs API at runtime (API key hard-coded inline; see
#   get_census_data()).
#
# Dependencies:
#   replication_files/utils/plot_rules.R
#
# Outputs:
#   output/figures/balance/age.pdf
#   output/figures/balance/education.pdf
#   output/figures/balance/gender.pdf
#   output/figures/balance/income.pdf
#   output/figures/agg_selection_lots.pdf
#   output/figures/pew_comparison.pdf
#
# Note: Previous version of this script also produced (now removed as dead code,
# not in paper):
#   - wta_comparison.pdf (WTA density plot; not used)
#   - disagg_selection_lots.pdf (alt of agg_selection_lots)
#   - tables/pew_benchmarks.tex (PEW macros; not \input{} anywhere)
#   - tables/demo_summary.tex (stargazer demo; not in paper)
#   - tables/survey_pew_comparison.tex (Survey-side PEW macros; not used)
#   - tables/wta_statistics.tex (WTA macros; not used)
#   - consequences_data_sharing.tex (xtable; redundant with
#     other_survey_regressions.R Table C.8 `tab:data_sharing_purpose`)
#   - Trailing stargazer(t1, t2) on annoying_1/annoying_2 (console only;
#     redundant with other_survey_regressions.R Table C.3
#     `tab:experimenter_demand`)
# Removed dead computation:
#   - Top-level `labels` table (0 call sites)
#   - All WTA section: dfp, wta_stats_full/exp/wta_stats, WTA sink
#   - sum_long + disagg ggplot (intermediate to disagg_selection_lots.pdf)
#   - beliefs_long + anova_comp + summary(anova_comp) (console only)
#   - Entire Section 4: consequences_processed pipeline + category_summary +
#     n_consequences / n_positive / n_negative summaries + valence_table +
#     prop.table + xtable + final consequence_valence mutate
#   - Trailing annoying_1/annoying_2 mutate + t1/t2 felm + stargazer
# Removed dead library() calls:
#   - library(stargazer), library(xtable), library(lfe)
# Removed Sys.info()-based WD branching (replaced by a single fixed setwd).
# =============================================================================

# Set working directory to code_github root so all relative paths resolve.
setwd("~/Dropbox/spring2025experiment/code_github")

# Source utility scripts
source("replication_files/utils/plot_rules.R")

# Load required libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(acs)

# Output directory
FIGURES_DIR <- "output/figures/"
dir.create(FIGURES_DIR,                       showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(FIGURES_DIR, "balance/"),   showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# PEW BENCHMARK CONSTANTS
# =============================================================================
# Values from PEW Research privacy survey (2023). Used by
# get_privacy_attitude_pew_df() below to overlay PEW columns on Fig C.3.

PEW_BENCHMARKS <- list(
  immediate_accept = list(
    always_often = 57 / 97,
    sometimes    = 22 / 97,
    rarely_never = 18 / 97,
    n            = 5101 * 0.97
  ),
  policy_effective = list(
    extremely_very = 8  / 96,
    somewhat       = 27 / 96,
    not_effective  = 61 / 96,
    n              = 5101 * 0.96
  ),
  extensive_margin = list(
    yes = 49 / 96,
    no  = 47 / 96,
    n   = 5101 * 0.96
  )
)

# =============================================================================
# BELIEF -> INTERNAL ATTRIBUTE KEY MAPPING
# =============================================================================
# Maps raw `beliefsXXX_rN` survey columns to internal `feature-field` keys
# used in `privacy_info.csv`. Drives df.belief join for Fig C.2.

belief_to_internal <- c(
  # beliefscollection -> collect
  beliefscollection_r1 = "collect-log",
  beliefscollection_r2 = "collect-bio",
  beliefscollection_r3 = "collect-sensitive",
  beliefscollection_r4 = "collect-financial",
  beliefscollection_r5 = "collect-offsite",
  beliefscollection_r6 = "collect-location",
  beliefscollection_r7 = "collect-social",
  
  # beliefsuse -> share / related
  beliefsuse_r1 = "anonymized-anonymized",
  beliefsuse_r2 = "share-social",
  beliefsuse_r3 = "share-finance",
  beliefsuse_r4 = "share-ads",
  beliefsuse_r5 = "share-law",
  beliefsuse_r6 = "share-service",
  beliefsuse_r7 = "share-partners",
  beliefsuse_r8 = "personalization-personalization",
  
  # beliefscontrol -> control-related
  beliefscontrol_r1 = "change-change",
  beliefscontrol_r2 = "automated-automated",
  beliefscontrol_r3 = "deletion-deletion",
  beliefscontrol_r4 = "storage-storage",
  
  # beliefsquality
  beliefsquality_r1 = "quality-content",
  beliefsquality_r2 = "quality-ads",
  beliefsquality_r3 = "quality-navigation"
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Fetch and process 2021 ACS demographic distributions from Census API.
# Returns a list with age, gender, income, education, and ethnicity dataframes.
get_census_data <- function() {
  api.key.install("5a55e425c21ed4b512cdc03fab445dcb00f32892")
  
  geo <- geo.make(state = "*")
  
  age_gender_data <- acs.fetch(geography = geo,
                               table.number = "B01001",
                               endyear = 2021,
                               span = 5)
  age_gender_df <- as.data.frame(age_gender_data@estimate) %>%
    rownames_to_column("state") %>%
    pivot_longer(cols = -state, names_to = "variable", values_to = "value")
  
  income_data <- acs.fetch(geography = geo,
                           table.number = "B19001",
                           endyear = 2021,
                           span = 5)
  income_df <- as.data.frame(income_data@estimate) %>%
    rownames_to_column("state") %>%
    pivot_longer(cols = -state, names_to = "variable", values_to = "value")
  
  ethnicity_data <- acs.fetch(geography = geo,
                              table.number = "B03002",
                              endyear = 2021,
                              span = 5)
  ethnicity_df <- as.data.frame(ethnicity_data@estimate) %>%
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
      variable %in% c("B01001_007", "B01001_008", "B01001_009", "B01001_010",
                      "B01001_031", "B01001_032", "B01001_033", "B01001_034") ~ "18-24",
      variable %in% c("B01001_011", "B01001_035") ~ "25-29",
      variable %in% c("B01001_012", "B01001_036") ~ "30-34",
      variable %in% c("B01001_013", "B01001_037") ~ "35-39",
      variable %in% c("B01001_014", "B01001_038") ~ "40-44",
      variable %in% c("B01001_015", "B01001_039") ~ "45-49",
      variable %in% c("B01001_016", "B01001_040") ~ "50-54",
      variable %in% c("B01001_017", "B01001_041") ~ "55-59",
      variable %in% c("B01001_018", "B01001_019",
                      "B01001_042", "B01001_043") ~ "60-64",
      variable %in% c("B01001_020", "B01001_021",
                      "B01001_044", "B01001_045") ~ "65-70",
      variable %in% c("B01001_022", "B01001_046") ~ "70-74",
      variable %in% c("B01001_023", "B01001_024", "B01001_025",
                      "B01001_047", "B01001_048", "B01001_049") ~ "75+"
    )) %>%
    filter(!is.na(mapped_age_group) & !is.na(original_age_group)) %>%
    group_by(mapped_age_group, original_age_group) %>%
    summarise(estimate = sum(value), .groups = "drop") %>%
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
    summarise(estimate = sum(value), .groups = "drop") %>%
    mutate(proportion = estimate / sum(estimate))
  
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
    summarise(estimate = sum(value), .groups = "drop") %>%
    mutate(proportion = estimate / sum(estimate))
  
  education_data <- acs.fetch(geography = geo,
                              table.number = "B15003",
                              endyear = 2021,
                              span = 5)
  education_df <- as.data.frame(education_data@estimate) %>%
    rownames_to_column("state") %>%
    pivot_longer(cols = -state, names_to = "variable", values_to = "value") %>%
    mutate(variable = case_when(
      variable %in% c("B15003_002", "B15003_003", "B15003_004", "B15003_005",
                      "B15003_006", "B15003_007", "B15003_008", "B15003_009",
                      "B15003_010", "B15003_011", "B15003_012", "B15003_013",
                      "B15003_014", "B15003_015", "B15003_016", "B15003_017",
                      "B15003_018") ~ "Less than College",
      variable %in% c("B15003_019", "B15003_020", "B15003_021") ~ "Some College",
      variable %in% c("B15003_022", "B15003_023", "B15003_024",
                      "B15003_025") ~ "Bachelor's Degree or Higher",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(variable)) %>%
    group_by(variable) %>%
    summarise(estimate = sum(value), .groups = "drop") %>%
    mutate(proportion = estimate / sum(estimate))
  
  gender_data <- acs.fetch(geography = geo,
                           table.number = "B01001",
                           endyear = 2021,
                           span = 5)
  gender_df <- as.data.frame(gender_data@estimate) %>%
    rownames_to_column("state") %>%
    pivot_longer(cols = -state, names_to = "variable", values_to = "value") %>%
    filter(variable %in% c("B01001_002", "B01001_026") & !is.na(variable)) %>%
    mutate(variable = case_when(
      variable == "B01001_002" ~ "Male",
      variable == "B01001_026" ~ "Female",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(variable)) %>%
    group_by(variable) %>%
    summarise(estimate = sum(value), .groups = "drop") %>%
    mutate(proportion = estimate / sum(estimate))
  
  return(list(age_data       = age_gender_df,
              gender_data    = gender_df,
              income_data    = income_df,
              education_data = education_df,
              ethnicity_data = ethnicity_df))
}

# Compute Survey/Extension/PEW proportions for the three PEW attitude questions.
# Returns a stacked dataframe ready for facet_wrap by question.
get_privacy_attitude_pew_df <- function(df) {
  
  get_pew_agg_immediate <- function(agg_df, condition = FALSE) {
    if (condition) agg_df <- agg_df %>% filter(experiment_condition != "")
    privacy_policy_immediate <- agg_df %>%
      filter(privacy.policy.pew != "") %>%
      mutate(total = n()) %>%
      group_by(privacy.policy.pew) %>%
      summarise(proportion = n() / first(total)) %>%
      ungroup() %>%
      mutate(privacy.policy.pew = case_when(
        privacy.policy.pew %in% c("Always or almost always", "Often") ~ "Always/Often",
        privacy.policy.pew %in% c("Rarely", "Never")                  ~ "Rarely/Never",
        TRUE                                                          ~ privacy.policy.pew
      )) %>%
      group_by(privacy.policy.pew) %>%
      summarise(proportion = sum(proportion)) %>%
      ungroup()
    return(privacy_policy_immediate)
  }
  privacy_policy_immediate_entry_sample <- get_pew_agg_immediate(df, FALSE) %>%
    rename(proportion_entry = proportion)
  privacy_policy_immediate_exp_sample <- get_pew_agg_immediate(df, TRUE) %>%
    rename(proportion_exp = proportion)
  privacy_policy_immediate <- privacy_policy_immediate_exp_sample %>%
    left_join(privacy_policy_immediate_entry_sample, by = "privacy.policy.pew")
  
  privacy_policy_immediate$pew   <- c(PEW_BENCHMARKS$immediate_accept$always_often,
                                      PEW_BENCHMARKS$immediate_accept$rarely_never,
                                      PEW_BENCHMARKS$immediate_accept$sometimes)
  privacy_policy_immediate$score <- c(0.0, 1.0, 0.5)
  privacy_policy_immediate$label <- "Immediately Accept Terms"
  privacy_policy_immediate$npew  <- PEW_BENCHMARKS$immediate_accept$n
  privacy_policy_immediate <- privacy_policy_immediate %>%
    rename(raw_label = privacy.policy.pew)
  
  get_pew_effective <- function(agg_df, condition = FALSE) {
    if (condition) agg_df <- agg_df %>% filter(experiment_condition != "")
    privacy_policy_effective <- agg_df %>%
      filter(privacy.pol.eff.pew != "") %>%
      mutate(total = n()) %>%
      group_by(privacy.pol.eff.pew) %>%
      summarise(proportion = n() / first(total)) %>%
      ungroup() %>%
      mutate(privacy.pol.eff.pew = case_when(
        privacy.pol.eff.pew %in% c("Extremely effective", "Very effective")     ~ "Extremely/Very effective",
        privacy.pol.eff.pew %in% c("Not at all effective", "Not too effective") ~ "Not too/Not at all effective",
        TRUE                                                                    ~ privacy.pol.eff.pew
      )) %>%
      group_by(privacy.pol.eff.pew) %>%
      summarise(proportion = sum(proportion)) %>%
      ungroup()
    return(privacy_policy_effective)
  }
  privacy_policy_effective_entry_sample <- get_pew_effective(df, FALSE) %>%
    rename(proportion_entry = proportion)
  privacy_policy_effective_exp_sample <- get_pew_effective(df, TRUE) %>%
    rename(proportion_exp = proportion)
  privacy_policy_effective <- privacy_policy_effective_exp_sample %>%
    left_join(privacy_policy_effective_entry_sample, by = "privacy.pol.eff.pew")
  
  privacy_policy_effective$pew   <- c(PEW_BENCHMARKS$policy_effective$extremely_very,
                                      PEW_BENCHMARKS$policy_effective$not_effective,
                                      PEW_BENCHMARKS$policy_effective$somewhat)
  privacy_policy_effective$score <- c(0.0, 1.0, 0.5)
  privacy_policy_effective$label <- "Privacy Policies Effective"
  privacy_policy_effective$npew  <- PEW_BENCHMARKS$policy_effective$n
  privacy_policy_effective <- privacy_policy_effective %>%
    rename(raw_label = privacy.pol.eff.pew)
  
  get_pew_extensive <- function(agg_df, condition = FALSE) {
    if (condition) agg_df <- agg_df %>% filter(experiment_condition != "")
    extensive_margin_pew <- agg_df %>%
      filter(extensive.margin.pew != "") %>%
      mutate(total = n()) %>%
      group_by(extensive.margin.pew) %>%
      summarise(proportion = n() / first(total)) %>%
      ungroup()
    return(extensive_margin_pew)
  }
  extensive_margin_pew_entry_sample <- get_pew_extensive(df, FALSE) %>%
    rename(proportion_entry = proportion)
  privacy_policy_extensive_exp_sample <- get_pew_extensive(df, TRUE) %>%
    rename(proportion_exp = proportion)
  extensive_margin_pew <- privacy_policy_extensive_exp_sample %>%
    left_join(extensive_margin_pew_entry_sample, by = "extensive.margin.pew")
  
  extensive_margin_pew$pew   <- c(PEW_BENCHMARKS$extensive_margin$yes,
                                  PEW_BENCHMARKS$extensive_margin$no)
  extensive_margin_pew$score <- c(0.0, 1.0)
  extensive_margin_pew$npew  <- PEW_BENCHMARKS$extensive_margin$n
  extensive_margin_pew$label <- "Extensive Margin Privacy Policies"
  extensive_margin_pew <- extensive_margin_pew %>%
    rename(raw_label = extensive.margin.pew)
  
  privacy_attitudes_pew <- rbind(extensive_margin_pew,
                                 privacy_policy_effective,
                                 privacy_policy_immediate)
  return(privacy_attitudes_pew)
}

# Plot survey vs ACS (vs Extension) for one demographic dimension.
compare_survey_to_acs <- function(demographic_values, acs_data, demographic_variable,
                                  acs_variable_name, demographic_variable_str,
                                  order_levels = NULL, experiment_data = NULL) {
  
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
  
  comparison_data <- full_join(acs_proportion, survey_proportion,
                               by = c("category" = demographic_variable))
  
  if (!is.null(experiment_data)) {
    exp_proportion <- experiment_data %>%
      filter(!!sym(demographic_variable) != "" &
               !!sym(demographic_variable) != "Prefer not to say" &
               !!sym(demographic_variable) != "Prefer not to answer") %>%
      mutate(total_entries = n()) %>%
      group_by(!!sym(demographic_variable)) %>%
      summarise(experiment_proportion = n() / first(total_entries)) %>%
      ungroup()
    comparison_data <- full_join(comparison_data, exp_proportion,
                                 by = c("category" = demographic_variable))
  }
  
  comparison_data[is.na(comparison_data)] <- 0
  
  if (!is.null(order_levels)) {
    comparison_data$category <- factor(comparison_data$category,
                                       levels = order_levels, ordered = TRUE)
  }
  
  if (!is.null(experiment_data)) {
    comparison_data_long <- comparison_data %>%
      pivot_longer(cols = c(acs_proportion, survey_proportion, experiment_proportion),
                   names_to = "source", values_to = "proportion") %>%
      mutate(source = factor(recode(source,
                                    acs_proportion        = "ACS",
                                    survey_proportion     = "Survey",
                                    experiment_proportion = "Extension"),
                             levels = c("Survey", "Extension", "ACS")))
  } else {
    comparison_data_long <- comparison_data %>%
      pivot_longer(cols = c(acs_proportion, survey_proportion),
                   names_to = "source", values_to = "proportion") %>%
      mutate(source = factor(recode(source,
                                    acs_proportion    = "ACS",
                                    survey_proportion = "Survey"),
                             levels = c("Survey", "ACS")))
  }
  
  p <- ggplot(comparison_data_long,
              aes(x = category, y = proportion, fill = source)) +
    geom_bar(stat = "identity",
             position = position_dodge(width = 0.9), width = 0.85) +
    labs(x = demographic_variable_str, y = "Proportion", fill = "Source") +
    scale_fill_manual(values = BENCHMARK_COLORS) +
    theme_privacy_experiment(show_grid_x = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = c(0, 0.01))
  return(p)
}

# Compute ground-truth privacy practice prevalence from privacy_info.csv.
# Returns one row per (feature, field) with mean rating across top-500 sites.
compute_ground_truth_privacy_distribution <- function() {
  privacy_info <- read.csv("../data/final_extension_data/privacy_info.csv")
  privacy_info <- privacy_info %>% mutate(rating_numeric = ifelse(rating == "Yes", 1, 0))
  t <- privacy_info %>%
    group_by(feature, field) %>%
    summarise(mean_rating = mean(rating_numeric), .groups = "drop")
  return(t)
}

# =============================================================================
# LOAD MAIN DATASET
# =============================================================================

survey_merged <- read.csv("../data/Survey/survey_merged_final.csv",
                          stringsAsFactors = FALSE)

# Demographic recodes
if ("Age" %in% names(survey_merged)) {
  survey_merged <- survey_merged %>%
    mutate(age_range = case_when(
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
      Age >= 75            ~ "75+",
      TRUE                 ~ NA_character_
    ))
}

if ("Education" %in% names(survey_merged)) {
  survey_merged <- survey_merged %>%
    mutate(formatted_education = case_when(
      Education %in% c(1, 2, 3)              ~ "Less than College",
      Education == 4 | Education == 5 | Education == 6 ~ "Some College",
      Education >= 7                         ~ "Bachelor's Degree or Higher",
      TRUE                                   ~ NA_character_
    ))
}

if ("Gender" %in% names(survey_merged)) {
  survey_merged <- survey_merged %>%
    mutate(gender = case_when(
      Gender == 1 ~ "Male",
      Gender == 2 ~ "Female",
      TRUE        ~ NA_character_
    ))
}

if ("Income" %in% names(survey_merged)) {
  survey_merged <- survey_merged %>%
    mutate(income_full = case_when(
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
      TRUE         ~ NA_character_
    ))
}

# =============================================================================
# Fig C.1 [fig:dem_balance]: Demographic Composition of Experimental Sample
# Four panels: age (a), education (b), gender (c), income (d).
# =============================================================================

acs_data       <- get_census_data()
age_data       <- acs_data$age_data
gender_data    <- acs_data$gender_data
education_data <- acs_data$education_data
income_data    <- acs_data$income_data

income_data <- income_data %>%
  mutate(census_income_buckets = case_when(
    census_income %in% c("< $10,000")                                              ~ "< $10,000",
    census_income %in% c("$10,001 - $14,999", "$15,000 - $19,999")                 ~ "$10,000 - $19,999",
    census_income %in% c("$20,000 - $24,999", "$25,000 - $29,999")                 ~ "$20,000 - $29,999",
    census_income %in% c("$30,000 - $34,999", "$35,000 - $39,999",
                         "$40,000 - $44,999", "$45,000 - $49,999")                 ~ "$30,000 - $49,999",
    census_income %in% c("$50,000 - $59,999")                                      ~ "$50,000 - $59,999",
    census_income %in% c("$60,000 - $74,999")                                      ~ "$60,000 - $74,999",
    census_income %in% c("$75,000 - $99,999")                                      ~ "$75,000 - $99,999",
    census_income %in% c("$100,000 - $124,999")                                    ~ "$100,000 - $124,999",
    census_income %in% c("$125,000 - $149,999")                                    ~ "$125,000 - $149,999",
    census_income %in% c("$150,000 - $199,999")                                    ~ "$150,000 - $199,999",
    census_income %in% c("$200,000+")                                              ~ "Greater than $200,000",
    TRUE                                                                           ~ NA_character_
  )) %>%
  group_by(census_income_buckets) %>%
  summarise(estimate   = sum(estimate,   na.rm = TRUE),
            proportion = sum(proportion, na.rm = TRUE),
            .groups = "drop")

demographic_cols <- c("Age", "Gender", "Education",
                      "gender", "formatted_education", "age_range",
                      "income_full")

# Join experiment-condition data so extension subsample can be plotted as a 3rd bar.
experiment_users <- read.csv("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv",
                             stringsAsFactors = FALSE)
experiment_users <- experiment_users %>%
  select(email, experiment_id, experiment_condition, in_experiment, cutoff_wta) %>%
  rename(emailid = email)
survey_merged <- survey_merged %>% left_join(experiment_users, by = "emailid")

demographic_values     <- survey_merged[, demographic_cols]
demographic_values_exp <- survey_merged %>%
  filter(!is.na(experiment_condition) & experiment_condition != "") %>%
  select(all_of(demographic_cols))

# Fig C.1(a): Age
age_comparison_plot <- compare_survey_to_acs(
  demographic_values, age_data, "age_range", "mapped_age_group", "Age",
  experiment_data = demographic_values_exp
)
ggsave(paste0(FIGURES_DIR, "balance/age.pdf"),
       device = "pdf", plot = age_comparison_plot,
       width = 8.14, height = 7.6)

# Fig C.1(d): Income
income_order <- c("< $10,000",  "$10,000 - $19,999", "$20,000 - $29,999",
                  "$30,000 - $49,999", "$50,000 - $59,999", "$60,000 - $74,999",
                  "$75,000 - $99,999", "$100,000 - $124,999", "$125,000 - $149,999",
                  "$150,000 - $199,999", "Greater than $200,000")
income_comparison_plot <- compare_survey_to_acs(
  demographic_values     %>% filter(income_full != "Prefer not to answer"),
  income_data, "income_full", "census_income_buckets", "Income",
  order_levels    = income_order,
  experiment_data = demographic_values_exp %>%
    filter(income_full != "Prefer not to answer")
)
ggsave(paste0(FIGURES_DIR, "balance/income.pdf"),
       device = "pdf", plot = income_comparison_plot,
       width = 8.14, height = 7.6)

# Fig C.1(c): Gender
gender_comparison_plot <- compare_survey_to_acs(
  demographic_values, gender_data, "gender", "variable", "Gender",
  experiment_data = demographic_values_exp
)
ggsave(paste0(FIGURES_DIR, "balance/gender.pdf"),
       device = "pdf", plot = gender_comparison_plot,
       width = 8.14, height = 7.6)

# Fig C.1(b): Education
education_order <- c("Bachelor's Degree or Higher",
                     "Some College",
                     "Less than College")
education_comparison_plot <- compare_survey_to_acs(
  demographic_values, education_data, "formatted_education", "variable",
  "Education Levels",
  order_levels    = education_order,
  experiment_data = demographic_values_exp
)
ggsave(paste0(FIGURES_DIR, "balance/education.pdf"),
       device = "pdf", plot = education_comparison_plot,
       width = 8.14, height = 7.6)

# =============================================================================
# Fig C.3 [fig:pew_comparison]: Survey vs Extension vs PEW benchmark
# =============================================================================

survey_merged <- survey_merged %>%
  mutate(
    privacy.policy.pew = case_when(
      pewq1 == 1 ~ "Always or almost always",
      pewq1 == 2 ~ "Often",
      pewq1 == 3 ~ "Sometimes",
      pewq1 == 4 ~ "Rarely",
      pewq1 == 5 ~ "Never",
      TRUE       ~ ""
    ),
    privacy.pol.eff.pew = case_when(
      pewq2 == 1 ~ "Extremely effective",
      pewq2 == 2 ~ "Very effective",
      pewq2 == 3 ~ "Somewhat effective",
      pewq2 == 4 ~ "Not too effective",
      pewq2 == 5 ~ "Not at all effective",
      TRUE       ~ ""
    ),
    extensive.margin.pew = case_when(
      pewq3 == 1 ~ "Yes, I have",
      pewq3 == 2 ~ "No",
      TRUE       ~ ""
    ),
    cookies_question. = case_when(
      pewq4 == 1 ~ "True",
      pewq4 == 2 ~ "False",
      pewq4 == 3 ~ "I don't know",
      TRUE       ~ ""
    )
  )

privacy_attitudes_pew <- get_privacy_attitude_pew_df(survey_merged)

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
      "Always/Often", "Sometimes", "Rarely/Never",
      "Not too/Not at all effective", "Somewhat effective", "Extremely/Very effective",
      "Yes, I have"
    ))
  ) %>%
  filter(!(label == "Extensive Margin Privacy Policies" & raw_label == "No"))

g <- ggplot(plot_df, aes(x = raw_label, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.85) +
  facet_wrap(~label, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0, 0.01)) +
  scale_fill_benchmark() +
  labs(x = "Response category", y = "Share", fill = NULL) +
  theme_privacy_experiment(show_grid_x = FALSE) +
  theme(axis.text.x   = element_text(angle = 30, hjust = 1),
        strip.text    = element_text(size = 10, face = "bold"),
        panel.spacing = unit(1.5, "lines"))
ggsave(paste0(FIGURES_DIR, "pew_comparison.pdf"),
       device = "pdf", plot = g, width = 10, height = 7.6)

# =============================================================================
# Fig C.2 [fig:selection_on_survey_payments]: Belief misspecification + PEW + WTA
# across the three exogenous survey-payment offers ($3, $4, $5).
# =============================================================================

connect <- read.csv("../data/final_extension_data/connect.csv")
connect <- connect %>%
  select(email, compensation) %>%
  rename(emailid = email)

survey_merged <- survey_merged %>% left_join(connect, by = "emailid")

# Per-respondent absolute belief errors vs ground truth, plus PEW + WTA.
df.belief <- survey_merged %>%
  select(emailid, compensation, starts_with("beliefs")) %>%
  select(-matches("_post")) %>%
  pivot_longer(
    cols = starts_with("beliefs"),
    names_to  = "name",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  mutate(value = case_when(
    value == 5 ~ 1,
    value == 4 ~ 0.75,
    value == 3 ~ 0.5,
    value == 2 ~ 0.25,
    value == 1 ~ 0
  ))

# Map raw belief column names to internal `feature-field` keys.
belief_map <- enframe(belief_to_internal, name = "name", value = "q_html_key")

df.belief <- df.belief %>%
  left_join(belief_map, by = "name") %>%
  select(-name) %>%
  filter(!is.na(q_html_key))

# Compute absolute distance to ground-truth prevalence.
df.belief <- df.belief %>%
  left_join(
    compute_ground_truth_privacy_distribution() %>%
      rename(universe_rating = mean_rating) %>%
      mutate(q_html_key = paste(feature, field, sep = "-")) %>%
      select(q_html_key, universe_rating),
    by = "q_html_key"
  ) %>%
  mutate(abs_distance_overall_u = abs(value - universe_rating))

df.belief <- df.belief %>%
  select(-value, -universe_rating) %>%
  filter(!is.na(abs_distance_overall_u)) %>%
  pivot_wider(
    id_cols     = c(emailid, compensation),
    names_from  = q_html_key,
    values_fn   = list(abs_distance_overall_u = ~ first(.x)),
    values_from = abs_distance_overall_u,
    names_sort  = TRUE
  ) %>%
  left_join(survey_merged %>% select(emailid, matches("pewq"), "WTA")) %>%
  mutate(WTA = log(as.numeric(WTA)))

# Demean numeric columns within sample, then aggregate by (compensation, var_group).
df_demeaned <- df.belief %>%
  filter(!is.na(compensation)) %>%
  mutate(across(where(is.numeric) & !any_of("compensation"),
                ~ .x - mean(.x, na.rm = TRUE)))

long <- df_demeaned %>%
  pivot_longer(cols = where(is.numeric) & !any_of("compensation"),
               names_to = "variable", values_to = "value") %>%
  mutate(var_group = case_when(
    str_detect(variable, "collect")  ~ "Beliefs: Collection",
    str_detect(variable, "share")    ~ "Beliefs: Control",
    str_detect(variable, "(?i)pew")  ~ "PEW",
    toupper(variable) == "WTA"       ~ "WTA",
    TRUE                             ~ "Beliefs: Use"
  ))

sum_by_group <- long %>%
  group_by(compensation, var_group) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    se   = sd(value,   na.rm = TRUE) / sqrt(sum(!is.na(value))),
    n    = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(compensation_label = paste0("$", compensation))

ggplot(sum_by_group,
       aes(x = var_group, y = mean,
           color = compensation_label,
           group = compensation_label)) +
  geom_hline_zero(linetype = "dashed", color = "gray50") +
  geom_errorbar(
    aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
    width     = ERRORBAR_WIDTH,
    linewidth = LINE_WIDTH,
    position  = position_dodge(width = DODGE_WIDTH_3)
  ) +
  geom_point(size = POINT_SIZE,
             position = position_dodge(width = DODGE_WIDTH_3)) +
  labs(x = NULL, y = "Mean (demeaned)",
       color = "Survey compensation") +
  theme_privacy_experiment() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(paste0(FIGURES_DIR, "agg_selection_lots.pdf"),
       plot = last_plot(), units = "in", height = 6, width = 10)