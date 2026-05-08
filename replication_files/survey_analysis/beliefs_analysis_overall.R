rm(list = ls())

if (Sys.info()[['nodename']] == 'GSB-P4FVDL7QF6'){
  WD <- "/Users/sggold/Library/CloudStorage/Dropbox/Shared-Project-Folders/Privacy-Experiment/spring2025experiment"
} else {
  WD <- "/Users/guyaridor/Dropbox/Privacy-Experiment/spring2025experiment"
}

setwd(WD)

source("code/utils/values.R")
source("code/utils/time_usage_helpers.R")
source("code/utils/info_acq_helpers.R")

# Set working directory and load required libraries
#WD <- "/Users/markli/Library/CloudStorage/Dropbox/"
#setwd("~/Dropbox/")

# Load required libraries
library(tidyverse)
library(quantreg)
library(lubridate)
library(stargazer)
library(xtable)
library(stringr)
library(acs)
library(fixest)


# Set up directories
FIGURES_DIR <- "results/baseline_survey_descriptives/"
TABLES_DIR <- "results/baseline_survey_descriptives/"

# Create directory structure INCLUDING balance subdirectory
# dir.create("results/baseline_survey_descriptives/", showWarnings = FALSE, recursive = TRUE)
# dir.create("results/baseline_survey_descriptives/balance/", showWarnings = FALSE, recursive = TRUE)

cat("✅ Directory structure created\n\n")

# ============================================================================
# DEFINE ALL HELPER FUNCTIONS
# ============================================================================
round_to_quarter <- function(x) round(x / 0.25) * 0.25
# Convert to numeric for behavior questions
convert_to_numeric <- function(dat, col) {
  col <- ensym(col)
  dat <- dat %>%
    mutate(
      !!col := case_when(
        !!col == "Not at all" ~ 1,
        !!col == "Always" ~ 5,
        !!col != "" ~ as.numeric(!!col),
        TRUE ~ NA_real_
      )
    )
  return(dat)
}

# ============================================================================
# LOAD MAIN DATASETS - TRADITIONAL CSV READING
# ============================================================================

cat("=== LOADING MERGED SURVEY DATA ===\n")

# Load the merged dataset - TRADITIONAL READ
survey_merged <- read.csv("data/Survey/survey_merged_final.csv", stringsAsFactors = FALSE)
cat("✅ Loaded survey_merged:", nrow(survey_merged), "rows,", ncol(survey_merged), "columns\n")

# Remove attention check fails
survey_merged <- survey_merged %>% filter(attentioncheck1 == 3) 
survey_merged <- survey_merged %>% filter(attentioncheck == 2) 

# Remove inappropriate time spent (~ 600 removed, primarily >600)
survey_merged <- survey_merged %>% filter(sys_ElapsedTime < 6000) #over ten hours is too long
survey_merged <- survey_merged %>% filter(sys_ElapsedTime > 600) #under ten minutes is too short

privacy_info_desc <- read.csv("data/final_extension_data/privacy_info_desc.csv")
meta_data <- read.csv("data/final_extension_data/experiment_conditions_pilot_july_2024.csv")

compute_ground_truth_privacy_distribution <- function() {
  privacy_info <- read.csv("data/final_extension_data/privacy_info.csv")
  privacy_info <- privacy_info %>% mutate(rating_numeric = ifelse(rating == "Yes", 1, 0))
  t <- privacy_info %>% group_by(feature, field) %>% summarise(mean_rating = mean(rating_numeric), .groups="drop")
  return(t)
}

compute_privacy_beliefs_table <- function(df, columns_of_interest, labels) {
  existing_columns <- columns_of_interest[columns_of_interest %in% names(df)]
  
  if(length(existing_columns) == 0) {
    cat("⚠️ No valid columns found for beliefs table\n")
    return(data.frame())
  }
  
  column_means <- sapply(df[existing_columns], function(x) mean(x, na.rm = TRUE))
  column_variances <- sapply(df[existing_columns], function(x) sd(x, na.rm = TRUE))
  
  names(column_means) <- labels[names(column_means)]
  column_means <- data.frame(column_means)
  column_means$name <- rownames(column_means)
  column_means$internal_name <- existing_columns
  rownames(column_means) <- NULL
  names(column_means) <- c("mean", "name", "internal_name")
  
  names(column_variances) <- labels[names(column_variances)]
  column_variances <- data.frame(column_variances)
  column_variances$name <- rownames(column_variances)
  column_variances$internal_name <- existing_columns
  rownames(column_variances) <- NULL
  names(column_variances) <- c("sd", "name", "internal_name")
  
  belief_sum <- column_means %>% 
    left_join(column_variances %>% select(sd, internal_name), by="internal_name")
  belief_sum <- belief_sum[c("internal_name", "name", "mean", "sd")]
  belief_sum <- belief_sum %>% arrange(desc(mean))
  
  return(belief_sum)
}


## SECTION 1: Overall belief comparison  ##


### SECTION 1: compute belief summary stats and compare to ground truth ###

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

internal_question <- sapply(strsplit(belief_to_internal, "-"), `[`, 1)
internal_field    <- sapply(strsplit(belief_to_internal, "-"), `[`, 2)

names(internal_question) <- names(belief_to_internal)
names(internal_field)    <- names(belief_to_internal)
labels <- labels[names(belief_to_internal)]
belief_columns_of_interest <- names(labels)

belief_cols <- c(paste0("beliefscollection_r", 1:7),
                 paste0("beliefsuse_r", 1:8),
                 paste0("beliefscontrol_r", 1:4),
                 paste0("beliefsquality_r", 1:3))

for(col in belief_cols) {
  if(col %in% names(survey_merged)) {
    survey_merged[[col]] <- (survey_merged[[col]] - 1) * 25  # Convert 1-5 to 0-100
  }
}
raw_beliefs <- compute_privacy_beliefs_table(survey_merged, belief_columns_of_interest, labels)
raw_beliefs <- raw_beliefs %>%
  mutate(feature = sapply(strsplit(belief_to_internal[internal_name], "-"), `[`, 1),
         field    = sapply(strsplit(belief_to_internal[internal_name], "-"), `[`, 2),
         category          = case_when(
           grepl("^beliefscollection", internal_name) ~ "collection",
           grepl("^beliefsuse",        internal_name) ~ "use",
           grepl("^beliefscontrol",    internal_name) ~ "control",
           grepl("^beliefsquality",    internal_name) ~ "quality",
           TRUE ~ NA_character_
         ))
true_distr_chars <- compute_ground_truth_privacy_distribution()
true_distr_chars <- true_distr_chars %>% select(feature, field, mean_rating) %>% rename(true_mean = mean_rating) %>% mutate(true_mean = 100 * true_mean)
raw_beliefs <- raw_beliefs %>% left_join(true_distr_chars, by=c("feature", "field"))
raw_beliefs_indiv <- raw_beliefs %>% mutate(diff_from_truth = mean - true_mean) %>% rename(belief_mean = mean) %>% filter(!is.na(true_mean)) %>% select(name, belief_mean, true_mean, diff_from_truth, sd) %>% arrange(desc(diff_from_truth))
raw_beliefs_agg <- raw_beliefs %>% group_by(category) %>% summarise(cat_mean = mean(true_mean, na.rm=T), cat_beliefs = mean(mean, na.rm=T), cat_beliefs_sd = sd(mean, na.rm=T), .groups='drop') %>% mutate(diff_from_truth = cat_beliefs - cat_mean) %>% arrange(desc(diff_from_truth))


raw_beliefs_agg <- raw_beliefs_agg %>%
  dplyr::rename(
    `Data Type` = category,
    `True Mean` = cat_mean,
    `Mean Beliefs` = cat_beliefs,
    `SD Beliefs` = cat_beliefs_sd,
    `Difference from Truth` = diff_from_truth,
  )

tab <- xtable(raw_beliefs_agg, caption = "Beliefs vs Truth", digits = c(0,0,1,1,1,1))

print(tab,
      include.rownames = FALSE,
      file = paste(TABLES_DIR, "agg_belief_correctness.tex"),
      sanitize.text.function = identity)

raw_beliefs_indiv <- raw_beliefs_indiv %>%
  dplyr::rename(
    `Data Type` = name,
    `True Mean` = true_mean,
    `Mean Beliefs` = belief_mean,
    `SD Beliefs` = sd,
    `Difference from Truth` = diff_from_truth,
  )

tab <- xtable(raw_beliefs_indiv, caption = "Beliefs vs Truth", digits = c(0,0,1,1,1,1))

print(tab,
      include.rownames = FALSE,
      file = paste(TABLES_DIR, "agg_indiv_field_belief_correctness.tex"),
      sanitize.text.function = identity)


# Step 1: Compute the 95% Confidence Intervals
N <- nrow(survey_merged)

raw_beliefs_indiv_ci <- raw_beliefs_indiv %>%
  mutate(
    SE = `SD Beliefs` / sqrt(N),                          # Standard Error
    CI_low = `Mean Beliefs` - 1.96 * SE,                  # Lower 95% CI
    CI_high = `Mean Beliefs` + 1.96 * SE,                 # Upper 95% CI
    `Data Type` = factor(`Data Type`, 
                         levels = unique(`Data Type`[order(`True Mean`)])) # order for plotting
  )

# Step 2: Prepare data for plotting both True Mean and Mean Beliefs
points_df <- raw_beliefs_indiv_ci %>%
  select(`Data Type`, `True Mean`, `Mean Beliefs`) %>%
  pivot_longer(cols = c(`True Mean`, `Mean Beliefs`),
               names_to = "Measure", values_to = "Value")

# Step 3: Plot with 95% CI around Mean Beliefs
g <- ggplot() +
  # Confidence intervals (95%)
  geom_errorbar(
    data = raw_beliefs_indiv_ci,
    aes(x = `Data Type`, ymin = CI_low, ymax = CI_high),
    width = 0.25, color = "gray40", alpha = 0.7
  ) +
  # Points for Mean Beliefs and True Means
  geom_point(
    data = points_df,
    aes(x = `Data Type`, y = Value, fill = Measure, shape = Measure),
    size = 3, color = "black"
  ) +
  coord_flip() +
  scale_shape_manual(values = c("True Mean" = 21, "Mean Beliefs" = 22)) +
  scale_fill_manual(values  = c("True Mean" = "#2ca25f", "Mean Beliefs" = "#3182bd")) +
  labs(
    y = "Percentage of Sites Engaged in Practice",
    x = "Privacy Attribute",
    fill  = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave(paste0(FIGURES_DIR, "beliefs_vs_truth.pdf"), g, width = 8, height = 6)


exposed_websites <- get_aggregated_time_data_with_privacy_info()


non_weighted_summary <- exposed_websites %>%
  group_by(experiment_id) %>%
  summarise(across(starts_with("p_"), ~ mean(.x, na.rm = TRUE), .names = "{.col}"),
            .groups = "drop")

weighted_summary <- exposed_websites %>%
  group_by(experiment_id) %>%
  summarise(across(
    starts_with("p_"),
    ~ sum(.x * time_share, na.rm = TRUE) / sum(time_share, na.rm = TRUE),
    .names = "weighted_{.col}"
  ), .groups = "drop")

combined_summary <- non_weighted_summary %>%
  inner_join(weighted_summary, by = "experiment_id")

# ------------------------------------------------------------
# 2) Exposure table for ALL html keys (not just q1/q2)
# 
# ------------------------------------------------------------
expo_unw <- combined_summary %>%
  select(experiment_id, starts_with("p_")) %>%
  pivot_longer(-experiment_id, names_to = "p_col", values_to = "exposure_value") %>%
  mutate(q_html_key = str_remove(p_col, "^p_")) %>%
  select(-p_col)

expo_w <- combined_summary %>%
  select(experiment_id, starts_with("weighted_p_")) %>%
  pivot_longer(-experiment_id, names_to = "p_col", values_to = "weighted_exposure_value") %>%
  mutate(q_html_key = str_remove(p_col, "^weighted_p_")) %>%
  select(-p_col)

exposures_all <- expo_unw %>%
  left_join(expo_w, by = c("experiment_id", "q_html_key")) %>%
  mutate(
    exposure_value_rounded          = round_to_quarter(exposure_value),
    weighted_exposure_value_rounded = round_to_quarter(weighted_exposure_value)
  )

# ------------------------------------------------------------
# 3) Map each email -> experiment_id (+ condition) once
# ------------------------------------------------------------
email_exp <- get_personalized_info_long() %>%
  select(email, experiment_id, experiment_condition) %>%
  distinct() %>%
  rename(emailid = email)

# ------------------------------------------------------------
# 4) Beliefs → long 
# ------------------------------------------------------------
df.overall_belief_change <- survey_merged %>% 
  select(emailid, starts_with("beliefs")) %>%
  left_join(email_exp, by = "emailid") %>%
  pivot_longer(
    cols = starts_with("beliefs"),
    names_to = "name",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  mutate(
    post  = str_ends(name, "_post"),
    var   = str_remove(name, "_post$"),
    value = as.numeric(str_remove(value, "%"))
  ) %>%
  select(-name)

# lets see how much change there really is
df.belief_test <- df.overall_belief_change %>%
  pivot_wider(
    id_cols = c('emailid','experiment_id','experiment_condition','var'),
    names_from = post,
    values_from = value
  ) %>%
  mutate(diff = `TRUE` - `FALSE`) %>%
  group_by(emailid, experiment_id, experiment_condition) %>%
  summarise(
    mean_FALSE = mean(`FALSE`, na.rm = TRUE),
    sd_FALSE   = sd(`FALSE`,   na.rm = TRUE),
    mean_TRUE  = mean(`TRUE`,  na.rm = TRUE),
    sd_TRUE    = sd(`TRUE`,    na.rm = TRUE),
    mean_diff  = mean(`TRUE` - `FALSE`, na.rm = TRUE),
    sd_diff    = sd(`TRUE`  - `FALSE`,  na.rm = TRUE),
    n_pairs    = sum(!is.na(`TRUE`) & !is.na(`FALSE`)),
    .groups = "drop"
  ) %>%
  filter(!is.na(experiment_id))

# map belief var -> internal q_html_key
belief_map <- enframe(belief_to_internal, name = "var", value = "q_html_key")

df.overall_belief_change <- df.overall_belief_change %>%
  left_join(belief_map, by = "var") %>%
  select(-var) %>%
  filter(!is.na(q_html_key)) %>%
  # join ALL exposures by (experiment_id, q_html_key)
  left_join(exposures_all, by = c("experiment_id", "q_html_key"))

# ------------------------------------------------------------
# 5) Ground truth + six distance outcomes (u = unrounded, r = rounded)
#    (a) overall vs ground truth
#    (b) vs exposed (unweighted)
#    (c) vs exposed (time-weighted)
# ------------------------------------------------------------
df.overall_belief_change <- df.overall_belief_change %>%
  left_join(
    compute_ground_truth_privacy_distribution() %>%
      rename(universe_rating = mean_rating) %>%
      mutate(
        q_html_key = paste(feature, field, sep = "-"),
        universe_rating_rounded = round_to_quarter(universe_rating)
      ) %>%
      select(q_html_key, universe_rating, universe_rating_rounded),
    by = "q_html_key"
  ) %>%
  mutate(
    # (a) Ground truth
    abs_distance_overall_u    = abs(value/100 - universe_rating),
    abs_distance_overall_r    = abs(value/100 - universe_rating_rounded),
    
    # (b) Exposed (unweighted)
    abs_distance_exposed_u    = abs(value/100 - exposure_value),
    abs_distance_exposed_r    = abs(value/100 - exposure_value_rounded),
    
    # (c) Exposed (time-weighted)
    abs_distance_exposed_wt_u = abs(value/100 - weighted_exposure_value),
    abs_distance_exposed_wt_r = abs(value/100 - weighted_exposure_value_rounded)
  )

# df.belief_test <- df.overall_belief_change %>%
#   select(emailid, experiment_id, experiment_condition, q_html_key, post, abs_distance_overall_u) %>%
#   pivot_wider(
#     id_cols = c('emailid','experiment_id','experiment_condition','q_html_key'),
#     names_from = post,
#     values_from = abs_distance_overall_u
#   ) %>%
#   mutate(diff = `TRUE` - `FALSE`) %>%
#   group_by(emailid, experiment_id, experiment_condition) %>%
#   summarise(
#     mean_FALSE = mean(`FALSE`, na.rm = TRUE),
#     sd_FALSE   = sd(`FALSE`,   na.rm = TRUE),
#     mean_TRUE  = mean(`TRUE`,  na.rm = TRUE),
#     sd_TRUE    = sd(`TRUE`,    na.rm = TRUE),
#     mean_diff  = mean(`TRUE` - `FALSE`, na.rm = TRUE),
#     sd_diff    = sd(`TRUE`  - `FALSE`,  na.rm = TRUE),
#     n_pairs    = sum(!is.na(`TRUE`) & !is.na(`FALSE`)),
#     .groups = "drop"
#   ) %>%
#   filter(!is.na(experiment_id))

# ------------------------------------------------------------
# 6) (At the end) Merge in q1/q2 html keys for convenience/reporting
# ------------------------------------------------------------
personalized_info_end <- get_personalized_info_long() %>%
  group_by(experiment_id) %>% slice(1) %>% ungroup() %>%
  select(email, experiment_id, experiment_condition, q1_html_key, q2_html_key) %>%
  mutate(
    q1_html_key = str_to_lower(q1_html_key),
    q2_html_key = str_to_lower(q2_html_key)
  ) %>%
  rename(emailid = email)

df.overall_belief_change <- df.overall_belief_change %>%
  left_join(personalized_info_end,
            by = c("emailid", "experiment_id", "experiment_condition"))

df_for_models <- df.overall_belief_change %>%
  left_join(
    meta_data %>% filter(!is.na(block_idx)) %>% select(email, wave_id, block_idx) %>% rename(emailid = email),
    by = "emailid"
  ) %>%
  mutate(
    wave_id = ifelse(wave_id == 3, 2, wave_id),
    block_by_wave = paste(wave_id, block_idx, sep = "_")
  )

df_for_models <- df_for_models %>% filter(experiment_condition != "")
df_models_base <- df_for_models %>%
  select(
    emailid, experiment_condition, q_html_key, block_by_wave, post,
    q1_html_key, q2_html_key,
    starts_with("abs_distance_")
  ) %>%
  pivot_longer(
    cols = starts_with("abs_distance_"),
    names_to = "measure",
    values_to = "distance"
  ) %>%
  pivot_wider(
    names_from = post, values_from = distance, names_prefix = "dist_"
  ) %>%
  rename(
    distance_pre  = dist_FALSE,
    distance_post = dist_TRUE
  )

# Optional: readable labels for output
measure_labels <- c(
  abs_distance_overall_u    = "(a) GT unrounded",
  abs_distance_overall_r    = "(a) GT rounded",
  abs_distance_exposed_u    = "(b) Exposed unweighted (u)",
  abs_distance_exposed_r    = "(b) Exposed unweighted (r)",
  abs_distance_exposed_wt_u = "(c) Exposed time-weighted (u)",
  abs_distance_exposed_wt_r = "(c) Exposed time-weighted (r)"
)

df_models_base <- df_models_base %>%
  mutate(measure_label = recode(measure, !!!measure_labels))

# ======================================================
# 3) Define DiD spec on absolute distance
# ======================================================
run_did <- function(dat, MIN_FIELD = 50) {
  min_field <- dat %>%
    group_by(q_html_key, emailid) %>% slice(1) %>% ungroup() %>%
    group_by(q_html_key) %>% summarise(n_field = n()) %>% filter(n_field >= MIN_FIELD)
  print(unique(min_field$q_html_key))
  feols(
    distance_post ~ experiment_condition + distance_pre |  q_html_key + block_by_wave,
    cluster = "emailid",
    data = dat %>% filter(q_html_key %in% unique(min_field$q_html_key))
  )
}

# ======================================================
# 4) Run models for ALL fields
# ======================================================
models_all <- df_models_base %>%
  group_split(measure_label) %>%
  set_names(unique(df_models_base$measure_label)) %>%
  map(run_did)

# ======================================================
# 5) Run models for TREATED fields only (q1/q2 per respondent)
# ======================================================
df_models_treated <- df_models_base %>%
  filter((!is.na(q1_html_key) & q_html_key == q1_html_key) |
           (!is.na(q2_html_key) & q_html_key == q2_html_key))

test <- df_models_treated %>% mutate(diff = distance_pre - distance_post)

summary_means <- df_models_treated%>%
  group_by(measure_label, experiment_condition) %>%
  summarise(
    mean_pre  = mean(distance_pre,  na.rm = TRUE),
    mean_post = mean(distance_post, na.rm = TRUE),
    diff_post_pre = mean_post - mean_pre,
    n_obs = n(),
    .groups = "drop"
  )

summary_means <- df_models_base%>%
  group_by(measure_label, experiment_condition) %>%
  summarise(
    mean_pre  = mean(distance_pre,  na.rm = TRUE),
    mean_post = mean(distance_post, na.rm = TRUE),
    diff_post_pre = mean_post - mean_pre,
    n_obs = n(),
    .groups = "drop"
  )

cat("\n==== PRE / POST MEANS BY TREATMENT ====\n")
print(summary_means, n = Inf)

models_treated <- df_models_treated %>%
  group_split(measure_label) %>%
  set_names(unique(df_models_treated$measure_label)) %>%
  map(run_did)

# ======================================================
# 6) Display results
# ======================================================
cat("\n==== ALL FIELDS (DID MODELS) ====\n")
etable(models_all, se = "cluster")

cat("\n\n==== TREATED FIELDS ONLY (q1/q2, DID MODELS) ====\n")
etable(models_treated)


# ============================================================================
# SECTION 2: Overall PEW Changes
# ============================================================================

pew_joined <- survey_merged %>%
  left_join(
    email_exp %>% select(emailid, experiment_condition, experiment_id), by="emailid"
  ) %>%
  #remove people who didnt end up in extension
  filter(completed_both == TRUE) %>%
  filter(experiment_condition != "") %>%
  mutate(
    
    # PRE-TREATMENT VALUES 
    
    pewq2_pre = case_when(
      pewq2 == 5 ~ 0.0,
      pewq2 == 4 ~ 0.25,
      pewq2 == 3 ~ 0.5,
      pewq2 == 2 ~ 0.75,
      pewq2 == 1 ~ 1.0,
      TRUE ~ NA_real_
    ),
    
    pewq1_pre = case_when(
      pewq1 == 5 ~ 0.0,
      pewq1 == 4 ~ 0.25,
      pewq1 == 3 ~ 0.5,
      pewq1 == 2 ~ 0.75,
      pewq1 == 1 ~ 1.0,
      TRUE ~ NA_real_
    ),
    
    pewq3_pre = as.numeric(pewq3 == 1),
    
    pewq4_pre = as.numeric(pewq4 == 1),
    
    # POST-TREATMENT VALUES - FIXED FOR TEXT RESPONSES
    
    # pewq1_post: Text scale for how often read policies
    pewq1_post = case_when(
      pewq1_post == "Never" ~ 0.0,
      pewq1_post == "Rarely" ~ 0.25,
      pewq1_post == "Sometimes" ~ 0.5,
      pewq1_post == "Often" ~ 0.75,
      pewq1_post %in% c("Always", "Always or almost always") ~ 1.0,
      TRUE ~ NA_real_
    ),
    
    # pewq2_post: Text scale for effectiveness
    pewq2_post = case_when(
      pewq2_post == "Not at all effective" ~ 0.0,
      pewq2_post == "Not too effective" ~ 0.25,
      pewq2_post == "Somewhat effective" ~ 0.5,
      pewq2_post == "Very effective" ~ 0.75,
      pewq2_post == "Extremely effective" ~ 1.0,
      TRUE ~ NA_real_
    ),
    
    # pewq3_post: "Yes, I have" or "No, I have not" (refused service)
    pewq3_post = as.numeric(pewq3_post == "Yes, I have"),
    
    # pewq4_post: "True" or "False" (knowledge question)
    pewq4_post = as.numeric(pewq4_post == "True")
    
  ) %>%
  select(emailid, experiment_id, experiment_condition, matches('pew')) %>%
  left_join(
    meta_data %>% select(
      email, wave_id, block_idx
    ) %>%
      rename(emailid = email)
  ) %>%
  mutate(wave_id = ifelse(wave_id == 3, 2, wave_id)) %>%
  mutate(block_by_wave = paste(wave_id,block_idx,sep = '_')) %>%
  distinct()

#pew_joined <- pew_joined %>%
#  mutate_at(vars(matches('pewq1')), funs(abs(. - 0.3020833))) %>%
#  mutate_at(vars(matches('pewq2')), funs(abs(. - 0.7842105))) %>%
#  mutate_at(vars(matches('pewq3')), funs(abs(. - 0.49)))

# Run regressions if we have data


meta_data <- read.csv("data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
meta_data <- meta_data %>%
  mutate(wave_id = ifelse(wave_id == 3, 2, wave_id)) %>%
  mutate(block_by_wave = paste(wave_id,block_idx,sep = '_'))
experiment_users <- meta_data %>% filter(experiment_condition != "" & !(experiment_id %in% BAD_USERS))

## SECTION 1: measuring treatment effects on privacy policy visits

## get bock
visited_privacy_policy <- get_privacy_policy_visits()

visited_privacy_policy <- visited_privacy_policy %>% mutate(
  treatment_date = case_when(
    wave_id == 1 ~ TREATMENT_DATE_WAVE_1,
    wave_id == 2 ~ TREATMENT_DATE_WAVE_2
  ),
  weeks_since_intervention = as.integer(floor((mdy(day) - ymd(treatment_date)) / 7))
)
visited_privacy_policy_agg <- visited_privacy_policy %>%
  group_by(experiment_id, weeks_since_intervention, experiment_condition, post) %>%
  summarise(number_of_visits = n(), distinct_domains = n_distinct(website_aggregated_high_level)) %>% ungroup()

experiment_users <- meta_data %>% filter(experiment_condition != "" & !(experiment_id %in% BAD_USERS))
user_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(weeks_since_intervention = c(-2, -1, 0, 1, 2, 3, 4, 5))

user_post_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(post = c(0, 1))

visited_privacy_policy_full <- user_grid %>%
  left_join(visited_privacy_policy_agg,
            by = c("experiment_id", "experiment_condition", "weeks_since_intervention")) %>%
  mutate(number_of_visits = replace_na(number_of_visits, 0),
         distinct_domains = replace_na(distinct_domains, 0))
visited_privacy_policy_full <- visited_privacy_policy_full %>% mutate(ever_visited_policy = as.numeric(number_of_visits > 0))
visited_privacy_policy_full <- visited_privacy_policy_full %>% group_by(experiment_id) %>% summarise(ever_visited_policy = sum(ever_visited_policy) > 0) %>% ungroup()

pew_joined <- pew_joined %>% left_join(visited_privacy_policy_full, by="experiment_id")
cat("Running PEW treatment effects regressions:\n\n")

# Model 1: Immediately accept terms
models <- list()
models[['t1']] <- feols(pewq1_post ~ experiment_condition, 
                        data = pew_joined)

# Model 2: Read Privacy Policies
models[['t2']] <- feols(pewq2_post ~ ever_visited_policy, 
                        data = pew_joined)

models[['t3']] <- feols(pewq2_post ~ experiment_condition, 
                        data = pew_joined)

# Model 4: Cookies Knowledge
models[['t4']] <- feols(pewq4_post ~ experiment_condition, 
                        data = pew_joined)


# Create combined table

dict <- c(
  "experiment_conditioninfo"      = "Information Treatment",
  "experiment_conditionsaliency"  = "Saliency Treatment",
  "ever_visited_policyTRUE"     = "Visited Privacy Policy",
  "Intercept"       = "Constant"
)


etable(models,
       headers    = c("Read Policies", "Policy Effectiveness", "Policy Effectiveness", "Cookie Knowledge"),
       dict       = dict,
       tex        = TRUE,
       title      = "PEW Attitudes Treatment Effects",
       file       = paste(TABLES_DIR, "beliefs_treatment_effects", "pew_all_effects.tex")
)

## moved this one out of the model text, since refused service is different from attitudes --> still relevant!
# Model 3: Extensive margin
models[['t3']] <- feols(pewq3_post ~ experiment_condition, 
                        data = pew_joined)