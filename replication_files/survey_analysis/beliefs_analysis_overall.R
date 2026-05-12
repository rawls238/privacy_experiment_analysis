# =============================================================================
# BELIEFS ANALYSIS OVERALL
# =============================================================================
#
# Produces:
#   Main paper [Section 5.1, Fig 5, fig:privacy_info_beliefs,
#               "Aggregate Beliefs about Privacy Practices"]:
#     output/figures/beliefs_vs_truth.pdf
#
#   The figure displays beliefs for both the extension and survey sample,
#   showing that selection into the experiment does not induce large
#   differences in beliefs about the prevalence of privacy practices online.
#
# Inputs:
#   ../data/Survey/survey_merged_final.csv
#   ../data/final_extension_data/privacy_info.csv         (ground truth)
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#     (extension sample identification)
#
# Dependencies:
#   replication_files/utils/values.R           (BAD_USERS)
#   replication_files/utils/time_usage_helpers.R
#   replication_files/utils/info_acq_helpers.R
#
# Outputs:
#   output/figures/beliefs_vs_truth.pdf
#
# Note: Previous version of this script also produced (now removed as dead code):
#   - agg_belief_correctness.tex             (aggregate by category, not in paper)
#   - agg_indiv_field_belief_correctness.tex (per-attribute table version, not in paper)
#   - pew_all_effects.tex                    (PEW DiD treatment effects, not in paper)
#   - SECTION 2 belief-distance DiD          (console-only)
# Removed dead libraries (verified zero use after dead-code removal):
#   - library(quantreg), library(stargazer), library(xtable),
#     library(acs), library(fixest)
# Removed dead helpers:
#   - convert_to_numeric() (zero call sites)
#   - round_to_quarter()   (only used by removed SECTION 2)
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

# Output directory
FIGURES_DIR <- "output/figures/"

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Compute ground truth privacy practice prevalence from privacy_info.csv:
# share of websites (by feature, field) where rating == "Yes".
compute_ground_truth_privacy_distribution <- function() {
  privacy_info <- read.csv("../data/final_extension_data/privacy_info.csv")
  privacy_info <- privacy_info %>% mutate(rating_numeric = ifelse(rating == "Yes", 1, 0))
  privacy_info %>%
    group_by(feature, field) %>%
    summarise(mean_rating = mean(rating_numeric), .groups = "drop")
}

# Summarise belief columns (already rescaled to 0-100) -> mean & sd by attribute.
compute_privacy_beliefs_table <- function(df, columns_of_interest, labels) {
  existing_columns <- columns_of_interest[columns_of_interest %in% names(df)]
  
  if (length(existing_columns) == 0) {
    cat("No valid belief columns found\n")
    return(data.frame())
  }
  
  column_means     <- sapply(df[existing_columns], function(x) mean(x, na.rm = TRUE))
  column_variances <- sapply(df[existing_columns], function(x) sd(x,   na.rm = TRUE))
  
  belief_sum <- data.frame(
    internal_name = existing_columns,
    name          = unname(labels[existing_columns]),
    mean          = unname(column_means),
    sd            = unname(column_variances),
    stringsAsFactors = FALSE
  )
  
  belief_sum %>% arrange(desc(mean))
}

# =============================================================================
# LOAD AND CLEAN SURVEY DATA
# =============================================================================

cat("=== LOADING MERGED SURVEY DATA ===\n")

survey_merged <- read.csv("../data/Survey/survey_merged_final.csv",
                          stringsAsFactors = FALSE)
cat(sprintf("Loaded survey_merged: %d rows, %d columns\n",
            nrow(survey_merged), ncol(survey_merged)))

# Remove attention-check fails and implausible elapsed-time outliers
survey_merged <- survey_merged %>%
  filter(attentioncheck1 == 3) %>%
  filter(attentioncheck  == 2) %>%
  filter(sys_ElapsedTime < 6000) %>%  # >10h is too long
  filter(sys_ElapsedTime > 600)       # <10m is too short
cat(sprintf("After attention + elapsed-time filters: %d rows\n", nrow(survey_merged)))

# =============================================================================
# IDENTIFY EXTENSION SAMPLE
# =============================================================================
# Extension sample = participants who installed the Chrome extension AND
# passed BAD_USERS filter. The survey sample is the full cleaned survey_merged.
# Both samples are plotted in Fig 5 (per paper Section 5.1 footnote).

experiment_user_info <- read.csv(
  "../data/final_extension_data/experiment_conditions_pilot_july_2024.csv",
  stringsAsFactors = FALSE
)

extension_users <- experiment_user_info %>%
  filter(experiment_condition != "" & !(experiment_id %in% BAD_USERS))

# Email-based linkage (emailid in survey_merged <-> email in experiment_conditions)
extension_emails <- tolower(trimws(extension_users$email))

survey_merged_full <- survey_merged
survey_merged_ext  <- survey_merged %>%
  filter(tolower(trimws(emailid)) %in% extension_emails)

cat(sprintf("Survey sample (full):     %d respondents\n", nrow(survey_merged_full)))
cat(sprintf("Survey sample (extension): %d respondents\n", nrow(survey_merged_ext)))

# =============================================================================
# BELIEF MAPPING (questionnaire columns -> internal privacy attribute keys)
# =============================================================================

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
  
  # beliefsquality (no ground-truth counterpart)
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

belief_cols <- names(belief_to_internal)

# =============================================================================
# RESCALE BELIEFS (1-5 Likert -> 0-100 percentage)
# =============================================================================

rescale_beliefs <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df)) {
      df[[col]] <- (df[[col]] - 1) * 25
    }
  }
  df
}

survey_merged_full <- rescale_beliefs(survey_merged_full, belief_cols)
survey_merged_ext  <- rescale_beliefs(survey_merged_ext,  belief_cols)

# =============================================================================
# COMPUTE MEAN BELIEFS PER ATTRIBUTE (per sample) + JOIN GROUND TRUTH
# =============================================================================

attach_category <- function(belief_tbl) {
  belief_tbl %>%
    mutate(
      feature = sapply(strsplit(belief_to_internal[internal_name], "-"), `[`, 1),
      field   = sapply(strsplit(belief_to_internal[internal_name], "-"), `[`, 2),
      category = case_when(
        grepl("^beliefscollection", internal_name) ~ "collection",
        grepl("^beliefsuse",        internal_name) ~ "use",
        grepl("^beliefscontrol",    internal_name) ~ "control",
        grepl("^beliefsquality",    internal_name) ~ "quality",
        TRUE ~ NA_character_
      )
    )
}

beliefs_full <- compute_privacy_beliefs_table(survey_merged_full, belief_cols, labels) %>%
  attach_category()
beliefs_ext  <- compute_privacy_beliefs_table(survey_merged_ext,  belief_cols, labels) %>%
  attach_category()

# Ground truth: only attributes with a privacy_info counterpart.
truth_dist <- compute_ground_truth_privacy_distribution() %>%
  select(feature, field, mean_rating) %>%
  rename(true_mean = mean_rating) %>%
  mutate(true_mean = 100 * true_mean)

beliefs_full <- beliefs_full %>%
  left_join(truth_dist, by = c("feature", "field")) %>%
  filter(!is.na(true_mean)) %>%
  rename(belief_mean_survey = mean) %>%
  select(name, belief_mean_survey, true_mean)

beliefs_ext <- beliefs_ext %>%
  left_join(truth_dist, by = c("feature", "field")) %>%
  filter(!is.na(true_mean)) %>%
  rename(belief_mean_experiment = mean) %>%
  select(name, belief_mean_experiment)

beliefs_combined <- beliefs_full %>%
  left_join(beliefs_ext, by = "name") %>%
  arrange(true_mean)

# =============================================================================
# Fig 5: BELIEFS VS GROUND TRUTH (3 series: Survey / Experiment / True Mean)
# =============================================================================

plot_df <- beliefs_combined %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  pivot_longer(cols = c(true_mean, belief_mean_survey, belief_mean_experiment),
               names_to = "Measure", values_to = "Value") %>%
  mutate(Measure = recode(Measure,
                          true_mean              = "True Mean",
                          belief_mean_survey     = "Mean Beliefs (Survey)",
                          belief_mean_experiment = "Mean Beliefs (Experiment)")) %>%
  mutate(Measure = factor(Measure,
                          levels = c("Mean Beliefs (Survey)",
                                     "Mean Beliefs (Experiment)",
                                     "True Mean")))

g <- ggplot(plot_df,
            aes(x = name, y = Value,
                fill = Measure, shape = Measure, color = Measure)) +
  geom_point(size = 3) +
  coord_flip() +
  scale_shape_manual(values = c(
    "Mean Beliefs (Survey)"     = 22,  # square
    "Mean Beliefs (Experiment)" = 24,  # triangle
    "True Mean"                 = 21   # circle
  )) +
  scale_fill_manual(values = c(
    "Mean Beliefs (Survey)"     = "#3182bd",  # blue
    "Mean Beliefs (Experiment)" = "#e6550d",  # red/orange
    "True Mean"                 = "#2ca25f"   # green
  )) +
  scale_color_manual(values = c(
    "Mean Beliefs (Survey)"     = "black",
    "Mean Beliefs (Experiment)" = "black",
    "True Mean"                 = "black"
  )) +
  labs(
    y     = "Percentage of Sites Engaged in Practice",
    x     = "Privacy Attribute",
    fill  = NULL, shape = NULL, color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "top"
  )

ggsave(paste0(FIGURES_DIR, "beliefs_vs_truth.pdf"),
       g, width = 8, height = 6)

cat(sprintf("Saved: %sbeliefs_vs_truth.pdf\n", FIGURES_DIR))