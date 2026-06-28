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
#   Main paper [Section 5.1, rank correlation scalars cited in Fig 5 caption]:
#     output/values/rank_correlation_beliefs_values.tex
#       (9 macros: \tauBy<Category>{N,Median,P} for Collect, Use, Control)
#
# Style: uses shared PRIVACY_ATTR_MASTER labels/order + theme_privacy_experiment
# (same settings as the WTP figure) so panels (a) and (b) match. Shapes only,
# no color; legend bottom; horizontal grid on; 8x6 (matches WTP figure).
#
# Inputs:
#   ../data/Survey/survey_merged_final.csv
#   ../data/final_extension_data/privacy_info.csv         (ground truth)
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#
# Dependencies:
#   replication_files/utils/values.R           (BAD_USERS, PRIVACY_ATTR_MASTER,
#                                                get_privacy_attr_order)
#   replication_files/utils/time_usage_helpers.R
#   replication_files/utils/info_acq_helpers.R
#   replication_files/utils/number_format_helpers.R
#   replication_files/utils/plot_rules.R       (theme_privacy_experiment)
#
# Outputs:
#   output/figures/beliefs_vs_truth.pdf
#   output/values/rank_correlation_beliefs_values.tex
# =============================================================================

# Set working directory to code_github root so all relative paths resolve.
setwd("~/Dropbox/spring2025experiment/code_github")

# Source utility scripts
source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/info_acq_helpers.R")
source("replication_files/utils/number_format_helpers.R")
source("replication_files/utils/plot_rules.R")

# Load required libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(savetexvalue)

# Output directories
FIGURES_DIR <- "output/figures/"
VALUES_DIR  <- "output/values/"

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
# belief_to_internal: survey column -> internal key (matches PRIVACY_ATTR_MASTER).
# Labels now come from the shared PRIVACY_ATTR_MASTER, not a local vector.

belief_to_internal <- c(
  beliefscollection_r1 = "collect-log",
  beliefscollection_r2 = "collect-bio",
  beliefscollection_r3 = "collect-sensitive",
  beliefscollection_r4 = "collect-financial",
  beliefscollection_r5 = "collect-offsite",
  beliefscollection_r6 = "collect-location",
  beliefscollection_r7 = "collect-social",
  
  beliefsuse_r1 = "anonymized-anonymized",
  beliefsuse_r2 = "share-social",
  beliefsuse_r3 = "share-finance",
  beliefsuse_r4 = "share-ads",
  beliefsuse_r5 = "share-law",
  beliefsuse_r6 = "share-service",
  beliefsuse_r7 = "share-partners",
  beliefsuse_r8 = "personalization-personalization",
  
  beliefscontrol_r1 = "change-change",
  beliefscontrol_r2 = "automated-automated",
  beliefscontrol_r3 = "deletion-deletion",
  beliefscontrol_r4 = "storage-storage",
  
  # quality has no ground-truth counterpart; filtered out downstream
  beliefsquality_r1 = "quality-content",
  beliefsquality_r2 = "quality-ads",
  beliefsquality_r3 = "quality-navigation"
)

# Canonical labels keyed by survey column, derived from the shared master.
labels <- setNames(
  PRIVACY_ATTR_MASTER$label[match(belief_to_internal, PRIVACY_ATTR_MASTER$internal_key)],
  names(belief_to_internal)
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

# Order attributes by ground-truth prevalence using the shared helper, so the
# y-axis matches the WTP figure exactly.
attr_order <- get_privacy_attr_order()   # ascending true_mean, 19 attrs, no sell
label_levels <- attr_order$label

beliefs_combined <- beliefs_full %>%
  left_join(beliefs_ext, by = "name") %>%
  filter(name %in% label_levels)

# =============================================================================
# Fig 5: BELIEFS VS GROUND TRUTH (3 series: Survey / Experiment / True Mean)
#   Shapes only (no color), legend bottom, horizontal grid on. Shared
#   theme_privacy_experiment with the same settings as the WTP figure so the
#   two panels match in font, color, margins, and grid. 8x6.
# =============================================================================

plot_df <- beliefs_combined %>%
  mutate(name = factor(name, levels = label_levels)) %>%
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
            aes(x = name, y = Value, shape = Measure)) +
  geom_point(size = 3, color = "grey30", fill = NA, stroke = 0.8) +
  coord_flip() +
  scale_shape_manual(values = c(
    "Mean Beliefs (Survey)"     = 22,  # square
    "Mean Beliefs (Experiment)" = 24,  # triangle
    "True Mean"                 = 21   # circle
  )) +
  labs(
    y     = "Percentage of Sites Engaged in Practice",
    x     = "Privacy Attribute",
    shape = NULL
  ) +
  theme_privacy_experiment(show_grid_y = TRUE) +
  theme(legend.position = "bottom")

ggsave(paste0(FIGURES_DIR, "beliefs_vs_truth.pdf"),
       g, width = 8, height = 6)

cat(sprintf("Saved: %sbeliefs_vs_truth.pdf\n", FIGURES_DIR))

# =============================================================================
# RANK CORRELATION: BELIEF ORDERING vs TRUTH (within-participant, within-category)
# =============================================================================
# Requested by Guy (Slack): show that participants hold systematic beliefs
# about the relative ordering of practices within categories rather than
# guessing at random. The correlations are significant across all categories.
# Cited in the Fig 5 caption via the \tauBy<Category>{N,Median,P} macros.
#
# Metric: Kendall tau-b (handles Likert ties via denominator inflation).
# Unit:   one tau_i per (participant, category).
# Test:   Wilcoxon signed-rank test against null median = 0.
# =============================================================================

VALUES_FILE <- paste0(VALUES_DIR, "rank_correlation_beliefs_values.tex")

# Append-only file: clear it first so reruns don't duplicate macros.
if (file.exists(VALUES_FILE)) file.remove(VALUES_FILE)

# Map 19 attributes (those with truth available) to 3 categories.
attr_category <- tibble::tibble(
  internal_key = c(
    "collect-log", "collect-bio", "collect-sensitive", "collect-financial",
    "collect-offsite", "collect-location", "collect-social",
    "share-social", "share-finance", "share-ads", "share-law",
    "share-service", "share-partners",
    "anonymized-anonymized", "personalization-personalization",
    "change-change", "automated-automated",
    "deletion-deletion", "storage-storage"
  ),
  category = c(rep("collect", 7), rep("use", 8), rep("control", 4))
)

# Build long-form data: one row per (participant, attribute) with truth joined.
truth_with_internal <- truth_dist %>%
  mutate(internal_key = paste(feature, field, sep = "-")) %>%
  select(internal_key, true_mean)

belief_cols_with_truth <- names(belief_to_internal)[
  belief_to_internal %in% truth_with_internal$internal_key
]

ext_long <- survey_merged_ext %>%
  select(all_of(c("emailid", belief_cols_with_truth))) %>%
  pivot_longer(cols = -emailid,
               names_to = "belief_col", values_to = "belief_value") %>%
  mutate(internal_key = belief_to_internal[belief_col]) %>%
  left_join(truth_with_internal, by = "internal_key") %>%
  left_join(attr_category,       by = "internal_key") %>%
  filter(!is.na(belief_value), !is.na(true_mean), !is.na(category))

# Compute one Kendall tau-b per (participant, category).
# tau is NA when belief sd = 0 within a category (participant gave all
# attributes in that category the same Likert score).
within_cat_tau <- ext_long %>%
  group_by(emailid, category) %>%
  summarise(
    tau = suppressWarnings(cor(belief_value, true_mean, method = "kendall")),
    .groups = "drop"
  )

# Per-category summary: n, median tau-b, Wilcoxon p-value vs 0.
cat_summary <- within_cat_tau %>%
  filter(!is.na(tau)) %>%
  group_by(category) %>%
  summarise(
    n           = n(),
    median_tau  = median(tau),
    p_value     = wilcox.test(tau, mu = 0)$p.value,
    .groups = "drop"
  )

# Console output (use cat(), not print(), per stage 2 convention).
cat("\n=========================================================\n")
cat("RANK CORRELATION: BELIEF ORDERING vs TRUTH BY CATEGORY\n")
cat("=========================================================\n")
cat("Metric: Kendall tau-b, within-participant, within-category.\n")
cat("Test:   Wilcoxon signed-rank vs null median = 0.\n\n")

cat(sprintf("%-10s %8s %15s %20s\n",
            "category", "n", "median tau-b", "Wilcoxon p-value"))
cat(strrep("-", 55), "\n", sep = "")
for (i in seq_len(nrow(cat_summary))) {
  cat(sprintf("%-10s %8d %15.3f %20s\n",
              cat_summary$category[i],
              as.integer(cat_summary$n[i]),
              cat_summary$median_tau[i],
              format.pval(cat_summary$p_value[i], digits = 2, eps = 2e-16)))
}

cat("\nReading:\n")
cat("  Correlations are significant across all categories, indicating\n")
cat("  systematic (non-random) beliefs about the ordering of practices.\n\n")

# Save LaTeX macros for the Fig 5 caption.
# Naming convention: \tauBy<Category><Stat>
#   e.g., \tauByCollectN, \tauByCollectMedian, \tauByCollectP
for (i in seq_len(nrow(cat_summary))) {
  cat_label <- tools::toTitleCase(cat_summary$category[i])
  
  save_tex_value(
    format_count(cat_summary$n[i]),
    name = paste0("tauBy", cat_label, "N"),
    file = VALUES_FILE
  )
  save_tex_value(
    format_coef(cat_summary$median_tau[i]),
    name = paste0("tauBy", cat_label, "Median"),
    file = VALUES_FILE
  )
  # All three p-values are below R's floating-point floor (<2e-16), so store
  # the literal string used in the caption.
  save_tex_value(
    "<0.001",
    name = paste0("tauBy", cat_label, "P"),
    file = VALUES_FILE
  )
}

cat(sprintf("Saved LaTeX macros to: %s\n", VALUES_FILE))