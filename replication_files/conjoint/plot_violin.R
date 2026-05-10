# ============================================================================
# PLOT VIOLIN: Privacy Valuations and Most Valued Information
# ============================================================================
#
# This script generates all panels of the privacy WTP figures:
#
#   Main paper [fig:privacy_combined, "Privacy Valuations and Most Valued Information"]:
#     Fig 6(a) [fig:privacy_info_partworth, "WTP for Privacy Attributes"]:
#       individual_heterogeneity_dollars_with_website_extension.pdf
#     Fig 6(b) [fig:privacy_info_top_2, "Most Valued Information"]:
#       top2_privacy_attributes_extension.pdf
#
#   Appendix [fig:privacy_combined_full, "Privacy Valuations and Most Valued Information (Full Sample)"]:
#     Fig C.5(a) [fig:privacy_info_partworth_full]:
#       individual_heterogeneity_dollars_with_website_full.pdf
#     Fig C.5(b) [fig:privacy_info_top_2_full]:
#       top2_privacy_attributes_full.pdf
#
#   Appendix [fig:extension_survey_conjoint_sample, "Privacy Valuations between Extension and Survey Sample"]:
#     Fig C.4: individual_heterogeneity_experiment_vs_survey.pdf
#
# Dependencies: ../utils/plot_rules.R
# Outputs to: ../../output/figures/
# ============================================================================

library(tidyverse)

# Source unified style rules
source("../utils/plot_rules.R")

# ============================================================================
# BAD USERS — define or load externally
# ============================================================================
# TODO: source from a shared config if BAD_USERS is defined elsewhere
# BAD_USERS <- c(...)
if (!exists("BAD_USERS")) BAD_USERS <- c()

# ============================================================================
# DATA LOADING
# ============================================================================

cat(strrep("=", 80), "\n")
cat("CONVERTING BETA PARAMETERS TO RAW DOLLAR VALUES\n")
cat(strrep("=", 80), "\n")

# Survey baseline (for joining sys_RespNum to experiment users)
survey_intro_0703 <- read_csv("data/Survey/final_baseline_survey.csv",
                              show_col_types = FALSE)
survey_intro_0703 <- survey_intro_0703 %>%
  select(sys_RespNum, emailid) %>%
  mutate(email = tolower(emailid))

# Experiment metadata
meta_data <- read.csv("data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
meta_data <- meta_data %>%
  mutate(wave_id     = ifelse(wave_id == 3, 2, wave_id),
         block_by_wave = paste(wave_id, block_idx, sep = "_"))

experiment_users <- meta_data %>%
  filter(experiment_condition != "", !(experiment_id %in% BAD_USERS)) %>%
  left_join(survey_intro_0703, by = "email") %>%
  rename(sys_respnum = sys_RespNum) %>%
  select(sys_respnum, experiment_condition, experiment_id)

# Individual-level conjoint estimates
with_website_indiv <- read_csv(
  "results/Conjoint_result/conjoint_with_website/tables/individual_parameters_summary.csv",
  show_col_types = FALSE
)
with_website_indiv <- with_website_indiv %>%
  left_join(experiment_users, by = "sys_respnum") %>%
  mutate(sample_group = ifelse(!is.na(experiment_id), "Extension", "Survey"))


# ============================================================================
# FEATURE DEFINITIONS
# ============================================================================

feature_labels <- c(
  control_delete    = "Data Deletion",
  control_storage   = "Data is Secured",
  control_automated = "Automated Deletion",
  control_change    = "Notification of Change",
  usel_anonymized   = "Data is Anonymized",
  
  use_law             = "Share w/ Law Enforcement",
  use_advertising     = "Share w/ Advertisers",
  use_financial       = "Share w/ Financial Transactions",
  use_social          = "Share w/ Social Media",
  use_partners        = "Share w/ 3rd Party Partners",
  use_service         = "Share w/ 3rd Party Services",
  use_personalization = "Site Personalization",
  
  collection_log       = "Collect Browsing On-Site",
  collection_offsite   = "Collect Browsing Off-Site",
  collection_sensitive = "Collect Sensitive Info",
  collection_financial = "Collect Financial Data",
  collection_bio       = "Collect Demographic Data",
  collection_location  = "Collect Location",
  collection_social    = "Collect Social Media"
)

control_features <- c("control_delete", "control_storage", "control_automated",
                      "control_change", "usel_anonymized")
use_features <- c("use_law", "use_advertising", "use_financial", "use_social",
                  "use_partners", "use_service", "use_personalization")
collection_features <- c("collection_log", "collection_offsite", "collection_sensitive",
                         "collection_financial", "collection_bio", "collection_location",
                         "collection_social")

privacy_features <- names(feature_labels)

get_feature_category <- function(feature_name) {
  case_when(
    feature_name %in% control_features    ~ "control",
    feature_name %in% use_features        ~ "use",
    feature_name %in% collection_features ~ "collect",
    TRUE                                  ~ NA_character_
  )
}


# ============================================================================
# CONVERSION: Beta → Dollar Value (WTP = β_feature / β_price)
# ============================================================================

convert_to_dollars <- function(indiv_data, model_name) {
  cat(sprintf("\n%s:\n", model_name))
  cat(strrep("-", 60), "\n")
  
  price_data <- indiv_data %>%
    filter(feature_name == "price_linear") %>%
    select(respondent_N_id, price_coef = mean)
  
  cat(sprintf("  Price coefficients: %d individuals\n", nrow(price_data)))
  cat(sprintf("  Mean price coef: %.4f\n", mean(price_data$price_coef)))
  cat(sprintf("  Median price coef: %.4f\n", median(price_data$price_coef)))
  
  privacy_data <- indiv_data %>%
    filter(feature_name %in% privacy_features) %>%
    inner_join(price_data, by = "respondent_N_id") %>%
    mutate(dollar_value = mean / price_coef) %>%
    filter(!is.na(dollar_value), !is.infinite(dollar_value)) %>%
    mutate(
      feature_type  = get_feature_category(feature_name),
      feature_label = feature_labels[feature_name]
    )
  
  cat(sprintf("  Clean rows: %d\n", nrow(privacy_data)))
  privacy_data
}

with_web_dollars <- convert_to_dollars(with_website_indiv, "With Website Model")


# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("DOLLAR VALUE SUMMARY STATISTICS\n")
cat(strrep("=", 80), "\n")

for (group in c("Extension", "Survey")) {
  group_data <- with_web_dollars %>% filter(sample_group == group)
  cat(sprintf("\n%s (N = %d respondents):\n",
              group, n_distinct(group_data$respondent_N_id)))
  for (feat_type in c("control", "use", "collect")) {
    type_vals <- group_data %>%
      filter(feature_type == feat_type) %>%
      pull(dollar_value)
    cat(sprintf("  %-12s: median = $%7.2f, mean = $%7.2f\n",
                feat_type, median(type_vals), mean(type_vals)))
  }
}


# ============================================================================
# PLOT CONSTANTS
# ============================================================================

SCALE_MIN <- -8
SCALE_MAX <-  8
DATA_MIN  <- -30
DATA_MAX  <-  30


# ============================================================================
# Fig 6(a): SINGLE-GROUP VIOLIN PLOT
# ============================================================================
# Kernel density computed on data clipped to [DATA_MIN, DATA_MAX],
# then coord_cartesian zooms display to [SCALE_MIN, SCALE_MAX] without
# dropping data — matching the Python pipeline behavior.
# ============================================================================

make_dollar_plot <- function(df, output_file) {
  feature_order <- df %>%
    group_by(feature_name) %>%
    summarise(med = median(dollar_value), .groups = "drop") %>%
    arrange(med) %>%
    pull(feature_name)
  
  plot_df <- df %>%
    mutate(
      dollar_clipped = pmin(pmax(dollar_value, DATA_MIN), DATA_MAX),
      feature_label  = factor(feature_labels[feature_name],
                              levels = feature_labels[feature_order]),
      feature_type   = factor(feature_type, levels = PRIVACY_CATEGORY_ORDER)
    )
  
  p <- ggplot(plot_df, aes(x = dollar_clipped, y = feature_label,
                           fill = feature_type)) +
    geom_violin(color = NA, scale = "width", width = 0.7,
                trim = FALSE, adjust = 1.5) +
    geom_boxplot(
      aes(x = pmin(pmax(dollar_value, SCALE_MIN), SCALE_MAX)),
      width = 0.12, outlier.shape = NA,
      fill = "white", color = BOX_BORDER_COLOR, linewidth = 0.3,
      fatten = 1.5
    ) +
    geom_vline(xintercept = 0, color = ZERO_LINE_COLOR,
               linewidth = 0.5, alpha = ZERO_LINE_ALPHA) +
    scale_fill_privacy_category() +
    scale_x_continuous(
      breaks = seq(-8, 8, by = 2),
      labels = scales::dollar_format(prefix = "$", accuracy = 1)
    ) +
    coord_cartesian(xlim = c(SCALE_MIN, SCALE_MAX)) +
    labs(x = "Dollar Value", y = "Privacy Attribute", fill = NULL) +
    theme_privacy_experiment(show_grid_y = FALSE) +
    theme(legend.position = "bottom")
  
  ggsave(output_file, p, width = 10, height = 8, dpi = 300)
  cat(sprintf("Saved: %s\n", output_file))
  p
}


# ============================================================================
# C.5: COMPARISON VIOLIN PLOT (Extension vs Survey)
# ============================================================================

make_comparison_plot <- function(df, output_file) {
  feature_order <- df %>%
    group_by(feature_name) %>%
    summarise(med = median(dollar_value), .groups = "drop") %>%
    arrange(med) %>%
    pull(feature_name)
  
  plot_df <- df %>%
    mutate(
      dollar_clipped = pmin(pmax(dollar_value, DATA_MIN), DATA_MAX),
      feature_label  = factor(feature_labels[feature_name],
                              levels = feature_labels[feature_order]),
      sample_group   = factor(sample_group, levels = c("Survey", "Extension"))
    )
  
  # Median per feature x group
  medians_df <- plot_df %>%
    group_by(feature_label, sample_group) %>%
    summarise(med_dollar = median(dollar_value, na.rm = TRUE), .groups = "drop") %>%
    mutate(med_clipped = pmin(pmax(med_dollar, SCALE_MIN), SCALE_MAX))
  
  # Use BENCHMARK_COLORS from plot_rules.R
  sample_fills  <- c("Survey" = BENCHMARK_COLORS[["Survey"]], "Extension" = BENCHMARK_COLORS[["Extension"]])
  sample_colors <- sample_fills
  
  p <- ggplot(plot_df, aes(x = dollar_clipped, y = feature_label)) +
    geom_violin(
      aes(fill = sample_group),
      color = NA, scale = "width", width = 0.7,
      trim = FALSE, adjust = 1.5,
      position = position_dodge(width = 0.7)
    ) +
    geom_boxplot(
      aes(x = pmin(pmax(dollar_value, SCALE_MIN), SCALE_MAX),
          color = sample_group,
          group = interaction(feature_label, sample_group)),
      width = 0.12, outlier.shape = NA,
      fill = "white", linewidth = 0.4, fatten = 1.5,
      position = position_dodge(width = 0.7)
    ) +
    geom_point(
      data = medians_df,
      aes(x = med_clipped, y = feature_label, color = sample_group),
      size = 1.8, shape = 18,
      position = position_dodge(width = 0.7)
    ) +
    geom_vline(xintercept = 0, color = ZERO_LINE_COLOR,
               linewidth = 0.5, alpha = ZERO_LINE_ALPHA) +
    scale_fill_manual(values = sample_fills, name = "Sample",
                      breaks = c("Survey", "Extension")) +
    scale_color_manual(values = sample_colors, name = "Sample",
                       breaks = c("Survey", "Extension")) +
    scale_x_continuous(
      breaks = seq(-8, 8, by = 2),
      labels = scales::dollar_format(prefix = "$", accuracy = 1)
    ) +
    coord_cartesian(xlim = c(SCALE_MIN, SCALE_MAX)) +
    labs(x = "Dollar Value", y = "Privacy Attribute") +
    theme_privacy_experiment(show_grid_y = FALSE) +
    theme(legend.position = "bottom")
  
  ggsave(output_file, p, width = 10, height = 8, dpi = 300)
  cat(sprintf("Saved: %s\n", output_file))
  p
}


# ============================================================================
# Fig 6(b): MOST VALUED INFORMATION — Top-2 Personalized Attributes
# ============================================================================
# Per Equation (3) in the paper, for each respondent i and attribute j:
#   IV_ij = p_ij * (1 - p_ij) * |beta_ij|
# where p_ij is the respondent's belief about attribute prevalence and
# beta_ij is the conjoint part-worth. Each respondent's top-2 attributes
# by IV are selected, then we count how often each attribute appears.
#
# Parameters:
#   use_full_sample:
#     FALSE (default) = extension sample only (in_experiment == "true")
#     TRUE = all conjoint respondents
#
# Input:
#   - data/Conjoint/Conjoint-Finalized/tables/individual_parameters_wide_means.csv
#   - data/Survey/survey_merged_final.csv (baseline beliefs)
#   - data/final_extension_data/experiment_conditions_pilot_july_2024.csv
# ============================================================================

make_top2_info_plot <- function(use_full_sample = FALSE) {
  
  # --- Load conjoint betas (one row per respondent) ---
  utils <- read.csv(
    "data/Conjoint/Conjoint-Finalized/tables/individual_parameters_wide_means.csv",
    stringsAsFactors = FALSE
  )
  
  # --- Load survey beliefs ---
  survey_beliefs <- read.csv("data/Survey/survey_merged_final.csv",
                             stringsAsFactors = FALSE)
  if ("sys_RespNum" %in% names(survey_beliefs)) {
    survey_beliefs <- survey_beliefs %>% rename(RespondentId = sys_RespNum)
  }
  
  # --- Determine sample ---
  if (use_full_sample) {
    # All respondents with both conjoint betas and survey beliefs
    valid_respondents <- intersect(
      unique(utils$RespondentId),
      unique(survey_beliefs$RespondentId)
    )
    cat(sprintf("  Full conjoint sample: %d respondents\n", length(valid_respondents)))
  } else {
    # Extension sample only: in_experiment == "true", all waves
    meta <- read.csv(
      "data/final_extension_data/experiment_conditions_pilot_july_2024.csv",
      stringsAsFactors = FALSE
    )
    exp_emails <- meta %>%
      filter(in_experiment == "true") %>%
      pull(email) %>%
      unique()
    
    resp_email <- survey_beliefs %>%
      select(RespondentId, emailid) %>%
      distinct()
    resp_in_exp <- resp_email %>% filter(emailid %in% exp_emails)
    
    valid_respondents <- intersect(
      unique(utils$RespondentId),
      resp_in_exp$RespondentId
    )
    cat(sprintf("  Extension sample: %d respondents\n", length(valid_respondents)))
  }
  
  # --- Belief column -> conjoint attribute name ---
  belief_to_attr <- c(
    beliefscollection_r1 = "collection_log",
    beliefscollection_r2 = "collection_bio",
    beliefscollection_r3 = "collection_sensitive",
    beliefscollection_r4 = "collection_financial",
    beliefscollection_r5 = "collection_offsite",
    beliefscollection_r6 = "collection_location",
    beliefscollection_r7 = "collection_social",
    beliefsuse_r1        = "usel_anonymized",
    beliefsuse_r2        = "use_social",
    beliefsuse_r3        = "use_financial",
    beliefsuse_r4        = "use_advertising",
    beliefsuse_r5        = "use_law",
    beliefsuse_r6        = "use_service",
    beliefsuse_r7        = "use_partners",
    beliefsuse_r8        = "use_personalization",
    beliefscontrol_r1    = "control_change",
    beliefscontrol_r2    = "control_automated",
    beliefscontrol_r3    = "control_delete",
    beliefscontrol_r4    = "control_storage"
  )
  
  # --- Beliefs: wide -> long, map 1-5 scale -> probability ---
  beliefs_long <- survey_beliefs %>%
    filter(RespondentId %in% valid_respondents) %>%
    select(RespondentId, all_of(names(belief_to_attr))) %>%
    pivot_longer(-RespondentId, names_to = "belief_col", values_to = "belief_raw") %>%
    mutate(
      attribute = belief_to_attr[belief_col],
      p_ij = case_when(
        belief_raw == 1 ~ 0.0,
        belief_raw == 2 ~ 0.25,
        belief_raw == 3 ~ 0.50,
        belief_raw == 4 ~ 0.75,
        belief_raw == 5 ~ 1.0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(RespondentId, attribute, p_ij) %>%
    filter(!is.na(p_ij))
  
  # --- Utilities: wide -> long (privacy attributes only) ---
  utils_long <- utils %>%
    filter(RespondentId %in% valid_respondents) %>%
    select(RespondentId, all_of(unname(belief_to_attr))) %>%
    pivot_longer(-RespondentId, names_to = "attribute", values_to = "beta_ij")
  
  # --- Information value: IV = p(1-p)|beta| ---
  iv_df <- inner_join(utils_long, beliefs_long,
                      by = c("RespondentId", "attribute")) %>%
    mutate(information_value = p_ij * (1 - p_ij) * abs(beta_ij))
  
  cat(sprintf("  IV computed: %d respondents, %d rows\n",
              n_distinct(iv_df$RespondentId), nrow(iv_df)))
  
  # --- Top 2 per respondent ---
  top2 <- iv_df %>%
    group_by(RespondentId) %>%
    slice_max(information_value, n = 2) %>%
    ungroup()
  
  # --- Count frequency and build plot data ---
  cat_cap <- c(control = "Control", use = "Use", collect = "Collect")
  
  top2_counts <- top2 %>%
    count(attribute, name = "n_users") %>%
    mutate(
      label        = feature_labels[attribute],
      category     = get_feature_category(attribute),
      category_cap = factor(cat_cap[category],
                            levels = c("Control", "Use", "Collect"))
    ) %>%
    arrange(n_users) %>%
    mutate(label = factor(label, levels = label))
  
  fill_colors <- c(
    "Control" = unname(PRIVACY_CATEGORY_COLORS["control"]),
    "Use"     = unname(PRIVACY_CATEGORY_COLORS["use"]),
    "Collect" = unname(PRIVACY_CATEGORY_COLORS["collect"])
  )
  
  # --- Plot ---
  g <- ggplot(top2_counts, aes(x = label, y = n_users, fill = category_cap)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = n_users), hjust = -0.15, size = 3.2, color = TEXT_COLOR) +
    coord_flip() +
    scale_fill_manual(values = fill_colors, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    theme_privacy_experiment(show_grid_y = FALSE) +
    labs(
      x = NULL,
      y = "Count of Appearances in Users' Top-2 Preferences"
    )
  
  return(g)
}


# ============================================================================
# RUN
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("CREATING PLOTS\n")
cat(strrep("=", 80), "\n\n")

FIGURES_DIR <- "../../output/figures/"

# --- Main paper: Extension sample ---
cat("=== Main Paper (Extension Sample) ===\n\n")

cat("--- Fig 6(a): WTP Violin (extension) ---\n")
make_dollar_plot(
  with_web_dollars %>% filter(sample_group == "Extension"),
  paste0(FIGURES_DIR, "individual_heterogeneity_dollars_with_website_extension.pdf")
)

cat("\n--- Fig 6(b): Top-2 Information (extension) ---\n")
g_ext <- make_top2_info_plot(use_full_sample = FALSE)
ggsave(paste0(FIGURES_DIR, "top2_privacy_attributes_extension.pdf"),
       g_ext, width = 10, height = 8)
cat("Saved: top2_privacy_attributes_extension.pdf\n")

# --- Appendix: Full conjoint sample ---
cat("\n=== Appendix (Full Conjoint Sample) ===\n\n")

cat("--- Fig C.5(a): WTP Violin (full) ---\n")
make_dollar_plot(
  with_web_dollars,
  paste0(FIGURES_DIR, "individual_heterogeneity_dollars_with_website_full.pdf")
)

cat("\n--- Fig C.5(b): Top-2 Information (full) ---\n")
g_full <- make_top2_info_plot(use_full_sample = TRUE)
ggsave(paste0(FIGURES_DIR, "top2_privacy_attributes_full.pdf"),
       g_full, width = 10, height = 8)
cat("Saved: top2_privacy_attributes_full.pdf\n")

# --- Appendix C.4: Extension vs Survey comparison ---
cat("\n=== Appendix C.4: Extension vs Survey Comparison ===\n\n")
make_comparison_plot(
  with_web_dollars,
  paste0(FIGURES_DIR, "individual_heterogeneity_experiment_vs_survey.pdf")
)

cat("\nAll plots complete!\n")