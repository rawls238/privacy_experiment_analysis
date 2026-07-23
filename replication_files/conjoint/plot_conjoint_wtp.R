# ============================================================================
# PLOT CONJOINT WTP: Paper Figures and Macros from the Final WTP-Space Model
# ============================================================================
#
# PURPOSE:
#   Generate the paper outputs that use the final WTP-space conjoint estimates.
#   Experimental Top-2 assignment figures are intentionally handled in the
#   separate script plot_exp_top2_feature.R.
#
# OUTPUTS:
#   - output/figures/individual_heterogeneity_dollars_with_website_extension.pdf
#   - output/figures/individual_heterogeneity_dollars_with_website_full.pdf
#   - output/figures/individual_heterogeneity_experiment_vs_survey.pdf
#   - output/values/conjoint_wtp_values.tex
#
# WTP MACRO CONVENTION:
#   Each respondent contributes a posterior-median full-swing WTP estimate.
#   wtpCollectFinancial and wtpShareFinancial report the cross-respondent
#   median for the corresponding attribute. wtpTopThreeMedian reports the
#   cross-respondent median of each person's top-three WTP sum.
#   REVIEW ALL PROSE CITING THESE MACROS AFTER RECOMPILING:
#     grep -rn "wtpCollectFinancial\|wtpShareFinancial\|wtpTopThreeMedian" --include="*.tex" ..
#
# VALIDATION:
#   - dollar_value_* columns are draw-wise posterior summaries of -2*omega,
#     matching the established plot sign convention (protective = negative).
#   - No ratio conversion (beta / price_coef) is used: WTP is estimated directly
#     in WTP space, and dollar_value is the draw-wise posterior quantity.
#   - No manual clipping or winsorization is applied to violin or boxplot data.
#
# WTP CONVENTION (full swing):
#   Privacy attributes are effects-coded {invasive = -1, protective = +1}.
#   dollar_value_* = -2 * omega (per-unit WTP in dollars times the full swing,
#   using the established plot sign convention). No further scaling is applied.
#
# INPUTS (relative to code_github/):
#   ../results/Conjoint_result/conjoint_with_website/tables/individual_wtp_summary.csv
#   ../results/Conjoint_result/conjoint_with_website/tables/individual_price_coefficients_summary.csv
#   ../data/Survey/final_baseline_survey.csv
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
# ============================================================================

library(tidyverse)
library(savetexvalue)

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/values.R")
source("replication_files/utils/plot_rules.R")
source("replication_files/utils/number_format_helpers.R")

FIGURES_DIR <- "output/figures/"
VALUES_DIR  <- "output/values/"

CONJOINT_TABLE_DIR <- "../results/Conjoint_result/conjoint_with_website/tables"

# Per-person point estimate for the violins/macros. The exported dollar_value
# column equals dollar_value_median (validated); switch to "dollar_value_mean"
# here if the mean is preferred.
POINT_EST <- "dollar_value"

if (!exists("BAD_USERS")) BAD_USERS <- c()

# ============================================================================
# DATA LOADING
# ============================================================================

cat(strrep("=", 80), "\n")
cat("LOADING WTP-SPACE DOLLAR VALUES (full-swing, draw-wise posterior summaries)\n")
cat(strrep("=", 80), "\n")

survey_intro_0703 <- read_csv("../data/Survey/final_baseline_survey.csv",
                              show_col_types = FALSE)
survey_intro_0703 <- survey_intro_0703 %>%
  select(sys_RespNum, emailid) %>%
  mutate(email = tolower(emailid))

meta_data <- read.csv("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
experiment_users <- meta_data %>%
  filter(experiment_condition != "", !(experiment_id %in% BAD_USERS)) %>%
  left_join(survey_intro_0703, by = "email") %>%
  rename(sys_respnum = sys_RespNum) %>%
  select(sys_respnum, experiment_id)

wtp_indiv <- read_csv(
  file.path(CONJOINT_TABLE_DIR, "individual_wtp_summary.csv"),
  show_col_types = FALSE
)

price_coef_indiv <- read_csv(
  file.path(CONJOINT_TABLE_DIR, "individual_price_coefficients_summary.csv"),
  show_col_types = FALSE
)

# --- Format guards (REVERSED vs the old preference-space script) ---
# The WTP-space export must contain dollar_value columns and must NOT contain
# eta_price / price_linear rows (eta lives only in the price coefficient file).
if (!all(c("dollar_value", "dollar_value_median", "omega_median") %in% names(wtp_indiv))) {
  stop("Expected dollar_value / omega columns in individual_wtp_summary.csv. ",
       "The script may be reading an old preference-space table.")
}
if ("eta_price" %in% unique(wtp_indiv$feature_name)) {
  stop("Found eta_price rows in individual_wtp_summary.csv. ",
       "This looks like the OLD preference-space export; point CONJOINT_TABLE_DIR at the WTP-space tables.")
}
if ("price_linear" %in% unique(wtp_indiv$feature_name)) {
  stop("Found price_linear rows in individual_wtp_summary.csv; unexpected for the WTP-space export.")
}
if (!all(c("respondent_N_id", "mean") %in% names(price_coef_indiv))) {
  stop("individual_price_coefficients_summary.csv must contain respondent_N_id and mean columns.")
}
if (any(price_coef_indiv$mean >= 0, na.rm = TRUE)) {
  stop("Expected all exported price coefficients to be negative (-exp(eta)). ",
       "Check individual_price_coefficients_summary.csv.")
}
# ID integrity: parameter and price tables must agree on respondent_N_id -> sys_respnum.
validate_conjoint_id_integrity <- function(indiv_data, price_data) {
  required_indiv_cols <- c("respondent_N_id", "sys_respnum", "feature_name")
  required_price_cols <- c("respondent_N_id", "sys_respnum", "mean")
  
  missing_indiv_cols <- setdiff(required_indiv_cols, names(indiv_data))
  missing_price_cols <- setdiff(required_price_cols, names(price_data))
  
  if (length(missing_indiv_cols) > 0) {
    stop("individual_wtp_summary.csv is missing required columns: ",
         paste(missing_indiv_cols, collapse = ", "))
  }
  if (length(missing_price_cols) > 0) {
    stop("individual_price_coefficients_summary.csv is missing required columns: ",
         paste(missing_price_cols, collapse = ", "))
  }
  
  indiv_map <- indiv_data %>% distinct(respondent_N_id, sys_respnum)
  price_map <- price_data %>% distinct(respondent_N_id, sys_respnum)
  
  if (nrow(indiv_map %>% count(respondent_N_id) %>% filter(n > 1)) > 0) {
    stop("respondent_N_id maps to multiple sys_respnum in individual_wtp_summary.csv.")
  }
  if (nrow(price_map %>% count(respondent_N_id) %>% filter(n > 1)) > 0) {
    stop("respondent_N_id maps to multiple sys_respnum in individual_price_coefficients_summary.csv.")
  }
  
  missing_price_ids <- setdiff(indiv_map$respondent_N_id, price_map$respondent_N_id)
  extra_price_ids   <- setdiff(price_map$respondent_N_id, indiv_map$respondent_N_id)
  if (length(missing_price_ids) > 0) {
    stop("Missing price coefficients for ", length(missing_price_ids), " respondent_N_id values.")
  }
  if (length(extra_price_ids) > 0) {
    stop("Price table has ", length(extra_price_ids), " respondent_N_id values absent from the WTP table.")
  }
  
  joined_map <- indiv_map %>%
    inner_join(price_map, by = "respondent_N_id", suffix = c("_indiv", "_price"))
  if (nrow(joined_map %>% filter(as.character(sys_respnum_indiv) != as.character(sys_respnum_price))) > 0) {
    stop("respondent_N_id maps to different sys_respnum across tables.")
  }
  
  cat("\u2713 ID integrity check passed.\n")
}

validate_conjoint_id_integrity(wtp_indiv, price_coef_indiv)


wtp_indiv <- wtp_indiv %>%
  left_join(experiment_users, by = "sys_respnum") %>%
  mutate(sample_group = ifelse(!is.na(experiment_id), "Extension", "Survey"))


# ============================================================================
# FEATURE DEFINITIONS (labels + category from shared PRIVACY_ATTR_MASTER)
# ============================================================================

feature_labels <- setNames(PRIVACY_ATTR_MASTER$label, PRIVACY_ATTR_MASTER$feature_name)

control_features    <- PRIVACY_ATTR_MASTER$feature_name[PRIVACY_ATTR_MASTER$category == "control"]
use_features        <- PRIVACY_ATTR_MASTER$feature_name[PRIVACY_ATTR_MASTER$category == "use"]
collection_features <- PRIVACY_ATTR_MASTER$feature_name[PRIVACY_ATTR_MASTER$category == "collect"]

privacy_features <- PRIVACY_ATTR_MASTER$feature_name

get_feature_category <- function(feature_name) {
  case_when(
    feature_name %in% control_features    ~ "control",
    feature_name %in% use_features        ~ "use",
    feature_name %in% collection_features ~ "collect",
    TRUE                                  ~ NA_character_
  )
}


# ============================================================================
# BUILD DOLLAR-VALUE FRAME (no conversion needed)
# ============================================================================
# dollar_value_* are already draw-wise posterior summaries of -2*omega — the
# exact posterior under the established sign convention. The previous
# (mean / price_coef) * SWING ratio step is intentionally absent because it
# carried E[beta]/E[lambda] ratio bias.

with_web_dollars <- wtp_indiv %>%
  filter(feature_name %in% privacy_features) %>%
  mutate(
    dollar_value  = .data[[POINT_EST]],
    feature_type  = get_feature_category(feature_name),
    feature_label = feature_labels[feature_name]
  ) %>%
  filter(!is.na(dollar_value), !is.infinite(dollar_value))

cat(sprintf("\nWith Website Model (WTP-space):\n"))
cat(strrep("-", 60), "\n")
cat(sprintf("  Respondents: %d\n", n_distinct(with_web_dollars$respondent_N_id)))
cat(sprintf("  Clean rows: %d\n", nrow(with_web_dollars)))
cat(sprintf("  Point estimate column: %s\n", POINT_EST))
cat(sprintf("  Mean price coef: %.4f\n", mean(price_coef_indiv$mean)))
cat(sprintf("  Median price coef: %.4f\n", median(price_coef_indiv$mean)))


# ============================================================================
# WTP scalar macros (full-swing)
# ============================================================================

ext_dollars <- with_web_dollars %>% filter(sample_group == "Extension")

fin_wtp <- ext_dollars %>%
  filter(feature_name %in% c("collection_financial", "use_financial")) %>%
  group_by(feature_name) %>%
  summarise(
    median_wtp = median(dollar_value, na.rm = TRUE),
    .groups = "drop"
  )

# Each respondent contributes a posterior-median full-swing WTP estimate
# because POINT_EST = dollar_value = dollar_value_median. These macros report
# the cross-respondent median.
wtp_collect_financial <- abs(
  fin_wtp$median_wtp[
    fin_wtp$feature_name == "collection_financial"
  ]
)
wtp_share_financial <- abs(
  fin_wtp$median_wtp[
    fin_wtp$feature_name == "use_financial"
  ]
)

# Top-3 most protective-valued attributes per person, ranked on omega_median
# (equivalent to ranking on protective WTP; lambda-invariant within person).
top3_per_person <- ext_dollars %>%
  group_by(respondent_N_id) %>%
  slice_max(omega_median, n = 3, with_ties = FALSE) %>%
  summarise(wtp_top3 = sum(dollar_value), .groups = "drop")

wtp_top_three_median <- abs(median(top3_per_person$wtp_top3))

cat(sprintf("\nWTP (extension, full-swing):\n"))
cat(sprintf("  collect financial (cross-respondent median) = %.2f\n",
            wtp_collect_financial))
cat(sprintf("  share   financial (cross-respondent median) = %.2f\n",
            wtp_share_financial))
cat(sprintf("  top-3 most valued (cross-respondent median) = %.2f\n",
            wtp_top_three_median))

conjoint_wtp_file <- "conjoint_wtp_values"
suppressWarnings(file.remove(file.path(VALUES_DIR, paste0(conjoint_wtp_file, ".tex"))))
save_tex_value(sprintf("%.2f", wtp_collect_financial),
               name = "wtpCollectFinancial",
               file = file.path(VALUES_DIR, paste0(conjoint_wtp_file, ".tex")))
save_tex_value(sprintf("%.2f", wtp_share_financial),
               name = "wtpShareFinancial",
               file = file.path(VALUES_DIR, paste0(conjoint_wtp_file, ".tex")))
save_tex_value(sprintf("%.2f", wtp_top_three_median),
               name = "wtpTopThreeMedian",
               file = file.path(VALUES_DIR, paste0(conjoint_wtp_file, ".tex")))
cat(sprintf("Saved 3 macros to %s%s.tex\n", VALUES_DIR, conjoint_wtp_file))


# ============================================================================
# SUMMARY STATISTICS (console only)
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("DOLLAR VALUE SUMMARY STATISTICS (full-swing)\n")
cat(strrep("=", 80), "\n")

for (group in c("Extension", "Survey")) {
  group_data <- with_web_dollars %>% filter(sample_group == group)
  cat(sprintf("\n%s (N = %d respondents):\n",
              group, n_distinct(group_data$respondent_N_id)))
  for (feat_type in c("control", "use", "collect")) {
    type_vals <- group_data %>% filter(feature_type == feat_type) %>% pull(dollar_value)
    cat(sprintf("  %-12s: median = $%7.2f, mean = $%7.2f\n",
                feat_type, median(type_vals), mean(type_vals)))
  }
}


# ============================================================================
# PLOT CONSTANTS
#   SCALE_MIN/MAX define the displayed x-axis window only.
#   Violin and boxplot statistics use the original respondent-level WTP values
#   without clipping or winsorization. coord_cartesian() changes only the
#   displayed range.
# ============================================================================

SCALE_MIN <- -12
SCALE_MAX <-  12


# ============================================================================
# Fig 6(a): SINGLE-GROUP VIOLIN PLOT  (unchanged plotting logic)
# ============================================================================

make_dollar_plot <- function(df, output_file,
                             order_by = c("beliefs", "wtp")) {
  order_by <- match.arg(order_by)
  
  if (order_by == "beliefs") {
    ord <- get_privacy_attr_order()
    feature_order <- ord$feature_name
    ordered_labels <- ord$label
  } else {
    # ggplot places the first factor level at the bottom. Sorting medians from
    # largest to smallest therefore displays the smallest median at the top
    # and the largest median at the bottom.
    feature_order <- df %>%
      group_by(feature_name) %>%
      summarise(
        med = median(dollar_value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(med)) %>%
      pull(feature_name)
    
    ordered_labels <- feature_labels[feature_order]
  }
  
  plot_df <- df %>%
    filter(feature_name %in% feature_order) %>%
    mutate(
      feature_label = factor(
        feature_labels[feature_name],
        levels = ordered_labels
      ),
      feature_type = factor(
        feature_type,
        levels = PRIVACY_CATEGORY_ORDER
      )
    )
  
  p <- ggplot(
    plot_df,
    aes(
      x = dollar_value,
      y = feature_label,
      fill = feature_type
    )
  ) +
    geom_violin(
      color = NA,
      scale = "width",
      width = 0.7,
      trim = TRUE,
      adjust = 0.8
    ) +
    geom_boxplot(
      aes(x = dollar_value),
      width = 0.12,
      outlier.shape = NA,
      fill = "white",
      color = BOX_BORDER_COLOR,
      linewidth = 0.3,
      fatten = 1.5
    ) +
    geom_vline(
      xintercept = 0,
      color = ZERO_LINE_COLOR,
      linewidth = 0.5,
      alpha = ZERO_LINE_ALPHA
    ) +
    scale_fill_privacy_category() +
    scale_x_continuous(
      breaks = seq(SCALE_MIN, SCALE_MAX, by = 4),
      labels = scales::dollar_format(prefix = "$", accuracy = 1)
    ) +
    coord_cartesian(xlim = c(SCALE_MIN, SCALE_MAX)) +
    labs(
      x = "Dollar Value",
      y = "Privacy Attribute",
      fill = NULL
    ) +
    theme_privacy_experiment(show_grid_y = TRUE) +
    theme(legend.position = "bottom")
  
  ggsave(output_file, p, width = 8, height = 6, dpi = 300)
  cat(sprintf("Saved: %s\n", output_file))
  p
}


# ============================================================================
# C.5: COMPARISON VIOLIN PLOT (Extension vs Survey)  (unchanged)
# ============================================================================

make_comparison_plot <- function(df, output_file) {
  # Apply one common order to both samples based on the pooled full-sample
  # cross-respondent median. The smallest median appears at the top and the
  # largest median at the bottom.
  feature_order <- df %>%
    group_by(feature_name) %>%
    summarise(
      med = median(dollar_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(med)) %>%
    pull(feature_name)
  
  ordered_labels <- feature_labels[feature_order]
  
  plot_df <- df %>%
    filter(feature_name %in% feature_order) %>%
    mutate(
      feature_label = factor(
        feature_labels[feature_name],
        levels = ordered_labels
      ),
      sample_group = factor(
        sample_group,
        levels = c("Survey", "Extension")
      )
    )
  
  medians_df <- plot_df %>%
    group_by(feature_label, sample_group) %>%
    summarise(
      med_dollar = median(dollar_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  sample_fills <- c(
    "Survey" = BENCHMARK_COLORS[["Survey"]],
    "Extension" = BENCHMARK_COLORS[["Extension"]]
  )
  sample_colors <- sample_fills
  
  p <- ggplot(
    plot_df,
    aes(
      x = dollar_value,
      y = feature_label
    )
  ) +
    geom_violin(
      aes(fill = sample_group),
      color = NA,
      scale = "width",
      width = 0.7,
      trim = TRUE,
      adjust = 0.8,
      position = position_dodge(width = 0.7)
    ) +
    geom_boxplot(
      aes(
        x = dollar_value,
        color = sample_group,
        group = interaction(feature_label, sample_group)
      ),
      width = 0.12,
      outlier.shape = NA,
      fill = "white",
      linewidth = 0.4,
      fatten = 1.5,
      position = position_dodge(width = 0.7)
    ) +
    geom_point(
      data = medians_df,
      aes(
        x = med_dollar,
        y = feature_label,
        color = sample_group
      ),
      size = 1.8,
      shape = 18,
      position = position_dodge(width = 0.7)
    ) +
    geom_vline(
      xintercept = 0,
      color = ZERO_LINE_COLOR,
      linewidth = 0.5,
      alpha = ZERO_LINE_ALPHA
    ) +
    scale_fill_manual(
      values = sample_fills,
      name = "Sample",
      breaks = c("Survey", "Extension")
    ) +
    scale_color_manual(
      values = sample_colors,
      name = "Sample",
      breaks = c("Survey", "Extension")
    ) +
    scale_x_continuous(
      breaks = seq(SCALE_MIN, SCALE_MAX, by = 4),
      labels = scales::dollar_format(prefix = "$", accuracy = 1)
    ) +
    coord_cartesian(xlim = c(SCALE_MIN, SCALE_MAX)) +
    labs(
      x = "Dollar Value",
      y = "Privacy Attribute"
    ) +
    theme_privacy_experiment(show_grid_y = TRUE) +
    theme(legend.position = "bottom")
  
  ggsave(output_file, p, width = 8, height = 6, dpi = 300)
  cat(sprintf("Saved: %s\n", output_file))
  p
}


# ============================================================================
# RUN
# ============================================================================

cat("\n", strrep("=", 80), "\n")
cat("CREATING CONJOINT WTP OUTPUTS\n")
cat(strrep("=", 80), "\n\n")

# Remove the deprecated extension-sample alternative plot if it exists.
obsolete_extension_alt <- paste0(
  FIGURES_DIR,
  "individual_heterogeneity_dollars_with_website_extension_2.pdf"
)
if (file.exists(obsolete_extension_alt)) {
  file.remove(obsolete_extension_alt)
  cat(sprintf("Removed obsolete output: %s\n\n", obsolete_extension_alt))
}

cat("=== Main Paper: Extension-Sample WTP ===\n\n")
make_dollar_plot(
  with_web_dollars %>% filter(sample_group == "Extension"),
  paste0(FIGURES_DIR, "individual_heterogeneity_dollars_with_website_extension.pdf"),
  order_by = "beliefs"
)

cat("\n=== Appendix: Full-Sample WTP ===\n\n")
make_dollar_plot(
  with_web_dollars,
  paste0(FIGURES_DIR, "individual_heterogeneity_dollars_with_website_full.pdf"),
  order_by = "wtp"
)

cat("\n=== Appendix: Extension vs Survey WTP ===\n\n")
make_comparison_plot(
  with_web_dollars,
  paste0(FIGURES_DIR, "individual_heterogeneity_experiment_vs_survey.pdf")
)

cat("\nAll conjoint WTP outputs complete!\n")