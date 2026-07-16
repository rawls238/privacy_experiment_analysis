# ============================================================================
# PLOT VIOLIN (WTP-space): Privacy Valuations and Most Valued Information
# ============================================================================
#
# ADAPTED FOR THE WTP-SPACE MODEL EXPORTS (conjoint_wtp run). FINAL VERSION.
#
# STATUS:
#   - Table validation: 27/27 checks passed (validate_conjoint_wtp_tables.R)
#   - Pipeline pre-flight: 41/41 checks passed (test_plot_violin_wtp.R)
#   - Old preference-space tables archived in ../old_tables_2/ alongside the
#     original plot_violin.R; this script pairs with the WTP-space tables/.
#
# EXPECTED MACRO CHANGES vs the previous .tex (verified in pre-flight):
#   wtpCollectFinancial  9.01 -> ~5.02  (-44%)
#   wtpShareFinancial    7.67 -> ~4.21  (-45%)
#   wtpTopThreeMedian   18.12 -> ~17.52 ( -3%)
#   Mean-based macros drop because the old E[beta]/E[lambda] ratio tails
#   inflated means far above medians; median-based macros are stable.
#   REVIEW ALL PROSE CITING THESE MACROS AFTER RECOMPILING:
#     grep -rn "wtpCollectFinancial\|wtpShareFinancial\|wtpTopThreeMedian" --include="*.tex" ..
# Validated by validate_conjoint_wtp_tables.R (27/27 checks passed):
#   - dollar_value_* columns are draw-wise posterior summaries of -2*omega,
#     matching the OLD plot sign convention exactly (protective = negative).
#   - No ratio conversion (beta / price_coef) is needed or wanted: the old
#     posterior-mean ratio E[beta]/E[lambda] carried ratio bias; dollar_value
#     is the exact draw-wise posterior quantity.
#   - Old vs new attribute medians: Spearman rho = 0.991; individual 5-95
#     tail width shrinks from ~$20 to ~$6.5.
#
# Produces (same filenames as before):
#   Fig 6(a):  individual_heterogeneity_dollars_with_website_extension.pdf
#              individual_heterogeneity_dollars_with_website_extension_2.pdf
#   Fig 6(b):  top2_privacy_attributes_extension.pdf
#   Fig C.5(a): individual_heterogeneity_dollars_with_website_full.pdf
#   Fig C.5(b): top2_privacy_attributes_full.pdf
#   Fig C.4:   individual_heterogeneity_experiment_vs_survey.pdf
#   WTP scalars: output/values/conjoint_wtp_values.tex
#
# WTP CONVENTION (full swing): privacy attributes are effects-coded
# {invasive = -1, protective = +1}. dollar_value_* = -2 * omega (per-unit WTP
# in $ times the full swing, old sign convention). No further scaling here.
#
# TOP-2 FIGURE NOTE: information value IV_ij = p_ij*(1-p_ij)*|beta_ij| with
# beta = lambda * omega. lambda_i > 0 is constant within person, so the
# WITHIN-PERSON top-2 ranking is invariant to lambda; we rank on |omega|
# (dollar-scale IV) built from individual_wtp_summary.csv. The old wide-means
# beta table is therefore not required.
#
# Inputs (relative to code_github/):
#   ../results/Conjoint_result/conjoint_with_website/tables/individual_wtp_summary.csv
#   ../results/Conjoint_result/conjoint_with_website/tables/individual_price_coefficients_summary.csv
#   ../data/Survey/final_baseline_survey.csv
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#   ../data/Survey/survey_merged_final.csv
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
meta_data <- meta_data %>%
  mutate(wave_id     = ifelse(wave_id == 3, 2, wave_id),
         block_by_wave = paste(wave_id, block_idx, sep = "_"))

experiment_users <- meta_data %>%
  filter(experiment_condition != "", !(experiment_id %in% BAD_USERS)) %>%
  left_join(survey_intro_0703, by = "email") %>%
  rename(sys_respnum = sys_RespNum) %>%
  select(sys_respnum, experiment_condition, experiment_id)

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
# exact posterior of the old sign convention. The old (mean / price_coef) *
# SWING ratio step is intentionally GONE (it carried E[beta]/E[lambda] bias).

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
  summarise(mean_wtp = mean(dollar_value), .groups = "drop")

wtp_collect_financial <- abs(fin_wtp$mean_wtp[fin_wtp$feature_name == "collection_financial"])
wtp_share_financial   <- abs(fin_wtp$mean_wtp[fin_wtp$feature_name == "use_financial"])

# Top-3 most protective-valued attributes per person, ranked on omega_median
# (equivalent to ranking on protective WTP; lambda-invariant within person).
top3_per_person <- ext_dollars %>%
  group_by(respondent_N_id) %>%
  slice_max(omega_median, n = 3, with_ties = FALSE) %>%
  summarise(wtp_top3 = sum(dollar_value), .groups = "drop")

wtp_top_three_median <- abs(median(top3_per_person$wtp_top3))

cat(sprintf("\nWTP (extension, full-swing):\n"))
cat(sprintf("  collect financial (mean) = %.2f\n", wtp_collect_financial))
cat(sprintf("  share   financial (mean) = %.2f\n", wtp_share_financial))
cat(sprintf("  top-3 most valued (median) = %.2f\n", wtp_top_three_median))

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
#   SCALE_MIN/MAX: displayed x-axis range. WTP-space tails are ~3x narrower
#   (5-95 width ~$6.5 vs ~$20 old), so -12..12 now covers nearly all mass.
#   DATA_MIN/MAX:  clip for extreme values (rarely binds in the new export).
# ============================================================================

SCALE_MIN <- -12
SCALE_MAX <-  12
DATA_MIN  <- -60
DATA_MAX  <-  60


# ============================================================================
# Fig 6(a): SINGLE-GROUP VIOLIN PLOT  (unchanged plotting logic)
# ============================================================================

make_dollar_plot <- function(df, output_file, order_by = c("beliefs", "wtp")) {
  order_by <- match.arg(order_by)
  
  if (order_by == "beliefs") {
    ord <- get_privacy_attr_order()          # true_mean ascending
    feature_order  <- ord$feature_name       # no rev: highest true_mean ends at top
    ordered_labels <- ord$label
  } else {
    feature_order <- df %>%
      group_by(feature_name) %>%
      summarise(med = median(dollar_value), .groups = "drop") %>%
      arrange(desc(med)) %>%                 # lowest WTP last -> drawn at top
      pull(feature_name)
    ordered_labels <- feature_labels[feature_order]
  }
  
  plot_df <- df %>%
    filter(feature_name %in% feature_order) %>%
    mutate(
      dollar_clipped = pmin(pmax(dollar_value, DATA_MIN), DATA_MAX),
      feature_label  = factor(feature_labels[feature_name], levels = ordered_labels),
      feature_type   = factor(feature_type, levels = PRIVACY_CATEGORY_ORDER)
    )
  
  p <- ggplot(plot_df, aes(x = dollar_clipped, y = feature_label,
                           fill = feature_type)) +
    geom_violin(color = NA, scale = "width", width = 0.7,
                trim = TRUE, adjust = 0.8) +
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
      breaks = seq(SCALE_MIN, SCALE_MAX, by = 4),
      labels = scales::dollar_format(prefix = "$", accuracy = 1)
    ) +
    coord_cartesian(xlim = c(SCALE_MIN, SCALE_MAX)) +
    labs(x = "Dollar Value", y = "Privacy Attribute", fill = NULL) +
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
  ord <- get_privacy_attr_order()
  feature_order  <- ord$feature_name
  ordered_labels <- ord$label
  
  plot_df <- df %>%
    filter(feature_name %in% feature_order) %>%
    mutate(
      dollar_clipped = pmin(pmax(dollar_value, DATA_MIN), DATA_MAX),
      feature_label  = factor(feature_labels[feature_name], levels = ordered_labels),
      sample_group   = factor(sample_group, levels = c("Survey", "Extension"))
    )
  
  medians_df <- plot_df %>%
    group_by(feature_label, sample_group) %>%
    summarise(med_dollar = median(dollar_value, na.rm = TRUE), .groups = "drop") %>%
    mutate(med_clipped = pmin(pmax(med_dollar, SCALE_MIN), SCALE_MAX))
  
  sample_fills  <- c("Survey" = BENCHMARK_COLORS[["Survey"]], "Extension" = BENCHMARK_COLORS[["Extension"]])
  sample_colors <- sample_fills
  
  p <- ggplot(plot_df, aes(x = dollar_clipped, y = feature_label)) +
    geom_violin(
      aes(fill = sample_group),
      color = NA, scale = "width", width = 0.7,
      trim = TRUE, adjust = 0.8,
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
      breaks = seq(SCALE_MIN, SCALE_MAX, by = 4),
      labels = scales::dollar_format(prefix = "$", accuracy = 1)
    ) +
    coord_cartesian(xlim = c(SCALE_MIN, SCALE_MAX)) +
    labs(x = "Dollar Value", y = "Privacy Attribute") +
    theme_privacy_experiment(show_grid_y = TRUE) +
    theme(legend.position = "bottom")
  
  ggsave(output_file, p, width = 8, height = 6, dpi = 300)
  cat(sprintf("Saved: %s\n", output_file))
  p
}


# ============================================================================
# Fig 6(b): MOST VALUED INFORMATION — Top-2 Personalized Attributes
# IV_ij = p_ij * (1 - p_ij) * |omega_ij| (dollar-scale information value).
# WITHIN-PERSON top-2 is invariant to lambda_i > 0, so ranking on omega is
# equivalent to the old beta-based ranking; the wide beta table is not needed.
# The omega wide table is built from individual_wtp_summary.csv directly.
# ============================================================================

make_top2_info_plot <- function(use_full_sample = FALSE) {
  
  # Build wide omega table (posterior means) from the long WTP summary
  utils <- wtp_indiv %>%
    select(RespondentId = sys_respnum, feature_name, omega_mean) %>%
    pivot_wider(names_from = feature_name, values_from = omega_mean) %>%
    as.data.frame()
  
  if (!"RespondentId" %in% names(utils)) {
    stop("Failed to construct wide omega table with RespondentId.")
  }
  if ("eta_price" %in% names(utils) || "price_linear" %in% names(utils)) {
    stop("Wide omega table unexpectedly contains eta_price/price_linear columns.")
  }
  
  survey_beliefs <- read.csv("../data/Survey/survey_merged_final.csv",
                             stringsAsFactors = FALSE)
  if ("sys_RespNum" %in% names(survey_beliefs)) {
    survey_beliefs <- survey_beliefs %>% rename(RespondentId = sys_RespNum)
  }
  
  if (use_full_sample) {
    valid_respondents <- intersect(
      unique(utils$RespondentId),
      unique(survey_beliefs$RespondentId)
    )
    cat(sprintf("  Full conjoint sample: %d respondents\n", length(valid_respondents)))
  } else {
    meta <- read.csv(
      "../data/final_extension_data/experiment_conditions_pilot_july_2024.csv",
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
  
  missing_utility_cols <- setdiff(unname(belief_to_attr), names(utils))
  if (length(missing_utility_cols) > 0) {
    stop("Missing expected omega columns in the wide table: ",
         paste(missing_utility_cols, collapse = ", "))
  }
  
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
  
  utils_long <- utils %>%
    filter(RespondentId %in% valid_respondents) %>%
    select(RespondentId, all_of(unname(belief_to_attr))) %>%
    pivot_longer(-RespondentId, names_to = "attribute", values_to = "omega_ij")
  
  iv_df <- inner_join(utils_long, beliefs_long,
                      by = c("RespondentId", "attribute")) %>%
    mutate(information_value = p_ij * (1 - p_ij) * abs(omega_ij))
  
  cat(sprintf("  IV computed: %d respondents, %d rows\n",
              n_distinct(iv_df$RespondentId), nrow(iv_df)))
  
  top2 <- iv_df %>%
    group_by(RespondentId) %>%
    slice_max(information_value, n = 2) %>%
    ungroup()
  
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

cat("=== Main Paper (Extension Sample) ===\n\n")

cat("--- Fig 6(a): WTP Violin (extension) -- match panel (a) order ---\n")
make_dollar_plot(
  with_web_dollars %>% filter(sample_group == "Extension"),
  paste0(FIGURES_DIR, "individual_heterogeneity_dollars_with_website_extension.pdf"),
  order_by = "beliefs"
)

cat("\n--- Fig 6(a) ALT: WTP Violin (extension) -- WTP order, lowest at top ---\n")
make_dollar_plot(
  with_web_dollars %>% filter(sample_group == "Extension"),
  paste0(FIGURES_DIR, "individual_heterogeneity_dollars_with_website_extension_2.pdf"),
  order_by = "wtp"
)

cat("\n--- Fig 6(b): Top-2 Information (extension) ---\n")
g_ext <- make_top2_info_plot(use_full_sample = FALSE)
ggsave(paste0(FIGURES_DIR, "top2_privacy_attributes_extension.pdf"),
       g_ext, width = 8, height = 6)
cat("Saved: top2_privacy_attributes_extension.pdf\n")

cat("\n=== Appendix (Full Conjoint Sample) ===\n\n")

cat("--- Fig C.5(a): WTP Violin (full) -- match panel (a) order ---\n")
make_dollar_plot(
  with_web_dollars,
  paste0(FIGURES_DIR, "individual_heterogeneity_dollars_with_website_full.pdf"),
  order_by = "beliefs"
)

cat("\n--- Fig C.5(b): Top-2 Information (full) ---\n")
g_full <- make_top2_info_plot(use_full_sample = TRUE)
ggsave(paste0(FIGURES_DIR, "top2_privacy_attributes_full.pdf"),
       g_full, width = 8, height = 6)
cat("Saved: top2_privacy_attributes_full.pdf\n")

cat("\n=== Appendix C.4: Extension vs Survey Comparison ===\n\n")
make_comparison_plot(
  with_web_dollars,
  paste0(FIGURES_DIR, "individual_heterogeneity_experiment_vs_survey.pdf")
)

cat("\nAll plots complete!\n")