# =============================================================================
# TIME-USAGE TREATMENT EFFECTS (SG)
# =============================================================================
#
# Produces:
#   Main paper:
#     Figure 6 [fig:extensive,
#               "Browsing Behavior Treatment Effects: Extensive Margin"]
#       Fig 6(a) [fig:extensive_a]:
#         output/figures/assortment_concentration[_wt].pdf
#       Fig 6(b) [fig:extensive_b]:
#         output/figures/assortment_privacy[_wt].pdf
#     Extensive-margin scalars cited in the website-choice prose:
#       output/values/assortment_did_values.tex
#       (4 macros: \extensiveHHIPvalue, \extensiveTopSharePvalue,
#        \privacyDifferentialPctSD, \portfolioPrivacyPctSD)
#   Appendix:
#     Figure C.9 [fig:intensive,
#                 "Browsing Behavior Treatment Effects: Intensive Margin"]
#       Fig C.9(a) [fig:intensive_a]:
#         output/figures/preregistered_spec_i_baseline[_wt].pdf
#       Fig C.9(b) [fig:intensive_b]:
#         output/figures/preregistered_triple_interaction_baseline[_wt].pdf
#
#   `_wt` suffix is empty for unweighted and `_{weight_spec}` otherwise. Set
#   WEIGHT_SPEC at the top of the script: "unweighted", "weight_census",
#   "weight_pew", "weight_combined", or "all" to loop over all four. Paper
#   figures are unweighted; the weighted variants are robustness checks (no
#   paper artifact). The savetexvalue macros are written for the unweighted
#   spec only.
#
# Inputs:
#   ../data/processed_data/joined_time_data.csv   (cached; default path)
#   ../data/Survey/individual_level_weights.csv   (only if WEIGHT_SPEC != "unweighted")
#
#   CONSTRUCT_FROM_SCRATCH = TRUE branch additionally reads:
#     ../data/Survey/survey_merged_final.csv
#     ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#     ../data/final_extension_data/privacy_info.csv
#     (and the helper-function inputs in utils/time_usage_helpers.R)
#
# Dependencies:
#   replication_files/utils/values.R              (BAD_USERS, SURVEY_WEBSITES,
#                                                  TREATMENT_DATE_WAVE_*)
#   replication_files/utils/time_usage_helpers.R  (get_time_panel,
#                                                  get_clean_time_data,
#                                                  high_level_aggregate, etc.)
#   replication_files/utils/info_acq_helpers.R
#   replication_files/utils/plot_rules.R
#   replication_files/utils/number_format_helpers.R
#   replication_files/time_use_analysis/analysis_assortment_did.R
#     (provides run_assortment_analysis + run_preregistered_specs)
#   savetexvalue (devtools::install_github("Ori-Shoham/savetexvalue"))
#
# Outputs:
#   output/figures/preregistered_spec_i_baseline[_wt].pdf
#   output/figures/preregistered_triple_interaction_baseline[_wt].pdf
#   output/figures/assortment_concentration[_wt].pdf
#   output/figures/assortment_privacy[_wt].pdf
#   output/values/assortment_did_values.tex
#
# CHANGES this version:
#   - SURVEY FILTER BUG FIXED. The filtered object was assigned to `full_dat`
#     and written to the CSV, but `full_time_dat` -- which every downstream
#     analysis uses -- was never reassigned. The CONSTRUCT_FROM_SCRATCH = TRUE
#     branch therefore analysed data that still contained survey platforms
#     (24.1% of recorded hours), while the FALSE branch read the filtered CSV.
#     The two branches produced different samples.
#   - SURVEY FILTER RULE CHANGED to an exact match against
#     website_aggregated_high_level, matching the rest of the pipeline.
#     Substring matching on the raw domain overcaught ordinary browsing whose
#     domain merely contains a platform name (mturk.notion.site,
#     usertesting.wistia.com, socoandtheocmix.com).
#   - WINSORIZING TURNED OFF. get_balanced_panel is called with its 0.0 / 1.0
#     defaults. The 5/95 trim existed only for the untransformed visit count,
#     whose OLS standard error it shrank ~14x; Poisson is robust to those
#     outliers and reproduces the trimmed answer without modifying data. The
#     trim also computed its cutoffs within post x condition, so the cutoffs
#     themselves carried a difference-in-differences structure.
#   - APPENDIX C TOP-WEBSITES BLOCK REMOVED. It wrote the same
#     top_websites_values.tex as time_use_baseline_scalars.R but with
#     different numbers, because this script applies time_spent > 30 and
#     BAD_USERS and that one does not -- so whichever ran last won. The table
#     is a descriptive statistic and now belongs solely to
#     time_use_baseline_scalars.R.
#   - Browsing time now arrives through get_time_panel() (via
#     get_clean_time_data()), which deduplicates to one row per
#     (participant, website, day) by taking the maximum and applies
#     time_spent > 30 upstream. No change was needed here for that.
#
# Note: Previous version of this driver also produced (now removed as dead
# code, not in paper):
#   - time_spent_log.pdf
#   - baseline_privacy_requested.pdf, baseline_privacy_full.pdf
#   - delta_privacy_full_overall.pdf, delta_privacy_requested.pdf
#   - overlay_privacy_full_density.pdf, overlay_privacy_requested_density.pdf
#   - pre_decile_shift_requested.pdf, pre_decile_shift_full.pdf
#   plus the three run_did_analysis() panels (balanced/full/unbalanced) and
#   plot_privacy_summary() (did_main_results_*.tex, did_triple_diff_*.tex,
#   did_analysis_summary_*.txt, privacy_summary_by_model.png,
#   privacy_summary_across_panels.txt). See analysis_assortment_did.R header
#   for the function-level cleanup notes.
# Removed dead computation:
#   - extension_inactivity <- read.csv(...) (loaded but never used)
#   - Panel constructions balanced_panel_weeks,
#     balanced_panel_weeks_extensive_margin, unbalanced_panel,
#     balanced_panel_post_extensive_margin and the to_factor() calls on them
#     (only balanced_panel_post feeds the kept Fig C.9 call).
#   - personalized_scores, top_site_per_category_overall,
#     top_site_per_category_requested (only fed the removed CF / overlay /
#     pre-decile plots).
#   - All user_weighted / user_site_baseline / cf_results / scores_long /
#     make_pre_decile_plot helper objects (fed only the removed plots).
#   - Second run_preregistered_specs() call on
#     balanced_panel_post_extensive_margin (paper only uses _baseline).
# =============================================================================

library(tidyverse)
library(lubridate)
library(jsonlite)
library(data.table)
library(tidyr)
library(tibble)
library(rlang)
library(fixest)
library(savetexvalue)

# Set working directory to code_github root so all relative paths resolve.
setwd("~/Dropbox/spring2025experiment/code_github")

# Toggle: TRUE re-runs the full data-construction pipeline (~150 lines below);
# FALSE reads the cached `joined_time_data.csv` produced by an earlier run.
CONSTRUCT_FROM_SCRATCH <- TRUE
# Flag for whether to use population averages (FALSE) or individual measures
# (TRUE) when constructing privacy scores. Consumed only inside the
# CONSTRUCT_FROM_SCRATCH branch.
individual_weights <- FALSE

# Output directories
FIGURES_DIR <- "output/figures/"
TABLES_DIR  <- "output/tables/"
VALUES_DIR  <- "output/values/"

## [WEIGHT MODIFICATION] ======================================================
## Set to one of: "unweighted", "weight_census", "weight_pew",
## "weight_combined", "all". "all" loops over all four specs at the bottom
## without re-loading data.
WEIGHT_SPEC   <- "unweighted"
OUTPUT_SUFFIX <- if (WEIGHT_SPEC == "unweighted") "" else paste0("_", WEIGHT_SPEC)
cat("=== Weight specification:", WEIGHT_SPEC, "===\n")
cat("=== Output suffix:",       OUTPUT_SUFFIX, "===\n\n")
## =============================================================================

# Source utility scripts
source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/info_acq_helpers.R")
source("replication_files/utils/number_format_helpers.R")

# =============================================================================
# Survey-weighting helpers (consumed by run_assortment_analysis and
# run_preregistered_specs in analysis_assortment_did.R)
# =============================================================================

#' Join survey weights to a data frame on experiment_id.
#' Returns df unchanged when wt_spec == "unweighted".
join_weights <- function(df, wt_spec, weight_col = "survey_weight") {
  if (wt_spec == "unweighted") return(df)
  
  weights_path <- "../data/Survey/individual_level_weights.csv"
  if (!file.exists(weights_path)) stop("Weight file not found: ", weights_path)
  
  weights_df <- read.csv(weights_path, stringsAsFactors = FALSE)
  if (!wt_spec %in% names(weights_df)) {
    stop("Weight column '", wt_spec, "' not found in weights file. ",
         "Available columns: ", paste(names(weights_df), collapse = ", "))
  }
  
  weights_df <- weights_df %>%
    filter(sample == "extension") %>%
    select(experiment_id, !!weight_col := all_of(wt_spec))
  
  n_before  <- nrow(df)
  df        <- df %>% left_join(weights_df, by = "experiment_id")
  n_matched <- sum(!is.na(df[[weight_col]]))
  
  cat("  Weights joined:", n_matched, "/", n_before, "rows matched (",
      round(100 * n_matched / n_before, 1), "%)\n")
  return(df)
}

#' Run feols with optional survey weights (weights = ~survey_weight when set).
run_weighted_feols <- function(fml, data, cluster_var, wt_spec = WEIGHT_SPEC) {
  if (wt_spec == "unweighted" || !"survey_weight" %in% names(data)) {
    feols(fml, cluster = cluster_var, data = data)
  } else {
    feols(fml, cluster = cluster_var, data = data, weights = ~survey_weight)
  }
}

# =============================================================================
# get_balanced_panel(): build a balanced panel of (user, website) tuples
# observed in `values_to_include` and impute zeros across `all_values`.
# values_to_include = c(-2, -1) -> sites visited in both baseline weeks;
# values_to_include = c()       -> no baseline restriction (full panel).
#
# lower_pct / upper_pct winsorize the aggregated measures. They default to
# 0.0 / 1.0, i.e. OFF, and the paper calls now leave them at the default --
# see the CHANGES note in the header.
# =============================================================================

get_balanced_panel <- function(dat,
                               index_var         = "weeks_since_intervention",
                               values_to_include = c(-2, -1),
                               all_values        = c(-2, -1, 0, 1, 2, 3),
                               lower_pct = 0.0,
                               upper_pct = 1.0) {
  idx <- rlang::sym(index_var)
  
  # Aggregate by chosen index variable.
  dat <- dat %>%
    group_by(!!idx, experiment_id, experiment_condition,
             website_aggregated_high_level) %>%
    mutate(total_time_spent  = sum(time_spent),
           total_visit_count = sum(visit_count)) %>%
    slice(1) %>%
    ungroup()
  
  # Winsorize (no-op at the 0.0 / 1.0 defaults).
  dat <- dat %>%
    group_by(!!idx, experiment_condition) %>%
    mutate(
      total_time_spent = pmin(pmax(total_time_spent,
                                   quantile(total_time_spent, lower_pct, na.rm = TRUE)),
                              quantile(total_time_spent, upper_pct, na.rm = TRUE)),
      total_visit_count = pmin(pmax(total_visit_count,
                                    quantile(total_visit_count, lower_pct, na.rm = TRUE)),
                               quantile(total_visit_count, upper_pct, na.rm = TRUE))
    ) %>%
    ungroup()
  
  # Websites that satisfy the inclusion rule.
  if (length(values_to_include) == 0) {
    first_period_websites <- dat %>%
      select(experiment_id, website_aggregated_high_level) %>%
      distinct()
  } else {
    first_period_websites <- dat %>%
      filter((!!idx) %in% values_to_include & total_time_spent > 0) %>%
      group_by(experiment_id, website_aggregated_high_level) %>%
      filter(n() == length(values_to_include)) %>%
      ungroup() %>%
      select(experiment_id, website_aggregated_high_level) %>%
      distinct()
  }
  
  experiment_data <- dat %>%
    select(intersect(
      c("experiment_id", "block_by_wave", "experiment_condition",
        "q1_html_key", "q2_html_key", "q1_belief", "q2_belief"),
      names(dat)
    )) %>%
    distinct()
  
  website_privacy_data <- dat %>%
    select(intersect(
      c("experiment_id", "website_aggregated_high_level",
        "privacy_for_requested_attribute", "privacy_full_beta_p", "privacy_exist",
        "category_level_1", "category_level_2", "category_level_3",
        "q1_realized_privacy", "q2_realized_privacy"),
      names(dat)
    )) %>%
    distinct() %>%
    # Collapse to one row per (user, site). A few user-site pairs (the "x" /
    # twitter alias) carry both an NA and a non-NA privacy_for_requested_attribute
    # row, which makes the downstream left_join many-to-many. Keep the non-NA
    # privacy row so the join stays 1:1.
    group_by(experiment_id, website_aggregated_high_level) %>%
    arrange(is.na(privacy_for_requested_attribute), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
  
  balanced_panel <- first_period_websites %>%
    expand_grid(!!idx := all_values) %>%
    left_join(dat, by = c("experiment_id", "website_aggregated_high_level", index_var)) %>%
    mutate(
      total_visit_count = ifelse(is.na(total_visit_count), 0, total_visit_count),
      total_time_spent  = ifelse(is.na(total_time_spent),  0, total_time_spent)
    ) %>%
    left_join(experiment_data,      by = c("experiment_id")) %>%
    left_join(website_privacy_data, by = c("experiment_id", "website_aggregated_high_level"))
  
  safe_coalesce <- function(df, col_name) {
    col_x <- paste0(col_name, ".x")
    col_y <- paste0(col_name, ".y")
    if (col_x %in% names(df) && col_y %in% names(df)) coalesce(df[[col_x]], df[[col_y]])
    else if (col_x    %in% names(df))                 df[[col_x]]
    else if (col_y    %in% names(df))                 df[[col_y]]
    else if (col_name %in% names(df))                 df[[col_name]]
    else NA
  }
  
  balanced_panel <- balanced_panel %>%
    mutate(
      block_by_wave                   = safe_coalesce(pick(everything()), "block_by_wave"),
      experiment_condition            = safe_coalesce(pick(everything()), "experiment_condition"),
      q1_html_key                     = safe_coalesce(pick(everything()), "q1_html_key"),
      q2_html_key                     = safe_coalesce(pick(everything()), "q2_html_key"),
      q1_belief                       = safe_coalesce(pick(everything()), "q1_belief"),
      q2_belief                       = safe_coalesce(pick(everything()), "q2_belief"),
      q1_realized_privacy             = safe_coalesce(pick(everything()), "q1_realized_privacy"),
      q2_realized_privacy             = safe_coalesce(pick(everything()), "q2_realized_privacy"),
      privacy_for_requested_attribute = safe_coalesce(pick(everything()), "privacy_for_requested_attribute"),
      privacy_full_beta_p             = safe_coalesce(pick(everything()), "privacy_full_beta_p"),
      privacy_exist                   = safe_coalesce(pick(everything()), "privacy_exist"),
      category_level_1                = safe_coalesce(pick(everything()), "category_level_1")
    )
  
  if ("category_level_2.x" %in% names(balanced_panel) ||
      "category_level_2.y" %in% names(balanced_panel) ||
      "category_level_2"   %in% names(balanced_panel)) {
    balanced_panel <- balanced_panel %>%
      mutate(category_level_2 = safe_coalesce(pick(everything()), "category_level_2"))
  }
  if ("category_level_3.x" %in% names(balanced_panel) ||
      "category_level_3.y" %in% names(balanced_panel) ||
      "category_level_3"   %in% names(balanced_panel)) {
    balanced_panel <- balanced_panel %>%
      mutate(category_level_3 = safe_coalesce(pick(everything()), "category_level_3"))
  }
  
  balanced_panel <- balanced_panel %>%
    group_by(experiment_id, website_aggregated_high_level, !!idx) %>%
    slice(1) %>%
    ungroup()
  
  select_cols <- c("experiment_id", "website_aggregated_high_level", index_var,
                   "total_visit_count", "total_time_spent", "block_by_wave",
                   "experiment_condition", "q1_realized_privacy", "q2_realized_privacy",
                   "privacy_for_requested_attribute", "privacy_exist", "privacy_full_beta_p",
                   "q1_html_key", "q2_html_key", "q1_belief", "q2_belief",
                   "category_level_1", "category_level_2", "category_level_3")
  select_cols <- intersect(select_cols, names(balanced_panel))
  balanced_panel <- balanced_panel %>% select(all_of(select_cols))
  
  individual_website <- balanced_panel %>%
    filter(privacy_exist & !is.na(privacy_for_requested_attribute)) %>%
    group_by(experiment_id, website_aggregated_high_level) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(experiment_id) %>%
    mutate(med_privacy = median(privacy_for_requested_attribute, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(indiv_high_privacy = privacy_for_requested_attribute > med_privacy) %>%
    select(experiment_id, indiv_high_privacy, website_aggregated_high_level)
  
  med <- median(balanced_panel$privacy_for_requested_attribute, na.rm = TRUE)
  balanced_panel <- balanced_panel %>%
    left_join(individual_website,
              by = c("experiment_id", "website_aggregated_high_level")) %>%
    mutate(has_visits                  = total_visit_count > 0,
           privacy_discretized_median  = privacy_for_requested_attribute > med)
  
  return(balanced_panel)
}

# =============================================================================
# DATA CONSTRUCTION (CONSTRUCT_FROM_SCRATCH = TRUE) OR LOAD CACHED CSV
# =============================================================================

if (CONSTRUCT_FROM_SCRATCH) {
  survey_dat <- read.csv("../data/Survey/survey_merged_final.csv",
                         stringsAsFactors = FALSE)
  
  survey_dat <- survey_dat %>%
    filter(
      Age != 5,
      (RaceSimple_1 == 1 | RaceSimple_2 == 1 | RaceSimple_3 == 1 |
         RaceSimple_4 == 1 | RaceSimple_5 == 1 | RaceSimple_6 == 1),
      !is.na(favoritewebsite)         & trimws(favoritewebsite) != "",
      !is.na(conjcat_conjCategory)    & trimws(conjcat_conjCategory) != ""
    )
  
  time_data <- get_clean_time_data()
  
  personalized_info <- get_personalized_info_only() %>%
    select(email, experiment_id, q1_feature, q1_field, q2_feature, q2_field) %>%
    mutate(q1_html_key = paste(q1_feature, q1_field, sep = "-"),
           q2_html_key = paste(q2_feature, q2_field, sep = "-")) %>%
    select(email, experiment_id, q1_html_key, q2_html_key)
  
  beliefs <- get_privacy_beliefs() %>%
    rename(email = emailid) %>%
    mutate(email = tolower(email))
  
  beliefs_long <- beliefs %>%
    pivot_longer(-email, names_to = "beliefs_key_full", values_to = "belief_value") %>%
    mutate(html_key = tolower(sub("^beliefs_", "", beliefs_key_full))) %>%
    select(email, html_key, belief_value)
  
  personalized_info <- personalized_info %>%
    mutate(email       = tolower(email),
           q1_html_key = tolower(q1_html_key),
           q2_html_key = tolower(q2_html_key))
  
  personalized_beliefs <- personalized_info %>%
    left_join(beliefs_long, by = c("email", "q1_html_key" = "html_key")) %>%
    rename(q1_belief = belief_value) %>%
    left_join(beliefs_long, by = c("email", "q2_html_key" = "html_key")) %>%
    rename(q2_belief = belief_value) %>%
    select(email, experiment_id, q1_html_key, q2_html_key, q1_belief, q2_belief)
  
  meta_data <- read.csv("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv") %>%
    mutate(wave_id       = ifelse(wave_id == 3, 2, wave_id),
           block_by_wave = paste(wave_id, block_idx, sep = "_"))
  
  experiment_users <- meta_data %>%
    filter(experiment_condition != "" & !(experiment_id %in% BAD_USERS))
  
  personalized_beliefs <- personalized_beliefs %>%
    left_join(experiment_users %>% select(experiment_id, block_by_wave),
              by = "experiment_id")
  
  time_data_with_beliefs <- time_data %>%
    left_join(personalized_beliefs, by = "experiment_id")
  time_data_with_beliefs <- high_level_aggregate(time_data_with_beliefs)
  
  domain_classfication <- get_domain_classification()
  time_data_2_domain_class_high_level <- time_data_with_beliefs %>%
    left_join(domain_classfication %>%
                select(name_aggregated_high_level, categories, category_length,
                       category_level_1),
              by = c("website_aggregated_high_level" = "name_aggregated_high_level"))
  
  privacy_info <- get_privacy_info_wide()
  time_data_2_domain_class_privacy <- map_privacy_data(time_data_2_domain_class_high_level,
                                                       privacy_info)
  
  if (individual_weights) {
    privacy_weights <- get_privacy_attribute_weights_by_individual()
  } else {
    privacy_weights <- get_privacy_attribute_weights_population_by_user()
  }
  
  time_data_2_domain_class_privacy_weights <- time_data_2_domain_class_privacy %>%
    inner_join(privacy_weights, by = "experiment_id")
  
  privacy_cols <- grep("^p_",    names(time_data_2_domain_class_privacy_weights), value = TRUE)
  beta_cols    <- grep("^beta_", names(time_data_2_domain_class_privacy_weights), value = TRUE)
  privacy_map  <- setNames(privacy_cols, sub("^p_",    "", privacy_cols))
  beta_map     <- setNames(beta_cols,    sub("^beta_", "", beta_cols))
  
  get_realized_from_map <- function(df, key_vec, map_vec) {
    vapply(seq_along(key_vec), function(i) {
      key <- key_vec[i]
      if (!is.na(key) && nzchar(key) && key %in% names(map_vec)) {
        df[[map_vec[[key]]]][i]
      } else NA
    }, FUN.VALUE = NA_real_)
  }
  
  time_data_2_domain_class_privacy_weights$q1_realized_privacy <-
    get_realized_from_map(time_data_2_domain_class_privacy_weights,
                          time_data_2_domain_class_privacy_weights$q1_html_key,
                          privacy_map)
  time_data_2_domain_class_privacy_weights$q2_realized_privacy <-
    get_realized_from_map(time_data_2_domain_class_privacy_weights,
                          time_data_2_domain_class_privacy_weights$q2_html_key,
                          privacy_map)
  time_data_2_domain_class_privacy_weights$q1_preference_weight <-
    get_realized_from_map(time_data_2_domain_class_privacy_weights,
                          time_data_2_domain_class_privacy_weights$q1_html_key,
                          beta_map)
  time_data_2_domain_class_privacy_weights$q2_preference_weight <-
    get_realized_from_map(time_data_2_domain_class_privacy_weights,
                          time_data_2_domain_class_privacy_weights$q2_html_key,
                          beta_map)
  
  full_time_dat <- time_data_2_domain_class_privacy_weights %>%
    mutate(
      privacy_for_requested_attribute = case_when(
        q1_preference_weight > 0 & q2_preference_weight > 0 &
          (q1_preference_weight + q2_preference_weight) != 0 ~
          (q1_preference_weight / (q1_preference_weight +  q2_preference_weight)) *  q1_realized_privacy +
          (q2_preference_weight / (q1_preference_weight +  q2_preference_weight)) *  q2_realized_privacy,
        q1_preference_weight > 0 & q2_preference_weight < 0 &
          (q1_preference_weight + -q2_preference_weight) != 0 ~
          ( q1_preference_weight / ( q1_preference_weight + -q2_preference_weight)) *  q1_realized_privacy +
          (-q2_preference_weight / ( q1_preference_weight + -q2_preference_weight)) * (1 - q2_realized_privacy),
        q1_preference_weight < 0 & q2_preference_weight > 0 &
          (-q1_preference_weight + q2_preference_weight) != 0 ~
          (-q1_preference_weight / (-q1_preference_weight +  q2_preference_weight)) * (1 - q1_realized_privacy) +
          ( q2_preference_weight / (-q1_preference_weight +  q2_preference_weight)) *  q2_realized_privacy,
        q1_preference_weight < 0 & q2_preference_weight < 0 &
          (-q1_preference_weight + -q2_preference_weight) != 0 ~
          (-q1_preference_weight / (-q1_preference_weight + -q2_preference_weight)) * (1 - q1_realized_privacy) +
          (-q2_preference_weight / (-q1_preference_weight + -q2_preference_weight)) * (1 - q2_realized_privacy),
        TRUE ~ NA_real_
      )
    )
  
  full_time_dat <- compute_privacy_scores(full_time_dat)
  full_time_dat <- compute_exposed_privacy_scores(full_time_dat)
  
  # [CHANGED] Survey-platform filter: exact match on the aggregated domain, and
  # the result is assigned back to full_time_dat. Previously the filtered object
  # went only to `full_dat` and the CSV, leaving the in-memory analysis
  # unfiltered -- so this branch and the cached-CSV branch analysed different
  # samples.
  full_dat <- full_time_dat %>%
    filter(!(tolower(website_aggregated_high_level) %in% SURVEY_WEBSITES))
  
  cat(sprintf("Survey-platform filter: %s -> %s rows (%s removed)\n",
              format(nrow(full_time_dat), big.mark = ","),
              format(nrow(full_dat), big.mark = ","),
              format(nrow(full_time_dat) - nrow(full_dat), big.mark = ",")))
  
  write.csv(full_dat, "../data/processed_data/joined_time_data.csv", row.names = FALSE)
  full_time_dat <- full_dat
} else {
  full_time_dat <- read.csv("../data/processed_data/joined_time_data.csv")
}

# =============================================================================
# Post-load cleaning
# =============================================================================
# time_spent > 30 is now applied upstream in get_time_panel(), so for data
# built by the current pipeline this filter is a no-op. It is kept as a guard
# in case the cached CSV predates that change.

full_time_dat <- full_time_dat %>%
  filter(!(experiment_id %in% BAD_USERS)) %>%
  filter(time_spent > 30)

cat(sprintf("Analysis sample: %s rows | %s participants | %s sites\n",
            format(nrow(full_time_dat), big.mark = ","),
            format(n_distinct(full_time_dat$experiment_id), big.mark = ","),
            format(n_distinct(full_time_dat$website_aggregated_high_level),
                   big.mark = ",")))

# =============================================================================
# Build the one panel that feeds Fig 6 + Fig C.9:
#   balanced_panel_post -> baseline-restricted panel, indexed by post.
# =============================================================================
# [CHANGED] winsorizing left at the 0.0 / 1.0 defaults -- see the header.

balanced_panel_post <- get_balanced_panel(
  full_time_dat,
  index_var         = "post",
  values_to_include = c(0),
  all_values        = c(0, 1)
)

to_factor <- function(df,
                      weeks_levels = c(-2, -1, 0, 1, 2, 3),
                      post_levels  = c(0, 1)) {
  if ("experiment_id" %in% names(df))                df$experiment_id <- as.factor(df$experiment_id)
  if ("block_by_wave" %in% names(df))                df$block_by_wave <- as.factor(df$block_by_wave)
  if ("website_aggregated_high_level" %in% names(df)) {
    df$website_aggregated_high_level <- as.factor(df$website_aggregated_high_level)
  }
  if ("weeks_since_intervention" %in% names(df)) {
    df$weeks_since_intervention <- factor(df$weeks_since_intervention, levels = weeks_levels)
  }
  if ("post" %in% names(df)) {
    df$post <- factor(df$post, levels = post_levels)
  }
  df
}

balanced_panel_post <- to_factor(balanced_panel_post)

# =============================================================================
# Load plot rules + assortment/preregistered analysis helpers
# =============================================================================

source("replication_files/utils/plot_rules.R")
source("replication_files/time_use_analysis/analysis_assortment_did.R")

# =============================================================================
# [WEIGHT MODIFICATION] Run paper Fig 6 + Fig C.9 across all requested specs.
# When WEIGHT_SPEC == "all", loops over all four; otherwise runs the single
# spec. Data loading above runs ONCE; only the regression/plot calls loop.
# =============================================================================

.ALL_SPECS <- c("unweighted", "weight_census", "weight_pew", "weight_combined")
.specs_to_run <- if (WEIGHT_SPEC == "all") .ALL_SPECS else WEIGHT_SPEC

for (.ws in .specs_to_run) {
  
  cat("\n\n############################################################\n")
  cat("## Paper regressions with weight spec:", .ws, "\n")
  cat("############################################################\n\n")
  
  .wt_suffix <- if (.ws == "unweighted") "" else paste0("_", .ws)
  
  # Fig 6: extensive-margin assortment analysis.
  extensive <- run_assortment_analysis(
    full_time_dat,
    output_dir  = TABLES_DIR,
    figures_dir = FIGURES_DIR,
    wt_spec     = .ws,
    wt_suffix   = .wt_suffix
  )
  
  # -------------------------------------------------------------------------
  # Scalar macros: extensive-margin numbers cited in the website-choice prose.
  # Unweighted spec only; all values pulled from the assortment models / data
  # above, never hard-coded.
  #   \extensiveHHIPvalue        info-vs-control p-value, HHI change
  #   \extensiveTopSharePvalue   info-vs-control p-value, top-1 share change
  #                              (referred to as "top-2 share" in the prose)
  #   \privacyDifferentialPctSD  info coef / SD, privacy differential
  #   \portfolioPrivacyPctSD     info coef / SD, portfolio privacy change
  # -------------------------------------------------------------------------
  if (.ws == "unweighted") {
    p_hhi  <- extensive$model_hhi$coeftable["experiment_conditioninfo",  "Pr(>|t|)"]
    p_top1 <- extensive$model_top1$coeftable["experiment_conditioninfo", "Pr(>|t|)"]
    
    sd_diff <- sd(extensive$user_entry_exit$privacy_differential, na.rm = TRUE)
    sd_pc   <- sd(extensive$concentration_wide$privacy_change,    na.rm = TRUE)
    b_diff  <- extensive$model_differential$coeftable["experiment_conditioninfo",   "Estimate"]
    b_pc    <- extensive$model_privacy_change$coeftable["experiment_conditioninfo", "Estimate"]
    
    assortment_values_file <- "assortment_did_values"
    assortment_values_full <- paste0(VALUES_DIR, assortment_values_file, ".tex")
    if (file.exists(assortment_values_full)) file.remove(assortment_values_full)
    
    save_tex_value(values = format_pvalue(p_hhi),
                   names = "extensiveHHIPvalue",
                   file_name = assortment_values_file, path = VALUES_DIR)
    save_tex_value(values = format_pvalue(p_top1),
                   names = "extensiveTopSharePvalue",
                   file_name = assortment_values_file, path = VALUES_DIR)
    save_tex_value(values = format_pct(100 * b_diff / sd_diff),
                   names = "privacyDifferentialPctSD",
                   file_name = assortment_values_file, path = VALUES_DIR)
    save_tex_value(values = format_pct(100 * b_pc / sd_pc),
                   names = "portfolioPrivacyPctSD",
                   file_name = assortment_values_file, path = VALUES_DIR)
    
    cat(sprintf("\nSaved 4 macros to %s%s.tex\n", VALUES_DIR, assortment_values_file))
  }
  
  # Fig C.9: pre-registered intensive-margin specs on the baseline panel.
  preregistered_balanced <- run_preregistered_specs(
    balanced_panel = balanced_panel_post,
    full_time_dat  = full_time_dat,
    panel_label    = "Baseline",
    privacy_col    = "privacy_for_requested_attribute",
    output_dir     = TABLES_DIR,
    figures_dir    = FIGURES_DIR,
    wt_spec        = .ws,
    wt_suffix      = .wt_suffix
  )
  
}

cat("\nAll weight specifications completed.\n")