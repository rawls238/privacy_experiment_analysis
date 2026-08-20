# =============================================================================
# ASSORTMENT DID HELPERS (sourced by time_usage_treatment_effects_SG.R)
# =============================================================================
#
# Produces (via the two exported functions below; output filenames depend on
# panel_label and wt_suffix passed by the calling driver):
#
#   run_assortment_analysis() -> Section 7 Fig 6 [fig:extensive,
#     "Browsing Behavior Treatment Effects: Extensive Margin"]:
#       Fig 6(a) [fig:extensive_a]: <figures_dir>/assortment_concentration[_wt].pdf
#       Fig 6(b) [fig:extensive_b]: <figures_dir>/assortment_privacy[_wt].pdf
#
#   run_preregistered_specs() -> Appendix Fig C.9 [fig:intensive,
#     "Browsing Behavior Treatment Effects: Intensive Margin"] when called
#     with panel_label = "Baseline":
#       Fig C.9(a): <figures_dir>/preregistered_spec_i_baseline[_wt].pdf
#       Fig C.9(b): <figures_dir>/preregistered_triple_interaction_baseline[_wt].pdf
#
# This file does not set its own setwd / source utils. The driver
# (time_usage_treatment_effects_SG.R) is expected to be running with
# code_github/ as the working directory and to have already sourced
# replication_files/utils/plot_rules.R (ERRORBAR_WIDTH, LINE_WIDTH,
# POINT_SIZE, DODGE_WIDTH_2, scale_color_treatment, theme_privacy_experiment,
# geom_hline_zero) and defined the weight helpers join_weights() /
# run_weighted_feols() / run_weighted_fepois().
#
# Weight params: both functions retain `wt_spec` / `wt_suffix` parameters so
# the driver can run weighted variants for robustness. Paper figures are
# unweighted; weighted PDFs are produced only if the driver calls these
# functions with a non-"unweighted" wt_spec.
#
# Note: Previous version of this file also defined (now removed as dead code,
# not in paper):
#   - run_did_analysis(): produced did_main_results_*.tex, did_triple_diff_*.tex,
#     did_analysis_summary_*.txt (3 panels x 3 outputs). Paper Fig 9 uses
#     run_preregistered_specs() instead.
#   - plot_privacy_summary(): produced privacy_summary_by_model.png and a
#     companion .txt. Depended on run_did_analysis() output; README lists as
#     exploratory.
#   - run_robust_did_analysis(): produced did_two_part_*.tex, did_robust_*.tex,
#     did_robust_summary_*.txt. Driver never called this function; dead.
# Removed dead outputs inside the two retained functions:
#   - run_assortment_analysis():
#       assortment_treatment_effects[_wt].tex
#       assortment_entry_exit_privacy[_wt].png
#       assortment_differential_distribution[_wt].png
#       assortment_differential_bar[_wt].png
#       assortment_concentration_changes[_wt].png
#     and the intermediate summaries / ggplot objects they fed
#     (entry_exit_summary, differential_summary, differential_with_se,
#     concentration_long, concentration_summary_long, p_entry_exit,
#     p_differential_dist, p_differential_bar, p_concentration).
#   - run_preregistered_specs():
#       preregistered_spec_i[_panel][_wt].tex
#       preregistered_spec_ii[_panel][_wt].tex
#       preregistered_spec_iii[_panel].tex
#       preregistered_spec_iii[_panel].png
#       preregistered_specs_summary[_panel][_wt].txt
#     Spec (iii) block (HHI panel construction + 3 market-competition
#     regressions + companion coef plot) removed entirely; README lists as
#     "pre-registered spec not used".
# =============================================================================


# =============================================================================
# ANALYSIS 2: WEBSITE ASSORTMENT CHANGE ANALYSIS -> Fig 6 (Section 7)
# =============================================================================
# Treatment effects on portfolio composition: entry/exit privacy
# differentials, concentration (HHI / top-1 share), and time-weighted privacy
# change. Output is two coefficient plots (Fig 6a + 6b).

run_assortment_analysis <- function(full_dat,
                                    output_dir = TABLES_DIR,
                                    figures_dir = FIGURES_DIR,
                                    wt_spec = "unweighted",
                                    wt_suffix = "") {
  
  cat("\n--- ANALYSIS 2: WEBSITE ASSORTMENT CHANGES ---\n")
  cat("Weight specification:", wt_spec, "\n\n")
  
  # -------------------------------------------------------------------------
  # 2.1: Site-level time shares per user-category-period
  # -------------------------------------------------------------------------
  
  site_shares <- full_dat %>%
    filter(!is.na(privacy_for_requested_attribute) &
             !is.na(category_level_1) & category_level_1 != "") %>%
    group_by(experiment_id, experiment_condition, category_level_1, post,
             website_aggregated_high_level) %>%
    summarise(
      site_time    = sum(time_spent, na.rm = TRUE),
      site_privacy = first(privacy_for_requested_attribute),
      .groups = "drop"
    ) %>%
    group_by(experiment_id, experiment_condition, category_level_1, post) %>%
    mutate(
      time_share = site_time / sum(site_time),
      total_time = sum(site_time)
    ) %>%
    ungroup()
  
  cat("Site shares calculated:", nrow(site_shares), "observations\n")
  
  # -------------------------------------------------------------------------
  # 2.2: Entry/Exit Analysis -> privacy_differential regression (Fig 6b)
  # -------------------------------------------------------------------------
  
  site_privacy_pre <- site_shares %>%
    filter(post == FALSE) %>%
    select(experiment_id, category_level_1, website_aggregated_high_level,
           site_privacy_pre = site_privacy, time_share_pre = time_share)
  
  site_privacy_post <- site_shares %>%
    filter(post == TRUE) %>%
    select(experiment_id, category_level_1, website_aggregated_high_level,
           site_privacy_post = site_privacy, time_share_post = time_share)
  
  entry_exit <- site_privacy_pre %>%
    full_join(site_privacy_post,
              by = c("experiment_id", "category_level_1",
                     "website_aggregated_high_level")) %>%
    mutate(
      site_status = case_when(
        is.na(time_share_pre)  & !is.na(time_share_post) ~ "new",
        !is.na(time_share_pre) &  is.na(time_share_post) ~ "dropped",
        TRUE                                             ~ "retained"
      ),
      privacy_score = coalesce(site_privacy_post, site_privacy_pre)
    ) %>%
    left_join(
      full_dat %>% select(experiment_id, experiment_condition) %>% distinct(),
      by = "experiment_id"
    )
  
  user_entry_exit <- entry_exit %>%
    filter(site_status %in% c("new", "dropped")) %>%
    group_by(experiment_id, experiment_condition, category_level_1, site_status) %>%
    summarise(
      avg_privacy = mean(privacy_score, na.rm = TRUE),
      n_sites     = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from  = site_status,
      values_from = c(avg_privacy, n_sites),
      names_sep   = "_"
    ) %>%
    mutate(privacy_differential = avg_privacy_new - avg_privacy_dropped) %>%
    filter(!is.na(privacy_differential))
  
  user_entry_exit$experiment_condition <- factor(
    user_entry_exit$experiment_condition,
    levels = c("control", "info", "saliency")
  )
  user_entry_exit$category_level_1 <- as.factor(user_entry_exit$category_level_1)
  
  user_entry_exit <- join_weights(user_entry_exit, wt_spec)
  
  model_differential <- run_weighted_feols(
    privacy_differential ~ experiment_condition | category_level_1,
    data        = user_entry_exit,
    cluster_var = ~experiment_id,
    wt_spec     = wt_spec
  )
  
  cat("\nPrivacy Differential Regression (Category FE, clustered SE):\n")
  print(summary(model_differential))
  
  # -------------------------------------------------------------------------
  # 2.3: Concentration Changes -> HHI / top1 / privacy_change regressions
  # -------------------------------------------------------------------------
  
  concentration_metrics <- site_shares %>%
    group_by(experiment_id, experiment_condition, category_level_1, post) %>%
    summarise(
      n_sites     = n_distinct(website_aggregated_high_level),
      hhi         = sum(time_share^2),
      top1_share  = max(time_share),
      top3_share  = sum(sort(time_share, decreasing = TRUE)[1:min(3, n())]),
      avg_privacy = sum(time_share * site_privacy, na.rm = TRUE),
      .groups = "drop"
    )
  
  concentration_wide <- concentration_metrics %>%
    pivot_wider(
      id_cols     = c(experiment_id, experiment_condition, category_level_1),
      names_from  = post,
      values_from = c(n_sites, hhi, top1_share, top3_share, avg_privacy),
      names_sep   = "_"
    ) %>%
    mutate(
      hhi_change     = hhi_TRUE        - hhi_FALSE,
      top1_change    = top1_share_TRUE - top1_share_FALSE,
      top3_change    = top3_share_TRUE - top3_share_FALSE,
      n_sites_change = n_sites_TRUE    - n_sites_FALSE,
      privacy_change = avg_privacy_TRUE - avg_privacy_FALSE
    ) %>%
    filter(!is.na(hhi_change))
  
  concentration_wide$experiment_condition <- factor(
    concentration_wide$experiment_condition,
    levels = c("control", "info", "saliency")
  )
  concentration_wide$category_level_1 <- as.factor(concentration_wide$category_level_1)
  
  concentration_wide <- join_weights(concentration_wide, wt_spec)
  
  model_hhi <- run_weighted_feols(
    hhi_change ~ experiment_condition | category_level_1,
    data = concentration_wide, cluster_var = ~experiment_id, wt_spec = wt_spec
  )
  model_top1 <- run_weighted_feols(
    top1_change ~ experiment_condition | category_level_1,
    data = concentration_wide, cluster_var = ~experiment_id, wt_spec = wt_spec
  )
  model_privacy_change <- run_weighted_feols(
    privacy_change ~ experiment_condition | category_level_1,
    data = concentration_wide, cluster_var = ~experiment_id, wt_spec = wt_spec
  )
  
  cat("\nUser HHI Change Regression:\n");    print(summary(model_hhi))
  cat("\nTop Site Share Change Regression:\n");print(summary(model_top1))
  cat("\nPrivacy Change Regression:\n");      print(summary(model_privacy_change))
  
  # -------------------------------------------------------------------------
  # 2.4: Fig 6(a) - Concentration coefficient plot (HHI + Top Site Share)
  # -------------------------------------------------------------------------
  
  concentration_coefs <- bind_rows(
    broom::tidy(model_hhi)  %>% filter(grepl("experiment_condition", term)) %>%
      mutate(outcome = "Portfolio HHI"),
    broom::tidy(model_top1) %>% filter(grepl("experiment_condition", term)) %>%
      mutate(outcome = "Top Site Share")
  ) %>%
    mutate(
      Condition = case_when(
        grepl("info",     term) ~ "Information",
        grepl("saliency", term) ~ "Saliency"
      ),
      Condition = factor(Condition, levels = c("Saliency", "Information")),
      outcome   = factor(outcome,   levels = c("Portfolio HHI", "Top Site Share"))
    )
  
  p_concentration_coefs <- ggplot(concentration_coefs,
                                  aes(x = outcome, y = estimate, color = Condition)) +
    geom_hline_zero(linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * std.error,
          ymax = estimate + 1.96 * std.error),
      width     = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH,
      position  = position_dodge(width = DODGE_WIDTH_2)
    ) +
    geom_point(size = POINT_SIZE, position = position_dodge(width = DODGE_WIDTH_2)) +
    scale_color_treatment() +
    theme_privacy_experiment() +
    labs(x = "", y = "Coefficient", color = NULL)
  
  ggsave(paste0(figures_dir, "assortment_concentration", wt_suffix, ".pdf"),
         p_concentration_coefs, width = 6, height = 5, device = cairo_pdf)
  cat("Saved: assortment_concentration", wt_suffix, ".pdf\n", sep = "")
  
  # -------------------------------------------------------------------------
  # 2.5: Fig 6(b) - Privacy coefficient plot (Differential + Score)
  # -------------------------------------------------------------------------
  
  privacy_coefs <- bind_rows(
    broom::tidy(model_differential)   %>% filter(grepl("experiment_condition", term)) %>%
      mutate(outcome = "Privacy Differential\n(New - Dropped)"),
    broom::tidy(model_privacy_change) %>% filter(grepl("experiment_condition", term)) %>%
      mutate(outcome = "Privacy Score")
  ) %>%
    mutate(
      Condition = case_when(
        grepl("info",     term) ~ "Information",
        grepl("saliency", term) ~ "Saliency"
      ),
      Condition = factor(Condition, levels = c("Saliency", "Information")),
      outcome   = factor(outcome,
                         levels = c("Privacy Differential\n(New - Dropped)",
                                    "Privacy Score"))
    )
  
  p_privacy_coefs <- ggplot(privacy_coefs,
                            aes(x = outcome, y = estimate, color = Condition)) +
    geom_hline_zero(linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * std.error,
          ymax = estimate + 1.96 * std.error),
      width     = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH,
      position  = position_dodge(width = DODGE_WIDTH_2)
    ) +
    geom_point(size = POINT_SIZE, position = position_dodge(width = DODGE_WIDTH_2)) +
    scale_color_treatment() +
    theme_privacy_experiment() +
    labs(x = "", y = "Coefficient", color = NULL)
  
  ggsave(paste0(figures_dir, "assortment_privacy", wt_suffix, ".pdf"),
         p_privacy_coefs, width = 6, height = 5, device = cairo_pdf)
  cat("Saved: assortment_privacy", wt_suffix, ".pdf\n", sep = "")
  
  return(list(
    entry_exit             = entry_exit,
    user_entry_exit        = user_entry_exit,
    concentration_wide     = concentration_wide,
    model_differential     = model_differential,
    model_hhi              = model_hhi,
    model_top1             = model_top1,
    model_privacy_change   = model_privacy_change,
    plots = list(
      concentration_coefs = p_concentration_coefs,
      privacy_coefs       = p_privacy_coefs
    )
  ))
}


# =============================================================================
# ANALYSIS 4.2: PRE-REGISTERED WEBSITE CHOICE SPECIFICATIONS -> Appendix Fig C.9
# =============================================================================
# Pre-registered specifications (i) aggregate ATE on raw time (PPML) / n visits
# and (ii) triple-difference with privacy heterogeneity. Both as coefficient
# plots (Fig C.9a, C.9b) when panel_label = "Baseline".

run_preregistered_specs <- function(balanced_panel,
                                    full_time_dat,
                                    panel_label = "balanced panel",
                                    privacy_col = "privacy_for_requested_attribute",
                                    output_dir  = TABLES_DIR,
                                    figures_dir = FIGURES_DIR,
                                    wt_spec     = "unweighted",
                                    wt_suffix   = "") {
  
  cat("\n============================================================================\n")
  cat("ANALYSIS 4.2: PRE-REGISTERED WEBSITE CHOICE SPECIFICATIONS\n")
  cat("Panel:", panel_label, "\n")
  cat("Weight specification:", wt_spec, "\n")
  cat("============================================================================\n\n")
  
  panel_data <- balanced_panel %>%
    filter(!is.na(.data[[privacy_col]])) %>%
    mutate(
      privacy_score                 = .data[[privacy_col]],
      experiment_condition          = factor(experiment_condition,
                                             levels = c("control", "info", "saliency")),
      experiment_id                 = as.factor(experiment_id),
      website_aggregated_high_level = as.factor(website_aggregated_high_level),
      post = if (is.factor(post)) as.numeric(as.character(post)) == 1 else as.logical(post)
    )
  
  panel_data <- panel_data %>%
    mutate(
      time_minutes = total_time_spent / 60,
      n_visits     = total_visit_count
    )
  
  if (any(!is.finite(panel_data$time_minutes) | panel_data$time_minutes < 0)) {
    stop("PPML time outcome must be finite and non-negative.")
  }
  
  global_med_privacy <- median(panel_data$privacy_score, na.rm = TRUE)
  
  panel_data <- join_weights(panel_data, wt_spec)
  
  cat("Panel observations:", nrow(panel_data), "\n")
  cat("Unique users:",       n_distinct(panel_data$experiment_id), "\n")
  cat("Unique websites:",    n_distinct(panel_data$website_aggregated_high_level), "\n")
  cat("Global median privacy score:", round(global_med_privacy, 3), "\n\n")
  
  # -------------------------------------------------------------------------
  # SPECIFICATION (i): Aggregate effect on website choices
  #   y_ijt = T_i * Post_t + eta_j + eta_i + e_ijt
  # -------------------------------------------------------------------------
  
  spec_i_time <- run_weighted_fepois(
    time_minutes ~ experiment_condition * post
    | experiment_id + website_aggregated_high_level,
    data = panel_data, cluster_var = ~experiment_id, wt_spec = wt_spec
  )
  
  spec_i_visits <- NULL
  if ("n_visits" %in% names(panel_data) || "total_visit_count" %in% names(panel_data)) {
    visit_col <- if ("n_visits" %in% names(panel_data)) "n_visits" else "total_visit_count"
    spec_i_visits <- run_weighted_feols(
      as.formula(paste(visit_col,
                       "~ experiment_condition * post | experiment_id + website_aggregated_high_level")),
      data = panel_data, cluster_var = ~experiment_id, wt_spec = wt_spec
    )
  }
  
  cat("Specification (i) - Raw Time PPML:\n"); print(summary(spec_i_time))
  if (!is.null(spec_i_visits)) {
    cat("\nSpecification (i) - Number of Visits:\n"); print(summary(spec_i_visits))
  }
  
  # -------------------------------------------------------------------------
  # SPECIFICATION (ii): Triple-difference with privacy heterogeneity
  #   y_ijt = P_ij * T_i * Post_t + ... + eta_j + eta_i + e_ijt
  # -------------------------------------------------------------------------
  
  spec_ii_continuous <- run_weighted_fepois(
    time_minutes ~ privacy_score * experiment_condition * post
    | experiment_id + website_aggregated_high_level,
    data = panel_data, cluster_var = ~experiment_id, wt_spec = wt_spec
  )
  
  spec_ii_continuous_visits <- NULL
  if ("n_visits" %in% names(panel_data) || "total_visit_count" %in% names(panel_data)) {
    visit_col <- if ("n_visits" %in% names(panel_data)) "n_visits" else "total_visit_count"
    spec_ii_continuous_visits <- run_weighted_feols(
      as.formula(paste(visit_col,
                       "~ privacy_score * experiment_condition * post | experiment_id + website_aggregated_high_level")),
      data = panel_data, cluster_var = ~experiment_id, wt_spec = wt_spec
    )
  }
  
  cat("\nSpecification (ii) - Continuous Privacy Score:\n")
  print(summary(spec_ii_continuous))
  if (!is.null(spec_ii_continuous_visits)) {
    cat("\nSpecification (ii) - Continuous Privacy Score - N Visits:\n")
    print(summary(spec_ii_continuous_visits))
  }
  
  # -------------------------------------------------------------------------
  # Fig C.9(a) - Spec (i) coefficient plot
  # File: preregistered_spec_i_{panel}[_wt].pdf
  # -------------------------------------------------------------------------
  
  suffix <- paste0("_", gsub(" ", "_", tolower(panel_label)))
  
  # Convert PPML log coefficients and their confidence limits to proportional
  # effects in percentage points. Visit-count OLS coefficients stay in levels.
  tidy_ppml_percent <- function(model) {
    broom::tidy(model) %>%
      mutate(
        conf.low  = 100 * (exp(estimate - 1.96 * std.error) - 1),
        conf.high = 100 * (exp(estimate + 1.96 * std.error) - 1),
        estimate  = 100 * (exp(estimate) - 1)
      )
  }
  
  tidy_level_effect <- function(model) {
    broom::tidy(model) %>%
      mutate(
        conf.low  = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error
      )
  }
  
  spec_i_coefs <- tidy_ppml_percent(spec_i_time) %>%
    filter(grepl(":post", term)) %>%
    mutate(outcome   = "Time Spent (% Change)",
           Treatment = case_when(grepl("info",     term) ~ "Information",
                                 grepl("saliency", term) ~ "Saliency"),
           Treatment = factor(Treatment, levels = c("Saliency", "Information")))
  
  if (nrow(spec_i_coefs) != 2L) {
    stop("Figure C.9(a): expected two treatment-by-post PPML coefficients.")
  }
  
  if (!is.null(spec_i_visits)) {
    spec_i_coefs <- bind_rows(
      spec_i_coefs,
      tidy_level_effect(spec_i_visits) %>%
        filter(grepl(":post", term)) %>%
        mutate(outcome   = "N Visits",
               Treatment = case_when(grepl("info",     term) ~ "Information",
                                     grepl("saliency", term) ~ "Saliency"),
               Treatment = factor(Treatment, levels = c("Saliency", "Information")))
    )
  }
  
  # Keep time on the left and visits on the right in Figure C.9(a).
  spec_i_coefs <- spec_i_coefs %>%
    mutate(outcome = factor(
      outcome,
      levels = c("Time Spent (% Change)", "N Visits")
    ))
  
  p_spec_i <- ggplot(spec_i_coefs,
                     aes(x = Treatment, y = estimate, color = Treatment)) +
    geom_hline_zero(linetype = "dashed") +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width     = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH
    ) +
    geom_point(size = POINT_SIZE) +
    facet_wrap(~outcome, scales = "free_y") +
    scale_color_treatment() +
    theme_privacy_experiment() +
    labs(x = "", y = "Coefficient (95% CI)", color = "Treatment")
  
  ggsave(paste0(figures_dir, "preregistered_spec_i", suffix, wt_suffix, ".pdf"),
         p_spec_i, width = 8, height = 5, device = cairo_pdf)
  cat("Saved: preregistered_spec_i", suffix, wt_suffix, ".pdf\n", sep = "")
  
  # -------------------------------------------------------------------------
  # Fig C.9(b) - Continuous privacy-score triple-interaction coefficient plot
  # File: preregistered_triple_interaction_{panel}[_wt].pdf
  # -------------------------------------------------------------------------
  
  triple_interaction_coefs <- tidy_ppml_percent(spec_ii_continuous) %>%
    filter(grepl("privacy_score", term),
           grepl("experiment_condition", term),
           grepl("post", term)) %>%
    mutate(outcome   = "Time Spent (% Change)",
           Treatment = case_when(grepl("info",     term) ~ "Information",
                                 grepl("saliency", term) ~ "Saliency"),
           Treatment = factor(Treatment, levels = c("Saliency", "Information")))
  
  if (nrow(triple_interaction_coefs) != 2L) {
    stop("Figure C.9(b): expected two continuous privacy-by-treatment-by-post PPML coefficients.")
  }
  
  if (!is.null(spec_ii_continuous_visits)) {
    triple_interaction_coefs <- bind_rows(
      triple_interaction_coefs,
      tidy_level_effect(spec_ii_continuous_visits) %>%
        filter(grepl("privacy_score", term),
               grepl("experiment_condition", term),
               grepl("post", term)) %>%
        mutate(outcome   = "N Visits",
               Treatment = case_when(grepl("info",     term) ~ "Information",
                                     grepl("saliency", term) ~ "Saliency"),
               Treatment = factor(Treatment, levels = c("Saliency", "Information")))
    )
  }
  
  # Use the same outcome order in Figure C.9(b).
  triple_interaction_coefs <- triple_interaction_coefs %>%
    mutate(outcome = factor(
      outcome,
      levels = c("Time Spent (% Change)", "N Visits")
    ))
  
  p_triple_interaction <- ggplot(triple_interaction_coefs,
                                 aes(x = Treatment, y = estimate, color = Treatment)) +
    geom_hline_zero(linetype = "dashed") +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width     = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH
    ) +
    geom_point(size = POINT_SIZE) +
    facet_wrap(~outcome, scales = "free_y") +
    scale_color_treatment() +
    theme_privacy_experiment() +
    labs(x = "", y = "Coefficient (95% CI)", color = "Treatment")
  
  ggsave(paste0(figures_dir, "preregistered_triple_interaction", suffix, wt_suffix, ".pdf"),
         p_triple_interaction, width = 8, height = 5, device = cairo_pdf)
  cat("Saved: preregistered_triple_interaction", suffix, wt_suffix, ".pdf\n", sep = "")
  
  return(list(
    panel_data              = panel_data,
    panel_label             = panel_label,
    global_med_privacy      = global_med_privacy,
    spec_i_time             = spec_i_time,
    spec_i_visits           = spec_i_visits,
    spec_ii_continuous      = spec_ii_continuous,
    spec_ii_continuous_visits = spec_ii_continuous_visits,
    plots = list(
      spec_i             = p_spec_i,
      triple_interaction = p_triple_interaction
    )
  ))
}