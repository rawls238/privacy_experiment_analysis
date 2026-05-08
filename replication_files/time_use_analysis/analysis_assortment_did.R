# ============================================================================
# ANALYSIS: WEBSITE ASSORTMENT CHANGES AND USER-LEVEL DID
# ============================================================================
# Purpose: Analyze treatment effects on website assortment and privacy choices
# Consistent with time_usage_treatment_effects.R patterns
#
# To use: source() this file from time_usage_treatment_effects.R after
# loading data and running get_balanced_panel()
#
# MODIFICATION LOG (Survey Weighting):
#   run_assortment_analysis: Added wt_spec, wt_suffix params (Fig 6)
#   run_preregistered_specs: Added wt_spec, wt_suffix params (Fig 5)
#   spec_ii_continuous: Changed feols -> run_weighted_feols [P3b]
# ============================================================================


# ============================================================================
# ANALYSIS 2: WEBSITE ASSORTMENT CHANGE ANALYSIS
# ============================================================================
# Examines how treatment affects the composition of visited websites:
# - Entry/exit of sites (new vs dropped)
# - Concentration changes (HHI)
# - Privacy reallocation across sites
# ============================================================================

run_assortment_analysis <- function(full_dat,
                                    output_dir = TABLES_DIR,
                                    figures_dir = FIGURES_DIR,
                                    wt_spec = "unweighted",
                                    wt_suffix = "") {
  
  cat("\n--- ANALYSIS 2: WEBSITE ASSORTMENT CHANGES ---\n")
  cat("Weight specification:", wt_spec, "\n\n")
  
  # --------------------------------------------------------------------------
  # 2.1: Calculate site-level time shares for each user-category-period
  # --------------------------------------------------------------------------
  
  site_shares <- full_dat %>%
    filter(!is.na(privacy_for_requested_attribute) &
             !is.na(category_level_1) & category_level_1 != "") %>%
    group_by(experiment_id, experiment_condition, category_level_1, post,
             website_aggregated_high_level) %>%
    summarise(
      site_time = sum(time_spent, na.rm = TRUE),
      site_privacy = first(privacy_for_requested_attribute),
      .groups = 'drop'
    ) %>%
    group_by(experiment_id, experiment_condition, category_level_1, post) %>%
    mutate(
      time_share = site_time / sum(site_time),
      total_time = sum(site_time)
    ) %>%
    ungroup()
  
  cat("Site shares calculated:", nrow(site_shares), "observations\n")
  
  # --------------------------------------------------------------------------
  # 2.2: Entry/Exit Analysis - Privacy of New vs Dropped Sites
  # --------------------------------------------------------------------------
  
  cat("\n--- 2.2: Site Entry/Exit Analysis ---\n")
  
  # Get privacy scores for sites in pre and post periods
  site_privacy_pre <- site_shares %>%
    filter(post == FALSE) %>%
    select(experiment_id, category_level_1, website_aggregated_high_level,
           site_privacy_pre = site_privacy, time_share_pre = time_share)
  
  site_privacy_post <- site_shares %>%
    filter(post == TRUE) %>%
    select(experiment_id, category_level_1, website_aggregated_high_level,
           site_privacy_post = site_privacy, time_share_post = time_share)
  
  # Identify new, dropped, and retained sites
  entry_exit <- site_privacy_pre %>%
    full_join(site_privacy_post,
              by = c("experiment_id", "category_level_1", "website_aggregated_high_level")) %>%
    mutate(
      site_status = case_when(
        is.na(time_share_pre) & !is.na(time_share_post) ~ "new",
        !is.na(time_share_pre) & is.na(time_share_post) ~ "dropped",
        TRUE ~ "retained"
      ),
      privacy_score = coalesce(site_privacy_post, site_privacy_pre)
    ) %>%
    left_join(
      full_dat %>% select(experiment_id, experiment_condition) %>% distinct(),
      by = "experiment_id"
    )
  
  # Summary statistics
  entry_exit_summary <- entry_exit %>%
    group_by(experiment_condition, site_status) %>%
    summarise(
      n_sites = n(),
      mean_privacy = mean(privacy_score, na.rm = TRUE),
      sd_privacy = sd(privacy_score, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = site_status,
      values_from = c(n_sites, mean_privacy, sd_privacy)
    )
  
  cat("\nPrivacy Scores by Site Status and Treatment:\n")
  print(entry_exit_summary)
  
  # Calculate user-level entry/exit privacy differentials
  user_entry_exit <- entry_exit %>%
    filter(site_status %in% c("new", "dropped")) %>%
    group_by(experiment_id, experiment_condition, category_level_1, site_status) %>%
    summarise(
      avg_privacy = mean(privacy_score, na.rm = TRUE),
      n_sites = n(),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = site_status,
      values_from = c(avg_privacy, n_sites),
      names_sep = "_"
    ) %>%
    mutate(
      privacy_differential = avg_privacy_new - avg_privacy_dropped
    ) %>%
    filter(!is.na(privacy_differential))
  
  # Summary by treatment
  differential_summary <- user_entry_exit %>%
    group_by(experiment_condition) %>%
    summarise(
      n = n(),
      mean_differential = mean(privacy_differential, na.rm = TRUE),
      sd_differential = sd(privacy_differential, na.rm = TRUE),
      pct_positive = mean(privacy_differential > 0, na.rm = TRUE) * 100,
      .groups = 'drop'
    )
  
  cat("\nPrivacy Differential (New - Dropped) by Treatment:\n")
  print(differential_summary)
  
  # Regression: Privacy differential ~ treatment (using feols)
  user_entry_exit$experiment_condition <- factor(
    user_entry_exit$experiment_condition,
    levels = c("control", "info", "saliency")
  )
  user_entry_exit$category_level_1 <- as.factor(user_entry_exit$category_level_1)
  
  # ---- [A1] Join survey weights to user_entry_exit ----
  user_entry_exit <- join_weights(user_entry_exit, wt_spec)
  
  # ---- [A2] model_differential: run_weighted_feols ----
  model_differential <- run_weighted_feols(
    privacy_differential ~ experiment_condition | category_level_1,
    data = user_entry_exit,
    cluster_var = ~experiment_id,
    wt_spec = wt_spec
  )
  
  cat("\nPrivacy Differential Regression (Category FE, clustered SE):\n")
  print(summary(model_differential))
  
  # --------------------------------------------------------------------------
  # 2.3: Concentration Changes (HHI)
  # --------------------------------------------------------------------------
  
  cat("\n--- 2.3: Concentration Analysis ---\n")
  
  # Calculate HHI for each user-category-period
  concentration_metrics <- site_shares %>%
    group_by(experiment_id, experiment_condition, category_level_1, post) %>%
    summarise(
      n_sites = n_distinct(website_aggregated_high_level),
      hhi = sum(time_share^2),
      top1_share = max(time_share),
      top3_share = sum(sort(time_share, decreasing = TRUE)[1:min(3, n())]),
      avg_privacy = sum(time_share * site_privacy, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Wide format for change calculations
  concentration_wide <- concentration_metrics %>%
    pivot_wider(
      id_cols = c(experiment_id, experiment_condition, category_level_1),
      names_from = post,
      values_from = c(n_sites, hhi, top1_share, top3_share, avg_privacy),
      names_sep = "_"
    ) %>%
    mutate(
      hhi_change = hhi_TRUE - hhi_FALSE,
      top1_change = top1_share_TRUE - top1_share_FALSE,
      top3_change = top3_share_TRUE - top3_share_FALSE,
      n_sites_change = n_sites_TRUE - n_sites_FALSE,
      privacy_change = avg_privacy_TRUE - avg_privacy_FALSE
    ) %>%
    filter(!is.na(hhi_change))
  
  # Summary
  concentration_summary <- concentration_wide %>%
    group_by(experiment_condition) %>%
    summarise(
      n = n(),
      mean_hhi_change = mean(hhi_change, na.rm = TRUE),
      mean_top1_change = mean(top1_change, na.rm = TRUE),
      mean_top3_change = mean(top3_change, na.rm = TRUE),
      mean_n_sites_change = mean(n_sites_change, na.rm = TRUE),
      mean_privacy_change = mean(privacy_change, na.rm = TRUE),
      .groups = 'drop'
    )
  
  cat("\nConcentration Changes in User Portfolios by Treatment:\n")
  print(concentration_summary)
  
  # Regressions
  concentration_wide$experiment_condition <- factor(
    concentration_wide$experiment_condition,
    levels = c("control", "info", "saliency")
  )
  concentration_wide$category_level_1 <- as.factor(concentration_wide$category_level_1)
  
  # ---- [A3] Join survey weights to concentration_wide ----
  concentration_wide <- join_weights(concentration_wide, wt_spec)
  
  # ---- [A4] model_hhi, model_top1, model_privacy_change: run_weighted_feols ----
  model_hhi <- run_weighted_feols(
    hhi_change ~ experiment_condition | category_level_1,
    data = concentration_wide,
    cluster_var = ~experiment_id,
    wt_spec = wt_spec
  )
  
  model_top1 <- run_weighted_feols(
    top1_change ~ experiment_condition | category_level_1,
    data = concentration_wide,
    cluster_var = ~experiment_id,
    wt_spec = wt_spec
  )
  
  model_privacy_change <- run_weighted_feols(
    privacy_change ~ experiment_condition | category_level_1,
    data = concentration_wide,
    cluster_var = ~experiment_id,
    wt_spec = wt_spec
  )
  
  cat("\nUser HHI Change Regression:\n")
  print(summary(model_hhi))
  
  cat("\nTop Site Share Change Regression:\n")
  print(summary(model_top1))
  
  cat("\nPrivacy Change Regression:\n")
  print(summary(model_privacy_change))
  
  # --------------------------------------------------------------------------
  # 2.4: Output Tables (etable)
  # --------------------------------------------------------------------------
  
  # ---- [A5] Output filenames add wt_suffix ----
  etable(
    model_differential, model_hhi, model_top1, model_privacy_change,
    tex = TRUE,
    file = paste0(output_dir, "assortment_treatment_effects", wt_suffix, ".tex"),
    title = "Treatment Effects on Website Assortment Changes",
    headers = c("Privacy Diff", "User HHI Change", "Top1 Change", "Privacy Change"),
    dict = c(
      experiment_conditioninfo = "Info Treatment",
      experiment_conditionsaliency = "Saliency Treatment"
    ),
    notes = paste0("Category FE; SE clustered by user. Privacy Diff = avg privacy of new sites - dropped sites.",
                   if (wt_spec != "unweighted") paste0(" Weights: ", wt_spec, ".") else ""),
    fitstat = c("n", "r2")
  )
  
  cat("\nSaved: assortment_treatment_effects", wt_suffix, ".tex\n", sep = "")
  
  # --------------------------------------------------------------------------
  # 2.5: Create Figures
  # --------------------------------------------------------------------------
  
  cat("\n--- Creating Assortment Figures ---\n")
  
  
  # Figure 1a: Concentration Coefficient Plot (HHI, Top Site Share)
  concentration_coefs <- bind_rows(
    broom::tidy(model_hhi) %>%
      filter(grepl("experiment_condition", term)) %>%
      mutate(outcome = "Portfolio HHI"),
    broom::tidy(model_top1) %>%
      filter(grepl("experiment_condition", term)) %>%
      mutate(outcome = "Top Site Share")
  ) %>%
    mutate(
      Condition = case_when(
        grepl("info", term) ~ "Information",
        grepl("saliency", term) ~ "Saliency"
      ),
      Condition = factor(Condition, levels = c("Saliency", "Information")),
      outcome = factor(outcome, levels = c("Portfolio HHI", "Top Site Share"))
    )
  
  p_concentration_coefs <- ggplot(concentration_coefs,
                                  aes(x = outcome, y = estimate, color = Condition)) +
    geom_hline_zero(linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
      width = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH,
      position = position_dodge(width = DODGE_WIDTH_2)
    ) +
    geom_point(size = POINT_SIZE, position = position_dodge(width = DODGE_WIDTH_2)) +
    scale_color_treatment() +
    theme_privacy_experiment() +
    labs(
      x = "",
      y = "Coefficient",
      color = NULL
    )
  
  ggsave(paste0(figures_dir, "assortment_concentration", wt_suffix, ".png"),
         p_concentration_coefs, width = 6, height = 5, dpi = 300)
  
  cat("Saved: assortment_concentration", wt_suffix, ".png\n", sep = "")
  
  # Figure 1b: Privacy Coefficient Plot (Privacy Differential, Privacy Score)
  privacy_coefs <- bind_rows(
    broom::tidy(model_differential) %>%
      filter(grepl("experiment_condition", term)) %>%
      mutate(outcome = "Privacy Differential\n(New - Dropped)"),
    broom::tidy(model_privacy_change) %>%
      filter(grepl("experiment_condition", term)) %>%
      mutate(outcome = "Privacy Score")
  ) %>%
    mutate(
      Condition = case_when(
        grepl("info", term) ~ "Information",
        grepl("saliency", term) ~ "Saliency"
      ),
      Condition = factor(Condition, levels = c("Saliency", "Information")),
      outcome = factor(outcome, levels = c("Privacy Differential\n(New - Dropped)",
                                           "Privacy Score"))
    )
  
  p_privacy_coefs <- ggplot(privacy_coefs,
                            aes(x = outcome, y = estimate, color = Condition)) +
    geom_hline_zero(linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
      width = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH,
      position = position_dodge(width = DODGE_WIDTH_2)
    ) +
    geom_point(size = POINT_SIZE, position = position_dodge(width = DODGE_WIDTH_2)) +
    scale_color_treatment() +
    theme_privacy_experiment() +
    labs(
      x = "",
      y = "Coefficient",
      color = NULL
    )
  
  ggsave(paste0(figures_dir, "assortment_privacy", wt_suffix, ".png"),
         p_privacy_coefs, width = 6, height = 5, dpi = 300)
  
  cat("Saved: assortment_privacy", wt_suffix, ".png\n", sep = "")
  
  # Figure 2: Entry/Exit Privacy by Treatment (Bar Chart with SE)
  entry_exit_by_treatment <- entry_exit %>%
    filter(site_status %in% c("new", "dropped")) %>%
    group_by(experiment_condition, site_status) %>%
    summarise(
      mean_privacy = mean(privacy_score, na.rm = TRUE),
      se_privacy = sd(privacy_score, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      experiment_condition = factor(experiment_condition,
                                    levels = c("control", "info", "saliency")),
      site_status = factor(site_status, levels = c("dropped", "new"),
                           labels = c("Dropped Sites", "New Sites"))
    )
  
  p_entry_exit <- ggplot(entry_exit_by_treatment,
                         aes(x = experiment_condition, y = mean_privacy,
                             fill = site_status)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
    geom_errorbar(aes(ymin = mean_privacy - 1.96*se_privacy,
                      ymax = mean_privacy + 1.96*se_privacy),
                  position = position_dodge(width = 0.7), width = 0.2, color = "gray30") +
    scale_fill_manual(values = c("Dropped Sites" = "#E41A1C", "New Sites" = "#4DAF4A"),
                      name = "Site Status") +
    labs(
      title = "Privacy of New vs Dropped Sites by Treatment",
      subtitle = "Mean privacy score (95% CI)",
      x = "Treatment Condition",
      y = "Mean Privacy Score"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "bottom"
    )
  
  ggsave(paste0(figures_dir, "assortment_entry_exit_privacy", wt_suffix, ".png"),
         p_entry_exit, width = 8, height = 5, dpi = 300)
  
  cat("Saved: assortment_entry_exit_privacy", wt_suffix, ".png\n", sep = "")
  
  # Figure 3: Privacy Differential Distribution by Treatment
  p_differential_dist <- ggplot(user_entry_exit,
                                aes(x = privacy_differential,
                                    fill = experiment_condition)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
    scale_fill_manual(values = c("control" = "#D62728", "info" = "#2CA02C", "saliency" = "#1F77B4"),
                      name = "Condition") +
    labs(
      title = "Distribution of Privacy Differential (New - Dropped Sites)",
      subtitle = "Positive = switching to more privacy-protective sites",
      x = "Privacy Differential",
      y = "Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "bottom"
    )
  
  ggsave(paste0(figures_dir, "assortment_differential_distribution", wt_suffix, ".png"),
         p_differential_dist, width = 9, height = 5, dpi = 300)
  
  cat("Saved: assortment_differential_distribution", wt_suffix, ".png\n", sep = "")
  
  # Figure 4: Privacy Differential by Treatment (Bar with SE)
  differential_with_se <- user_entry_exit %>%
    group_by(experiment_condition) %>%
    summarise(
      mean_diff = mean(privacy_differential, na.rm = TRUE),
      se_diff = sd(privacy_differential, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      experiment_condition = factor(experiment_condition,
                                    levels = c("control", "info", "saliency"))
    )
  
  p_differential_bar <- ggplot(differential_with_se,
                               aes(x = experiment_condition, y = mean_diff,
                                   fill = experiment_condition)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_errorbar(aes(ymin = mean_diff - 1.96*se_diff,
                      ymax = mean_diff + 1.96*se_diff),
                  width = 0.2, color = "gray30") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(values = c("control" = "#D62728", "info" = "#2CA02C", "saliency" = "#1F77B4")) +
    labs(
      title = "Mean Privacy Differential by Treatment",
      subtitle = "New sites privacy - Dropped sites privacy (95% CI)",
      x = "Treatment Condition",
      y = "Privacy Differential"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "none"
    )
  
  ggsave(paste0(figures_dir, "assortment_differential_bar", wt_suffix, ".png"),
         p_differential_bar, width = 7, height = 5, dpi = 300)
  
  cat("Saved: assortment_differential_bar", wt_suffix, ".png\n", sep = "")
  
  # Figure 5: Concentration Changes by Treatment
  concentration_long <- concentration_wide %>%
    select(experiment_id, experiment_condition, category_level_1,
           hhi_change, top1_change, n_sites_change, privacy_change) %>%
    pivot_longer(cols = c(hhi_change, top1_change, n_sites_change, privacy_change),
                 names_to = "metric", values_to = "change") %>%
    mutate(
      metric = case_when(
        metric == "hhi_change" ~ "HHI",
        metric == "top1_change" ~ "Top Site Share",
        metric == "n_sites_change" ~ "N Sites",
        metric == "privacy_change" ~ "Privacy Score"
      ),
      metric = factor(metric, levels = c("HHI", "Top Site Share", "N Sites", "Privacy Score"))
    )
  
  concentration_summary_long <- concentration_long %>%
    group_by(experiment_condition, metric) %>%
    summarise(
      mean_change = mean(change, na.rm = TRUE),
      se_change = sd(change, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    ) %>%
    mutate(
      experiment_condition = factor(experiment_condition,
                                    levels = c("control", "info", "saliency"))
    )
  
  p_concentration <- ggplot(concentration_summary_long,
                            aes(x = experiment_condition, y = mean_change,
                                fill = experiment_condition)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_errorbar(aes(ymin = mean_change - 1.96*se_change,
                      ymax = mean_change + 1.96*se_change),
                  width = 0.2, color = "gray30") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    facet_wrap(~metric, scales = "free_y", nrow = 1) +
    scale_fill_manual(values = c("control" = "#D62728", "info" = "#2CA02C", "saliency" = "#1F77B4")) +
    labs(
      title = "Concentration and Privacy Changes by Treatment",
      subtitle = "Post - Pre change (95% CI)",
      x = "Treatment Condition",
      y = "Change"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      strip.text = element_text(size = 10)
    )
  
  ggsave(paste0(figures_dir, "assortment_concentration_changes", wt_suffix, ".png"),
         p_concentration, width = 12, height = 5, dpi = 300)
  
  cat("Saved: assortment_concentration_changes", wt_suffix, ".png\n", sep = "")
  
  # Return results
  return(list(
    entry_exit = entry_exit,
    entry_exit_summary = entry_exit_summary,
    differential_summary = differential_summary,
    differential_with_se = differential_with_se,
    concentration_wide = concentration_wide,
    concentration_summary = concentration_summary,
    concentration_summary_long = concentration_summary_long,
    model_differential = model_differential,
    model_hhi = model_hhi,
    model_top1 = model_top1,
    model_privacy_change = model_privacy_change,
    plots = list(
      concentration_coefs = p_concentration_coefs,
      privacy_coefs = p_privacy_coefs,
      entry_exit = p_entry_exit,
      differential_dist = p_differential_dist,
      differential_bar = p_differential_bar,
      concentration = p_concentration
    )
  ))
}


# ============================================================================
# ANALYSIS 2B: USER-LEVEL DIFFERENCE-IN-DIFFERENCES
# ============================================================================
# Panel DiD analysis - takes a pre-built panel and runs regressions
# Pass any panel (balanced, unbalanced, weekly, etc.) and a label
# NOTE: This function is NOT modified for weighting (not in EC paper Fig 5-6)
# ============================================================================

run_did_analysis <- function(panel_data,
                             panel_label = "panel",
                             output_dir = TABLES_DIR,
                             figures_dir = FIGURES_DIR) {
  
  cat("\n--- ANALYSIS 2B: USER-LEVEL DIFFERENCE-IN-DIFFERENCES ---\n\n")
  
  cat("Panel label:", panel_label, "\n")
  cat("Panel observations:", nrow(panel_data), "\n")
  cat("Unique users:", n_distinct(panel_data$experiment_id), "\n")
  cat("Unique websites:", n_distinct(panel_data$website_aggregated_high_level), "\n\n")
  
  # --------------------------------------------------------------------------
  # Prepare data for DiD
  # --------------------------------------------------------------------------
  
  # Ensure factors are set correctly
  panel_data$experiment_condition <- factor(
    panel_data$experiment_condition,
    levels = c("control", "info", "saliency")
  )
  panel_data$experiment_id <- as.factor(panel_data$experiment_id)
  panel_data$website_aggregated_high_level <- as.factor(panel_data$website_aggregated_high_level)
  
  # Create log time variable if not already present
  if (!"log_time" %in% names(panel_data)) {
    panel_data <- panel_data %>%
      mutate(
        log_time = log1p(total_time_spent),
        has_time = total_time_spent > 0
      )
  }
  
  # --------------------------------------------------------------------------
  # Summary Statistics
  # --------------------------------------------------------------------------
  
  summary_stats <- panel_data %>%
    filter(!is.na(privacy_for_requested_attribute)) %>%
    group_by(experiment_condition, post) %>%
    summarise(
      N = n(),
      mean_privacy = mean(privacy_for_requested_attribute, na.rm = TRUE),
      sd_privacy = sd(privacy_for_requested_attribute, na.rm = TRUE),
      mean_log_time = mean(log_time, na.rm = TRUE),
      sd_log_time = sd(log_time, na.rm = TRUE),
      conditional_time = mean(ifelse(log_time == 0, NA, log_time), na.rm = TRUE),
      pct_active = mean(has_time, na.rm = TRUE) * 100,
      .groups = 'drop'
    )
  
  cat("Summary Statistics by Treatment and Period:\n")
  print(summary_stats)
  
  # --------------------------------------------------------------------------
  # Main DiD Regressions
  # --------------------------------------------------------------------------
  
  cat("\n--- DiD Regressions ---\n")
  
  # Filter to sites with privacy data
  panel_priv <- panel_data %>%
    filter(!is.na(privacy_for_requested_attribute))
  
  # Model 1: Log time with user + site FE
  did_time_1 <- feols(
    log_time ~ experiment_condition * post
    | experiment_id + website_aggregated_high_level,
    cluster = ~experiment_id,
    data = panel_priv
  )
  
  # Model 2: Log time with user FE only
  did_time_2 <- feols(
    log_time ~ experiment_condition * post
    | experiment_id + category_level_1,
    cluster = ~experiment_id,
    data = panel_priv
  )
  
  # Model 3: Privacy score DiD (time-weighted)
  # Create user-period level aggregates with time-weighted privacy
  user_period_panel <- panel_priv %>%
    group_by(experiment_id, experiment_condition, category_level_1, post) %>%
    summarise(
      total_time = sum(total_time_spent, na.rm = TRUE),
      log_time = log1p(sum(total_time_spent, na.rm = TRUE) / 60),
      n_sites = n_distinct(website_aggregated_high_level[total_time_spent > 0]),
      # Time-weighted privacy (using only active sites)
      avg_privacy = if (sum(total_time_spent, na.rm = TRUE) > 0) {
        sum(total_time_spent * privacy_full_beta_p, na.rm = TRUE) /
          sum(total_time_spent[!is.na(privacy_full_beta_p)], na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = 'drop'
    ) %>%
    filter(!is.na(avg_privacy))
  
  user_period_panel$experiment_condition <- factor(
    user_period_panel$experiment_condition,
    levels = c("control", "info", "saliency")
  )
  user_period_panel$experiment_id <- as.factor(user_period_panel$experiment_id)
  
  cat("\nUser-period panel:", nrow(user_period_panel), "observations\n")
  
  # Model 3: Privacy score (time-weighted) with user FE
  did_privacy_1 <- feols(
    avg_privacy ~ experiment_condition * post
    | experiment_id + category_level_1,
    cluster = ~experiment_id,
    data = user_period_panel
  )
  
  did_privacy_2 <- feols(
    avg_privacy ~ experiment_condition * post
    | experiment_id,
    cluster = ~experiment_id,
    data = user_period_panel
  )
  
  did_privacy_3 <- feols(
    privacy_full_beta_p ~ experiment_condition * post
    | experiment_id + website_aggregated_high_level,
    cluster = ~experiment_id,
    data = panel_priv
  )
  
  did_privacy_4 <- feols(
    privacy_full_beta_p ~ experiment_condition * post
    | experiment_id + category_level_1,
    cluster = ~experiment_id,
    data = panel_priv
  )
  
  # Model 4: N sites with user FE
  did_nsites <- feols(
    n_sites ~ experiment_condition * post
    | experiment_id + category_level_1,
    cluster = ~experiment_id,
    data = user_period_panel
  )
  
  # Model 5: Log time at user-period level
  did_time_user <- feols(
    log_time ~ experiment_condition * post
    | experiment_id + category_level_1,
    cluster = ~experiment_id,
    data = user_period_panel
  )
  
  cat("\n--- Model Results ---\n")
  
  cat("\nModel 1: Log Time (User + Site FE):\n")
  print(summary(did_time_1))
  
  cat("\nModel 2: Log Time (User FE):\n")
  print(summary(did_time_2))
  
  cat("\nModel 3: Privacy Score (Time-Weighted, User FE):\n")
  print(summary(did_privacy_1))
  print(summary(did_privacy_2))
  print(summary(did_privacy_3))
  print(summary(did_privacy_4))
  
  cat("\nModel 4: N Sites (User FE):\n")
  print(summary(did_nsites))
  
  # --------------------------------------------------------------------------
  # Triple-Difference with Privacy Heterogeneity
  # --------------------------------------------------------------------------
  
  cat("\n--- Triple-Difference with Privacy Heterogeneity ---\n")
  
  med_privacy <- median(panel_priv$privacy_for_requested_attribute, na.rm = TRUE)
  
  panel_priv <- panel_priv %>%
    mutate(
      high_privacy = privacy_for_requested_attribute < med_privacy
    )
  
  cat("Median privacy score:", round(med_privacy, 3), "\n")
  cat("High privacy (less invasive) sites:", sum(panel_priv$high_privacy, na.rm = TRUE), "\n")
  
  # Triple-diff model
  did_triple <- feols(
    log_time ~ experiment_condition * post * high_privacy
    | experiment_id + website_aggregated_high_level,
    cluster = ~experiment_id,
    data = panel_priv
  )
  
  cat("\nTriple-Difference Model (High Privacy = Less Invasive):\n")
  print(summary(did_triple))
  
  # --------------------------------------------------------------------------
  # Output Tables (etable)
  # --------------------------------------------------------------------------
  
  suffix <- paste0("_", gsub(" ", "_", tolower(panel_label)))
  
  etable(
    did_time_1, did_time_2, did_privacy_1, did_privacy_2, did_privacy_3, did_privacy_4, did_nsites,
    tex = TRUE,
    file = paste0(output_dir, "did_main_results", suffix, ".tex"),
    title = paste0("Difference-in-Differences: Treatment Effects on Browsing Behavior (",
                   panel_label, ")"),
    headers = c("Log Time", "Log Time", "Avg Privacy", "Avg Privacy", "Privacy", "Privacy", "N Sites"),
    extralines = list(
      "_Level" = c("Site", "Site", "User-Cat", "User-Cat", "Site", "Site", "User-Cat"),
      "_FE" = c("User+Site", "User+Cat", "User+Cat", "User", "User+Site", "User+Cat", "User+Cat")
    ),
    dict = c(
      experiment_conditioninfo = "Info",
      experiment_conditionsaliency = "Saliency",
      postTRUE = "Post",
      `experiment_conditioninfo:postTRUE` = "Info $\\times$ Post",
      `experiment_conditionsaliency:postTRUE` = "Saliency $\\times$ Post"
    ),
    notes = paste0("SE clustered by user. Avg Privacy = time-weighted at user-category level. Privacy = site-level. ",
                   panel_label, "."),
    fitstat = c("n", "r2")
  )
  
  cat("\nSaved: did_main_results", suffix, ".tex\n", sep = "")
  
  etable(
    did_triple,
    tex = TRUE,
    file = paste0(output_dir, "did_triple_diff", suffix, ".tex"),
    title = paste0("Triple-Difference: Privacy Heterogeneity in Treatment Effects (",
                   panel_label, ")"),
    headers = "Log Time",
    dict = c(
      experiment_conditioninfo = "Info",
      experiment_conditionsaliency = "Saliency",
      postTRUE = "Post",
      high_privacyTRUE = "High Privacy (Less Invasive)",
      `experiment_conditioninfo:postTRUE` = "Info $\\times$ Post",
      `experiment_conditionsaliency:postTRUE` = "Saliency $\\times$ Post",
      `postTRUE:high_privacyTRUE` = "Post $\\times$ High Privacy",
      `experiment_conditioninfo:high_privacyTRUE` = "Info $\\times$ High Privacy",
      `experiment_conditionsaliency:high_privacyTRUE` = "Saliency $\\times$ High Privacy",
      `experiment_conditioninfo:postTRUE:high_privacyTRUE` = "Info $\\times$ Post $\\times$ High Privacy",
      `experiment_conditionsaliency:postTRUE:high_privacyTRUE` = "Saliency $\\times$ Post $\\times$ High Privacy"
    ),
    notes = paste0("User + Site FE; SE clustered by user. High Privacy = below median score (less invasive). ",
                   panel_label, "."),
    fitstat = c("n", "r2")
  )
  
  cat("Saved: did_triple_diff", suffix, ".tex\n", sep = "")
  
  # --------------------------------------------------------------------------
  # Summary Output
  # --------------------------------------------------------------------------
  
  sink(paste0(output_dir, "did_analysis_summary", suffix, ".txt"))
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("ANALYSIS 2B: USER-LEVEL DIFFERENCE-IN-DIFFERENCES - SUMMARY\n")
  cat("Panel:", panel_label, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  
  cat("PANEL STRUCTURE:\n")
  cat("  Observations:", nrow(panel_priv), "\n")
  cat("  Unique users:", n_distinct(panel_priv$experiment_id), "\n")
  cat("  Unique websites:", n_distinct(panel_priv$website_aggregated_high_level), "\n\n")
  
  cat("SUMMARY STATISTICS:\n")
  print(summary_stats)
  
  cat("\n\nMAIN DID ESTIMATES (Treatment x Post):\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  cat("\n1. Log Time (User + Site FE):\n")
  print(coef(did_time_1))
  
  cat("\n2. Log Time (User + Cat FE):\n")
  print(coef(did_time_2))
  
  cat("\n3. Avg Privacy (User + Cat FE, time-weighted):\n")
  print(coef(did_privacy_1))
  
  cat("\n4. Avg Privacy (User FE, time-weighted):\n")
  print(coef(did_privacy_2))
  
  cat("\n5. Privacy (User + Site FE, site-level):\n")
  print(coef(did_privacy_3))
  
  cat("\n6. Privacy (User + Cat FE, site-level):\n")
  print(coef(did_privacy_4))
  
  cat("\n7. N Sites (User + Cat FE):\n")
  print(coef(did_nsites))
  
  cat("\n\nTRIPLE-DIFFERENCE (High Privacy x Treatment x Post):\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("Note: High Privacy = below median score = less invasive = better privacy\n\n")
  print(coef(did_triple))
  
  sink()
  
  cat("\nSaved: did_analysis_summary", suffix, ".txt\n", sep = "")
  
  # Return results
  return(list(
    panel_data = panel_priv,
    panel_label = panel_label,
    user_period_panel = user_period_panel,
    summary_stats = summary_stats,
    did_time_1 = did_time_1,
    did_time_2 = did_time_2,
    did_privacy_1 = did_privacy_1,
    did_privacy_2 = did_privacy_2,
    did_privacy_3 = did_privacy_3,
    did_privacy_4 = did_privacy_4,
    did_nsites = did_nsites,
    did_time_user = did_time_user,
    did_triple = did_triple
  ))
}


# ============================================================================
# ANALYSIS 2C: ROBUST ESTIMATION WITH ZEROS
# ============================================================================
# NOTE: This function is NOT modified for weighting (not in EC paper Fig 5-6)
# ============================================================================

run_robust_did_analysis <- function(panel_data,
                                    panel_label = "panel",
                                    output_dir = TABLES_DIR,
                                    figures_dir = FIGURES_DIR) {
  
  cat("\n--- ANALYSIS 2C: ROBUST DID WITH ZEROS ---\n\n")
  
  cat("Panel label:", panel_label, "\n")
  cat("Panel observations:", nrow(panel_data), "\n")
  
  panel_data$experiment_condition <- factor(
    panel_data$experiment_condition,
    levels = c("control", "info", "saliency")
  )
  panel_data$experiment_id <- as.factor(panel_data$experiment_id)
  panel_data$website_aggregated_high_level <- as.factor(panel_data$website_aggregated_high_level)
  
  panel_data <- panel_data %>%
    mutate(
      visited = as.numeric(total_time_spent > 0),
      log_time = log1p(total_time_spent),
      ihs_time = asinh(total_time_spent),
      has_time = total_time_spent > 0
    )
  
  panel_priv <- panel_data %>%
    filter(!is.na(privacy_for_requested_attribute))
  
  pct_zero <- mean(panel_priv$total_time_spent == 0) * 100
  cat("Percent zeros:", round(pct_zero, 1), "%\n")
  cat("Observations with positive time:", sum(panel_priv$has_time), "\n\n")
  
  cat("--- Extensive Margin (P(visit > 0)) ---\n")
  
  extensive_lpm <- feols(visited ~ experiment_condition * post | experiment_id + website_aggregated_high_level, cluster = ~experiment_id, data = panel_priv)
  extensive_lpm_cat <- feols(visited ~ experiment_condition * post | experiment_id + category_level_1, cluster = ~experiment_id, data = panel_priv)
  
  cat("\nExtensive margin (LPM, User + Site FE):\n")
  print(summary(extensive_lpm))
  
  cat("\n--- Intensive Margin (time | visit > 0) ---\n")
  
  panel_positive <- panel_priv %>% filter(total_time_spent > 0)
  
  intensive_log <- feols(log_time ~ experiment_condition * post | experiment_id + website_aggregated_high_level, cluster = ~experiment_id, data = panel_positive)
  intensive_log_cat <- feols(log_time ~ experiment_condition * post | experiment_id + category_level_1, cluster = ~experiment_id, data = panel_positive)
  
  cat("\nIntensive margin (Log time | visit > 0, User + Site FE):\n")
  print(summary(intensive_log))
  
  cat("\n--- IHS Transformation ---\n")
  
  ihs_model <- feols(ihs_time ~ experiment_condition * post | experiment_id + website_aggregated_high_level, cluster = ~experiment_id, data = panel_priv)
  ihs_model_cat <- feols(ihs_time ~ experiment_condition * post | experiment_id + category_level_1, cluster = ~experiment_id, data = panel_priv)
  
  cat("\nIHS model (User + Site FE):\n")
  print(summary(ihs_model))
  
  cat("\n--- Poisson PPML ---\n")
  
  poisson_model <- tryCatch({
    fepois(total_time_spent ~ experiment_condition * post | experiment_id + website_aggregated_high_level, cluster = ~experiment_id, data = panel_priv)
  }, error = function(e) { cat("Poisson with site FE failed:", e$message, "\n"); NULL })
  
  poisson_model_cat <- tryCatch({
    fepois(total_time_spent ~ experiment_condition * post | experiment_id + category_level_1, cluster = ~experiment_id, data = panel_priv)
  }, error = function(e) { cat("Poisson with category FE failed:", e$message, "\n"); NULL })
  
  if (!is.null(poisson_model)) { cat("\nPoisson PPML (User + Site FE):\n"); print(summary(poisson_model)) }
  
  cat("\n--- Privacy Score Analysis ---\n")
  
  user_cat_panel <- panel_priv %>%
    group_by(experiment_id, experiment_condition, category_level_1, post) %>%
    summarise(
      total_time = sum(total_time_spent, na.rm = TRUE),
      visited_any = as.numeric(sum(total_time_spent, na.rm = TRUE) > 0),
      avg_privacy = if (sum(total_time_spent, na.rm = TRUE) > 0) {
        sum(total_time_spent * privacy_for_requested_attribute, na.rm = TRUE) /
          sum(total_time_spent[!is.na(privacy_for_requested_attribute)], na.rm = TRUE)
      } else { NA_real_ },
      .groups = 'drop'
    )
  
  user_cat_panel$experiment_condition <- factor(user_cat_panel$experiment_condition, levels = c("control", "info", "saliency"))
  
  privacy_intensive <- feols(avg_privacy ~ experiment_condition * post | experiment_id + category_level_1, cluster = ~experiment_id, data = user_cat_panel %>% filter(!is.na(avg_privacy)))
  
  cat("\nPrivacy score (conditional on visiting, User + Cat FE):\n")
  print(summary(privacy_intensive))
  
  suffix <- paste0("_", gsub(" ", "_", tolower(panel_label)))
  
  etable(extensive_lpm, extensive_lpm_cat, intensive_log, intensive_log_cat, tex = TRUE,
         file = paste0(output_dir, "did_two_part", suffix, ".tex"),
         title = paste0("Two-Part Model: Extensive and Intensive Margins (", panel_label, ")"),
         headers = c("P(Visit)", "P(Visit)", "Log Time|Visit", "Log Time|Visit"),
         extralines = list("_Sample" = c("All", "All", "Visits>0", "Visits>0"), "_FE" = c("User+Site", "User+Cat", "User+Site", "User+Cat")),
         dict = c(experiment_conditioninfo = "Info", experiment_conditionsaliency = "Saliency", postTRUE = "Post",
                  `experiment_conditioninfo:postTRUE` = "Info $\\times$ Post", `experiment_conditionsaliency:postTRUE` = "Saliency $\\times$ Post"),
         notes = paste0("SE clustered by user. Extensive = linear probability model. Intensive = log time conditional on visit > 0. ", panel_label, "."),
         fitstat = c("n", "r2"))
  cat("\nSaved: did_two_part", suffix, ".tex\n", sep = "")
  
  models_robust <- list(ihs_model, ihs_model_cat)
  headers_robust <- c("IHS Time", "IHS Time")
  fe_robust <- c("User+Site", "User+Cat")
  if (!is.null(poisson_model)) { models_robust <- c(models_robust, list(poisson_model)); headers_robust <- c(headers_robust, "Poisson"); fe_robust <- c(fe_robust, "User+Site") }
  if (!is.null(poisson_model_cat)) { models_robust <- c(models_robust, list(poisson_model_cat)); headers_robust <- c(headers_robust, "Poisson"); fe_robust <- c(fe_robust, "User+Cat") }
  
  etable(models_robust, tex = TRUE,
         file = paste0(output_dir, "did_robust", suffix, ".tex"),
         title = paste0("Robust Estimation: IHS and Poisson (", panel_label, ")"),
         headers = headers_robust, extralines = list("_FE" = fe_robust),
         dict = c(experiment_conditioninfo = "Info", experiment_conditionsaliency = "Saliency", postTRUE = "Post",
                  `experiment_conditioninfo:postTRUE` = "Info $\\times$ Post", `experiment_conditionsaliency:postTRUE` = "Saliency $\\times$ Post"),
         notes = paste0("SE clustered by user. IHS = inverse hyperbolic sine. Poisson = PPML. ", panel_label, "."),
         fitstat = c("n", "r2"))
  cat("Saved: did_robust", suffix, ".tex\n", sep = "")
  
  sink(paste0(output_dir, "did_robust_summary", suffix, ".txt"))
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("ANALYSIS 2C: ROBUST DID WITH ZEROS - SUMMARY\n")
  cat("Panel:", panel_label, "\n")
  cat("Percent zeros:", round(pct_zero, 1), "%\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  cat("TWO-PART MODEL:\n"); cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("\n1. Extensive Margin (P(visit > 0), User + Site FE):\n"); print(coef(extensive_lpm))
  cat("\n2. Intensive Margin (Log time | visit > 0, User + Site FE):\n"); print(coef(intensive_log))
  cat("\n\nROBUST ESTIMATORS:\n"); cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("\n3. IHS (User + Site FE):\n"); print(coef(ihs_model))
  if (!is.null(poisson_model)) { cat("\n4. Poisson PPML (User + Site FE):\n"); print(coef(poisson_model)) }
  cat("\n\nPRIVACY SCORE:\n"); cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("\n5. Privacy (conditional on visiting, User + Cat FE):\n"); print(coef(privacy_intensive))
  sink()
  cat("\nSaved: did_robust_summary", suffix, ".txt\n", sep = "")
  
  return(list(panel_data = panel_priv, panel_label = panel_label, pct_zero = pct_zero,
              extensive_lpm = extensive_lpm, extensive_lpm_cat = extensive_lpm_cat,
              intensive_log = intensive_log, intensive_log_cat = intensive_log_cat,
              ihs_model = ihs_model, ihs_model_cat = ihs_model_cat,
              poisson_model = poisson_model, poisson_model_cat = poisson_model_cat,
              privacy_intensive = privacy_intensive, user_cat_panel = user_cat_panel))
}


# ============================================================================
# ANALYSIS 4.2: PRE-REGISTERED WEBSITE CHOICE SPECIFICATIONS
# ============================================================================
# MODIFICATION: Added wt_spec/wt_suffix for Fig 5 regressions (spec i, ii)
# spec_ii_continuous now also uses run_weighted_feols [P3b]
# ============================================================================

run_preregistered_specs <- function(balanced_panel,
                                    full_time_dat,
                                    panel_label = "balanced panel",
                                    privacy_col = "privacy_for_requested_attribute",
                                    output_dir = TABLES_DIR,
                                    figures_dir = FIGURES_DIR,
                                    wt_spec = "unweighted",
                                    wt_suffix = "") {
  
  cat("\n============================================================================\n")
  cat("ANALYSIS 4.2: PRE-REGISTERED WEBSITE CHOICE SPECIFICATIONS\n")
  cat("Panel:", panel_label, "\n")
  cat("Weight specification:", wt_spec, "\n")
  cat("============================================================================\n\n")
  
  panel_data <- balanced_panel %>%
    filter(!is.na(.data[[privacy_col]])) %>%
    mutate(
      privacy_score = .data[[privacy_col]],
      experiment_condition = factor(experiment_condition, levels = c("control", "info", "saliency")),
      experiment_id = as.factor(experiment_id),
      website_aggregated_high_level = as.factor(website_aggregated_high_level),
      post = if (is.factor(post)) as.numeric(as.character(post)) == 1 else as.logical(post)
    )
  
  if (!"log_time" %in% names(panel_data)) {
    panel_data <- panel_data %>% mutate(log_time = log1p(total_time_spent / 60), n_visits = total_visit_count)
  }
  
  panel_data <- panel_data %>%
    group_by(experiment_id) %>%
    mutate(participant_med_privacy = median(privacy_score, na.rm = TRUE), high_privacy = privacy_score > participant_med_privacy) %>%
    ungroup()
  
  global_med_privacy <- median(panel_data$privacy_score, na.rm = TRUE)
  
  # ---- [P1] Join survey weights to panel_data ----
  panel_data <- join_weights(panel_data, wt_spec)
  
  cat("Panel observations:", nrow(panel_data), "\n")
  cat("Unique users:", n_distinct(panel_data$experiment_id), "\n")
  cat("Unique websites:", n_distinct(panel_data$website_aggregated_high_level), "\n")
  cat("Global median privacy score:", round(global_med_privacy, 3), "\n")
  cat("Mean participant-level median:", round(mean(panel_data$participant_med_privacy, na.rm = TRUE), 3), "\n")
  cat("High privacy sites (participant-level median):", sum(panel_data$high_privacy, na.rm = TRUE), "\n\n")
  
  # --------------------------------------------------------------------------
  # SPECIFICATION (i): Aggregate Effect on Website Choices
  # --------------------------------------------------------------------------
  
  cat("SPECIFICATION (i): Aggregate Effect on Website Choices\n")
  cat("Model: y_ijt = T_i + Post·T_i + η_j + η_i + ε_ijt\n")
  cat("------------------------------------------------------------\n\n")
  
  # ---- [P2] spec_i_time, spec_i_visits: run_weighted_feols ----
  spec_i_time <- run_weighted_feols(
    log_time ~ experiment_condition * post | experiment_id + website_aggregated_high_level,
    data = panel_data, cluster_var = ~experiment_id, wt_spec = wt_spec)
  
  spec_i_visits <- NULL
  if ("n_visits" %in% names(panel_data) || "total_visit_count" %in% names(panel_data)) {
    visit_col <- if ("n_visits" %in% names(panel_data)) "n_visits" else "total_visit_count"
    spec_i_visits <- run_weighted_feols(
      as.formula(paste(visit_col, "~ experiment_condition * post | experiment_id + website_aggregated_high_level")),
      data = panel_data, cluster_var = ~experiment_id, wt_spec = wt_spec)
  }
  
  cat("Specification (i) - Log Time:\n"); print(summary(spec_i_time))
  if (!is.null(spec_i_visits)) { cat("\nSpecification (i) - Number of Visits:\n"); print(summary(spec_i_visits)) }
  
  # --------------------------------------------------------------------------
  # SPECIFICATION (ii): Triple-Difference with Privacy Heterogeneity
  # --------------------------------------------------------------------------
  
  cat("\nSPECIFICATION (ii): Triple-Difference with Privacy Heterogeneity\n")
  cat("Model: y_ijt = P_ij·T_i·Post + ... + η_j + η_i + ε_ijt\n")
  cat("------------------------------------------------------------\n\n")
  
  # Using continuous privacy score
  # ---- [P3b] spec_ii_continuous: run_weighted_feols ----
  spec_ii_continuous <- run_weighted_feols(
    log_time ~ privacy_score * experiment_condition * post
    | experiment_id + website_aggregated_high_level,
    data = panel_data,
    cluster_var = ~experiment_id,
    wt_spec = wt_spec
  )
  
  # ---- [P3] spec_ii_discrete, spec_ii_discrete_visits: run_weighted_feols ----
  spec_ii_discrete <- run_weighted_feols(
    log_time ~ high_privacy * experiment_condition * post | experiment_id + website_aggregated_high_level,
    data = panel_data, cluster_var = ~experiment_id, wt_spec = wt_spec)
  
  spec_ii_discrete_visits <- NULL
  if ("n_visits" %in% names(panel_data) || "total_visit_count" %in% names(panel_data)) {
    visit_col <- if ("n_visits" %in% names(panel_data)) "n_visits" else "total_visit_count"
    spec_ii_discrete_visits <- run_weighted_feols(
      as.formula(paste(visit_col, "~ high_privacy * experiment_condition * post | experiment_id + website_aggregated_high_level")),
      data = panel_data, cluster_var = ~experiment_id, wt_spec = wt_spec)
  }
  
  cat("Specification (ii) - Continuous Privacy Score:\n"); print(summary(spec_ii_continuous))
  cat("\nSpecification (ii) - Discrete Privacy (High vs Low) - Log Time:\n"); print(summary(spec_ii_discrete))
  if (!is.null(spec_ii_discrete_visits)) { cat("\nSpecification (ii) - Discrete Privacy (High vs Low) - N Visits:\n"); print(summary(spec_ii_discrete_visits)) }
  
  triple_diff_coefs <- broom::tidy(spec_ii_discrete) %>% filter(grepl("high_privacyTRUE.*:post", term))
  cat("\nKey Triple-Difference Coefficients (Post × Treatment × High Privacy):\n"); print(triple_diff_coefs)
  
  # --------------------------------------------------------------------------
  # SPECIFICATION (iii): HHI Market Competition
  # NOTE: Spec (iii) is NOT weighted (market-level aggregates)
  # --------------------------------------------------------------------------
  
  cat("\nSPECIFICATION (iii): Market Competition (HHI)\n")
  cat("Model: y_omt = T_o + Post·T_o + η_m + ε_omt\n")
  cat("------------------------------------------------------------\n\n")
  
  hhi_panel <- full_time_dat %>%
    filter(!is.na(.data[[privacy_col]]) & !is.na(category_level_1) & category_level_1 != "") %>%
    group_by(experiment_condition, category_level_1, post) %>%
    summarise(total_time_market = sum(time_spent, na.rm = TRUE), n_users = n_distinct(experiment_id), n_sites = n_distinct(website_aggregated_high_level), .groups = 'drop') %>%
    left_join(
      full_time_dat %>%
        filter(!is.na(.data[[privacy_col]]) & !is.na(category_level_1) & category_level_1 != "") %>%
        group_by(experiment_condition, category_level_1, post, website_aggregated_high_level) %>%
        summarise(site_time = sum(time_spent, na.rm = TRUE), .groups = 'drop') %>%
        group_by(experiment_condition, category_level_1, post) %>%
        mutate(total_cat_time = sum(site_time, na.rm = TRUE), share = site_time / total_cat_time) %>%
        summarise(hhi = sum(share^2, na.rm = TRUE), top1_share = max(share, na.rm = TRUE),
                  top3_share = sum(sort(share, decreasing = TRUE)[1:min(3, n())], na.rm = TRUE), .groups = 'drop'),
      by = c("experiment_condition", "category_level_1", "post")
    ) %>%
    mutate(experiment_condition = factor(experiment_condition, levels = c("control", "info", "saliency")),
           category_level_1 = as.factor(category_level_1), post = as.logical(post))
  
  cat("HHI panel:", nrow(hhi_panel), "observations\n")
  
  spec_iii_hhi <- feols(hhi ~ experiment_condition * post | category_level_1, cluster = ~category_level_1, data = hhi_panel)
  spec_iii_top1 <- feols(top1_share ~ experiment_condition * post | category_level_1, cluster = ~category_level_1, data = hhi_panel)
  spec_iii_nsites <- feols(n_sites ~ experiment_condition * post | category_level_1, cluster = ~category_level_1, data = hhi_panel)
  
  cat("Specification (iii) - HHI:\n"); print(summary(spec_iii_hhi))
  cat("\nSpecification (iii) - Top Site Share:\n"); print(summary(spec_iii_top1))
  cat("\nSpecification (iii) - Number of Sites:\n"); print(summary(spec_iii_nsites))
  
  # --------------------------------------------------------------------------
  # Output Tables
  # --------------------------------------------------------------------------
  
  suffix <- paste0("_", gsub(" ", "_", tolower(panel_label)))
  
  # ---- [P4] Output filenames add wt_suffix ----
  spec_i_models <- list(spec_i_time)
  spec_i_headers <- c("Log Time")
  if (!is.null(spec_i_visits)) { spec_i_models <- c(spec_i_models, list(spec_i_visits)); spec_i_headers <- c(spec_i_headers, "N Visits") }
  
  etable(spec_i_models,
         title = paste0("Pre-Registered Spec (i): Aggregate Effect on Website Choices (", panel_label, ")"),
         headers = spec_i_headers,
         dict = c(experiment_conditioninfo = "Info", experiment_conditionsaliency = "Saliency", postTRUE = "Post",
                  `experiment_conditioninfo:postTRUE` = "Info $\\times$ Post", `experiment_conditionsaliency:postTRUE` = "Saliency $\\times$ Post"),
         notes = paste0("SE clustered by user. User and website FE. From pre-registration Section 4.2.",
                        if (wt_spec != "unweighted") paste0(" Weights: ", wt_spec, ".") else ""),
         tex = TRUE, file = paste0(output_dir, "preregistered_spec_i", suffix, wt_suffix, ".tex"), replace = TRUE, fitstat = c("n", "r2"))
  cat("\nSaved: preregistered_spec_i", suffix, wt_suffix, ".tex\n", sep = "")
  
  etable(spec_ii_continuous, spec_ii_discrete,
         title = paste0("Pre-Registered Spec (ii): Privacy Heterogeneity Triple-Diff (", panel_label, ")"),
         headers = c("Continuous Privacy", "Discrete Privacy"),
         dict = c(experiment_conditioninfo = "Info", experiment_conditionsaliency = "Saliency", postTRUE = "Post",
                  high_privacyTRUE = "High Privacy", privacy_score = "Privacy Score"),
         notes = paste0("SE clustered by user. User and website FE. High privacy = above participant-level median. Global median = ", round(global_med_privacy, 3), ".",
                        if (wt_spec != "unweighted") paste0(" Weights: ", wt_spec, ".") else ""),
         tex = TRUE, file = paste0(output_dir, "preregistered_spec_ii", suffix, wt_suffix, ".tex"), replace = TRUE, fitstat = c("n", "r2"))
  cat("Saved: preregistered_spec_ii", suffix, wt_suffix, ".tex\n", sep = "")
  
  etable(spec_iii_hhi, spec_iii_top1, spec_iii_nsites,
         title = paste0("Pre-Registered Spec (iii): Market Competition Effects (", panel_label, ")"),
         headers = c("HHI", "Top Site Share", "N Sites"),
         dict = c(experiment_conditioninfo = "Info", experiment_conditionsaliency = "Saliency", postTRUE = "Post",
                  `experiment_conditioninfo:postTRUE` = "Info $\\times$ Post", `experiment_conditionsaliency:postTRUE` = "Saliency $\\times$ Post"),
         notes = "SE clustered by category. Category FE. From pre-registration Section 4.2.",
         tex = TRUE, file = paste0(output_dir, "preregistered_spec_iii", suffix, ".tex"), replace = TRUE, fitstat = c("n", "r2"))
  cat("Saved: preregistered_spec_iii", suffix, ".tex\n", sep = "")
  
  # --------------------------------------------------------------------------
  # Coefficient Plots
  # --------------------------------------------------------------------------
  
  spec_i_coefs <- broom::tidy(spec_i_time) %>% filter(grepl(":post", term)) %>%
    mutate(outcome = "Log Time", Treatment = case_when(grepl("info", term) ~ "Information", grepl("saliency", term) ~ "Saliency"),
           Treatment = factor(Treatment, levels = c("Saliency", "Information")))
  if (!is.null(spec_i_visits)) {
    spec_i_coefs <- bind_rows(spec_i_coefs,
                              broom::tidy(spec_i_visits) %>% filter(grepl(":post", term)) %>%
                                mutate(outcome = "N Visits", Treatment = case_when(grepl("info", term) ~ "Information", grepl("saliency", term) ~ "Saliency"),
                                       Treatment = factor(Treatment, levels = c("Saliency", "Information"))))
  }
  
  p_spec_i <- ggplot(spec_i_coefs, aes(x = Treatment, y = estimate, color = Treatment)) +
    geom_hline_zero(linetype = "dashed") +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = ERRORBAR_WIDTH, linewidth = LINE_WIDTH) +
    geom_point(size = POINT_SIZE) + facet_wrap(~outcome, scales = "free_y") +
    scale_color_treatment() + theme_privacy_experiment() + labs(x = "", y = "Coefficient (95% CI)", color = "Treatment")
  ggsave(paste0(figures_dir, "preregistered_spec_i", suffix, wt_suffix, ".png"), p_spec_i, width = 8, height = 5, dpi = 300)
  
  # Triple interaction coefficients
  triple_interaction_coefs <- broom::tidy(spec_ii_discrete) %>%
    filter(grepl("high_privacyTRUE:experiment_condition.*:postTRUE", term)) %>%
    mutate(outcome = "Log Time", Treatment = case_when(grepl("info", term) ~ "Information", grepl("saliency", term) ~ "Saliency"),
           Treatment = factor(Treatment, levels = c("Saliency", "Information")))
  if (!is.null(spec_ii_discrete_visits)) {
    triple_interaction_coefs <- bind_rows(triple_interaction_coefs,
                                          broom::tidy(spec_ii_discrete_visits) %>%
                                            filter(grepl("high_privacyTRUE:experiment_condition.*:postTRUE", term)) %>%
                                            mutate(outcome = "N Visits", Treatment = case_when(grepl("info", term) ~ "Information", grepl("saliency", term) ~ "Saliency"),
                                                   Treatment = factor(Treatment, levels = c("Saliency", "Information"))))
  }
  
  p_triple_interaction <- ggplot(triple_interaction_coefs, aes(x = Treatment, y = estimate, color = Treatment)) +
    geom_hline_zero(linetype = "dashed") +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), width = ERRORBAR_WIDTH, linewidth = LINE_WIDTH) +
    geom_point(size = POINT_SIZE) + facet_wrap(~outcome, scales = "free_y") +
    scale_color_treatment() + theme_privacy_experiment() + labs(x = "", y = "Coefficient (95% CI)", color = "Treatment")
  ggsave(paste0(figures_dir, "preregistered_triple_interaction", suffix, wt_suffix, ".png"), p_triple_interaction, width = 8, height = 5, dpi = 300)
  
  # Spec iii coef plot (no wt_suffix)
  spec_iii_coefs <- bind_rows(
    broom::tidy(spec_iii_hhi) %>% filter(grepl(":post", term)) %>% mutate(outcome = "HHI"),
    broom::tidy(spec_iii_top1) %>% filter(grepl(":post", term)) %>% mutate(outcome = "Top Site Share"),
    broom::tidy(spec_iii_nsites) %>% filter(grepl(":post", term)) %>% mutate(outcome = "N Sites")
  ) %>% mutate(Condition = case_when(grepl("info", term) ~ "Information", grepl("saliency", term) ~ "Saliency"),
               Condition = factor(Condition, levels = c("Saliency", "Information")))
  
  p_spec_iii <- ggplot(spec_iii_coefs, aes(x = outcome, y = estimate, color = Condition)) +
    geom_hline_zero(linetype = "dashed") +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                  width = ERRORBAR_WIDTH, linewidth = LINE_WIDTH, position = position_dodge(width = DODGE_WIDTH_2)) +
    geom_point(size = POINT_SIZE, position = position_dodge(width = DODGE_WIDTH_2)) +
    scale_color_treatment() + theme_privacy_experiment() + labs(x = "", y = "Coefficient (95% CI)", color = NULL)
  ggsave(paste0(figures_dir, "preregistered_spec_iii", suffix, ".png"), p_spec_iii, width = 10, height = 5, dpi = 300)
  
  cat("\nSaved specification coefficient plots.\n")
  
  # --------------------------------------------------------------------------
  # Summary Output
  # --------------------------------------------------------------------------
  
  sink(paste0(output_dir, "preregistered_specs_summary", suffix, wt_suffix, ".txt"))
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("ANALYSIS 4.2: PRE-REGISTERED WEBSITE CHOICE SPECIFICATIONS\n")
  cat("Panel:", panel_label, "\n")
  cat("Weight specification:", wt_spec, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  cat("PANEL STRUCTURE:\n")
  cat("  Observations:", nrow(panel_data), "\n")
  cat("  Unique users:", n_distinct(panel_data$experiment_id), "\n")
  cat("  Unique websites:", n_distinct(panel_data$website_aggregated_high_level), "\n")
  cat("  Global median privacy score:", round(global_med_privacy, 3), "\n\n")
  cat("SPECIFICATION (i): Aggregate Effect\n"); cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("\nLog Time:\n"); print(broom::tidy(spec_i_time) %>% filter(grepl(":post", term)))
  if (!is.null(spec_i_visits)) { cat("\nN Visits:\n"); print(broom::tidy(spec_i_visits) %>% filter(grepl(":post", term))) }
  cat("\n\nSPECIFICATION (ii): Triple-Difference\n"); cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("\nDiscrete Privacy Coefficients:\n"); print(broom::tidy(spec_ii_discrete) %>% filter(grepl("high_privacy", term)))
  cat("\n\nSPECIFICATION (iii): Market Competition\n"); cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("\nHHI:\n"); print(broom::tidy(spec_iii_hhi) %>% filter(grepl(":post", term)))
  cat("\nTop Site Share:\n"); print(broom::tidy(spec_iii_top1) %>% filter(grepl(":post", term)))
  cat("\nN Sites:\n"); print(broom::tidy(spec_iii_nsites) %>% filter(grepl(":post", term)))
  sink()
  cat("\nSaved: preregistered_specs_summary", suffix, wt_suffix, ".txt\n", sep = "")
  
  return(list(
    panel_data = panel_data, panel_label = panel_label, hhi_panel = hhi_panel,
    global_med_privacy = global_med_privacy,
    spec_i_time = spec_i_time, spec_i_visits = spec_i_visits,
    spec_ii_continuous = spec_ii_continuous, spec_ii_discrete = spec_ii_discrete,
    spec_ii_discrete_visits = spec_ii_discrete_visits, triple_diff_coefs = triple_diff_coefs,
    spec_iii_hhi = spec_iii_hhi, spec_iii_top1 = spec_iii_top1, spec_iii_nsites = spec_iii_nsites,
    plots = list(spec_i = p_spec_i, triple_interaction = p_triple_interaction, spec_iii = p_spec_iii)
  ))
}


# ============================================================================
# SUMMARY FIGURE: PRIVACY REGRESSIONS ACROSS PANELS
# ============================================================================
# NOTE: This function is NOT modified for weighting (not in EC paper Fig 5-6)
# ============================================================================

plot_privacy_summary <- function(balanced_results, full_panel_results, unbalanced_results,
                                 output_dir = TABLES_DIR, figures_dir = FIGURES_DIR) {
  
  cat("\n============================================================================\n")
  cat("SUMMARY: PRIVACY REGRESSIONS ACROSS PANEL SPECIFICATIONS\n")
  cat("============================================================================\n\n")
  
  extract_privacy_coefs <- function(did_result, panel_name) {
    coefs_list <- list()
    if (!is.null(did_result$did_privacy_1)) { coefs_list$privacy_1 <- broom::tidy(did_result$did_privacy_1) %>% filter(grepl(":post", term, ignore.case = TRUE)) %>% mutate(model = "Avg Privacy\n(User+Cat FE)", panel = panel_name) }
    if (!is.null(did_result$did_privacy_2)) { coefs_list$privacy_2 <- broom::tidy(did_result$did_privacy_2) %>% filter(grepl(":post", term, ignore.case = TRUE)) %>% mutate(model = "Avg Privacy\n(User FE)", panel = panel_name) }
    if (!is.null(did_result$did_privacy_3)) { coefs_list$privacy_3 <- broom::tidy(did_result$did_privacy_3) %>% filter(grepl(":post", term, ignore.case = TRUE)) %>% mutate(model = "Site Privacy\n(User+Site FE)", panel = panel_name) }
    if (!is.null(did_result$did_privacy_4)) { coefs_list$privacy_4 <- broom::tidy(did_result$did_privacy_4) %>% filter(grepl(":post", term, ignore.case = TRUE)) %>% mutate(model = "Site Privacy\n(User+Cat FE)", panel = panel_name) }
    bind_rows(coefs_list)
  }
  
  all_coefs <- bind_rows(
    extract_privacy_coefs(balanced_results, "Balanced\n(Baseline)"),
    extract_privacy_coefs(full_panel_results, "Full Panel\n(All Sites)"),
    extract_privacy_coefs(unbalanced_results, "Unbalanced")
  ) %>% mutate(treatment = case_when(grepl("info", term, ignore.case = TRUE) ~ "Information", grepl("saliency", term, ignore.case = TRUE) ~ "Saliency", TRUE ~ "Other")) %>%
    filter(treatment != "Other")
  
  cat("Extracted", nrow(all_coefs), "coefficients across panels\n")
  
  all_coefs <- all_coefs %>% mutate(
    panel = factor(panel, levels = c("Balanced\n(Baseline)", "Full Panel\n(All Sites)", "Unbalanced")),
    model = factor(model, levels = c("Avg Privacy\n(User+Cat FE)", "Avg Privacy\n(User FE)", "Site Privacy\n(User+Site FE)", "Site Privacy\n(User+Cat FE)")),
    treatment = factor(treatment, levels = c("Information", "Saliency")))
  
  p_privacy_by_model <- ggplot(all_coefs, aes(x = panel, y = estimate, ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error, color = treatment, shape = treatment)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_pointrange(position = position_dodge(width = 0.4), size = 0.6) +
    facet_wrap(~model, scales = "free_y", nrow = 1) +
    scale_color_manual(values = c("Information" = "#2CA02C", "Saliency" = "#1F77B4"), name = "Treatment") +
    scale_shape_manual(values = c("Information" = 16, "Saliency" = 17), name = "Treatment") +
    labs(title = "Privacy Treatment Effects Across Panel Specifications", subtitle = "Treatment × Post coefficients (95% CI)", x = "Panel Type", y = "Coefficient") +
    theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold", size = 13), axis.text.x = element_text(angle = 45, hjust = 1, size = 9), legend.position = "bottom", strip.text = element_text(size = 9))
  ggsave(paste0(figures_dir, "privacy_summary_by_model.png"), p_privacy_by_model, width = 14, height = 5, dpi = 300)
  
  summary_table <- all_coefs %>% mutate(sig_level = case_when(abs(estimate/std.error) > 2.576 ~ "***", abs(estimate/std.error) > 1.96 ~ "**", abs(estimate/std.error) > 1.645 ~ "*", TRUE ~ "")) %>%
    select(panel, model, treatment, estimate, std.error, sig_level) %>% arrange(panel, model, treatment)
  
  cat("\nSUMMARY TABLE:\n"); print(summary_table, n = 50)
  
  sink(paste0(output_dir, "privacy_summary_across_panels.txt"))
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("SUMMARY: PRIVACY TREATMENT EFFECTS ACROSS PANEL SPECIFICATIONS\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")
  cat("Significance: *** p<0.01, ** p<0.05, * p<0.10\n\n")
  print(summary_table, n = 50)
  sink()
  
  return(list(all_coefs = all_coefs, summary_table = summary_table, plots = list(by_model = p_privacy_by_model)))
}