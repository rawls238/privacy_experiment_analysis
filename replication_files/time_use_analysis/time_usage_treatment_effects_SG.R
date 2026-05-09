library(tidyverse)
library(lubridate)
library(jsonlite)
library(data.table)
library(lfe)
library(tidyr)
library(tibble)
library(rlang)
library(xtable)
library(stargazer)
library(fixest)

#WD <- "/Users/guyaridor/Dropbox/Privacy-Experiment/spring2025experiment"
#WD <- "/Users/sggold/Dropbox/Shared-Project-Folders/Privacy-Experiment/spring2025experiment"
WD <- "/Users/marklee/Dropbox/spring2025experiment"

setwd(WD)

CONSTRUCT_FROM_SCRATCH <- FALSE
#flag for if we want to use the population averages to construct scores or individual measures
individual_weights <- FALSE
FIGURES_DIR <- "results/time_usage_results/population/"
TABLES_DIR <- "results/tables/population/"

# ============================================================================
# [MOD 1] Survey Weighting Configuration
# ============================================================================
# Set to one of: "unweighted", "weight_census", "weight_pew", "weight_combined", "all".
# "all" loops over all 4 specs at the bottom (MOD 3) without re-loading data.
WEIGHT_SPEC <- "all"
OUTPUT_SUFFIX <- if (WEIGHT_SPEC == "unweighted") "" else paste0("_", WEIGHT_SPEC)
cat("=== Weight specification:", WEIGHT_SPEC, "===\n")
cat("=== Output suffix:", OUTPUT_SUFFIX, "===\n\n")
# ============================================================================

## load helper functions ##
source("code/utils/values.R")
source("code/utils/time_usage_helpers.R")
source("code/utils/info_acq_helpers.R")

# ============================================================================
# [MOD 2] Survey Weighting Helper Functions
# ============================================================================
# These are used by run_assortment_analysis() and run_preregistered_specs()
# in analysis_assortment_did.R for EC paper Fig 5-6 regressions

#' Join survey weights to a data frame
#' @param df Data frame with experiment_id column
#' @param wt_spec Weight specification string
#' @param weight_col Name for the output weight column (default: "survey_weight")
#' @return df with survey_weight column added (or unchanged if unweighted)
join_weights <- function(df, wt_spec, weight_col = "survey_weight") {
  if (wt_spec == "unweighted") return(df)
  
  weights_path <- "data/Survey/individual_level_weights.csv"
  if (!file.exists(weights_path)) {
    stop("Weight file not found: ", weights_path)
  }
  
  weights_df <- read.csv(weights_path, stringsAsFactors = FALSE)
  
  if (!wt_spec %in% names(weights_df)) {
    stop("Weight column '", wt_spec, "' not found in weights file. ",
         "Available columns: ", paste(names(weights_df), collapse = ", "))
  }
  
  weights_df <- weights_df %>%
    select(experiment_id, !!weight_col := all_of(wt_spec))
  
  n_before <- nrow(df)
  df <- df %>% left_join(weights_df, by = "experiment_id")
  n_matched <- sum(!is.na(df[[weight_col]]))
  
  cat("  Weights joined:", n_matched, "/", n_before, "rows matched (",
      round(100 * n_matched / n_before, 1), "%)\n")
  
  return(df)
}

#' Run feols with optional survey weights
#' @param fml Formula for feols
#' @param data Data frame
#' @param cluster_var Cluster variable (e.g., ~experiment_id)
#' @param wt_spec Weight specification string
#' @return fixest model object
run_weighted_feols <- function(fml, data, cluster_var, wt_spec = WEIGHT_SPEC) {
  if (wt_spec == "unweighted" || !"survey_weight" %in% names(data)) {
    feols(fml, cluster = cluster_var, data = data)
  } else {
    feols(fml, cluster = cluster_var, data = data, weights = ~survey_weight)
  }
}
# ============================================================================

## this function constructs a balanced panel of websites based on the websites visited in the weeks_to_include
## so if weeks_to_include is c(1, 2) then we include a balanced panel of websites that users visited in both weeks 1 and 2 (imputing zeros for the later weeks)
## if we do c(1, 2, 3, 4, 5) then this requires non-zero usage every week for a (website, user) tuple to be included
get_balanced_panel <- function(dat,
                               index_var = "weeks_since_intervention",
                               values_to_include = c(-2, -1),
                               all_values = c(-2, -1, 0, 1, 2, 3),
                               lower_pct = 0.0,
                               upper_pct = 1.0) {
  # choose index variable
  idx <- rlang::sym(index_var)
  
  ## aggregate by chosen index variable
  dat <- dat %>%
    group_by(!!idx, experiment_id, experiment_condition, website_aggregated_high_level) %>%
    mutate(
      total_time_spent = sum(time_spent),
      total_visit_count = sum(visit_count)
    ) %>%
    slice(1) %>%
    ungroup()
  
  # winsorize
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
  
  # get websites with data in the included values
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
  
  # experiment-level data (only select columns that exist)
  experiment_data <- dat %>%
    select(
      intersect(
        c("experiment_id", "block_by_wave", "experiment_condition",
          "q1_html_key", "q2_html_key", "q1_belief", "q2_belief"),
        names(dat)
      )
    ) %>%
    distinct()
  
  website_privacy_data <- dat %>%
    select(
      intersect(
        c("experiment_id", "website_aggregated_high_level",
          "privacy_for_requested_attribute", "privacy_full_beta_p", "privacy_exist",
          "category_level_1", "category_level_2", "category_level_3",
          "q1_realized_privacy", "q2_realized_privacy"),
        names(dat)
      )
    ) %>%
    distinct()
  
  # balanced panel construction
  balanced_panel <- first_period_websites %>%
    expand_grid(!!idx := all_values) %>%
    left_join(dat, by = c("experiment_id", "website_aggregated_high_level", index_var)) %>%
    mutate(
      total_visit_count = ifelse(is.na(total_visit_count), 0, total_visit_count),
      total_time_spent  = ifelse(is.na(total_time_spent), 0, total_time_spent)
    ) %>%
    left_join(experiment_data, by = c("experiment_id")) %>%
    left_join(website_privacy_data, by = c("experiment_id", "website_aggregated_high_level"))
  
  # Helper function to safely coalesce columns that may or may not exist
  safe_coalesce <- function(df, col_name) {
    col_x <- paste0(col_name, ".x")
    col_y <- paste0(col_name, ".y")
    if (col_x %in% names(df) && col_y %in% names(df)) {
      coalesce(df[[col_x]], df[[col_y]])
    } else if (col_x %in% names(df)) {
      df[[col_x]]
    } else if (col_y %in% names(df)) {
      df[[col_y]]
    } else if (col_name %in% names(df)) {
      df[[col_name]]
    } else {
      NA
    }
  }
  
  # Apply coalesce for columns that exist
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
  
  # Add category_level_2 and category_level_3 only if they exist
  if ("category_level_2.x" %in% names(balanced_panel) || "category_level_2.y" %in% names(balanced_panel) || "category_level_2" %in% names(balanced_panel)) {
    balanced_panel <- balanced_panel %>%
      mutate(category_level_2 = safe_coalesce(pick(everything()), "category_level_2"))
  }
  if ("category_level_3.x" %in% names(balanced_panel) || "category_level_3.y" %in% names(balanced_panel) || "category_level_3" %in% names(balanced_panel)) {
    balanced_panel <- balanced_panel %>%
      mutate(category_level_3 = safe_coalesce(pick(everything()), "category_level_3"))
  }
  
  balanced_panel <- balanced_panel %>%
    group_by(experiment_id, website_aggregated_high_level, !!idx) %>%
    slice(1) %>%
    ungroup()
  
  # Select only columns that exist
  select_cols <- c("experiment_id", "website_aggregated_high_level", index_var,
                   "total_visit_count", "total_time_spent", "block_by_wave",
                   "experiment_condition", "q1_realized_privacy", "q2_realized_privacy",
                   "privacy_for_requested_attribute", "privacy_exist", "privacy_full_beta_p",
                   "q1_html_key", "q2_html_key", "q1_belief", "q2_belief",
                   "category_level_1", "category_level_2", "category_level_3")
  select_cols <- intersect(select_cols, names(balanced_panel))
  balanced_panel <- balanced_panel %>% select(all_of(select_cols))
  
  # high_privacy flag
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
  
  med <- median(balanced_panel$privacy_for_requested_attribute, na.rm=T)
  balanced_panel <- balanced_panel %>%
    left_join(individual_website, by = c("experiment_id", "website_aggregated_high_level")) %>%
    mutate(has_visits = total_visit_count > 0, privacy_discretized_median = privacy_for_requested_attribute > med)
  
  return(balanced_panel)
}



if (CONSTRUCT_FROM_SCRATCH) {
  survey_dat <- read.csv("data/Survey/survey_merged_final.csv", stringsAsFactors = FALSE)
  
  # Remove people with invalid demographic field.
  survey_dat <- survey_dat %>%
    filter(
      # Remove invalid age
      Age != 5,
      # Must have at least one race selected
      (RaceSimple_1 == 1 | RaceSimple_2 == 1 | RaceSimple_3 == 1 | 
         RaceSimple_4 == 1 | RaceSimple_5 == 1 | RaceSimple_6 == 1),
      # Must have favoritewebsite
      !is.na(favoritewebsite) & trimws(favoritewebsite) != "",
      # Must have conjcat_conjCategory
      !is.na(conjcat_conjCategory) & trimws(conjcat_conjCategory) != ""
    )
  #cat("Survey data cleaned: ", nrow(survey_dat), " -> ", nrow(survey_dat_clean), " rows\n")
  
  # apply the same 
  time_data <- get_clean_time_data()
  
  personalized_info <- get_personalized_info_only() %>% select(email, experiment_id, q1_feature, q1_field, q2_feature, q2_field)
  personalized_info <- personalized_info %>% mutate(q1_html_key = paste(q1_feature, q1_field, sep="-"), q2_html_key = paste(q2_feature, q2_field, sep="-"))
  personalized_info <- personalized_info %>% select(email, experiment_id, q1_html_key, q2_html_key)
  
  beliefs <- get_privacy_beliefs() %>%
    rename(email = emailid) %>%
    mutate(email = tolower(email))
  
  beliefs_long <- beliefs %>%
    pivot_longer(-email, names_to = "beliefs_key_full", values_to = "belief_value") %>%
    mutate(
      html_key = tolower(sub("^beliefs_", "", beliefs_key_full))  # lowercase html_key
    ) %>%
    select(email, html_key, belief_value)
  
  personalized_info <- personalized_info %>%
    mutate(
      email = tolower(email),
      q1_html_key = tolower(q1_html_key),
      q2_html_key = tolower(q2_html_key)
    )
  
  personalized_beliefs <- personalized_info %>%
    left_join(beliefs_long, by = c("email", "q1_html_key" = "html_key")) %>%
    rename(q1_belief = belief_value) %>%
    left_join(beliefs_long, by = c("email", "q2_html_key" = "html_key")) %>%
    rename(q2_belief = belief_value) %>%
    select(email, experiment_id, q1_html_key, q2_html_key, q1_belief, q2_belief)
  
  meta_data <- read.csv("data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
  meta_data <- meta_data %>%
    mutate(wave_id = ifelse(wave_id == 3, 2, wave_id)) %>%
    mutate(block_by_wave = paste(wave_id,block_idx,sep = '_'))
  
  # Note here the data cleaning is not sufficient
  experiment_users <- meta_data %>% filter(experiment_condition != "" & !(experiment_id %in% BAD_USERS))
  
  personalized_beliefs <- personalized_beliefs %>% left_join(experiment_users %>% select(experiment_id, block_by_wave), by="experiment_id")
  
  time_data_with_beliefs <- time_data %>% left_join(personalized_beliefs, by="experiment_id")
  time_data_with_beliefs = high_level_aggregate(time_data_with_beliefs) 
  
  # 3. map domain
  domain_classfication <- get_domain_classification()
  
  
  # 4. high-level aggregation
  time_data_2_domain_class_high_level = time_data_with_beliefs %>% left_join(domain_classfication %>% select(name_aggregated_high_level, categories, category_length, category_level_1), by=c("website_aggregated_high_level" = "name_aggregated_high_level"))
  
  # 5. map privacy_info -- check whether we need this twice
  privacy_info <- get_privacy_info_wide()
  time_data_2_domain_class_privacy = map_privacy_data(time_data_2_domain_class_high_level, privacy_info)
  
  if (individual_weights){
    
    privacy_weights <- get_privacy_attribute_weights_by_individual()
    
  } else {
    
    privacy_weights <- get_privacy_attribute_weights_population_by_user()
    
  }
  
  # apply inner join to remove invalid records, as our conjoint has better cleaning rule
  time_data_2_domain_class_privacy_weights<- time_data_2_domain_class_privacy %>% inner_join(privacy_weights, by="experiment_id")
  
  # check websites that have privacy_info but no domain_classification
  check_3 <- time_data_2_domain_class_privacy_weights |>
    select(website, website_aggregated, privacy_exist, category_length) |>
    filter(privacy_exist == TRUE & is.na(category_length)) |>
    distinct()
  
  # check mapping result
  check = time_data_2_domain_class_privacy_weights |>
    select(website, website_aggregated, website_aggregated_high_level, privacy_exist, category_level_1) |>
    unique()
  
  privacy_cols <- grep("^p_", names(time_data_2_domain_class_privacy_weights), value = TRUE)
  beta_cols    <- grep("^beta_", names(time_data_2_domain_class_privacy_weights), value = TRUE)
  
  privacy_map <- setNames(privacy_cols, sub("^p_", "", privacy_cols))   
  beta_map    <- setNames(beta_cols,    sub("^beta_", "", beta_cols))         
  
  get_realized_from_map <- function(df, key_vec, map_vec) {
    vapply(seq_along(key_vec), function(i) {
      key <- key_vec[i]
      if (!is.na(key) && nzchar(key) && key %in% names(map_vec)) {
        df[[ map_vec[[key]] ]][i]
      } else {
        NA
      }
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
        # both positive
        q1_preference_weight > 0 & q2_preference_weight > 0 &
          (q1_preference_weight + q2_preference_weight) != 0 ~
          ( q1_preference_weight / ( q1_preference_weight +  q2_preference_weight)) * ( q1_realized_privacy) +
          ( q2_preference_weight / ( q1_preference_weight +  q2_preference_weight)) * ( q2_realized_privacy),
        
        # q1 positive, q2 negative
        q1_preference_weight > 0 & q2_preference_weight < 0 &
          (q1_preference_weight + -q2_preference_weight) != 0 ~
          ( q1_preference_weight / ( q1_preference_weight + -q2_preference_weight)) * ( q1_realized_privacy) +
          (-q2_preference_weight / ( q1_preference_weight + -q2_preference_weight)) * (1 - q2_realized_privacy),
        
        # q1 negative, q2 positive
        q1_preference_weight < 0 & q2_preference_weight > 0 &
          (-q1_preference_weight + q2_preference_weight) != 0 ~
          (-q1_preference_weight / (-q1_preference_weight +  q2_preference_weight)) * (1 - q1_realized_privacy) +
          ( q2_preference_weight / (-q1_preference_weight +  q2_preference_weight)) * ( q2_realized_privacy),
        
        # both negative
        q1_preference_weight < 0 & q2_preference_weight < 0 &
          (-q1_preference_weight + -q2_preference_weight) != 0 ~
          (-q1_preference_weight / (-q1_preference_weight + -q2_preference_weight)) * (1 - q1_realized_privacy) +
          (-q2_preference_weight / (-q1_preference_weight + -q2_preference_weight)) * (1 - q2_realized_privacy),
        
        TRUE ~ NA_real_
      )
    )
  full_time_dat <- compute_privacy_scores(full_time_dat)
  full_time_dat <- compute_exposed_privacy_scores(full_time_dat)
  
  # calculate top 500 site's traffic (website_aggregated_high_level)
  top_500 = full_time_dat |> 
    select(website_aggregated_high_level) |> 
    count(website_aggregated_high_level) |> 
    arrange(desc(n)) |> 
    slice_head(n = 500)
  
  # top 500 site (195763) / num of partcipants (289676) = traffic (0.6758)
  sum(top_500$n) / nrow(time_data_with_beliefs) # 0.6757999 (XX%)
  
  # among our participants, YY% of leisure time is spent on a site with privacy information 
  
  full_time_dat_leisure <- full_time_dat |>
    filter(!str_detect(website, str_c(SURVEY_WEBSITES, collapse = "|"))) 
  
  top_500 = full_time_dat_leisure |> 
    select(website_aggregated_high_level) |> 
    count(website_aggregated_high_level) |> 
    arrange(desc(n)) |> 
    slice_head(n = 500)
  sum((full_time_dat_leisure %>% filter(privacy_exist))$time_spent) / sum(full_time_dat$time_spent) # 0.616999 (YY%)
  # 67311840 / 109093160
  full_dat <- full_time_dat_leisure
  write.csv(full_dat, "data/processed_data/joined_time_data.csv", row.names=F)
} else {
  full_time_dat <- read.csv("data/processed_data/joined_time_data.csv")
}

extension_inactivity <- read.csv("data/processed_data/extension_inactivity_downdate.csv")

full_time_dat <- full_time_dat %>% filter(!(experiment_id %in% BAD_USERS))
full_time_dat <- full_time_dat %>% filter(time_spent > 30)
#full_time_dat <- compute_privacy_scores(full_time_dat)

## computetop visited websites

filtered <- full_time_dat %>%
  filter(weeks_since_intervention %in% c(-2, -1))

summary_table <- filtered %>% filter(!website_aggregated_high_level %in% SURVEY_WEBSITES) %>%
  # per user per day, hours
  group_by(experiment_id, date) %>%
  summarise(daily_hours = sum(time_spent) / 3600, .groups = "drop_last") %>%
  # average across days per user
  group_by(experiment_id) %>%
  summarise(avg_daily_hours = mean(daily_hours), .groups = "drop_last") 
print("OVERALL TIME ONLINE")
print(summary(summary_table$avg_daily_hours))

filtered <- full_time_dat %>%
  filter(weeks_since_intervention %in% c(-2, -1) & privacy_exist)

users_all <- filtered %>% summarise(n = n_distinct(experiment_id)) %>% pull(n)

summary_table <- filtered %>%
  # per user per day, hours
  group_by(website_aggregated_high_level, experiment_id, date) %>%
  summarise(daily_hours = sum(time_spent) / 3600, .groups = "drop_last") %>%
  # average across days per user
  group_by(website_aggregated_high_level, experiment_id) %>%
  summarise(avg_daily_hours = mean(daily_hours), .groups = "drop_last") %>%
  # website-level: mean over active users, count active users
  group_by(website_aggregated_high_level) %>%
  summarise(
    avg_daily_hours_per_active_user = mean(avg_daily_hours),
    n_users = n_distinct(experiment_id),
    .groups = "drop"
  ) %>%
  filter(n_users >= 50) %>%
  mutate(
    avg_daily_hours_all_users = (avg_daily_hours_per_active_user * n_users) / users_all
  ) %>%
  arrange(desc(avg_daily_hours_all_users))

print(xtable(summary_table), latex=TRUE)
balanced_panel_weeks <- get_balanced_panel(
  full_time_dat,
  index_var = "weeks_since_intervention",
  values_to_include = c(-2, -1),
  all_values = c(-2, -1, 0, 1, 2, 3)
)

unbalanced_panel <- full_time_dat %>%
  group_by(experiment_id, weeks_since_intervention, post, website_aggregated_high_level, privacy_exist, category_level_1, experiment_condition) %>%
  summarise(
    exposed_privacy_score = first(exposed_privacy_score),
    privacy_full_beta_p = first(privacy_full_beta_p),
    privacy_for_requested_attribute = first(privacy_for_requested_attribute),
    total_time_spent = sum(time_spent, na.rm = TRUE),
    total_visit_count = n(),
    .groups = "drop"
  )

balanced_panel_weeks <- balanced_panel_weeks %>% mutate(post = ifelse(!(weeks_since_intervention %in% c(-1, -2)), 1, 0))

balanced_panel_post <- get_balanced_panel(
  full_time_dat,
  index_var = "post",
  values_to_include = c(0),
  all_values = c(0, 1),
  lower_pct = 0.05,
  upper_pct = 0.95
)

balanced_panel_weeks_extensive_margin <- get_balanced_panel(
  full_time_dat,
  index_var = "weeks_since_intervention",
  values_to_include = c(),
  all_values = c(-2, -1, 0, 1, 2, 3),
  lower_pct = 0.05,
  upper_pct = 0.95
)

balanced_panel_weeks_extensive_margin <- balanced_panel_weeks_extensive_margin %>% mutate(post = ifelse(!(weeks_since_intervention %in% c(-1, -2)), 1, 0))

balanced_panel_post_extensive_margin <- get_balanced_panel(
  full_time_dat,
  index_var = "post",
  values_to_include = c(),
  all_values = c(0, 1),
  lower_pct = 0.1,
  upper_pct = 0.9
)

to_factor <- function(df,
                      weeks_levels = c(-2, -1, 0, 1, 2, 3),
                      post_levels  = c(0, 1)) {
  if ("experiment_id" %in% names(df)) df$experiment_id <- as.factor(df$experiment_id)
  if ("block_by_wave" %in% names(df)) df$block_by_wave <- as.factor(df$block_by_wave)
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

balanced_panel_weeks <- to_factor(balanced_panel_weeks)
balanced_panel_post <- to_factor(balanced_panel_post)
balanced_panel_weeks_extensive_margin <- to_factor(balanced_panel_weeks_extensive_margin)
balanced_panel_post_extensive_margin <- to_factor(balanced_panel_post_extensive_margin)

personalized_scores <- get_personalized_scores(full_time_dat)
top_site_per_category_overall <- personalized_scores %>% group_by(experiment_id, category_level_1) %>% arrange(desc(personalized_score)) %>% slice(1) %>% ungroup()
top_site_per_category_requested <- personalized_scores %>% group_by(experiment_id, category_level_1) %>% arrange(desc(personalized_score_requested_attr)) %>% slice(1) %>% ungroup()

#------ summary stats -------

## product plot of time spent over time ##

user_week <- full_time_dat %>%
  group_by(experiment_id, experiment_condition, weeks_since_intervention) %>%
  summarise(minutes_week = sum(time_spent) / 60, .groups = "drop") %>%  # seconds -> minutes
  mutate(minutes_per_day = minutes_week / 7,
         log_mpd = log1p(minutes_per_day))  # log(1 + minutes/day)

# 2) Aggregate on the log scale with t-based 95% CI
summary_df <- user_week %>%
  group_by(experiment_condition, weeks_since_intervention) %>%
  summarise(
    n = n(),
    mean_log = mean(log_mpd),
    sd_log   = coalesce(sd(log_mpd), 0),
    se       = ifelse(n > 1, sd_log / sqrt(n), 0),
    tcrit    = qt(0.975, df = pmax(n - 1, 1)),
    ci_low_log  = mean_log - tcrit * se,
    ci_high_log = mean_log + tcrit * se,
    .groups = "drop"
  )
p <- ggplot(
  summary_df %>% filter(weeks_since_intervention %in% c(-2, -1, 0, 1, 2, 3)),
  aes(x = weeks_since_intervention,
      y = mean_log,
      color = experiment_condition,
      fill  = experiment_condition)
) +
  geom_ribbon(aes(ymin = ci_low_log, ymax = ci_high_log), alpha = 0.20, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    x = "Weeks since intervention",
    y = "log(1 + minutes per user per day)",
    color = "Condition", fill = "Condition"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(3, 4.5))   # safer than ylim()

# Save outputs
ggsave(paste0(FIGURES_DIR, "time_spent_log.pdf"), plot = p, width = 8, height = 6, device = "pdf")

### PRODUCE A PLOT OF EXPERIENCED PRIVACY ACROSS USERS
user_weighted <- function(user_site_df, value_col,
                          id_cols = c("experiment_id","experiment_condition"),
                          w_col = "w_site") {
  user_site_df %>%
    group_by(across(all_of(id_cols))) %>%
    summarise(
      score = {
        v <- .data[[value_col]]
        w <- .data[[w_col]]
        den <- sum(w[!is.na(v)], na.rm = TRUE)           # re-normalize over non-missing v
        if (den > 0) sum(w * v, na.rm = TRUE) / den else NA_real_
      },
      .groups = "drop"
    )
}

# Use the exact same time-weighting/denominator as baseline column base_mask_col
user_weighted_same_mask <- function(user_site_df,
                                    value_col,         # CF values to average
                                    base_mask_col,     # baseline column defining the mask/denominator
                                    id_cols = c("experiment_id","experiment_condition"),
                                    w_col = "w_site") {
  user_site_df %>%
    group_by(across(all_of(id_cols))) %>%
    summarise(
      score = {
        v_cf  <- .data[[value_col]]
        v0    <- .data[[base_mask_col]]
        w     <- .data[[w_col]]
        mask  <- !is.na(v0)
        den   <- sum(w[mask], na.rm = TRUE)              # SAME denominator as baseline (requested)
        if (den > 0) sum(w[mask] * v_cf[mask], na.rm = TRUE) / den else NA_real_
      },
      .groups = "drop"
    )
}

# ---------- 1) Build site-level weights from post == 0 ----------
df_base <- balanced_panel_post_extensive_margin %>%
  filter(post == 0 & privacy_exist & category_level_1 != "") %>%
  mutate(
    time   = coalesce(total_time_spent, 0),   # seconds
    visits = coalesce(total_visit_count, 0)
  )

user_site_baseline <- df_base %>%
  group_by(experiment_id, experiment_condition,
           website_aggregated_high_level, category_level_1) %>%
  summarise(
    time_sum   = sum(time,   na.rm = TRUE),
    visits_sum = sum(visits, na.rm = TRUE),
    priv_req   = dplyr::first(privacy_for_requested_attribute),
    priv_full  = dplyr::first(privacy_full_beta_p),
    .groups = "drop"
  ) %>%
  group_by(experiment_id, experiment_condition) %>%
  mutate(
    den_time  = sum(time_sum,   na.rm = TRUE),
    den_visit = sum(visits_sum, na.rm = TRUE),
    w_site    = if_else(den_time > 0, time_sum / den_time,
                        if_else(den_visit > 0, visits_sum / den_visit, NA_real_))
  ) %>%
  ungroup()

# ---------- 2) Baseline user scores ----------
baseline_full <- user_weighted(user_site_baseline, "priv_full") %>%
  rename(experienced_privacy_full = score)

baseline_req  <- user_weighted(user_site_baseline, "priv_req") %>%
  rename(experienced_privacy_req = score)

user_privacy_baseline <- baseline_full %>%
  left_join(baseline_req, by = c("experiment_id","experiment_condition"))

# ---------- 3) Merge per-user/per-category TOP tables ----------
user_site_with_tops <- user_site_baseline %>%
  left_join(
    top_site_per_category_overall %>%
      select(experiment_id, category_level_1,
             max_cat_full = personalized_score),
    by = c("experiment_id","category_level_1")
  ) %>%
  left_join(
    top_site_per_category_requested %>%
      select(experiment_id, category_level_1,
             max_cat_req = personalized_score_requested_attr),
    by = c("experiment_id","category_level_1")
  )

# ---------- 4) Counterfactual user scores ----------
cf_full <- user_weighted(user_site_with_tops, "max_cat_full") %>%
  rename(personalized_score_cf_overall = score)

cf_req  <- user_weighted_same_mask(
  user_site_with_tops,
  value_col     = "max_cat_req",
  base_mask_col = "priv_req"
) %>% rename(personalized_score_cf_requested = score)

# ---------- 5) Combine & deltas ----------
cf_results <- user_privacy_baseline %>%
  left_join(cf_full, by = c("experiment_id","experiment_condition")) %>%
  left_join(cf_req,  by = c("experiment_id","experiment_condition")) %>%
  mutate(
    delta_full_overall  = personalized_score_cf_overall  - experienced_privacy_full,
    delta_req_requested = personalized_score_cf_requested - experienced_privacy_req
  )

# ---------- 6) Plots ----------
q25_50_75 <- function(x) stats::quantile(x, c(0.25, 0.50, 0.75), na.rm = TRUE)

qs_req   <- q25_50_75(user_privacy_baseline$experienced_privacy_req)
qs_full  <- q25_50_75(user_privacy_baseline$experienced_privacy_full)
qs_dfull <- q25_50_75(cf_results$delta_full_overall)
qs_dreq  <- q25_50_75(cf_results$delta_req_requested)

light_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    axis.ticks       = element_blank()
  )

p_baseline_req <- ggplot(user_privacy_baseline, aes(x = experienced_privacy_req)) +
  geom_histogram(binwidth = 0.05, boundary = 0, closed = "left",
                 fill = "grey92", color = "grey60") +
  geom_vline(xintercept = qs_req, linetype = c("dashed","solid","dashed"),
             linewidth = c(0.5, 0.8, 0.5), color = "grey25") +
  labs(x = "Experienced Privacy (Requested Attributes)", y = "Users") +
  light_theme

p_baseline_full <- ggplot(user_privacy_baseline, aes(x = experienced_privacy_full)) +
  geom_histogram(binwidth = 0.05, boundary = 0, closed = "left",
                 fill = "grey92", color = "grey60") +
  geom_vline(xintercept = qs_full, linetype = c("dashed","solid","dashed"),
             linewidth = c(0.5, 0.8, 0.5), color = "grey25") +
  labs(x = "Experienced Privacy (Full Set of Attributes)", y = "Users") +
  light_theme

print(p_baseline_req)
print(p_baseline_full)

p_delta_full <- ggplot(cf_results, aes(x = delta_full_overall)) +
  geom_histogram(binwidth = 0.02, boundary = 0, closed = "left",
                 fill = "grey92", color = "grey60") +
  geom_vline(xintercept = qs_dfull, linetype = c("dashed","solid","dashed"),
             linewidth = c(0.5, 0.8, 0.5), color = "grey25") +
  labs(x = "Possible Experienced Privacy Gains (Full Attributes)", y = "Users") +
  light_theme

p_delta_req <- ggplot(cf_results, aes(x = delta_req_requested)) +
  geom_histogram(binwidth = 0.02, boundary = 0, closed = "left",
                 fill = "grey92", color = "grey60") +
  geom_vline(xintercept = qs_dreq, linetype = c("dashed","solid","dashed"),
             linewidth = c(0.5, 0.8, 0.5), color = "grey25") +
  labs(x = "Possible Experienced Privacy Gains (Requested Attributes)", y = "Users") +
  light_theme

ggsave(paste0(FIGURES_DIR, "baseline_privacy_requested.pdf"),
       p_baseline_req, width = 8, height = 6, device = "pdf")
ggsave(paste0(FIGURES_DIR, "baseline_privacy_full.pdf"),
       p_baseline_full, width = 8, height = 6, device = "pdf")
ggsave(paste0(FIGURES_DIR, "delta_privacy_full_overall.pdf"),
       p_delta_full, width = 8, height = 6, device = "pdf")
ggsave(paste0(FIGURES_DIR, "delta_privacy_requested.pdf"),
       p_delta_req, width = 8, height = 6, device = "pdf")

# --- Overlay density plots ---
plot_df_full <- user_privacy_baseline %>%
  select(experiment_id, experiment_condition, baseline = experienced_privacy_full) %>%
  left_join(cf_results %>%
              select(experiment_id, experiment_condition,
                     counterfactual = personalized_score_cf_overall),
            by = c("experiment_id","experiment_condition")) %>%
  pivot_longer(c(baseline, counterfactual), names_to = "series", values_to = "value")

plot_df_req <- user_privacy_baseline %>%
  select(experiment_id, experiment_condition, baseline = experienced_privacy_req) %>%
  left_join(cf_results %>%
              select(experiment_id, experiment_condition,
                     counterfactual = personalized_score_cf_requested),
            by = c("experiment_id","experiment_condition")) %>%
  pivot_longer(c(baseline, counterfactual), names_to = "series", values_to = "value")

light_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    axis.ticks       = element_blank(),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.box       = "vertical",
    legend.title     = element_blank()
  )

col_base <- "grey20"
col_cf   <- "grey40"

p_overlay_full <- ggplot(plot_df_full, aes(x = value, color = series, linetype = series)) +
  geom_density(na.rm = TRUE, adjust = 1.2, linewidth = 1) +
  scale_color_manual(values = c(baseline = col_base, counterfactual = col_cf),
                     labels = c(baseline = "Baseline", counterfactual = "Counterfactual")) +
  scale_linetype_manual(values = c(baseline = "solid", counterfactual = "dashed"),
                        labels = c(baseline = "Baseline", counterfactual = "Counterfactual")) +
  labs(x = "Experienced Privacy (Full Set of Attributes)", y = "Density") +
  light_theme

p_overlay_req <- ggplot(plot_df_req, aes(x = value, color = series, linetype = series)) +
  geom_density(na.rm = TRUE, adjust = 1.2, linewidth = 1) +
  scale_color_manual(values = c(baseline = col_base, counterfactual = col_cf),
                     labels = c(baseline = "Baseline", counterfactual = "Counterfactual")) +
  scale_linetype_manual(values = c(baseline = "solid", counterfactual = "dashed"),
                        labels = c(baseline = "Baseline", counterfactual = "Counterfactual")) +
  labs(x = "Experienced Privacy (Requested Attributes)", y = "Density") +
  light_theme

print(p_overlay_full)
print(p_overlay_req)

ggsave(paste0(FIGURES_DIR, "overlay_privacy_full_density.pdf"),
       p_overlay_full, width = 8, height = 6, device = "pdf")
ggsave(paste0(FIGURES_DIR, "overlay_privacy_requested_density.pdf"),
       p_overlay_req, width = 8, height = 6, device = "pdf")

# ---------- Pre-decile plots ----------
df_post <- balanced_panel_post_extensive_margin %>%
  filter(post == 1 & privacy_exist & category_level_1 != "") %>%
  mutate(time = coalesce(total_time_spent, 0), visits = coalesce(total_visit_count, 0))

user_site_post <- df_post %>%
  group_by(experiment_id, experiment_condition,
           website_aggregated_high_level, category_level_1) %>%
  summarise(
    time_sum = sum(time, na.rm = TRUE), visits_sum = sum(visits, na.rm = TRUE),
    priv_req = dplyr::first(privacy_for_requested_attribute),
    priv_full = dplyr::first(privacy_full_beta_p), .groups = "drop"
  ) %>%
  group_by(experiment_id, experiment_condition) %>%
  mutate(
    den_time = sum(time_sum, na.rm = TRUE), den_visit = sum(visits_sum, na.rm = TRUE),
    w_site = if_else(den_time > 0, time_sum / den_time,
                     if_else(den_visit > 0, visits_sum / den_visit, NA_real_))
  ) %>% ungroup()

post_full <- user_weighted(user_site_post, "priv_full") %>% rename(experienced_privacy_full_post = score)
post_req <- user_weighted(user_site_post, "priv_req") %>% rename(experienced_privacy_req_post = score)
user_privacy_post <- post_full %>% left_join(post_req, by = c("experiment_id", "experiment_condition"))

scores_long <- user_privacy_baseline %>%
  left_join(user_privacy_post, by = c("experiment_id", "experiment_condition")) %>%
  select(experiment_id, experiment_condition,
         pre_req = experienced_privacy_req, post_req = experienced_privacy_req_post,
         pre_full = experienced_privacy_full, post_full = experienced_privacy_full_post) %>%
  pivot_longer(cols = c(pre_req, post_req, pre_full, post_full),
               names_to = c("period", "measure"), names_pattern = "(.*)_(.*)",
               values_to = "value") %>%
  pivot_wider(names_from = period, values_from = value) %>%
  mutate(measure = if_else(measure == "req", "Requested", "Full"))

make_pre_decile_plot <- function(scores_long, measure_label, ylab) {
  dat <- scores_long %>%
    filter(measure == measure_label) %>%
    group_by(experiment_condition) %>%
    mutate(pre_decile = ntile(pre, 10)) %>%
    ungroup() %>%
    group_by(experiment_condition, pre_decile) %>%
    summarise(delta_median = median(post - pre, na.rm = TRUE),
              delta_mean = mean(post - pre, na.rm = TRUE),
              n = n(), .groups = "drop") %>%
    mutate(tau = pre_decile / 10)
  
  pal <- c("control"="#D62728", "info"="#2CA02C", "saliency"="#1F77B4")
  
  ggplot(dat, aes(x = tau, y = delta_median, color = experiment_condition)) +
    geom_hline(yintercept = 0, color = "grey75", linewidth = 0.6) +
    geom_line(linewidth = 1.2, lineend = "round") +
    geom_point(size = 2) +
    scale_color_manual(values = pal, breaks = intersect(names(pal), unique(dat$experiment_condition))) +
    scale_x_continuous(breaks = seq(0.1, 1, by = 0.1),
                       labels = paste0(seq(10, 100, by = 10), "%")) +
    labs(x = "Pre-period decile (by user pre score)", y = ylab) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey90"),
          axis.ticks = element_blank(),
          legend.position = "bottom", legend.title = element_blank())
}

p_predec_req  <- make_pre_decile_plot(scores_long, "Requested", "Median(Post − Pre), requested")
p_predec_full <- make_pre_decile_plot(scores_long, "Full",      "Median(Post − Pre), full")
print(p_predec_req)
print(p_predec_full)

ggsave(file.path(FIGURES_DIR, "pre_decile_shift_requested.pdf"),
       p_predec_req, width = 8, height = 6, device = "pdf")
ggsave(file.path(FIGURES_DIR, "pre_decile_shift_full.pdf"),
       p_predec_full, width = 8, height = 6, device = "pdf")


# ==============================================================
# USER DiD
# ==============================================================

source("code/utils/plot_rules.R")  # add visualization rules
source('./code/time_use_analysis/analysis_assortment_did.R')

pre_balanced_did <- run_did_analysis(balanced_panel_weeks,
                                     panel_label = "balanced panel",
                                     output_dir = TABLES_DIR,
                                     figures_dir = FIGURES_DIR)

post_balanced_did <- run_did_analysis(balanced_panel_weeks_extensive_margin,
                                      panel_label = "full panel",
                                      output_dir = TABLES_DIR,
                                      figures_dir = FIGURES_DIR)

unbalanced_did <- run_did_analysis(unbalanced_panel,
                                   panel_label = "unbalanced panel",
                                   output_dir = TABLES_DIR,
                                   figures_dir = FIGURES_DIR)

# Summary figure comparing privacy regressions across all three panels
privacy_summary <- plot_privacy_summary(
  balanced_results = pre_balanced_did,
  full_panel_results = post_balanced_did,
  unbalanced_results = unbalanced_did,
  output_dir = TABLES_DIR,
  figures_dir = FIGURES_DIR
)

# ============================================================================
# [MOD 3] Run EC paper functions across all weight specs (Fig 5-6)
# Options: "unweighted", "weight_census", "weight_pew", "weight_combined", "all"
# When WEIGHT_SPEC == "all", loops over all 4 specs without re-loading data.
# Data loading (everything above) runs ONCE. Only the EC paper regressions loop.
# ============================================================================

.ALL_SPECS <- c("unweighted", "weight_census", "weight_pew", "weight_combined")
if (WEIGHT_SPEC == "all") {
  .specs_to_run <- .ALL_SPECS
} else {
  .specs_to_run <- WEIGHT_SPEC
}

for (.ws in .specs_to_run) {
  
  cat("\n\n############################################################\n")
  cat("## EC paper regressions with:", .ws, "\n")
  cat("############################################################\n\n")
  
  .wt_suffix <- if (.ws == "unweighted") "" else paste0("_", .ws)
  
  # Fig 6: Extensive margin assortment analysis
  extensive <- run_assortment_analysis(full_time_dat,
                                       output_dir = TABLES_DIR,
                                       figures_dir = FIGURES_DIR,
                                       wt_spec = .ws,
                                       wt_suffix = .wt_suffix)
  
  # ==============================================================
  # ANALYSIS 4.2: PRE-REGISTERED WEBSITE CHOICE SPECIFICATIONS
  # ==============================================================
  
  # Fig 5: Run pre-registered specs on the balanced panel (baseline sites)
  preregistered_balanced <- run_preregistered_specs(
    balanced_panel = balanced_panel_post,
    full_time_dat = full_time_dat,
    panel_label = "Baseline",
    privacy_col = "privacy_for_requested_attribute",
    output_dir = TABLES_DIR,
    figures_dir = FIGURES_DIR,
    wt_spec = .ws,
    wt_suffix = .wt_suffix
  )
  
  # Also run on the extensive margin panel (all sites, not just baseline)
  preregistered_extensive <- run_preregistered_specs(
    balanced_panel = balanced_panel_post_extensive_margin,
    full_time_dat = full_time_dat,
    panel_label = "extensive margin",
    privacy_col = "privacy_for_requested_attribute",
    output_dir = TABLES_DIR,
    figures_dir = FIGURES_DIR,
    wt_spec = .ws,
    wt_suffix = .wt_suffix
  )
  
} # end weight spec loop
cat("\nAll weight specifications completed.\n")