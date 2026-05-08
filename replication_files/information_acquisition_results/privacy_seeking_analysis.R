## [WEIGHT MODIFICATION] ======================================================
## Flag to control weighting scheme
## Options: "unweighted", "weight_census", "weight_pew", "weight_combined", "all"
if (!exists("._WEIGHT_LOCK")) {
  WEIGHT_SPEC <- "all" # change the flag here
}

if (WEIGHT_SPEC == "all") {
  ._WEIGHT_LOCK <- TRUE
  for (.ws in c("unweighted", "weight_census", "weight_pew", "weight_combined")) {
    WEIGHT_SPEC <- .ws
    cat("\n\n######## Running privacy_seeking_analysis:", .ws, "########\n\n")
    source("code/information_acquisition_results/privacy_seeking_analysis.R")
  }
  rm(._WEIGHT_LOCK, .ws)
  stop("All weight specs completed.", call. = FALSE)
}

rm(list = setdiff(ls(), c("WEIGHT_SPEC", "._WEIGHT_LOCK")))
## =============================================================================

if (Sys.info()[['nodename']] == 'GSB-P4FVDL7QF6'){
  WD <- "/Users/sggold/Library/CloudStorage/Dropbox/Shared-Project-Folders/Privacy-Experiment/spring2025experiment"
} else if (grepl("marklee", Sys.info()[['user']])) {
  WD <- "/Users/marklee/Dropbox/spring2025experiment"
} else {
  WD <- "/Users/guyaridor/Dropbox/Privacy-Experiment/spring2025experiment"
}

# setwd(WD)

source("code/utils/values.R")
source("code/utils/time_usage_helpers.R")
source("code/utils/info_acq_helpers.R")
source("code/utils/plot_rules.R")


# Load required libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(acs)
library(fixest)
library(lfe)

## [WEIGHT MODIFICATION] ======================================================
OUTPUT_SUFFIX <- if (WEIGHT_SPEC == "unweighted") "" else paste0("_", WEIGHT_SPEC)

## Helper function to run feols with or without weights
run_weighted_feols <- function(fml, data, cluster_var, wt_spec = WEIGHT_SPEC) {
  if (wt_spec == "unweighted") {
    feols(fml, cluster = cluster_var, data = data)
  } else {
    feols(fml, cluster = cluster_var, weights = ~wt, data = data)
  }
}

## Helper function to join weights to any analysis dataframe
## Sets wt = 1 when unweighted so downstream code always has wt column
join_weights <- function(df, wt_spec = WEIGHT_SPEC) {
  if (wt_spec == "unweighted") {
    df$wt <- 1
    return(df)
  }
  
  weights_df <- read.csv("data/Survey/individual_level_weights.csv", stringsAsFactors = FALSE)
  weights_ext <- weights_df %>%
    filter(sample == "extension") %>%
    select(experiment_id, all_of(wt_spec)) %>%
    rename(wt = !!wt_spec)
  
  df <- df %>%
    mutate(experiment_id_chr = as.character(experiment_id)) %>%
    left_join(weights_ext %>% mutate(experiment_id = as.character(experiment_id)),
              by = c("experiment_id_chr" = "experiment_id")) %>%
    select(-experiment_id_chr) %>%
    mutate(wt = ifelse(is.na(wt), 1, wt))
  
  cat("  Weight join:", sum(!is.na(df$wt)), "/", nrow(df), "rows\n")
  cat("  Weight summary: min=", round(min(df$wt, na.rm=TRUE), 3),
      " max=", round(max(df$wt, na.rm=TRUE), 3),
      " mean=", round(mean(df$wt, na.rm=TRUE), 3), "\n")
  return(df)
}
## =============================================================================


## positively strange behavior from this account
BAD_USERS <- c("2607a1f")
FIGURES_DIR <- "results/privacy_policy/"
TABLES_DIR <- "results/privacy_policy/"

make_time_varying_plot <- function(coefs, outcome_var, graph_title) {
  # Standardize condition labels
  coefs <- coefs %>%
    mutate(Condition = case_when(
      condition == "info" ~ "Information",
      condition == "saliency" ~ "Saliency",
      condition == "control" ~ "Control",
      TRUE ~ condition
    )) %>%
    mutate(Condition = factor(Condition, levels = TREATMENT_ORDER))
  
  graph <- ggplot(coefs, aes(x = weeks_since_intervention,
                             y = coef_estimate,
                             color = Condition)) +
    geom_hline_zero(linetype = "dashed") +
    geom_vline_intervention(xintercept = 0) +
    geom_errorbar(
      aes(ymin = coef_ci_lower, ymax = coef_ci_upper),
      width = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH
    ) +
    geom_point(size = POINT_SIZE) +
    scale_color_treatment() +
    theme_privacy_experiment(legend_position = "bottom", show_grid_x = TRUE, show_grid_y = TRUE) +
    labs(
      x = "Weeks Since Intervention",
      y = outcome_var,
      title = graph_title,
      color = NULL
    )
  
  return(graph)
}



meta_data <- read.csv("data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
meta_data <- meta_data %>%
  mutate(wave_id = ifelse(wave_id == 3, 2, wave_id)) %>%
  mutate(block_by_wave = paste(wave_id,block_idx,sep = '_'))
experiment_users <- meta_data %>% filter(experiment_condition != "" & !(experiment_id %in% BAD_USERS))

## SECTION 1: measuring treatment effects on privacy policy visits
## [NOT IN EC PAPER — left unchanged]
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
  tidyr::crossing(weeks_since_intervention = c(-2, -1, 0, 1, 2, 3))

user_post_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(post = c(0, 1))

visited_privacy_policy_full <- user_grid %>%
  left_join(visited_privacy_policy_agg,
            by = c("experiment_id", "experiment_condition", "weeks_since_intervention")) %>%
  mutate(number_of_visits = replace_na(number_of_visits, 0),
         distinct_domains = replace_na(distinct_domains, 0))
visited_privacy_policy_full <- visited_privacy_policy_full %>% mutate(ever_visited_policy = as.numeric(number_of_visits > 0))

t1 <- felm(asinh(number_of_visits) ~ experiment_condition*as.factor(weeks_since_intervention) | experiment_id + block_by_wave | 0 | experiment_id , visited_privacy_policy_full)
t2 <- felm(asinh(distinct_domains) ~ experiment_condition*as.factor(weeks_since_intervention) |  experiment_id + block_by_wave | 0 | experiment_id, visited_privacy_policy_full)
t3 <- felm(number_of_visits ~ experiment_condition*as.factor(weeks_since_intervention) |  experiment_id + block_by_wave | 0 | experiment_id, visited_privacy_policy_full)
t4 <- felm(distinct_domains ~ experiment_condition*as.factor(weeks_since_intervention) | experiment_id + block_by_wave | 0 | experiment_id, visited_privacy_policy_full)
t5 <- felm(ever_visited_policy ~ experiment_condition*as.factor(weeks_since_intervention) |  experiment_id + block_by_wave | 0 | experiment_id, visited_privacy_policy_full)


coefs_t3 <- broom::tidy(t3, conf.int = TRUE) %>%
  filter(grepl(":as.factor", term)) %>%
  mutate(
    weeks_since_intervention = str_extract(term, "\\)\\-?\\d+") %>%
      str_remove("\\)") %>%
      as.integer(),
    condition = case_when(
      str_detect(term, "experiment_conditioninfo:") ~ "info",
      str_detect(term, "experiment_conditionsaliency:") ~ "saliency",
      str_detect(term, "as.factor\\(weeks_since_intervention\\)") ~ "control",
      TRUE ~ NA_character_
    ),
    coef_estimate = estimate,
    coef_ci_lower = conf.low,
    coef_ci_upper = conf.high
  ) %>% bind_rows(
    tibble(
      term = c("baseline_info", "baseline_saliency"),
      weeks_since_intervention = -2,
      condition = c("info", "saliency"),
      coef_estimate = 0,
      coef_ci_lower = 0,
      coef_ci_upper = 0
    )
  )
g <- make_time_varying_plot(coefs_t3, "Number of Privacy Policy Visits", "")
ggsave(paste0(FIGURES_DIR, "privacy_policy_treatment_effects_visits", OUTPUT_SUFFIX, ".pdf"), g, width = 8, height = 6)

coefs_t5 <- broom::tidy(t5, conf.int = TRUE) %>%
  filter(grepl(":as.factor", term)) %>%
  mutate(
    weeks_since_intervention = str_extract(term, "\\)\\-?\\d+") %>%
      str_remove("\\)") %>%
      as.integer(),
    condition = case_when(
      str_detect(term, "experiment_conditioninfo:") ~ "info",
      str_detect(term, "experiment_conditionsaliency:") ~ "saliency",
      str_detect(term, "as.factor\\(weeks_since_intervention\\)") ~ "control",
      TRUE ~ NA_character_
    ),
    coef_estimate = estimate,
    coef_ci_lower = conf.low,
    coef_ci_upper = conf.high
  ) %>% bind_rows(
    tibble(
      term = c("baseline_info", "baseline_saliency"),
      weeks_since_intervention = -2,
      condition = c("info", "saliency"),
      coef_estimate = 0,
      coef_ci_lower = 0,
      coef_ci_upper = 0
    )
  )
g <- make_time_varying_plot(coefs_t5, "Number of Unique Privacy Policies Visited", "")
ggsave(paste0(FIGURES_DIR, "privacy_policy_treatment_effects_ever_visit", OUTPUT_SUFFIX, ".pdf"), g, width = 8, height = 6)


policy_summary <- visited_privacy_policy_full %>%
  filter(weeks_since_intervention >= 0) %>%
  group_by(experiment_id, experiment_condition) %>%
  summarise(total_visits = sum(number_of_visits, na.rm = TRUE), .groups = "drop") %>%
  mutate(ever_visited_policy = total_visits > 0) %>%
  group_by(experiment_condition) %>%
  summarise(frac_visited = mean(ever_visited_policy, na.rm = TRUE), .groups = "drop") %>%
  mutate(experiment_condition = factor(experiment_condition,
                                       levels = c("control", "saliency", "info"),
                                       labels = c("Control", "Saliency", "Information")))

g <- ggplot(policy_summary, aes(x = experiment_condition, y = frac_visited, fill = experiment_condition)) +
  geom_col(width = 0.65, color = "white", show.legend = FALSE) +
  scale_fill_treatment() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_privacy_experiment(legend_position = "none", show_grid_x = FALSE, show_grid_y = TRUE) +
  labs(
    x = NULL,
    y = "Fraction Visiting Privacy Policy"
  )

ggsave(paste0(FIGURES_DIR, "ever_visited_privacy_policy", OUTPUT_SUFFIX, ".pdf"), g, width = 8, height = 6)


visited_privacy_policy_full <- user_post_grid %>%
  left_join(visited_privacy_policy_agg,
            by = c("experiment_id", "experiment_condition", "post")) %>%
  mutate(number_of_visits = replace_na(number_of_visits, 0),
         distinct_domains = replace_na(distinct_domains, 0))
visited_privacy_policy_full <- visited_privacy_policy_full %>% mutate(ever_visited_policy = as.numeric(number_of_visits > 0))

t1 <- felm(asinh(number_of_visits) ~ experiment_condition*post | block_by_wave | 0 | experiment_id , visited_privacy_policy_full)
t2 <- felm(asinh(distinct_domains) ~ experiment_condition*post | block_by_wave | 0 | experiment_id, visited_privacy_policy_full)
t3 <- felm(number_of_visits ~ experiment_condition*post | block_by_wave | 0 | experiment_id, visited_privacy_policy_full)
t4 <- felm(distinct_domains ~ experiment_condition*post | block_by_wave | 0 | experiment_id, visited_privacy_policy_full)
t5 <- felm(ever_visited_policy ~ experiment_condition*post | block_by_wave | 0 | experiment_id, visited_privacy_policy_full)
## [END SECTION 1 — unchanged]




#### Section 2: characterize general information acquisition


info_acq <- get_experiment_info_acq()
privacy_policy_visits <- get_privacy_policy_visits()
privacy_policy_visits <- privacy_policy_visits %>% mutate(
  privacy_policy_direct = TRUE,
  info_acq = TRUE,
  all_info = FALSE,
  info_explainer = FALSE
) %>% select(
  domain_aggregated_high_level, post, privacy_policy_direct, all_info, info_acq, experiment_id, info_explainer, tstamp
)
info_acq <- rbind(info_acq, privacy_policy_visits)

## de-dupe any events that occur within a minute of each other
info_acq <- info_acq %>%
  arrange(experiment_id, domain_aggregated_high_level, tstamp) %>%
  group_by(experiment_id, domain_aggregated_high_level) %>%
  filter(is.na(lag(tstamp)) | (tstamp - lag(tstamp)) >= 60) %>%
  ungroup()

experiment_users <- meta_data %>% filter(experiment_condition != "")
info_acq <- info_acq %>% left_join(experiment_users %>% select(wave_id, block_by_wave, experiment_condition, experiment_id), by="experiment_id")
info_acq <- info_acq %>%
  mutate(
    treatment_date = case_when(
      wave_id == 1 ~ TREATMENT_DATE_WAVE_1,
      wave_id == 2 ~ TREATMENT_DATE_WAVE_2
    ),
    # convert raw timestamp to date using lubridate
    tstamp_date = as_date(as_datetime(tstamp)),  
    weeks_since_intervention = as.integer(floor((tstamp_date - ymd(treatment_date)) / 7))
  )
info_acq_agg <- info_acq %>%
  group_by(experiment_id, weeks_since_intervention, experiment_condition, post) %>%
  summarise(number_of_visits = n(), distinct_domains = n_distinct(domain_aggregated_high_level)) %>% ungroup()

experiment_users <- meta_data %>% filter(experiment_condition != "" & !(experiment_id %in% BAD_USERS))
user_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(weeks_since_intervention = c(-2, -1, 0, 1, 2, 3))

user_post_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(post = c(0, 1))

info_acq_full <- user_grid %>%
  left_join(info_acq_agg,
            by = c("experiment_id", "experiment_condition", "weeks_since_intervention")) %>%
  mutate(number_of_visits = replace_na(number_of_visits, 0),
         distinct_domains = replace_na(distinct_domains, 0))
info_acq_full <- info_acq_full %>% mutate(ever_visited_policy = as.numeric(number_of_visits > 0))

## [WEIGHT MODIFICATION] join weights for Fig 4a regression
cat("\n=== Section 2: Info Acquisition (Fig 4a) ===\n")
info_acq_full <- join_weights(info_acq_full)

## Non-EC-paper regressions (unchanged, no weights)
t1 <- felm(asinh(number_of_visits) ~ experiment_condition*as.factor(weeks_since_intervention) | block_by_wave | 0 | experiment_id, info_acq_full)
t2 <- felm(asinh(distinct_domains) ~ experiment_condition*as.factor(weeks_since_intervention) | block_by_wave | 0 | experiment_id, info_acq_full)

## [WEIGHT MODIFICATION] Fig 4a regression: converted felm -> feols for unified weighting
t3 <- run_weighted_feols(
  number_of_visits ~ experiment_condition*as.factor(weeks_since_intervention) | block_by_wave,
  cluster = ~experiment_id,
  data = info_acq_full)

## Non-EC-paper regressions (unchanged, no weights)
t4 <- felm(distinct_domains ~ experiment_condition*as.factor(weeks_since_intervention) | block_by_wave | 0 | experiment_id, info_acq_full)
t5 <- felm(ever_visited_policy ~ experiment_condition*as.factor(weeks_since_intervention) | block_by_wave | 0 | experiment_id, info_acq_full)


coefs_t3 <- broom::tidy(t3, conf.int = TRUE) %>%
  filter(grepl(":as.factor", term)) %>%
  mutate(
    weeks_since_intervention = str_extract(term, "\\)\\-?\\d+") %>%
      str_remove("\\)") %>%
      as.integer(),
    condition = case_when(
      str_detect(term, "experiment_conditioninfo:") ~ "info",
      str_detect(term, "experiment_conditionsaliency:") ~ "saliency",
      str_detect(term, "as.factor\\(weeks_since_intervention\\)") ~ "control",
      TRUE ~ NA_character_
    ),
    coef_estimate = estimate,
    coef_ci_lower = conf.low,
    coef_ci_upper = conf.high
  ) %>% bind_rows(
    tibble(
      term = c("baseline_info", "baseline_saliency"),
      weeks_since_intervention = -2,
      condition = c("info", "saliency"),
      coef_estimate = 0,
      coef_ci_lower = 0,
      coef_ci_upper = 0
    )
  )
g <- make_time_varying_plot(coefs_t3, "# of Info Acquisition Events", "")
ggsave(paste0(FIGURES_DIR, "info_acq_treatment_effects_total_sites", OUTPUT_SUFFIX, ".pdf"), g, width = 8, height = 6)

coefs_t5 <- broom::tidy(t5, conf.int = TRUE) %>%
  filter(grepl(":as.factor", term)) %>%
  mutate(
    weeks_since_intervention = str_extract(term, "\\)\\-?\\d+") %>%
      str_remove("\\)") %>%
      as.integer(),
    condition = case_when(
      str_detect(term, "experiment_conditioninfo:") ~ "info",
      str_detect(term, "experiment_conditionsaliency:") ~ "saliency",
      str_detect(term, "as.factor\\(weeks_since_intervention\\)") ~ "control",
      TRUE ~ NA_character_
    ),
    coef_estimate = estimate,
    coef_ci_lower = conf.low,
    coef_ci_upper = conf.high
  ) %>% bind_rows(
    tibble(
      term = c("baseline_info", "baseline_saliency"),
      weeks_since_intervention = -2,
      condition = c("info", "saliency"),
      coef_estimate = 0,
      coef_ci_lower = 0,
      coef_ci_upper = 0
    )
  )
g <- make_time_varying_plot(coefs_t5, "# of Unique Sites with Info Acquisition", "")
ggsave(paste0(FIGURES_DIR, "info_acq_treatment_effects_unique_sites", OUTPUT_SUFFIX, ".pdf"), g, width = 8, height = 6)


### Section 3: Privacy settings and cookie banner interactions
visited_privacy_settings <- get_privacy_setting_visits()

visited_privacy_settings <- visited_privacy_settings %>% mutate(
  treatment_date = case_when(
    wave_id == 1 ~ TREATMENT_DATE_WAVE_1,
    wave_id == 2 ~ TREATMENT_DATE_WAVE_2
  ),
  weeks_since_intervention = as.integer(floor((mdy(day) - ymd(treatment_date)) / 7))
)
visited_privacy_settings_agg <- visited_privacy_settings %>%
  group_by(experiment_id, weeks_since_intervention, experiment_condition, post) %>%
  summarise(number_of_visits = n(), distinct_domains = n_distinct(website_aggregated_high_level)) %>% ungroup()

user_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(weeks_since_intervention = c(-2, -1, 0, 1, 2, 3))

user_post_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(post = c(0, 1))

visited_privacy_settings_full <- user_grid %>%
  left_join(visited_privacy_settings_agg,
            by = c("experiment_id", "experiment_condition", "weeks_since_intervention")) %>%
  mutate(number_of_visits = replace_na(number_of_visits, 0),
         distinct_domains = replace_na(distinct_domains, 0))
visited_privacy_settings_full <- visited_privacy_settings_full %>% mutate(ever_visited_setting = as.numeric(number_of_visits > 0))
visited_privacy_settings_full <- visited_privacy_settings_full %>% mutate(post = as.numeric(weeks_since_intervention >= 0))

# Set factor levels so saliency shows before information
visited_privacy_settings_full$experiment_condition <- factor(
  visited_privacy_settings_full$experiment_condition,
  levels = c("control", "saliency", "info")
)

## [WEIGHT MODIFICATION] join weights for Table 6
cat("\n=== Section 3a: Privacy Settings (Table 6) ===\n")
visited_privacy_settings_full <- join_weights(visited_privacy_settings_full)

## [WEIGHT MODIFICATION] Table 6 regressions
t1 <- run_weighted_feols(
  number_of_visits ~ experiment_condition*as.factor(post) | as.factor(weeks_since_intervention) + experiment_id + block_by_wave,
  cluster = ~experiment_id,
  data = visited_privacy_settings_full)
t2 <- run_weighted_feols(
  ever_visited_setting ~ experiment_condition*as.factor(post) | as.factor(weeks_since_intervention) + experiment_id + block_by_wave,
  cluster = ~experiment_id,
  data = visited_privacy_settings_full)
t3 <- run_weighted_feols(
  distinct_domains ~ experiment_condition*as.factor(post) | as.factor(weeks_since_intervention) + experiment_id + block_by_wave,
  cluster = ~experiment_id,
  data = visited_privacy_settings_full)

## [WEIGHT MODIFICATION] add OUTPUT_SUFFIX to filename
etable(t1, t2, t3, tex = TRUE, file = paste0(TABLES_DIR, "privacy_settings", OUTPUT_SUFFIX, ".tex"))

event_logs <- get_event_logs()

cookie_banner_interaction <- event_logs %>% filter(event %in% c("COOKIE_BANNER_INTERACTION") & weeks_since_intervention %in% c(-2, -1, 0, 1, 2, 3))
cookie_banner_interaction_agg <- cookie_banner_interaction %>%
  group_by(experiment_id, weeks_since_intervention, experiment_condition) %>%
  summarise(number_of_cookie_banner_interactions = n(), distinct_domains = n_distinct(domain)) %>% ungroup()

user_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(weeks_since_intervention = c(-2, -1, 0, 1, 2, 3))

cookie_banner_interaction_full <- user_grid %>%
  left_join(cookie_banner_interaction_agg,
            by = c("experiment_id", "experiment_condition", "weeks_since_intervention")) %>%
  mutate(number_of_cookie_banner_interactions = replace_na(number_of_cookie_banner_interactions, 0),
         distinct_domains = replace_na(distinct_domains, 0))
cookie_banner_interaction_full <- cookie_banner_interaction_full %>% mutate(post = as.numeric(weeks_since_intervention >= 0))
## drop crazy outlier
cookie_banner_interaction_full <- cookie_banner_interaction_full %>% filter(!experiment_id %in% c("2607a1f"))


# Set factor levels so saliency shows before information
cookie_banner_interaction_full$experiment_condition <- factor(
  cookie_banner_interaction_full$experiment_condition,
  levels = c("control", "saliency", "info")
)

## [WEIGHT MODIFICATION] join weights for Table 3
cat("\n=== Section 3b: Cookie Banner (Table 3) ===\n")
cookie_banner_interaction_full <- join_weights(cookie_banner_interaction_full)

## [WEIGHT MODIFICATION] Table 3 regressions
t3 <- run_weighted_feols(
  number_of_cookie_banner_interactions ~ experiment_condition*as.factor(post) | as.factor(weeks_since_intervention) + experiment_id + block_by_wave,
  cluster = ~experiment_id,
  data = cookie_banner_interaction_full)
t4 <- run_weighted_feols(
  distinct_domains ~ experiment_condition*as.factor(post) | as.factor(weeks_since_intervention) + experiment_id + block_by_wave,
  cluster = ~experiment_id,
  data = cookie_banner_interaction_full)

## [WEIGHT MODIFICATION] add OUTPUT_SUFFIX to filename
etable(t3, t4, tex = TRUE, file = paste0(TABLES_DIR, "cookie_banner_interactions", OUTPUT_SUFFIX, ".tex"))

dict <- c(
  "experiment_conditioninfo x as.factor(post)1"      = "Information Treatment",
  "experiment_conditioninfo x as.factor(post)1"  = "Saliency Treatment",
  "(Intercept)"     = "Constant",
  "Intercept"       = "Constant"
)