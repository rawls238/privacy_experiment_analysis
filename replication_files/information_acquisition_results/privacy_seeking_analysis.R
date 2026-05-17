# =============================================================================
# PRIVACY-SEEKING ANALYSIS
# =============================================================================
#
# Produces:
#   Main paper:
#     Fig 7 [fig:info_acq, "Treatment Effect on Search Behaviors"]:
#       Fig 7(a) [fig:info_acq_total_sites]:
#         output/figures/info_acq_treatment_effects_total_sites[_suffix].pdf
#       Fig 7(b) [fig:info_acq_privacy_policy]:
#         output/figures/ever_visited_privacy_policy[_suffix].pdf
#   Appendix C:
#     Table [tab:cookie_banner_interactions_treatment,
#            "Treatment Effect on Cookie Banner Interactions"]:
#       output/tables/cookie_banner_interactions_treatment[_suffix].tex
#
#   `_suffix` is empty for unweighted and `_{weight_spec}` otherwise. Set
#   WEIGHT_SPEC at the top of the script: "unweighted", "weight_census",
#   "weight_pew", "weight_combined", or "all" to run all four.
#
# Inputs:
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#   ../data/Survey/individual_level_weights.csv  (only if WEIGHT_SPEC != "unweighted")
#   (additional inputs accessed via helper functions in
#    replication_files/utils/info_acq_helpers.R and time_usage_helpers.R)
#
# Dependencies:
#   replication_files/utils/values.R       (BAD_USERS, treatment dates)
#   replication_files/utils/time_usage_helpers.R
#   replication_files/utils/info_acq_helpers.R
#   replication_files/utils/plot_rules.R
#
# Outputs:
#   output/figures/info_acq_treatment_effects_total_sites[_suffix].pdf
#   output/figures/ever_visited_privacy_policy[_suffix].pdf
#   output/tables/cookie_banner_interactions_treatment[_suffix].tex
#
# Note: Previous version of this script also produced (now removed as dead code,
# not in paper):
#   - privacy_policy_treatment_effects_visits.pdf (exploratory)
#   - privacy_policy_treatment_effects_ever_visit.pdf (exploratory)
#   - info_acq_treatment_effects_unique_sites.pdf (alt version of total_sites)
#   - privacy_settings.tex (tab:privacy_settings_visits is in a \begin{comment}
#     block in the paper, not actually shown)
# Removed dead computation (felm calls whose output never reached ggsave/etable):
#   - Sec 1 first-batch felm t1/t2/t4 (only t3, t5 fed B-deleted ggsaves)
#   - Sec 1 second-batch felm t1-t5 (post-only spec, never printed/saved)
#   - Sec 2 felm t1/t2/t4/t5 (only t3 feeds the kept ggsave; t5 fed unique_sites)
#   - Sec 3a privacy_settings regression block (t1, t2, t3 + etable)
#   - Trailing `dict` definition that was never referenced
# Removed dead library() calls:
#   - library(acs) (no use in script)
#   - library(lfe) (felm functions all removed)
# Removed inline BAD_USERS definition (sourced from utils/values.R instead).
# =============================================================================

# Set working directory to code_github root so all relative paths resolve.
# Done first (before the weight dispatcher) so the recursive source() call
# inside the dispatcher can find this script via a relative path.
setwd("~/Dropbox/spring2025experiment/code_github")

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
    source("replication_files/information_acquisition_results/privacy_seeking_analysis.R")
  }
  rm(._WEIGHT_LOCK, .ws)
  stop("All weight specs completed.", call. = FALSE)
}

rm(list = setdiff(ls(), c("WEIGHT_SPEC", "._WEIGHT_LOCK")))
## =============================================================================

# Source utility scripts
source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/info_acq_helpers.R")
source("replication_files/utils/plot_rules.R")

# Load required libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(fixest)

# Output directories
FIGURES_DIR <- "output/figures/"
TABLES_DIR  <- "output/tables/"

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
  
  weights_df <- read.csv("../data/Survey/individual_level_weights.csv",
                         stringsAsFactors = FALSE)
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
  cat("  Weight summary: min=", round(min(df$wt, na.rm = TRUE), 3),
      " max=", round(max(df$wt, na.rm = TRUE), 3),
      " mean=", round(mean(df$wt, na.rm = TRUE), 3), "\n")
  return(df)
}
## =============================================================================

# Plot helper: weekly DiD coefficient plot used for Fig 7(a).
make_time_varying_plot <- function(coefs, outcome_var, graph_title) {
  coefs <- coefs %>%
    mutate(Condition = case_when(
      condition == "info"     ~ "Information",
      condition == "saliency" ~ "Saliency",
      condition == "control"  ~ "Control",
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
    theme_privacy_experiment(legend_position = "bottom",
                             show_grid_x = TRUE, show_grid_y = TRUE) +
    labs(
      x = "Weeks Since Intervention",
      y = outcome_var,
      title = graph_title,
      color = NULL
    )
  
  return(graph)
}

# Load experiment metadata (shared by all sections below).
meta_data <- read.csv("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
meta_data <- meta_data %>%
  mutate(wave_id = ifelse(wave_id == 3, 2, wave_id)) %>%
  mutate(block_by_wave = paste(wave_id, block_idx, sep = "_"))
experiment_users <- meta_data %>%
  filter(experiment_condition != "" & !(experiment_id %in% BAD_USERS))

# =============================================================================
# SECTION 1: Privacy policy visits -> Fig 7(b) ever_visited_privacy_policy.pdf
# =============================================================================
# Note: previous version also estimated weekly DiDs (felm t1-t5, twice) for
# privacy_policy_treatment_effects_visits.pdf and
# privacy_policy_treatment_effects_ever_visit.pdf. Both pdfs were exploratory
# and not used in the paper; the regressions and ggsaves have been removed.

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
  summarise(number_of_visits = n(),
            distinct_domains = n_distinct(website_aggregated_high_level)) %>%
  ungroup()

user_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(weeks_since_intervention = c(-2, -1, 0, 1, 2, 3))

visited_privacy_policy_full <- user_grid %>%
  left_join(visited_privacy_policy_agg,
            by = c("experiment_id", "experiment_condition", "weeks_since_intervention")) %>%
  mutate(number_of_visits = replace_na(number_of_visits, 0),
         distinct_domains = replace_na(distinct_domains, 0))

# Fig 7(b): fraction of participants who ever visited a privacy policy in the
# post-intervention period, by treatment group.
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

g <- ggplot(policy_summary, aes(x = experiment_condition, y = frac_visited,
                                fill = experiment_condition)) +
  geom_col(width = 0.65, color = "white", show.legend = FALSE) +
  scale_fill_treatment() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_privacy_experiment(legend_position = "none",
                           show_grid_x = FALSE, show_grid_y = TRUE) +
  labs(
    x = NULL,
    y = "Fraction Visiting Privacy Policy"
  )

ggsave(paste0(FIGURES_DIR, "ever_visited_privacy_policy", OUTPUT_SUFFIX, ".pdf"),
       g, width = 8, height = 6)

# =============================================================================
# SECTION 2: General information acquisition
#            -> Fig 7(a) info_acq_treatment_effects_total_sites.pdf
# =============================================================================
# Note: previous version also estimated asinh / distinct_domains / ever-visited
# versions of the DiD (felm t1, t2, t4, t5) and produced
# info_acq_treatment_effects_unique_sites.pdf. Neither the alt regressions nor
# the unique_sites pdf appear in the paper; all removed.

info_acq <- get_experiment_info_acq()
privacy_policy_visits <- get_privacy_policy_visits()
privacy_policy_visits <- privacy_policy_visits %>%
  mutate(
    privacy_policy_direct = TRUE,
    info_acq              = TRUE,
    all_info              = FALSE,
    info_explainer        = FALSE
  ) %>%
  select(domain_aggregated_high_level, post, privacy_policy_direct, all_info,
         info_acq, experiment_id, info_explainer, tstamp)
info_acq <- rbind(info_acq, privacy_policy_visits)

# De-dupe any events that occur within a minute of each other.
info_acq <- info_acq %>%
  arrange(experiment_id, domain_aggregated_high_level, tstamp) %>%
  group_by(experiment_id, domain_aggregated_high_level) %>%
  filter(is.na(lag(tstamp)) | (tstamp - lag(tstamp)) >= 60) %>%
  ungroup()

info_acq <- info_acq %>%
  left_join(
    experiment_users %>%
      select(wave_id, block_by_wave, experiment_condition, experiment_id),
    by = "experiment_id"
  ) %>%
  mutate(
    treatment_date = case_when(
      wave_id == 1 ~ TREATMENT_DATE_WAVE_1,
      wave_id == 2 ~ TREATMENT_DATE_WAVE_2
    ),
    tstamp_date              = as_date(as_datetime(tstamp)),
    weeks_since_intervention = as.integer(floor((tstamp_date - ymd(treatment_date)) / 7))
  )

info_acq_agg <- info_acq %>%
  group_by(experiment_id, weeks_since_intervention, experiment_condition, post) %>%
  summarise(number_of_visits = n(),
            distinct_domains = n_distinct(domain_aggregated_high_level)) %>%
  ungroup()

user_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(weeks_since_intervention = c(-2, -1, 0, 1, 2, 3))

info_acq_full <- user_grid %>%
  left_join(info_acq_agg,
            by = c("experiment_id", "experiment_condition", "weeks_since_intervention")) %>%
  mutate(number_of_visits = replace_na(number_of_visits, 0),
         distinct_domains = replace_na(distinct_domains, 0))

cat("\n=== Section 2: Info Acquisition (Fig 7a) ===\n")
info_acq_full <- join_weights(info_acq_full)

# Fig 7(a) regression: weekly DiD on number of info-acquisition events.
t3 <- run_weighted_feols(
  number_of_visits ~ experiment_condition * as.factor(weeks_since_intervention) | block_by_wave,
  cluster = ~experiment_id,
  data    = info_acq_full
)

coefs_t3 <- broom::tidy(t3, conf.int = TRUE) %>%
  filter(grepl(":as.factor", term)) %>%
  mutate(
    weeks_since_intervention = str_extract(term, "\\)\\-?\\d+") %>%
      str_remove("\\)") %>%
      as.integer(),
    condition = case_when(
      str_detect(term, "experiment_conditioninfo:")     ~ "info",
      str_detect(term, "experiment_conditionsaliency:") ~ "saliency",
      str_detect(term, "as.factor\\(weeks_since_intervention\\)") ~ "control",
      TRUE ~ NA_character_
    ),
    coef_estimate = estimate,
    coef_ci_lower = conf.low,
    coef_ci_upper = conf.high
  ) %>%
  bind_rows(
    tibble(
      term                     = c("baseline_info", "baseline_saliency"),
      weeks_since_intervention = -2,
      condition                = c("info", "saliency"),
      coef_estimate            = 0,
      coef_ci_lower            = 0,
      coef_ci_upper            = 0
    )
  )

g <- make_time_varying_plot(coefs_t3, "# of Info Acquisition Events", "")
ggsave(paste0(FIGURES_DIR, "info_acq_treatment_effects_total_sites", OUTPUT_SUFFIX, ".pdf"),
       g, width = 8, height = 6)

# =============================================================================
# SECTION 3: Cookie banner interactions
#            -> App C tab:cookie_banner_interactions_treatment
# =============================================================================
# Note: previous version also estimated a privacy-settings DiD block that
# produced privacy_settings.tex. The matching paper table
# (tab:privacy_settings_visits) is wrapped in a \begin{comment} block in the
# writeup and is not shown in the paper. The privacy-settings block has been
# removed; the cookie-banner block below is kept.

event_logs <- get_event_logs()

cookie_banner_interaction <- event_logs %>%
  filter(event %in% c("COOKIE_BANNER_INTERACTION") &
           weeks_since_intervention %in% c(-2, -1, 0, 1, 2, 3))
cookie_banner_interaction_agg <- cookie_banner_interaction %>%
  group_by(experiment_id, weeks_since_intervention, experiment_condition) %>%
  summarise(number_of_cookie_banner_interactions = n(),
            distinct_domains                     = n_distinct(domain)) %>%
  ungroup()

user_grid <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  tidyr::crossing(weeks_since_intervention = c(-2, -1, 0, 1, 2, 3))

cookie_banner_interaction_full <- user_grid %>%
  left_join(cookie_banner_interaction_agg,
            by = c("experiment_id", "experiment_condition", "weeks_since_intervention")) %>%
  mutate(number_of_cookie_banner_interactions = replace_na(number_of_cookie_banner_interactions, 0),
         distinct_domains                     = replace_na(distinct_domains, 0)) %>%
  mutate(post = as.numeric(weeks_since_intervention >= 0))

# Drop the same outlier participant flagged in BAD_USERS / earlier in the script.
cookie_banner_interaction_full <- cookie_banner_interaction_full %>%
  filter(!experiment_id %in% BAD_USERS)

# Set factor levels so saliency shows before information (matches paper col order).
cookie_banner_interaction_full$experiment_condition <- factor(
  cookie_banner_interaction_full$experiment_condition,
  levels = c("control", "saliency", "info")
)

cat("\n=== Section 3: Cookie Banner (App C Table) ===\n")
cookie_banner_interaction_full <- join_weights(cookie_banner_interaction_full)

t_total <- run_weighted_feols(
  number_of_cookie_banner_interactions ~ experiment_condition * as.factor(post)
  | as.factor(weeks_since_intervention) + experiment_id + block_by_wave,
  cluster = ~experiment_id,
  data    = cookie_banner_interaction_full
)
t_distinct <- run_weighted_feols(
  distinct_domains ~ experiment_condition * as.factor(post)
  | as.factor(weeks_since_intervention) + experiment_id + block_by_wave,
  cluster = ~experiment_id,
  data    = cookie_banner_interaction_full
)

dict <- c(
  "experiment_conditioninfo:as.factor(post)1"     = "Information Treatment x Post",
  "experiment_conditionsaliency:as.factor(post)1" = "Saliency Treatment x Post",
  "experiment_id"                                 = "Participant FE",
  "block_by_wave"                                 = "Block FE",
  "as.factor(weeks_since_intervention)"           = "Weeks FE"
)

etable(
  t_total, t_distinct,
  headers     = c("Cookie Banner Total Interactions", "Cookie Banner Distinct Domains"),
  dict        = dict,
  tex         = TRUE,
  replace     = TRUE,
  depvar      = FALSE,
  title       = "Treatment Effect on Cookie Banner Interactions",
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
  digits      = 3,
  file        = paste0(TABLES_DIR, "cookie_banner_interactions_treatment",
                       OUTPUT_SUFFIX, ".tex")
)