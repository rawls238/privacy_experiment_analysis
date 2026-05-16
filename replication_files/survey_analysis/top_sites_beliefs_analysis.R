# =============================================================================
# TOP SITES BELIEFS ANALYSIS
# =============================================================================
#
# Produces:
#   Main paper [Section 6, Table tab:belief_correctness_top_sites,
#               "Belief Correctness on Top Sites -- Provided Information"]:
#     output/tables/top_sites_information.tex
#
#   Main paper [Section 6, Fig 8, fig:beliefs_by_search,
#               "Beliefs by Search Behavior"]:
#     output/figures/beliefs_by_info_acq.pdf
#
#   App C [Table tab:belief_correctness_top_sitesrandom_info,
#          "Belief Correctness on Top Sites -- Random Information"]:
#     output/tables/top_sites_information_rand_info.tex
#
#   App C [Fig fig:cdf_belief_correctness_top_sites, "CDF of Belief Correctness"]:
#     output/figures/cumulative_belief_distance.pdf
#
#   App C [Fig fig:top_sites_heterogeneity_exposure,
#          "Heterogeneity based on (Ex Ante) Exposure - Tercile"]:
#     output/figures/heterogeneous_beliefs_site_level_by_exposure_tercile.pdf
#
# Weight specs: when WEIGHT_SPEC == "all", produces unweighted + 3 weighted
#   variants of every output (suffix _weight_census, _weight_pew, _weight_combined).
#
# Inputs:
#   ../data/Survey/survey_merged_final.csv
#   ../data/final_extension_data/privacy_info_desc.csv
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#   ../data/Survey/individual_level_weights.csv         (when weighted)
#
# Dependencies:
#   replication_files/utils/time_usage_helpers.R
#   replication_files/utils/info_acq_helpers.R
#   replication_files/utils/plot_rules.R
#
# Note: Previous version of this script also produced (now removed as dead code):
#   - info_exposure_terciles.pdf                              (exploratory, not in paper)
#   - info_exposure_median.pdf                                (exploratory, not in paper)
#   - heterogeneous_beliefs_site_level_by_exposure_median.pdf (alt of tercile, not in paper)
#   - PEW regression block                                    (console-only, no file output)
# Removed dead library() calls:
#   - library(quantreg)  (zero rq() call)
#   - library(stargazer) (zero stargazer() call)
#   - library(xtable)    (zero xtable() call)
#   - library(acs)       (zero acs()/geocode() call)
# Removed dead helpers:
#   - convert_to_numeric()         (zero call sites)
#   - get_internal_privacy_field() (zero call sites)
# =============================================================================

# ----------------------------------------------------------------------------
# Weight spec dispatcher
# ----------------------------------------------------------------------------
# Options: "unweighted", "weight_census", "weight_pew", "weight_combined", "all"
if (!exists("._WEIGHT_LOCK")) {
  WEIGHT_SPEC <- "all"
}

if (WEIGHT_SPEC == "all") {
  ._WEIGHT_LOCK <- TRUE
  for (.ws in c("unweighted", "weight_census", "weight_pew", "weight_combined")) {
    WEIGHT_SPEC <- .ws
    cat("\n\n######## Running top_sites_beliefs_analysis:", .ws, "########\n\n")
    source("replication_files/survey_analysis/top_sites_beliefs_analysis.R")
  }
  rm(._WEIGHT_LOCK, .ws)
  stop("All weight specs completed.", call. = FALSE)
}

rm(list = setdiff(ls(), c("WEIGHT_SPEC", "._WEIGHT_LOCK")))

# ----------------------------------------------------------------------------
# Working directory + utils
# ----------------------------------------------------------------------------
setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/info_acq_helpers.R")
source("replication_files/utils/plot_rules.R")

# Libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(fixest)

# Output directories
FIGURES_DIR <- "output/figures/"
TABLES_DIR  <- "output/tables/"

OUTPUT_SUFFIX <- if (WEIGHT_SPEC == "unweighted") "" else paste0("_", WEIGHT_SPEC)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

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
  beliefsquality_r1 = NA,
  beliefsquality_r2 = NA,
  beliefsquality_r3 = NA
)

# Run feols with optional survey weights
run_weighted_feols <- function(fml, data, cluster_var, wt_spec = WEIGHT_SPEC) {
  if (wt_spec == "unweighted") {
    feols(fml, cluster = cluster_var, data = data)
  } else {
    feols(fml, cluster = cluster_var, weights = ~wt, data = data)
  }
}

# =============================================================================
# LOAD MAIN DATASETS
# =============================================================================

cat("=== LOADING MERGED SURVEY DATA ===\n")

survey_merged <- read.csv("../data/Survey/survey_merged_final.csv",
                          stringsAsFactors = FALSE)
cat(sprintf("Loaded survey_merged: %d rows, %d columns\n",
            nrow(survey_merged), ncol(survey_merged)))

# Remove attention check fails
survey_merged <- survey_merged %>% filter(attentioncheck1 == 3)
survey_merged <- survey_merged %>% filter(attentioncheck  == 2)

# Remove inappropriate time spent
survey_merged <- survey_merged %>% filter(sys_ElapsedTime < 8393)
survey_merged <- survey_merged %>% filter(sys_ElapsedTime > 421)

privacy_info_desc <- read.csv("../data/final_extension_data/privacy_info_desc.csv")
meta_data         <- read.csv("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv")

personalized_info_long <- get_personalized_info_long()

# =============================================================================
# BUILD RESPONSE-LEVEL DATA (q1 / q2 / qrand)
# =============================================================================

cat("\nStep 6a: Processing Q1 responses...\n")

# q1
responses_q1 <- survey_merged %>%
  select(emailid, starts_with("b.requested.inf.1_")) %>%
  pivot_longer(cols = starts_with("b.requested.inf.1_"),
               names_to = "position", values_to = "response") %>%
  mutate(survey_position = as.numeric(gsub("b\\.requested\\.inf\\.1_", "", position)),
         question_type   = "q1") %>%
  filter(!is.na(response)) %>%
  mutate(response_numeric = as.numeric(gsub("%", "", response))) %>%
  select(email = emailid, question_type, survey_position, response_numeric)

# q2
responses_q2 <- survey_merged %>%
  select(emailid, starts_with("b.requested.inf.2_")) %>%
  pivot_longer(cols = starts_with("b.requested.inf.2_"),
               names_to = "position", values_to = "response") %>%
  mutate(survey_position = as.numeric(gsub("b\\.requested\\.inf\\.2_", "", position)),
         question_type   = "q2") %>%
  filter(!is.na(response)) %>%
  mutate(response_numeric = as.numeric(gsub("%", "", response))) %>%
  select(email = emailid, question_type, survey_position, response_numeric)

# qrand
responses_qrand <- survey_merged %>%
  select(emailid, starts_with("b.random.inf_")) %>%
  pivot_longer(cols = starts_with("b.random.inf_"),
               names_to = "position", values_to = "response") %>%
  mutate(survey_position = as.numeric(gsub("b\\.random\\.inf_", "", position)),
         question_type   = "qrand") %>%
  filter(!is.na(response)) %>%
  mutate(response_numeric = as.numeric(gsub("%", "", response))) %>%
  select(email = emailid, question_type, survey_position, response_numeric)

responses_long <- rbind(responses_q1, responses_q2, responses_qrand)

responses_placebo_long <- responses_long %>% filter(survey_position == 1)

responses_long <- responses_long %>%
  filter(survey_position > 1) %>%
  mutate(website_num = survey_position - 1) %>%
  select(email, question_type, survey_position, website_num, response_numeric)

cat(sprintf("\nFinal responses_long: %d rows\n", nrow(responses_long)))
cat("Survey positions: ", paste(sort(unique(responses_long$survey_position)), collapse = " "), "\n")
cat("Website_num values: ", paste(sort(unique(responses_long$website_num)), collapse = " "), "\n")

# =============================================================================
# JOIN RESPONSES WITH WEBSITE INFO + GROUND TRUTH
# =============================================================================

cat("\nStep 7: Joining responses with website information...\n")

df.site_belief_change <- personalized_info_long %>%
  inner_join(responses_long, by = c("email", "website_num"))

df.site_belief_change <- df.site_belief_change %>%
  mutate(
    asked_feature = case_when(
      question_type == "q1"    ~ q1_feature,
      question_type == "q2"    ~ q2_feature,
      question_type == "qrand" ~ qrand_feature,
      TRUE ~ NA_character_
    ),
    asked_field = case_when(
      question_type == "q1"    ~ q1_field,
      question_type == "q2"    ~ q2_field,
      question_type == "qrand" ~ qrand_field,
      TRUE ~ NA_character_
    )
  )

cat("\nStep 8: Adding ground truth from privacy_info...\n")

df.site_belief_change <- df.site_belief_change %>%
  mutate(asked_field_clean = tolower(asked_field))

privacy_info <- get_privacy_info_raw()
privacy_info <- privacy_info %>%
  mutate(field_clean = tolower(field),
         website_high_level = domain_aggregated_high_level)

df.site_belief_change <- df.site_belief_change %>%
  left_join(
    privacy_info %>% select(website_high_level, feature, field_clean, rating),
    by = c("website_high_level" = "website_high_level",
           "asked_feature"      = "feature",
           "asked_field_clean"  = "field_clean")
  )

matched <- sum(!is.na(df.site_belief_change$rating))
total   <- nrow(df.site_belief_change)
cat(sprintf("  Matched %d out of %d rows (%.1f%%)\n",
            matched, total, 100 * matched / total))

cat("\nRemoving unmatched rows (no ground truth available)...\n")
df.site_belief_change <- df.site_belief_change %>%
  filter(!is.na(rating))

# =============================================================================
# COMPUTE BELIEF DISTANCE + CORRECTNESS
# =============================================================================

df.site_belief_change <- df.site_belief_change %>%
  mutate(
    belief_distance = case_when(
      rating == "Yes" ~ abs(100 - response_numeric),
      rating == "No"  ~ abs(0   - response_numeric),
      TRUE ~ NA_real_
    ),
    correct_inclusive = case_when(
      rating == "Yes" & response_numeric >= 50 ~ TRUE,
      rating == "No"  & response_numeric <  50 ~ TRUE,
      TRUE ~ FALSE
    ),
    correct_exclude50 = case_when(
      response_numeric == 50 ~ NA,
      rating == "Yes" & response_numeric > 50 ~ TRUE,
      rating == "No"  & response_numeric < 50 ~ TRUE,
      TRUE ~ FALSE
    ),
    correct_strict = case_when(
      rating == "Yes" & response_numeric >  50 ~ TRUE,
      rating == "No"  & response_numeric <= 50 ~ TRUE,
      TRUE ~ FALSE
    )
  )

df.site_belief_change <- df.site_belief_change %>%
  filter(experiment_condition != "")

# Console diagnostics
accuracy_comparison <- data.frame(
  threshold = c(">=50 is Yes", "Exclude 50%", ">50 is Yes"),
  accuracy = c(
    mean(df.site_belief_change$correct_inclusive, na.rm = TRUE),
    mean(df.site_belief_change$correct_exclude50, na.rm = TRUE),
    mean(df.site_belief_change$correct_strict,    na.rm = TRUE)
  ),
  n_responses = c(
    sum(!is.na(df.site_belief_change$correct_inclusive)),
    sum(!is.na(df.site_belief_change$correct_exclude50)),
    sum(!is.na(df.site_belief_change$correct_strict))
  )
)

cat("\nAccuracy under different thresholds:\n")
print(accuracy_comparison)

cat(sprintf("\nAverage belief distance from truth: %.1f percentage points\n",
            mean(df.site_belief_change$belief_distance, na.rm = TRUE)))

accuracy_by_q <- df.site_belief_change %>%
  group_by(question_type) %>%
  summarise(
    n            = n(),
    accuracy     = round(mean(correct_inclusive, na.rm = TRUE), 3),
    avg_distance = round(mean(belief_distance,   na.rm = TRUE), 1),
    .groups = "drop"
  )
cat("\nAccuracy by question type:\n")
print(accuracy_by_q)

# =============================================================================
# JOIN DEMOGRAPHIC + BLOCK FE METADATA
# =============================================================================

df.site_belief_change <- df.site_belief_change %>%
  mutate(
    experiment_id        = as.factor(experiment_id),
    website_high_level   = as.factor(website_high_level),
    experiment_condition = as.factor(experiment_condition)
  ) %>%
  filter(!is.na(experiment_condition) & experiment_condition != "")

if (!"html_key" %in% names(df.site_belief_change)) {
  cat("\n  WARNING: html_key not found in data; adding from privacy_info\n")
  if ("html_key" %in% names(privacy_info)) {
    df.site_belief_change <- df.site_belief_change %>%
      left_join(
        privacy_info %>%
          select(website_high_level, feature, field_clean, html_key) %>% distinct(),
        by = c("website_high_level", "asked_feature" = "feature",
               "asked_field_clean" = "field_clean")
      )
    df.site_belief_change$html_key <- as.factor(df.site_belief_change$html_key)
    cat("  Added html_key from privacy_info\n")
  }
}

df.site_belief_change <- df.site_belief_change %>%
  left_join(
    survey_merged %>%
      select(emailid, pewq1, pewq2, pewq3, pewq4, PolicyEffectiveness,
             Age, Gender, Income) %>%
      rename(email = emailid),
    by = "email"
  ) %>%
  left_join(
    meta_data %>% select(email, wave_id, block_idx), by = "email"
  ) %>%
  mutate(wave_id       = ifelse(wave_id == 3, 2, wave_id),
         block_by_wave = paste(wave_id, block_idx, sep = "_"))

# =============================================================================
# JOIN SURVEY WEIGHTS (if weighted spec)
# =============================================================================

if (WEIGHT_SPEC != "unweighted") {
  cat(sprintf("\n=== LOADING WEIGHTS (%s) ===\n", WEIGHT_SPEC))
  
  weights_df <- read.csv("../data/Survey/individual_level_weights.csv",
                         stringsAsFactors = FALSE)
  
  weights_ext <- weights_df %>%
    filter(sample == "extension") %>%
    select(experiment_id, all_of(WEIGHT_SPEC)) %>%
    rename(wt = !!WEIGHT_SPEC)
  
  df.site_belief_change <- df.site_belief_change %>%
    mutate(experiment_id_chr = as.character(experiment_id)) %>%
    left_join(
      weights_ext %>% mutate(experiment_id = as.character(experiment_id)),
      by = c("experiment_id_chr" = "experiment_id")
    ) %>%
    select(-experiment_id_chr)
  
  n_with_wt <- sum(!is.na(df.site_belief_change$wt))
  n_total   <- nrow(df.site_belief_change)
  cat(sprintf("  Weight join: matched %d / %d rows\n", n_with_wt, n_total))
  cat(sprintf("  Weight summary: min=%.3f max=%.3f mean=%.3f\n",
              min(df.site_belief_change$wt,  na.rm = TRUE),
              max(df.site_belief_change$wt,  na.rm = TRUE),
              mean(df.site_belief_change$wt, na.rm = TRUE)))
  
  df.site_belief_change <- df.site_belief_change %>%
    mutate(wt = ifelse(is.na(wt), 1, wt))
} else {
  cat("\n=== RUNNING UNWEIGHTED (no weights loaded) ===\n")
  df.site_belief_change$wt <- 1
}

cat(sprintf("  WEIGHT_SPEC: %s\n", WEIGHT_SPEC))

# =============================================================================
# REGRESSIONS — TOP SITES (q1, q2: provided information)
# =============================================================================

fixed.effects <- c(web_html = "|website_high_level + block_by_wave")
dependent.vars <- c("belief_distance", "correct_exclude50", "correct_strict")

# Set factor levels so saliency shows before information
df.site_belief_change$experiment_condition <- factor(
  df.site_belief_change$experiment_condition,
  levels = c("control", "saliency", "info")
)

model <- list()
for (fe in names(fixed.effects)) {
  for (dv in dependent.vars) {
    model[[fe]][[dv]] <- run_weighted_feols(
      as.formula(paste(dv, "~experiment_condition ", fixed.effects[[fe]])),
      cluster = "experiment_id",
      data    = df.site_belief_change %>% filter(question_type != "qrand")
    )
  }
}

dict <- c(
  "experiment_conditioninfo"     = "Information Treatment",
  "experiment_conditionsaliency" = "Saliency Treatment",
  "(Intercept)"                  = "Constant",
  "Intercept"                    = "Constant",
  "website_high_level"           = "Website FE",
  "block_by_wave"                = "Block FE"
)

etable(model$web_html,
       headers     = c("$\\lvert \\text{Belief} - \\text{Truth} \\rvert$",
                       "Correctness (Weak)", "Correctness (Strict)"),
       dict        = dict,
       tex         = TRUE,
       replace     = TRUE,
       digits      = 3,
       depvar      = FALSE, 
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
       title       = "Belief Correctness on Top Sites -- Provided Information",
       file        = paste0(TABLES_DIR, "top_sites_information",
                            OUTPUT_SUFFIX, ".tex"))

# =============================================================================
# REGRESSIONS — RANDOM INFO ROBUSTNESS (qrand)
# =============================================================================

for (fe in names(fixed.effects)) {
  for (dv in dependent.vars) {
    model[[fe]][[dv]] <- run_weighted_feols(
      as.formula(paste(dv, "~experiment_condition ", fixed.effects[[fe]])),
      cluster = "experiment_id",
      data    = df.site_belief_change %>% filter(question_type == "qrand")
    )
  }
}

etable(model$web_html,
       headers     = c("$\\lvert \\text{Belief} - \\text{Truth} \\rvert$",
                       "Correctness (Weak)", "Correctness (Strict)"),
       dict        = dict,
       tex         = TRUE,
       replace     = TRUE,
       digits      = 3,
       depvar      = FALSE, 
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
       title       = "Belief Correctness on Top Sites -- Random Information",
       file        = paste0(TABLES_DIR, "top_sites_information_rand_info",
                            OUTPUT_SUFFIX, ".tex"))

# =============================================================================
# FIG cdf_belief_correctness_top_sites: weighted CDF across treatment groups
# =============================================================================

avg_correctness <- df.site_belief_change %>%
  filter(question_type != "qrand") %>%
  group_by(experiment_id, experiment_condition) %>%
  summarise(belief_distance = weighted.mean(belief_distance, wt, na.rm = TRUE),
            wt              = first(wt),
            .groups = "drop") %>%
  mutate(experiment_condition = factor(experiment_condition,
                                       levels = c("control", "saliency", "info"),
                                       labels = c("Control", "Saliency", "Information"))) %>%
  arrange(experiment_condition, belief_distance) %>%
  group_by(experiment_condition) %>%
  mutate(cum_wt = cumsum(wt) / sum(wt)) %>%
  ungroup()

g <- ggplot(avg_correctness,
            aes(x = belief_distance, y = cum_wt, color = experiment_condition)) +
  geom_step(linewidth = LINE_WIDTH) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_color_treatment() +
  theme_privacy_experiment() +
  labs(x = "| Belief - Truth |", y = "CDF", color = "Condition")

ggsave(paste0(FIGURES_DIR, "cumulative_belief_distance", OUTPUT_SUFFIX, ".pdf"),
       g, width = 8, height = 6)

# =============================================================================
# FIG top_sites_heterogeneity_exposure: tercile split by baseline exposure
# =============================================================================

exposed_websites <- get_aggregated_time_data_with_privacy_info()
exposure <- exposed_websites %>% filter(!post)

beliefs_with_exposure <- df.site_belief_change %>%
  mutate(website_high_level = ifelse(website_high_level == "twitter", "x", website_high_level)) %>%
  filter(question_type != "qrand") %>%
  rename(website_aggregated_high_level = website_high_level) %>%
  left_join(exposure, by = c("website_aggregated_high_level", "experiment_id"))

website_exposure <- beliefs_with_exposure %>%
  group_by(experiment_id, website_aggregated_high_level) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(visit_count = dplyr::coalesce(visit_count, 0)) %>%
  group_by(experiment_condition) %>%
  mutate(
    exposure_tercile_num = ntile(visit_count, 3),
    exposure_tercile = factor(exposure_tercile_num, levels = 1:3,
                              labels = c("Low Exposure", "Medium Exposure", "High Exposure"))
  ) %>%
  ungroup() %>%
  select(experiment_id, website_aggregated_high_level, experiment_condition,
         exposure_tercile)

beliefs_with_exposure <- beliefs_with_exposure %>%
  left_join(website_exposure,
            by = c("website_aggregated_high_level", "experiment_id", "experiment_condition"))

df_plot <- beliefs_with_exposure %>%
  mutate(
    experiment_condition = factor(experiment_condition,
                                  levels = c("control", "saliency", "info"),
                                  labels = c("Control", "Saliency", "Information")),
    exposure_tercile = factor(exposure_tercile,
                              levels = c("Low Exposure", "Medium Exposure", "High Exposure"))
  )

# Weighted mean and weighted SE
impact_tercile <- df_plot %>%
  group_by(experiment_condition, exposure_tercile) %>%
  summarise(
    n      = n(),
    y_mean = weighted.mean(belief_distance, wt, na.rm = TRUE),
    y_se   = sqrt(sum(wt * (belief_distance - weighted.mean(belief_distance, wt, na.rm = TRUE))^2,
                      na.rm = TRUE) / sum(wt, na.rm = TRUE)) / sqrt(n),
    tcrit  = qt(0.975, df = pmax(n - 1, 1)),
    ymin   = y_mean - tcrit * y_se,
    ymax   = y_mean + tcrit * y_se,
    .groups = "drop"
  )

p_tercile <- ggplot(impact_tercile,
                    aes(x = experiment_condition, y = y_mean, color = exposure_tercile)) +
  geom_point(position = position_dodge(width = DODGE_WIDTH_3), size = POINT_SIZE) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
                position = position_dodge(width = DODGE_WIDTH_3),
                width = ERRORBAR_WIDTH, linewidth = LINE_WIDTH) +
  scale_color_ordinal_3() +
  theme_privacy_experiment() +
  labs(x = NULL, y = "| Belief - Truth |", color = NULL)

ggsave(paste0(FIGURES_DIR, "heterogeneous_beliefs_site_level_by_exposure_tercile",
              OUTPUT_SUFFIX, ".pdf"),
       p_tercile, width = 8, height = 6)

# =============================================================================
# FIG 8 (beliefs_by_search): treatment x information acquisition behavior
# =============================================================================

info_acq <- get_experiment_info_acq()
info_acq <- info_acq %>%
  group_by(experiment_id, domain_aggregated_high_level) %>%
  summarise(info_acq = any(info_acq), .groups = "drop")

privacy_policy_visits <- get_privacy_policy_visits()
privacy_policy_visits <- privacy_policy_visits %>%
  select(experiment_id, domain_aggregated_high_level)
privacy_policy_visits$info_acq <- TRUE

info_acq <- rbind(info_acq, privacy_policy_visits)
info_acq <- info_acq %>%
  group_by(experiment_id, domain_aggregated_high_level) %>%
  summarise(info_acq = any(info_acq), .groups = "drop")

beliefs_with_info_acq <- df.site_belief_change %>%
  rename(domain_aggregated_high_level = website_high_level) %>%
  left_join(info_acq, by = c("domain_aggregated_high_level", "experiment_id")) %>%
  filter(question_type != "qrand") %>%
  mutate(info_acq = ifelse(is.na(info_acq), FALSE, info_acq))

dat <- beliefs_with_info_acq %>%
  mutate(
    experiment_condition = factor(experiment_condition,
                                  levels = c("control", "saliency", "info"),
                                  labels = c("Control", "Saliency", "Information")),
    info_acq = factor(info_acq, levels = c(TRUE, FALSE),
                      labels = c("Did Acquire Information",
                                 "Didn't Acquire Information"))
  )

# Weighted mean and weighted SE
sum_df <- dat %>%
  group_by(experiment_condition, info_acq) %>%
  summarise(
    n    = sum(!is.na(belief_distance)),
    mean = weighted.mean(belief_distance, wt, na.rm = TRUE),
    se   = sqrt(sum(wt * (belief_distance - weighted.mean(belief_distance, wt, na.rm = TRUE))^2,
                    na.rm = TRUE) / sum(wt, na.rm = TRUE)) / sqrt(n),
    tcrit = qt(0.975, df = pmax(n - 1, 1)),
    ymin  = mean - tcrit * se,
    ymax  = mean + tcrit * se,
    .groups = "drop"
  )

g <- ggplot(sum_df, aes(x = experiment_condition, y = mean,
                        color = info_acq, group = info_acq)) +
  geom_point(position = position_dodge(width = DODGE_WIDTH_2), size = POINT_SIZE) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
                width = ERRORBAR_WIDTH, linewidth = LINE_WIDTH,
                position = position_dodge(width = DODGE_WIDTH_2)) +
  scale_color_binary() +
  theme_privacy_experiment() +
  labs(x = NULL, y = "| Belief - Truth |", color = NULL)

ggsave(paste0(FIGURES_DIR, "beliefs_by_info_acq", OUTPUT_SUFFIX, ".pdf"),
       g, width = 8, height = 6)

cat("\nDone.\n")