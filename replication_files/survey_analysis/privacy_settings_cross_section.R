# =============================================================================
# PRIVACY SETTINGS CROSS SECTION
# =============================================================================
#
# Produces:
#   Appendix E [Data Sharing section, objective analog of
#               tab:treatment_effect_data_sharing column (4)]:
#     output/tables/privacy_settings_cross_section.tex
#   Inline scalars cited in the surrounding prose:
#     output/values/privacy_settings_values.tex
#       \settingsEverControlPct \settingsEverSalCoef
#       \settingsEverInfoCoef   \settingsEverInfoPval
#
# RESEARCH QUESTION
#   Objective counterpart to the self-reported "changed privacy settings"
#   measure: did treated users actually visit a privacy settings page on at
#   least one website during the post-intervention period?
#
# MEASUREMENT
#   Settings visits come from get_privacy_setting_visits() in
#   info_acq_helpers.R: keyword-detected privacy-page visits from the
#   extension's event logs, manually annotated by the research team, keeping
#   labels PRIVACY_SETTINGS and PRIVACY_SEARCH (372 events, 200 users).
#   Coverage is a lower bound: only settings pages whose URL triggered the
#   privacy keyword detector enter the annotation file.
#
# SPECIFICATION
#   Cross-sectional Eq (E.1), identical to the self-report table:
#     y_i = b1*Saliency_i + b2*Info_i + eta_{block_by_wave} + eps_i
#   y_i in {ever visited (0/1), number of visits, distinct domains},
#   measured over weeks 0-3 since intervention (pre-cookie-deletion).
#   Full experiment-user grid; users with no visits coded 0.
#   SEs clustered at the participant level (one obs per user, so this matches
#   heteroskedasticity-robust; kept for spec consistency with the
#   self-report table).
#
# Inputs:
#   ../data/processed_data/manual_privacy_policy_visit_annotations.csv
#     (via get_privacy_setting_visits)
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#
# Dependencies:
#   replication_files/utils/values.R
#   replication_files/utils/time_usage_helpers.R
#   replication_files/utils/info_acq_helpers.R
#   replication_files/utils/number_format_helpers.R
#   replication_files/utils/tex_helpers.R
#
# Outputs:
#   output/tables/privacy_settings_cross_section.tex
#   output/values/privacy_settings_values.tex
#
# USAGE
#   setwd("~/Dropbox/spring2025experiment/code_github")
#   source("replication_files/survey_analysis/privacy_settings_cross_section.R")
# =============================================================================

# Set working directory to code_github root so all relative paths resolve.
setwd("~/Dropbox/spring2025experiment/code_github")

# Source utility scripts
source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/info_acq_helpers.R")
source("replication_files/utils/number_format_helpers.R")
source("replication_files/utils/tex_helpers.R")

# Load required libraries
library(tidyverse)
library(lubridate)
library(fixest)
library(savetexvalue)

# Output directories
TABLES_DIR <- "output/tables/"
VALUES_DIR <- "output/values/"

# =============================================================================
# 1. Experiment users (full grid)
# =============================================================================
meta_data <- read.csv(EXPERIMENT_CONDITIONS_CSV)
meta_data <- meta_data %>%
  mutate(wave_id = ifelse(wave_id == 3, 2, wave_id)) %>%
  mutate(block_by_wave = paste(wave_id, block_idx, sep = "_"))
experiment_users <- meta_data %>%
  filter(experiment_condition != "" & !(experiment_id %in% BAD_USERS))

# =============================================================================
# 2. Settings visits, event-time aligned, post-intervention window
# =============================================================================
settings_visits <- get_privacy_setting_visits()

settings_visits <- settings_visits %>%
  mutate(
    treatment_date = case_when(
      wave_id == 1 ~ TREATMENT_DATE_WAVE_1,
      wave_id == 2 ~ TREATMENT_DATE_WAVE_2
    ),
    weeks_since_intervention = as.integer(floor((mdy(day) - ymd(treatment_date)) / 7))
  )

settings_user <- settings_visits %>%
  filter(weeks_since_intervention >= 0 & weeks_since_intervention <= 3) %>%
  group_by(experiment_id) %>%
  summarise(n_visits  = n(),
            n_domains = n_distinct(website_aggregated_high_level),
            .groups = "drop")

# =============================================================================
# 3. User-level analysis frame (zeros for non-visitors)
# =============================================================================
settings_analysis <- experiment_users %>%
  distinct(experiment_id, experiment_condition, block_by_wave) %>%
  left_join(settings_user, by = "experiment_id") %>%
  mutate(n_visits   = replace_na(n_visits, 0),
         n_domains  = replace_na(n_domains, 0),
         ever_visit = as.numeric(n_visits > 0),
         experiment_condition = factor(experiment_condition,
                                       levels = c("control", "saliency", "info")))

cat(sprintf("Settings analysis sample: %d users, %d ever visited (%.1f%%)\n",
            nrow(settings_analysis), sum(settings_analysis$ever_visit),
            100 * mean(settings_analysis$ever_visit)))
cat("Ever-visit rate by arm:\n")
print(settings_analysis %>%
        group_by(experiment_condition) %>%
        summarise(n = n(), ever = sum(ever_visit),
                  rate = mean(ever_visit), .groups = "drop"))

# =============================================================================
# 4. Eq (E.1) regressions -> privacy_settings_cross_section.tex
# =============================================================================
m_ever <- feols(ever_visit ~ experiment_condition | block_by_wave,
                cluster = ~experiment_id, data = settings_analysis)
m_nvis <- feols(n_visits ~ experiment_condition | block_by_wave,
                cluster = ~experiment_id, data = settings_analysis)
m_ndom <- feols(n_domains ~ experiment_condition | block_by_wave,
                cluster = ~experiment_id, data = settings_analysis)

dict <- c(
  "experiment_conditionsaliency" = "Saliency Treatment",
  "experiment_conditioninfo"     = "Information Treatment",
  "block_by_wave"                = "Block FE"
)

ps_tex <- etable(m_ever, m_nvis, m_ndom,
                 headers = c("Ever Visited Settings", "Number of Visits",
                             "Distinct Domains"),
                 dict = dict, tex = TRUE, depvar = FALSE,
                 signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                 digits = 3)
write_tabular_only(ps_tex,
                   file = paste0(TABLES_DIR, "privacy_settings_cross_section.tex"))

# =============================================================================
# 5. Inline scalars -> privacy_settings_values.tex
# =============================================================================
ps_values_file <- file.path(VALUES_DIR, "privacy_settings_values.tex")
suppressWarnings(file.remove(ps_values_file))

ctrl_pct <- 100 * mean(settings_analysis$ever_visit[
  settings_analysis$experiment_condition == "control"])

save_tex_value(format_pct(ctrl_pct),
               name = "settingsEverControlPct", file = ps_values_file)
save_tex_value(format_coef(coef(m_ever)["experiment_conditionsaliency"]),
               name = "settingsEverSalCoef", file = ps_values_file)
save_tex_value(format_coef(coef(m_ever)["experiment_conditioninfo"]),
               name = "settingsEverInfoCoef", file = ps_values_file)
save_tex_value(format_pvalue(pvalue(m_ever)["experiment_conditioninfo"]),
               name = "settingsEverInfoPval", file = ps_values_file)

cat(sprintf("Saved 4 macros to %sprivacy_settings_values.tex\n", VALUES_DIR))
cat("=== DONE ===\n")