# =============================================================================
# test.R -- ROUND 3: B1 (privacy settings) + A (5th column) dry runs
# Diagnostics only, nothing written to output/
# =============================================================================
library(tidyverse); library(lubridate); library(fixest)
source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/info_acq_helpers.R")

hr <- function(x) cat("\n", strrep("=", 70), "\n[", x, "]\n", strrep("=", 70), "\n", sep = "")

# ---- B1 dry run --------------------------------------------------------------
hr("B1 TEST: settings visits object")
sv <- get_privacy_setting_visits()
cat("rows:", nrow(sv), "| users:", uniqueN <- length(unique(sv$experiment_id)), "\n")
cat("columns:\n"); print(names(sv))
cat("day sample:", head(sv$day, 3), "| wave_id values:", unique(sv$wave_id), "\n")

sv <- sv %>%
  mutate(treatment_date = case_when(wave_id == 1 ~ TREATMENT_DATE_WAVE_1,
                                    wave_id == 2 ~ TREATMENT_DATE_WAVE_2),
         wsi = as.integer(floor((mdy(day) - ymd(treatment_date)) / 7)))
cat("\nweeks_since_intervention distribution:\n"); print(table(sv$wsi, useNA = "always"))

meta <- read.csv(EXPERIMENT_CONDITIONS_CSV) %>%
  mutate(wave_id = ifelse(wave_id == 3, 2, wave_id),
         block_by_wave = paste(wave_id, block_idx, sep = "_")) %>%
  filter(experiment_condition != "" & !(experiment_id %in% BAD_USERS))

sv_user <- sv %>% filter(wsi >= 0 & wsi <= 3) %>%
  group_by(experiment_id) %>%
  summarise(n_visits = n(), n_domains = n_distinct(website_aggregated_high_level))

an <- meta %>% distinct(experiment_id, experiment_condition, block_by_wave) %>%
  left_join(sv_user, by = "experiment_id") %>%
  mutate(across(c(n_visits, n_domains), ~replace_na(., 0)),
         ever = as.numeric(n_visits > 0),
         experiment_condition = factor(experiment_condition,
                                       levels = c("control", "saliency", "info")))

hr("B1 TEST: ever-visit rates by arm (KEY NUMBERS)")
print(an %>% group_by(experiment_condition) %>%
        summarise(n = n(), ever = sum(ever), rate = round(mean(ever), 4)))

hr("B1 TEST: Eq E.1 regression")
m <- feols(ever ~ experiment_condition | block_by_wave, cluster = ~experiment_id, data = an)
print(summary(m))

# ---- A dry run ----------------------------------------------------------------
hr("A TEST: 5th column (any_shared)")
survey_merged <- read.csv("../data/Survey/survey_merged_final.csv", stringsAsFactors = FALSE)
eui <- read.csv(EXPERIMENT_CONDITIONS_CSV, stringsAsFactors = FALSE)
dsa <- survey_merged %>%
  left_join(eui %>% select(email, experiment_id_aux = experiment_id,
                           experiment_condition, wave_id, block_idx),
            by = c("emailid" = "email")) %>%
  filter(completed_both == TRUE & !is.na(data_sharing_1) &
           !is.na(experiment_condition) & experiment_condition != "") %>%
  mutate(across(paste0("data_sharing_", 1:4),
                ~ifelse(. != "No" & !is.na(.), 1, 0), .names = "{.col}_b"),
         any_shared = as.numeric(data_sharing_1_b + data_sharing_2_b +
                                   data_sharing_3_b + data_sharing_4_b > 0),
         experiment_condition = factor(experiment_condition,
                                       levels = c("control", "saliency", "info")),
         wave_id = ifelse(wave_id == 3, 2, wave_id),
         block_by_wave = paste(wave_id, block_idx, sep = "_")) %>%
  rename(experiment_id = experiment_id_aux)

cat("sample:", nrow(dsa), "| any_shared mean:", round(mean(dsa$any_shared), 3),
    "| control mean:", round(mean(dsa$any_shared[dsa$experiment_condition == "control"]), 3), "\n")
m5 <- feols(any_shared ~ experiment_condition | block_by_wave,
            cluster = ~experiment_id, data = dsa)
print(summary(m5))

cat("\n=== ROUND 3 DONE ===\n")