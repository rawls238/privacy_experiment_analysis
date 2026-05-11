# =============================================================================
# INFO ACQUISITION HELPERS
# =============================================================================
# Utility functions for loading and processing info-acquisition related data
# (privacy policy visits, event logs, belief data).
#
# Sourced from: replication_files/{survey_analysis,information_acquisition_results,
#                                    time_use_analysis,...}/*.R
# Working directory contract: caller must setwd to ~/Dropbox/spring2025experiment/code_github
# so that relative paths below resolve correctly.
# =============================================================================

source("replication_files/utils/values.R")

# ---- Data paths (stage 2 convention: wd = code_github, data sits one level up) ----
DATA_DIR     <- "../data/"
SURVEY_DIR   <- paste0(DATA_DIR, "Survey/")
EXT_DATA_DIR <- paste0(DATA_DIR, "final_extension_data/")
PROC_DIR     <- paste0(DATA_DIR, "processed_data/")

# Individual file paths
PRIVACY_POLICY_TEXT_CSV    <- paste0(EXT_DATA_DIR, "privacy_policy_text.csv")
PRIVACY_POLICY_VISITS_CSV  <- paste0(PROC_DIR,     "manual_privacy_policy_visit_annotations.csv")
EXPERIMENT_CONDITIONS_CSV  <- paste0(EXT_DATA_DIR, "experiment_conditions_pilot_july_2024.csv")
EVENT_LOGS_CSV             <- paste0(EXT_DATA_DIR, "event_logs.csv")
SURVEY_MERGED_FINAL_CSV    <- paste0(SURVEY_DIR,   "survey_merged_final.csv")


get_privacy_policy_visits <- function() {
  privacy_policy_text <- read.csv(PRIVACY_POLICY_TEXT_CSV)
  
  ##
  privacy_policy_text  <- aggregate_time_data(privacy_policy_text, field="domain")
  privacy_policy_text <- high_level_aggregate(privacy_policy_text, field="domain_aggregated")
  privacy_policy_text <- privacy_policy_text %>%
    mutate(
      policy_link = str_extract(privacy_policy_link, "(?<=//)[^/]+")
    )
  privacy_policy_text  <- aggregate_time_data(privacy_policy_text, field="policy_link")
  privacy_policy_text <- high_level_aggregate(privacy_policy_text, field="policy_link_aggregated")
  visited_privacy_policy <- read.csv(PRIVACY_POLICY_VISITS_CSV)
  visited_privacy_policy <- visited_privacy_policy %>% filter(!(experiment_id %in% BAD_USERS) & manual_privacy_classification == "PRIVACY_POLICY")
  
  visited_privacy_policy <- visited_privacy_policy %>%
    mutate(
      website = str_extract(domain, "(?<=//)[^/]+")
    )
  visited_privacy_policy <- aggregate_time_data(visited_privacy_policy, field="website")
  visited_privacy_policy <- high_level_aggregate(visited_privacy_policy, field="website_aggregated")
  
  ## join the actual policies and the visited domains
  
  
  domain_to_policy_host <- privacy_policy_text %>%
    distinct(domain_aggregated_high_level, policy_link_aggregated_high_level)
  
  policy_by_host <- privacy_policy_text %>% select(domain_aggregated_high_level, policy_link_aggregated_high_level) %>%
    arrange(policy_link_aggregated_high_level) %>%
    distinct(policy_link_aggregated_high_level, .keep_all = TRUE)
  
  visited_with_host <- visited_privacy_policy %>%
    left_join(
      domain_to_policy_host,
      by = c("website_aggregated_high_level" = "domain_aggregated_high_level")
    ) %>%
    mutate(
      policy_host_domain = coalesce(policy_link_aggregated_high_level, website_aggreginated_high_level := website_aggregated_high_level)
    ) %>%
    select(-policy_link_aggregated_high_level)
  
  visited_with_policy <- visited_with_host %>%
    left_join(
      policy_by_host,
      by = c("policy_host_domain" = "policy_link_aggregated_high_level")
    )
  return(visited_with_policy)
}


get_event_logs <- function() {
  
  
  meta_data <- read.csv(EXPERIMENT_CONDITIONS_CSV)
  meta_data <- meta_data %>% select(experiment_id, email, experiment_condition, block_idx, wave_id)
  meta_data <- meta_data %>% mutate(wave_id = ifelse(wave_id == 3, 2, wave_id)) %>%
    mutate(block_by_wave = paste(wave_id,block_idx,sep = '_'))
  event_logs <- read.csv(EVENT_LOGS_CSV)
  event_logs <- event_logs %>% left_join(meta_data, by="experiment_id")
  event_logs <- event_logs %>% mutate(tstamp_posix = as_datetime(tstamp),
                                      day = as_date(tstamp_posix))
  event_logs <- event_logs %>% filter(experiment_condition != "")
  event_logs <- event_logs %>% filter((wave_id == 1 & day >= START_DATE_WAVE_1 & day <= END_DATE_WAVE_1) | 
                                        (wave_id == 2 & day >= START_DATE_WAVE_2 & day <= END_DATE_WAVE_2))
  event_logs <- event_logs %>% 
    mutate(post = case_when(
      wave_id == 1 ~ day >= TREATMENT_DATE_WAVE_1,
      wave_id == 2 ~ day >= TREATMENT_DATE_WAVE_2
    ),
    treatment_date = case_when(
      wave_id == 1 ~ TREATMENT_DATE_WAVE_1,
      wave_id == 2 ~ TREATMENT_DATE_WAVE_2
    ),
    weeks_since_intervention = as.integer(floor((day - ymd(treatment_date)) / 7))
    )
  return(event_logs)
}

get_info_exposure_events <- function() {
  event_logs <- get_event_logs()
  event_logs <- event_logs %>%
    mutate(has_random_info = str_detect(event, "RANDOM_INFO"))
  random_info <- event_logs %>% filter(has_random_info)
  random_info <- random_info %>%
    mutate(exposed_field = str_remove(event, "^RANDOM_INFO\\s*"))
  
  random_info  <- aggregate_time_data(random_info, field="domain")
  random_info <- high_level_aggregate(random_info, field="domain_aggregated")
  random_info <- random_info %>% select(experiment_id, tstamp, post, domain_aggregated_high_level, has_random_info, exposed_field)
  return(random_info)
}

get_experiment_info_acq <- function(keep_all_cols = FALSE) {
  event_logs <- get_event_logs()
  event_logs <- event_logs %>%
    mutate(all_info = str_detect(event, "display_all_info"),
           info_explainer = str_detect(event, "display_long_info"),
           privacy_policy_direct = str_detect(event, "EXTERNAL_PRIVACY_POLICY_REDIRECT"))
  event_logs <- event_logs %>% mutate(info_acq = all_info | info_explainer | privacy_policy_direct)
  info_acq <- event_logs %>% filter(info_acq == TRUE)
  
  info_acq  <- aggregate_time_data(info_acq, field="domain")
  info_acq <- high_level_aggregate(info_acq, field="domain_aggregated")
  if (!keep_all_cols) {
    info_acq <- info_acq %>% select(experiment_id, tstamp, post, domain_aggregated_high_level, all_info, info_explainer, all_info, privacy_policy_direct, info_acq)
  }
  return(info_acq)
}



get_privacy_attitude_pew_df <- function() {
  
  pewq1$pew <- c(56/96, 18/96, 22/96)
  pewq1$score <- c(0.0, 1.0, 0.5)
  pewq1$label <- "Immediately Accept Terms"
  pewq1$npew <- 5101 * 0.96
  
  pewq2$pew <- c(7/95, 61/95, 27/95)
  pewq2$score <- c(0.0, 1.0, 0.5)
  pewq2$label <- "Privacy Policies Effective"
  pewq2$npew <- 5101 * 0.95
  
  pewq3$pew <- c(51, 49)
  pewq3$score <- c(0.0, 1.0)
  pewq3$label <- "Extensive Margin Privacy Policies"
  pewq3$npew <- 5101
  
  privacy_attitudes_pew <- rbind(extensive_margin_pew, privacy_policy_effective, privacy_policy_immediate)
  return(privacy_attitudes_pew)
}


belief_to_internal <- c(
  # beliefscollection → collect
  beliefscollection_r1 = "collect-log",
  beliefscollection_r2 = "collect-bio",
  beliefscollection_r3 = "collect-sensitive",
  beliefscollection_r4 = "collect-financial",
  beliefscollection_r5 = "collect-offsite",
  beliefscollection_r6 = "collect-location",
  beliefscollection_r7 = "collect-social",
  
  # beliefsuse → share / related
  beliefsuse_r1 = "anonymized-anonymized",
  beliefsuse_r2 = "share-social",
  beliefsuse_r3 = "share-finance",
  beliefsuse_r4 = "share-ads",
  beliefsuse_r5 = "share-law",
  beliefsuse_r6 = "share-service",
  beliefsuse_r7 = "share-partners",
  beliefsuse_r8 = "personalization-personalization",
  
  # beliefscontrol → control-related
  beliefscontrol_r1 = "change-change",
  beliefscontrol_r2 = "automated-automated",
  beliefscontrol_r3 = "deletion-deletion",
  beliefscontrol_r4 = "storage-storage",
  
  # beliefsquality → no direct mapping
  beliefsquality_r1 = NA,
  beliefsquality_r2 = NA,
  beliefsquality_r3 = NA
)


get_internal_privacy_field <- function(df) {
  
  belief_to_internal <- c(
    # beliefscollection → collect
    beliefscollection_r1 = "collect-log",
    beliefscollection_r2 = "collect-bio",
    beliefscollection_r3 = "collect-sensitive",
    beliefscollection_r4 = "collect-financial",
    beliefscollection_r5 = "collect-offsite",
    beliefscollection_r6 = "collect-location",
    beliefscollection_r7 = "collect-social",
    
    # beliefsuse → share / related
    beliefsuse_r1 = "anonymized-anonymized",
    beliefsuse_r2 = "share-social",
    beliefsuse_r3 = "share-finance",
    beliefsuse_r4 = "share-ads",
    beliefsuse_r5 = "share-law",
    beliefsuse_r6 = "share-service",
    beliefsuse_r7 = "share-partners",
    beliefsuse_r8 = "personalization-Personalization",
    
    # beliefscontrol → control-related
    beliefscontrol_r1 = "change-change",
    beliefscontrol_r2 = "automated-automated",
    beliefscontrol_r3 = "deletion-Deletion",
    beliefscontrol_r4 = "storage-storage",
    
    # beliefsquality → no direct mapping
    beliefsquality_r1 = NA,
    beliefsquality_r2 = NA,
    beliefsquality_r3 = NA
  )
  
  df %>%
    mutate(across(
      .cols = intersect(names(df), names(belief_to_internal)),
      .fns  = ~ .x, # keep values
      .names = "{.col}_internal_field"
    )) %>%
    rename_with(
      ~ belief_to_internal[.x],
      .cols = ends_with("_internal_field")
    )
}

get_privacy_setting_visits <- function() {
  visited_privacy_policy <- read.csv(PRIVACY_POLICY_VISITS_CSV)
  visited_privacy_policy <- visited_privacy_policy %>% filter(!(experiment_id %in% BAD_USERS) & manual_privacy_classification %in% c("PRIVACY_SETTINGS", "PRIVACY_SEARCH"))
  
  visited_privacy_policy <- visited_privacy_policy %>%
    mutate(
      website = str_extract(domain, "(?<=//)[^/]+")
    )
  visited_privacy_policy <- aggregate_time_data(visited_privacy_policy, field="website")
  visited_privacy_policy <- high_level_aggregate(visited_privacy_policy, field="website_aggregated")
  return(visited_privacy_policy)
}

get_privacy_beliefs <- function() {
  dat <- read.csv(SURVEY_MERGED_FINAL_CSV, stringsAsFactors = FALSE)
  
  # beliefscollection → collect
  belief_to_internal <- c(
    beliefscollection_r1 = "collect-log",
    beliefscollection_r2 = "collect-bio",
    beliefscollection_r3 = "collect-sensitive",
    beliefscollection_r4 = "collect-financial",
    beliefscollection_r5 = "collect-offsite",
    beliefscollection_r6 = "collect-location",
    beliefscollection_r7 = "collect-social",
    
    # beliefsuse → share / related
    beliefsuse_r1 = "anonymized-anonymized",
    beliefsuse_r2 = "share-social",
    beliefsuse_r3 = "share-finance",
    beliefsuse_r4 = "share-ads",
    beliefsuse_r5 = "share-law",
    beliefsuse_r6 = "share-service",
    beliefsuse_r7 = "share-partners",
    beliefsuse_r8 = "personalization-personalization",
    
    # beliefscontrol → control-related
    beliefscontrol_r1 = "change-change",
    beliefscontrol_r2 = "automated-automated",
    beliefscontrol_r3 = "deletion-deletion",
    beliefscontrol_r4 = "storage-storage",
    
    # beliefsquality
    beliefsquality_r1 = "quality-content",
    beliefsquality_r2 = "quality-ads",
    beliefsquality_r3 = "quality-navigation"
  )
  
  # Columns we’ll try to use (and keep only those that actually exist)
  belief_cols <- names(belief_to_internal)
  belief_cols_present <- intersect(belief_cols, names(dat))
  
  # Re-scale 1–5 Likert to 0–100 where present
  for (col in belief_cols_present) {
    dat[[col]] <- (dat[[col]] - 1) * 25
  }
  
  # Subset to id + beliefs
  keep_cols <- c("emailid", belief_cols_present)
  keep_cols <- keep_cols[keep_cols %in% names(dat)]
  belief_dat <- dat[, keep_cols, drop = FALSE]
  
  # Rename beliefs_* to beliefs_<key> based on mapping
  new_names <- setNames(
    paste0("beliefs_", unname(belief_to_internal[belief_cols_present])),
    belief_cols_present
  )
  names(belief_dat)[match(names(new_names), names(belief_dat))] <- new_names
  
  return(belief_dat)
}