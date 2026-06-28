library(lubridate)
START_DATE_WAVE_1 <- ymd("2025-06-14")
START_DATE_WAVE_2 <- ymd("2025-06-28")
TREATMENT_DATE_WAVE_1 <- ymd("2025-06-28")
TREATMENT_DATE_WAVE_2 <- ymd("2025-07-12")
COOKIE_TREATMENT_WAVE_1_1 <- ymd("2025-07-26")
COOKIE_TREATMENT_WAVE_1_2 <- ymd("2025-08-02")
COOKIE_TREATMENT_WAVE_2_1 <- ymd("2025-08-09")
COOKIE_TREATMENT_WAVE_2_2 <- ymd("2025-08-16")
END_DATE_WAVE_1 <- ymd("2025-08-09")
END_DATE_WAVE_2 <- ymd("2025-08-23")
BAD_USERS <- c("2607a1f")
SURVEY_WEBSITES <- c(
  "qualtrics", "vercel", "respondent", "cmix", "raterproject", "questionpro",
  "usertesting", "netlify", "primeopinion", "paidviewpoint", "surveymonkey",
  "surveymeasure", "alchemer", "prolific", "raterhub", "mturk", "cloudresearch", "guyaridor"
)
html_key_to_values <- list(
  "anonymized-anonymized" = "control",
  "sell-sell" = "control",
  "automated-automated" = "control",
  "change-change" = "control",
  "deletion-deletion" = "control",
  "personalization-personalization" = "use",
  "collect-bio" = "collect",
  "collect-location" = "collect",
  "collect-financial" = "collect",
  "collect-sensitive" = "collect",
  "share-finance" = "use",
  "storage-storage" = "control",
  "collect-log" = "collect",
  "collect-offsite" = "collect",
  "share-ads" = "use",
  "share-laws" = "use",
  "share-social" = "use",
  "share-service" = "use",
  "collect-social" = "collect"
)

# Canonical privacy-attribute labels + ordering, shared across all figures.
# Replaces the divergent label vectors in beliefs_analysis_overall.R and
# plot_violin.R. Excludes sell-sell (not one of the 19 paper attributes).
# Note: anonymized's conjoint column is "usel_anonymized" (real name, not typo).
PRIVACY_ATTR_MASTER <- data.frame(
  internal_key = c(
    "collect-log", "collect-offsite", "collect-sensitive", "collect-financial",
    "collect-bio", "collect-location", "collect-social",
    "share-law", "share-ads", "share-finance", "share-social",
    "share-partners", "share-service", "personalization-personalization",
    "anonymized-anonymized",
    "change-change", "automated-automated", "deletion-deletion", "storage-storage"
  ),
  feature = c(
    "collect", "collect", "collect", "collect", "collect", "collect", "collect",
    "share", "share", "share", "share", "share", "share", "personalization",
    "anonymized",
    "change", "automated", "deletion", "storage"
  ),
  field = c(
    "log", "offsite", "sensitive", "financial", "bio", "location", "social",
    "law", "ads", "finance", "social", "partners", "service", "personalization",
    "anonymized",
    "change", "automated", "deletion", "storage"
  ),
  label = c(
    "Collects Browsing Behavior on Site",
    "Collects Browsing Behavior on Other Sites",
    "Collects Sensitive Personal Information",
    "Collects Financial Data",
    "Collects Demographic Data",
    "Collects Location Data",
    "Collects Social Media Profiles",
    "Share Data with Law Enforcement",
    "Share Data with Advertisers",
    "Share Data with Financial Providers",
    "Share Data with Social Media",
    "Share Data with Non-Service Providers",
    "Share Data with Service Providers",
    "Use Data to Personalize Experience",
    "Share Data Only After Anonymized",
    "Notified of Policy Changes",
    "Data Auto-Deleted After Time Period",
    "Can Delete Data on Request",
    "Data Stored Securely and Anonymized"
  ),
  category = c(
    "collect", "collect", "collect", "collect", "collect", "collect", "collect",
    "use", "use", "use", "use", "use", "use", "use",
    "use",
    "control", "control", "control", "control"
  ),
  feature_name = c(
    "collection_log", "collection_offsite", "collection_sensitive",
    "collection_financial", "collection_bio", "collection_location",
    "collection_social",
    "use_law", "use_advertising", "use_financial", "use_social",
    "use_partners", "use_service", "use_personalization",
    "usel_anonymized",
    "control_change", "control_automated", "control_delete", "control_storage"
  ),
  stringsAsFactors = FALSE
)

# Master table + true_mean, sorted ascending to match the beliefs figure.
# Recomputes true_mean from privacy_info.csv so order is independent of run
# order. Base R only (keeps values.R dependency-light). sell-sell dropped by merge.
get_privacy_attr_order <- function(
    privacy_info_path = "../data/final_extension_data/privacy_info.csv") {
  
  pi <- read.csv(privacy_info_path, stringsAsFactors = FALSE)
  pi$rating_numeric <- ifelse(pi$rating == "Yes", 1, 0)
  
  truth <- aggregate(rating_numeric ~ feature + field, data = pi, FUN = mean)
  truth$true_mean <- 100 * truth$rating_numeric
  truth$rating_numeric <- NULL
  
  merged <- merge(PRIVACY_ATTR_MASTER, truth,
                  by = c("feature", "field"), all.x = TRUE)
  merged[order(merged$true_mean), ]
}