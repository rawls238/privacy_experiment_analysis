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

