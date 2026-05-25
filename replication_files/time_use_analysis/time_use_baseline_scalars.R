# =============================================================================
# BASELINE DESCRIPTIVE SCALARS (time_use_analysis)
# =============================================================================
#
# Produces:
#   Section 4 inline scalars [main paper prose, baseline period descriptives]:
#     output/values/baseline_time_use_values.tex
#       3 \newcommand macros:
#         \baselineDailyHoursPrivacy   -- daily hours on privacy_exist sites
#                                         (leisure only)
#         \baselineDailyHoursLeisure   -- daily hours on all leisure sites
#                                         (drop SURVEY_WEBSITES)
#         \baselineDailyHoursAll       -- daily hours including SURVEY_WEBSITES
#
#   Appendix C [tab:top_websites, "Top Websites used by Participants"]:
#     output/values/top_websites_values.tex
#       60 \newcommand macros (15 rows x 4 fields):
#         \topWeb<Rank><Field>
#           Rank  in {One, Two, Three, ..., Fifteen}
#           Field in {Domain, ActiveHours, NUsers, AllHours}
#       The LaTeX tabular itself is hand-written in writeup_v3.tex.
#
# Scope:
#   Non-regression descriptive scalars for the time-use analysis area.
#   Regression-derived scalars and figures stay in
#   time_usage_treatment_effects_SG.R.
#
# Data source:
#   get_clean_time_data() from utils/time_usage_helpers.R. This returns the
#   raw browsing panel BEFORE the SURVEY_WEBSITES drop that
#   time_usage_treatment_effects_SG.R applies before writing the cached CSV.
#   We need SURVEY_WEBSITES rows in scope to compute the "including survey
#   platforms" scalar (Section 4 prose: 3.1 hours/day).
#
# Filter rules (per paper Section 4):
#   - Always drop BAD_USERS.
#   - Restrict to baseline period: weeks_since_intervention in {-2, -1}.
#   - Do NOT filter time_spent > 30 (paper-wide convention for descriptives).
#
#   Scalar-specific scope:
#     \baselineDailyHoursPrivacy: drop SURVEY_WEBSITES, keep privacy_exist
#     \baselineDailyHoursLeisure: drop SURVEY_WEBSITES, all sites
#     \baselineDailyHoursAll    : KEEP SURVEY_WEBSITES, all sites
#
#   tab:top_websites:
#     drop SURVEY_WEBSITES, keep privacy_exist (paper Section 4: "domains...
#     for which we collect privacy information"; footnote: drop
#     survey/recruitment platforms).
#
# Daily-hours formula (Approach A; matches paper prose 1.2 / 2.4 / 3.1):
#   For each user:
#     daily_hours_u = mean over the user's active days in the subset of
#                     sum(time_spent on that day) / 3600
#   Scalar = mean(daily_hours_u) across users.
#
# Inputs:
#   Inputs of get_clean_time_data() (see utils/time_usage_helpers.R header).
#
# Dependencies:
#   replication_files/utils/values.R              (BAD_USERS, SURVEY_WEBSITES)
#   replication_files/utils/time_usage_helpers.R  (get_clean_time_data)
#   savetexvalue (devtools::install_github("Ori-Shoham/savetexvalue"))
#
# Outputs:
#   output/values/baseline_time_use_values.tex
#   output/values/top_websites_values.tex
# =============================================================================

library(tidyverse)
library(savetexvalue)

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")

VALUES_DIR <- "output/values/"

# =============================================================================
# Load raw time-use panel (includes SURVEY_WEBSITES rows)
# =============================================================================

raw <- get_clean_time_data()

# Baseline period + drop BAD_USERS. Two slices for the two scopes:
#   base_with_survey: includes labor/recruitment platforms (for 3.1 scalar)
#   base_leisure    : excludes them (for top_websites + 1.2 / 2.4 scalars)
base_with_survey <- raw %>%
  filter(!(experiment_id %in% BAD_USERS)) %>%
  filter(weeks_since_intervention %in% c(-2, -1))

base_leisure <- base_with_survey %>%
  filter(!str_detect(website, str_c(SURVEY_WEBSITES, collapse = "|")))

# =============================================================================
# Section 4 inline scalars: daily hours per user (Approach A)
# =============================================================================

# Per-user mean of daily hours over the user's active days in `df_subset`,
# then mean across users. Users with zero active days in the subset
# contribute no daily observations and are dropped from the outer mean.
approach_A <- function(df_subset) {
  df_subset %>%
    group_by(experiment_id, date) %>%
    summarise(daily_hours = sum(time_spent) / 3600, .groups = "drop_last") %>%
    group_by(experiment_id) %>%
    summarise(per_user_mean = mean(daily_hours), .groups = "drop") %>%
    summarise(m = mean(per_user_mean)) %>%
    pull(m)
}

baseline_hours_privacy <- approach_A(base_leisure %>% filter(privacy_exist))
baseline_hours_leisure <- approach_A(base_leisure)
baseline_hours_all     <- approach_A(base_with_survey)

cat("Baseline daily hours (Approach A):\n")
cat("  Privacy sites (leisure)        :", round(baseline_hours_privacy, 3),
    " (paper prose: 1.2)\n")
cat("  All leisure sites              :", round(baseline_hours_leisure, 3),
    " (paper prose: 2.4)\n")
cat("  All sites incl. SURVEY_WEBSITES:", round(baseline_hours_all, 3),
    " (paper prose: 3.1)\n\n")

# Write macros. savetexvalue is append-only -> wipe first.
hours_values_file <- "baseline_time_use_values"
hours_values_full <- file.path(VALUES_DIR, paste0(hours_values_file, ".tex"))
if (file.exists(hours_values_full)) file.remove(hours_values_full)

save_tex_value(
  values    = baseline_hours_privacy,
  names     = "baselineDailyHoursPrivacy",
  file_name = hours_values_file,
  path      = VALUES_DIR,
  accuracy  = 0.1
)
save_tex_value(
  values    = baseline_hours_leisure,
  names     = "baselineDailyHoursLeisure",
  file_name = hours_values_file,
  path      = VALUES_DIR,
  accuracy  = 0.1
)
save_tex_value(
  values    = baseline_hours_all,
  names     = "baselineDailyHoursAll",
  file_name = hours_values_file,
  path      = VALUES_DIR,
  accuracy  = 0.1
)
cat("Saved:", hours_values_full, "\n\n")

# =============================================================================
# Appendix C [tab:top_websites]: Top 15 websites in the baseline period
# Universe: privacy_exist sites, leisure only.
# =============================================================================

# Capitalization lookup for known domains; str_to_title() is the fallback
# for any new top-15 domain that appears as the sample evolves. NOTE:
# x.com etc. aggregate to slug "x" upstream (see aggregate_time_data +
# high_level_aggregate in time_usage_helpers.R after the twitter -> x fix).
website_display_name <- function(slugs) {
  lookup <- c(
    youtube   = "Youtube",
    google    = "Google",
    facebook  = "Facebook",
    amazon    = "Amazon",
    reddit    = "Reddit",
    netflix   = "Netflix",
    chatgpt   = "Chatgpt",
    twitch    = "Twitch",
    x         = "X",
    hulu      = "Hulu",
    yahoo     = "Yahoo",
    walmart   = "Walmart",
    microsoft = "Microsoft",
    ebay      = "eBay",
    linkedin  = "LinkedIn",
    canva     = "Canva"
  )
  out <- lookup[slugs]
  fallback <- stringr::str_to_title(slugs)
  ifelse(is.na(out), fallback, out)
}

# English ordinals (LaTeX command names cannot contain digits).
rank_word <- c("One", "Two", "Three", "Four", "Five",
               "Six", "Seven", "Eight", "Nine", "Ten",
               "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen")

# Restrict to privacy-info sites (leisure already excluded above).
filtered <- base_leisure %>% filter(privacy_exist)

users_all <- filtered %>%
  summarise(n = n_distinct(experiment_id)) %>%
  pull(n)

summary_table <- filtered %>%
  group_by(website_aggregated_high_level, experiment_id, date) %>%
  summarise(daily_hours = sum(time_spent) / 3600, .groups = "drop_last") %>%
  group_by(website_aggregated_high_level, experiment_id) %>%
  summarise(avg_daily_hours = mean(daily_hours), .groups = "drop_last") %>%
  group_by(website_aggregated_high_level) %>%
  summarise(
    avg_daily_hours_per_active_user = mean(avg_daily_hours),
    n_users                         = n_distinct(experiment_id),
    .groups = "drop"
  ) %>%
  filter(n_users >= 50) %>%
  mutate(avg_daily_hours_all_users = (avg_daily_hours_per_active_user * n_users) / users_all) %>%
  arrange(desc(avg_daily_hours_all_users)) %>%
  slice_head(n = 15) %>%
  mutate(
    rank         = row_number(),
    domain_label = website_display_name(website_aggregated_high_level)
  )

cat("Top 15 websites (baseline, privacy_exist, leisure, n_users >= 50):\n")
print(summary_table)
cat("\nTotal users in baseline period (users_all):", users_all, "\n\n")

# Write 60 macros via savetexvalue. Append-only -> wipe first.
topweb_values_file <- "top_websites_values"
topweb_values_full <- file.path(VALUES_DIR, paste0(topweb_values_file, ".tex"))
if (file.exists(topweb_values_full)) file.remove(topweb_values_full)

# Field 1: Domain (string passthrough).
save_tex_value(
  values    = summary_table$domain_label,
  names     = paste0("topWeb", rank_word[summary_table$rank], "Domain"),
  file_name = topweb_values_file,
  path      = VALUES_DIR
)

# Field 2: Daily Hours (per Active User) - 2 decimals.
save_tex_value(
  values    = summary_table$avg_daily_hours_per_active_user,
  names     = paste0("topWeb", rank_word[summary_table$rank], "ActiveHours"),
  file_name = topweb_values_file,
  path      = VALUES_DIR,
  accuracy  = 0.01
)

# Field 3: Number of Users - bare integer (pass as character so
# savetexvalue's scales::number() does not insert thousands commas / .00).
save_tex_value(
  values    = as.character(summary_table$n_users),
  names     = paste0("topWeb", rank_word[summary_table$rank], "NUsers"),
  file_name = topweb_values_file,
  path      = VALUES_DIR
)

# Field 4: Daily Hours (over all users) - 2 decimals.
save_tex_value(
  values    = summary_table$avg_daily_hours_all_users,
  names     = paste0("topWeb", rank_word[summary_table$rank], "AllHours"),
  file_name = topweb_values_file,
  path      = VALUES_DIR,
  accuracy  = 0.01
)

cat("Saved:", topweb_values_full, "\n")