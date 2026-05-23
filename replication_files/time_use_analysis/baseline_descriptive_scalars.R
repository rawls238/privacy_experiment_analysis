# =============================================================================
# BASELINE DESCRIPTIVE SCALARS (time_use_analysis)
# =============================================================================
#
# Produces:
#   Appendix C [tab:top_websites, "Top Websites used by Participants"]:
#     output/values/top_websites_values.tex
#       60 \newcommand macros (15 rows x 4 fields):
#         \topWeb<Rank><Field>
#           Rank  in {One, Two, Three, ..., Fifteen}
#           Field in {Domain, ActiveHours, NUsers, AllHours}
#       The LaTeX tabular itself is hand-written in writeupv3/writeup_v3.tex.
#
# Scope:
#   This script is for non-regression descriptive scalars in the time-use
#   analysis area. It does NOT produce figures or regression tables.
#   Figures (Fig 9, Fig 10) and regression-derived scalars stay in
#   time_usage_treatment_effects_SG.R.
#
# Filter rules (per paper text + footnote):
#   - drop BAD_USERS
#   - drop SURVEY_WEBSITES (paper footnote: "drop time spent on survey
#     websites e.g. qualtrics, surveymonkey and recruitment platforms e.g.
#     prolific, cloudresearch, mturk, paidviewpoint")
#   - keep only privacy_exist (paper text: "domains... for which we collect
#     privacy information")
#   - restrict to baseline period: weeks_since_intervention in {-2, -1}
#   - DO NOT filter time_spent > 30
#   - n_users is the count of distinct experiment_ids that visited the site
#     within the baseline period
#
# Inputs:
#   ../data/processed_data/joined_time_data.csv
#
# Dependencies:
#   replication_files/utils/values.R   (BAD_USERS)
#   savetexvalue (devtools::install_github("Ori-Shoham/savetexvalue"))
#
# Outputs:
#   output/values/top_websites_values.tex
# =============================================================================

library(tidyverse)
library(savetexvalue)

# Working directory: code_github root so relative paths resolve.
setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/values.R")

VALUES_DIR <- "output/values/"

# =============================================================================
# Load + minimal cleaning
# =============================================================================

full_time_dat <- read.csv("../data/processed_data/joined_time_data.csv")

# Drop BAD_USERS and labor/survey/recruitment platforms. SURVEY_WEBSITES is
# applied via str_detect on the raw website column (matches subdomains too).
full_time_dat <- full_time_dat %>%
  filter(!(experiment_id %in% BAD_USERS)) %>%
  filter(!str_detect(website, str_c(SURVEY_WEBSITES, collapse = "|")))

# =============================================================================
# App C [tab:top_websites]: Top 15 websites in the baseline period
# =============================================================================

# --- Capitalization lookup for known domains; str_to_title() is the fallback
# --- for any new top-15 domain that appears as the sample evolves.
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

# --- English ordinals (LaTeX command names cannot contain digits).
rank_word <- c("One", "Two", "Three", "Four", "Five",
               "Six", "Seven", "Eight", "Nine", "Ten",
               "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen")

# --- Restrict to baseline period AND to sites for which privacy info was
# --- collected. All four columns of the table are computed on this slice.
filtered <- full_time_dat %>%
  filter(weeks_since_intervention %in% c(-2, -1) & privacy_exist)

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

cat("Top 15 websites (baseline weeks -2, -1, n_users >= 50):\n")
print(summary_table)
cat("\nTotal users in baseline period (users_all):", users_all, "\n\n")

# =============================================================================
# Write 60 macros via savetexvalue.
# Append-only: wipe the file first to avoid duplicate \newcommand entries on
# reruns.
# =============================================================================

values_file <- "top_websites_values"
values_full <- file.path(VALUES_DIR, paste0(values_file, ".tex"))
if (file.exists(values_full)) file.remove(values_full)

# Field 1: Domain (string passthrough).
save_tex_value(
  values    = summary_table$domain_label,
  names     = paste0("topWeb", rank_word[summary_table$rank], "Domain"),
  file_name = values_file,
  path      = VALUES_DIR
)

# Field 2: Daily Hours (per Active User) - 2 decimals.
save_tex_value(
  values    = summary_table$avg_daily_hours_per_active_user,
  names     = paste0("topWeb", rank_word[summary_table$rank], "ActiveHours"),
  file_name = values_file,
  path      = VALUES_DIR,
  accuracy  = 0.01
)

# Field 3: Number of Users - bare integer (pass as character to bypass
# savetexvalue's scales::number() default, which would add a thousands
# separator and ".00").
save_tex_value(
  values    = as.character(summary_table$n_users),
  names     = paste0("topWeb", rank_word[summary_table$rank], "NUsers"),
  file_name = values_file,
  path      = VALUES_DIR
)

# Field 4: Daily Hours (over all users) - 2 decimals.
save_tex_value(
  values    = summary_table$avg_daily_hours_all_users,
  names     = paste0("topWeb", rank_word[summary_table$rank], "AllHours"),
  file_name = values_file,
  path      = VALUES_DIR,
  accuracy  = 0.01
)

cat("Saved:", values_full, "\n")
