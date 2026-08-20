#!/usr/bin/env Rscript
# =============================================================================
# cookie_deletion_overall_effects_v3.R
#
# Appendix E.1: Overall Effects and Timing of Third-Party Cookie Deletion
#
# This script replaces the E.1 portion of the former cookie_deletion.R.
# It produces the same paper-facing output filenames and preserves the existing
# macro names used by Overleaf.
#
# Design
#   - Treatment: early cookie deletion.
#   - Control: late cookie deletion, before its deletion begins.
#   - Window: tau = -7,...,6 around the early-deletion date in each wave.
#   - Main dwell rule: participant-website-days with time_spent > 30 seconds.
#   - Survey-platform websites are excluded from the main analysis.
#   - Cookie and browsing-time samples are constructed separately.
#
# Estimation
#   - Cookie outcomes: participant-website-day PPML with participant, website,
#     and calendar-date fixed effects; participant-clustered standard errors.
#   - Cookies per Visit uses Set-Cookie actions as the outcome and
#     log(visit_count) as an offset.
#   - Unique Cookies uses the raw count of distinct cookie identities observed
#     during the participant-website-day, without a visit offset.
#   - Overall timing figure: descriptive daily totals for the early-deletion
#     group. Cookie and time streams are constructed separately and each
#     fourteen-day series is standardized using its own mean and SD.
#
# Paper-facing outputs retained
#   output/figures/cookie_deletion_percentage_change.pdf
#   output/tables/cookie_deletion_did_regression.tex
#   output/figures/cpv_baseline_by_category.pdf
#   output/figures/cpv_heterogeneity_by_website_category.pdf
#   output/values/data_sharing_cookie_values.tex
#   output/values/data_sharing_cookie_str_values.tex
#
# The two category-figure filenames retain their legacy "cpv" names so the
# current Overleaf inputs continue to work. Their V3 content uses Unique
# Third-Party Cookies, the paper's main cookie measure.
# =============================================================================

suppressMessages({
  library(jsonlite)  # Must precede time_usage_helpers.R.
  library(data.table)
  library(fst)
  library(fixest)
  library(ggplot2)
})

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/number_format_helpers.R")
source("replication_files/utils/tex_helpers.R")
source("replication_files/utils/plot_rules.R")

setDTthreads(4L)
setFixest_nthreads(4L)
options(datatable.verbose = FALSE, scipen = 999)


# =============================================================================
# 0. CONSTANTS AND HELPERS
# =============================================================================

TIME_PATH <- "../data/final_extension_data/time_data_2.csv"
COOKIE_PATH <- "../data/processed_data/panel_cookies.fst"
ASSIGNMENT_PATH <- paste0(
  "../data/final_extension_data/",
  "experiment_conditions_pilot_july_2024.csv"
)

FIGURES_DIR <- "output/figures"
TABLES_DIR <- "output/tables"
VALUES_PATH <- "output/values/data_sharing_cookie_values.tex"
STR_VALUES_PATH <- "output/values/data_sharing_cookie_str_values.tex"

EVENT_FIGURE_PATH <- file.path(
  FIGURES_DIR,
  "cookie_deletion_percentage_change.pdf"
)
MAIN_TABLE_PATH <- file.path(
  TABLES_DIR,
  "cookie_deletion_did_regression.tex"
)
BASELINE_FIGURE_PATH <- file.path(
  FIGURES_DIR,
  "cpv_baseline_by_category.pdf"
)
CATEGORY_FIGURE_PATH <- file.path(
  FIGURES_DIR,
  "cpv_heterogeneity_by_website_category.pdf"
)

KEY <- c("experiment_id", "website", "date")
TAU_MIN <- -7L
TAU_MAX <- 6L
MIN_SECONDS <- 30
VLINE_X <- -0.5

FIG_W <- 8
FIG_H_EVENT <- 5
FIG_H_CATEGORY <- 5
POINT_COLOR <- "gray30"

SIGNIF <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)
BAD_USERS_E <- union(BAD_USERS, c("6ccc7d5", "7d6864c"))

dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(VALUES_PATH), recursive = TRUE, showWarnings = FALSE)

require_columns <- function(x, required, label) {
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "))
  }
}

extract_percent <- function(model, term = "post_treated") {
  if (!term %in% names(coef(model))) {
    stop("Model does not contain coefficient: ", term)
  }
  
  beta <- unname(coef(model)[term])
  ci <- confint(model, term)
  
  data.table(
    beta = beta,
    se = unname(se(model)[term]),
    percent_effect = 100 * (exp(beta) - 1),
    conf_low = 100 * (exp(as.numeric(ci[1L, 1L])) - 1),
    conf_high = 100 * (exp(as.numeric(ci[1L, 2L])) - 1),
    p_value = unname(pvalue(model)[term]),
    observations = nobs(model)
  )
}

# Both Appendix E scripts update the same macro file. Replace only the macros
# owned by this script so running E.1 never deletes E.2's macros, and vice versa.
upsert_tex_macros <- function(path, values) {
  lines <- if (file.exists(path)) {
    readLines(path, warn = FALSE)
  } else {
    character()
  }
  
  for (name in names(values)) {
    trimmed <- trimws(lines)
    prefix_plain <- paste0("\\newcommand\\", name)
    prefix_braced <- paste0("\\newcommand{\\", name, "}")
    lines <- lines[
      !startsWith(trimmed, prefix_plain) &
        !startsWith(trimmed, prefix_braced)
    ]
    lines <- c(
      lines,
      sprintf("\\newcommand\\%s {%s}", name, values[[name]])
    )
  }
  
  writeLines(lines, path)
}

upsert_string_macro <- function(path, name, value) {
  lines <- if (file.exists(path)) {
    readLines(path, warn = FALSE)
  } else {
    character()
  }
  
  trimmed <- trimws(lines)
  prefix_plain <- paste0("\\newcommand\\", name)
  prefix_braced <- paste0("\\newcommand{\\", name, "}")
  lines <- lines[
    !startsWith(trimmed, prefix_plain) &
      !startsWith(trimmed, prefix_braced)
  ]
  lines <- c(lines, sprintf("\\newcommand{\\%s}{%s}", name, value))
  writeLines(lines, path)
}

plot_category_effects <- function(results, y_label) {
  ggplot(results, aes(x = category_factor, y = percent_effect)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray50"
    ) +
    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high),
      width = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH,
      color = POINT_COLOR
    ) +
    geom_point(size = POINT_SIZE, color = POINT_COLOR) +
    labs(x = NULL, y = y_label) +
    theme_privacy_experiment(show_grid_x = FALSE, show_grid_y = TRUE) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


# =============================================================================
# 1. RANDOMIZED EARLY/LATE ASSIGNMENT
# =============================================================================

assignment <- fread(ASSIGNMENT_PATH)
require_columns(
  assignment,
  c(
    "experiment_id",
    "in_experiment",
    "wave_id",
    "cookie_treatment_idx"
  ),
  "Assignment file"
)

assignment[, experiment_id := as.character(experiment_id)]
assignment[, wave_id := suppressWarnings(as.integer(wave_id))]
assignment[wave_id == 3L, wave_id := 2L]
assignment[, cookie_treatment_idx := suppressWarnings(
  as.integer(cookie_treatment_idx)
)]

assignment <- assignment[
  tolower(as.character(in_experiment)) == "true" &
    !experiment_id %chin% BAD_USERS_E &
    wave_id %in% c(1L, 2L) &
    cookie_treatment_idx %in% c(1L, 2L),
  .(experiment_id, wave_id, cookie_treatment_idx)
]

if (anyDuplicated(assignment$experiment_id)) {
  stop("Duplicate experiment_id in randomized assignment sample")
}

assignment[, treated := as.integer(cookie_treatment_idx == 1L)]
assignment[, anchor := as.Date("2025-08-09")]
assignment[wave_id == 1L, anchor := as.Date("2025-07-26")]

cat(sprintf(
  "Randomized sample: %s participants\n",
  format(nrow(assignment), big.mark = ",")
))


# =============================================================================
# 2. BROWSING DATA: SHARED ELIGIBILITY RULE, NO COOKIE JOIN
# =============================================================================

time_panel <- get_time_panel(
  path = TIME_PATH,
  min_seconds = NULL,
  verbose = TRUE
)
setDT(time_panel)
require_columns(
  time_panel,
  c(KEY, "time_spent", "visit_count"),
  "Time panel"
)

time_panel[, `:=`(
  experiment_id = as.character(experiment_id),
  website = as.character(website),
  date = as.Date(date)
)]

if (anyDuplicated(time_panel, by = KEY)) {
  stop("Duplicate participant-website-date key in time panel")
}

# Match the established Appendix E exclusion of survey-platform websites.
site_df <- data.frame(
  website = unique(time_panel$website),
  stringsAsFactors = FALSE
)
site_df <- aggregate_time_data(site_df, field = "website")
site_df <- high_level_aggregate(site_df, field = "website_aggregated")
site_lookup <- as.data.table(unique(
  site_df[, c("website", "website_aggregated_high_level")]
))
site_lookup[, is_survey :=
              tolower(website_aggregated_high_level) %in% tolower(SURVEY_WEBSITES)]
site_lookup[is.na(is_survey), is_survey := FALSE]

time_panel <- merge(
  time_panel,
  site_lookup[, .(website, website_aggregated_high_level, is_survey)],
  by = "website",
  all.x = TRUE,
  sort = FALSE
)
time_panel[is.na(is_survey), is_survey := FALSE]

time_panel <- merge(
  time_panel[is_survey == FALSE],
  assignment,
  by = "experiment_id",
  all = FALSE,
  sort = FALSE
)
time_panel[, tau := as.integer(date - anchor)]

# Main Appendix E dwell rule. The underlying full stream remains available
# above; only the analysis rows are restricted here.
time_analysis <- time_panel[
  tau >= TAU_MIN & tau <= TAU_MAX &
    !is.na(time_spent) & time_spent > MIN_SECONDS &
    !is.na(visit_count) & visit_count > 0
]
time_analysis[, post_treated := as.integer(tau >= 0L) * treated]

cat(sprintf(
  paste0(
    "Browsing analysis: %s participant-website-days | ",
    "%s participants | %s websites\n"
  ),
  format(nrow(time_analysis), big.mark = ","),
  format(uniqueN(time_analysis$experiment_id), big.mark = ","),
  format(uniqueN(time_analysis$website), big.mark = ",")
))


# =============================================================================
# 3. SEPARATE COOKIE ANALYSIS DATA
# =============================================================================

cookie_columns <- c(
  KEY,
  "tracker_record_observed",
  "set_cookie_actions_1st_p",
  "set_cookie_actions_3rd_p",
  "unique_snapshot_cookies_1st_p",
  "unique_snapshot_cookies_3rd_p"
)
cookie_outcomes <- c(
  "set_cookie_actions_1st_p",
  "set_cookie_actions_3rd_p",
  "unique_snapshot_cookies_1st_p",
  "unique_snapshot_cookies_3rd_p"
)

cookies <- read_fst(
  COOKIE_PATH,
  columns = cookie_columns,
  as.data.table = TRUE
)
require_columns(cookies, cookie_columns, "Cookie panel")
cookies[, `:=`(
  experiment_id = as.character(experiment_id),
  website = as.character(website),
  date = as.Date(date)
)]

if (anyDuplicated(cookies, by = KEY)) {
  stop("Duplicate participant-website-date key in cookie panel")
}

# A cookie outcome is analyzed only when the validated cookie panel contains a
# measurement for that browsing row. Unmatched browsing rows remain missing;
# matched rows with zero cookie activity remain valid zeros.
cookie_analysis <- merge(
  time_analysis,
  cookies,
  by = KEY,
  all = FALSE,
  sort = FALSE
)

if (anyNA(cookie_analysis[, ..cookie_outcomes])) {
  stop("Matched cookie rows contain missing cookie outcomes")
}
if (any(cookie_analysis$visit_count <= 0)) {
  stop("Cookies-per-visit exposure must be positive")
}

if (any(vapply(
  cookie_outcomes,
  function(v) any(cookie_analysis[[v]] < 0),
  logical(1)
))) {
  stop("Cookie outcomes must be nonnegative")
}

cat(sprintf(
  paste0(
    "Cookie analysis: %s matched rows (%.1f%% of eligible browsing rows) | ",
    "%s participants | %s websites\n"
  ),
  format(nrow(cookie_analysis), big.mark = ","),
  100 * nrow(cookie_analysis) / nrow(time_analysis),
  format(uniqueN(cookie_analysis$experiment_id), big.mark = ","),
  format(uniqueN(cookie_analysis$website), big.mark = ",")
))


# =============================================================================
# 4. DESCRIPTIVE DAILY TRENDS: UNIQUE COOKIES AND BROWSING TIME
# =============================================================================

# Browsing-time stream: sum all retained time rows for early-deletion users on
# each relative day. This stream is not restricted to cookie-matched rows.
time_trend <- time_analysis[treated == 1L, .(
  raw_total = sum(time_spent, na.rm = TRUE)
), by = tau]
time_trend[, outcome := "Browsing Time"]

# Cookie stream: use the validated cookie panel directly rather than joining it
# to the browsing-time stream. Apply the survey-site exclusion independently.
cookie_site_df <- data.frame(
  website = unique(cookies$website),
  stringsAsFactors = FALSE
)
cookie_site_df <- aggregate_time_data(cookie_site_df, field = "website")
cookie_site_df <- high_level_aggregate(
  cookie_site_df,
  field = "website_aggregated"
)
cookie_site_lookup <- as.data.table(unique(
  cookie_site_df[, c("website", "website_aggregated_high_level")]
))
cookie_site_lookup[, is_survey :=
                     tolower(website_aggregated_high_level) %in% tolower(SURVEY_WEBSITES)]
cookie_site_lookup[is.na(is_survey), is_survey := FALSE]

cookie_stream <- merge(
  cookies,
  cookie_site_lookup[, .(website, is_survey)],
  by = "website",
  all.x = TRUE,
  sort = FALSE
)
cookie_stream[is.na(is_survey), is_survey := FALSE]
cookie_stream <- merge(
  cookie_stream[is_survey == FALSE],
  assignment,
  by = "experiment_id",
  all = FALSE,
  sort = FALSE
)
cookie_stream[, tau := as.integer(date - anchor)]
cookie_stream <- cookie_stream[
  treated == 1L & tau >= TAU_MIN & tau <= TAU_MAX
]

cookie_trend <- cookie_stream[, .(
  raw_total = sum(unique_snapshot_cookies_3rd_p, na.rm = TRUE)
), by = tau]
cookie_trend[, outcome := "Unique Third-Party Cookies"]

# Complete each stream to the fourteen relative days. A day with no received
# rows contributes a recorded total of zero. This is a descriptive statement
# about the received data streams, not an assertion of true zero activity.
complete_trend <- function(x, outcome_label) {
  out <- merge(
    data.table(tau = TAU_MIN:TAU_MAX),
    x[, .(tau, raw_total)],
    by = "tau",
    all.x = TRUE,
    sort = TRUE
  )
  out[is.na(raw_total), raw_total := 0]
  out[, outcome := outcome_label]
  out[]
}

event_plot <- rbindlist(list(
  complete_trend(cookie_trend, "Unique Third-Party Cookies"),
  complete_trend(time_trend, "Browsing Time")
))

# Standardization only places the two outcomes on a common scale. It does not
# estimate a treatment effect or change either series' time pattern.
event_plot[, series_mean := mean(raw_total), by = outcome]
event_plot[, series_sd := sd(raw_total), by = outcome]
if (any(!is.finite(event_plot$series_sd) | event_plot$series_sd <= 0)) {
  stop("At least one daily trend has an invalid standard deviation")
}
event_plot[, standardized_level :=
             (raw_total - series_mean) / series_sd]

outcome_order <- c(
  "Unique Third-Party Cookies",
  "Browsing Time"
)
event_plot[, outcome := factor(outcome, levels = outcome_order)]
setorder(event_plot, outcome, tau)

outcome_lines <- c(
  "Unique Third-Party Cookies" = "solid",
  "Browsing Time" = "dashed"
)

g_event <- ggplot(
  event_plot,
  aes(
    x = tau,
    y = standardized_level,
    group = outcome,
    linetype = outcome
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_vline(
    xintercept = VLINE_X,
    linetype = "solid",
    color = "gray50"
  ) +
  annotate(
    "text",
    x = VLINE_X + 0.1,
    y = Inf,
    label = "Deletion begins",
    hjust = 0,
    vjust = 1.5,
    size = 4.2,
    color = TEXT_COLOR
  ) +
  geom_line(linewidth = LINE_WIDTH, color = POINT_COLOR) +
  geom_point(size = POINT_SIZE, color = POINT_COLOR) +
  scale_x_continuous(breaks = TAU_MIN:TAU_MAX) +
  scale_linetype_manual(
    name = "Outcome",
    values = outcome_lines,
    breaks = outcome_order
  ) +
  labs(
    x = "Days Relative to Early-Group Deletion Start",
    y = "Deviation from 14-Day Mean"
  ) +
  theme_privacy_experiment(show_grid_x = TRUE, show_grid_y = TRUE) +
  theme(
    legend.position = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  )

ggsave(
  EVENT_FIGURE_PATH,
  g_event,
  width = FIG_W,
  height = FIG_H_EVENT
)
cat("Saved: ", EVENT_FIGURE_PATH, "\n", sep = "")


# =============================================================================
# 5. MAIN COOKIE TABLE: FIRST/THIRD PARTY x CPV/UNIQUE COOKIES
# =============================================================================

m_cpv_1p <- fepois(
  set_cookie_actions_1st_p ~ post_treated |
    experiment_id + website + date,
  data = cookie_analysis,
  offset = ~log(visit_count),
  cluster = ~experiment_id,
  notes = FALSE
)

m_cpv_3p <- fepois(
  set_cookie_actions_3rd_p ~ post_treated |
    experiment_id + website + date,
  data = cookie_analysis,
  offset = ~log(visit_count),
  cluster = ~experiment_id,
  notes = FALSE
)

m_uc_1p <- fepois(
  unique_snapshot_cookies_1st_p ~ post_treated |
    experiment_id + website + date,
  data = cookie_analysis,
  cluster = ~experiment_id,
  notes = FALSE
)

m_uc_3p <- fepois(
  unique_snapshot_cookies_3rd_p ~ post_treated |
    experiment_id + website + date,
  data = cookie_analysis,
  cluster = ~experiment_id,
  notes = FALSE
)

DICT_COOKIE <- c(
  post_treated = "Post $\\times$ Cookie Deletion",
  experiment_id = "Participant FE",
  website = "Website FE",
  date = "Date FE"
)

main_table <- etable(
  m_uc_1p,
  m_uc_3p,
  m_cpv_1p,
  m_cpv_3p,
  headers = c(
    "Unique First-Party Cookies",
    "Unique Third-Party Cookies",
    "First-Party Cookies per Visit",
    "Third-Party Cookies per Visit"
  ),
  dict = DICT_COOKIE,
  digits = 3,
  signif.code = SIGNIF,
  depvar = FALSE,
  fitstat = c("n", "pr2"),
  tex = TRUE
)

write_tabular_only(main_table, file = MAIN_TABLE_PATH)
cat("Saved: ", MAIN_TABLE_PATH, "\n", sep = "")


# =============================================================================
# 6. BASELINE UNIQUE THIRD-PARTY COOKIES BY WEBSITE CATEGORY
# =============================================================================

domain_class <- get_domain_classification()
setDT(domain_class)
require_columns(
  domain_class,
  c("name_aggregated_high_level", "category_level_1"),
  "Domain classification"
)

domain_class_slim <- unique(domain_class[, .(
  website_aggregated_high_level = name_aggregated_high_level,
  category = category_level_1
)])

cookie_analysis <- merge(
  cookie_analysis,
  domain_class_slim,
  by = "website_aggregated_high_level",
  all.x = TRUE,
  sort = FALSE
)

cookie_by_category <- cookie_analysis[
  !is.na(category) & category != ""
]

big_categories <- cookie_by_category[, .(
  observations = .N,
  participants = uniqueN(experiment_id)
), by = category][
  observations >= 500L & participants >= 50L,
  category
]

if (!length(big_categories)) {
  stop("No website categories satisfy the E.1 sample thresholds")
}

site_baseline <- cookie_by_category[
  tau < 0L & category %in% big_categories,
  .(
    site_mean_unique_3p = mean(
      unique_snapshot_cookies_3rd_p,
      na.rm = TRUE
    )
  ),
  by = .(website, category)
]

baseline_stats <- site_baseline[, .(
  p25 = quantile(site_mean_unique_3p, 0.25),
  median = median(site_mean_unique_3p),
  p75 = quantile(site_mean_unique_3p, 0.75)
), by = category][order(median)]
baseline_stats[, category_factor := factor(category, levels = category)]

g_baseline <- ggplot(
  baseline_stats,
  aes(x = category_factor, y = median)
) +
  geom_errorbar(
    aes(ymin = p25, ymax = p75),
    width = ERRORBAR_WIDTH,
    linewidth = LINE_WIDTH,
    color = POINT_COLOR
  ) +
  geom_point(size = POINT_SIZE, color = POINT_COLOR) +
  labs(x = NULL, y = "Baseline Unique Third-Party Cookies") +
  theme_privacy_experiment(show_grid_x = FALSE, show_grid_y = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  BASELINE_FIGURE_PATH,
  g_baseline,
  width = FIG_W,
  height = FIG_H_CATEGORY
)
cat("Saved: ", BASELINE_FIGURE_PATH, "\n", sep = "")


# =============================================================================
# 7. COOKIE-DELETION EFFECTS BY WEBSITE CATEGORY
# =============================================================================

fit_category_uc_ppml <- function(category_name) {
  d <- cookie_by_category[category == category_name]
  
  model <- tryCatch(
    fepois(
      unique_snapshot_cookies_3rd_p ~ post_treated |
        experiment_id + website + date,
      data = d,
      cluster = ~experiment_id,
      notes = FALSE
    ),
    error = function(e) {
      message("Category model failed for ", category_name, ": ", e$message)
      NULL
    }
  )
  
  if (is.null(model) || !"post_treated" %in% names(coef(model))) {
    return(NULL)
  }
  
  result <- extract_percent(model)
  result[, category := category_name]
  result
}

uc_category_results <- rbindlist(lapply(
  sort(big_categories),
  fit_category_uc_ppml
))

if (!nrow(uc_category_results)) {
  stop("No unique-cookie category-level PPML models were estimable")
}

setorder(uc_category_results, percent_effect)
uc_category_results[, category_factor := factor(category, levels = category)]

ggsave(
  CATEGORY_FIGURE_PATH,
  plot_category_effects(
    uc_category_results,
    "Estimated Effect on Unique Third-Party Cookies (%)"
  ),
  width = FIG_W,
  height = FIG_H_CATEGORY
)
cat("Saved: ", CATEGORY_FIGURE_PATH, "\n", sep = "")


# =============================================================================
# 8. PRESERVE EXISTING OVERLEAF MACROS
# =============================================================================

cpv_1p_result <- extract_percent(m_cpv_1p)
cpv_3p_result <- extract_percent(m_cpv_3p)
uc_1p_result <- extract_percent(m_uc_1p)
uc_3p_result <- extract_percent(m_uc_3p)

n_significant <- function(x) {
  sum(x$conf_low > 0 | x$conf_high < 0)
}
n_significant_negative <- function(x) sum(x$conf_high < 0)
n_significant_positive <- function(x) sum(x$conf_low > 0)

macro_values <- c(
  # Main Table E.1 results. Existing names remain unchanged.
  cookieCpvCoef = format_coef(cpv_3p_result$beta),
  cookieCpvPct = format_pct(abs(cpv_3p_result$percent_effect)),
  ucMainCoef = format_coef(uc_3p_result$beta),
  ucMainPct = format_pct(abs(uc_3p_result$percent_effect)),
  # Unique-cookie category macros for the revised Figures E.2 and E.3.
  baseCatTopMedUc = format_count(round(max(baseline_stats$median))),
  baseCatBottomMedUc = format_count(round(min(baseline_stats$median))),
  nSigUcCategories = as.character(n_significant(uc_category_results)),
  nSigNegUcCategories = as.character(
    n_significant_negative(uc_category_results)
  ),
  nSigPosUcCategories = as.character(
    n_significant_positive(uc_category_results)
  ),
  nUcCategories = as.character(nrow(uc_category_results)),
  # First-party comparison results in Table E.1.
  cookieFirstPartyCpvCoef = format_coef(cpv_1p_result$beta),
  cookieFirstPartyCpvPct = format_pct(cpv_1p_result$percent_effect),
  cookieFirstPartyUcCoef = format_coef(uc_1p_result$beta),
  cookieFirstPartyUcPct = format_pct(uc_1p_result$percent_effect)
)

upsert_tex_macros(VALUES_PATH, macro_values)
cat("Updated E.1 macros in: ", VALUES_PATH, "\n", sep = "")

significant_positive_categories <- uc_category_results[
  conf_low > 0,
  category
]
sig_pos_text <- if (length(significant_positive_categories)) {
  paste(
    gsub("&", "\\\\&", significant_positive_categories),
    collapse = ", "
  )
} else {
  "none"
}
upsert_string_macro(
  STR_VALUES_PATH,
  "sigPosUcCategory",
  sig_pos_text
)
cat("Updated E.1 string macros in: ", STR_VALUES_PATH, "\n", sep = "")


# =============================================================================
# 9. COPY/PASTE CONSOLE SUMMARY
# =============================================================================

summary_results <- rbindlist(list(
  cbind(
    data.table(outcome = "Unique First-Party Cookies"),
    uc_1p_result
  ),
  cbind(
    data.table(outcome = "Unique Third-Party Cookies"),
    uc_3p_result
  ),
  cbind(
    data.table(outcome = "First-Party Cookies per Visit"),
    cpv_1p_result
  ),
  cbind(
    data.table(outcome = "Third-Party Cookies per Visit"),
    cpv_3p_result
  )
))

cat("\nAPPENDIX E.1 MAIN PPML RESULTS\n")
cat("================================\n")
print(summary_results)

cat("\nCATEGORY SUMMARY\n")
print(data.table(
  outcome = "Unique Third-Party Cookies",
  categories = nrow(uc_category_results),
  significant_negative = n_significant_negative(uc_category_results),
  significant_positive = n_significant_positive(uc_category_results)
))

cat("=== APPENDIX E.1 DONE ===\n")