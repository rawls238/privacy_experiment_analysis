#!/usr/bin/env Rscript
# =============================================================================
# cookie_deletion_log_check.R
#
# Appendix E.2: Browsing Time and the Validity of the Logging Data
#
# This script replaces the E.2 portion of the former cookie_deletion.R while
# preserving the paper-facing output filenames and macro names used by
# Overleaf.
#
# Design
#   - Treatment: early cookie deletion.
#   - Control: late cookie deletion before its deletion begins.
#   - Window: tau = -7,...,6 around each wave's early-deletion date.
#   - Main dwell rule: participant-website-days with time_spent > 30 seconds.
#   - Survey-platform websites are excluded.
#   - Browsing-time analyses use the full browsing stream, without a cookie
#     join. Missing participant-days enter as zero recorded browsing time.
#
# Estimation
#   - Recorded browsing time: raw seconds estimated by PPML.
#   - Main fixed effects: participant and day; standard errors clustered by
#     participant.
#   - The any-recorded-browsing outcome remains an FE-LPM because it is binary.
#   - Event-log-status comparisons are descriptive diagnostics, not causal
#     subgroup effects.
#
# Paper-facing outputs retained
#   output/tables/time_did_regression.tex
#   output/figures/time_extensive_by_quintile.pdf
#   output/figures/time_intensive_by_quintile.pdf
#   output/figures/time_heterogeneity_by_website_category.pdf
#   output/tables/cookie_deletion_by_log_status.tex
#   output/figures/time_heterogeneity_by_user_quintile_log_status.pdf
#   output/figures/time_heterogeneity_by_website_category_log_status.pdf
#   output/values/data_sharing_cookie_values.tex
#   output/values/data_sharing_cookie_str_values.tex
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
EVENT_LOG_PATH <- "../data/final_extension_data/event_logs.csv"
ASSIGNMENT_PATH <- paste0(
  "../data/final_extension_data/",
  "experiment_conditions_pilot_july_2024.csv"
)

FIGURES_DIR <- "output/figures"
TABLES_DIR <- "output/tables"
VALUES_PATH <- "output/values/data_sharing_cookie_values.tex"
STR_VALUES_PATH <- "output/values/data_sharing_cookie_str_values.tex"

TIME_TABLE_PATH <- file.path(TABLES_DIR, "time_did_regression.tex")
TIME_ANY_Q_FIGURE_PATH <- file.path(
  FIGURES_DIR,
  "time_extensive_by_quintile.pdf"
)
TIME_Q_FIGURE_PATH <- file.path(
  FIGURES_DIR,
  "time_intensive_by_quintile.pdf"
)
TIME_CATEGORY_FIGURE_PATH <- file.path(
  FIGURES_DIR,
  "time_heterogeneity_by_website_category.pdf"
)
LOG_STATUS_TABLE_PATH <- file.path(
  TABLES_DIR,
  "cookie_deletion_by_log_status.tex"
)
TIME_Q_LOG_FIGURE_PATH <- file.path(
  FIGURES_DIR,
  "time_heterogeneity_by_user_quintile_log_status.pdf"
)
TIME_CATEGORY_LOG_FIGURE_PATH <- file.path(
  FIGURES_DIR,
  "time_heterogeneity_by_website_category_log_status.pdf"
)
KEY <- c("experiment_id", "website", "date")
TAU_MIN <- -7L
TAU_MAX <- 6L
MIN_SECONDS <- 30

FIG_W <- 8
FIG_H_QUINTILE <- 5
FIG_H_CATEGORY <- 5
POINT_COLOR <- "gray30"
LOG_SHAPES <- c("No Log" = 16, "Has Log" = 17)
SIGNIF <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)
BAD_USERS_E <- union(BAD_USERS, c("6ccc7d5", "7d6864c"))

# Fixed to E.1 categories so cookie and browsing-time heterogeneity use the
# same category set.
WEBSITE_CATEGORIES <- c(
  "Arts & Entertainment",
  "Business & Industrial",
  "Computers & Electronics",
  "Finance",
  "Food & Drink",
  "Games",
  "Health",
  "Hobbies & Leisure",
  "Home & Garden",
  "Internet & Telecom",
  "Jobs & Education",
  "News",
  "Online Communities",
  "Reference",
  "Shopping",
  "Travel"
)

dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(VALUES_PATH), recursive = TRUE, showWarnings = FALSE)

require_columns <- function(x, required, label) {
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "))
  }
}

extract_ppml_percent <- function(model, term = "post_treated") {
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

extract_lpm_pp <- function(model, term = "post_treated") {
  if (!term %in% names(coef(model))) {
    stop("Model does not contain coefficient: ", term)
  }
  
  beta <- unname(coef(model)[term])
  ci <- confint(model, term)
  
  data.table(
    beta = beta,
    se = unname(se(model)[term]),
    percentage_points = 100 * beta,
    conf_low = 100 * as.numeric(ci[1L, 1L]),
    conf_high = 100 * as.numeric(ci[1L, 2L]),
    p_value = unname(pvalue(model)[term]),
    observations = nobs(model)
  )
}

fit_ppml_group <- function(data, groups, group_col, outcome) {
  out <- lapply(groups, function(g) {
    d <- data[get(group_col) == g]
    model <- tryCatch(
      fepois(
        as.formula(paste0(
          outcome,
          " ~ post_treated | experiment_id + date"
        )),
        data = d,
        cluster = ~experiment_id,
        notes = FALSE
      ),
      error = function(e) {
        message("PPML failed for ", group_col, " = ", g, ": ", e$message)
        NULL
      }
    )
    
    if (is.null(model) || !"post_treated" %in% names(coef(model))) {
      return(NULL)
    }
    
    result <- extract_ppml_percent(model)
    result[, group_value := as.character(g)]
    result
  })
  
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

fit_lpm_group <- function(data, groups, group_col, outcome) {
  out <- lapply(groups, function(g) {
    d <- data[get(group_col) == g]
    model <- tryCatch(
      feols(
        as.formula(paste0(
          outcome,
          " ~ post_treated | experiment_id + date"
        )),
        data = d,
        cluster = ~experiment_id,
        notes = FALSE
      ),
      error = function(e) {
        message("FE-LPM failed for ", group_col, " = ", g, ": ", e$message)
        NULL
      }
    )
    
    if (is.null(model) || !"post_treated" %in% names(coef(model))) {
      return(NULL)
    }
    
    result <- extract_lpm_pp(model)
    result[, group_value := as.character(g)]
    result
  })
  
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

fit_ppml_group_log_status <- function(data, groups, group_col, outcome) {
  out <- list()
  
  for (g in groups) {
    for (status in c(0L, 1L)) {
      d <- data[get(group_col) == g & has_log == status]
      model <- tryCatch(
        fepois(
          as.formula(paste0(
            outcome,
            " ~ post_treated | experiment_id + date"
          )),
          data = d,
          cluster = ~experiment_id,
          notes = FALSE
        ),
        error = function(e) {
          message(
            "PPML failed for ", group_col, " = ", g,
            ", has_log = ", status, ": ", e$message
          )
          NULL
        }
      )
      
      if (is.null(model) || !"post_treated" %in% names(coef(model))) {
        next
      }
      
      result <- extract_ppml_percent(model)
      result[, `:=`(
        group_value = as.character(g),
        log_status = if (status == 1L) "Has Log" else "No Log"
      )]
      out[[paste(g, status, sep = "_")]] <- result
    }
  }
  
  result <- rbindlist(out, use.names = TRUE, fill = TRUE)
  result[, log_status := factor(
    log_status,
    levels = c("No Log", "Has Log")
  )]
  result
}

fit_log_status_interactions <- function(data, groups, group_col, outcome) {
  out <- lapply(groups, function(g) {
    d <- data[get(group_col) == g]
    model <- tryCatch(
      fepois(
        as.formula(paste0(
          outcome,
          " ~ post_treated + post_treated:has_log | experiment_id + date"
        )),
        data = d,
        cluster = ~experiment_id,
        notes = FALSE
      ),
      error = function(e) NULL
    )
    
    term <- "post_treated:has_log"
    if (is.null(model) || !term %in% names(coef(model))) {
      return(NULL)
    }
    
    data.table(
      group_value = as.character(g),
      interaction_beta = unname(coef(model)[term]),
      interaction_se = unname(se(model)[term]),
      interaction_p = unname(pvalue(model)[term]),
      observations = nobs(model)
    )
  })
  
  rbindlist(out, use.names = TRUE, fill = TRUE)
}

inverse_variance_q_test <- function(results) {
  keep <- results[is.finite(beta) & is.finite(se) & se > 0]
  if (nrow(keep) < 2L) {
    return(data.table(
      common_beta = NA_real_,
      q_statistic = NA_real_,
      degrees_freedom = NA_integer_,
      p_value = NA_real_
    ))
  }
  
  weights <- 1 / keep$se^2
  common_beta <- sum(weights * keep$beta) / sum(weights)
  q_statistic <- sum(weights * (keep$beta - common_beta)^2)
  degrees_freedom <- nrow(keep) - 1L
  
  data.table(
    common_beta = common_beta,
    q_statistic = q_statistic,
    degrees_freedom = degrees_freedom,
    p_value = pchisq(q_statistic, degrees_freedom, lower.tail = FALSE)
  )
}

upsert_tex_macros <- function(path, values) {
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character()
  
  for (name in names(values)) {
    trimmed <- trimws(lines)
    prefix_plain <- paste0("\\newcommand\\", name)
    prefix_braced <- paste0("\\newcommand{\\", name, "}")
    lines <- lines[
      !startsWith(trimmed, prefix_plain) &
        !startsWith(trimmed, prefix_braced)
    ]
    lines <- c(lines, sprintf("\\newcommand\\%s {%s}", name, values[[name]]))
  }
  
  writeLines(lines, path)
}

upsert_string_macro <- function(path, name, value) {
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character()
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

plot_single_effects <- function(results, x_label, y_label, order_values) {
  results[, display_group := factor(group_value, levels = order_values)]
  
  ggplot(results, aes(x = display_group, y = percent_effect)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high),
      width = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH,
      color = POINT_COLOR
    ) +
    geom_point(size = POINT_SIZE, color = POINT_COLOR) +
    labs(x = x_label, y = y_label) +
    theme_privacy_experiment(show_grid_x = FALSE, show_grid_y = TRUE)
}

plot_lpm_effects <- function(results, x_label, y_label, order_values) {
  results[, display_group := factor(group_value, levels = order_values)]
  
  ggplot(results, aes(x = display_group, y = percentage_points)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high),
      width = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH,
      color = POINT_COLOR
    ) +
    geom_point(size = POINT_SIZE, color = POINT_COLOR) +
    labs(x = x_label, y = y_label) +
    theme_privacy_experiment(show_grid_x = FALSE, show_grid_y = TRUE)
}

plot_log_status_effects <- function(
    results,
    x_label,
    y_label,
    order_values
) {
  results[, display_group := factor(group_value, levels = order_values)]
  dodge <- position_dodge(width = 0.5)
  
  ggplot(
    results,
    aes(
      x = display_group,
      y = percent_effect,
      shape = log_status,
      group = log_status
    )
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high),
      width = ERRORBAR_WIDTH,
      linewidth = LINE_WIDTH,
      color = POINT_COLOR,
      position = dodge
    ) +
    geom_point(
      size = POINT_SIZE,
      color = POINT_COLOR,
      position = dodge
    ) +
    scale_shape_manual(name = NULL, values = LOG_SHAPES) +
    labs(x = x_label, y = y_label) +
    theme_privacy_experiment(show_grid_x = FALSE, show_grid_y = TRUE) +
    theme(legend.position = "bottom")
}


# =============================================================================
# 1. RANDOMIZED ASSIGNMENT AND EVENT-LOG STATUS
# =============================================================================

assignment <- fread(ASSIGNMENT_PATH)
require_columns(
  assignment,
  c("experiment_id", "in_experiment", "wave_id", "cookie_treatment_idx"),
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

events <- fread(EVENT_LOG_PATH, select = c("experiment_id", "event"))
events[, experiment_id := as.character(experiment_id)]
deletion_log_users <- unique(events[
  grepl("^AUTOMATIC_COOKIE_DELETION", event),
  experiment_id
])
assignment[, has_log := as.integer(experiment_id %chin% deletion_log_users)]
rm(events, deletion_log_users)
gc(verbose = FALSE)

cat(sprintf(
  "Randomized sample: %s participants | %.1f%% without a deletion log\n",
  format(nrow(assignment), big.mark = ","),
  100 * mean(assignment$has_log == 0L)
))


# =============================================================================
# 2. FULL BROWSING STREAM AND WEBSITE CATEGORIES
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

time_analysis <- time_panel[
  tau >= TAU_MIN & tau <= TAU_MAX &
    !is.na(time_spent) & time_spent > MIN_SECONDS &
    !is.na(visit_count) & visit_count > 0
]
time_analysis[, post_treated := as.integer(tau >= 0L) * treated]

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

time_analysis <- merge(
  time_analysis,
  domain_class_slim,
  by = "website_aggregated_high_level",
  all.x = TRUE,
  sort = FALSE
)

cat(sprintf(
  paste0(
    "Eligible browsing rows: %s | %s participants | %s websites | ",
    "%s categories\n"
  ),
  format(nrow(time_analysis), big.mark = ","),
  format(uniqueN(time_analysis$experiment_id), big.mark = ","),
  format(uniqueN(time_analysis$website), big.mark = ","),
  format(uniqueN(time_analysis$category, na.rm = TRUE), big.mark = ",")
))


# =============================================================================
# 3. COMPLETE PARTICIPANT-DAY PANEL
# =============================================================================

day_observed <- time_analysis[, .(
  total_time = sum(time_spent, na.rm = TRUE)
), by = .(experiment_id, date, tau)]

day_panel <- merge(
  CJ(
    experiment_id = assignment$experiment_id,
    tau = TAU_MIN:TAU_MAX,
    unique = TRUE
  ),
  assignment[, .(
    experiment_id,
    wave_id,
    treated,
    anchor,
    has_log
  )],
  by = "experiment_id",
  all.x = TRUE,
  sort = FALSE
)
day_panel[, date := anchor + tau]
day_panel <- merge(
  day_panel,
  day_observed,
  by = c("experiment_id", "date", "tau"),
  all.x = TRUE,
  sort = FALSE
)
day_panel[is.na(total_time), total_time := 0]
day_panel[, `:=`(
  post_treated = as.integer(tau >= 0L) * treated,
  any_browsing_recorded = as.integer(total_time > 0)
)]

if (nrow(day_panel) != nrow(assignment) * (TAU_MAX - TAU_MIN + 1L)) {
  stop("Participant-day panel is not balanced to fourteen days")
}
if (any(day_panel$total_time < 0 | !is.finite(day_panel$total_time))) {
  stop("Participant-day time must be finite and nonnegative")
}

cat(sprintf(
  paste0(
    "Participant-day panel: %s observations | %.1f%% zero recorded-time ",
    "days\n"
  ),
  format(nrow(day_panel), big.mark = ","),
  100 * mean(day_panel$total_time == 0)
))


# =============================================================================
# 4. MAIN RECORDED-TIME EFFECT
# =============================================================================

m_time_main <- fepois(
  total_time ~ post_treated | experiment_id + date,
  data = day_panel,
  cluster = ~experiment_id,
  notes = FALSE
)

DICT_TIME <- c(
  post_treated = "Post $\\times$ Cookie Deletion",
  experiment_id = "Participant FE",
  date = "Day FE"
)

time_table <- etable(
  m_time_main,
  headers = "Browsing Time",
  dict = DICT_TIME,
  digits = 3,
  signif.code = SIGNIF,
  depvar = FALSE,
  fitstat = c("n", "pr2"),
  tex = TRUE
)
write_tabular_only(time_table, file = TIME_TABLE_PATH)
cat("Saved: ", TIME_TABLE_PATH, "\n", sep = "")

time_main_result <- extract_ppml_percent(m_time_main)


# =============================================================================
# 5. RECORDED-TIME EFFECT BY PRE-PERIOD TIME QUINTILE
# =============================================================================

pre_time <- day_panel[tau < 0L, .(
  pre_total_time = sum(total_time)
), by = experiment_id][pre_total_time > 0]

setorder(pre_time, pre_total_time, experiment_id)
pre_time[, pre_time_quintile := paste0(
  "Q",
  ceiling(seq_len(.N) * 5 / .N)
)]

quintile_panel <- merge(
  day_panel,
  pre_time,
  by = "experiment_id",
  all = FALSE,
  sort = FALSE
)
quintile_levels <- paste0("Q", 1:5)

quintile_time_results <- fit_ppml_group(
  quintile_panel,
  quintile_levels,
  "pre_time_quintile",
  "total_time"
)
if (nrow(quintile_time_results) != 5L) {
  stop("Pre-time analysis did not produce all five PPML estimates")
}

quintile_any_results <- fit_lpm_group(
  quintile_panel,
  quintile_levels,
  "pre_time_quintile",
  "any_browsing_recorded"
)
if (nrow(quintile_any_results) != 5L) {
  stop("Pre-time analysis did not produce all five FE-LPM estimates")
}

quintile_equality <- inverse_variance_q_test(quintile_time_results)

ggsave(
  TIME_ANY_Q_FIGURE_PATH,
  plot_lpm_effects(
    quintile_any_results,
    "Pre-Period Browsing-Time Quintile",
    "Effect on Days with Browsing Records (pp)",
    quintile_levels
  ),
  width = FIG_W,
  height = FIG_H_QUINTILE
)
cat("Saved: ", TIME_ANY_Q_FIGURE_PATH, "\n", sep = "")

ggsave(
  TIME_Q_FIGURE_PATH,
  plot_single_effects(
    quintile_time_results,
    "Pre-Period Browsing-Time Quintile",
    "Effect on Browsing Time (%)",
    quintile_levels
  ),
  width = FIG_W,
  height = FIG_H_QUINTILE
)
cat("Saved: ", TIME_Q_FIGURE_PATH, "\n", sep = "")


# =============================================================================
# 6. COMPLETE PARTICIPANT-CATEGORY-DAY PANEL
# =============================================================================

category_observed <- time_analysis[
  !is.na(category) & category != "",
  .(category_time = sum(time_spent, na.rm = TRUE)),
  by = .(experiment_id, category, date, tau)
]

# A participant-category pair enters if the participant used that category at
# least once before deletion. Post-period non-use then enters as zero time.
pre_category_pairs <- unique(category_observed[
  tau < 0L,
  .(experiment_id, category)
])

missing_categories <- setdiff(
  WEBSITE_CATEGORIES,
  unique(pre_category_pairs$category)
)
if (length(missing_categories)) {
  stop(
    "E.1 categories missing from the E.2 pre-period browsing sample: ",
    paste(missing_categories, collapse = ", ")
  )
}

category_panel <- pre_category_pairs[
  category %in% WEBSITE_CATEGORIES,
  .(tau = TAU_MIN:TAU_MAX),
  by = .(experiment_id, category)
]
category_panel <- merge(
  category_panel,
  assignment[, .(experiment_id, treated, anchor, has_log)],
  by = "experiment_id",
  all.x = TRUE,
  sort = FALSE
)
category_panel[, date := anchor + tau]
category_panel <- merge(
  category_panel,
  category_observed,
  by = c("experiment_id", "category", "date", "tau"),
  all.x = TRUE,
  sort = FALSE
)
category_panel[is.na(category_time), category_time := 0]
category_panel[, post_treated := as.integer(tau >= 0L) * treated]

category_results <- fit_ppml_group(
  category_panel,
  WEBSITE_CATEGORIES,
  "category",
  "category_time"
)
if (!setequal(category_results$group_value, WEBSITE_CATEGORIES)) {
  stop("Not all 16 E.2 category-level PPML models were estimable")
}

setorder(category_results, percent_effect)
category_order <- category_results$group_value
category_equality <- inverse_variance_q_test(category_results)

ggsave(
  TIME_CATEGORY_FIGURE_PATH,
  plot_single_effects(
    category_results,
    NULL,
    "Effect on Browsing Time (%)",
    category_order
  ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  width = FIG_W,
  height = FIG_H_CATEGORY
)
cat("Saved: ", TIME_CATEGORY_FIGURE_PATH, "\n", sep = "")


# =============================================================================
# 7. UNIQUE THIRD-PARTY COOKIES BY EVENT-LOG STATUS
# =============================================================================

cookie_columns <- c(
  KEY,
  "tracker_record_observed",
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

cookie_log_sample <- merge(
  time_analysis,
  cookies,
  by = KEY,
  all = FALSE,
  sort = FALSE
)
if (anyNA(cookie_log_sample$unique_snapshot_cookies_3rd_p)) {
  stop("Matched cookie rows contain missing unique-cookie outcomes")
}

fit_cookie_log_model <- function(data) {
  fepois(
    unique_snapshot_cookies_3rd_p ~ post_treated |
      experiment_id + website + date,
    data = data,
    cluster = ~experiment_id,
    notes = FALSE
  )
}

m_cookie_log_all <- fit_cookie_log_model(cookie_log_sample)
m_cookie_log_has <- fit_cookie_log_model(cookie_log_sample[has_log == 1L])
m_cookie_log_no <- fit_cookie_log_model(cookie_log_sample[has_log == 0L])

DICT_COOKIE_LOG <- c(
  post_treated = "Post $\\times$ Cookie Deletion",
  experiment_id = "Participant FE",
  website = "Website FE",
  date = "Day FE"
)

log_status_table <- etable(
  m_cookie_log_all,
  m_cookie_log_has,
  m_cookie_log_no,
  headers = c("All Users", "Has Deletion Log", "No Deletion Log"),
  dict = DICT_COOKIE_LOG,
  digits = 3,
  signif.code = SIGNIF,
  depvar = FALSE,
  fitstat = c("n", "pr2"),
  tex = TRUE
)
write_tabular_only(log_status_table, file = LOG_STATUS_TABLE_PATH)
cat("Saved: ", LOG_STATUS_TABLE_PATH, "\n", sep = "")

cookie_log_all_result <- extract_ppml_percent(m_cookie_log_all)
cookie_log_has_result <- extract_ppml_percent(m_cookie_log_has)
cookie_log_no_result <- extract_ppml_percent(m_cookie_log_no)


# =============================================================================
# 8. RECORDED TIME BY EVENT-LOG STATUS
# =============================================================================

quintile_log_results <- fit_ppml_group_log_status(
  quintile_panel,
  quintile_levels,
  "pre_time_quintile",
  "total_time"
)
quintile_log_interactions <- fit_log_status_interactions(
  quintile_panel,
  quintile_levels,
  "pre_time_quintile",
  "total_time"
)

ggsave(
  TIME_Q_LOG_FIGURE_PATH,
  plot_log_status_effects(
    quintile_log_results,
    "Pre-Period Browsing-Time Quintile",
    "Effect on Browsing Time (%)",
    quintile_levels
  ),
  width = FIG_W,
  height = FIG_H_QUINTILE
)
cat("Saved: ", TIME_Q_LOG_FIGURE_PATH, "\n", sep = "")

category_log_results <- fit_ppml_group_log_status(
  category_panel,
  category_order,
  "category",
  "category_time"
)
category_log_interactions <- fit_log_status_interactions(
  category_panel,
  category_order,
  "category",
  "category_time"
)

ggsave(
  TIME_CATEGORY_LOG_FIGURE_PATH,
  plot_log_status_effects(
    category_log_results,
    NULL,
    "Effect on Browsing Time (%)",
    category_order
  ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  width = FIG_W,
  height = FIG_H_CATEGORY
)
cat("Saved: ", TIME_CATEGORY_LOG_FIGURE_PATH, "\n", sep = "")


# =============================================================================
# 9. UPDATE E.2 MACROS WITHOUT DELETING E.1 MACROS
# =============================================================================

n_significant <- function(x) sum(x$conf_low > 0 | x$conf_high < 0)
n_significant_negative <- function(x) sum(x$conf_high < 0)
n_significant_positive <- function(x) sum(x$conf_low > 0)
n_negative <- function(x) sum(x$percent_effect < 0)

q1_time <- quintile_time_results[group_value == "Q1"]
q1_any <- quintile_any_results[group_value == "Q1"]
q1_zero_day_pct <- 100 * quintile_panel[
  pre_time_quintile == "Q1",
  mean(total_time == 0)
]

macro_values <- c(
  cookieTimeCoef = format_coef(time_main_result$beta),
  cookieTimePct = format_pct(abs(time_main_result$percent_effect)),
  cookieTimeSignedPct = format_pct(time_main_result$percent_effect),
  noLogPct = format_pct(100 * mean(assignment$has_log == 0L)),
  timeExtQOneCoef = format_coef(q1_any$beta),
  timeExtQOnePval = format_pvalue(q1_any$p_value),
  timeIntQOneCoef = format_coef(q1_time$beta),
  timeIntQOnePval = format_pvalue(q1_time$p_value),
  timeQOnePct = format_pct(q1_time$percent_effect),
  qOneZeroDayPct = format_pct(q1_zero_day_pct),
  timeQuintileEqualityPval = format_pvalue(quintile_equality$p_value),
  nTimeCategories = as.character(nrow(category_results)),
  nNegTimeCategories = as.character(n_negative(category_results)),
  nSigTimeCategories = as.character(n_significant(category_results)),
  nSigNegTimeCategories = as.character(
    n_significant_negative(category_results)
  ),
  nSigPosTimeCategories = as.character(
    n_significant_positive(category_results)
  ),
  timeCategoryEqualityPval = format_pvalue(category_equality$p_value),
  nSigTimeQuintileLogDifferences = as.character(
    sum(quintile_log_interactions$interaction_p < 0.05, na.rm = TRUE)
  ),
  nSigTimeCategoryLogDifferences = as.character(
    sum(category_log_interactions$interaction_p < 0.05, na.rm = TRUE)
  ),
  ucLogAllCoef = format_coef(cookie_log_all_result$beta),
  ucLogAllPct = format_pct(cookie_log_all_result$percent_effect),
  ucHasLogCoef = format_coef(cookie_log_has_result$beta),
  ucHasLogPct = format_pct(cookie_log_has_result$percent_effect),
  ucNoLogCoef = format_coef(cookie_log_no_result$beta),
  ucNoLogPct = format_pct(cookie_log_no_result$percent_effect),
  # Preserve the legacy names until the E.2 prose is updated.
  cookieHasLogCoef = format_coef(cookie_log_has_result$beta),
  cookieNoLogCoef = format_coef(cookie_log_no_result$beta)
)

upsert_tex_macros(VALUES_PATH, macro_values)
cat("Updated E.2 macros in: ", VALUES_PATH, "\n", sep = "")

significant_time_categories <- category_results[
  conf_high < 0,
  group_value
]
sig_time_text <- if (length(significant_time_categories)) {
  paste(gsub("&", "\\\\&", significant_time_categories), collapse = ", ")
} else {
  "none"
}
upsert_string_macro(
  STR_VALUES_PATH,
  "sigNegTimeCategories",
  sig_time_text
)
cat("Updated E.2 string macros in: ", STR_VALUES_PATH, "\n", sep = "")


# =============================================================================
# 10. COPY/PASTE CONSOLE SUMMARY
# =============================================================================

cat("\nAPPENDIX E.2: BROWSING TIME AND LOGGING VALIDITY\n")
cat("=================================================\n")

cat("\nA. MAIN PARTICIPANT-DAY PPML EFFECT\n")
print(time_main_result)

cat("\nB. PPML EFFECT BY PRE-TIME QUINTILE\n")
print(quintile_time_results[, .(
  quintile = group_value,
  percent_effect,
  conf_low,
  conf_high,
  p_value,
  observations
)])
cat("\nEqual proportional effects across quintiles:\n")
print(quintile_equality)

cat("\nC. ANY RECORDED BROWSING DAY BY PRE-TIME QUINTILE\n")
print(quintile_any_results[, .(
  quintile = group_value,
  percentage_points,
  conf_low,
  conf_high,
  p_value,
  observations
)])

cat("\nD. PPML EFFECT BY WEBSITE CATEGORY\n")
print(category_results[, .(
  category = group_value,
  percent_effect,
  conf_low,
  conf_high,
  p_value,
  observations
)])
cat("\nEqual proportional effects across categories:\n")
print(category_equality)

cat("\nE. UNIQUE THIRD-PARTY COOKIES BY EVENT-LOG STATUS\n")
print(rbindlist(list(
  cbind(data.table(sample = "All Users"), cookie_log_all_result),
  cbind(data.table(sample = "Has Deletion Log"), cookie_log_has_result),
  cbind(data.table(sample = "No Deletion Log"), cookie_log_no_result)
)))

cat("\nF. FORMAL LOG-STATUS INTERACTION TESTS: QUINTILES\n")
print(quintile_log_interactions)

cat("\nG. FORMAL LOG-STATUS INTERACTION TESTS: CATEGORIES\n")
print(category_log_interactions)

cat("\nOUTPUT SUMMARY\n")
cat("- Main outcome uses raw participant-day browsing seconds and PPML.\n")
cat("- Browsing panels use the full time stream and never require a cookie match.\n")
cat("- Fixed effects are labeled Participant FE and Day FE.\n")
cat("- Website-category analyses use the same 16 categories as E.1.\n")
cat("- Event-log-status comparisons are descriptive diagnostics.\n")
cat("=== APPENDIX E.2 DONE ===\n")