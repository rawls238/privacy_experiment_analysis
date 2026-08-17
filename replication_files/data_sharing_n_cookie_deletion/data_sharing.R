#!/usr/bin/env Rscript
# =============================================================================
# data_sharing.R -- Appendix D within-website data sharing
#
# Confirmed design
#   - Continuing participant-website pairs are defined from browsing data only.
#   - No minimum-dwell threshold.
#   - Cookie data are left-joined only after the browsing sample is fixed.
#   - Missing cookie-panel rows remain missing; observed zeros remain zero.
#   - Cookies per Visit: third-party Set-Cookie actions, PPML with visits as
#     exposure (implemented as offset = log(visit_count)).
#   - Unique Third-Party Cookies: distinct third-party cookie identities seen
#     in request snapshots during the participant-website-day, PPML in levels.
#     "Snapshot" is a construction detail, not the displayed outcome name.
#   - Participant, website, and calendar-date FE; participant-clustered SE.
#   - No winsorization and no named-website exclusions.
# =============================================================================

suppressMessages({
  library(jsonlite)  # Must precede time_usage_helpers.R.
  library(data.table)
  library(fst)
  library(fixest)
  library(savetexvalue)
})

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/number_format_helpers.R")
source("replication_files/utils/tex_helpers.R")

select <- dplyr::select
setDTthreads(4L)
options(datatable.verbose = FALSE, scipen = 999)

TIME_PATH <- "../data/final_extension_data/time_data_2.csv"
COOKIE_PATH <- "../data/processed_data/panel_cookies.fst"
ASSIGNMENT_PATH <- paste0(
  "../data/final_extension_data/",
  "experiment_conditions_pilot_july_2024.csv"
)

TABLE_PATH <- "output/tables/info_treatment_balanced_panel_did.tex"
VALUES_PATH <- "output/values/data_sharing_info_values.tex"
RESULTS_PATH <- paste0(
  "output/diagnostics/appendix_d_ppml/",
  "appendix_d_ppml_coefficients.csv"
)

KEY <- c("experiment_id", "website", "date")
SIGNIF <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)
BAD_USERS <- union(BAD_USERS, c("6ccc7d5", "7d6864c"))

dir.create(dirname(TABLE_PATH), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(VALUES_PATH), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(RESULTS_PATH), recursive = TRUE, showWarnings = FALSE)

require_columns <- function(x, required, label) {
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(label, " is missing: ", paste(missing, collapse = ", "))
  }
}


# =============================================================================
# 1. Browsing-defined continuing participant-website pairs
# =============================================================================

ec <- fread(ASSIGNMENT_PATH)
require_columns(
  ec,
  c("experiment_id", "in_experiment", "wave_id", "experiment_condition"),
  "Assignment file"
)
ec[, experiment_id := as.character(experiment_id)]
ec[, wave_id := as.integer(wave_id)]
ec <- ec[
  tolower(as.character(in_experiment)) == "true" &
    !experiment_id %in% BAD_USERS
]
ec[wave_id == 3L, wave_id := 2L]
ec <- ec[
  wave_id %in% c(1L, 2L) &
    experiment_condition %in% c("control", "saliency", "info"),
  .(experiment_id, wave_id, experiment_condition)
]
if (anyDuplicated(ec$experiment_id)) stop("Duplicate experiment_id in assignment")

panel <- get_time_panel(
  path = TIME_PATH,
  min_seconds = 0,
  verbose = TRUE
)
require_columns(panel, c(KEY, "time_spent", "visit_count"), "Time panel")
panel[, experiment_id := as.character(experiment_id)]
panel[, date := as.Date(date)]
if (anyDuplicated(panel, by = KEY)) stop("Duplicate key in time panel")

panel <- merge(
  panel[experiment_id %in% ec$experiment_id],
  ec,
  by = "experiment_id",
  all.x = TRUE,
  sort = FALSE
)

panel[, wave_start := fifelse(
  wave_id == 1L, START_DATE_WAVE_1, START_DATE_WAVE_2
)]
panel[, treatment_date := fifelse(
  wave_id == 1L, TREATMENT_DATE_WAVE_1, TREATMENT_DATE_WAVE_2
)]
panel[, cookie_cutoff := fifelse(
  wave_id == 1L, COOKIE_TREATMENT_WAVE_1_1, COOKIE_TREATMENT_WAVE_2_1
)]
panel <- panel[
  date >= wave_start & date < cookie_cutoff &
    !is.na(visit_count) & visit_count > 0
]

# Existing survey-platform exclusion.
site_df <- data.frame(website = unique(panel$website))
site_df <- aggregate_time_data(site_df, field = "website")
site_df <- high_level_aggregate(site_df, field = "website_aggregated")
site_lookup <- as.data.table(unique(
  site_df[, c("website", "website_aggregated_high_level")]
))
panel <- merge(panel, site_lookup, by = "website", all.x = TRUE, sort = FALSE)
panel <- panel[
  !(tolower(website_aggregated_high_level) %in% SURVEY_WEBSITES)
]

panel[, post := as.integer(date >= treatment_date)]
panel[, treatment := factor(
  experiment_condition,
  levels = c("control", "saliency", "info")
)]

# This pair list is determined before cookie availability is examined.
pair_support <- panel[, .(
  has_pre = any(post == 0L),
  has_post = any(post == 1L)
), by = .(experiment_id, website)]
continuing_pairs <- pair_support[
  has_pre & has_post,
  .(experiment_id, website)
]

pair_pct <- 100 * nrow(continuing_pairs) / nrow(pair_support)
bal_browsing <- merge(
  panel,
  continuing_pairs,
  by = c("experiment_id", "website"),
  sort = FALSE
)
obs_pct <- 100 * nrow(bal_browsing) / nrow(panel)
if (anyDuplicated(bal_browsing, by = KEY)) {
  stop("Duplicate key in continuing-pair browsing sample")
}


# =============================================================================
# 2. Attach cookie outcomes without redefining the browsing sample
# =============================================================================

cookies <- read_fst(COOKIE_PATH, as.data.table = TRUE)
require_columns(
  cookies,
  c(
    KEY,
    "tracker_record_observed",
    "set_cookie_actions_3rd_p",
    "unique_snapshot_cookies_3rd_p"
  ),
  "Cookie panel"
)
cookies[, experiment_id := as.character(experiment_id)]
cookies[, date := as.Date(date)]
if (anyDuplicated(cookies, by = KEY)) stop("Duplicate key in cookie panel")

bal <- merge(
  bal_browsing,
  cookies[, .(
    experiment_id,
    website,
    date,
    tracker_record_observed,
    set_cookie_actions_3rd_p,
    unique_snapshot_cookies_3rd_p
  )],
  by = KEY,
  all.x = TRUE,
  sort = FALSE
)
if (nrow(bal) != nrow(bal_browsing)) stop("Cookie join changed row count")
if (anyDuplicated(bal, by = KEY)) stop("Cookie join produced duplicate keys")

bal[, cookie_measurement_observed := !is.na(tracker_record_observed)]
cookie_coverage_pct <- 100 * mean(bal$cookie_measurement_observed)

# Missing tracker-panel rows stay missing and do not enter the cookie models.
# Matched rows with zero recorded cookies remain valid zeros.
bal <- bal[cookie_measurement_observed]
if (anyNA(bal[, .(
  visit_count,
  set_cookie_actions_3rd_p,
  unique_snapshot_cookies_3rd_p
)])) {
  stop("Matched cookie rows contain missing model variables")
}
if (any(bal$visit_count <= 0)) stop("visit_count exposure must be positive")
if (any(bal$set_cookie_actions_3rd_p < 0) ||
    any(bal$unique_snapshot_cookies_3rd_p < 0)) {
  stop("Cookie outcomes must be nonnegative")
}

cat(sprintf(
  paste0(
    "Continuing sample: %.1f%% of pairs, %.1f%% of browsing rows, ",
    "%d participants, %d websites\n"
  ),
  pair_pct, obs_pct, uniqueN(bal$experiment_id), uniqueN(bal$website)
))
cat(sprintf(
  "Cookie measurement coverage within continuing browsing rows: %.1f%%\n",
  cookie_coverage_pct
))


# =============================================================================
# 3. PPML models and Table D.3
# =============================================================================

# The offset fixes the coefficient on log(visit_count) at one:
# E[Set-Cookie actions | X] = visit_count * exp(X beta + fixed effects).
m_bal_cpv <- fepois(
  set_cookie_actions_3rd_p ~ i(treatment, post, ref = "control") |
    experiment_id + website + date,
  data = bal,
  offset = ~log(visit_count),
  cluster = ~experiment_id,
  notes = FALSE
)

# Displayed name: Unique Third-Party Cookies. Internally this is deduplicated
# within participant-website-day from request-cookie snapshots.
m_bal_uc <- fepois(
  unique_snapshot_cookies_3rd_p ~ i(treatment, post, ref = "control") |
    experiment_id + website + date,
  data = bal,
  cluster = ~experiment_id,
  notes = FALSE
)

DICT_BAL <- c(
  `treatment::saliency:post` = "Saliency $\\times$ Post",
  `treatment::info:post` = "Information $\\times$ Post",
  `experiment_id` = "Participant FE",
  `website` = "Website FE",
  `date` = "Date FE"
)

bal_tex <- etable(
  m_bal_cpv,
  m_bal_uc,
  headers = c("Cookies per Visit", "Unique Third-Party Cookies"),
  dict = DICT_BAL,
  digits = 3,
  signif.code = SIGNIF,
  depvar = FALSE,
  fitstat = c("n", "pr2"),
  tex = TRUE
)
write_tabular_only(bal_tex, file = TABLE_PATH)


# =============================================================================
# 4. Readable percentage effects and LaTeX macros
# =============================================================================

model_result <- function(model, term, outcome, treatment_name) {
  if (!term %in% names(coef(model))) {
    stop("Model did not estimate required term: ", term)
  }
  beta <- unname(coef(model)[term])
  interval <- confint(model, parm = term, level = 0.95)
  data.table(
    outcome = outcome,
    treatment = treatment_name,
    beta_log_scale = beta,
    se_log_scale = unname(se(model)[term]),
    percent_effect = 100 * (exp(beta) - 1),
    percent_ci_low = 100 * (exp(interval[1, 1]) - 1),
    percent_ci_high = 100 * (exp(interval[1, 2]) - 1),
    p_value = unname(pvalue(model)[term]),
    observations = nobs(model)
  )
}

results <- rbindlist(list(
  model_result(
    m_bal_cpv, "treatment::saliency:post",
    "Cookies per Visit", "Saliency"
  ),
  model_result(
    m_bal_cpv, "treatment::info:post",
    "Cookies per Visit", "Information"
  ),
  model_result(
    m_bal_uc, "treatment::saliency:post",
    "Unique Third-Party Cookies", "Saliency"
  ),
  model_result(
    m_bal_uc, "treatment::info:post",
    "Unique Third-Party Cookies", "Information"
  )
))
fwrite(results, RESULTS_PATH)
print(results)

suppressWarnings(file.remove(VALUES_PATH))

cpv_sal <- results[outcome == "Cookies per Visit" & treatment == "Saliency"]
cpv_info <- results[outcome == "Cookies per Visit" & treatment == "Information"]
uc_sal <- results[
  outcome == "Unique Third-Party Cookies" & treatment == "Saliency"
]
uc_info <- results[
  outcome == "Unique Third-Party Cookies" & treatment == "Information"
]

# Existing coefficient/p-value macro names are preserved for compatibility.
save_tex_value(format_coef(cpv_sal$beta_log_scale),
               name = "cpvBalSalCoef", file = VALUES_PATH)
save_tex_value(format_pvalue(cpv_sal$p_value),
               name = "cpvBalSalPval", file = VALUES_PATH)
save_tex_value(format_coef(cpv_info$beta_log_scale),
               name = "cpvBalInfoCoef", file = VALUES_PATH)
save_tex_value(format_pvalue(cpv_info$p_value),
               name = "cpvBalInfoPval", file = VALUES_PATH)
save_tex_value(format_coef(uc_sal$beta_log_scale),
               name = "cpvBalUcSalCoef", file = VALUES_PATH)
save_tex_value(format_pvalue(uc_sal$p_value),
               name = "cpvBalUcSalPval", file = VALUES_PATH)
save_tex_value(format_coef(uc_info$beta_log_scale),
               name = "cpvBalUcInfoCoef", file = VALUES_PATH)
save_tex_value(format_pvalue(uc_info$p_value),
               name = "cpvBalUcInfoPval", file = VALUES_PATH)

# New percentage-effect macros for the revised writeup.
save_tex_value(format_pct(cpv_sal$percent_effect),
               name = "cpvBalSalPct", file = VALUES_PATH)
save_tex_value(format_pct(cpv_info$percent_effect),
               name = "cpvBalInfoPct", file = VALUES_PATH)
save_tex_value(format_pct(uc_sal$percent_effect),
               name = "cpvBalUcSalPct", file = VALUES_PATH)
save_tex_value(format_pct(uc_info$percent_effect),
               name = "cpvBalUcInfoPct", file = VALUES_PATH)

save_tex_value(format_pct(pair_pct),
               name = "cpvBalPairPct", file = VALUES_PATH)
save_tex_value(format_pct(obs_pct),
               name = "cpvBalObsPct", file = VALUES_PATH)
save_tex_value(format_pct(cookie_coverage_pct),
               name = "cpvBalCookieCoveragePct", file = VALUES_PATH)
save_tex_value(format_count(uniqueN(bal$experiment_id)),
               name = "cpvBalNUsers", file = VALUES_PATH)

cat(sprintf("Saved Table D.3 to %s\n", TABLE_PATH))
cat(sprintf("Saved 16 macros to %s\n", VALUES_PATH))
cat(sprintf("Saved coefficient results to %s\n", RESULTS_PATH))
cat("=== DONE ===\n")