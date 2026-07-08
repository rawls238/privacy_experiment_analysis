# =============================================================================
# cookie_deletion.R   (replication_files pipeline version)
#
# Produces the Cookie Deletion analyses for the paper appendix
# (writeup_v4.tex, "Data Sharing and Cookie Deletion Analysis", E.2-E.4).
#
# Guy's question: "Is cookie deletion plausibly random, or a systematic bug?"
#
#   SECTION 1  Did deletion happen, and what are the overall effects?
#     1.1 Daily CPV trajectory (c1 / c2)                    -> fig:cpv_over_time
#     1.2 Daily browsing-time trajectory (c1 / c2)          -> fig:time_over_time
#     1.3 CPV DiD regression                                -> tab:cookie_deletion_did
#     1.4 Time DiD regression                               -> tab:time_did_regression
#     1.5 CPV DiD excluding deletion-unaffected sites
#         1.5a unaffected-site shares (Pre/Post/Change)     -> tab:..._unaffected_sites_summary
#         1.5b DiD excluding unaffected sites               -> tab:..._did_excluding_unaffected
#     1.6 Baseline CPV by website category                  -> fig:cpv_baseline_by_category
#
#   SECTION 2  Is deletion plausibly random? (CPV outcome)
#     2.1 By event-log status                               -> tab:cookie_deletion_by_log_status
#     2.2 By user time quintile                             -> fig:deletion_by_quintile
#     2.3 By site (top 15)                                  -> fig:deletion_by_site
#     2.4 By site category                                  -> fig:deletion_by_category
#     2.5 Quintile x log status (MAR)                       -> fig:deletion_by_quintile_log_status
#     2.6 Top-15 sites x log status (MAR)                   -> fig:deletion_by_site_log_status
#     2.7 Category x log status (MAR)                       -> fig:deletion_by_category_log_status
#
#   SECTION 3  Browsing-time heterogeneity
#     3.2 Time by quintile, TWO MARGINS                     -> fig:time_extensive_by_quintile
#                                                              fig:time_intensive_by_quintile
#     3.3 ... 3.7 site / category / log-status mirrors      -> fig:time_by_* (+ _log_status)
#
#   Inline scalars (Pooled) cited in E prose:
#     output/values/data_sharing_cookie_values.tex
#       \cookieCpvCoef \cookieCpvPct \cookieCpvExclCoef
#       \cookieTimeCoef \cookieTimePct
#       \cookieHasLogCoef \cookieNoLogCoef \noLogPct
#       \unaffCookieSharePre \unaffCookieSharePost (+ visit/time shares)
#       \timeExtQOneCoef \timeExtQOnePval \timeIntQOneCoef \qOneZeroDayPct
#       \timeIntQOnePval \baseCatTopMedCpv \baseCatBottomMedCpv
#
# DESIGN (unchanged from stage 1):
#   Two 2x2 DiDs pooled via event-time alignment (NOT staggered DiD).
#   c1 (early deletion) vs c2 (late, clean control in the gap week).
#   Event time tau = date - c1_deletion_date_per_wave
#     Wave 1 anchor: Jul 26   Wave 2 anchor: Aug 9
#   Window tau in [-7, 6] (1 week pre + 1 week gap).
#
# CHANGES vs stage-1 script (code/cookie_deletion/...):
#   - Sourced from replication_files/utils (no local SURVEY_WEBSITES / plot
#     theme / log-status colors). library(jsonlite) before utils so
#     get_domain_classification() resolves fromJSON().
#   - Paths: ../data/.. ; outputs to output/{figures,tables,values}/.
#   - Pooled only: wave-1 / wave-2 columns dropped from every regression table
#     (paper wave prose rewritten separately). The c1/c2 split in 1.1/1.2 and
#     the has_log / quintile / category splits are NOT wave splits and stay.
#   - Tables via write_tabular_only() (bare tabular, no \begingroup..\par..).
#   - Figures restyled to plot_rules.R: single-series significance plots use
#     ONE color (read CI vs zero), log-status plots distinguish groups by SHAPE
#     (not color), single-line trajectories use neutral grey (gray30),
#     reference/annotation lines are gray (no red).
#   - Console diagnostics / per-row prints / SUMMARY block removed.
#   - 1.6: baseline CPV by category figure (median point + IQR bars),
#     giving the pre-deletion scope for deletion to operate; context for the
#     per-category effect figures in Section 2.
#   - 3.2 REWRITTEN (two margins): the former log(1+time) quintile figure is
#     removed. With zero-browsing days concentrated in Q1 (~67% of days on the
#     completed user-day panel), the log(1+x) coefficient is unit-dependent
#     and has no behavioral interpretation (Chen & Roth 2024, QJE). Replaced
#     by an extensive margin (1[browsed today], completed panel) and an
#     intensive margin (log time, positive days only) figure. NOTE: the
#     log-status quintile time figure (3.5) still uses log(1+time); it is a
#     MAR diagnostic (between-group contrast) and is left for review.
#
# USAGE
#   setwd("~/Dropbox/spring2025experiment/code_github")
#   source("replication_files/data_sharing_n_cookie_deletion/cookie_deletion.R")
# =============================================================================

library(jsonlite)   # MUST precede utils: time_usage_helpers.R uses fromJSON()
library(data.table)
library(fst)
library(fixest)
library(ggplot2)
library(savetexvalue)

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/values.R")              # SURVEY_WEBSITES, BAD_USERS
source("replication_files/utils/time_usage_helpers.R")  # aggregate_time_data, get_domain_classification
source("replication_files/utils/number_format_helpers.R")
source("replication_files/utils/tex_helpers.R")         # write_tabular_only
source("replication_files/utils/plot_rules.R")          # theme_privacy_experiment, palette consts

select <- dplyr::select  # prevent data.table masking inside get_domain_classification

FIGURES_DIR <- "output/figures/"
TABLES_DIR  <- "output/tables/"
VALUES_DIR  <- "output/values/"

# Cookie-deletion-specific bad users (deletion-loop logging artifacts, not
# global). Global BAD_USERS = c("2607a1f") comes from values.R; extend it here:
#   6ccc7d5 - 200k AUTOMATIC_COOKIE_DELETION events in 10 min (deletion loop)
#   7d6864c - 5 deletion events on a day with 0 panel browsing (anomaly)
BAD_USERS <- union(BAD_USERS, c("6ccc7d5", "7d6864c"))


# =============================================================================
# CONSTANTS (analysis-specific; intentionally local, not in utils)
# =============================================================================
TAU_MIN     <- -7
TAU_MAX     <- 6
VLINE_X     <- -0.5            # deletion-start boundary on the event-time axis
N_TOP_SITES <- 15

# Figure sizes, standardized by plot type. Change one constant to restyle a
# whole class of figures. Height scales with item count and point density:
#   TREND / QUINT   : time-axis line, or 5-item vertical coef plot  -> short
#   WIDE_SINGLE     : 15-17 item horizontal coef plot, 1 point/item
#   WIDE_LOGSTATUS  : 15-17 item horizontal, 2 points/item (shape split) -> tall
FIG_W                <- 8
FIG_H_TREND          <- 5
FIG_H_QUINT          <- 5
FIG_H_WIDE_SINGLE    <- 6.5
FIG_H_WIDE_LOGSTATUS <- 8.5

# Single-series / shape-plot styling. Neutral grey everywhere (no color: the
# single-line trajectories and coefficient points do not encode groups, so grey
# suffices; significance is read off the CI vs the zero line).
POINT_COLOR <- "gray30"        # coefficient points / error bars
LINE_COLOR  <- "gray30"        # single-line trajectories (was deep-blue accent)
LOG_SHAPES  <- c("No Log" = 16, "Has Log" = 17)  # distinguish by shape, not color

# Sites whose 3rd-party cookies are NOT affected by the deletion mechanism
# (Google-ecosystem services + Facebook). Identified from positive per-site DiD
# coefficients in Section 2.3.
UNAFFECTED_SITES <- c(
  "www.facebook.com",
  "docs.google.com",
  "gemini.google.com",
  "datacompute.google.com",
  "mail.google.com"
)


# =============================================================================
# PLOT HELPERS (one consistent style for the figures)
# =============================================================================

# Single-line trajectory with CI ribbon (1.1, 1.2). Expects columns
# xt, mean_y, ci_lo, ci_hi. Accent line color; gray deletion-start line.
plot_trajectory <- function(dt, ylab) {
  ggplot(dt, aes(x = xt, y = mean_y)) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = LINE_COLOR, alpha = 0.15) +
    geom_line(color = LINE_COLOR, linewidth = LINE_WIDTH) +
    geom_point(color = LINE_COLOR, size = POINT_SIZE * 0.6) +
    geom_vline(xintercept = VLINE_X, linetype = "dashed", color = "gray50") +
    scale_x_continuous(breaks = TAU_MIN:TAU_MAX) +
    labs(x = "Days Relative to Deletion Start", y = ylab) +
    theme_privacy_experiment(show_grid_x = TRUE, show_grid_y = TRUE)
}

# Single-series coefficient plot (2.2-2.4, 3.2-3.4). ONE color; significance is
# read from whether the CI crosses the dashed zero line. Expects grp (ordered
# factor), coef, ci_lo, ci_hi.
plot_coef <- function(dt, mode = c("vertical", "horizontal"), value_lab, group_lab) {
  mode <- match.arg(mode)
  if (mode == "vertical") {
    ggplot(dt, aes(x = grp, y = coef)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                    width = ERRORBAR_WIDTH, linewidth = LINE_WIDTH, color = POINT_COLOR) +
      geom_point(size = POINT_SIZE, color = POINT_COLOR) +
      labs(x = group_lab, y = value_lab) +
      theme_privacy_experiment(show_grid_x = FALSE, show_grid_y = TRUE)
  } else {
    ggplot(dt, aes(x = coef, y = grp)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                     height = 0.25, linewidth = LINE_WIDTH, color = POINT_COLOR) +
      geom_point(size = POINT_SIZE, color = POINT_COLOR) +
      labs(x = value_lab, y = group_lab) +
      theme_privacy_experiment(show_grid_x = TRUE, show_grid_y = FALSE)
  }
}

# Log-status split (2.5-2.7, 3.5-3.7). has_log vs no_log distinguished by SHAPE,
# not color (per house rule: minimize color). Expects grp, coef, ci_lo, ci_hi,
# log_status.
plot_coef_logstatus <- function(dt, mode = c("vertical", "horizontal"),
                                value_lab, group_lab) {
  mode <- match.arg(mode)
  if (mode == "vertical") {
    ggplot(dt, aes(x = grp, y = coef, shape = log_status, group = log_status)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                    width = ERRORBAR_WIDTH, linewidth = LINE_WIDTH, color = POINT_COLOR,
                    position = position_dodge(width = DODGE_WIDTH_2)) +
      geom_point(size = POINT_SIZE, color = POINT_COLOR,
                 position = position_dodge(width = DODGE_WIDTH_2)) +
      scale_shape_manual(values = LOG_SHAPES) +
      labs(x = group_lab, y = value_lab, shape = NULL) +
      theme_privacy_experiment(show_grid_x = FALSE, show_grid_y = TRUE)
  } else {
    ggplot(dt, aes(x = coef, y = grp, shape = log_status, group = log_status)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                     height = 0.25, linewidth = LINE_WIDTH, color = POINT_COLOR,
                     position = position_dodge(width = DODGE_WIDTH_2)) +
      geom_point(size = POINT_SIZE, color = POINT_COLOR,
                 position = position_dodge(width = DODGE_WIDTH_2)) +
      scale_shape_manual(values = LOG_SHAPES) +
      labs(x = value_lab, y = group_lab, shape = NULL) +
      theme_privacy_experiment(show_grid_x = TRUE, show_grid_y = FALSE)
  }
}

# etable dictionaries (FE row labels + readable dep-var names)
DICT_CPV  <- c(post_treated = "Post $\\times$ Cookie Deletion",
               experiment_id = "Participant FE", website = "Website FE",
               dow = "Day-of-Week FE",
               log_cpv_3p = "log CPV")
DICT_TIME <- c(post_treated = "Post $\\times$ Cookie Deletion",
               experiment_id = "Participant FE", website = "Website FE",
               dow = "Day-of-Week FE",
               log_time = "log time")
SIGNIF <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)


# =============================================================================
# 0. DATA PREPARATION   (logic unchanged; pre-flight verified Pooled = -0.3288)
# =============================================================================
panel <- read_fst("../data/tracker_panel/panel_merged_CLEAN.fst", as.data.table = TRUE)

ec <- fread("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
ec_clean <- ec[in_experiment == "true" & !experiment_id %in% BAD_USERS]
ec_clean[wave_id == 3, wave_id := 2L]

drop_cols <- intersect(c("wave_id", "treatment", "experiment_condition"), names(panel))
if (length(drop_cols) > 0) panel[, (drop_cols) := NULL]
panel <- panel[experiment_id %in% ec_clean$experiment_id]
panel <- merge(panel, ec_clean[, .(experiment_id, wave_id, experiment_condition)],
               by = "experiment_id", all.x = TRUE)
panel[wave_id == 3, wave_id := 2L]

panel[, wave_start := fifelse(wave_id == 1L, as.Date("2025-06-14"), as.Date("2025-06-28"))]
panel[, c1_anchor  := fifelse(wave_id == 1L, as.Date("2025-07-26"), as.Date("2025-08-09"))]
panel[, c2_anchor  := fifelse(wave_id == 1L, as.Date("2025-08-02"), as.Date("2025-08-16"))]
panel[, tau := as.integer(date - c1_anchor)]

# SURVEY_WEBSITES filter (via aggregated domain to avoid substring overcatch)
site_df <- data.frame(website = unique(panel$website), stringsAsFactors = FALSE)
site_df <- aggregate_time_data(site_df, field = "website")
site_df <- high_level_aggregate(site_df, field = "website_aggregated")
site_lookup <- as.data.table(unique(
  site_df[, c("website", "website_aggregated_high_level")]))
panel <- merge(panel, site_lookup, by = "website", all.x = TRUE)
panel <- panel[!(tolower(website_aggregated_high_level) %in% SURVEY_WEBSITES)]

# Website category
domain_class <- get_domain_classification()
setDT(domain_class)
domain_class_slim <- unique(domain_class[, .(
  website_agg = name_aggregated_high_level, category = category_level_1)])
panel <- merge(panel, domain_class_slim,
               by.x = "website_aggregated_high_level", by.y = "website_agg",
               all.x = TRUE)

# CPV analysis sample
t1 <- panel[tau >= TAU_MIN & tau <= TAU_MAX & !is.na(visit_count) & visit_count > 0]
t1[, cpv_3p       := n_cookies_third_party / visit_count]
t1[, log_cpv_3p   := log(1 + cpv_3p)]
t1[, log_time     := log(1 + time_spent)]
t1[, treated      := as.integer(cookie_treatment_idx == 1)]
t1[, post         := as.integer(tau >= 0)]
t1[, post_treated := post * treated]
t1[, dow          := factor(weekdays(date))]

# has_log: whether user has ANY AUTOMATIC_COOKIE_DELETION event
events <- fread("../data/final_extension_data/event_logs.csv",
                select = c("experiment_id", "event"))
del_users <- unique(events[grepl("^AUTOMATIC_COOKIE_DELETION", event), experiment_id])
t1[, has_log := as.integer(experiment_id %in% del_users)]
rm(events); gc(verbose = FALSE)

# --- top15_sites (shared by 2.3, 2.6, 3.3, 3.6) -----------------------------
# Sites with >= 5 unique users in EVERY (has_log x post x treated) cell, so the
# post_treated coefficient is identifiable in both has_log subsamples (2.6/3.6).
site_pre_time <- t1[post == 0 & !is.na(time_spent) & time_spent > 0,
                    .(total_time_sec = sum(time_spent)),
                    by = website][order(-total_time_sec)]

site_8cell <- t1[, .(
  c1 = uniqueN(experiment_id[has_log == 0 & post == 0 & treated == 0]),
  c2 = uniqueN(experiment_id[has_log == 0 & post == 0 & treated == 1]),
  c3 = uniqueN(experiment_id[has_log == 0 & post == 1 & treated == 0]),
  c4 = uniqueN(experiment_id[has_log == 0 & post == 1 & treated == 1]),
  c5 = uniqueN(experiment_id[has_log == 1 & post == 0 & treated == 0]),
  c6 = uniqueN(experiment_id[has_log == 1 & post == 0 & treated == 1]),
  c7 = uniqueN(experiment_id[has_log == 1 & post == 1 & treated == 0]),
  c8 = uniqueN(experiment_id[has_log == 1 & post == 1 & treated == 1])
), by = website]
site_8cell[, min_cell := pmin(c1, c2, c3, c4, c5, c6, c7, c8)]
site_8cell[, pass_8cell := min_cell >= 5]

ranked_sites <- merge(site_pre_time, site_8cell,
                      by = "website", all.x = TRUE)[order(-total_time_sec)]
top15_sites <- ranked_sites[pass_8cell == TRUE][1:N_TOP_SITES, website]


# =============================================================================
# SECTION 1: Did deletion happen, and what are the overall effects?
# =============================================================================

# --- 1.1 Daily CPV trajectory (c1 / c2) -> fig:cpv_over_time -----------------
cpv_panel <- panel[!is.na(visit_count) & visit_count > 0]
cpv_panel[, cpv_3p := n_cookies_third_party / visit_count]
cpv_panel[, log_cpv_3p := log(1 + cpv_3p)]
cpv_panel[, own_anchor := fifelse(cookie_treatment_idx == 1, c1_anchor, c2_anchor)]
cpv_panel[, own_tau := as.integer(date - own_anchor)]

agg_traj <- function(dat, yvar) {
  d <- dat[own_tau >= TAU_MIN & own_tau <= TAU_MAX,
           .(mean_y = mean(get(yvar)), se = sd(get(yvar)) / sqrt(.N), n = .N), by = own_tau]
  d[, tcrit := qt(0.975, pmax(n - 1, 1))]  # t-dist, consistent with coef plots
  d[, `:=`(xt = own_tau, ci_lo = mean_y - tcrit * se, ci_hi = mean_y + tcrit * se)]
  d[order(xt)]
}

cpv_c1 <- agg_traj(cpv_panel[cookie_treatment_idx == 1], "log_cpv_3p")
cpv_c2 <- agg_traj(cpv_panel[cookie_treatment_idx == 2], "log_cpv_3p")
ggsave(paste0(FIGURES_DIR, "cpv_over_time_c1.pdf"),
       plot_trajectory(cpv_c1, "Mean log CPV"), width = FIG_W, height = FIG_H_TREND)
ggsave(paste0(FIGURES_DIR, "cpv_over_time_c2.pdf"),
       plot_trajectory(cpv_c2, "Mean log CPV"), width = FIG_W, height = FIG_H_TREND)
cat("Saved: cpv_over_time_c1.pdf, cpv_over_time_c2.pdf\n")

# --- 1.2 Daily browsing-time trajectory (c1 / c2) -> fig:time_over_time ------
panel_time <- panel[!is.na(time_spent) & time_spent >= 0 &
                      !is.na(visit_count) & visit_count > 0]
user_day <- panel_time[, .(total_time = sum(time_spent)),
                       by = .(experiment_id, date, cookie_treatment_idx,
                              wave_id, c1_anchor, c2_anchor)]
user_day[, log_time := log(1 + total_time)]

agg_time_traj <- function(dat, anchor_col) {
  d <- copy(dat)
  d[, own_tau := as.integer(date - get(anchor_col))]
  d <- d[own_tau >= TAU_MIN & own_tau <= TAU_MAX,
         .(mean_y = mean(log_time), se = sd(log_time) / sqrt(.N), n = .N), by = own_tau]
  d[, tcrit := qt(0.975, pmax(n - 1, 1))]  # t-dist, consistent with coef plots
  d[, `:=`(xt = own_tau, ci_lo = mean_y - tcrit * se, ci_hi = mean_y + tcrit * se)]
  d[order(xt)]
}

time_c1 <- agg_time_traj(user_day[cookie_treatment_idx == 1], "c1_anchor")
time_c2 <- agg_time_traj(user_day[cookie_treatment_idx == 2], "c2_anchor")
ggsave(paste0(FIGURES_DIR, "time_over_time_c1.pdf"),
       plot_trajectory(time_c1, "Mean log time"), width = FIG_W, height = FIG_H_TREND)
ggsave(paste0(FIGURES_DIR, "time_over_time_c2.pdf"),
       plot_trajectory(time_c2, "Mean log time"), width = FIG_W, height = FIG_H_TREND)
cat("Saved: time_over_time_c1.pdf, time_over_time_c2.pdf\n")
rm(cpv_panel); gc(verbose = FALSE)

# --- 1.3 CPV DiD regression (Pooled) -> tab:cookie_deletion_did --------------
m_pool <- feols(log_cpv_3p ~ post_treated | experiment_id + website + dow,
                data = t1, cluster = ~experiment_id, notes = FALSE)
write_tabular_only(
  etable(m_pool, dict = DICT_CPV, digits = 3, signif.code = SIGNIF, tex = TRUE),
  file = paste0(TABLES_DIR, "cookie_deletion_did_regression.tex"))

# --- 1.4 Time DiD regression (Pooled) -> tab:time_did_regression -------------
did_time <- panel_time[tau >= TAU_MIN & tau <= TAU_MAX]
did_time[, `:=`(treated = as.integer(cookie_treatment_idx == 1L),
                post = as.integer(tau >= 0))]
did_time[, post_treated := post * treated]
did_time[, log_time := log(1 + time_spent)]
did_time[, dow := factor(weekdays(date))]

m_time_pool <- feols(log_time ~ post_treated | experiment_id + website + dow,
                     data = did_time, cluster = ~experiment_id, notes = FALSE)
write_tabular_only(
  etable(m_time_pool, dict = DICT_TIME, digits = 3, signif.code = SIGNIF, tex = TRUE),
  file = paste0(TABLES_DIR, "time_did_regression.tex"))

# --- 1.5a Unaffected-site shares (Pooled Pre/Post/Change) -------------------
#          -> tab:cookie_deletion_unaffected_sites_summary
t1[, is_unaffected := website %in% UNAFFECTED_SITES]

share_pp <- t1[, .(
  total_visits  = sum(visit_count),
  unaff_visits  = sum(visit_count[is_unaffected]),
  total_time    = sum(time_spent,            na.rm = TRUE),
  unaff_time    = sum(time_spent[is_unaffected], na.rm = TRUE),
  total_cookies = sum(n_cookies_third_party, na.rm = TRUE),
  unaff_cookies = sum(n_cookies_third_party[is_unaffected], na.rm = TRUE)
), by = post]
share_pp[, `:=`(visit_share  = 100 * unaff_visits  / total_visits,
                time_share   = 100 * unaff_time    / total_time,
                cookie_share = 100 * unaff_cookies / total_cookies)]

get_share <- function(metric, p) share_pp[post == p, get(metric)]
share_rows <- data.table(
  Metric = c("Visit Share", "Time Share", "Cookie Share"),
  Pre    = c(get_share("visit_share", 0),  get_share("time_share", 0),  get_share("cookie_share", 0)),
  Post   = c(get_share("visit_share", 1),  get_share("time_share", 1),  get_share("cookie_share", 1)))
share_rows[, Change := Post - Pre]

share_lines <- c(
  "\\begin{tabular}{lccc}",
  "\\tabularnewline \\midrule \\midrule",
  "Metric & Pre & Post & Change \\\\",
  "\\midrule",
  vapply(seq_len(nrow(share_rows)), function(i) {
    r <- share_rows[i]
    sprintf("%s & %.2f\\%% & %.2f\\%% & %+.2fpp \\\\",
            r$Metric, r$Pre, r$Post, r$Change)
  }, character(1)),
  "\\midrule \\midrule",
  "\\end{tabular}")
writeLines(share_lines, paste0(TABLES_DIR, "cookie_deletion_unaffected_sites_summary.tex"))
cat("Saved (tabular only):", paste0(TABLES_DIR, "cookie_deletion_unaffected_sites_summary.tex"), "\n")

# --- 1.5b DiD excluding unaffected sites (Pooled) ---------------------------
#          -> tab:cookie_deletion_did_excluding_unaffected
t1_excl <- t1[is_unaffected == FALSE]
m_pool_excl <- feols(log_cpv_3p ~ post_treated | experiment_id + website + dow,
                     data = t1_excl, cluster = ~experiment_id, notes = FALSE)
write_tabular_only(
  etable(m_pool_excl, dict = DICT_CPV, digits = 3, signif.code = SIGNIF, tex = TRUE),
  file = paste0(TABLES_DIR, "cookie_deletion_did_excluding_unaffected.tex"))

# --- 1.6 Baseline CPV by website category -> fig:cpv_baseline_by_category ----
# Median (point) and IQR (bars) of site-level mean baseline CPV within each
# category, pre-deletion week (post == 0). Shows the scope available for the
# deletion mechanism to operate; context for the per-category effect figures
# in Section 2. Category set recomputed on the baseline subsample with the
# same thresholds as Section 2 (>=500 obs & >=50 users).
base_cat <- t1[post == 0 & !is.na(category) & category != ""]
base_big_cats <- base_cat[, .(n_obs = .N, n_users = uniqueN(experiment_id)),
                          by = category][n_obs >= 500 & n_users >= 50, category]

site_base <- base_cat[category %in% base_big_cats,
                      .(site_mean_cpv = mean(cpv_3p, na.rm = TRUE)),
                      by = .(website, category)]

base_stats <- site_base[, .(
  p25 = quantile(site_mean_cpv, 0.25),
  med = median(site_mean_cpv),
  p75 = quantile(site_mean_cpv, 0.75)
), by = category][order(med)]
base_stats[, grp := factor(category, levels = category)]

p_base <- ggplot(base_stats, aes(x = med, y = grp)) +
  geom_errorbarh(aes(xmin = p25, xmax = p75),
                 height = 0.25, linewidth = LINE_WIDTH, color = POINT_COLOR) +
  geom_point(size = POINT_SIZE, color = POINT_COLOR) +
  labs(x = "Baseline Third-Party Cookies per Visit", y = NULL) +
  theme_privacy_experiment(show_grid_x = TRUE, show_grid_y = FALSE)

ggsave(paste0(FIGURES_DIR, "cpv_baseline_by_category.pdf"),
       p_base, width = FIG_W, height = FIG_H_WIDE_SINGLE)
cat("Saved: cpv_baseline_by_category.pdf\n")


# =============================================================================
# SECTION 2: Is cookie deletion plausibly random? (CPV outcome)
# =============================================================================

# --- 2.1 By event-log status -> tab:cookie_deletion_by_log_status -----------
m_all <- feols(log_cpv_3p ~ post_treated | experiment_id + website + dow,
               data = t1, cluster = ~experiment_id, notes = FALSE)
m_has <- feols(log_cpv_3p ~ post_treated | experiment_id + website + dow,
               data = t1[has_log == 1], cluster = ~experiment_id, notes = FALSE)
m_no  <- feols(log_cpv_3p ~ post_treated | experiment_id + website + dow,
               data = t1[has_log == 0], cluster = ~experiment_id, notes = FALSE)
write_tabular_only(
  etable(m_all, m_has, m_no,
         headers = c("All Users", "Has Deletion Log", "No Deletion Log"),
         dict = DICT_CPV, digits = 3, signif.code = SIGNIF, tex = TRUE),
  file = paste0(TABLES_DIR, "cookie_deletion_by_log_status.tex"))

# --- subgroup DiD helper: returns one (coef, ci_lo, ci_hi, p) row per group --
fit_by_group <- function(data, groups, group_col, yvar, fe_with_website = TRUE) {
  fe <- if (fe_with_website) "experiment_id + website + dow" else "experiment_id + dow"
  fml <- as.formula(sprintf("%s ~ post_treated | %s", yvar, fe))
  out <- lapply(groups, function(g) {
    sub <- data[get(group_col) == g]
    m <- tryCatch(feols(fml, data = sub, cluster = ~experiment_id, notes = FALSE),
                  error = function(e) NULL)
    if (is.null(m) || !"post_treated" %in% names(coef(m))) return(NULL)
    ci <- confint(m, "post_treated", level = 0.95)  # t-dist CI, matches p-value
    data.table(grp_raw = as.character(g),
               coef = coef(m)["post_treated"],
               se   = se(m)["post_treated"],
               p    = pvalue(m)["post_treated"],
               ci_lo = as.numeric(ci[1, 1]),
               ci_hi = as.numeric(ci[1, 2]))
  })
  res <- rbindlist(out)
  res
}

fit_by_group_logstatus <- function(data, groups, group_col, yvar, fe_with_website = TRUE) {
  fe <- if (fe_with_website) "experiment_id + website + dow" else "experiment_id + dow"
  fml <- as.formula(sprintf("%s ~ post_treated | %s", yvar, fe))
  out <- list()
  for (g in groups) for (lg in c(0, 1)) {
    sub <- data[get(group_col) == g & has_log == lg]
    m <- tryCatch(feols(fml, data = sub, cluster = ~experiment_id, notes = FALSE),
                  error = function(e) NULL)
    if (is.null(m) || !"post_treated" %in% names(coef(m))) next
    ci <- confint(m, "post_treated", level = 0.95)  # t-dist CI, matches p-value
    out[[paste(g, lg)]] <- data.table(
      grp_raw = as.character(g),
      log_status = if (lg == 1) "Has Log" else "No Log",
      coef = coef(m)["post_treated"], se = se(m)["post_treated"],
      p = pvalue(m)["post_treated"],
      ci_lo = as.numeric(ci[1, 1]), ci_hi = as.numeric(ci[1, 2]))
  }
  res <- rbindlist(out)
  res[, log_status := factor(log_status, levels = c("No Log", "Has Log"))]
  res
}

# user time quintile (assigned on pre-period total time)
user_pre_time <- t1[post == 0, .(pre_total_time = sum(time_spent, na.rm = TRUE)),
                    by = experiment_id][!is.na(pre_total_time) & pre_total_time > 0]
user_pre_time[, time_quintile := cut(pre_total_time,
                                     breaks = quantile(pre_total_time, 0:5 / 5, na.rm = TRUE),
                                     labels = paste0("Q", 1:5), include.lowest = TRUE)]
t1_q <- merge(t1, user_pre_time[, .(experiment_id, time_quintile)],
              by = "experiment_id", all.x = TRUE)[!is.na(time_quintile)]

# big categories
t1_cat <- t1[!is.na(category) & category != ""]
big_cats <- t1_cat[, .(n_obs = .N, n_users = uniqueN(experiment_id)),
                   by = category][n_obs >= 500 & n_users >= 50, category]

QUINT_LAB <- "User Time Quintile (Q1 = Lowest pre-period time spent)"

# --- 2.2 CPV by user time quintile -> fig:deletion_by_quintile --------------
q_dt <- fit_by_group(t1_q, paste0("Q", 1:5), "time_quintile", "log_cpv_3p")
q_dt[, grp := factor(grp_raw, levels = paste0("Q", 1:5))]
ggsave(paste0(FIGURES_DIR, "cpv_heterogeneity_by_user_quintile.pdf"),
       plot_coef(q_dt, "vertical", "Estimated Effect on log CPV", QUINT_LAB),
       width = FIG_W, height = FIG_H_QUINT)

# --- 2.3 CPV by site (top 15) -> fig:deletion_by_site ------------------------
site_dt <- fit_by_group(t1[website %in% top15_sites], top15_sites, "website",
                        "log_cpv_3p", fe_with_website = FALSE)
site_dt[, display_name := gsub("^www\\.", "", grp_raw)]
site_dt <- site_dt[order(coef)]
site_dt[, grp := factor(display_name, levels = rev(display_name))]
ggsave(paste0(FIGURES_DIR, "cpv_did_by_site.pdf"),
       plot_coef(site_dt, "horizontal",
                 "Estimated Effect on log CPV", NULL),
       width = FIG_W, height = FIG_H_WIDE_SINGLE)

# --- 2.4 CPV by site category -> fig:deletion_by_category --------------------
cat_dt <- fit_by_group(t1_cat, sort(big_cats), "category", "log_cpv_3p")
cat_dt <- cat_dt[order(coef)]
cat_dt[, grp := factor(grp_raw, levels = grp_raw)]
ggsave(paste0(FIGURES_DIR, "cpv_heterogeneity_by_website_category.pdf"),
       plot_coef(cat_dt, "horizontal",
                 "Estimated Effect on log CPV", NULL),
       width = FIG_W, height = FIG_H_WIDE_SINGLE)

# --- 2.5 Quintile x log status (MAR) -> fig:deletion_by_quintile_log_status --
q_log <- fit_by_group_logstatus(t1_q, paste0("Q", 1:5), "time_quintile", "log_cpv_3p")
q_log[, grp := factor(grp_raw, levels = paste0("Q", 1:5))]
ggsave(paste0(FIGURES_DIR, "cpv_heterogeneity_by_user_quintile_log_status.pdf"),
       plot_coef_logstatus(q_log, "vertical", "Estimated Effect on log CPV", QUINT_LAB),
       width = FIG_W, height = FIG_H_QUINT)

# --- 2.6 Top-15 sites x log status (MAR) -> fig:deletion_by_site_log_status --
site_log <- fit_by_group_logstatus(t1[website %in% top15_sites], top15_sites,
                                   "website", "log_cpv_3p", fe_with_website = FALSE)
site_log[, display_name := gsub("^www\\.", "", grp_raw)]
order_site <- site_log[log_status == "Has Log"][order(coef), display_name]
site_log[, grp := factor(display_name, levels = rev(order_site))]
ggsave(paste0(FIGURES_DIR, "cpv_did_by_site_log_status.pdf"),
       plot_coef_logstatus(site_log, "horizontal",
                           "Estimated Effect on log CPV", NULL),
       width = FIG_W, height = FIG_H_WIDE_LOGSTATUS)

# --- 2.7 Category x log status (MAR) -> fig:deletion_by_category_log_status --
cat_log <- fit_by_group_logstatus(t1_cat, sort(big_cats), "category", "log_cpv_3p")
order_cat <- cat_log[log_status == "No Log"][order(coef), grp_raw]
cat_log[, grp := factor(grp_raw, levels = rev(order_cat))]
ggsave(paste0(FIGURES_DIR, "cpv_heterogeneity_by_website_category_log_status.pdf"),
       plot_coef_logstatus(cat_log, "horizontal",
                           "Estimated Effect on log CPV", NULL),
       width = FIG_W, height = FIG_H_WIDE_LOGSTATUS)


# =============================================================================
# SECTION 3: Browsing-time heterogeneity
# =============================================================================
TIME_LAB <- "Estimated Effect on log time"

# --- 3.2 Time by quintile: TWO MARGINS ---------------------------------------
#         -> fig:time_extensive_by_quintile, fig:time_intensive_by_quintile
# The former log(1+time) quintile figure is REMOVED. With zero-browsing days
# concentrated in Q1 (~67% of days on the completed user-day panel), the
# log(1+x) coefficient is unit-dependent and has no behavioral interpretation
# (Chen & Roth 2024). We report the two margins separately:
#   Extensive: completed user-day panel, outcome 1[browsed today]
#   Intensive: positive-browsing days only, outcome log(time)
ud_obs <- t1[, .(total_time = sum(time_spent, na.rm = TRUE)),
             by = .(experiment_id, tau, treated, post)]
ud_grid <- CJ(experiment_id = unique(t1$experiment_id), tau = TAU_MIN:TAU_MAX)
ud <- merge(ud_grid, unique(t1[, .(experiment_id, treated, wave_id)]),
            by = "experiment_id")
ud <- merge(ud, ud_obs[, .(experiment_id, tau, total_time)],
            by = c("experiment_id", "tau"), all.x = TRUE)
ud[, total_time := fifelse(is.na(total_time), 0, total_time)]
ud[, post := as.integer(tau >= 0)]
ud[, post_treated := post * treated]
ud[, any_browse := as.integer(total_time > 0)]
ud[, cal_date := fifelse(wave_id == 1L, as.Date("2025-07-26"),
                         as.Date("2025-08-09")) + tau]
ud[, dow := factor(weekdays(cal_date))]
ud <- merge(ud, user_pre_time[, .(experiment_id, time_quintile)],
            by = "experiment_id")[!is.na(time_quintile)]

fit_margin <- function(data, yvar) {
  out <- lapply(paste0("Q", 1:5), function(q) {
    m <- tryCatch(feols(as.formula(paste(yvar, "~ post_treated | experiment_id + dow")),
                        data = data[time_quintile == q],
                        cluster = ~experiment_id, notes = FALSE),
                  error = function(e) NULL)
    if (is.null(m) || !"post_treated" %in% names(coef(m))) return(NULL)
    ci <- confint(m, "post_treated", level = 0.95)
    data.table(grp_raw = q, coef = coef(m)["post_treated"],
               p = pvalue(m)["post_treated"],
               ci_lo = as.numeric(ci[1, 1]), ci_hi = as.numeric(ci[1, 2]))
  })
  res <- rbindlist(out)
  res[, grp := factor(grp_raw, levels = paste0("Q", 1:5))]
  res
}

ext_dt <- fit_margin(ud, "any_browse")
ggsave(paste0(FIGURES_DIR, "time_extensive_by_quintile.pdf"),
       plot_coef(ext_dt, "vertical",
                 "Estimated Effect on P(Browsed That Day)", QUINT_LAB),
       width = FIG_W, height = FIG_H_QUINT)

udi <- ud[total_time > 0]
udi[, log_time_pos := log(total_time)]
int_dt <- fit_margin(udi, "log_time_pos")
ggsave(paste0(FIGURES_DIR, "time_intensive_by_quintile.pdf"),
       plot_coef(int_dt, "vertical",
                 "Estimated Effect on log Time (Browsing Days)", QUINT_LAB),
       width = FIG_W, height = FIG_H_QUINT)
cat("Saved: time_extensive_by_quintile.pdf, time_intensive_by_quintile.pdf\n")

q1_zero_pct <- 100 * ud[time_quintile == "Q1", mean(total_time == 0)]

# --- 3.3 time by site (top 15) -> fig:time_by_site ---------------------------
site_time <- fit_by_group(t1[website %in% top15_sites], top15_sites, "website",
                          "log_time", fe_with_website = FALSE)
site_time[, display_name := gsub("^www\\.", "", grp_raw)]
site_time <- site_time[order(coef)]
site_time[, grp := factor(display_name, levels = rev(display_name))]
ggsave(paste0(FIGURES_DIR, "time_did_by_site.pdf"),
       plot_coef(site_time, "horizontal", TIME_LAB, NULL), width = FIG_W, height = FIG_H_WIDE_SINGLE)

# --- 3.4 time by category -> fig:time_by_category -----------------------------
cat_time <- fit_by_group(t1_cat, sort(big_cats), "category", "log_time")
cat_time <- cat_time[order(coef)]
cat_time[, grp := factor(grp_raw, levels = grp_raw)]
ggsave(paste0(FIGURES_DIR, "time_heterogeneity_by_website_category.pdf"),
       plot_coef(cat_time, "horizontal", TIME_LAB, NULL), width = FIG_W, height = FIG_H_WIDE_SINGLE)

# --- 3.5 time quintile x log status (MAR) -------------------------------------
# NOTE: still log(1+time); kept as a between-group MAR diagnostic pending review.
q_time_log <- fit_by_group_logstatus(t1_q, paste0("Q", 1:5), "time_quintile", "log_time")
q_time_log[, grp := factor(grp_raw, levels = paste0("Q", 1:5))]
ggsave(paste0(FIGURES_DIR, "time_heterogeneity_by_user_quintile_log_status.pdf"),
       plot_coef_logstatus(q_time_log, "vertical", TIME_LAB, QUINT_LAB),
       width = FIG_W, height = FIG_H_QUINT)

# --- 3.6 time sites x log status (MAR) ----------------------------------------
site_time_log <- fit_by_group_logstatus(t1[website %in% top15_sites], top15_sites,
                                        "website", "log_time", fe_with_website = FALSE)
site_time_log[, display_name := gsub("^www\\.", "", grp_raw)]
order_site_t <- site_time_log[log_status == "Has Log"][order(coef), display_name]
site_time_log[, grp := factor(display_name, levels = rev(order_site_t))]
ggsave(paste0(FIGURES_DIR, "time_did_by_site_log_status.pdf"),
       plot_coef_logstatus(site_time_log, "horizontal", TIME_LAB, NULL),
       width = FIG_W, height = FIG_H_WIDE_LOGSTATUS)

# --- 3.7 time category x log status (MAR) --------------------------------------
cat_time_log <- fit_by_group_logstatus(t1_cat, sort(big_cats), "category", "log_time")
order_cat_t <- cat_time_log[log_status == "No Log"][order(coef), grp_raw]
cat_time_log[, grp := factor(grp_raw, levels = rev(order_cat_t))]
ggsave(paste0(FIGURES_DIR, "time_heterogeneity_by_website_category_log_status.pdf"),
       plot_coef_logstatus(cat_time_log, "horizontal", TIME_LAB, NULL),
       width = FIG_W, height = FIG_H_WIDE_LOGSTATUS)

cat("Saved: 18 figures to", FIGURES_DIR, "\n")
cat("Saved: 5 tables to", TABLES_DIR, "\n")

# =============================================================================
# INLINE SCALARS (Pooled) -> output/values/data_sharing_cookie_values.tex
# =============================================================================
no_log_pct <- 100 * (1 - uniqueN(t1[has_log == 1]$experiment_id) /
                       uniqueN(t1$experiment_id))

cookie_values_file <- file.path(VALUES_DIR, "data_sharing_cookie_values.tex")
suppressWarnings(file.remove(cookie_values_file))

save_tex_value(format_coef(coef(m_pool)["post_treated"]),
               name = "cookieCpvCoef", file = cookie_values_file)
save_tex_value(format_pct(100 * abs(exp(coef(m_pool)["post_treated"]) - 1)),
               name = "cookieCpvPct", file = cookie_values_file)
save_tex_value(format_coef(coef(m_pool_excl)["post_treated"]),
               name = "cookieCpvExclCoef", file = cookie_values_file)
save_tex_value(format_coef(coef(m_time_pool)["post_treated"]),
               name = "cookieTimeCoef", file = cookie_values_file)
save_tex_value(format_pct(100 * abs(exp(coef(m_time_pool)["post_treated"]) - 1)),
               name = "cookieTimePct", file = cookie_values_file)
save_tex_value(format_coef(coef(m_has)["post_treated"]),
               name = "cookieHasLogCoef", file = cookie_values_file)
save_tex_value(format_coef(coef(m_no)["post_treated"]),
               name = "cookieNoLogCoef", file = cookie_values_file)
save_tex_value(format_pct(no_log_pct),
               name = "noLogPct", file = cookie_values_file)
save_tex_value(format_pct(share_pp[post == 0, cookie_share]),
               name = "unaffCookieSharePre", file = cookie_values_file)
save_tex_value(format_pct(share_pp[post == 1, cookie_share]),
               name = "unaffCookieSharePost", file = cookie_values_file)

# Unaffected-site visit / time shares (pre / post)
save_tex_value(format_pct(share_pp[post == 0, visit_share]),
               name = "unaffVisitSharePre", file = cookie_values_file)
save_tex_value(format_pct(share_pp[post == 1, visit_share]),
               name = "unaffVisitSharePost", file = cookie_values_file)
save_tex_value(format_pct(share_pp[post == 0, time_share]),
               name = "unaffTimeSharePre", file = cookie_values_file)
save_tex_value(format_pct(share_pp[post == 1, time_share]),
               name = "unaffTimeSharePost", file = cookie_values_file)

# Two-margin quintile macros (Section 3.2)
save_tex_value(format_coef(ext_dt[grp_raw == "Q1", coef]),
               name = "timeExtQOneCoef", file = cookie_values_file)
save_tex_value(format_pvalue(ext_dt[grp_raw == "Q1", p]),
               name = "timeExtQOnePval", file = cookie_values_file)
save_tex_value(format_coef(int_dt[grp_raw == "Q1", coef]),
               name = "timeIntQOneCoef", file = cookie_values_file)
save_tex_value(format_pvalue(int_dt[grp_raw == "Q1", p]),
               name = "timeIntQOnePval", file = cookie_values_file)
save_tex_value(format_pct(q1_zero_pct),
               name = "qOneZeroDayPct", file = cookie_values_file)

# Baseline-category medians cited in the Section 1.6 figure prose
save_tex_value(format_count(round(base_stats[, max(med)])),
               name = "baseCatTopMedCpv", file = cookie_values_file)
save_tex_value(format_count(round(base_stats[, min(med)])),
               name = "baseCatBottomMedCpv", file = cookie_values_file)

# Significance / sign counts for prose. "Significant" = 95% CI excludes zero,
# which (since CIs now come from confint()) is identical to p < 0.05.
n_sig <- function(dt) sum(dt$ci_lo > 0 | dt$ci_hi < 0)
n_neg <- function(dt) sum(dt$coef < 0)

save_tex_value(as.character(n_sig(cat_dt)),
               name = "nSigCpvCategories", file = cookie_values_file)
save_tex_value(as.character(nrow(cat_dt)),
               name = "nCpvCategories", file = cookie_values_file)
save_tex_value(as.character(n_sig(site_time)),
               name = "nSigTimeSites", file = cookie_values_file)
save_tex_value(as.character(n_neg(site_time)),
               name = "nNegTimeSites", file = cookie_values_file)
save_tex_value(as.character(nrow(site_time)),
               name = "nTimeSites", file = cookie_values_file)
save_tex_value(as.character(n_sig(cat_time)),
               name = "nSigTimeCategories", file = cookie_values_file)

cat("Saved macros to", cookie_values_file, "\n")

# --- Diagnostic print (NOT written to paper): quintile significance + counts,
#     so prose about margins / "X of N" can be written from data. -------------
print_sig <- function(dt, label, keycol = "grp_raw") {
  cat("\n[", label, "]\n", sep = "")
  d <- copy(dt)
  d[, sig := ci_lo > 0 | ci_hi < 0]
  d[, dir := ifelse(coef < 0, "neg", "pos")]
  print(d[, c(keycol, "coef", "ci_lo", "ci_hi", "sig", "dir"), with = FALSE])
  cat(sprintf("  -> %d of %d significant; %d negative\n", sum(d$sig), nrow(d), sum(d$coef < 0)))
}
cat("\n", strrep("=", 60), "\nSIGNIFICANCE DIAGNOSTICS (for prose; not in paper)\n",
    strrep("=", 60), "\n", sep = "")
print_sig(q_dt,      "CPV quintile")
print_sig(ext_dt,    "TIME extensive margin by quintile")
print_sig(int_dt,    "TIME intensive margin by quintile")
print_sig(site_dt,   "CPV site")
print_sig(cat_dt,    "CPV category")
print_sig(site_time, "TIME site")
print_sig(cat_time,  "TIME category")
cat("=== DONE ===\n")