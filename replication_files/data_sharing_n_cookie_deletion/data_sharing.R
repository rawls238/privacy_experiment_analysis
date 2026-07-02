# =============================================================================
# data_sharing.R   (replication_files pipeline version)
#   (was info_treatment_cpv_regression.R)
#
# Information intervention effect on data sharing (CPV) -- Appendix E.1
# (writeup_v4.tex, "Effect on Third-Party Cookies per Visit", subsec:info_treatment_cpv).
#
# RESEARCH QUESTION
#   Did the information intervention reduce users' overall data sharing during
#   the post-information / pre-cookie-deletion period?
#
#   This is the INFORMATION-ARM analysis (treated = saliency / information arms
#   vs control). It is the complement of cookie_deletion.R, which analyses the
#   cookie-DELETION manipulation (early vs late deletion group). Both feed
#   Appendix E but use different treatments / samples.
#
# TWO SPECIFICATIONS (pooled across waves):
#   1. Cross-section  -> tab:info_treatment_cpv
#      Unit: user. Outcome: mean of log(1 + daily CPV) over the user's post-info
#      days. Spec: y_i = b1*Sal_i + b2*Info_i + alpha_{block_by_wave} + eps_i.
#   2. DiD            -> tab:info_treatment_cpv_did
#      Unit: user x day (pre + post info). Outcome: log(1 + daily CPV).
#      Spec: y_it = b1*(Sal_i*Post_t) + b2*(Info_i*Post_t) + alpha_i + gamma_t.
#   3. Trend plot     -> fig:cpv_trend_by_treatment
#      Daily mean log CPV by arm, pooled across waves on an event-time axis
#      (days relative to each user's own information-intervention date).
#
#   Inline scalars (Pooled) cited in E.1 prose:
#     output/values/data_sharing_info_values.tex
#       \cpvXsInfoCoef \cpvXsInfoPval \cpvXsInfoPct
#       \cpvDidInfoCoef \cpvDidInfoPval \cpvDidInfoPct
#       \cpvDidSalCoef \cpvDidSalPval
#
# SAMPLE
#   Cross-section: post-info / pre-deletion only
#     Wave 1: 06/28 - 07/25   Wave 2: 07/12 - 08/08
#   DiD: pre-info + post-info (excluding cookie deletion)
#     Wave 1 pre 06/14-06/27 / post 06/28-07/25
#     Wave 2 pre 06/28-07/11 / post 07/12-08/08
#
# CHANGES vs stage-1 script (code/data_sharing/info_treatment_cpv_regression.R):
#   - Sourced from replication_files/utils (no local SURVEY_WEBSITES / theme /
#     TX_COLORS). library(jsonlite) before utils.
#   - Paths: ../data/.. ; outputs to output/{figures,tables,values}/.
#   - Pooled only: wave-1 / wave-2 columns dropped from both tables; the
#     side-by-side console comparison block removed.
#   - The two per-wave trend plots are pooled into ONE event-time figure
#     (cpv_trend_by_treatment.pdf). The 3 arms keep color (scale_color_treatment,
#     house rule for treatment arms); the intervention line is gray (no red).
#   - Tables via write_tabular_only() (bare tabular).
#   - Macros written to an INDEPENDENT bundle (data_sharing_info_values.tex), so
#     this script and cookie_deletion.R do not clobber each other's values and
#     run order does not matter.
#
# USAGE
#   setwd("~/Dropbox/spring2025experiment/code_github")
#   source("replication_files/data_sharing_n_cookie_deletion/data_sharing.R")
# =============================================================================

library(jsonlite)   # MUST precede utils: time_usage_helpers.R uses fromJSON()
library(data.table)
library(fst)
library(fixest)
library(ggplot2)
library(savetexvalue)

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/values.R")              # SURVEY_WEBSITES, BAD_USERS
source("replication_files/utils/time_usage_helpers.R")  # aggregate_time_data, high_level_aggregate
source("replication_files/utils/number_format_helpers.R")
source("replication_files/utils/tex_helpers.R")         # write_tabular_only
source("replication_files/utils/plot_rules.R")          # theme_privacy_experiment, scale_color_treatment

select <- dplyr::select  # prevent data.table masking

FIGURES_DIR <- "output/figures/"
TABLES_DIR  <- "output/tables/"
VALUES_DIR  <- "output/values/"

# Cookie-deletion-specific bad users (deletion-loop logging artifacts, not
# global). Global BAD_USERS = c("2607a1f") from values.R; extend to match the
# cookie-deletion sample exactly:
#   6ccc7d5 - 200k AUTOMATIC_COOKIE_DELETION events in 10 min
#   7d6864c - 5 deletion events on a day with 0 panel browsing
BAD_USERS <- union(BAD_USERS, c("6ccc7d5", "7d6864c"))

# Significance stars (pipeline convention, matches cookie_deletion.R)
SIGNIF <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)

# Figure size (standardized with cookie_deletion.R; trend = time-axis line plot)
FIG_W       <- 8
FIG_H_TREND <- 5


# =============================================================================
# 1. DATA PREP
# =============================================================================
panel <- read_fst("../data/tracker_panel/panel_merged_CLEAN.fst", as.data.table = TRUE)

ec <- fread("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
ec_clean <- ec[in_experiment == "true" & !experiment_id %in% BAD_USERS]
ec_clean[wave_id == 3, wave_id := 2L]

# Drop panel's existing condition columns and re-merge clean (incl. block_idx)
drop_cols <- intersect(c("treatment", "experiment_condition", "wave_id", "block_idx"),
                       names(panel))
if (length(drop_cols) > 0) panel[, (drop_cols) := NULL]
panel <- panel[experiment_id %in% ec_clean$experiment_id]
panel <- merge(panel,
               ec_clean[, .(experiment_id, wave_id, experiment_condition, block_idx)],
               by = "experiment_id", all.x = TRUE)
panel[wave_id == 3, wave_id := 2L]
setnames(panel, "experiment_condition", "treatment")
panel[, treatment := factor(treatment, levels = c("control", "saliency", "info"))]

# block_by_wave = block_idx x wave_id (randomization unit, same as self-report table)
panel[, block_by_wave := paste(block_idx, wave_id, sep = "_")]

# Timeline
panel[, wave_start     := fifelse(wave_id == 1L, as.Date("2025-06-14"), as.Date("2025-06-28"))]
panel[, treatment_date := fifelse(wave_id == 1L, as.Date("2025-06-28"), as.Date("2025-07-12"))]
panel[, cookie_cutoff  := fifelse(wave_id == 1L, as.Date("2025-07-26"), as.Date("2025-08-09"))]

# Filter to pre-info + post-info (excluding the cookie-deletion period)
panel[, in_sample := date >= wave_start & date < cookie_cutoff]
panel[, has_data  := !is.na(visit_count) & visit_count > 0]
t4_raw <- panel[in_sample & has_data]

# SURVEY_WEBSITES filter (via aggregated domain to avoid substring overcatch)
site_df <- data.frame(website = unique(t4_raw$website), stringsAsFactors = FALSE)
site_df <- aggregate_time_data(site_df, field = "website")
site_df <- high_level_aggregate(site_df, field = "website_aggregated")
site_lookup <- as.data.table(unique(
  site_df[, c("website", "website_aggregated_high_level")]))
t4_raw <- merge(t4_raw, site_lookup, by = "website", all.x = TRUE)
t4_raw <- t4_raw[!(tolower(website_aggregated_high_level) %in% SURVEY_WEBSITES)]


# =============================================================================
# 2. AGGREGATE TO USER x DAY LEVEL
# =============================================================================
# daily_CPV = total cookies today / total visits today (captures all behavioral
# channels: avoidance, within-site opt-out, time shifts).
dt_user_day <- t4_raw[, .(
  total_cookies  = sum(n_cookies_third_party, na.rm = TRUE),
  total_visits   = sum(visit_count,           na.rm = TRUE),
  treatment      = first(treatment),
  wave_id        = first(wave_id),
  block_by_wave  = first(block_by_wave),
  treatment_date = first(treatment_date)
), by = .(experiment_id, date)]

dt_user_day[, daily_cpv     := total_cookies / total_visits]
dt_user_day[, log_daily_cpv := log(1 + daily_cpv)]
dt_user_day[, post          := as.integer(date >= treatment_date)]


# =============================================================================
# 3. SPEC 1: CROSS-SECTION (Pooled) -> tab:info_treatment_cpv
# =============================================================================
# Unit: user, aggregating post-info / pre-deletion days.
# Outcome: mean of log(1 + daily CPV) across the user's post days.
# FE: block_by_wave (randomization unit, same as self-report table).
dt_user_post <- dt_user_day[post == 1, .(
  mean_log_cpv  = mean(log_daily_cpv),
  treatment     = first(treatment),
  wave_id       = first(wave_id),
  block_by_wave = first(block_by_wave)
), by = experiment_id]

xs_pool <- feols(mean_log_cpv ~ i(treatment, ref = "control") | block_by_wave,
                 data = dt_user_post, se = "hetero", notes = FALSE)

DICT_XS <- c(`mean_log_cpv`        = "mean of log(1 + Daily Third-Party Cookies per Visit)",
             `treatment::saliency` = "Saliency Treatment",
             `treatment::info`     = "Information Treatment",
             `block_by_wave`       = "block-by-wave")

write_tabular_only(
  etable(xs_pool, keep = "%treatment", dict = DICT_XS,
         digits = 4, signif.code = SIGNIF,
         fitstat = c("n", "r2"), tex = TRUE),
  file = paste0(TABLES_DIR, "info_treatment_cross_section.tex"))


# =============================================================================
# 4. SPEC 2: DiD (Pooled) -> tab:info_treatment_cpv_did
# =============================================================================
# Unit: user x day (pre + post info). Outcome: log(1 + daily CPV).
# FE: user + calendar date. SE clustered at user.
did_pool <- feols(log_daily_cpv ~ i(treatment, post, ref = "control")
                  | experiment_id + date,
                  data = dt_user_day, cluster = ~experiment_id, notes = FALSE)

DICT_DID <- c(`log_daily_cpv`            = "log(1 + Daily Third-Party Cookies per Visit)",
              `treatment::saliency:post` = "Saliency $\\times$ Post",
              `treatment::info:post`     = "Information $\\times$ Post",
              `experiment_id`            = "User", `date` = "Date")

write_tabular_only(
  etable(did_pool, keep = "%treatment", dict = DICT_DID,
         digits = 4, signif.code = SIGNIF,
         fitstat = c("n", "r2"), tex = TRUE),
  file = paste0(TABLES_DIR, "info_treatment_did.tex"))

cat("Saved: info_treatment_cross_section.tex, info_treatment_did.tex\n")


# =============================================================================
# 5. CPV TREND PLOT (Pooled, event time) -> fig:cpv_trend_by_treatment
# =============================================================================
# Stage 1 drew two calendar-date plots (one per wave). Pooled here onto a single
# event-time axis: x = days relative to each user's own information-intervention
# date (Wave 1: 06/28, Wave 2: 07/12). Three arms keep color (treatment arms
# always use scale_color_treatment); intervention line is gray (no red).
dt_user_day[, info_tau := as.integer(date - treatment_date)]

daily_by_tx <- dt_user_day[, .(mean_log_cpv = mean(log_daily_cpv)),
                           by = .(info_tau, treatment)]
daily_by_tx[, treatment_label := factor(
  fcase(treatment == "control",  "Control",
        treatment == "saliency", "Saliency",
        treatment == "info",     "Information"),
  levels = TREATMENT_ORDER)]
setorder(daily_by_tx, treatment_label, info_tau)

p_trend <- ggplot(daily_by_tx,
                  aes(x = info_tau, y = mean_log_cpv, color = treatment_label)) +
  geom_line(linewidth = LINE_WIDTH) +
  geom_point(size = POINT_SIZE * 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_treatment() +
  labs(x = "Days Relative to Information Intervention",
       y = "Mean log(1 + Daily Cookies per Visit)", color = NULL) +
  theme_privacy_experiment(legend_position = "bottom",
                           show_grid_x = TRUE, show_grid_y = TRUE)

ggsave(paste0(FIGURES_DIR, "cpv_trend_by_treatment.pdf"), p_trend, width = FIG_W, height = FIG_H_TREND)
cat("Saved: cpv_trend_by_treatment.pdf\n")


# =============================================================================
# INLINE SCALARS (Pooled) -> output/values/data_sharing_info_values.tex
# =============================================================================
# Independent bundle (not shared with cookie_deletion.R) so run order is free.
info_values_file <- file.path(VALUES_DIR, "data_sharing_info_values.tex")
suppressWarnings(file.remove(info_values_file))

# Cross-section: Information vs control
xs_info_b <- coef(xs_pool)["treatment::info"]
xs_info_p <- pvalue(xs_pool)["treatment::info"]
save_tex_value(format_coef(xs_info_b),
               name = "cpvXsInfoCoef", file = info_values_file)
save_tex_value(format_pvalue(xs_info_p),
               name = "cpvXsInfoPval", file = info_values_file)
save_tex_value(format_pct(100 * (exp(xs_info_b) - 1)),
               name = "cpvXsInfoPct", file = info_values_file)

# DiD: Information x Post and Saliency x Post
did_info_b <- coef(did_pool)["treatment::info:post"]
did_info_p <- pvalue(did_pool)["treatment::info:post"]
did_sal_b  <- coef(did_pool)["treatment::saliency:post"]
did_sal_p  <- pvalue(did_pool)["treatment::saliency:post"]
save_tex_value(format_coef(did_info_b),
               name = "cpvDidInfoCoef", file = info_values_file)
save_tex_value(format_pvalue(did_info_p),
               name = "cpvDidInfoPval", file = info_values_file)
save_tex_value(format_pct(100 * (exp(did_info_b) - 1)),
               name = "cpvDidInfoPct", file = info_values_file)
save_tex_value(format_coef(did_sal_b),
               name = "cpvDidSalCoef", file = info_values_file)
save_tex_value(format_pvalue(did_sal_p),
               name = "cpvDidSalPval", file = info_values_file)

cat("Saved 8 macros to", info_values_file, "\n")
cat("=== DONE ===\n")