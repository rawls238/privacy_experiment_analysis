# =============================================================================
# data_sharing.R   (replication_files pipeline version)
#
# Within-website data sharing: balanced-panel CPV DiD -- Appendix E
# (writeup_v4.tex, Data Sharing section, "Table XX" / MARK FILL IN HERE).
#
# RESEARCH QUESTION
#   Did the information intervention change data sharing WITHIN the websites
#   users continue to use? The outcome is third-party cookies per visit at
#   the (user, website, day) level, so with user + website FE the estimate
#   captures within-user, within-site changes -- not substitution across
#   sites (site choice is analyzed elsewhere in the paper).
#
# SPECIFICATION (Table XX)
#   log(1 + daily CPV)_ijt = b1*(Sal_i x Post_t) + b2*(Info_i x Post_t)
#                            + eta_i + eta_j + eta_t + eps_ijt
#   User, website, and calendar-date FE; SEs clustered at the user level.
#
# BALANCED PANEL
#   Restricted to (user, website) pairs observed at least once in BOTH the
#   pre-intervention and post-intervention periods, so the coefficient is
#   identified from within-pair changes. Strictly-daily balance is infeasible
#   (<1% of pairs); the >=1-day-each-side definition retains ~18% of pairs
#   and ~61% of observations.
#
# SAMPLE
#   Information-intervention experiment window (pre + post info, excluding
#   the cookie-deletion period):
#     Wave 1: 06/14 - 07/25   Wave 2: 06/28 - 08/08
#
# CHANGES vs previous version of this script:
#   - REMOVED Spec 1 (user-level cross-section), Spec 2 (user-level DiD),
#     and the event-time trend plot, plus their 8 macros (\cpvXs*, \cpvDid*).
#     User-level aggregation mixes within-site changes with cross-site
#     substitution; per author decision (Guy, Slack) this section is about
#     within-site changes only, so those analyses are superseded by the
#     balanced-panel specification below. The corresponding paper content
#     was moved into a \begin{comment} block in writeup_v4.tex.
#   - Outputs: info_treatment_balanced_panel_did.tex replaces
#     info_treatment_cross_section.tex / info_treatment_did.tex /
#     cpv_trend_by_treatment.pdf.
#   - data_sharing_info_values.tex macro set replaced (\cpvBal*).
#
# Inputs:
#   ../data/tracker_panel/panel_merged_CLEAN.fst
#   ../data/final_extension_data/experiment_conditions_pilot_july_2024.csv
#
# Dependencies:
#   replication_files/utils/values.R
#   replication_files/utils/time_usage_helpers.R
#   replication_files/utils/number_format_helpers.R
#   replication_files/utils/tex_helpers.R
#
# Outputs:
#   output/tables/info_treatment_balanced_panel_did.tex
#   output/values/data_sharing_info_values.tex
#     \cpvBalSalCoef \cpvBalSalPval \cpvBalInfoCoef \cpvBalInfoPval
#     \cpvBalPairPct \cpvBalObsPct \cpvBalNUsers
#
# USAGE
#   setwd("~/Dropbox/spring2025experiment/code_github")
#   source("replication_files/data_sharing_n_cookie_deletion/data_sharing.R")
# =============================================================================

library(jsonlite)   # MUST precede utils: time_usage_helpers.R uses fromJSON()
library(data.table)
library(fst)
library(fixest)
library(savetexvalue)

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/values.R")              # SURVEY_WEBSITES, BAD_USERS, dates
source("replication_files/utils/time_usage_helpers.R")  # aggregate_time_data, high_level_aggregate
source("replication_files/utils/number_format_helpers.R")
source("replication_files/utils/tex_helpers.R")         # write_tabular_only

select <- dplyr::select  # prevent data.table masking

TABLES_DIR <- "output/tables/"
VALUES_DIR <- "output/values/"

# Cookie-deletion-specific bad users (deletion-loop logging artifacts), kept
# consistent with cookie_deletion_checks_narrow_down.R:
#   6ccc7d5 - 200k AUTOMATIC_COOKIE_DELETION events in 10 min
#   7d6864c - 5 deletion events on a day with 0 panel browsing
BAD_USERS <- union(BAD_USERS, c("6ccc7d5", "7d6864c"))

SIGNIF <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)

# =============================================================================
# 1. DATA PREP
# =============================================================================
panel <- read_fst("../data/tracker_panel/panel_merged_CLEAN.fst", as.data.table = TRUE)

ec <- fread("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
ec_clean <- ec[in_experiment == "true" & !experiment_id %in% BAD_USERS]
ec_clean[wave_id == 3, wave_id := 2L]

drop_cols <- intersect(c("treatment", "experiment_condition", "wave_id", "block_idx"),
                       names(panel))
if (length(drop_cols) > 0) panel[, (drop_cols) := NULL]
panel <- panel[experiment_id %in% ec_clean$experiment_id]
panel <- merge(panel, ec_clean[, .(experiment_id, wave_id, experiment_condition)],
               by = "experiment_id", all.x = TRUE)
panel[wave_id == 3, wave_id := 2L]

# Info-experiment timeline (values.R constants)
panel[, wave_start     := fifelse(wave_id == 1L, START_DATE_WAVE_1,     START_DATE_WAVE_2)]
panel[, treatment_date := fifelse(wave_id == 1L, TREATMENT_DATE_WAVE_1, TREATMENT_DATE_WAVE_2)]
panel[, cookie_cutoff  := fifelse(wave_id == 1L, COOKIE_TREATMENT_WAVE_1_1,
                                  COOKIE_TREATMENT_WAVE_2_1)]
panel <- panel[date >= wave_start & date < cookie_cutoff &
                 !is.na(visit_count) & visit_count > 0]

# SURVEY_WEBSITES filter (via aggregated domain to avoid substring overcatch)
site_df <- data.frame(website = unique(panel$website), stringsAsFactors = FALSE)
site_df <- aggregate_time_data(site_df, field = "website")
site_df <- high_level_aggregate(site_df, field = "website_aggregated")
site_lookup <- as.data.table(unique(
  site_df[, c("website", "website_aggregated_high_level")]))
panel <- merge(panel, site_lookup, by = "website", all.x = TRUE)
panel <- panel[!(tolower(website_aggregated_high_level) %in% SURVEY_WEBSITES)]

panel[, post      := as.integer(date >= treatment_date)]
panel[, log_cpv   := log(1 + n_cookies_third_party / visit_count)]
panel[, treatment := factor(experiment_condition,
                            levels = c("control", "saliency", "info"))]

# =============================================================================
# 2. BALANCED PANEL: (user, website) pairs observed pre AND post
# =============================================================================
pw <- panel[, .(n_pre = sum(post == 0), n_post = sum(post == 1)),
            by = .(experiment_id, website)]
pair_pct <- 100 * pw[n_pre > 0 & n_post > 0, .N] / nrow(pw)

bal <- merge(panel, pw[n_pre > 0 & n_post > 0, .(experiment_id, website)],
             by = c("experiment_id", "website"))
obs_pct <- 100 * nrow(bal) / nrow(panel)

cat(sprintf("Balanced panel: %.1f%% of pairs, %.1f%% of observations, %d users, %d sites\n",
            pair_pct, obs_pct, uniqueN(bal$experiment_id), uniqueN(bal$website)))

# =============================================================================
# 3. TABLE XX: within-site DiD -> info_treatment_balanced_panel_did.tex
# =============================================================================
m_bal <- feols(log_cpv ~ i(treatment, post, ref = "control")
               | experiment_id + website + date,
               data = bal, cluster = ~experiment_id, notes = FALSE)

DICT_BAL <- c(`treatment::saliency:post` = "Saliency $\\times$ Post",
              `treatment::info:post`     = "Information $\\times$ Post",
              `experiment_id`            = "Participant FE",
              `website`                  = "Website FE",
              `date`                     = "Date FE")

bal_tex <- etable(m_bal, dict = DICT_BAL, digits = 3, signif.code = SIGNIF,
                  depvar = FALSE, fitstat = c("n", "r2"), tex = TRUE)
write_tabular_only(bal_tex,
                   file = paste0(TABLES_DIR, "info_treatment_balanced_panel_did.tex"))

# =============================================================================
# 4. INLINE SCALARS -> data_sharing_info_values.tex
# =============================================================================
info_values_file <- file.path(VALUES_DIR, "data_sharing_info_values.tex")
suppressWarnings(file.remove(info_values_file))

save_tex_value(format_coef(coef(m_bal)["treatment::saliency:post"]),
               name = "cpvBalSalCoef", file = info_values_file)
save_tex_value(format_pvalue(pvalue(m_bal)["treatment::saliency:post"]),
               name = "cpvBalSalPval", file = info_values_file)
save_tex_value(format_coef(coef(m_bal)["treatment::info:post"]),
               name = "cpvBalInfoCoef", file = info_values_file)
save_tex_value(format_pvalue(pvalue(m_bal)["treatment::info:post"]),
               name = "cpvBalInfoPval", file = info_values_file)
save_tex_value(format_pct(pair_pct),
               name = "cpvBalPairPct", file = info_values_file)
save_tex_value(format_pct(obs_pct),
               name = "cpvBalObsPct", file = info_values_file)
save_tex_value(format_count(uniqueN(bal$experiment_id)),
               name = "cpvBalNUsers", file = info_values_file)

cat(sprintf("Saved 7 macros to %s\n", info_values_file))
cat("=== DONE ===\n")