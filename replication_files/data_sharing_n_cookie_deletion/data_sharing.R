# =============================================================================
# data_sharing.R   (replication_files pipeline version)
#
# Within-website data sharing: balanced-panel CPV DiD -- Appendix D
# (main_v2.tex, D.2 Data Sharing, Table D.3 [tab:cpv_balanced_panel]).
#
# RESEARCH QUESTION
#   Did the information intervention change data sharing WITHIN the websites
#   users continue to use? Outcomes are measured at the (user, website, day)
#   level, so with user + website FE the estimate captures within-user,
#   within-site changes -- not substitution across sites (site choice is
#   analyzed elsewhere in the paper).
#
# SPECIFICATION (Table D.3)
#   y_ijt = b1*(Sal_i x Post_t) + b2*(Info_i x Post_t)
#           + eta_i + eta_j + eta_t + eps_ijt
#   User, website, and calendar-date FE; SEs clustered at the user level.
#   Two dependent variables:
#     (1) log(1 + third-party cookies per visit)   -- event intensity
#     (2) log(1 + unique third-party cookies)      -- exposure breadth
#
# BALANCED PANEL
#   Restricted to (user, website) pairs observed at least once in BOTH the
#   pre-intervention and post-intervention periods, so the coefficient is
#   identified from within-pair changes. Strictly-daily balance is infeasible
#   (<1% of pairs); the >=1-day-each-side definition retains ~18% of pairs
#   and ~60% of observations.
#
# SAMPLE
#   Information-intervention experiment window (pre + post info, excluding
#   the cookie-deletion period):
#     Wave 1: 06/14 - 07/25   Wave 2: 06/28 - 08/08
#
# CHANGES vs previous version of this script:
#   - COOKIE MEASURE REPLACED. The old panel column n_cookies_third_party
#     classified third-party cookies by substring match, which contradicts the
#     definition stated in the paper (registrable domain: cookies set by
#     accounts.google.com on mail.google.com are FIRST party). Cookie measures
#     now come from panel_cookies_v2.fst, the same rebuilt panel used by
#     cookie_deletion.R, so Appendix D and Appendix E share one definition.
#     The old panel's ~4k duplicated (user, website, day) keys are removed
#     before the merge; verified lossless (duplicated keys agree on
#     time_spent and visit_count in every case).
#   - SECOND DEPENDENT VARIABLE ADDED. log(1 + unique_cookies_3rd_p) joins CPV
#     as column (2), mirroring Table E.1. CPV is an event count and is
#     inflated on dynamic sites (Gmail, Drive) that re-set the same cookie
#     through background polling; the unique count is not.
#   - COLUMN HEADERS ADDED so the dependent variable is named, matching the
#     format of Tables D.1 and D.2.
#   - Four \cpvBalUc* macros added.
#
#   Earlier version had also REMOVED Spec 1 (user-level cross-section), Spec 2
#   (user-level DiD), and the event-time trend plot, plus their 8 macros
#   (\cpvXs*, \cpvDid*). User-level aggregation mixes within-site changes with
#   cross-site substitution; per author decision (Guy, Slack) this section is
#   about within-site changes only. The corresponding paper content was moved
#   into a \begin{comment} block.
#
# Inputs:
#   ../data/tracker_panel/panel_merged_CLEAN.fst
#   ../data/processed_data/panel_cookies_v2.fst
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
#     \cpvBalSalCoef   \cpvBalSalPval   \cpvBalInfoCoef   \cpvBalInfoPval
#     \cpvBalUcSalCoef \cpvBalUcSalPval \cpvBalUcInfoCoef \cpvBalUcInfoPval
#     \cpvBalPairPct   \cpvBalObsPct    \cpvBalNUsers
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
# consistent with cookie_deletion.R:
#   6ccc7d5 - 200k AUTOMATIC_COOKIE_DELETION events in 10 min
#   7d6864c - 5 deletion events on a day with 0 panel browsing
BAD_USERS <- union(BAD_USERS, c("6ccc7d5", "7d6864c"))

SIGNIF <- c("***" = 0.01, "**" = 0.05, "*" = 0.1)

# =============================================================================
# 1. DATA PREP
# =============================================================================
panel <- read_fst("../data/tracker_panel/panel_merged_CLEAN.fst", as.data.table = TRUE)

# --- [CHANGED] align the cookie measure with cookie_deletion.R --------------
# Drop the old substring-matched columns, deduplicate, and merge the rebuilt
# cookie panel (registrable-domain classification, gate-validated).
panel[, date := as.Date(date)]
panel[, c("n_cookies_third_party", "n_trackers_third_party",
          "cookies_per_visit") := NULL]

n_before <- nrow(panel)
panel <- unique(panel, by = c("experiment_id", "website", "date"))
cat(sprintf("Dedupe: %d -> %d rows (%d dropped)\n",
            n_before, nrow(panel), n_before - nrow(panel)))

cookies <- read_fst("../data/processed_data/panel_cookies_v2.fst",
                    as.data.table = TRUE)
cookies[, date := as.Date(date)]
panel <- merge(panel,
               cookies[, .(experiment_id, website, date,
                           cookie_events_3rd_p, unique_cookies_3rd_p)],
               by = c("experiment_id", "website", "date"), all.x = TRUE)

# Guard: a key-type mismatch would yield an all-NA merge, which the
# fill-with-zero step below would silently turn into a valid-looking all-zero
# outcome, with no error anywhere downstream.
match_rate <- 100 * mean(!is.na(panel$cookie_events_3rd_p))
cat(sprintf("Cookie merge match rate: %.1f%%\n", match_rate))
stopifnot(match_rate > 50)

# Unmatched rows are (user, site, day) cells with no third-party cookie
# activity: panel_cookies_v2 only holds cells where an event fired. Verified
# non-differential across treatment x post before adopting this fill.
panel[is.na(cookie_events_3rd_p),  cookie_events_3rd_p  := 0L]
panel[is.na(unique_cookies_3rd_p), unique_cookies_3rd_p := 0L]
rm(cookies); gc(verbose = FALSE)

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
panel[, treatment := factor(experiment_condition,
                            levels = c("control", "saliency", "info"))]

# --- [CHANGED] dependent variables -----------------------------------------
# CPV normalizes event counts by visits (intensity). UC is NOT divided by
# visits: a unique-cookie count does not scale linearly with visit count, so
# dividing would inject spurious variation. Matches cookie_deletion.R.
panel[, log_cpv := log(1 + cookie_events_3rd_p / visit_count)]
panel[, log_uc  := log(1 + unique_cookies_3rd_p)]

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
# 3. TABLE D.3: within-site DiD -> info_treatment_balanced_panel_did.tex
# =============================================================================
# [CHANGED] Two dependent variables, mirroring Table E.1, with column headers
# naming each one so the format matches Tables D.1 and D.2.
m_bal_cpv <- feols(log_cpv ~ i(treatment, post, ref = "control")
                   | experiment_id + website + date,
                   data = bal, cluster = ~experiment_id, notes = FALSE)
m_bal_uc  <- feols(log_uc ~ i(treatment, post, ref = "control")
                   | experiment_id + website + date,
                   data = bal, cluster = ~experiment_id, notes = FALSE)

DICT_BAL <- c(`treatment::saliency:post` = "Saliency $\\times$ Post",
              `treatment::info:post`     = "Information $\\times$ Post",
              `experiment_id`            = "Participant FE",
              `website`                  = "Website FE",
              `date`                     = "Date FE")

bal_tex <- etable(m_bal_cpv, m_bal_uc,
                  headers = c("log(CPV)", "log(Unique 3rd Party Cookies)"),
                  dict = DICT_BAL, digits = 3, signif.code = SIGNIF,
                  depvar = FALSE, fitstat = c("n", "r2"), tex = TRUE)
write_tabular_only(bal_tex,
                   file = paste0(TABLES_DIR, "info_treatment_balanced_panel_did.tex"))

# =============================================================================
# 4. INLINE SCALARS -> data_sharing_info_values.tex
# =============================================================================
info_values_file <- paste0(VALUES_DIR, "data_sharing_info_values.tex")
suppressWarnings(file.remove(info_values_file))

# --- column (1): cookies per visit ------------------------------------------
save_tex_value(format_coef(coef(m_bal_cpv)["treatment::saliency:post"]),
               name = "cpvBalSalCoef", file = info_values_file)
save_tex_value(format_pvalue(pvalue(m_bal_cpv)["treatment::saliency:post"]),
               name = "cpvBalSalPval", file = info_values_file)
save_tex_value(format_coef(coef(m_bal_cpv)["treatment::info:post"]),
               name = "cpvBalInfoCoef", file = info_values_file)
save_tex_value(format_pvalue(pvalue(m_bal_cpv)["treatment::info:post"]),
               name = "cpvBalInfoPval", file = info_values_file)

# --- [CHANGED] column (2): unique third-party cookies -----------------------
save_tex_value(format_coef(coef(m_bal_uc)["treatment::saliency:post"]),
               name = "cpvBalUcSalCoef", file = info_values_file)
save_tex_value(format_pvalue(pvalue(m_bal_uc)["treatment::saliency:post"]),
               name = "cpvBalUcSalPval", file = info_values_file)
save_tex_value(format_coef(coef(m_bal_uc)["treatment::info:post"]),
               name = "cpvBalUcInfoCoef", file = info_values_file)
save_tex_value(format_pvalue(pvalue(m_bal_uc)["treatment::info:post"]),
               name = "cpvBalUcInfoPval", file = info_values_file)

# --- sample composition ------------------------------------------------------
save_tex_value(format_pct(pair_pct),
               name = "cpvBalPairPct", file = info_values_file)
save_tex_value(format_pct(obs_pct),
               name = "cpvBalObsPct", file = info_values_file)
save_tex_value(format_count(uniqueN(bal$experiment_id)),
               name = "cpvBalNUsers", file = info_values_file)

cat(sprintf("Saved 11 macros to %s\n", info_values_file))
cat("=== DONE ===\n")