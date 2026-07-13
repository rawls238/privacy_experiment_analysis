# =============================================================================
# check_extreme_cookies.R   (THROWAWAY diagnostic -- do NOT commit)
#
# Detailed investigation of extreme third-party cookie counts. Read-only on
# panel_merged_CLEAN.fst; writes NOTHING to output/. Console + /tmp only.
#
# Blocks:
#   1. Global distribution of n_cookies_third_party and cookies_per_visit
#   2. Record-level top-30 outliers (user, website, day)
#   3. Per-user extremes (max daily count, total) -> suspected bad users
#   4. Per-website extremes (mean/median/p99/max CPV, baseline CPV)
#   5. nytimes deep-dive + top-1%-trimmed DiD refit (is -2.13 outlier-driven?)
#   6. Winsorized / trimmed sensitivity of the pooled CPV DiD (-0.329)
#
# Run: Rscript ~/Downloads/check_extreme_cookies.R 2>&1 | tee /tmp/extreme_cookies.txt
# =============================================================================

setwd("~/Dropbox/spring2025experiment/code_github")
suppressMessages({ library(data.table); library(fst); library(fixest) })
source("replication_files/utils/values.R")              # BAD_USERS, SURVEY_WEBSITES
source("replication_files/utils/time_usage_helpers.R")  # aggregate_time_data, high_level_aggregate
source("replication_files/utils/number_format_helpers.R")
select <- dplyr::select

BAD_USERS <- union(BAD_USERS, c("6ccc7d5", "7d6864c"))
TAU_MIN <- -7; TAU_MAX <- 6
options(datatable.print.class = FALSE)

pr <- function(...) cat(sprintf(...), "\n")
hr <- function(t) cat("\n", strrep("=", 70), "\n", t, "\n", strrep("=", 70), "\n", sep = "")

# -----------------------------------------------------------------------------
# Load raw panel (BEFORE bad-user removal, so we can SEE the bad users too)
# -----------------------------------------------------------------------------
raw <- read_fst("../data/tracker_panel/panel_merged_CLEAN.fst", as.data.table = TRUE)
pr("Loaded panel_merged_CLEAN.fst: %d rows, %d cols", nrow(raw), ncol(raw))
raw[, cpv := n_cookies_third_party / visit_count]   # recompute to be safe

# =============================================================================
hr("BLOCK 1: GLOBAL DISTRIBUTION")
# =============================================================================
qs <- c(0.5, 0.9, 0.95, 0.99, 0.999, 1)
cat("\nn_cookies_third_party quantiles (all rows):\n")
print(round(quantile(raw$n_cookies_third_party, qs, na.rm = TRUE), 1))
cat("\ncookies_per_visit (n_cookies_third_party / visit_count) quantiles, visit_count>0:\n")
print(round(quantile(raw[visit_count > 0, cpv], qs, na.rm = TRUE), 2))
pr("\nrows with n_cookies_third_party > 100 : %d", raw[n_cookies_third_party > 100, .N])
pr("rows with n_cookies_third_party > 500 : %d", raw[n_cookies_third_party > 500, .N])
pr("rows with cpv > 50                    : %d", raw[visit_count > 0 & cpv > 50, .N])

# =============================================================================
hr("BLOCK 2: RECORD-LEVEL TOP-30 OUTLIERS (raw cookie count)")
# =============================================================================
top_rec <- raw[order(-n_cookies_third_party)][1:30,
                                              .(experiment_id, website, date, n_cookies_third_party, visit_count,
                                                cpv = round(cpv, 1), in_bad_users = experiment_id %in% BAD_USERS)]
print(top_rec)

cat("\nSame, but by cookies-per-visit (visit_count > 0):\n")
top_cpv <- raw[visit_count > 0][order(-cpv)][1:30,
                                             .(experiment_id, website, date, n_cookies_third_party, visit_count,
                                               cpv = round(cpv, 1), in_bad_users = experiment_id %in% BAD_USERS)]
print(top_cpv)

# =============================================================================
hr("BLOCK 3: PER-USER EXTREMES (suspected bad users beyond the known 2)")
# =============================================================================
user_ext <- raw[, .(
  max_daily_cookies = max(n_cookies_third_party, na.rm = TRUE),
  total_cookies     = sum(n_cookies_third_party, na.rm = TRUE),
  n_days            = uniqueN(date),
  max_cpv           = max(cpv, na.rm = TRUE)
), by = experiment_id][order(-max_daily_cookies)]
user_ext[, known_bad := experiment_id %in% BAD_USERS]
cat("\nTop 25 users by max single-day third-party cookie count:\n")
print(user_ext[1:25])
pr("\nMedian across users of max_daily_cookies: %.0f", median(user_ext$max_daily_cookies))
pr("Users with max_daily_cookies > 500 (excl. known bad): %d",
   user_ext[max_daily_cookies > 500 & !known_bad, .N])

# =============================================================================
hr("BLOCK 4: PER-WEBSITE EXTREMES")
# =============================================================================
# Restrict to non-bad users for website-level stats
w <- raw[!experiment_id %in% BAD_USERS & visit_count > 0]
site_stats <- w[, .(
  n_obs       = .N,
  n_users     = uniqueN(experiment_id),
  mean_cpv    = round(mean(cpv), 1),
  median_cpv  = round(median(cpv), 1),
  p99_cpv     = round(quantile(cpv, 0.99), 1),
  max_cpv     = round(max(cpv), 1),
  max_cookies = max(n_cookies_third_party)
), by = website][n_obs >= 100][order(-mean_cpv)]
cat("\nTop 30 websites by mean cookies-per-visit (>=100 obs):\n")
print(site_stats[1:30])

# =============================================================================
hr("BLOCK 5: NYTIMES DEEP-DIVE  (why is the DiD -2.13?)")
# =============================================================================
# Rebuild the exact t1 analysis sample used in cookie_deletion.R so the DiD
# here is comparable to the -2.13 reported per-site coefficient.
panel <- copy(raw)
ec <- fread("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
ec_clean <- ec[in_experiment == "true" & !experiment_id %in% BAD_USERS]
ec_clean[wave_id == 3, wave_id := 2L]
drop_cols <- intersect(c("wave_id","treatment","experiment_condition"), names(panel))
if (length(drop_cols) > 0) panel[, (drop_cols) := NULL]
panel <- panel[experiment_id %in% ec_clean$experiment_id]
panel <- merge(panel, ec_clean[, .(experiment_id, wave_id, experiment_condition)],
               by = "experiment_id", all.x = TRUE)
panel[wave_id == 3, wave_id := 2L]
panel[, c1_anchor := fifelse(wave_id == 1L, as.Date("2025-07-26"), as.Date("2025-08-09"))]
panel[, tau := as.integer(date - c1_anchor)]
t1 <- panel[tau >= TAU_MIN & tau <= TAU_MAX & !is.na(visit_count) & visit_count > 0]
t1[, cpv_3p := n_cookies_third_party / visit_count]
t1[, log_cpv_3p := log(1 + cpv_3p)]
t1[, treated := as.integer(cookie_treatment_idx == 1)]
t1[, post := as.integer(tau >= 0)]
t1[, post_treated := post * treated]
t1[, dow := factor(weekdays(date))]

nyt <- t1[website == "www.nytimes.com"]
pr("nytimes rows in analysis window: %d  (users: %d)", nrow(nyt), uniqueN(nyt$experiment_id))
cat("\nnytimes cpv distribution (pre vs post):\n")
print(nyt[, .(n = .N, mean_cpv = round(mean(cpv_3p),1), median_cpv = round(median(cpv_3p),1),
              p99 = round(quantile(cpv_3p,0.99),1), max = round(max(cpv_3p),1)), by = post][order(post)])

fit_site <- function(d) {
  m <- tryCatch(feols(log_cpv_3p ~ post_treated | experiment_id + dow,
                      data = d, cluster = ~experiment_id, notes = FALSE),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ci <- confint(m, "post_treated")
  pr("  coef = %+.3f   CI [%+.3f, %+.3f]   N = %d",
     coef(m)["post_treated"], ci[1,1], ci[1,2], nobs(m))
}
cat("\nnytimes DiD, FULL sample:\n");            fit_site(nyt)
cut99 <- quantile(nyt$cpv_3p, 0.99, na.rm = TRUE)
cat(sprintf("\nnytimes DiD, dropping cpv > p99 (=%.0f):\n", cut99)); fit_site(nyt[cpv_3p <= cut99])
cut95 <- quantile(nyt$cpv_3p, 0.95, na.rm = TRUE)
cat(sprintf("\nnytimes DiD, dropping cpv > p95 (=%.0f):\n", cut95)); fit_site(nyt[cpv_3p <= cut95])

# =============================================================================
hr("BLOCK 6: SENSITIVITY OF POOLED CPV DiD (-0.329) TO EXTREMES")
# =============================================================================
base <- feols(log_cpv_3p ~ post_treated | experiment_id + website + dow,
              data = t1, cluster = ~experiment_id, notes = FALSE)
pr("Baseline pooled DiD                         : %+.4f (N=%d)",
   coef(base)["post_treated"], nobs(base))

for (p in c(0.999, 0.99, 0.95)) {
  cut <- quantile(t1$cpv_3p, p, na.rm = TRUE)
  d <- t1[cpv_3p <= cut]
  m <- feols(log_cpv_3p ~ post_treated | experiment_id + website + dow,
             data = d, cluster = ~experiment_id, notes = FALSE)
  pr("Dropping cpv > p%-5.1f (cut=%6.0f)          : %+.4f (N=%d, dropped %d)",
     100*p, cut, coef(m)["post_treated"], nobs(m), nrow(t1) - nrow(d))
}

# Winsorize at p99 instead of dropping
cutw <- quantile(t1$cpv_3p, 0.99, na.rm = TRUE)
tw <- copy(t1); tw[cpv_3p > cutw, cpv_3p := cutw][, log_cpv_3p := log(1 + cpv_3p)]
mw <- feols(log_cpv_3p ~ post_treated | experiment_id + website + dow,
            data = tw, cluster = ~experiment_id, notes = FALSE)
pr("Winsorized at p99                           : %+.4f (N=%d)", coef(mw)["post_treated"], nobs(mw))

cat("\n=== DONE (nothing written to output/) ===\n")