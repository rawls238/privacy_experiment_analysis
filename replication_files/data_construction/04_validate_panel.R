# =============================================================================
# 04_validate_panel.R
# (replication_files/data_construction/04_validate_panel.R)
#
# Validation gate for panel_cookies_v2.fst. The rebuilt panel may not feed any
# downstream analysis until this script passes.
#
#   V1: coverage vs the old panel (users / websites / user-days)
#   V2: per-site old-vs-new reconciliation, top 50 (old n_cookies_third_party
#       vs new events vs new UC, side by side with % change)
#   V3: named-site spot checks (gmail collapses; nytimes/wowhead move little)
#   V4: DiD on corrected data -- THE HEADLINE NUMBERS:
#         (1) log(1 + CPV_v2)  where CPV_v2 = events_3p / visit_count
#         (2) log(1 + UC)      unique third-party cookies
#         (3) log(1 + trackers)
#       visit_count comes from the old panel (time pipeline is unaffected by
#       the cookie bugs), merged on (experiment_id, website, date).
#
# Read-only on the old panel. Writes NOTHING to output/.
# Run: Rscript replication_files/data_construction/04_validate_panel.R \
#        2>&1 | tee /tmp/panel_validation.txt
# =============================================================================

setwd("~/Dropbox/spring2025experiment/code_github")
suppressMessages({ library(data.table); library(fst); library(fixest) })
setDTthreads(0)
source("replication_files/utils/values.R")   # BAD_USERS

BAD_USERS <- union(BAD_USERS, c("6ccc7d5", "7d6864c"))
TAU_MIN <- -7; TAU_MAX <- 6

pr    <- function(...) cat(sprintf(...), "\n")
comma <- function(x) format(x, big.mark = ",")
hr    <- function(t) cat("\n", strrep("=", 72), "\n", t, "\n", strrep("=", 72), "\n", sep = "")
PASS  <- function(ok, msg) { ok <- isTRUE(ok); cat(if (ok) "  PASS  " else "  FAIL  ", msg, "\n", sep = ""); ok }
all_ok <- TRUE

new <- read_fst("../data/processed_data/panel_cookies_v2.fst", as.data.table = TRUE)
old <- read_fst("../data/tracker_panel/panel_merged_CLEAN.fst",  as.data.table = TRUE)
old <- old[, .(experiment_id, website, date = as.Date(date),
               n_cookies_third_party, n_trackers_third_party,
               visit_count, time_spent)]

# =============================================================================
hr("V1: COVERAGE vs OLD PANEL")
# =============================================================================
pr("new: %s cells | %s users | %s sites | %s to %s",
   comma(nrow(new)), comma(uniqueN(new$experiment_id)),
   comma(uniqueN(new$website)), min(new$date), max(new$date))
pr("old: %s cells | %s users | %s sites | %s to %s",
   comma(nrow(old)), comma(uniqueN(old$experiment_id)),
   comma(uniqueN(old$website)), min(old$date), max(old$date))
common_dates <- new[date >= max(min(old$date), min(new$date)) &
                      date <= min(max(old$date), max(new$date))]
mg <- merge(old, new, by = c("experiment_id", "website", "date"))
pr("cells matched old<->new on common keys: %s (%.1f%% of old cells in the overlap window)",
   comma(nrow(mg)),
   100 * nrow(mg) / old[date >= min(new$date) & date <= max(new$date), .N])
all_ok <- PASS(nrow(mg) > 0.7 * old[date >= min(new$date) & date <= max(new$date), .N],
               "at least 70% of old cells matched in the overlap window") && all_ok

# =============================================================================
hr("V2: PER-SITE RECONCILIATION (top 50 by old third-party count)")
# =============================================================================
rec <- mg[, .(old_events = sum(n_cookies_third_party),
              new_events = sum(cookie_events_3rd_p),
              new_uc     = sum(unique_cookies_3rd_p)),
          by = website][order(-old_events)][1:50]
rec[, pct_events := round(100 * (new_events - old_events) / pmax(old_events, 1), 1)]
print(rec)
pr("\nInterpretation: sites dominated by own-subdomain cookies (gmail, docs,")
pr("calendar) should collapse; genuine third-party sites (nytimes, wowhead)")
pr("should change little on events.")

# =============================================================================
hr("V3: NAMED-SITE SPOT CHECKS")
# =============================================================================
spot <- function(site) rec[website == site]
g <- spot("mail.google.com"); n <- spot("www.nytimes.com"); w <- spot("www.wowhead.com")
if (nrow(g)) all_ok <- PASS(g$new_events < 0.05 * g$old_events,
                            sprintf("mail.google.com third-party events collapse (old %s -> new %s)",
                                    comma(g$old_events), comma(g$new_events))) && all_ok
if (nrow(n)) all_ok <- PASS(abs(n$pct_events) < 50,
                            sprintf("www.nytimes.com events change moderately (%.1f%%)", n$pct_events)) && all_ok
if (nrow(w)) all_ok <- PASS(abs(w$pct_events) < 5,
                            sprintf("www.wowhead.com nearly unchanged (%.1f%%)", w$pct_events)) && all_ok

# =============================================================================
hr("V4: DiD ON CORRECTED DATA -- HEADLINE NUMBERS")
# =============================================================================
ec <- fread("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
ec2 <- ec[in_experiment == "true" & !experiment_id %in% BAD_USERS,
          .(experiment_id, wave_id, cookie_treatment_idx)]
ec2[wave_id == 3, wave_id := 2L]

d <- merge(new, ec2, by = "experiment_id")
# visit_count from the old panel (time pipeline unaffected by cookie bugs)
d <- merge(d, old[, .(experiment_id, website, date, visit_count)],
           by = c("experiment_id", "website", "date"), all.x = TRUE)
d[, anchor := fifelse(wave_id == 1L, as.Date("2025-07-26"), as.Date("2025-08-09"))]
d[, tau := as.integer(date - anchor)]
d <- d[tau >= TAU_MIN & tau <= TAU_MAX]
d[, `:=`(treated = as.integer(cookie_treatment_idx == 1),
         post = as.integer(tau >= 0))]
d[, post_treated := post * treated]
d[, dow := factor(weekdays(date))]

dv <- d[!is.na(visit_count) & visit_count > 0]
dv[, cpv_corrected := cookie_events_3rd_p / visit_count]
pr("DiD sample (visit_count > 0): %s cells, %s users",
   comma(nrow(dv)), comma(uniqueN(dv$experiment_id)))
pr("cells lacking visit_count (excluded from CPV, kept for UC): %s (%.1f%%)",
   comma(d[is.na(visit_count) | visit_count <= 0, .N]),
   100 * d[is.na(visit_count) | visit_count <= 0, .N] / nrow(d))

run <- function(data, fml_lhs, label) {
  m <- feols(as.formula(paste0(fml_lhs,
                               " ~ post_treated | experiment_id + website + dow")),
             data = data, cluster = ~experiment_id, notes = FALSE)
  ci <- confint(m, "post_treated")
  pr("  %-28s coef %+.4f  se %.4f  CI [%+.3f, %+.3f]  N %s",
     label, coef(m)["post_treated"], se(m)["post_treated"],
     ci[1,1], ci[1,2], comma(nobs(m)))
}
cat("\nCorrected-data DiD (registrable classification):\n")
run(dv, "log(1 + cpv_corrected)",            "CPV corrected (events/visit)")
run(dv, "log(1 + unique_cookies_3rd_p)",   "UC (unique 3p cookies)")
run(dv, "log(1 + unique_trackers_3rd_p)",     "Trackers (unique 3p)")
cat("\nFor reference, OLD-measure DiD on the same matched sample:\n")
dv2 <- merge(dv, old[, .(experiment_id, website, date, n_cookies_third_party)],
             by = c("experiment_id", "website", "date"), all.x = TRUE)
dv2 <- dv2[!is.na(n_cookies_third_party)]
dv2[, cpv_old := n_cookies_third_party / visit_count]
run(dv2, "log(1 + cpv_old)",          "CPV_old (buggy classification)")


# =============================================================================
hr("V5: INDEPENDENT END-TO-END RECOMPUTATION (3 sampled cells)")
# =============================================================================
# For 3 randomly sampled (user, website, day) cells, recompute events/unique
# DIRECTLY from cookies_classified_v2.fst (row-level filter + manual count) and
# require exact equality with the panel. Guards against aggregation defects
# that internal invariants cannot catch.
ckl <- read_fst("../data/processed_data/cookies_classified_v2.fst", as.data.table = TRUE)
set.seed(42)
smp <- new[cookie_events_3rd_p > 0][sample(.N, 3)]
v5_ok <- TRUE
for (i in 1:3) {
  cell <- smp[i]
  raw <- ckl[experiment_id == cell$experiment_id &
               website_domain == cell$website & date == cell$date]
  ev  <- raw[is_third_party == TRUE, .N]
  uq  <- raw[is_third_party == TRUE, uniqueN(paste(cookie_domain, cookie_name))]
  ok  <- ev == cell$cookie_events_3rd_p && uq == cell$unique_cookies_3rd_p
  pr("  cell %d: %s | %s | %s  -> manual (ev %d, uq %d) vs panel (ev %d, uq %d)  %s",
     i, cell$experiment_id, cell$website, cell$date,
     ev, uq, cell$cookie_events_3rd_p, cell$unique_cookies_3rd_p,
     ifelse(ok, "MATCH", "MISMATCH"))
  v5_ok <- v5_ok && ok
}
rm(ckl); gc(verbose = FALSE)
all_ok <- PASS(v5_ok, "independent recomputation matches panel exactly (3/3 cells)") && all_ok

# =============================================================================
hr(sprintf("GATE RESULT: %s", ifelse(all_ok, "ALL PASS -- rebuilt panel cleared for downstream use",
                                     "FAILURES -- do NOT use v2 downstream yet")))
# =============================================================================
cat("=== DONE 04 ===\n")