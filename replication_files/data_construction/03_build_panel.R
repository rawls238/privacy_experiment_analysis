# =============================================================================
# 03_build_panel.R
# (replication_files/data_construction/03_build_panel.R)
#
# Aggregate the classified cookie-level file (02 output) to a
# (experiment_id, website, date) panel with the CORRECTED measures:
#
#   cookie_events_3rd_p   : third-party cookie SET-EVENT count
#                          (same concept as the old n_cookies_third_party,
#                           but under the registrable-domain classification)
#   unique_cookies_3rd_p      : UNIQUE third-party cookies, uniqueN(domain, name)
#                          -- the new exposure measure (UC)
#   unique_trackers_3rd_p        : unique third-party tracker_ids
#   cookie_events_1st_p   : first-party event count (conservation checks)
#   unique_cookies_1st_p      : unique first-party cookies
#
# KEY CHOICE: the panel key keeps the ORIGINAL website host
# (website_domain, e.g. mail.google.com), NOT the registrable domain, so this
# panel merges 1:1 with the existing time/visit pipeline
# (panel_merged_CLEAN.fst uses hosts). The registrable domain is used only for
# CLASSIFICATION inside each row.
#
# INPUT : ../data/processed_data/cookies_classified_v2.fst   (02 output)
# OUTPUT: ../data/processed_data/panel_cookies_v2.fst        (new file)
# =============================================================================

setwd("~/Dropbox/spring2025experiment/code_github")
suppressMessages({ library(data.table); library(fst) })
setDTthreads(0)

IN_FST  <- "../data/processed_data/cookies_classified_v2.fst"
OUT_FST <- "../data/processed_data/panel_cookies_v2.fst"

pr    <- function(...) cat(sprintf(...), "\n")
comma <- function(x) format(x, big.mark = ",")

ck <- read_fst(IN_FST, as.data.table = TRUE)
pr("classified cookie rows: %s", comma(nrow(ck)))

# integer cookie identity for fast uniqueN
ck[, ck_id := .GRP, by = .(cookie_domain, cookie_name)]

pan <- ck[, .(
  cookie_events_3rd_p = sum(is_third_party),
  unique_cookies_3rd_p    = uniqueN(ck_id[is_third_party]),
  unique_trackers_3rd_p      = uniqueN(tracker_id[is_third_party]),
  cookie_events_1st_p = sum(!is_third_party),
  unique_cookies_1st_p    = uniqueN(ck_id[!is_third_party])
), by = .(experiment_id, website = website_domain, date)]

# --- invariants (fail fast; the NA rule in 02 guarantees no NA here) ----------
stopifnot(pan[, all(cookie_events_3rd_p + cookie_events_1st_p > 0)])
stopifnot(pan[, all(unique_cookies_3rd_p <= pmax(cookie_events_3rd_p, 1))])
stopifnot(pan[, all(unique_cookies_1st_p <= pmax(cookie_events_1st_p, 1))])
stopifnot(!anyNA(pan))

pr("panel cells: %s | users: %s | websites: %s | dates %s to %s",
   comma(nrow(pan)), comma(uniqueN(pan$experiment_id)),
   comma(uniqueN(pan$website)), min(pan$date), max(pan$date))

write_fst(pan, OUT_FST, compress = 50)
pr("Wrote %s", OUT_FST)
cat("=== DONE 03 ===\n")