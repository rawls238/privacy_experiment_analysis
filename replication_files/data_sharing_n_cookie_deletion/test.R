# =============================================================================
# test.R  --  Validation suite for the third-party cookie measure fixes
# (replication_files/data_sharing_n_cookie_deletion/test.R)
#
# Context: the current pipeline (parse_tracker.R) classifies a cookie as
# third-party iff the first-party website_domain is NOT a literal substring of
# the cookie domain, and counts third-party cookies as SET-EVENT rows. Audit
# findings: (A) substring matching mislabels a site's own cookies on
# subdomained sites (mail.google.com vs .google.com) -- 53.4% of all
# "third-party" events are same-registrable-domain; (C) event counting is
# inflated ~810x overall (134,457x on mail.google.com) by background polling
# that re-sets the same cookies.
#
# Decision: Fix A (registrable-domain matching, reusing the clean_site()
# approach already used for time data) + keep CPV (event intensity) + ADD
# UC = unique third-party cookies per (user, website, day).
#
# This suite validates the fixes BEFORE the pipeline is changed:
#   T1: clean_site() unit tests (30+ cases)
#   T2: old-vs-new classification confusion matrix + flip samples
#   T3: known mislabeled sites flip back to first-party
#   T4: control sites (no own-subdomain cookies) are unaffected by Fix A
#   T5: polling inflation by site -- is it only email/docs, or everywhere?
#   T6: per-site preview of the new measures (events vs UC, new classification)
#   T7: shard date coverage + three-measure DiD preview if window covered
#
# Read-only on data. Writes NOTHING to output/. Console + /tmp only.
# Run: Rscript replication_files/data_sharing_n_cookie_deletion/test.R \
#        2>&1 | tee /tmp/tp_fix_tests.txt
# =============================================================================

setwd("~/Dropbox/spring2025experiment/code_github")
suppressMessages({ library(data.table); library(fixest) })
setDTthreads(0)

PARSED_DIR <- "../data/processed_data/parsed_trackers/09_16"
TRACKERS   <- "../data/final_extension_data/trackers.csv"
EC_FILE    <- "../data/final_extension_data/experiment_conditions_pilot_july_2024.csv"

pr    <- function(...) cat(sprintf(...), "\n")
comma <- function(x) format(x, big.mark = ",")
hr    <- function(t) cat("\n", strrep("=", 72), "\n", t, "\n", strrep("=", 72), "\n", sep = "")
PASS  <- function(ok, msg) { cat(if (ok) "  PASS  " else "  FAIL  ", msg, "\n", sep = ""); ok }
all_ok <- TRUE

# clean_site() lives in utils/time_usage_helpers.R (single source of truth),
# validated here. jsonlite must load first (time_usage_helpers uses fromJSON).
suppressMessages(library(jsonlite))
source("replication_files/utils/time_usage_helpers.R")

# =============================================================================
hr("T1: clean_site() UNIT TESTS")
# =============================================================================
cases <- rbind(
  # input                        expected
  c("mail.google.com",          "google.com"),
  c("docs.google.com",          "google.com"),
  c(".google.com",              "google.com"),
  c("google.com",               "google.com"),
  c("www.google.com",           "google.com"),
  c("accounts.google.com",      "google.com"),
  c("play.google.com",          "google.com"),
  c(".play.google.com",         "google.com"),
  c("googleusercontent.com",    "googleusercontent.com"),
  c("lh3.googleusercontent.com","googleusercontent.com"),
  c("google-analytics.com",     "google-analytics.com"),
  c("www.google-analytics.com", "google-analytics.com"),
  c(".doubleclick.net",         "doubleclick.net"),
  c("stats.g.doubleclick.net",  "doubleclick.net"),
  c("papers.ssrn.com",          "ssrn.com"),
  c(".ssrn.com",                "ssrn.com"),
  c("online.citi.com",          "citi.com"),
  c("mail.yahoo.com",           "yahoo.com"),
  c("login.yahoo.com",          "yahoo.com"),
  c("bbc.co.uk",                "bbc.co.uk"),
  c("www.bbc.co.uk",            "bbc.co.uk"),
  c("news.bbc.co.uk",           "bbc.co.uk"),
  c("amazon.com.au",            "amazon.com.au"),
  c("www.amazon.com.au",        "amazon.com.au"),
  c("smile.amazon.co.uk",       "amazon.co.uk"),
  c("service.gov.uk",           "service.gov.uk"),
  c("m.facebook.com",           "facebook.com"),
  c("amp.cnn.com",              "cnn.com"),
  c("example.com:8080",         "example.com"),
  c("localhost",                "localhost"),
  c("192.168.1.1",              "192.168.1.1"),
  c("MAIL.GOOGLE.COM",          "google.com"),
  c("  mail.google.com  ",      "google.com"),
  c("chat.google.com",          "google.com"),
  c("google.com.hk",            "google.com.hk")
)
t1_ok <- TRUE
for (i in seq_len(nrow(cases))) {
  got <- clean_site(cases[i, 1])
  ok <- identical(got, cases[i, 2])
  if (!ok) pr("  FAIL  clean_site(%-28s) = %-22s expected %s",
              shQuote(cases[i,1]), got, cases[i,2])
  t1_ok <- t1_ok && ok
}
# NA / empty behavior
t1_ok <- t1_ok && is.na(clean_site(NA_character_)) && is.na(clean_site(""))
all_ok <- PASS(t1_ok, sprintf("clean_site(): %d cases + NA/empty handling", nrow(cases))) && all_ok

# =============================================================================
hr("LOADING DATA (all shards; needed by T2-T7)")
# =============================================================================
tr <- fread(TRACKERS, select = c("id", "domain"))
setnames(tr, c("id", "domain"), c("tracker_id", "website_domain"))

files <- list.files(PARSED_DIR, pattern = "parsed_.*[.]csv$", full.names = TRUE)
pr("Reading %d shards ...", length(files))
ck <- rbindlist(lapply(files, function(f)
  fread(f, select = c("experiment_id", "tracker_id", "tstamp",
                      "source", "domain", "name"),
        showProgress = FALSE)), fill = TRUE)
setnames(ck, "domain", "cookie_domain")
ck <- ck[source != "none" & !is.na(cookie_domain) & cookie_domain != ""]
ck[, source := NULL]
ck <- merge(ck, tr, by = "tracker_id", all.x = TRUE)[!is.na(website_domain)]
ck[, date := as.Date(as.POSIXct(tstamp, origin = "1970-01-01"))]
ck[, tstamp := NULL]
pr("cookie rows: %s | date range: %s to %s",
   comma(nrow(ck)), min(ck$date), max(ck$date))

# registrable domains via unique lookup (fast)
ud  <- unique(c(ck$cookie_domain, ck$website_domain))
lut <- data.table(dom = ud, reg = clean_site(ud)); setkey(lut, dom)
ck[, creg := lut[.(cookie_domain), reg]]
ck[, wreg := lut[.(website_domain), reg]]

# old rule (substring) and new rule (registrable)
if (requireNamespace("stringi", quietly = TRUE)) {
  ck[, third_old := !stringi::stri_detect_fixed(cookie_domain, website_domain)]
} else {
  ck[, third_old := !mapply(function(w, c) grepl(w, c, fixed = TRUE),
                            website_domain, cookie_domain)]
}
ck[is.na(third_old), third_old := TRUE]
ck[, third_new := creg != wreg]
ck[, ck_id := .GRP, by = .(cookie_domain, name)]

# =============================================================================
hr("T2: OLD vs NEW CLASSIFICATION -- CONFUSION MATRIX + FLIP SAMPLES")
# =============================================================================
cm <- ck[, .N, by = .(third_old, third_new)][order(-N)]
print(cm)
n_flip_to_first <- ck[third_old == TRUE  & third_new == FALSE, .N]
n_flip_to_third <- ck[third_old == FALSE & third_new == TRUE,  .N]
pr("\nFlips old-third -> new-FIRST (expected large, mislabeled own cookies): %s (%.1f%%)",
   comma(n_flip_to_first), 100 * n_flip_to_first / nrow(ck))
pr("Flips old-first -> new-THIRD (expected ~zero; substring is the looser rule): %s (%.4f%%)",
   comma(n_flip_to_third), 100 * n_flip_to_third / nrow(ck))

cat("\nSample of 30 flips to FIRST-party (should look like own-subdomain cookies):\n")
print(unique(ck[third_old == TRUE & third_new == FALSE,
                .(website_domain, cookie_domain, name)])[1:30])
if (n_flip_to_third > 0) {
  cat("\nSample of flips to THIRD-party (INSPECT -- these were substring false-firsts):\n")
  print(unique(ck[third_old == FALSE & third_new == TRUE,
                  .(website_domain, cookie_domain, name)])[1:30])
}
all_ok <- PASS(n_flip_to_third / nrow(ck) < 0.005,
               "reverse flips (first->third) below 0.5% of rows") && all_ok

# ---- T2b: ALL reverse flips, grouped ----------------------------------------
cat("\nT2b: ALL reverse-flip (first->third) groups, by (website, cookie domain):\n")
rev_groups <- ck[third_old == FALSE & third_new == TRUE,
                 .(rows = .N, example_cookie = name[1]),
                 by = .(website_domain, cookie_domain)][order(-rows)]
print(rev_groups)
pr("Reverse-flip groups: %d (inspect each -- all should be genuine third parties)",
   nrow(rev_groups))

# =============================================================================
hr("T3: KNOWN MISLABELED SITES FLIP BACK TO FIRST-PARTY")
# =============================================================================
t3 <- ck[wreg %in% c("citi.com","paypal.com","bankofamerica.com","ssrn.com",
                     "google.com","yahoo.com","ebay.com") & creg == wreg,
         .(still_third_new = sum(third_new), was_third_old = sum(third_old)),
         by = wreg]
print(t3)
all_ok <- PASS(all(t3$still_third_new == 0),
               "same-registrable cookies are never third-party under the new rule") && all_ok

# =============================================================================
hr("T4: CONTROL SITES (0% own-subdomain issue) UNAFFECTED BY FIX A")
# =============================================================================
ctrl <- ck[website_domain %in% c("app.prolific.com","www.wowhead.com",
                                 "genius.com","www.jigsawplanet.com")]
t4 <- ctrl[, .(old_third = sum(third_old), new_third = sum(third_new),
               pct_change = round(100 * (sum(third_new) - sum(third_old)) /
                                    pmax(sum(third_old), 1), 2)),
           by = website_domain]
print(t4)
all_ok <- PASS(all(abs(t4$pct_change) < 5),
               "control sites change < 5% under Fix A") && all_ok

# =============================================================================
hr("T8: NAMED-SITE ADJUDICATION -- read the actual cookies, per site")
# =============================================================================
adjudicate <- function(site) {
  d <- ck[website_domain == site]
  if (nrow(d) == 0) { pr("  (no rows for %s)", site); return(invisible()) }
  cat(sprintf("\n--- %s ---\n", site))
  cat("THIRD-party under new rule, top 15 cookie domains:\n")
  print(d[third_new == TRUE, .(rows = .N, uc = uniqueN(ck_id),
                               example = name[1]), by = cookie_domain][order(-rows)][1:15])
  cat("FIRST-party under new rule, top 10 cookie domains:\n")
  print(d[third_new == FALSE, .(rows = .N, uc = uniqueN(ck_id),
                                example = name[1]), by = cookie_domain][order(-rows)][1:10])
}
for (st in c("mail.google.com", "mail.yahoo.com", "www.nytimes.com",
             "papers.ssrn.com", "online.citi.com", "www.wowhead.com")) adjudicate(st)

# hard checks on the google case
g3 <- ck[website_domain == "mail.google.com" & third_new == TRUE &
           grepl("google\\.com$", cookie_domain), .N]
all_ok <- PASS(g3 == 0,
               "mail.google.com: no *.google.com cookie classified third-party (new rule)") && all_ok
g1 <- ck[website_domain == "mail.google.com" & third_new == FALSE &
           name %in% c("SID","HSID","SIDCC"), .N]
all_ok <- PASS(g1 > 0,
               "mail.google.com: SID/HSID/SIDCC present and classified first-party") && all_ok

# =============================================================================
hr("T9: CROSS-SITE COOKIE-NAME TRACKING -- known cookies, expected verdicts")
# =============================================================================
# _ga (Google Analytics): third-party everywhere except google-analytics itself.
ga <- ck[name == "_ga",
         .(rows = .N, n_first = sum(!third_new)), by = .(wreg, creg)]
ga_bad <- ga[creg != wreg & n_first > 0]
cat("\n_ga rows misclassified first-party on foreign sites (expect empty):\n")
print(ga_bad)
all_ok <- PASS(nrow(ga_bad) == 0, "_ga is third-party on every foreign site") && all_ok

# IDE / test_cookie (doubleclick): same expectation.
dc <- ck[name %in% c("IDE","test_cookie") & creg == "doubleclick.net" & !third_new, .N]
all_ok <- PASS(dc == 0, "doubleclick IDE/test_cookie never first-party") && all_ok

# __cf_bm (Cloudflare bot mgmt, set on the FIRST party domain): first-party when
# cookie domain matches the site.
cf <- ck[name == "__cf_bm" & creg == wreg & third_new, .N]
all_ok <- PASS(cf == 0, "__cf_bm on own domain never third-party") && all_ok
cat("\n__cf_bm verdict split (should be first-party when creg==wreg):\n")
print(ck[name == "__cf_bm", .N, by = .(same_reg = creg == wreg, third_new)])

# OptanonConsent (OneTrust, set on first-party domain):
cat("\nOptanonConsent verdict split:\n")
print(ck[name == "OptanonConsent", .N, by = .(same_reg = creg == wreg, third_new)])

# =============================================================================
hr("T10: BOUNDARY HUNTING -- ccTLD twins, CDNs, weird domains")
# =============================================================================
cat("\nA) google.com.hk-style ccTLD twins judged third-party (same entity,\n")
cat("   different registrable -- correct per PSL; check the volume is small):\n")
cc <- ck[third_new == TRUE &
           ((grepl("^google\\.com\\.", creg) & wreg == "google.com") |
              (grepl("^google\\.com\\.", wreg) & creg == "google.com") |
              (grepl("^amazon\\.", creg) & grepl("^amazon\\.", wreg) & creg != wreg)),
         .(rows = .N), by = .(wreg, creg)][order(-rows)]
print(cc)
pr("ccTLD-twin third-party rows total: %s (of %s third-party rows -> %.4f%%)",
   comma(sum(cc$rows)), comma(ck[third_new == TRUE, .N]),
   100 * sum(cc$rows) / ck[third_new == TRUE, .N])

cat("\nB) CDN / cloud-infra cookie domains and their verdicts:\n")
cdn <- ck[grepl("amazonaws|cloudfront|akamai|fastly|cloudflare", cookie_domain),
          .(rows = .N), by = .(creg, third_new)][order(-rows)][1:15]
print(cdn)

cat("\nC) Longest / oddest cookie domains (clean_site sanity):\n")
odd <- unique(ck[, .(cookie_domain, creg)])[order(-nchar(cookie_domain))][1:20]
print(odd)

cat("\nD) Domains where clean_site returned NA or equals the raw input\n")
cat("   despite having dots (potential parse oddities):\n")
odd2 <- unique(ck[is.na(creg) | (creg == cookie_domain & grepl("\\..*\\.", cookie_domain)),
                  .(cookie_domain, creg)])[1:20]
print(odd2)

# =============================================================================
hr("T11: CONSERVATION + INVARIANTS on a (user, website, day) mini panel")
# =============================================================================
mini <- ck[, .(events_third = sum(third_new), events_first = sum(!third_new),
               events_total = .N, uc_third = uniqueN(ck_id[third_new])),
           by = .(experiment_id, website_domain, date)]
all_ok <- PASS(mini[, all(events_third + events_first == events_total)],
               "conservation: first + third == total in every cell") && all_ok
all_ok <- PASS(mini[, all(uc_third <= pmax(events_third, 1))],
               "invariant: UC <= events in every cell") && all_ok
pr("mini panel cells: %s", comma(nrow(mini)))

# =============================================================================
hr("T5: POLLING INFLATION -- ONLY EMAIL/DOCS, OR EVERYWHERE?")
# =============================================================================
# Inflation = third-party set-events per unique third-party cookie, per site,
# under the NEW classification (so Fix A doesn't confound the answer).
site_infl <- ck[third_new == TRUE,
                .(events = .N, uc = uniqueN(ck_id)), by = website_domain][
                  events >= 10000][, infl := round(events / uc)][order(-infl)]

# crude site-type buckets to answer "is it only email/google-docs?"
site_infl[, type := fifelse(grepl("^mail[.]|mail[.].*[.]com|outlook|proton", website_domain), "email",
                            fifelse(grepl("google[.]com$", website_domain), "google-app",
                                    fifelse(grepl("youtube|hulu|paramount|netflix|twitch", website_domain), "video",
                                            fifelse(grepl("nytimes|cnn|foxnews|apnews|npr|dailymail|washingtonpost", website_domain), "news",
                                                    fifelse(grepl("amazon|ebay|etsy|walmart|target", website_domain), "shopping",
                                                            "other")))))]
cat("\nTop 25 sites by inflation (events per unique third-party cookie):\n")
print(site_infl[1:25, .(website_domain, type, events, uc, infl)])
cat("\nInflation distribution by site type (median / p90 / max):\n")
print(site_infl[, .(n_sites = .N,
                    med_infl = round(median(infl)),
                    p90_infl = round(quantile(infl, .9)),
                    max_infl = max(infl)), by = type][order(-med_infl)])
cat("\nShare of sites with inflation > 1000x, by type:\n")
print(site_infl[, .(pct_over_1000x = round(100 * mean(infl > 1000), 1),
                    n_sites = .N), by = type][order(-pct_over_1000x)])
pr("\n-> If 'other'/news/shopping also show large shares over 1000x, polling")
pr("   inflation is systemic, not an email/docs quirk.")

# =============================================================================
hr("T6: NEW-MEASURE PREVIEW -- events vs UC per site (new classification)")
# =============================================================================
prev <- ck[third_new == TRUE,
           .(tp_events = .N, tp_uc = uniqueN(ck_id)),
           by = website_domain][order(-tp_events)][1:20]
print(prev)

# =============================================================================
hr("T7: DATE COVERAGE + THREE-MEASURE DiD PREVIEW (if window covered)")
# =============================================================================
dr <- range(ck$date)
pr("All-shard date range: %s to %s", dr[1], dr[2])
NEED_MIN <- as.Date("2025-07-19"); NEED_MAX <- as.Date("2025-08-15")
covered <- dr[1] <= NEED_MIN && dr[2] >= NEED_MAX
pr("DiD window needed: %s to %s -> %s", NEED_MIN, NEED_MAX,
   ifelse(covered, "COVERED, running DiD preview", "NOT covered, skipping DiD"))

if (covered) {
  # (user, website, day) panel under the NEW classification
  pan <- ck[third_new == TRUE,
            .(n_events = .N,
              n_uc     = uniqueN(ck_id),
              n_trk    = uniqueN(tracker_id)),
            by = .(experiment_id, website_domain, date)]
  
  ec <- fread(EC_FILE)
  ec2 <- ec[in_experiment == "true",
            .(experiment_id, wave_id, cookie_treatment_idx)]
  ec2[wave_id == 3, wave_id := 2L]
  pan <- merge(pan, ec2, by = "experiment_id")
  pan[, anchor := fifelse(wave_id == 1L, as.Date("2025-07-26"), as.Date("2025-08-09"))]
  pan[, tau := as.integer(date - anchor)]
  pan <- pan[tau >= -7 & tau <= 6]
  pan[, `:=`(treated = as.integer(cookie_treatment_idx == 1),
             post = as.integer(tau >= 0))]
  pan[, post_treated := post * treated]
  pan[, dow := factor(weekdays(date))]
  
  run <- function(y) {
    m <- feols(as.formula(paste0("log(1+", y, ") ~ post_treated | experiment_id + website_domain + dow")),
               data = pan, cluster = ~experiment_id, notes = FALSE)
    pr("  %-10s  coef = %+.4f  (se %.4f)  N = %s",
       y, coef(m)["post_treated"], se(m)["post_treated"], comma(nobs(m)))
  }
  cat("\nDiD on the NEW classification, three measures (no visit denominator here;\n")
  cat("visit_count lives in the time pipeline and is merged later):\n")
  run("n_events")   # event count (what CPV's numerator will be, post Fix A)
  run("n_uc")       # unique third-party cookies (the new UC measure)
  run("n_trk")      # unique third-party trackers
}

# =============================================================================
hr(sprintf("SUITE RESULT: %s", ifelse(all_ok, "ALL PASS", "FAILURES -- inspect above")))
# =============================================================================
cat("\n=== DONE (nothing written to output/) ===\n")