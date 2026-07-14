# =============================================================================
# 02_classify_cookies.R
# (replication_files/data_construction/02_classify_cookies.R)
#
# Fix A: classify every parsed cookie row as first- vs third-party by
# REGISTRABLE DOMAIN (eTLD+1 via utils clean_site()), replacing the old
# substring rule in source_scripts/parse_tracker.R. Validated by
# data_sharing_n_cookie_deletion/test.R (all site audits pass, zero false
# flips; the only old-first -> new-third flips are genuine third parties).
#
# INPUT (all three parse batches; they are relay-style pulls -- 06_22 covers
# through 6/22, 07_03 covers 6/22-7/3, 09_16 starts 7/4 -- with possible
# boundary overlap, removed by row-level dedup below):
#   ../data/processed_data/parsed_trackers/{06_22,07_03,09_16}/user_trackers_*_parsed_*.csv
#   ../data/final_extension_data/trackers.csv     (tracker_id -> website host)
# OUTPUT (new file only; nothing existing is overwritten):
#   ../data/processed_data/cookies_classified_v2.fst
#     one row per cookie set-event, columns:
#       experiment_id, tracker_id, date, website_domain (host),
#       website_reg, cookie_domain, cookie_reg, cookie_name, is_third_party
#
# NA RULE (explicit, conservative): rows whose cookie_domain or website_domain
# fails registrable-domain extraction (clean_site() -> NA) are classified
# THIRD-party and KEPT, so no row is silently dropped and third-party counts
# are never understated. The NA share is printed below; it is a handful of
# malformed domains (see test.R T10-D).
# =============================================================================

setwd("~/Dropbox/spring2025experiment/code_github")
suppressMessages({ library(jsonlite); library(data.table); library(fst) })
setDTthreads(0)
source("replication_files/utils/time_usage_helpers.R")   # clean_site()

PARSED_DIRS <- file.path("../data/processed_data/parsed_trackers",
                         c("06_22", "07_03", "09_16"))
TRACKERS   <- "../data/final_extension_data/trackers.csv"
OUT_FST    <- "../data/processed_data/cookies_classified_v2.fst"

pr    <- function(...) cat(sprintf(...), "\n")
comma <- function(x) format(x, big.mark = ",")

stopifnot(!file.exists(OUT_FST) ||
            { pr("NOTE: %s exists and will be overwritten (it is the product of this script).", OUT_FST); TRUE })

# --- 1. tracker metadata: tracker_id -> first-party website host -------------
tr <- fread(TRACKERS, select = c("id", "domain"))
setnames(tr, c("id", "domain"), c("tracker_id", "website_domain"))
pr("trackers.csv: %s rows", comma(nrow(tr)))

# --- 2. parsed cookie rows: all three batches, then cross-batch dedup ---------
batches <- lapply(PARSED_DIRS, function(dir) {
  files <- list.files(dir, pattern = "parsed_.*[.]csv$", full.names = TRUE)
  b <- rbindlist(lapply(files, function(f)
    fread(f, select = c("experiment_id", "tracker_id", "tstamp",
                        "source", "domain", "name"),
          showProgress = FALSE)), fill = TRUE)
  pr("batch %-6s: %3d shards, %s rows", basename(dir), length(files), comma(nrow(b)))
  b
})
ck <- rbindlist(batches); rm(batches); gc(verbose = FALSE)
setnames(ck, c("domain", "name"), c("cookie_domain", "cookie_name"))

n_read <- nrow(ck)
ck <- ck[source != "none" & !is.na(cookie_domain) & cookie_domain != ""]
pr("cookie rows: %s read, %s kept after dropping cookieless requests (source == 'none')",
   comma(n_read), comma(nrow(ck)))
ck[, source := NULL]

# cross-batch dedup: relay pulls can overlap at their boundaries; the same
# underlying set-event appears identically in two batches. Row identity =
# (experiment_id, tracker_id, tstamp, cookie_domain, cookie_name).
n_before <- nrow(ck)
ck <- unique(ck, by = c("experiment_id", "tracker_id", "tstamp",
                        "cookie_domain", "cookie_name"))
pr("cross-batch dedup: removed %s duplicate rows (%.2f%%), %s remain",
   comma(n_before - nrow(ck)), 100 * (n_before - nrow(ck)) / n_before, comma(nrow(ck)))

# --- 3. attach first-party host; date -----------------------------------------
ck <- merge(ck, tr, by = "tracker_id", all.x = TRUE)
n_no_tracker <- ck[is.na(website_domain), .N]
pr("rows without tracker match (dropped): %s (%.4f%%)",
   comma(n_no_tracker), 100 * n_no_tracker / nrow(ck))
ck <- ck[!is.na(website_domain)]
ck[, date := as.Date(as.POSIXct(tstamp, origin = "1970-01-01"))]
ck[, tstamp := NULL]

# --- 4. registrable domains (unique-set lookup, fast) --------------------------
ud  <- unique(c(ck$cookie_domain, ck$website_domain))
lut <- data.table(dom = ud, reg = clean_site(ud)); setkey(lut, dom)
ck[, cookie_reg  := lut[.(cookie_domain), reg]]
ck[, website_reg := lut[.(website_domain), reg]]

# --- 5. classification + explicit NA rule --------------------------------------
ck[, is_third_party := cookie_reg != website_reg]
n_na <- ck[is.na(is_third_party), .N]
pr("rows with unresolvable registrable domain (classified third-party): %s (%.5f%%)",
   comma(n_na), 100 * n_na / nrow(ck))
ck[is.na(is_third_party), is_third_party := TRUE]

pr("classification: third-party %s (%.1f%%) | first-party %s (%.1f%%)",
   comma(ck[is_third_party == TRUE, .N]),  100 * mean(ck$is_third_party),
   comma(ck[is_third_party == FALSE, .N]), 100 * mean(!ck$is_third_party))

# --- 6. write -------------------------------------------------------------------
setcolorder(ck, c("experiment_id", "tracker_id", "date", "website_domain",
                  "website_reg", "cookie_domain", "cookie_reg", "cookie_name",
                  "is_third_party"))
write_fst(ck, OUT_FST, compress = 50)
pr("Wrote %s (%s rows) | date range %s to %s",
   OUT_FST, comma(nrow(ck)), min(ck$date), max(ck$date))
cat("=== DONE 02 ===\n")