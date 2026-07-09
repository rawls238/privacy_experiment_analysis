# Data Construction Pipeline — Replication from Raw Data

**Status: INVESTIGATION COMPLETE, IMPLEMENTATION NOT STARTED.**
This README is the handoff document. Everything discovered about the legacy
pipeline is recorded here; the migration plan at the bottom is approved and
ready to implement.

**Goal** (Guy, Slack): "eventually we anyways want to be able to replicate
from the raw data." Rebuild the pipeline raw tracker data →
`panel_merged_CLEAN.fst` as clean, documented scripts inside
`replication_files/`, with every intermediate validated against the legacy
files.

---

## 1. Pipeline architecture (as discovered)

Two branches join, then aggregate, then merge with time data:

```
BRANCH A: cookie rows                       BRANCH B: tracker metadata
data/processed_data/parsed_trackers/09_16/  trackers.csv (raw, server export)
  user_trackers_09_16_parsed_{N}.csv          |
  (300+ batch files, ~130-210MB each,         |  clean_n_match_domain.r
   one row per cookie or cookieless           |  (domain cleaning + aggregation
   request; already parsed from the           |   + high-level mapping)
   raw ~85GB pull of Sep 16)                  v
       |                                    tracker_cleaned_n_domain.csv
       |                                    (id -> visited domain,
       |                                     third_party_domain, timestamps)
       |                                      |
       +----------- JOIN on tracker_id -------+
       |
       v
  is_third_party classification (see §3)
       |
       v
  aggregate to (experiment_id, website, date):
    n_cookies_third_party = sum(is_third_party & source != "none")
    n_trackers_third_party = uniqueN(tracker_id[...])
       |
       v
  data/tracker_panel/regression_panel.csv        <- VALIDATION ANCHOR 1
       |
       |  merge time_data_2.csv (visit_count, time_spent per user-website-day)
       |  + cleaning (code/tracker_analysis/regressions.R, PART A)
       v
  data/tracker_panel/panel_merged_CLEAN.fst      <- VALIDATION ANCHOR 2
       |
       v
  replication_files/data_sharing_n_cookie_deletion/{cookie_deletion,data_sharing}.R
  (CPV = n_cookies_third_party / visit_count, log(1+x))
```

**Critical structural fact**: the parsed batches do NOT contain the visited
website — their `domain` column is the COOKIE's own domain (e.g.
`.google.com`). The visited site and the third-party domain come from the
Branch B metadata join on `tracker_id`. Classification cannot run without
this join.

## 2. Source scripts (copied into `source_scripts/` here)

| File | Origin | What it does | What to keep |
|---|---|---|---|
| `parse_tracker.R` (1,828 lines) | `code/tracker_analysis/` | Parses raw request CSVs into cookie rows (batching, ~500k rows per 100k requests); defines `is_third_party`; contains the aggregation function producing the panel counts | Parsing functions (PART 1), classification + aggregation (~lines 1640-1800). DROP: embedded regression experiments, demo/test blocks (more than half the file) |
| `clean_n_match_domain.r` | Yen cluster (`/yen/projects/faculty/sggold-privacy-policy/trakers`), copy obtained | Cleans/aggregates domains in trackers.csv; maps to high-level domains | Nearly all of it |
| `regressions.R` | `code/tracker_analysis/` | PART A: reads aggregated panel + time_data_2, cleans, merges, writes `panel_merged_CLEAN.fst` (line ~629) | PART A only. DROP: PART B regressions (superseded by replication_files versions) |

**Note**: the historical production run of the aggregation was INTERACTIVE —
parse_tracker.R only prints instructions (`fwrite(results$data, ...)` inside
a cat() at line ~2207); no script performs the actual production
aggregation + write. `regression_panel_PILOT.fst` (the input name in
regressions.R) does not exist anywhere on disk; only
`data/tracker_panel/regression_panel.csv` does. Our scripts will formalize
this missing link and validate against the csv.

## 3. Key definitions (verified in code, confirmed by Guy)

- **Third-party cookie**: cookie whose `domain` does not contain the visited
  site's domain (fixed substring match; mapply block at parse_tracker.R
  ~line 1647). Guy confirmed on Slack: "we want third-party = domain
  mismatch". NOT easylist — easylist (advertising+analytics subset) governs
  only which cookies the DELETION intervention removes. So the DV counts
  more cookies than deletion can remove; caveat already in Guy's
  baseline-figure prose.
- **`source` column values**: `set` (set-cookie header), `request` (cookie
  sent with request), `none` (cookieless request row). Counting condition
  `source != "none"` keeps actual cookies.
- **Cookies vs trackers**: cookie count = number of cookie ROWS;
  tracker count = number of UNIQUE tracker_ids. CPV uses cookies only;
  the tracker columns are never used in the paper.
- Worked example (use in docs/replies): visiting nytimes.com, doubleclick
  sets 3 cookies + google-analytics sets 2 → n_cookies_third_party = 5,
  n_trackers_third_party = 2.

## 4. Validation anchors (must match exactly)

| Anchor | Value |
|---|---|
| `panel_merged_CLEAN.fst` rows | 809,501 |
| unique experiment_id | 1,596 |
| date range | 2025-04-28 … 2025-08-24 |
| sum(n_cookies_third_party) | 199,344,318 |
| downstream regression check | pooled deletion DiD on log CPV = −0.3288 |

Validation design (per Mark): every new intermediate written with `_new`
suffix next to the legacy file; each script ends with a validation block —
row counts, numeric column sums (tol 1e-6), 1,000-row keyed sample
comparison; print VALIDATION PASSED or stop with a diff.

## 5. Data inventory (where things are, Mark's machine)

- Parsed batches: `data/processed_data/parsed_trackers/09_16/` (300+ files)
- Cleaned tracker metadata, TWO generations (which fed the panel is
  UNRESOLVED — script 01's validation decides):
  - `data/trackers_cleaned_n_categorized/` (Jan 12 / Mar 11)
  - `data/cleaned_trackers/` (Mar 10-11)
- Aggregated panel: `data/tracker_panel/regression_panel.csv`
- Final panel: `data/tracker_panel/panel_merged_CLEAN.fst`
- Time data: `data/processed_data/time_data_enrichment/time_data_2.csv`
  (+ enriched variants nearby)
- Raw Sep-16 pull (~85GB) and `trackers.csv`: NOT on this machine —
  presumed on Yen (`/yen/projects/faculty/sggold-privacy-policy/trakers`).
  Branch B therefore starts from the existing cleaned outputs unless/until
  trackers.csv is retrieved.

## 6. Approved migration plan (implement next)

Target files in this directory:

1. `01_clean_tracker_metadata.R` — migrate `clean_n_match_domain.r`;
   output `tracker_cleaned_n_domain_new.csv`; validate vs BOTH legacy
   generations and record which one matches (resolves §5 ambiguity).
   Blocked if trackers.csv unavailable locally → then document + validate
   the two legacy outputs against each other instead.
2. `02_build_cookie_panel.R` — batch-loop the parsed files; join metadata
   on tracker_id; classify is_third_party; aggregate to
   (experiment_id, website, date); output `regression_panel_new.csv`;
   validate vs `regression_panel.csv`. Heaviest step (~40-60GB pass,
   hours of runtime).
3. `03_build_panel_merged.R` — migrate regressions.R PART A; merge
   time_data_2; output `panel_merged_CLEAN_new.fst`; validate vs anchors
   in §4.
4. Keep this README updated with per-step status.

Migration principles (house rules): logic character-identical, strip dead
code, relative paths, full header comments (paper mapping style), `_new`
outputs, validation block in every script, test-before-production with
Mark's approval at each step.

## 7. Related context (not this folder's scope, but pending as of handoff)

- Undivided-DV robustness (Guy's request): tested, main −0.481 / has-log
  −0.376 / no-log −0.536 (all stronger than CPV versions); production
  table for Appendix E.1 pending.
- Vertical category plots (Guy: categories on x-axis), category-set
  unification (use shared big_cats, 17 categories), three macro fixes
  (optOutPP 6.9 / optOutPctIncrease 20.3 new macros; \noLogPct and
  \cookieTimePct swaps in prose), "time" wording units — all tested or
  specced, production pass pending.
