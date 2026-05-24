# Replication Files

Code that produces the figures and tables in the paper (`writeup_v2.tex`). Non-structural results only.

All scripts assume the working directory is `code_github/`. Data lives in `../data/` (sibling of `code_github/`).

Each script's header comment is the authoritative paper mapping. This README mirrors those headers — if the two disagree, the script header wins.

**Goal**:
1. Each script should replicate the figures/tables in the draft and save them to `output/`. The paper consumes these via `\input{}` and `\includegraphics{./output/...}`.
2. Each inline-referenced scalar should be saved via the `savetexvalue` package as a `\newcommand` macro to `output/values/`, and the paper should `\input{}` those values.

## Repo layout

- `replication_files/` — R scripts grouped by analysis area
- `output/{figures,tables,values}/` — script outputs (committed to git for Overleaf sync)
- `code/` — stage 1 source (kept for now; superseded by `replication_files/`)

The Overleaf project `privacy_experiment_analysis` is checked out separately at `~/Dropbox/spring2025experiment/overleaf_writeup/`. It mirrors `code_github/output/` and holds `writeup_v3.tex` (the new draft being built mechanically from v2 with output paths rewired).

---

## Style conventions

`etable()` arguments:

- `digits = 3`
- `signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1)`
- `replace = TRUE` (default appends, duplicating on re-run)
- `depvar = FALSE` whenever `dict` already maps the dependent variable or `headers` already labels each column. Keep default only for single-column tables.

**Rule 5 — FE row labels via `dict`:**
```r
dict = c("experiment_id" = "Participant FE",
         "block_by_wave" = "Block FE",
         "as.factor(weeks_since_intervention)" = "Weeks FE",
         "website_aggregated_high_level"       = "Website FE")
```

**Rule 6 — output target by content type:**
- Regression tables → `fixest::etable()` → `output/tables/*.tex`
- Summary-stats tables (non-regression) → `xtable::xtable(..., file = ...)` → `output/tables/*.tex`
- Inline-referenced scalars → `savetexvalue::save_tex_value()` → `output/values/*.tex` (`\newcommand` macros). Not for whole tables.

**Rule 7 — `save_tex_value()` numeric discipline:** `savetexvalue` defaults to `scales::number()`, which inserts thousands commas and a trailing `.00` on every numeric input. For integer counts pass `as.integer(x)` or `as.character(x)` so the output is bare digits (`935`, `1441`), not `935.00` / `1,441.00`. For decimal scalars set `accuracy = 0.01` explicitly (or `accuracy = 0.1` for one decimal). The `savetexvalue` file is append-only — wipe it (`file.remove()`) at the top of the script to avoid duplicate `\newcommand` entries on re-run. One scalar bundle per logical group, not per script. Use `cat()`, not `print()`, for any console verification output inside savetexvalue-producing scripts.

All figures go to `output/figures/` as `.pdf`. Plot styling is in `utils/plot_rules.R`.

### Survey weighting

Scripts dispatch four specs via `WEIGHT_SPEC`: `unweighted` (paper) + `weight_census` / `weight_pew` / `weight_combined` (robustness). Suffix `_{spec}` on output filenames; unweighted has no suffix. Weights from `data/Survey/individual_level_weights.csv`. `join_weights()` must filter `sample == "extension"` before joining (stage 1 was missing this; row counts ~doubled in weighted regressions).

Default is `WEIGHT_SPEC <- "unweighted"` so only paper artifacts are produced. Set to `"all"` to regenerate all four for robustness review.

Scripts running all four when `WEIGHT_SPEC == "all"`:
- `survey_analysis/other_survey_regressions.R`
- `survey_analysis/top_sites_beliefs_analysis.R`
- `information_acquisition_results/privacy_seeking_analysis.R`
- `time_use_analysis/time_usage_treatment_effects_SG.R`

---

## Stage 2 status

Stage 2 = paths relative to `code_github/`, outputs to `output/`, paper-label header, style conventions, dead code removed.

| Script | Stage 2 |
|---|---|
| `conjoint/plot_violin.R` | done |
| `privacy_descriptives/privacy_char_summary_stats_3.R` | done |
| `survey_analysis/beliefs_analysis_overall.R` | done |
| `survey_analysis/beliefs_vs_conjoint.R` | done |
| `survey_analysis/other_survey_regressions.R` | done |
| `survey_analysis/top_sites_beliefs_analysis.R` | done |
| `information_acquisition_results/privacy_seeking_analysis.R` | done |
| `survey_analysis/mturk_freeform_analysis.R` | done |
| `survey_analysis/selection_into_study.R` | done |
| `time_use_analysis/time_usage_treatment_effects_SG.R` | done |
| `time_use_analysis/analysis_assortment_did.R` | done |
| `time_use_analysis/baseline_descriptive_scalars.R` | done |

---

## Paper artifacts produced

### `conjoint/plot_violin.R`
- Fig 6 [fig:privacy_combined]: `individual_heterogeneity_dollars_with_website_extension.pdf`, `top2_privacy_attributes_extension.pdf`
- Fig C.4 [fig:extension_survey_conjoint_sample]: `individual_heterogeneity_experiment_vs_survey.pdf`
- Fig C.5 [fig:privacy_combined_full]: `individual_heterogeneity_dollars_with_website_full.pdf`, `top2_privacy_attributes_full.pdf`

### `privacy_descriptives/privacy_char_summary_stats_3.R`
- Fig 4 [fig:scores_by_category]: `domain_scores_box_plot.pdf`

### `survey_analysis/beliefs_analysis_overall.R`
- Fig 5 [fig:privacy_info_beliefs]: `beliefs_vs_truth.pdf`
- Section 5.1 rank correlation scalars cited in Fig 5 caption / surrounding prose: `output/values/rank_correlation_beliefs_values.tex` (9 macros: `\tauBy<Category>{N,Median,P}` for `Collect`, `Use`, `Control`). Within-participant Kendall τ-b within each attribute category; extension sample only. Wilcoxon signed-rank p-values stored as `<0.001` strings since all three are below R's default `eps = 2e-16` floor.

### `survey_analysis/beliefs_vs_conjoint.R`
- Table C.4 [tab:conjoint_vs_beliefs]: `misspec_pref_regression.tex`
- Table C.5 [tab:baseline_demo_heterogeneity]: `misspec_pref_demographics.tex`

### `survey_analysis/other_survey_regressions.R`
- Table C.3 [tab:experimenter_demand]: `experiment_modified_behavior.tex`
- Table C.8 [tab:data_sharing_purpose]: `output/values/data_sharing_purpose_values.tex` (22 macros: `\dataPurpose<X>N` / `\dataPurpose<X>Pct`). The table structure stays in the paper; only the 11 counts + 11 percents are replaced by macros.
- Table C.9 [tab:treatment_effect_data_sharing]: `data_sharing_treatment_effects.tex`

### `survey_analysis/top_sites_beliefs_analysis.R`
- Table [tab:belief_correctness_top_sites]: `top_sites_information.tex`
- Fig 8 [fig:beliefs_by_search]: `beliefs_by_info_acq.pdf`
- Table C [tab:belief_correctness_top_sitesrandom_info]: `top_sites_information_rand_info.tex`
- Fig C [fig:cdf_belief_correctness_top_sites]: `cumulative_belief_distance.pdf`
- Fig C [fig:top_sites_heterogeneity_exposure]: `heterogeneous_beliefs_site_level_by_exposure_tercile.pdf`

### `information_acquisition_results/privacy_seeking_analysis.R`
- Fig 7 [fig:info_acq_treatment_effects]: `info_acq_treatment_effects_total_sites.pdf`, `ever_visited_privacy_policy.pdf`
- Table C [tab:cookie_banner_interactions_treatment]: `cookie_banner_interactions_treatment.tex` (verified byte-for-byte)

### `survey_analysis/mturk_freeform_analysis.R`
- Fig D.1 [fig:freeform_change_responses]: `freeform_change_responses.pdf`

### `survey_analysis/selection_into_study.R`
- Fig C.1 [fig:dem_balance]: `balance/age.pdf`, `balance/education.pdf`, `balance/gender.pdf`, `balance/income.pdf`
- Fig C.2 [fig:selection_on_survey_payments]: `agg_selection_lots.pdf`
- Fig C.3 [fig:pew_comparison]: `pew_comparison.pdf`

### `time_use_analysis/time_usage_treatment_effects_SG.R` + `analysis_assortment_did.R`
The driver sources the helper. Together they produce:
- Fig 9 [fig:intensive]: `preregistered_spec_i_baseline.pdf`, `preregistered_triple_interaction_baseline.pdf`
- Fig 10 [fig:extensive]: `assortment_concentration.pdf`, `assortment_privacy.pdf`

Driver also rebuilds `../data/processed_data/joined_time_data.csv` when `CONSTRUCT_FROM_SCRATCH = TRUE`. The xtable for `tab:top_websites` was removed; see `baseline_descriptive_scalars.R` below.

**Open questions — needs author review.** The current code reproduces the stage 1 unweighted regression coefficients byte-for-byte, but generated figures differ from the figures currently in `writeup_v2.tex`. Both stage 1 `_SG.R` and stage 2 produce the same numbers; neither matches the paper's current figures. The script that originally produced the Overleaf figures has not been located. Substantive analysis choices worth confirming:

1. `time_spent > 30` filter drops ~40% of session-level rows (433,664 of 1,086,075). Threshold not documented.
2. `lower_pct = 0.05` / `upper_pct = 0.95` winsorization on the baseline panel — function default is no winsorization. Rationale not documented.
3. Baseline-visited panel uses `values_to_include = c(0)` (pre-period totals as one bucket), not week-by-week presence in weeks -2 and -1 as the paper caption describes.
4. `log_time = log1p(total_time_spent / 60)` — unit is log(1+minutes).

A `join_weights()` bug in stage 1 (~195% match rate) was fixed in stage 2; unweighted output unaffected.

### `time_use_analysis/baseline_descriptive_scalars.R`
- Section 4 inline daily-hours scalars: `output/values/baseline_time_use_values.tex` (3 macros: `\baselineDailyHoursPrivacy`, `\baselineDailyHoursLeisure`, `\baselineDailyHoursAll`). Computed via Approach A (per-user mean over the user's active days, then mean across users), matching paper prose 1.2 / 2.4 / 3.1.
- Table C.2 [tab:top_websites]: `output/values/top_websites_values.tex` (60 macros: `\topWeb<Rank><Field>`, Rank in `One..Fifteen`, Field in `Domain/ActiveHours/NUsers/AllHours`). The LaTeX tabular itself is hand-written in `writeup_v3.tex`.

Data source is `get_clean_time_data()` directly (not the cached `joined_time_data.csv`) so `SURVEY_WEBSITES` rows are still in scope for the "including survey platforms" scalar — the cache drops them upstream in `time_usage_treatment_effects_SG.R` before write.

Filter rules: drop `BAD_USERS`, baseline weeks `{-2, -1}`, no `time_spent > 30` filter. Per-scalar scope: `\baselineDailyHoursPrivacy` drops `SURVEY_WEBSITES` and keeps `privacy_exist`; `\baselineDailyHoursLeisure` drops `SURVEY_WEBSITES`; `\baselineDailyHoursAll` keeps `SURVEY_WEBSITES`. Top-table additionally requires `privacy_exist` and `n_users >= 50`.

The twitter -> x slug rename in `utils/time_usage_helpers.R` now runs inside `get_clean_time_data()` *before* `map_privacy_data()` (was previously after, causing all twitter rows to receive `privacy_exist = FALSE`; fixed). All baseline privacy-info-restricted scalars in this repository reflect post-fix data and run ~0.3 hours/user higher than the corresponding values in older paper drafts.

---

## Paper figures referenced but NOT in stage 2 output

These figures appear in `writeup_v2.tex` but no stage 2 script produces them. For `writeup_v3.tex` the convention is to comment out the entire `\begin{figure}...\end{figure}` block (or `\begin{table}...\end{table}`) and add a `% TODO:` line so compile passes and the gap is visible to authors.

### Cookie deletion / CPV — Appendix G
Generated by `code/cookie_deletion/...`; not yet refactored into `replication_files/`. 12 figures:
- `cpv_did_by_site.pdf`, `cpv_did_by_site_log_status.pdf`
- `cpv_heterogeneity_by_user_quintile.pdf`, `cpv_heterogeneity_by_user_quintile_log_status.pdf`
- `cpv_heterogeneity_by_website_category.pdf`, `cpv_heterogeneity_by_website_category_log_status.pdf`
- `cpv_over_time_c1.pdf`, `cpv_over_time_c2.pdf`
- `cpv_trend_by_treatment_wave1.pdf`, `cpv_trend_by_treatment_wave2.pdf`
- `time_did_by_site.pdf`, `time_did_by_site_log_status.pdf`
- `time_heterogeneity_by_user_quintile.pdf`, `time_heterogeneity_by_user_quintile_log_status.pdf`
- `time_heterogeneity_by_website_category.pdf`, `time_heterogeneity_by_website_category_log_status.pdf`
- `time_over_time_c1.pdf`, `time_over_time_c2.pdf`

### Structural model
Out of stage 2 scope (paper is "non-structural results only"). 3 figures from Sam's structural model pipeline:
- `fig_valuation_distribution.pdf`
- `fig_waterfall_gdpr.pdf`
- `fig_waterfall_info_provision.pdf`

### Experimental materials / screenshots
Hand-made design documentation (treatment dialogs, study flow, consent pages). Not produced by any R script. 8 files:
- `conjoint-example-paper.png`
- `info_dialog.png`, `info_full_dialog.png`, `info_full_attributes_dialog.png`
- `saliency_dialog.png`
- `screener_survey_consent.png`
- `outline_may2025.png`
- `subject_flow.pdf`

---

## Utils (sourced helpers)

- `utils/plot_rules.R` — ggplot theme, color scales, sizing constants.
- `utils/values.R` — date constants, `BAD_USERS`, `SURVEY_WEBSITES`, `AUX_DATA_DIR`.
- `utils/time_usage_helpers.R` — browser data cleaning, domain aggregation/classification, privacy scoring, conjoint utility loading. Path constants at top (`DATA_DIR`, `EXT_DATA_DIR`, `SURVEY_DIR`, `CONJOINT_DIR`, `AUX_DATA_DIR`). 17 in-use functions. The twitter -> x slug rename runs inside `get_clean_time_data()` before `map_privacy_data()` (fixed; see baseline scalars note above).
- `utils/info_acq_helpers.R` — privacy policy visit detection, info acquisition aggregation.

---

## TODO

The savetexvalue refactor is organized by tier so that twitter -> x affected outputs and the largest table rebuild come first.

### Tier 1 — descriptive scalars (one new script or one savetexvalue patch per row)

- **Group B**: NEW `survey_analysis/survey_descriptive_scalars.R` for Section 4 demographics (writeup_v3.tex line 394: 39.4 / 59.9% / 58.6%) and WTA (line 413: $20 / $19 / $25 / $52 / $40 / 89%). Source: `survey_merged_final.csv` + `experiment_conditions_pilot_july_2024.csv`.

- **Group C**: NEW `balance_table/exp_balance_table.R` producing the full `tab:exp_balance_table` (writeup_v3.tex lines 1207-1247: 17 rows × 5 cols) plus the Section 4 line 403 sample-count/attrition scalars (501/532, 500/533, 509/532, 480/472/474, p=0.37, p=0.96). Pool across waves; match paper columns `Control Mean | Saliency Mean | Information Mean | p(S vs C) | p(I vs C)`; include the "No Income Disclosure" row that the current Python `compare_means_and_generate_latex()` in `balance_table/assign_groups*.py` omits. Affected by the twitter -> x bug fix; numbers will differ from current paper text. Largest single deliverable in the remaining refactor.

- **Group D**: Add `save_tex_value()` calls to `privacy_descriptives/privacy_char_summary_stats_3.R` for line 298 (57 indicators, 17.8 minutes mean policy read time, 383.8 cumulative human hours).

- **Group E**: Add `save_tex_value()` calls to `survey_analysis/top_sites_beliefs_analysis.R` for line 148 (7.1pp) and line 499 (5.0pp / belief error 50 / 6.7-7.2pp).

- **Group F**: Add `save_tex_value()` calls to `survey_analysis/beliefs_vs_conjoint.R` for line 480 (0.016 / 0.04 SD / 0.01 SD).

- **Group G**: Add `save_tex_value()` calls to `information_acquisition_results/privacy_seeking_analysis.R` for line 521 (0.8 / 0.1-0.25 privacy policy visits) and the 17× ratio + observed control/saliency search rates (2.3% / 1.6% / 15.9% / 15.9%) referenced in the conclusion and model fit appendix.

### Tier 2 — regression-derived inline scalars (possibly twitter -> x affected)

- **Group I**: Add `save_tex_value()` calls to `time_use_analysis/analysis_assortment_did.R` for line 643 (p = 0.063 HHI, p = 0.065 top-2 share).

- **Group J**: Locate or build a popup DiD script producing `tab:popup_did` and the line 653 / 655 inline scalars ($\hat\beta_\text{pooled} = 0.015$, $t = 1.87$, 73%, $t = 1.29$; $\hat\beta = -0.152$, $\text{SE} = 0.101$). The generating script for `tab:popup_did` has not been located.

### Tier 3 — other paper sections, source script TBD

- **Group M**: Tracker count macros for line 1036 ("nearly 30,000 unique third party domains", "nearly 1.5 million unique trackers"). Producing script not located; likely under `code/tracker_analysis/` or `code/3rd_tracker_categorize/`, not yet refactored into `replication_files/`.

- **Group N**: Conjoint partworth inline scalars for line 472 ($6-8/month financial info attributes) and line 567 ($3/month median valuation). Producing script likely `conjoint/plot_violin.R` or a new conjoint scalar script.

### Out of stage 2 scope (handled separately by co-author)

All structural-model scalars: writeup_v3.tex lines 153 / 157 / 159 / 173 (intro summary), 830-839 (parameter table), 847-851 / 862-872 / 878-893 / 908-916 / 944 / 961-965 (interpretation + welfare counterfactuals), 1550-1554 (model fit appendix). Every $76 / 21% / 68% / 17% / $42 / $1.20 / $1.44 / $72 / 30% / $6.21 / $0.67 / $0.21 / $1.22 / σ / α^info / α^search / δ / c_s / ξ / λ / W* etc.

### Never refactor (survey design constants)

These are experimental design choices, not computed scalars: $3 / $4 / $5 survey compensation; $40 WTA cap; $1-$5 conjoint price levels; $0..$40 WTA offer ladder; ($28,$28)..($2,$70) Eckel-Grossman payoffs; 10-15 minutes survey duration; days 15-42 intervention window; 0.25 response scale increments; $\sigma_\tau = 0.75 / 0.5$ conjoint priors.

### Other open items

- `time_use_analysis/analysis_assortment_did.R` — confirm with authors which figures actually went into the published Overleaf version (Open questions above), then accept current output or trace back to the missing script.

- Cookie deletion (Appendix G) — refactor `code/cookie_deletion/...` into `replication_files/cookie_deletion/` so the 12 figures listed above are reproducible.

---

## Paper-side TODO

Switch every table cell and inline scalar from hard-coded to `\input{output/...}` so re-running scripts updates the paper. Tables verified byte-for-byte (3 dp):
- C.3 `tab:experimenter_demand`
- C.4 `tab:conjoint_vs_beliefs`
- C.5 `tab:baseline_demo_heterogeneity`
- C.9 `tab:treatment_effect_data_sharing`
- Section 6 `tab:belief_correctness_top_sites`
- App C `tab:cookie_banner_interactions_treatment`

Inline scalars already migrated to macros (just need `\input{}` + macro reference in `writeup_v3.tex`):
- `\baselineDailyHoursPrivacy` / `Leisure` / `All` (`output/values/baseline_time_use_values.tex`) — Section 4 prose ("1.2 / 2.4 / 3.1 hours") still hardcoded.
- `\topWeb<Rank><Field>` (60 macros, `output/values/top_websites_values.tex`) — `\input{}` already present in `writeup_v3.tex` preamble; tabular body uses macros.
- `\dataPurpose<X>N` / `\dataPurpose<X>Pct` (22 macros, `output/values/data_sharing_purpose_values.tex`).
- `\tauBy<Category>{N,Median,P}` (9 macros, `output/values/rank_correlation_beliefs_values.tex`) — new in Section 5.1 to support Guy's "miscalibrated but not pure noise" framing in the Fig 5 caption / surrounding text. Not yet referenced in `writeup_v3.tex`.

---

## Things removed

Outputs the original codebase generated but the paper doesn't use. Generating code removed to keep replication tight.

- `privacy_seeking_analysis.R` — exploratory policy/info-acq plots; dead `felm` regressions.
- `mturk_freeform_analysis.R` — overwritten pre-computations, redundant library loads.
- `selection_into_study.R` — diagnostic `.tex` files, alt visual specs, Section 4 pipeline duplicated elsewhere.
- `analysis_assortment_did.R` — `run_did_analysis`, `plot_privacy_summary`, `run_robust_did_analysis` deleted entirely, plus alt assortment specs and the unused pre-registered spec (iii) market-competition block. File ~880 → ~370 lines.
- `time_usage_treatment_effects_SG.R` — 9 diagnostic plots (~200 lines), 4 dead panels, dead `extension_inactivity` read, xtable for `tab:top_websites` (replaced by `baseline_descriptive_scalars.R` + savetexvalue). Default `WEIGHT_SPEC <- "unweighted"` (was `"all"`, producing 27 unused PDFs + 15 unused .tex weighted variants).
- `top_sites_beliefs_analysis.R` — alt exposure specs, unused PEW model block, dead helpers.
- `beliefs_vs_conjoint.R` — companion plot replaced by table.
- `beliefs_analysis_overall.R` — table versions of Fig 5, PEW DiD treatment effects table.
- `utils/` — `tracker_utils.R`, `aggregate_urls.R`, `union.R`, `union_time_period.R` (no call sites).
- `utils/time_usage_helpers.R` — 11 internal helpers deleted (no call sites across the repo, verified including `code/cookie_deletion/`):
  - `clean_urls`, `aggregate_time_data_trackers`, `standardize_ad_domains_trackers`, `high_level_aggregate_trackers`
  - `map_domain_trackers`, `map_domain`, `map_domain_improved`
  - `map_privacy_data_old`, `map_privacy_data_trackers`, `map_privacy_data_trackers_2`, `map_privacy_data_trackers_SG`

  File ~2900 → ~1442 lines.
