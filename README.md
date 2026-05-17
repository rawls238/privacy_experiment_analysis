# Replication Files

Code that produces the figures and tables in the paper (`writeup_v2.tex`). Non-structural results only.

All scripts assume the working directory is `code_github/`. Data lives in `../data/` (sibling of `code_github/`).

Each script's header comment is the authoritative paper mapping. This README mirrors those headers — if the two disagree, the script header wins.

**Goal**:
1. Each script should replicate the figures/tables in the draft and save them to `output/`. A new Overleaf project linked to this GitHub will consume those outputs. For now this only covers non-model-based results.
2. Each inline-referenced scalar should be saved via the `savetexvalue` package as a `\newcommand` macro to `output/values/`, and the paper should `\input{}` those values.

---

## Style conventions

`etable()` arguments:

- `digits = 3`
- `signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1)`
- `replace = TRUE` (default is append, which duplicates on re-run)
- `depvar = FALSE` whenever `dict` already maps the dependent variable or `headers` already labels each column. Keep default only for single-column tables.

**Rule 5 — FE row labels via `dict`.** Rename fixed-effect rows to paper-friendly names:
```r
dict = c("experiment_id"     = "Participant FE",
         "block_by_wave"     = "Block FE",
         "as.factor(weeks_since_intervention)" = "Weeks FE",
         "website_aggregated_high_level"       = "Website FE")
```

**Rule 6 — output target by content type:**
- Regression tables → `fixest::etable()` → `output/tables/*.tex`
- Summary-stats tables (non-regression) → `xtable::xtable(..., file = ...)` → `output/tables/*.tex`
- Inline-referenced scalars (e.g. "the average participant spends 1.2 hours …") → `savetexvalue::save_tex_value()` → `output/values/*.tex` as `\newcommand` macros. Do not use for whole tables.

All figures go to `output/figures/` as `.pdf`. Plot styling is in `utils/plot_rules.R`.

### Survey weighting

Survey scripts run four weight specs (`unweighted` + `weight_census` / `weight_pew` / `weight_combined`) via a `WEIGHT_SPEC` dispatcher. Output filenames take `_{weight_spec}` suffix; unweighted has no suffix. Weights come from `data/Survey/individual_level_weights.csv`. `join_weights()` must filter `sample == "extension"` before joining (the stage 1 driver was missing this filter, doubling row counts in weighted regressions — see Open questions).

The unweighted variant is what the paper reports. Scripts running all four:
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

---

## Paper artifacts produced

Source of truth is each script's header comment. This is the index.

### `conjoint/plot_violin.R`
- Fig 6 [fig:privacy_combined]: `individual_heterogeneity_dollars_with_website_extension.pdf`, `top2_privacy_attributes_extension.pdf`
- Fig C.4 [fig:extension_survey_conjoint_sample]: `individual_heterogeneity_experiment_vs_survey.pdf`
- Fig C.5 [fig:privacy_combined_full]: `individual_heterogeneity_dollars_with_website_full.pdf`, `top2_privacy_attributes_full.pdf`

### `privacy_descriptives/privacy_char_summary_stats_3.R`
- Fig 4 [fig:scores_by_category]: `domain_scores_box_plot.pdf`

### `survey_analysis/beliefs_analysis_overall.R`
- Fig 5 [fig:privacy_info_beliefs]: `beliefs_vs_truth.pdf`

### `survey_analysis/beliefs_vs_conjoint.R`
- Table C.4 [tab:conjoint_vs_beliefs]: `misspec_pref_regression.tex`
- Table C.5 [tab:baseline_demo_heterogeneity]: `misspec_pref_demographics.tex`

### `survey_analysis/other_survey_regressions.R`
- Table C.3 [tab:experimenter_demand]: `experiment_modified_behavior.tex`
- Table C.8 [tab:data_sharing_purpose]: `output/values/data_sharing_purpose_values.tex` (22 `\dataPurpose<X>N` / `\dataPurpose<X>Pct` macros)
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

### `time_use_analysis/time_usage_treatment_effects_SG.R` + `time_use_analysis/analysis_assortment_did.R`
The driver sources the helper. Together they produce:
- Fig 9 [fig:intensive]: `preregistered_spec_i_baseline.pdf`, `preregistered_triple_interaction_baseline.pdf`
- Fig 10 [fig:extensive]: `assortment_concentration.pdf`, `assortment_privacy.pdf`
- Table C [tab:top_websites]: `top_websites.tex` (xtable)

**Open questions — needs author review.** The current code reproduces the stage 1 unweighted regression coefficients byte-for-byte, but the resulting figures differ from the figures currently in `writeup_v2.tex`. Both the stage 1 `_SG.R` driver and the stage 2 refactor (calculation unchanged) produce the same numbers, and neither matches the paper's current figures. The script that originally produced the Overleaf figures has not been located. Substantive analysis choices worth confirming:

1. `time_spent > 30` filter drops ~40% of session-level rows (433,664 of 1,086,075). Threshold not documented.
2. `lower_pct = 0.05` / `upper_pct = 0.95` winsorization on the baseline panel — function default is no winsorization. Rationale not documented.
3. Baseline-visited panel uses `values_to_include = c(0)` (pre-period totals as one bucket), not week-by-week presence in weeks -2 and -1 as the paper caption describes.
4. `log_time = log1p(total_time_spent / 60)` — unit is log(1+minutes).

A `join_weights()` bug in stage 1 (missing `sample == "extension"` filter, ~195% match rate) was fixed in stage 2; unweighted output unaffected.

---

## Utils (sourced helpers)

- `utils/plot_rules.R` — ggplot theme, color scales, sizing constants.
- `utils/values.R` — date constants, `BAD_USERS`, `SURVEY_WEBSITES`.
- `utils/time_usage_helpers.R` — browser data cleaning, domain aggregation/classification, privacy scoring, conjoint utility loading. 17 in-use functions.
- `utils/info_acq_helpers.R` — privacy policy visit detection, info acquisition aggregation.

---

## TODO

- Table `tab:exp_balance_table` (Appendix C.1) — currently the Python scripts in `balance_table/` (`assign_groups.py`, `assign_groups_wave_1.py`, `assign_groups_wave_two.py`) compute balance during randomization via `compare_means_and_generate_latex()` and `print()` the LaTeX to console; the `.tex` output is never saved. The current paper table appears to be hand-copied from console output. Differences from the paper:
  - Script columns: `N | Mean | Std | N | Mean | Std | ... | Diff(S-C) | Diff(I-C) | p(S vs C) | p(I vs C)`
  - Paper columns: `Control Mean | Saliency Mean | Information Mean | p(S vs C) | p(I vs C)` (no N, Std, or Diff)
  - Paper has one extra row "No Income Disclosure" — not in the script's `cols_to_compare`

  Rewrite as a single script that pools across waves, matches paper columns, includes "No Income Disclosure", saves to `output/tables/exp_balance_table.tex`.

- Table `tab:popup_did` (Section 7.3) — generating script not located.

- `time_use_analysis/analysis_assortment_did.R` — confirm with authors which figures actually went into the published version of Overleaf (see Open questions above), then either accept current output or trace back to the missing script.

- Inline scalars in Section 4 ("1.2 hours / 2.4 hours / 3.1 hours") — currently hard-coded. Compute in `time_usage_treatment_effects_SG.R` and expose via `savetexvalue` once the paper-side `\input{}` migration starts.

---

## Paper-side TODO

Switch every table cell and inline scalar in the paper from hard-coded to `\input{output/.../...}`. Re-running scripts then updates the paper automatically.

Tables verified byte-for-byte against the paper (3 dp):
- C.3 `tab:experimenter_demand`
- C.4 `tab:conjoint_vs_beliefs`
- C.5 `tab:baseline_demo_heterogeneity`
- C.9 `tab:treatment_effect_data_sharing`
- Section 6 `tab:belief_correctness_top_sites`
- App C `tab:cookie_banner_interactions_treatment`

---

## Things removed

Outputs the original codebase generated but the paper doesn't use. The generating code has been removed in stage 2 to keep replication tight. Briefly:

- `privacy_seeking_analysis.R` — exploratory privacy-policy / info-acq plots not in paper; dead `felm` regressions.
- `mturk_freeform_analysis.R` — overwritten pre-computations, redundant library loads.
- `selection_into_study.R` — diagnostic stats `.tex` files, alt visual specs, Section 4 pipeline duplicated elsewhere.
- `analysis_assortment_did.R` — three entire functions deleted (`run_did_analysis`, `plot_privacy_summary`, `run_robust_did_analysis`), plus alt assortment specs and the unused pre-registered spec (iii) market-competition block. File ~880 → ~370 lines.
- `time_usage_treatment_effects_SG.R` — 9 diagnostic plots (~200 lines), 4 dead panels, dead `extension_inactivity` read, redundant function calls.
- `top_sites_beliefs_analysis.R` — alt exposure specs, unused PEW model block, dead helpers.
- `beliefs_vs_conjoint.R` — companion plot replaced by table.
- `beliefs_analysis_overall.R` — table versions of Fig 5, PEW DiD treatment effects table.
- `utils/` — `tracker_utils.R`, `aggregate_urls.R`, `union.R`, `union_time_period.R` deleted (no call sites).
- `utils/time_usage_helpers.R` — 11 internal helper functions deleted (no call sites across the repo, verified across all `.R` files including `code/cookie_deletion/`):
  - `clean_urls`, `aggregate_time_data_trackers`, `standardize_ad_domains_trackers`, `high_level_aggregate_trackers`
  - `map_domain_trackers`, `map_domain`, `map_domain_improved`
  - `map_privacy_data_old`, `map_privacy_data_trackers`, `map_privacy_data_trackers_2`, `map_privacy_data_trackers_SG`

  File ~2900 → ~1442 lines.