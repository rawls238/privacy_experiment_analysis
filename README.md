# Replication Files

Code that produces the figures and tables in the paper (`writeup_v2.tex`). Non-structural results only.

All scripts assume the working directory is `code_github/`. Data lives in `../data/` (sibling of `code_github/`).

Authoritative paper mapping for each script is the header comment at the top of that script. This README mirrors those headers — if the two disagree, the script header wins.

---

## Style conventions

All `etable()` calls in this repo must use the following arguments to keep output consistent with the paper:

- `digits = 3` — round all coefficients and SEs to 3 decimal places (etable falls back to scientific notation for values too small to display at this precision, e.g. `9.29e-5`).
- `signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1)` — paper convention.
- `replace = TRUE` — overwrite the existing `.tex` instead of appending (default is append, which produces duplicates on re-run).
- `depvar = FALSE` whenever `dict` already maps the dependent variable to a friendly name or `headers` already labels each column. The default behavior repeats the raw R column name as a sub-header (e.g. "belief_distance / correct_exclude50 / correct_strict"), which is redundant when the column already has a paper-style header. Keep `depvar` on (default) only for simple single-column tables where the dependent variable name is informative.

All `.tex` outputs are written to `output/tables/`; all figures are written to `output/figures/`. Plot styling is centralized in `utils/plot_rules.R`.

### Survey weighting

Survey-based scripts run four weight specifications via an internal dispatcher at the top of the script:

- `unweighted` (default; no suffix on output filenames)
- `weight_census` → suffix `_weight_census`
- `weight_pew` → suffix `_weight_pew`
- `weight_combined` → suffix `_weight_combined`

Weights come from `data/Survey/individual_level_weights.csv` and are loaded by `experiment_id`. To run all four in one shot, leave `WEIGHT_SPEC <- "all"` at the top of the script. The unweighted variant is what the paper reports; the three weighted variants are robustness checks.

Scripts that currently run all four specs:
- `survey_analysis/other_survey_regressions.R`
- `survey_analysis/top_sites_beliefs_analysis.R`
- `information_acquisition_results/privacy_seeking_analysis.R`

---

## Stage 2 status

Stage 2 = (a) paths relative to `code_github/`, (b) outputs to `output/{tables,figures}/`, (c) header comment listing every paper label + output file, (d) style conventions above, (e) dead code removed.

| Script | Stage 2 |
|---|---|
| `conjoint/plot_violin.R` | done |
| `privacy_descriptives/privacy_char_summary_stats_3.R` | done |
| `survey_analysis/beliefs_analysis_overall.R` | done |
| `survey_analysis/beliefs_vs_conjoint.R` | done |
| `survey_analysis/other_survey_regressions.R` | done |
| `survey_analysis/top_sites_beliefs_analysis.R` | done |
| `information_acquisition_results/privacy_seeking_analysis.R` | partial (weight dispatcher only; no header / paper labels) |
| `survey_analysis/mturk_freeform_analysis.R` | not yet (old `setwd` style) |
| `survey_analysis/selection_into_study.R` | not yet (old `setwd` style) |
| `time_use_analysis/time_usage_treatment_effects_SG.R` | not yet (old `setwd`, no paper-label header) |
| `time_use_analysis/analysis_assortment_did.R` | not yet (sourced by the SG driver above; partial header) |

---

## Paper artifacts produced

The mapping below is copied from each script's header comment. The script is the source of truth.

### `conjoint/plot_violin.R`
- Main paper [fig:privacy_combined, "Privacy Valuations and Most Valued Information"]
  - Fig 6(a) [fig:privacy_info_partworth]: `individual_heterogeneity_dollars_with_website_extension.pdf`
  - Fig 6(b) [fig:privacy_info_top_2]: `top2_privacy_attributes_extension.pdf`
- Appendix [fig:privacy_combined_full]
  - Fig C.5(a) [fig:privacy_info_partworth_full]: `individual_heterogeneity_dollars_with_website_full.pdf`
  - Fig C.5(b) [fig:privacy_info_top_2_full]: `top2_privacy_attributes_full.pdf`
- Appendix [fig:extension_survey_conjoint_sample]
  - Fig C.4: `individual_heterogeneity_experiment_vs_survey.pdf`

### `privacy_descriptives/privacy_char_summary_stats_3.R`
- Main paper [fig:scores_by_category]
  - Fig 4: `domain_scores_box_plot.pdf`

### `survey_analysis/beliefs_analysis_overall.R`
- Main paper [Section 5.1, fig:privacy_info_beliefs]
  - Fig 5: `beliefs_vs_truth.pdf`

### `survey_analysis/beliefs_vs_conjoint.R`
- Main paper
  - Table C.4 [tab:conjoint_vs_beliefs]: `misspec_pref_regression.tex`
  - Table C.5 [tab:baseline_demo_heterogeneity]: `misspec_pref_demographics.tex`

### `survey_analysis/other_survey_regressions.R`
- Main paper
  - Table C.3 [tab:experimenter_demand]: `experiment_modified_behavior[_suffix].tex`
  - Table C.8 [tab:data_sharing_purpose]: `output/values/data_sharing_purpose_values.tex` (22 macros: 11 `\dataPurpose<X>N` counts + 11 `\dataPurpose<X>Pct` percents)
  - Table C.9 [tab:treatment_effect_data_sharing]: `data_sharing_treatment_effects[_suffix].tex`

`_suffix` is empty for unweighted and `_{weight_spec}` otherwise.

### `survey_analysis/top_sites_beliefs_analysis.R`
- Main paper [Section 6, tab:belief_correctness_top_sites]: `top_sites_information.tex`
- Main paper [Section 6, Fig 8, fig:beliefs_by_search]: `beliefs_by_info_acq.pdf`
- App C [tab:belief_correctness_top_sitesrandom_info]: `top_sites_information_rand_info.tex`
- App C [fig:cdf_belief_correctness_top_sites]: `cumulative_belief_distance.pdf`
- App C [fig:top_sites_heterogeneity_exposure]: `heterogeneous_beliefs_site_level_by_exposure_tercile.pdf`

All five outputs also produced in 3 weighted variants.

### Scripts whose paper artifacts are not yet documented in their headers
These scripts produce output that ships in the paper, but the paper-label mapping is not in the script header yet, so it is not transcribed here. Open the script (or add a stage-2 header) to find the mapping authoritatively.

- `information_acquisition_results/privacy_seeking_analysis.R`
- `survey_analysis/mturk_freeform_analysis.R`
- `survey_analysis/selection_into_study.R`
- `time_use_analysis/time_usage_treatment_effects_SG.R` (drives Section 7 figures via `analysis_assortment_did.R`; also produces Appendix C `tab:top_websites` as xtable to console — needs to save to `.tex`)
- `time_use_analysis/analysis_assortment_did.R` (sourced by the SG driver, not run standalone)

---

## Utils (sourced helpers)

- `utils/plot_rules.R` — ggplot theme, color scales (treatment / privacy category / benchmark / binary / ordinal exposure), sizing constants. Style guide at top of file.
- `utils/values.R` — date constants (wave start/end, treatment dates, cookie treatment dates), `BAD_USERS`, `SURVEY_WEBSITES`.
- `utils/time_usage_helpers.R` — browser data cleaning, domain aggregation/classification, privacy info matching, privacy score computation, conjoint utility loading. 17 in-use functions, ~1442 lines.
- `utils/info_acq_helpers.R` — privacy policy visit detection, info acquisition event aggregation. Has path constants (`DATA_DIR`, `SURVEY_DIR`, `EXT_DATA_DIR`, `PROC_DIR`) at top.

---

## TODO — Replicate from scratch

These tables appear in the paper but the corresponding scripts either don't exist, only print to console, or have different column structure than the paper version.

### Table `tab:exp_balance_table` (Appendix C.1)

Source scripts copied to `balance_table/`:
- `balance_table/assign_groups.py` — base randomization + balance check
- `balance_table/assign_groups_wave_1.py` — wave 1 specific
- `balance_table/assign_groups_wave_two.py` — wave 2 specific

These scripts compute balance during randomization via `compare_means_and_generate_latex()` and `print()` the LaTeX to console — **the `.tex` output is never saved to disk**. The current paper table appears to be hand-copied from console output.

Differences from paper version:
- Script outputs columns: `N | Mean | Std | N | Mean | Std | ... | Diff(S-C) | Diff(I-C) | p(S vs C) | p(I vs C)`
- Paper table has columns: `Control Mean | Saliency Mean | Information Mean | p(S vs C) | p(I vs C)` (no N, Std, or Diff)
- Paper has one extra row: "No Income Disclosure" — not in script's `cols_to_compare` list

Replication plan: rewrite as a standalone script that pools across waves, matches paper column structure, includes "No Income Disclosure", and saves to `output/tables/exp_balance_table.tex`.

### Other TODOs

- Table `tab:popup_did` (Section 7.3) — generating script not located.
- Table `tab:top_websites` (Appendix C) — `time_usage_treatment_effects_SG.R` currently `print(xtable(...))` to console; needs to save `.tex`.

---

## Paper-side TODO

The paper currently hard-codes table cell values rather than `\input{output/tables/...}`. Stage 2 terminal goal is to switch every table to `\input{}` mode so re-running scripts updates the paper.

Tables verified byte-for-byte against the paper (3 dp):
- C.3 `tab:experimenter_demand`
- C.4 `tab:conjoint_vs_beliefs`
- C.5 `tab:baseline_demo_heterogeneity`
- C.9 `tab:treatment_effect_data_sharing`
- Section 6 `tab:belief_correctness_top_sites`

---

## Things removed (generated but not used in the paper)

The original codebase generated additional outputs not used in the paper. We removed the generating code to keep the replication scope tight.

### `information_acquisition_results/privacy_seeking_analysis.R`
- `privacy_policy_treatment_effects_visits.pdf` — exploratory, never made it to paper
- `privacy_policy_treatment_effects_ever_visit.pdf` — exploratory
- `info_acq_treatment_effects_unique_sites.pdf` — alt version of total_sites
- `privacy_settings.tex` — privacy settings DiD; the matching paper table (`tab:privacy_settings_visits`) is in `\begin{comment}` block, not actually in paper

### `survey_analysis/selection_into_study.R`
- `pew_benchmarks.tex`, `demo_summary.tex`, `survey_pew_comparison.tex`, `wta_statistics.tex` — diagnostic stats, not in paper
- `wta_comparison.pdf` — not in paper
- `disagg_selection_lots.pdf` — alt version of `agg_selection_lots`

### `time_use_analysis/analysis_assortment_did.R`
- `assortment_entry_exit_privacy.png`, `assortment_differential_distribution.png`, `assortment_differential_bar.png`, `assortment_concentration_changes.png` — alt assortment specs not in paper
- `preregistered_spec_iii.png` — pre-registered spec not used
- `privacy_summary_by_model.png` — exploratory
- Various `.txt` summary dumps (`did_analysis_summary*`, `preregistered_specs_summary*`) — console diagnostics

### `survey_analysis/top_sites_beliefs_analysis.R`
- `info_exposure_terciles.pdf`, `info_exposure_median.pdf` — alt exposure specs
- `heterogeneous_beliefs_site_level_by_exposure_median.pdf` — alt of `_tercile` version
- PEW model block (regression with `pewq1-4 + PolicyEffectiveness` covariates) — computed but never written to disk
- Helper functions `convert_to_numeric()` and `get_internal_privacy_field()` — dead code, zero call sites

### `survey_analysis/beliefs_vs_conjoint.R`
- `misspec_vs_pref_intensity_signed.pdf` — visual companion, replaced by table in paper

### `survey_analysis/beliefs_analysis_overall.R`
- `agg_belief_correctness.tex` — aggregate-by-category belief vs truth, table version of Fig 5
- `agg_indiv_field_belief_correctness.tex` — per-attribute belief vs truth, table version of Fig 5
- `pew_all_effects.tex` — PEW DiD treatment effects regression table

### `utils/` standalone files
- `tracker_utils.R`, `aggregate_urls.R`, `union.R`, `union_time_period.R` — dead code, no production script sourced them

### `utils/time_usage_helpers.R` — internal dead functions
11 helper functions removed (zero call sites across all `.R` scripts in the repo, verified including `code/cookie_deletion/`):
- `clean_urls`, `aggregate_time_data_trackers`, `standardize_ad_domains_trackers`, `high_level_aggregate_trackers`
- `map_domain_trackers`, `map_domain`, `map_domain_improved`
- `map_privacy_data_old`, `map_privacy_data_trackers`, `map_privacy_data_trackers_2`, `map_privacy_data_trackers_SG`

File reduced from ~2900 to ~1442 lines. Path constants (`DATA_DIR`, `EXT_DATA_DIR`, `SURVEY_DIR`, `CONJOINT_DIR`) added at the top so all data reads resolve correctly when the calling script `setwd()`s to `code_github/`.