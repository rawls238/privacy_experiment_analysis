# Replication Files

Code that produces the figures and tables in the paper (`writeup_v2.tex`). Non-structural results only.

---

## Section 5 — Privacy Practices, Beliefs & Stated Preferences

- `domain_scores_box_plot.pdf` (Fig 4) → `privacy_descriptives/privacy_char_summary_stats_3.R`
- `beliefs_vs_truth.pdf` (Fig 5) → `survey_analysis/beliefs_analysis_overall.R`
- `individual_heterogeneity_dollars_with_website_extension.pdf` (Fig 6a) → `conjoint/plot_violin.R`
- `top2_privacy_attributes_extension.pdf` (Fig 6b) → `conjoint/plot_violin.R`

## Section 6 — Information Acquisition & Beliefs

- Table `tab:belief_correctness_top_sites` → `survey_analysis/top_sites_beliefs_analysis.R` (output: `top_sites_information.tex`)
- `info_acq_treatment_effects_total_sites.pdf` (Fig 7a) → `information_acquisition_results/privacy_seeking_analysis.R`
- `ever_visited_privacy_policy.pdf` (Fig 7b) → `information_acquisition_results/privacy_seeking_analysis.R`
- `beliefs_by_info_acq.pdf` (Fig 8) → `survey_analysis/top_sites_beliefs_analysis.R`

## Section 7 — Effect on Website Choices

These figures are produced by `time_use_analysis/analysis_assortment_did.R`, which is **not run standalone** — it is sourced by `time_use_analysis/time_usage_treatment_effects_SG.R` (the driver).

- `preregistered_spec_i_baseline.png` (Fig 9a) → driver: `time_usage_treatment_effects_SG.R` → `analysis_assortment_did.R`
- `preregistered_triple_interaction_baseline.png` (Fig 9b) → driver: `time_usage_treatment_effects_SG.R` → `analysis_assortment_did.R`
- `assortment_concentration.png` (Fig 10a) → driver: `time_usage_treatment_effects_SG.R` → `analysis_assortment_did.R`
- `assortment_privacy.png` (Fig 10b) → driver: `time_usage_treatment_effects_SG.R` → `analysis_assortment_did.R`

## Appendix C — Additional Figures and Tables

- `age.pdf`, `education.pdf`, `gender.pdf`, `income.pdf` (Fig C.1) → `survey_analysis/selection_into_study.R`
- `agg_selection_lots.pdf` (Fig C.4) → `survey_analysis/selection_into_study.R`
- `pew_comparison.pdf` (Fig C.5) → `survey_analysis/selection_into_study.R`
- Table `tab:top_websites` → `time_use_analysis/time_usage_treatment_effects_SG.R` (currently `print(xtable(...))` to console; needs to save `.tex`)
- Table `tab:experimenter_demand` → `survey_analysis/other_survey_regressions.R` (output: `experiment_modified_behavior.tex`)
- `individual_heterogeneity_experiment_vs_survey.pdf` → `conjoint/plot_violin.R`
- `individual_heterogeneity_dollars_with_website_full.pdf` → `conjoint/plot_violin.R`
- `top2_privacy_attributes_full.pdf` → `conjoint/plot_violin.R`
- Table `tab:conjoint_vs_beliefs` → `survey_analysis/beliefs_vs_conjoint.R`
- Table `tab:baseline_demo_heterogeneity` → `survey_analysis/beliefs_vs_conjoint.R`
- `heterogeneous_beliefs_site_level_by_exposure_tercile.pdf` → `survey_analysis/top_sites_beliefs_analysis.R`
- Table `tab:belief_correctness_top_sitesrandom_info` → `survey_analysis/top_sites_beliefs_analysis.R` (output: `top_sites_information_rand_info.tex`)
- `cumulative_belief_distance.pdf` → `survey_analysis/top_sites_beliefs_analysis.R`
- Table `tab:cookie_banner_interactions_treatment` → `information_acquisition_results/privacy_seeking_analysis.R` (output: `cookie_banner_interactions.tex`)
- Table `tab:treatment_effect_data_sharing` → `survey_analysis/other_survey_regressions.R` (output: `data_sharing_treatment_effects.tex`)

## Appendix D — Self-Reported Changes

- `freeform_change_responses.pdf` → `survey_analysis/mturk_freeform_analysis.R`

---

## TODO — Replicate from scratch

These tables appear in the paper but the corresponding scripts either don't exist, only print to console, or have different column structure than the paper version. Plan: rewrite to produce `.tex` output, then verify against current paper values.

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
- Table `tab:data_sharing_purpose` (Appendix C.3) — `survey_analysis/other_survey_regressions.R` runs the regression but only prints to console; needs to add `etable(... file = ...)` to save `.tex`.

---

## Open Questions

- **Experimental logistics scripts**: Guy mentioned these should be in `replication_files/`. The `onboarding_sequence_scripts/` folder (recruiting, treatment assignment, email/onboarding pipeline, monitoring) is the most likely candidate but contains no paper analysis code beyond the balance table. Pending clarification on whether the rest should be included.

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

### `survey_analysis/beliefs_vs_conjoint.R`
- `misspec_vs_pref_intensity_signed.pdf` — visual companion, replaced by table in paper

### `utils/`
- `tracker_utils.R`, `aggregate_urls.R`, `union.R`, `union_time_period.R` — dead code, no production script sourced them