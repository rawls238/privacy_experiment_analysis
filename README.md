# Replication Files

Code that produces the figures and tables in the paper. Non-structural results only (Sections 5–7 and Appendices C, D).

## Section 5 — Privacy Practices, Beliefs & Stated Preferences

- `domain_scores_box_plot.pdf` (Fig 4) → `privacy_descriptives/privacy_char_summary_stats_3.R`
- `beliefs_vs_truth.pdf` (Fig 5) → `survey_analysis/beliefs_analysis_overall.R`
- `individual_heterogeneity_dollars_with_website_extension.pdf` (Fig 6a) → `conjoint/plot_violin.R`
- `top2_privacy_attributes_extension.pdf` (Fig 6b) → `conjoint/plot_violin.R`

## Section 6 — Information Acquisition & Beliefs

- Table 1 `tab:belief_correctness_top_sites` → `survey_analysis/top_sites_beliefs_analysis.R` (output: `top_sites_information.tex`)
- `info_acq_treatment_effects_total_sites.pdf` (Fig 7a) → `information_acquisition_results/privacy_seeking_analysis.R`
- `ever_visited_privacy_policy.pdf` (Fig 7b) → `information_acquisition_results/privacy_seeking_analysis.R`
- `beliefs_by_info_acq.pdf` (Fig 8) → `survey_analysis/top_sites_beliefs_analysis.R`

## Section 7 — Effect on Website Choices

- `preregistered_spec_i_baseline.png` (Fig 9a) → `time_use_analysis/analysis_assortment_did.R`
- `preregistered_triple_interaction_baseline.png` (Fig 9b) → `time_use_analysis/analysis_assortment_did.R`
- `assortment_concentration.png` (Fig 10a) → `time_use_analysis/analysis_assortment_did.R`
- `assortment_privacy.png` (Fig 10b) → `time_use_analysis/analysis_assortment_did.R`

## Appendix C — Additional Figures and Tables

- `age.pdf`, `education.pdf`, `gender.pdf`, `income.pdf` (Fig C.1) → `survey_analysis/selection_into_study.R`
- `agg_selection_lots.pdf` (Fig C.4) → `survey_analysis/selection_into_study.R`
- `pew_comparison.pdf` (Fig C.5) → `survey_analysis/selection_into_study.R`
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

## Cannot Find / Not R-Generated

These artifacts are in the paper but we couldn't locate the generating script — likely Python or hand-written. Need to confirm with Sam/Guy.

- Table `tab:popup_did` (Section 7.3) — closest candidate doesn't match spec
- Table `tab:exp_balance_table` (Appendix C.1) — likely Python (the "nan" string is a Python artifact)
- Table `tab:top_websites` (Appendix C.1) — descriptive, no script found
- Table `tab:data_sharing_purpose` (Appendix C.3) — `other_survey_regressions.R` prints to console but doesn't write `.tex`

## Removed

The original codebase generated additional outputs not used in the paper. We removed the generating code to keep the replication scope tight.

### From `information_acquisition_results/privacy_seeking_analysis.R`
- `privacy_policy_treatment_effects_visits.pdf` — exploratory, never made it to paper
- `privacy_policy_treatment_effects_ever_visit.pdf` — exploratory
- `info_acq_treatment_effects_unique_sites.pdf` — alt version of `total_sites`
- `privacy_settings.tex` — privacy settings DiD; the matching paper table (`tab:privacy_settings_visits`) is in `\begin{comment}` block, not actually in paper

### From `survey_analysis/selection_into_study.R`
- `pew_benchmarks.tex`, `demo_summary.tex`, `survey_pew_comparison.tex`, `wta_statistics.tex` — diagnostic stats, not in paper
- `wta_comparison.pdf` — not in paper
- `disagg_selection_lots.pdf` — alt version of `agg_selection_lots`

### From `time_use_analysis/analysis_assortment_did.R`
- `assortment_entry_exit_privacy.png`, `assortment_differential_distribution.png`, `assortment_differential_bar.png`, `assortment_concentration_changes.png` — alt assortment specs not in paper
- `preregistered_spec_iii.png` — pre-registered spec not used
- `privacy_summary_by_model.png` — exploratory
- Various `.txt` summary dumps (`did_analysis_summary*`, `preregistered_specs_summary*`) — console diagnostics

### From `survey_analysis/top_sites_beliefs_analysis.R`
- `info_exposure_terciles.pdf`, `info_exposure_median.pdf` — alt exposure specs
- `heterogeneous_beliefs_site_level_by_exposure_median.pdf` — alt of `_tercile` version

### From `survey_analysis/beliefs_vs_conjoint.R`
- `misspec_vs_pref_intensity_signed.pdf` — visual companion, replaced by table in paper

### From `utils/`
- `tracker_utils.R`, `aggregate_urls.R`, `union.R`, `union_time_period.R` — dead code, no production script sourced them

## Open Questions

- `Conjoint-Finalized/` vs `Conjoint-01092026/` — `beliefs_vs_conjoint.R` reads from one folder, `time_usage_helpers.R` from the other. Need to confirm whether these folders contain identical data.
- `time_usage_helpers.R` line 524 reads from `auxiliary_data/...` while everything else reads from `data/...`. Inconsistency to fix.