# Replication Files

R code that produces the figures, tables, and inline scalar macros for the
paper (`writeup_v3.tex`). Non-structural results only; the structural-model
artifacts are produced separately by a co-author.

**Working directory**: all scripts assume `code_github/`. Data lives in
`../data/` (a sibling of `code_github/`), with conjoint estimation output in
`../results/`.

**Authority**: each script's header comment is the canonical paper mapping. If
this README and a script header disagree, the header wins.

**Two products per script**:
1. Figures/tables saved to `output/{figures,tables}/`; the paper consumes them
   via `\includegraphics{}` and `\input{}`.
2. Inline-referenced scalars saved via `savetexvalue` as `\newcommand` macros
   to `output/values/`; the paper `\input{}`s the bundle and references each
   number by macro name, so re-running a script updates the paper.

---

## Repo layout

- `replication_files/` — analysis scripts grouped by area
- `replication_files/utils/` — shared helpers (sourced by analysis scripts)
- `output/{figures,tables,values}/` — script outputs (committed for Overleaf sync)
- `code/` — stage 1 source (kept for reference; superseded by `replication_files/`)

The Overleaf project is checked out separately at
`~/Dropbox/spring2025experiment/overleaf_writeup/`. It mirrors
`code_github/output/` and holds `writeup_v3.tex`. Sync is one-way:
`rsync code_github/output/ -> overleaf_writeup/output/`, then commit/push both
repos.

---

## Script -> output -> paper map

Paper locations are given by LaTeX label (stable across edits), not line number.

### `conjoint/plot_violin.R`
- Fig 6 [fig:privacy_combined]: `individual_heterogeneity_dollars_with_website_extension.pdf` (6a), `top2_privacy_attributes_extension.pdf` (6b)
- Fig C.4 [fig:extension_survey_conjoint_sample]: `individual_heterogeneity_experiment_vs_survey.pdf`
- Fig C.5 [fig:privacy_combined_full]: `individual_heterogeneity_dollars_with_website_full.pdf`, `top2_privacy_attributes_full.pdf`
- WTP scalars: `output/values/conjoint_wtp_values.tex` (3 macros) — `\wtpCollectFinancial`, `\wtpShareFinancial` (financial-data attributes), `\wtpTopThreeMean` (each person's top-3 most-valued attributes). Extension sample.

WTP convention (**full swing, 2-beta**): privacy attributes are effects-coded
{invasive = -1, protective = +1}. The 0 point is only an estimation reference
and has no economic meaning — a site either does or does not engage in a
practice, there is no "half" state. The economically meaningful WTP is the full
invasive -> protective swing (2 coded units), so
WTP = (part-worth / price coef) * 2, aggregated by sample mean over untrimmed
individual WTPs. Applied uniformly to the figures and the macros so prose and
plots share one scale. Figure x-axis runs to +/-16 (was +/-8 per-unit).

Paths: data in `../data/` and `../results/` (script reads with `../` prefixes;
earlier versions assumed a different working directory and could not run from
`code_github/`).

### `privacy_descriptives/privacy_char_summary_stats_3.R`
- Fig 4 [fig:scores_by_category]: `domain_scores_box_plot.pdf`

### `survey_analysis/beliefs_analysis_overall.R`
- Fig 5 [fig:privacy_info_beliefs]: `beliefs_vs_truth.pdf`
- Rank-correlation scalars (Fig 5 caption): `output/values/rank_correlation_beliefs_values.tex` (9 macros, `\tauBy<Category>{N,Median,P}` for Collect/Use/Control). Within-participant Kendall tau-b per attribute category, extension sample. Wilcoxon p-values stored as `<0.001` strings (all below R's `eps = 2e-16` floor).

### `survey_analysis/beliefs_vs_conjoint.R`
- Table C.4 [tab:conjoint_vs_beliefs]: `misspec_pref_regression.tex`
- Table C.5 [tab:baseline_demo_heterogeneity]: `misspec_pref_demographics.tex`
- Misspecification scalars: `output/values/beliefs_vs_conjoint_values.tex` (3 macros) — `\misspecUnderPartworth` (0.016), `\misspecUnderSD` (0.04), `\misspecLinearSD` (0.01). Match v2; no number shift.

### `survey_analysis/other_survey_regressions.R`
- Table C.3 [tab:experimenter_demand]: `experiment_modified_behavior.tex`
- Table C.8 [tab:data_sharing_purpose]: `output/values/data_sharing_purpose_values.tex` (22 macros; table structure stays in paper, counts/percents replaced by macros)
- Table C.9 [tab:treatment_effect_data_sharing]: `data_sharing_treatment_effects.tex`

### `survey_analysis/top_sites_beliefs_analysis.R`
- Table [tab:belief_correctness_top_sites]: `top_sites_information.tex`
- Fig 8 [fig:beliefs_by_search]: `beliefs_by_info_acq.pdf`
- Table [tab:belief_correctness_top_sitesrandom_info]: `top_sites_information_rand_info.tex`
- Fig [fig:cdf_belief_correctness_top_sites]: `cumulative_belief_distance.pdf`
- Fig [fig:top_sites_heterogeneity_exposure]: `heterogeneous_beliefs_site_level_by_exposure_tercile.pdf`
- Belief-correctness scalars: `output/values/top_sites_beliefs_values.tex` (5 macros) — `\beliefDistanceInfoPP` (5.9), `\beliefCorrectWeakInfoPP` (8.3), `\beliefCorrectStrictInfoPP` (8.1), `\beliefCorrectInfoVsRestPP` (7.9, strict info-vs-merged-baseline regression — the number v2's intro "7.1pp" should become), `\beliefErrorMedianBaseline` (50).

### `information_acquisition_results/privacy_seeking_analysis.R`
- Fig 7 [fig:info_acq]: `info_acq_treatment_effects_total_sites.pdf` (7a), `ever_visited_privacy_policy.pdf` (7b)
- Table [tab:cookie_banner_interactions_treatment]: `cookie_banner_interactions_treatment.tex`
- Search-behavior scalars: `output/values/privacy_seeking_values.tex` (4 macros) — `\policyVisitControlPct` (2.4), `\policyVisitSaliencyPct` (34.7), `\policyVisitInfoPct` (2.4), `\policyVisitSaliencyFold` (14.2).

### `survey_analysis/mturk_freeform_analysis.R`
- Fig D.1 [fig:freeform_change_responses]: `freeform_change_responses.pdf`

### `survey_analysis/selection_into_study.R`
- Fig C.1 [fig:dem_balance]: `balance/{age,education,gender,income}.pdf`
- Fig C.2 [fig:selection_on_survey_payments]: `agg_selection_lots.pdf`
- Fig C.3 [fig:pew_comparison]: `pew_comparison.pdf`

### `time_use_analysis/time_usage_treatment_effects_SG.R` + `analysis_assortment_did.R`
The driver sources the helper. Together:
- Fig 9 [fig:intensive]: `preregistered_spec_i_baseline.pdf`, `preregistered_triple_interaction_baseline.pdf`
- Fig 10 [fig:extensive]: `assortment_concentration.pdf`, `assortment_privacy.pdf`
- Extensive-margin scalars: `output/values/assortment_did_values.tex` (4 macros) — `\extensiveHHIPvalue` (0.069), `\extensiveTopSharePvalue` (0.058, v2 "top-2 share"), `\privacyDifferentialPctSD` (10.2), `\portfolioPrivacyPctSD` (4.4).
- Table C.2 [tab:top_websites]: `output/values/top_websites_values.tex` (60 macros, `\topWeb<Rank><Field>`; tabular hand-written in `writeup_v3.tex`).

Driver rebuilds `../data/processed_data/joined_time_data.csv` when
`CONSTRUCT_FROM_SCRATCH = TRUE`.

**Open question (needs author review):** current code reproduces the stage-1
unweighted coefficients byte-for-byte, but the generated Fig 9/10 differ from
the figures currently in `writeup_v3.tex`, and the script that produced the
original Overleaf figures has not been located. Undocumented analysis choices:
`time_spent > 30` filter (drops ~40% of session rows); `lower_pct=0.05` /
`upper_pct=0.95` winsorization; `values_to_include=c(0)` (pre-period as one
bucket, not week-by-week as the caption describes); `log1p(minutes)` units.

### `time_use_analysis/time_use_baseline_scalars.R`
- Baseline daily-hours scalars: `output/values/baseline_time_use_values.tex` (3 macros) — `\baselineDailyHoursPrivacy`, `\baselineDailyHoursLeisure`, `\baselineDailyHoursAll`. Per-user mean over active days, then mean across users.
- Table C.2 source: writes the 60 `top_websites_values.tex` macros, reading `get_clean_time_data()` directly (not the cached `joined_time_data.csv`) so `SURVEY_WEBSITES` rows stay in scope.

### `survey_analysis/survey_analysis_scalars.R`
- Demographics + WTA scalars: `output/values/survey_descriptive_values.tex` (10 macros) — `\surveyMeanAge`, `\surveyFemalePct`, `\surveyMalePct`, `\surveyPnaPct`, `\surveyCollegePct`, `\wtaInvitedMedian`, `\wtaInvitedMean`, `\wtaFullMedian`, `\wtaFullMean`, `\invitedSampleCoveragePct`.

Sample: baseline completers (non-NA `cleaned_WTA`, 8168 of 8187). WTA censor
> $1000 -> NA for mean/median (per Guy 2026-05-25); coverage uses untrimmed
`cleaned_WTA`. Demographics use raw `Gender` (not the buggy `is_male`).

### `survey_analysis/survey_flow_scalars.R`
- Figure 2 funnel + per-treatment N + attrition p-values: `output/values/participant_flow_values.tex` (20 macros).

### `balance_table/exp_balance_table.R`
- Appendix C [tab:exp_balance_table]: `output/tables/exp_balance_table.tex` (21 rows x 5 cols, LaTeX tabular only — no `\scalebox` wrapper; the paper wraps it in `\scalebox{0.8}{\input{...}}`). R port of `assign_groups_wave_two.py` `check_balance()`; reads existing treatment assignments (no re-randomization). ANOVA via `oneway.test(var.equal=TRUE)`.

---

## Number shifts vs v2

Numbers the current pipeline produces that differ from v2's text. v3 prose must
be updated to these. Cause column distinguishes bug fixes (the new number is
more correct), convention changes (deliberate), and non-reproducible v2 numbers
(v2's value cannot be regenerated from any committed code).

| Macro / location | v2 | new | cause |
|---|---|---|---|
| top_sites belief regression coefs | -4.971 / 0.0667 / 0.0715 | -5.89 / 0.083 / 0.081 | bug fix (get_privacy_info_raw many-to-many) |
| top_sites intro "7.1pp" -> `\beliefCorrectInfoVsRestPP` | 7.1 | 7.9 | bug fix + strict merged-baseline spec |
| `\policyVisitSaliencyFold` | 17 | 14.2 | v2 stale value (rebuttal already reported 14.2x) |
| `\policyVisitInfoPct` | 5 | 2.4 | v2 stale value |
| `\extensiveHHIPvalue` | 0.063 | 0.069 | rebuild diff (small; significance unchanged) |
| `\extensiveTopSharePvalue` | 0.065 | 0.058 | rebuild diff |
| `\privacyDifferentialPctSD` | 11 | 10.2 | rebuild diff |
| `\portfolioPrivacyPctSD` | 3 | 4.4 | rebuild diff |
| `\wtpCollectFinancial` (line 466 "$6-8") | 6-8 | 5.22 | full-swing 2-beta mean; v2 contradicted its own Fig 6a |
| `\wtpShareFinancial` (line 466 "$6-8") | 6-8 | 4.21 | full-swing 2-beta mean |
| `\wtpTopThreeMean` (line 580 "$3") | 3 | ~14 | full-swing 2-beta mean (v2's $3 was per-unit) |
| `\wtaFullMean` | 52 | 41.44 | v2 not reproducible; $1000 censor applied |
| `\invitedSampleCoveragePct` | 89 | 90.8 | v2 not reproducible |
| `\attritionEndlinePvalue` | 0.96 | 0.833 | v2 not reproducible (no standard test gives 0.96) |
| `\baselineSurveyN` | 8169 | 8168 | off by one (nrow of survey_merged_final) |
| extension start-N labels | info 532 / saliency 533 | info 533 / saliency 532 | v2 prose swapped the two arms |

Confirmed correct (do NOT change): `\wtaFullMean` 41.44 and
`\invitedSampleCoveragePct` 90.8 are the right values under the agreed cleaning
rules; v2's 52 / 89 are simply not reproducible.

**Not from this session's tabular refactor.** The tabular-only refactor (see
Bug fixes) touched only how tables are written to disk, not how any number is
computed, so it introduced no new entries in this table. One difference noticed
during review but NOT caused by the refactor: `exp_balance_table` info-arm
Income reads ~81011 vs v2's 80932.89. This predates the refactor — it is an
artifact of the R port of `check_balance()` (R port + fixed 14-day window vs
v2's hand-copied Python value), confirmed by `git diff` showing the refactor
changed only the `\scalebox` wrapper and a comment. Left for author review
(likely tied to the dropped "Removed due to grounding" info user, N 1597->1596);
not yet root-caused.

---

## Bug fixes

This session (tabular-only refactor for clean Overleaf `\input`):
- All `etable(tex=TRUE)` tables and the hand-built `exp_balance_table` now save
  ONLY the `\begin{tabular}...\end{tabular}` block. `etable` wraps output in
  `\begingroup\centering ... \par\endgroup`, whose `\par` breaks
  `\scalebox{}{\input{...}}` in the manuscript ("There's no line here to end").
  Fix: capture the `etable` string (drop `file=`/`replace=`) and pass it through
  `write_tabular_only()` in `utils/tex_helpers.R`, which slices out the tabular
  environment. `exp_balance_table.R` instead drops the `\scalebox{0.8}{...}`
  it used to bake into the string. Scaling/caption/notes now live in the
  manuscript float wrapper. Verified by unit tests against a real `etable`
  fixture (`testing.R`) and by full-content review of all 8 regenerated tables.
  Touched scripts: `other_survey_regressions.R` (2 tables),
  `top_sites_beliefs_analysis.R` (2), `beliefs_vs_conjoint.R` (2 file-writing
  etables; the 2 console-only diagnostic etables left as-is),
  `privacy_seeking_analysis.R` (1), `exp_balance_table.R` (1).
- `data_sharing_treatment_effects` was missing `depvar = FALSE`, so it printed
  the raw `data_sharing_*_binary` dependent-variable row above the headers
  (absent in v2). Added `depvar = FALSE`. No number change.

Prior session:
- `get_privacy_info_raw()` sub-domain many-to-many — inflated belief sample, shifted top_sites coefficients.
- `get_balanced_panel()` twitter/x alias many-to-many — 7 (user, site) pairs with dup NA/non-NA privacy rows; affected Fig 9.
- `plot_violin.R` relative paths — could not run from `code_github/`; fixed `source()` + `../data` / `../results` prefixes.

Earlier:
- `join_weights()` missing `sample == "extension"` filter — weighted-regression row counts ~doubled.
- twitter -> x slug rename ran after `map_privacy_data()` — all twitter rows got `privacy_exist = FALSE`; moved before.
- `exp_balance_table` Python `today()` denominator (run-date dependent, not reproducible) — replaced with fixed 14-day baseline window.

---

## Not reproducible / skipped (kept hardcoded)

Numbers whose generating code is not in this R pipeline. Do not invent macros
for these; document why and leave them hardcoded (or for the co-author).

- **Human-rating descriptives** (57 indicators, 17.8 min mean policy read time, 383.8 cumulative human hours): not data-derived in this pipeline; `privacy_char_summary_stats_3.R` only makes Fig 4.
- **Popup DiD footnote** (beta_pooled=0.015, t=1.87, 73%, t=1.29): no generating R script located.
- **Tracker counts** (~30,000 unique third-party domains, ~1.5 million trackers): produced by an independent four-pass domain-matching pipeline, not in `replication_files/`.
- **All structural-model scalars** (welfare, privacy cost, counterfactuals, GMM parameters: $5.76/day, $127.96, $34/month, $202/month, beta_price=-0.529, alpha^info, alpha^search, etc.): out of scope ("non-structural results only"); produced by the co-author's structural pipeline.

---

## Style conventions

### `etable()` arguments
- `digits = 3`
- `signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1)`
- `depvar = FALSE` when `dict` maps the dependent variable or `headers` labels each column; keep default only for single-column tables.
- Do NOT pass `file=` / `replace=` to `etable`. Capture the returned string and
  write it with `write_tabular_only()` (see Output target below), so the saved
  `.tex` is a bare tabular with no `\begingroup\centering ... \par\endgroup`
  wrapper. `replace` is meaningless once `etable` no longer writes the file.
- FE row labels via `dict` (e.g. `"experiment_id" = "Participant FE"`, `"block_by_wave" = "Block FE"`, `"as.factor(weeks_since_intervention)" = "Weeks FE"`, `"website_aggregated_high_level" = "Website FE"`).

### Output target by content type
- Regression tables -> `fixest::etable(tex=TRUE)` (no `file=`) -> `write_tabular_only()` -> `output/tables/*.tex`
- Summary-stats tables -> `xtable::xtable()` -> `output/tables/*.tex`
- Hand-built tabulars -> `writeLines()` of the bare `\begin{tabular}...\end{tabular}` (no `\scalebox`) -> `output/tables/*.tex`
- Inline scalars -> `savetexvalue::save_tex_value()` -> `output/values/*.tex` (`\newcommand` macros)

All `output/tables/*.tex` are bare tabular environments. Scaling, `\caption`,
`\label`, and notes live in the manuscript float wrapper (e.g.
`\scalebox{0.8}{\input{...}}`), not in the saved file. This keeps every table
file usable under `\scalebox{}{\input{}}` without LaTeX errors.

### Number formatting (the savetexvalue trap)
`savetexvalue`'s default formatter is `scales::number()`, and its `accuracy`
argument is **silently ignored** in our installed version (output is always
full floating-point precision). Workaround: route every numeric through a
helper in `utils/number_format_helpers.R` that returns a pre-formatted
character string; `savetexvalue` writes strings verbatim, bypassing the broken
formatter.

| Helper | Rule |
|---|---|
| `format_count` | bare integer |
| `format_pct` | 1 decimal (drop trailing zeros) |
| `format_share` | 3 decimals (drop trailing zeros) |
| `format_dollar` | integer if whole, else 2 decimals |
| `format_hours` | 2 significant figures |
| `format_age` | 1 decimal |
| `format_income` | bare integer |
| `format_belief` | 1 decimal (0-100 scale) |
| `format_coef` | 3 decimals |
| `format_pvalue` | 3 decimals if p >= 0.001, scientific otherwise |

For a 2-decimal value with no dedicated helper, `sprintf("%.2f", x)` is used
(e.g. the conjoint WTP macros).

The savetexvalue file is append-only: `file.remove()` it at the top of the
block before writing, so re-runs don't duplicate `\newcommand` entries. One
bundle per logical group, not per script. Use `cat()`, not `print()`, for
console verification inside these scripts.

### Survey weighting
Scripts dispatch `WEIGHT_SPEC`: `unweighted` (paper, default) plus
`weight_census` / `weight_pew` / `weight_combined` (robustness). Output
filenames get a `_{spec}` suffix; unweighted has none. `join_weights()` must
filter `sample == "extension"` before joining. Set `WEIGHT_SPEC <- "all"` to
regenerate all four; the scalar macros are written for the unweighted spec
only. Scripts with the four-spec dispatcher: `other_survey_regressions.R`,
`top_sites_beliefs_analysis.R`, `privacy_seeking_analysis.R`,
`time_usage_treatment_effects_SG.R`.

Figures go to `output/figures/` as `.pdf`; styling in `utils/plot_rules.R`.

---

## Utils (sourced helpers)

- `plot_rules.R` — ggplot theme, color scales, sizing constants.
- `values.R` — date constants, `BAD_USERS`, `SURVEY_WEBSITES`, `AUX_DATA_DIR`.
- `time_usage_helpers.R` — browser-data cleaning, domain aggregation/classification, privacy scoring, conjoint utility loading. The twitter -> x slug rename runs inside `get_clean_time_data()` before `map_privacy_data()`.
- `info_acq_helpers.R` — privacy-policy visit detection, info-acquisition aggregation.
- `number_format_helpers.R` — per-data-type formatting helpers (see Style conventions); single source of truth for accuracy rules and the savetexvalue workaround.
- `tex_helpers.R` — LaTeX output post-processing. `write_tabular_only()` extracts the bare `\begin{tabular}...\end{tabular}` from `etable(tex=TRUE)` output, dropping the `\begingroup\centering ... \par\endgroup` wrapper so the saved file is safe under `\scalebox{}{\input{}}`. Unit-tested in `testing.R`.

---

## Paper figures referenced but NOT produced here

For these, comment out the `\begin{figure}/\end{figure}` block in
`writeup_v3.tex` and add a `% TODO:` line so compile passes and the gap is
visible.

- **Cookie deletion / CPV (Appendix G)** — generated by `code/cookie_deletion/...`, not yet refactored into `replication_files/`. ~18 `cpv_*` / `time_*` figures.
- **Structural model** — 3 figures from the co-author's pipeline (`fig_valuation_distribution.pdf`, `fig_waterfall_gdpr.pdf`, `fig_waterfall_info_provision.pdf`).
- **Experimental materials / screenshots** — hand-made design docs (treatment dialogs, study flow, consent), not produced by any R script.
