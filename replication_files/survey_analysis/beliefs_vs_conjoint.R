# ============================================================================
# Correlate belief misspecification with conjoint preference intensities
# ============================================================================

rm(list = ls())

if (Sys.info()[['nodename']] == 'GSB-P4FVDL7QF6'){
  WD <- "/Users/sggold/Library/CloudStorage/Dropbox/Shared-Project-Folders/Privacy-Experiment/spring2025experiment"
} else {
  WD <- "/Users/guyaridor/Dropbox/Privacy-Experiment/spring2025experiment"
}

setwd(WD)

source("code/utils/values.R")
source("code/utils/time_usage_helpers.R")
source("code/utils/info_acq_helpers.R")

library(tidyverse)
library(fixest)
library(xtable)
library(ggrepel)

FIGURES_DIR <- "results/baseline_survey_descriptives/"
TABLES_DIR  <- "results/baseline_survey_descriptives/"

# ============================================================================
# 1) Load and prepare data
# ============================================================================

survey_merged <- read.csv("data/Survey/survey_merged_final.csv", stringsAsFactors = FALSE) %>%
  mutate(emailid = tolower(emailid))

meta_data <- read.csv("data/final_extension_data/experiment_conditions_pilot_july_2024.csv") %>%
  mutate(wave_id = ifelse(wave_id == 3, 2, wave_id),
         block_by_wave = paste(wave_id, block_idx, sep = "_"),
         emailid = tolower(email))

survey_merged <- survey_merged %>%
  left_join(meta_data %>% select(emailid, experiment_id, experiment_condition, wave_id, block_idx, block_by_wave),
            by = "emailid") %>%
  filter(attentioncheck1 == 3,
         attentioncheck == 2,
         sys_ElapsedTime < 6000,
         sys_ElapsedTime > 600)

# Conjoint: two-step crosswalk (RespondentId -> sys_RespNum -> experiment_id)
conjoint <- read.csv("data/Conjoint/Conjoint-Finalized/tables/individual_parameters_wide_means.csv")

crosswalk <- survey_merged %>%
  select(sys_RespNum, experiment_id) %>%
  filter(!is.na(sys_RespNum) & !is.na(experiment_id)) %>%
  distinct(sys_RespNum, .keep_all = TRUE)

conjoint <- conjoint %>%
  left_join(crosswalk, by = c("RespondentId" = "sys_RespNum"))

privacy_info <- read.csv("data/final_extension_data/privacy_info.csv")

# ============================================================================
# 2) Rescale beliefs from 1-5 to 0-100
# ============================================================================

belief_cols <- c(paste0("beliefscollection_r", 1:7),
                 paste0("beliefsuse_r", 1:8),
                 paste0("beliefscontrol_r", 1:4))

for (col in belief_cols) {
  if (col %in% names(survey_merged)) {
    survey_merged[[col]] <- (survey_merged[[col]] - 1) * 25
  }
}

# ============================================================================
# 3) Ground truth (fraction of sites with each practice, 0-100)
# ============================================================================

ground_truth <- privacy_info %>%
  mutate(rating_numeric = ifelse(rating == "Yes", 1, 0)) %>%
  group_by(feature, field) %>%
  summarise(true_pct = mean(rating_numeric) * 100, .groups = "drop") %>%
  mutate(q_html_key = paste(feature, field, sep = "-"),
         true_pct_rounded = round(true_pct / 25) * 25)

# ============================================================================
# 4) Crosswalks: belief survey var -> internal key -> conjoint column
# ============================================================================

belief_to_internal <- c(
  beliefscollection_r1 = "collect-log",
  beliefscollection_r2 = "collect-bio",
  beliefscollection_r3 = "collect-sensitive",
  beliefscollection_r4 = "collect-financial",
  beliefscollection_r5 = "collect-offsite",
  beliefscollection_r6 = "collect-location",
  beliefscollection_r7 = "collect-social",
  beliefsuse_r1 = "anonymized-anonymized",
  beliefsuse_r2 = "share-social",
  beliefsuse_r3 = "share-finance",
  beliefsuse_r4 = "share-ads",
  beliefsuse_r5 = "share-law",
  beliefsuse_r6 = "share-service",
  beliefsuse_r7 = "share-partners",
  beliefsuse_r8 = "personalization-personalization",
  beliefscontrol_r1 = "change-change",
  beliefscontrol_r2 = "automated-automated",
  beliefscontrol_r3 = "deletion-deletion",
  beliefscontrol_r4 = "storage-storage"
)

belief_to_t_col <- c(
  beliefscollection_r1 = "collection_log",
  beliefscollection_r2 = "collection_bio",
  beliefscollection_r3 = "collection_sensitive",
  beliefscollection_r4 = "collection_financial",
  beliefscollection_r5 = "collection_offsite",
  beliefscollection_r6 = "collection_location",
  beliefscollection_r7 = "collection_social",
  beliefsuse_r1 = "usel_anonymized",
  beliefsuse_r2 = "use_social",
  beliefsuse_r3 = "use_financial",
  beliefsuse_r4 = "use_advertising",
  beliefsuse_r5 = "use_law",
  beliefsuse_r6 = "use_service",
  beliefsuse_r7 = "use_partners",
  beliefsuse_r8 = "use_personalization",
  beliefscontrol_r1 = "control_change",
  beliefscontrol_r2 = "control_automated",
  beliefscontrol_r3 = "control_delete",
  beliefscontrol_r4 = "control_storage"
)

labels <- c(
  beliefscollection_r1 = "Collects Browsing Behavior on Site",
  beliefscollection_r2 = "Collects Demographic Data",
  beliefscollection_r3 = "Collects Sensitive Personal Information",
  beliefscollection_r4 = "Collects Financial Data",
  beliefscollection_r5 = "Collects Browsing Behavior on Other Sites",
  beliefscollection_r6 = "Collects Location Data",
  beliefscollection_r7 = "Collects Social Media Profiles",
  beliefsuse_r1 = "Share Data Only After Anonymized",
  beliefsuse_r2 = "Share Data with Social Media",
  beliefsuse_r3 = "Share Data with Financial Providers",
  beliefsuse_r4 = "Share Data with Advertisers",
  beliefsuse_r5 = "Share Data with Law Enforcement",
  beliefsuse_r6 = "Share Data with Service Providers",
  beliefsuse_r7 = "Share Data with Non-Service Providers",
  beliefsuse_r8 = "Use Data to Personalize Experience",
  beliefscontrol_r1 = "Notified of Policy Changes",
  beliefscontrol_r2 = "Data Auto-Deleted After Time Period",
  beliefscontrol_r3 = "Can Delete Data on Request",
  beliefscontrol_r4 = "Data Stored Securely and Anonymized"
)

# ============================================================================
# 5) Build analysis dataset: beliefs (long) x conjoint part-worths (long)
# ============================================================================

beliefs_long <- survey_merged %>%
  select(experiment_id, all_of(intersect(names(belief_to_t_col), names(survey_merged)))) %>%
  pivot_longer(cols = -experiment_id, names_to = "belief_var", values_to = "belief_value") %>%
  mutate(
    q_html_key = belief_to_internal[belief_var],
    t_col      = belief_to_t_col[belief_var],
    label      = labels[belief_var]
  ) %>%
  left_join(ground_truth %>% select(q_html_key, true_pct, true_pct_rounded), by = "q_html_key") %>%
  mutate(
    misspec         = belief_value - true_pct,
    abs_misspec     = abs(misspec),
    misspec_rounded = belief_value - true_pct_rounded
  )

conjoint_cols_present <- intersect(unique(belief_to_t_col), names(conjoint))

pref_long <- conjoint %>%
  filter(!is.na(experiment_id)) %>%
  select(experiment_id, all_of(conjoint_cols_present)) %>%
  pivot_longer(cols = -experiment_id, names_to = "t_col", values_to = "pref_intensity")

joined <- beliefs_long %>%
  left_join(pref_long, by = c("experiment_id", "t_col"))

# Align part-worth signs (flip control + anonymized so positive = dislikes practice)
flip_cols <- c("control_change", "control_automated", "control_delete",
               "control_storage", "usel_anonymized")

joined <- joined %>%
  mutate(
    category = case_when(
      grepl("^collection", t_col) ~ "Collection",
      grepl("^use|^usel", t_col)  ~ "Use/Sharing",
      grepl("^control", t_col)    ~ "Control"
    ),
    pref_aligned = ifelse(t_col %in% flip_cols, -pref_intensity, pref_intensity),
    misspec_bin = factor(
      case_when(misspec_rounded < 0 ~ "Underestimate",
                misspec_rounded > 0 ~ "Overestimate",
                TRUE ~ "Correct"),
      levels = c("Correct", "Underestimate", "Overestimate")
    ),
    overestimate  = pmax(misspec, 0),
    underestimate = pmin(misspec, 0)
  )

# Demographics (person-level, joined once)
# Education: 1=Grade school, 2=Some HS, 3=HS/GED, 4=Some college,
#            5=Trade/vocational, 6=Associate's, 7=Bachelor's,
#            8=Master's, 9=Professional, 10=Doctorate
# Income: 1=<$10k, 2=$10-19k, 3=$20-29k, 4=$30-49k, 5=$50-59k,
#         6=$60-74k, 7=$75-99k, 8=$100-124k, 9=$125-149k,
#         10=$150-199k, 11=$200k+, 12=Prefer not to answer
# Gender: 1=Male, 2=Female, 3=Prefer not to answer
demog <- survey_merged %>%
  distinct(experiment_id, .keep_all = TRUE) %>%
  select(experiment_id, Age, Education, Gender, Income) %>%
  mutate(
    age_bin = case_when(
      Age < 30 ~ "18-29", Age < 45 ~ "30-44",
      Age < 60 ~ "45-59", Age >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    high_education = as.numeric(Education >= 7),
    high_income = case_when(
      Income >= 7 & Income <= 11 ~ 1,
      Income <= 6 ~ 0,
      TRUE ~ NA_real_
    ),
    gender_label = case_when(
      Gender == 1 ~ "Male",
      Gender == 2 ~ "Female",
      Gender == 3 ~ "Other/PNA",
      TRUE ~ NA_character_
    ),
    gender_label = factor(gender_label, levels = c("Male", "Female", "Other/PNA"))
  )

joined <- joined %>%
  left_join(demog, by = "experiment_id")

cat("Analysis dataset:", nrow(joined), "rows\n")
cat("Non-missing belief-preference pairs:", sum(!is.na(joined$pref_aligned) & !is.na(joined$misspec)), "\n\n")

# ============================================================================
# 6) Key standard deviations for effect size context
# ============================================================================

sd_pref        <- sd(joined$pref_aligned, na.rm = TRUE)
sd_misspec     <- sd(joined$misspec, na.rm = TRUE)
sd_abs_misspec <- sd(joined$abs_misspec, na.rm = TRUE)

cat("=== Standard deviations ===\n")
cat("SD pref_aligned:", round(sd_pref, 4), "\n")
cat("SD misspec:",      round(sd_misspec, 2), "pp\n")
cat("SD |misspec|:",    round(sd_abs_misspec, 2), "pp\n\n")

# ============================================================================
# 7) Summary statistics
# ============================================================================

cat("=== Misspecification bin sizes (rounded truth) ===\n")
joined %>% filter(!is.na(pref_aligned)) %>% count(misspec_bin) %>% print()

cat("\n=== Preferences and misspecification by category ===\n")
joined %>%
  filter(!is.na(pref_aligned) & !is.na(misspec)) %>%
  group_by(category) %>%
  summarise(
    median_pref     = median(pref_aligned),
    median_abs_pref = median(abs(pref_aligned)),
    median_misspec  = median(misspec),
    median_abs_misspec = median(abs_misspec),
    .groups = "drop"
  ) %>% print()

# ============================================================================
# 8) Main regressions: misspecification and preferences
# ============================================================================

reg_linear <- feols(
  pref_aligned ~ misspec | t_col + experiment_id,
  cluster = "experiment_id", data = joined)

reg_linear_rounded <- feols(
  pref_aligned ~ misspec_rounded | t_col + experiment_id,
  cluster = "experiment_id", data = joined)

reg_linear_cat <- feols(
  pref_aligned ~ misspec * category | t_col + experiment_id,
  cluster = "experiment_id", data = joined)

reg_split <- feols(
  pref_aligned ~ overestimate + underestimate | t_col + experiment_id,
  cluster = "experiment_id", data = joined)

reg_bins <- feols(
  pref_aligned ~ misspec_bin | t_col + experiment_id,
  cluster = "experiment_id", data = joined)

reg_bins_cat <- feols(
  pref_aligned ~ misspec_bin * category | t_col + experiment_id,
  cluster = "experiment_id", data = joined)

cat("\n=== Main results ===\n")
etable(reg_linear, reg_linear_cat, reg_split, reg_bins, reg_bins_cat,
       headers = c("Linear", "Linear x Cat", "Over/Under", "Bins", "Bins x Cat"))

# Effect sizes
b_linear     <- coef(reg_linear)["misspec"]
b_collection <- coef(reg_linear_cat)["misspec"]
b_under      <- coef(reg_split)["underestimate"]
b_under_bin  <- coef(reg_bins)["misspec_binUnderestimate"]
b_over_bin   <- coef(reg_bins)["misspec_binOverestimate"]

cat("\n--- Effect sizes ---\n")
cat("Pooled: 1 SD misspec (", round(sd_misspec, 1), "pp) ->",
    round(b_linear * sd_misspec / sd_pref, 4), "SD of pref\n")
cat("Collection: 1 SD misspec ->",
    round(b_collection * sd_misspec / sd_pref, 4), "SD of pref\n")
cat("Underestimate (linear): 1 SD misspec ->",
    round(b_under * sd_misspec / sd_pref, 4), "SD of pref\n")
cat("Underestimate vs Correct (bin):",
    round(b_under_bin / sd_pref, 4), "SD of pref\n")
cat("Overestimate vs Correct (bin):",
    round(b_over_bin / sd_pref, 4), "SD of pref\n")

# ============================================================================
# 9) Demographic analysis
# ============================================================================

reg_misspec_demog <- feols(
  abs_misspec ~ age_bin + high_education + high_income + gender_label | t_col,
  cluster = "experiment_id", data = joined)

reg_pref_demog <- feols(
  pref_aligned ~ age_bin + high_education + high_income + gender_label | t_col,
  cluster = "experiment_id", data = joined)

cat("\n=== Demographics: misspecification vs preferences ===\n")
etable(reg_misspec_demog, reg_pref_demog,
       headers = c("|Misspecification|", "Preferences"))

# Effect sizes
b_educ_misspec <- coef(reg_misspec_demog)["high_education"]
b_educ_pref    <- coef(reg_pref_demog)["high_education"]
b_female_pref  <- coef(reg_pref_demog)["gender_labelFemale"]

cat("\n--- Demographic effect sizes ---\n")
cat("Education on |misspec|:", round(b_educ_misspec, 2), "pp =",
    round(b_educ_misspec / sd_abs_misspec, 4), "SD\n")
cat("Education on preferences:", round(b_educ_pref, 4), "=",
    round(b_educ_pref / sd_pref, 4), "SD\n")
cat("Female on preferences:", round(b_female_pref, 4), "=",
    round(b_female_pref / sd_pref, 4), "SD\n")

# ============================================================================
# 10) Export tables
# ============================================================================

# ============================================================================
# 10) Export tables
# ============================================================================

dict_main <- c(
  "misspec"                  = "Misspecification (pp)",
  "overestimate"             = "Overestimate (pp)",
  "underestimate"            = "Underestimate (pp)",
  "misspec_binUnderestimate" = "Underestimate",
  "misspec_binOverestimate"  = "Overestimate",
  "misspec_rounded"         = "Misspecification (pp)",
  "misspec:categoryControl"      = "Misspec $\\times$ Control",
  "misspec:categoryUse/Sharing"  = "Misspec $\\times$ Use/Sharing",
  "misspec_binUnderestimate:categoryControl"     = "Underestimate $\\times$ Control",
  "misspec_binOverestimate:categoryControl"      = "Overestimate $\\times$ Control",
  "misspec_binUnderestimate:categoryUse/Sharing" = "Underestimate $\\times$ Use/Sharing",
  "misspec_binOverestimate:categoryUse/Sharing"  = "Overestimate $\\times$ Use/Sharing",
  "t_col"         = "Attribute FE",
  "experiment_id" = "Participant FE",
  "pref_aligned"   = "Part-Worth (Aligned)"
)

dict_demog <- c(
  "high_education"        = "Bachelor's Degree or Above",
  "high_income"           = "Household Income $\\geq$ \\$75k",
  "gender_labelFemale"    = "Female",
  "gender_labelOther/PNA" = "Other/Prefer Not to Answer (Gender)",
  "age_bin30-44"          = "Age 30--44",
  "age_bin45-59"          = "Age 45--59",
  "age_bin60+"            = "Age 60+",
  "t_col"                 = "Attribute FE",
  "experiment_id"         = "Participant FE",
  "pref_aligned"          = "Part-Worth (Aligned)",
  "abs_misspec"           = "|Misspecification| (pp)"
)

etable(reg_bins, reg_linear_rounded, reg_split,
       headers = c("Discrete", "Linear", "Over/Under"),
       dict = dict_main, tex = TRUE, replace = TRUE,
       title = "Belief Misspecification and Privacy Preference Intensity",
       file = paste0(TABLES_DIR, "misspec_pref_regression.tex"))

etable(reg_misspec_demog, reg_pref_demog,
       headers = c("|Misspecification|", "Preferences"),
       dict = dict_demog, tex = TRUE, replace = TRUE,
       title = "Demographic Heterogeneity in Misspecification and Preferences",
       file = paste0(TABLES_DIR, "misspec_pref_demographics.tex"))

# ============================================================================
# 11) Scatter plot: attribute-level medians
# ============================================================================

attr_summary <- joined %>%
  filter(!is.na(pref_aligned) & !is.na(misspec)) %>%
  group_by(label, t_col) %>%
  summarise(
    median_misspec      = median(misspec),
    median_pref_aligned = median(pref_aligned),
    .groups = "drop"
  ) %>%
  mutate(
    category = case_when(
      grepl("^collection", t_col) ~ "Collection",
      grepl("^use|^usel", t_col)  ~ "Use/Sharing",
      grepl("^control", t_col)    ~ "Control"
    )
  )

g_signed <- ggplot(attr_summary, aes(x = median_misspec, y = median_pref_aligned,
                                     label = label, color = category)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3, max.overlaps = 20, box.padding = 0.5,
                  segment.color = "gray60", segment.size = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "black",
              inherit.aes = FALSE, aes(x = median_misspec, y = median_pref_aligned)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Collection" = "#e41a1c", "Use/Sharing" = "#377eb8", "Control" = "#4daf4a")) +
  labs(
    x = "Median Belief Misspecification (pp)",
    y = "Median WTP to Avoid Practice (Part-Worth, aligned)",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.margin = margin(5, 20, 5, 5))

ggsave(paste0(FIGURES_DIR, "misspec_vs_pref_intensity_signed.pdf"),
       g_signed, width = 11, height = 7)

cat("\nDone.\n")