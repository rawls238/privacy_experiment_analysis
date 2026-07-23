# ============================================================================
# plot_exp_top2_feature.R
# Experimental Top-2 personalized privacy features
# ============================================================================
#
# PURPOSE
#   Reconstruct the Top-2 personalized privacy features using the archived
#   conjoint estimates that were available for the experimental assignment.
#
# SCORE
#   V_ik = theta_ik * (1 - theta_ik) * abs(beta_ik)
#
# DATA SOURCES
#   Archived utility-space beta:
#     ../results/Conjoint_result/conjoint_with_website/old_tables/
#       individual_parameters_wide_means.csv
#
#   Baseline beliefs:
#     ../data/Survey/survey_merged_final.csv
#
#   Respondent-email mapping:
#     ../data/Survey/final_baseline_survey.csv
#
#   Experimental treatment assignments:
#     ../data/final_extension_data/
#       experiment_conditions_pilot_july_2024.csv
#
# OUTPUTS
#   output/figures/top2_privacy_attributes_extension.pdf
#   output/figures/top2_privacy_attributes_full.pdf
#
# NOTES
#   - No final WTP-space estimates are used in this script.
#   - No price coefficient or WTP conversion is needed.
#   - Exact score ties are broken randomly with a fixed seed.
#   - Top-2 is computed once for the full baseline sample; the experimental
#     figure is the subset assigned to the three official treatment arms.
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/values.R")
source("replication_files/utils/plot_rules.R")

SEED <- 523L

FIGURES_DIR <- "output/figures"

OLD_TABLE_DIR <- "../results/Conjoint_result/conjoint_with_website/old_tables"
BETA_FILE <- file.path(
  OLD_TABLE_DIR,
  "individual_parameters_wide_means.csv"
)
BELIEF_FILE <- "../data/Survey/survey_merged_final.csv"
BASELINE_FILE <- "../data/Survey/final_baseline_survey.csv"
EXPERIMENT_FILE <- paste0(
  "../data/final_extension_data/",
  "experiment_conditions_pilot_july_2024.csv"
)

EXPECTED_ARM_COUNTS <- c(
  Control = 532L,
  Saliency = 532L,
  Information = 533L
)

PRIVACY_FEATURES <- PRIVACY_ATTR_MASTER$feature_name

BELIEF_TO_FEATURE <- c(
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

read_required_csv <- function(path, label) {
  if (!file.exists(path)) {
    stop("Missing ", label, ": ", path)
  }
  readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  )
}

standardize_id <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

standardize_email <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x == ""] <- NA_character_
  x
}

map_official_arm <- function(x) {
  z <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  case_when(
    z == "control" ~ "Control",
    z == "saliency" ~ "Saliency",
    z == "info" ~ "Information",
    TRUE ~ NA_character_
  )
}

get_feature_category <- function(feature_name) {
  category_map <- setNames(
    PRIVACY_ATTR_MASTER$category,
    PRIVACY_ATTR_MASTER$feature_name
  )
  unname(category_map[feature_name])
}

make_top2_plot <- function(top2_data, output_file) {
  feature_labels <- setNames(
    PRIVACY_ATTR_MASTER$label,
    PRIVACY_ATTR_MASTER$feature_name
  )
  
  category_labels <- c(
    control = "Control",
    use = "Use",
    collect = "Collect"
  )
  
  top2_counts <- top2_data %>%
    count(feature_name, name = "n_users") %>%
    complete(
      feature_name = PRIVACY_FEATURES,
      fill = list(n_users = 0L)
    ) %>%
    mutate(
      label = unname(feature_labels[feature_name]),
      category = get_feature_category(feature_name),
      category_label = factor(
        unname(category_labels[category]),
        levels = c("Control", "Use", "Collect")
      )
    ) %>%
    arrange(n_users, label) %>%
    mutate(label = factor(label, levels = label))
  
  fill_colors <- c(
    "Control" = unname(PRIVACY_CATEGORY_COLORS["control"]),
    "Use" = unname(PRIVACY_CATEGORY_COLORS["use"]),
    "Collect" = unname(PRIVACY_CATEGORY_COLORS["collect"])
  )
  
  plot_object <- ggplot(
    top2_counts,
    aes(
      x = label,
      y = n_users,
      fill = category_label
    )
  ) +
    geom_col(width = 0.75) +
    geom_text(
      aes(label = n_users),
      hjust = -0.15,
      size = 3.2,
      color = TEXT_COLOR
    ) +
    coord_flip() +
    scale_fill_manual(
      values = fill_colors,
      name = NULL
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.12))
    ) +
    theme_privacy_experiment(show_grid_y = FALSE) +
    labs(
      x = NULL,
      y = "Count of Appearances in Users' Top-2 Preferences"
    )
  
  ggsave(
    output_file,
    plot_object,
    width = 8,
    height = 6
  )
  
  cat("Saved: ", output_file, "\n", sep = "")
  invisible(plot_object)
}

cat(strrep("=", 80), "\n")
cat("LOADING ARCHIVED CONJOINT AND EXPERIMENT INPUTS\n")
cat(strrep("=", 80), "\n")

beta_wide <- read_required_csv(
  BETA_FILE,
  "archived conjoint beta table"
)
beliefs_raw <- read_required_csv(
  BELIEF_FILE,
  "baseline belief table"
)
baseline_raw <- read_required_csv(
  BASELINE_FILE,
  "baseline respondent-email map"
)
experiment_raw <- read_required_csv(
  EXPERIMENT_FILE,
  "experiment metadata"
)

required_beta_columns <- c("RespondentId", PRIVACY_FEATURES)
missing_beta_columns <- setdiff(
  required_beta_columns,
  names(beta_wide)
)
if (length(missing_beta_columns) > 0L) {
  stop(
    "Archived beta table is missing: ",
    paste(missing_beta_columns, collapse = ", ")
  )
}

missing_belief_columns <- setdiff(
  names(BELIEF_TO_FEATURE),
  names(beliefs_raw)
)
if (length(missing_belief_columns) > 0L) {
  stop(
    "Belief table is missing: ",
    paste(missing_belief_columns, collapse = ", ")
  )
}

belief_id_column <- if ("sys_RespNum" %in% names(beliefs_raw)) {
  "sys_RespNum"
} else if ("RespondentId" %in% names(beliefs_raw)) {
  "RespondentId"
} else {
  stop("Could not identify respondent ID in belief table.")
}

baseline_id_column <- if ("sys_RespNum" %in% names(baseline_raw)) {
  "sys_RespNum"
} else if ("sys_respnum" %in% names(baseline_raw)) {
  "sys_respnum"
} else {
  stop("Could not identify respondent ID in baseline table.")
}

baseline_email_column <- if ("emailid" %in% names(baseline_raw)) {
  "emailid"
} else if ("email" %in% names(baseline_raw)) {
  "email"
} else {
  stop("Could not identify email in baseline table.")
}

experiment_email_column <- if ("email" %in% names(experiment_raw)) {
  "email"
} else if ("emailid" %in% names(experiment_raw)) {
  "emailid"
} else {
  stop("Could not identify email in experiment metadata.")
}

if (!"experiment_condition" %in% names(experiment_raw)) {
  stop("Experiment metadata is missing experiment_condition.")
}

beta_wide <- beta_wide %>%
  mutate(
    RespondentId = standardize_id(RespondentId)
  )

beliefs_raw <- beliefs_raw %>%
  mutate(
    RespondentId = standardize_id(.data[[belief_id_column]])
  )

baseline_map <- baseline_raw %>%
  transmute(
    RespondentId = standardize_id(.data[[baseline_id_column]]),
    email = standardize_email(.data[[baseline_email_column]])
  ) %>%
  filter(
    !is.na(RespondentId),
    !is.na(email)
  ) %>%
  distinct()

experiment_assignments <- experiment_raw %>%
  transmute(
    email = standardize_email(.data[[experiment_email_column]]),
    official_arm = map_official_arm(experiment_condition)
  ) %>%
  filter(
    !is.na(email),
    !is.na(official_arm)
  ) %>%
  distinct(email, official_arm)

email_arm_conflicts <- experiment_assignments %>%
  count(email, name = "n_arms") %>%
  filter(n_arms > 1L)

if (nrow(email_arm_conflicts) > 0L) {
  stop("Some emails map to multiple official treatment arms.")
}

cat("\n", strrep("=", 80), "\n", sep = "")
cat("BUILDING FULL-SAMPLE INFORMATION SCORES\n")
cat(strrep("=", 80), "\n")

beliefs_long <- beliefs_raw %>%
  select(
    RespondentId,
    all_of(names(BELIEF_TO_FEATURE))
  ) %>%
  pivot_longer(
    -RespondentId,
    names_to = "belief_column",
    values_to = "belief_raw"
  ) %>%
  mutate(
    feature_name = unname(
      BELIEF_TO_FEATURE[belief_column]
    ),
    theta = case_when(
      belief_raw == 1 ~ 0.00,
      belief_raw == 2 ~ 0.25,
      belief_raw == 3 ~ 0.50,
      belief_raw == 4 ~ 0.75,
      belief_raw == 5 ~ 1.00,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    RespondentId,
    feature_name,
    theta
  ) %>%
  filter(!is.na(theta))

beta_long <- beta_wide %>%
  select(
    RespondentId,
    all_of(PRIVACY_FEATURES)
  ) %>%
  pivot_longer(
    -RespondentId,
    names_to = "feature_name",
    values_to = "beta_old"
  ) %>%
  mutate(beta_old = as.numeric(beta_old))

score_data <- beta_long %>%
  inner_join(
    beliefs_long,
    by = c("RespondentId", "feature_name")
  ) %>%
  mutate(
    information_value =
      theta * (1 - theta) * abs(beta_old)
  )

score_coverage <- score_data %>%
  count(RespondentId, name = "n_features")

if (any(score_coverage$n_features != length(PRIVACY_FEATURES))) {
  stop("Some respondents do not have all 19 information scores.")
}

full_sample_n <- n_distinct(score_data$RespondentId)
if (full_sample_n != 8168L) {
  stop(
    "Expected 8,168 full-sample respondents, found ",
    full_sample_n,
    "."
  )
}

# Randomize only the ordering of tied scores. The fixed seed makes the
# reconstructed assignments reproducible.
set.seed(SEED)

top2_full <- score_data %>%
  arrange(
    RespondentId,
    feature_name
  ) %>%
  mutate(tie_break = runif(n())) %>%
  arrange(
    RespondentId,
    desc(information_value),
    tie_break
  ) %>%
  group_by(RespondentId) %>%
  slice_head(n = 2L) %>%
  ungroup()

top2_per_respondent <- top2_full %>%
  count(RespondentId, name = "n_selected")

if (
  nrow(top2_per_respondent) != full_sample_n ||
  any(top2_per_respondent$n_selected != 2L)
) {
  stop("Top-2 selection did not return exactly two features per respondent.")
}

if (nrow(top2_full) != 2L * full_sample_n) {
  stop("Full-sample Top-2 appearance count is incorrect.")
}

cat("Full conjoint sample: ", full_sample_n, " respondents\n", sep = "")
cat("Full-sample appearances: ", nrow(top2_full), "\n", sep = "")

cat("\n", strrep("=", 80), "\n", sep = "")
cat("IDENTIFYING THE 1,597-PERSON EXPERIMENTAL SAMPLE\n")
cat(strrep("=", 80), "\n")

official_sample_candidates <- baseline_map %>%
  left_join(
    experiment_assignments,
    by = "email",
    relationship = "many-to-one"
  ) %>%
  filter(!is.na(official_arm)) %>%
  distinct(
    RespondentId,
    official_arm
  )

full_sample_ids <- score_data %>%
  distinct(RespondentId)

official_sample <- official_sample_candidates %>%
  inner_join(
    full_sample_ids,
    by = "RespondentId"
  )

arm_counts <- official_sample %>%
  count(official_arm, name = "observed_n") %>%
  complete(
    official_arm = names(EXPECTED_ARM_COUNTS),
    fill = list(observed_n = 0L)
  ) %>%
  mutate(
    expected_n = unname(
      EXPECTED_ARM_COUNTS[official_arm]
    )
  )

if (
  nrow(official_sample) != sum(EXPECTED_ARM_COUNTS) ||
  any(arm_counts$observed_n != arm_counts$expected_n)
) {
  print(arm_counts)
  stop(
    "Experimental sample does not reproduce ",
    "Control=532, Saliency=532, Information=533."
  )
}

top2_extension <- top2_full %>%
  inner_join(
    official_sample,
    by = "RespondentId"
  )

if (nrow(top2_extension) != 2L * sum(EXPECTED_ARM_COUNTS)) {
  stop("Experimental-sample Top-2 appearance count is incorrect.")
}

cat("Experimental sample: 1,597 respondents\n")
print(
  arm_counts %>%
    select(
      official_arm,
      observed_n
    )
)
cat(
  "Experimental-sample appearances: ",
  nrow(top2_extension),
  "\n",
  sep = ""
)

cat("\n", strrep("=", 80), "\n", sep = "")
cat("CREATING TOP-2 FIGURES\n")
cat(strrep("=", 80), "\n")

make_top2_plot(
  top2_extension,
  file.path(
    FIGURES_DIR,
    "top2_privacy_attributes_extension.pdf"
  )
)

make_top2_plot(
  top2_full,
  file.path(
    FIGURES_DIR,
    "top2_privacy_attributes_full.pdf"
  )
)

cat("\nAll experimental Top-2 feature plots complete.\n")