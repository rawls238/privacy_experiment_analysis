# =============================================================================
# MTURK FREEFORM ANALYSIS
# =============================================================================
#
# Produces:
#   Appendix D [Section sec:app-self-reported-changes]
#     Fig D.1 [fig:freeform_change_responses, "Self-reported Changes in Behavior"]:
#       output/figures/freeform_change_responses.pdf
#
# Inputs:
#   ../data/Survey/freefrom_responses_coded.csv         (MTurk-coded responses)
#   ../data/Survey/freefrom_responses_coded_ties_manual.csv (researcher tiebreaks)
#
# Dependencies:
#   replication_files/utils/plot_rules.R   (TREATMENT_ORDER, ERRORBAR_WIDTH,
#                                           theme_privacy_experiment,
#                                           scale_fill_treatment)
#
# Outputs:
#   output/figures/freeform_change_responses.pdf
#
# Weight spec: only "unweighted" is implemented (the paper Appendix D figure
# is unweighted). See the WEIGHT_SPEC flag block below for a future-weighted
# implementation hook.
#
# Note: Previous version of this script also computed (now removed as dead code):
#   - A first-pass `ties` computation from `by_id` that was immediately
#     overwritten by `read.csv(..._ties_manual.csv)` on the very next line.
#   - A `final_choice %>% group_by(...) %>% summarise(a = n())` line that
#     produced a tibble that was never assigned or printed.
# Removed redundant library() calls (already loaded by tidyverse):
#   - library(dplyr)
#   - library(tidyr)
#   - library(ggplot2)
# =============================================================================

# Set working directory to code_github root so all relative paths resolve.
setwd("~/Dropbox/spring2025experiment/code_github")

## [WEIGHT MODIFICATION] ======================================================
## Weight specification flag. Currently only "unweighted" is implemented.
##
## To add weighted variants in the future:
##   1. Join weights from ../data/Survey/individual_level_weights.csv onto
##      `final_choice` by Input.experiment_id (sample == "extension").
##   2. Replace unweighted `count()` with weighted `summarise(n = sum(wt))`
##      when computing `plot_data_ci$n` and `plot_data_ci$total`.
##   3. Replace Wilson CI (binom.confint) with a weighted CI -- e.g. design-
##      based via `survey::svyciprop`, or effective-sample-size Wilson with
##      n_eff = (sum wt)^2 / sum(wt^2). Wilson assumes iid binomial and is
##      not correct for weighted shares.
##   4. Add OUTPUT_SUFFIX to the ggsave filename, mirroring other survey
##      scripts (privacy_seeking_analysis.R, other_survey_regressions.R).
WEIGHT_SPEC   <- "unweighted"
OUTPUT_SUFFIX <- if (WEIGHT_SPEC == "unweighted") "" else paste0("_", WEIGHT_SPEC)
stopifnot(WEIGHT_SPEC == "unweighted")  # remove this guard when weighted is implemented
## =============================================================================

# Source utility scripts
source("replication_files/utils/plot_rules.R")

# Load required libraries
library(tidyverse)
library(binom)

# Output directory
FIGURES_DIR <- "output/figures/"

# -----------------------------------------------------------------------------
# Load and clean MTurk-coded freeform responses
# -----------------------------------------------------------------------------

dat <- read.csv("../data/Survey/freefrom_responses_coded.csv")
dat <- dat %>% filter(!is.na(Input.freeform))  # drop rows with no response

# Identify the four groups of answer columns (one per category).
a_cols <- grep("^Answer\\.a_", names(dat), value = TRUE)
b_cols <- grep("^Answer\\.b_", names(dat), value = TRUE)
c_cols <- grep("^Answer\\.c_", names(dat), value = TRUE)
d_cols <- grep("^Answer\\.d_", names(dat), value = TRUE)

# Convert "1" / blank to numeric 1 / 0.
to01 <- function(x) as.integer(x %in% c(1, "1", TRUE))
dat01 <- dat %>% mutate(across(all_of(c(a_cols, b_cols, c_cols, d_cols)), to01))

# Collapse each row to a single 0/1 per category (1 if any column in the group
# is checked by this rater).
dat_collapsed <- dat01 %>%
  mutate(
    BehaviorChange = as.integer(rowSums(across(all_of(a_cols)), na.rm = TRUE) > 0),
    Awareness      = as.integer(rowSums(across(all_of(b_cols)), na.rm = TRUE) > 0),
    Fatalism       = as.integer(rowSums(across(all_of(c_cols)), na.rm = TRUE) > 0),
    NoChange       = as.integer(rowSums(across(all_of(d_cols)), na.rm = TRUE) > 0)
  )

# Aggregate across raters within each experiment_id. Each cell is now the
# number of raters who flagged this participant in that category.
by_id <- dat_collapsed %>%
  group_by(Input.experiment_id, Input.experiment_condition) %>%
  summarise(
    BehaviorChange = sum(BehaviorChange, na.rm = TRUE),
    Awareness      = sum(Awareness, na.rm = TRUE),
    Fatalism       = sum(Fatalism, na.rm = TRUE),
    NoChange       = sum(NoChange, na.rm = TRUE),
    num_responses  = n(),
    Input.freeform = paste(unique(na.omit(Input.freeform)), collapse = " | "),
    .groups        = "drop"
  )

# -----------------------------------------------------------------------------
# Pick dominant category per participant, with researcher tiebreaking
# -----------------------------------------------------------------------------
# For each participant, take the category with the highest rater count. If a
# participant has a unique max, keep it. Tied participants (multiple categories
# at max) are resolved via the researcher tiebreaking file
# `freefrom_responses_coded_ties_manual.csv`.

final_choice <- by_id %>%
  pivot_longer(cols = BehaviorChange:NoChange,
               names_to = "Category", values_to = "Count") %>%
  group_by(Input.experiment_id) %>%
  filter(Count == max(Count, na.rm = TRUE)) %>%
  filter(n() == 1) %>%
  ungroup()

# Researcher-resolved ties (overrides any in-script tie computation).
ties <- read.csv("../data/Survey/freefrom_responses_coded_ties_manual.csv")

final_choice <- rbind(final_choice, ties)

# -----------------------------------------------------------------------------
# Compute per-condition shares + Wilson 95% CIs
# -----------------------------------------------------------------------------

plot_data_ci <- final_choice %>%
  count(Input.experiment_condition, Category) %>%
  group_by(Input.experiment_condition) %>%
  mutate(
    total = sum(n),
    prop  = n / total
  ) %>%
  rowwise() %>%
  mutate(
    ci_low  = binom.confint(n, total, methods = "wilson")$lower,
    ci_high = binom.confint(n, total, methods = "wilson")$upper
  ) %>%
  ungroup()

# Relabel and order conditions / categories to match paper.
plot_data_ci <- plot_data_ci %>%
  mutate(
    Input.experiment_condition = recode(
      Input.experiment_condition,
      "control"  = "Control",
      "info"     = "Information",
      "saliency" = "Saliency"
    ),
    Input.experiment_condition = factor(
      Input.experiment_condition,
      levels = TREATMENT_ORDER
    ),
    Category = recode(
      Category,
      "BehaviorChange" = "Behavior Change",
      "Awareness"      = "Awareness",
      "Fatalism"       = "Fatalism",
      "NoChange"       = "No Change"
    )
  )

# -----------------------------------------------------------------------------
# Fig D.1: per-condition shares with Wilson 95% CIs
# -----------------------------------------------------------------------------

g <- ggplot(plot_data_ci,
            aes(x = Category, y = prop, fill = Input.experiment_condition)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.85) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width    = ERRORBAR_WIDTH,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_treatment() +
  labs(
    x    = "Response Category",
    y    = "Percent of Participants",
    fill = "Condition"
  ) +
  theme_privacy_experiment(show_grid_x = FALSE) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(paste0(FIGURES_DIR, "freeform_change_responses", OUTPUT_SUFFIX, ".pdf"),
       g, width = 8, height = 6)