library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(binom)

#setwd("/Users/guyaridor/Dropbox/Privacy-Experiment/spring2025experiment")
setwd("/Users/markli/Library/CloudStorage/Dropbox/spring2025experiment")
dat <- read.csv("data/Survey/freefrom_responses_coded.csv")
dat <- dat %>% filter(!is.na(Input.freeform)) ## means no response
FIGURES_DIR <- "results/baseline_survey_descriptives/"

source("code/utils/plot_rules.R")

# --- Identify groups of answer columns
a_cols <- grep("^Answer\\.a_", names(dat), value = TRUE)
b_cols <- grep("^Answer\\.b_", names(dat), value = TRUE)
c_cols <- grep("^Answer\\.c_", names(dat), value = TRUE)
d_cols <- grep("^Answer\\.d_", names(dat), value = TRUE)

# --- Convert "1"/blank → numeric 1/0
to01 <- function(x) as.integer(x %in% c(1, "1", TRUE))
dat01 <- dat %>% mutate(across(all_of(c(a_cols, b_cols, c_cols, d_cols)), to01))

# --- Collapse each row (1 if any in the group is checked)
dat_collapsed <- dat01 %>%
  mutate(
    BehaviorChange = as.integer(rowSums(across(all_of(a_cols)), na.rm = TRUE) > 0),
    Awareness      = as.integer(rowSums(across(all_of(b_cols)), na.rm = TRUE) > 0),
    Fatalism       = as.integer(rowSums(across(all_of(c_cols)), na.rm = TRUE) > 0),
    NoChange       = as.integer(rowSums(across(all_of(d_cols)), na.rm = TRUE) > 0)
  )

# --- Aggregate within each experiment_id (pick dominant category)
by_id <- dat_collapsed %>%
  group_by(Input.experiment_id, Input.experiment_condition) %>%
  summarise(
    BehaviorChange = sum(BehaviorChange, na.rm = TRUE),
    Awareness      = sum(Awareness, na.rm = TRUE),
    Fatalism       = sum(Fatalism, na.rm = TRUE),
    NoChange       = sum(NoChange, na.rm = TRUE),
    num_responses = n(),
    Input.freeform = paste(unique(na.omit(Input.freeform)), collapse = " | "),
    .groups = "drop"
  )

# pivot, drop ties, and retain Input.freeform
final_choice <- by_id %>%
  pivot_longer(cols = BehaviorChange:NoChange,
               names_to = "Category", values_to = "Count") %>%
  group_by(Input.experiment_id) %>%
  filter(Count == max(Count, na.rm = TRUE)) %>% 
  filter(n() == 1) %>% ungroup()

ties <- by_id %>%
  pivot_longer(cols = BehaviorChange:NoChange,
               names_to = "Category", values_to = "Count") %>%
  group_by(Input.experiment_id) %>%
  filter(Count == max(Count, na.rm = TRUE)) %>% 
  filter(n() > 1) %>% ungroup()

ties <- read.csv("data/Survey/freefrom_responses_coded_ties_manual.csv")

final_choice <- rbind(final_choice, ties)
final_choice %>% group_by(Input.experiment_id) %>% summarise(a = n())

# --- Aggregate across experiment conditions, compute shares + CI
plot_data_ci <- final_choice %>%
  count(Input.experiment_condition, Category) %>%
  group_by(Input.experiment_condition) %>%
  mutate(
    total = sum(n),
    prop = n / total
  ) %>%
  rowwise() %>%
  mutate(
    ci_low  = binom.confint(n, total, methods = "wilson")$lower,
    ci_high = binom.confint(n, total, methods = "wilson")$upper
  ) %>%
  ungroup()

# --- Relabel and order conditions
plot_data_ci <- plot_data_ci %>%
  mutate(
    Input.experiment_condition = recode(
      Input.experiment_condition,
      "control" = "Control",
      "info" = "Information",
      "saliency" = "Saliency"
    ),
    Input.experiment_condition = factor(
      Input.experiment_condition,
      levels = TREATMENT_ORDER
    )
  )

# --- Plot with CI bars

plot_data_ci <- plot_data_ci %>%
  mutate(Category = recode(Category,
                           "BehaviorChange" = "Behavior Change",
                           "Awareness"      = "Awareness",
                           "Fatalism"       = "Fatalism",
                           "NoChange"       = "No Change"
  ))
g <- ggplot(plot_data_ci,
            aes(x = Category, y = prop, fill = Input.experiment_condition)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.85) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = ERRORBAR_WIDTH,
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_treatment() +
  labs(
    x = "Response Category",
    y = "Percent of Participants",
    fill = "Condition"
  ) +
  theme_privacy_experiment(show_grid_x = FALSE) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )


ggsave(paste0(FIGURES_DIR, "freeform_change_responses.pdf"), g, width = 8, height = 6)