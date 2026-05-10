# ============================================================================
# PRIVACY CHARACTERISTICS BY WEBSITE CATEGORY — Figure 4
# ============================================================================
#
# Produces:
#   Main paper [fig:scores_by_category, "Privacy Practices across Website Categories"]:
#     Fig 4: domain_scores_box_plot.pdf
#
# Box plots showing the distribution of privacy practice scores
# (control / use / collect) across website categories.
#
# Inputs:
#   ../data/final_extension_data/privacy_info.csv
#   ../auxiliary_data/domain_classification_2.csv
#   ../auxiliary_data/domain_classification_extra.csv
#
# Dependencies:
#   replication_files/utils/plot_rules.R
#   replication_files/utils/time_usage_helpers.R
#
# Outputs:
#   output/figures/domain_scores_box_plot.pdf
#
# Note: Previous version of this script also produced (now removed as dead code):
#   - correlation_by_category/correlation_<cat>.pdf (correlation matrix per category)
#   - val_mean_plot.pdf (privacy field mean disclosure)
#   - tab:readability_regression (texreg console output, never saved to .tex)
#   - 4 readability regressions via stargazer (Flesch / Gunning Fog) — console only
#   - plot_domain_scores_mean_variance() function — defined but never called
# Removed paper-unused inputs:
#   - data/privacy_info_desc.csv (only used by removed plot_correlation_by_category)
#   - data/policy_complexity_scores_old.csv (only used by removed readability regressions)
# ============================================================================

library(tidyverse)
library(haven)
library(forcats)
library(jsonlite) 
summarize <- dplyr::summarize

# Set working directory to code_github root so all relative paths resolve.
setwd("~/Dropbox/spring2025experiment/code_github")

# Source unified style rules and domain classification helpers
source("replication_files/utils/plot_rules.R")
source("replication_files/utils/time_usage_helpers.R")

# ============================================================================
# DATA LOADING
# ============================================================================

# Privacy practice ratings per (domain, html_key)
df.domain <- read.csv("../data/final_extension_data/privacy_info.csv")

# ============================================================================
# CATEGORIZE PRIVACY ATTRIBUTES (control / use / collect)
# ============================================================================

html_key_to_values <- c(
  "anonymized-anonymized" = "control",
  "sell-sell" = "control",
  "automated-automated" = "control",
  "change-change" = "control",
  "deletion-deletion" = "control",
  "personalization-personalization" = "use",
  "collect-bio" = "collect",
  "collect-location" = "collect",
  "collect-financial" = "collect",
  "collect-sensitive" = "collect",
  "share-finance" = "use",
  "storage-storage" = "control",
  "collect-log" = "collect",
  "collect-offsite" = "collect",
  "share-ads" = "use",
  "share-law" = "use",
  "share-partners" = "use",
  "share-social" = "use",
  "share-service" = "use",
  "collect-social" = "collect"
)

keys <- names(html_key_to_values)
vals <- unname(html_key_to_values)
idx  <- match(df.domain$html_key, keys)
df.domain$privacy_category <- vals[idx]

df.domain <- df.domain %>%
  mutate(val_mean = as.numeric(rating == "Yes"),
         value    = val_mean)

# ============================================================================
# DOMAIN NAME CLEANUP (manual fixes for join with domain_classification)
# ============================================================================

df.domain <- df.domain %>%
  mutate(domain = case_when(
    domain == "www.aol.com"        ~ "aol.com",
    domain == "kidshealth.org"     ~ "kidshealth.com",
    domain == "wiktionary.org"     ~ "wiktionary.com",
    domain == "www.deviantart.com" ~ "deviantart.com",
    domain == "www.nhs.uk"         ~ "nhs",
    TRUE ~ domain
  ))

# ============================================================================
# JOIN DOMAIN CLASSIFICATION (Klazify-style website categories)
# ============================================================================

domain_classification <- rbind(
  read.csv("../auxiliary_data/domain_classification_2.csv"),
  read.csv("../auxiliary_data/domain_classification_extra.csv")
)
domain_classification <- domain_classification %>%
  group_by(name) %>%
  slice(1)

df.domain <- map_domain_3(df.domain, domain_classification)
df.domain <- df.domain %>% mutate(category = category_level_1)

# ============================================================================
# Fig 4: BOX PLOT OF PRIVACY PRACTICE SCORES BY WEBSITE CATEGORY
# ============================================================================

plot_domain_scores <- function(
    data,
    min_websites = 10,
    output_file  = NULL,
    width        = 12,
    height       = 6
) {
  # Compute domain-level scores by privacy_category
  domain_score <- data %>%
    group_by(category, privacy_category, domain) %>%
    summarise(score = sum(val_mean) / n_distinct(html_key), .groups = "drop") %>%
    filter(!is.na(category))
  
  # Filter out category/privacy combos with too few domains
  domain_score <- domain_score %>%
    add_count(category, privacy_category, name = "n_domains") %>%
    filter(n_domains > min_websites)
  
  # Set factor order: control -> use -> collect
  domain_score$privacy_category <- factor(
    domain_score$privacy_category,
    levels = PRIVACY_CATEGORY_ORDER
  )
  
  # Determine category ordering by median score across all privacy categories
  category_order <- domain_score %>%
    group_by(category) %>%
    summarise(median_score = median(score, na.rm = TRUE)) %>%
    arrange(median_score) %>%
    pull(category)
  domain_score$category <- factor(domain_score$category, levels = category_order)
  
  # Build the boxplot with plot_rules styling
  p <- ggplot(domain_score, aes(x = category, y = score, fill = privacy_category)) +
    geom_boxplot(
      position      = position_dodge(width = 0.8),
      outlier.shape = NA,
      width         = 0.7,
      linewidth     = 0.3
    ) +
    scale_x_discrete(expand = expansion(add = c(1.5, 1.5))) +
    scale_fill_privacy_category(
      name   = "Privacy Category",
      labels = PRIVACY_CATEGORY_LABELS
    ) +
    labs(
      x = "Website Category",
      y = "Fraction Engaging in Practice"
    ) +
    theme_privacy_experiment(base_size = 14, legend_position = "bottom") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  if (is.null(output_file)) {
    output_file <- "domain_scores_box_plot.pdf"
  }
  
  full_path <- file.path("output/figures", output_file)
  ggsave(
    filename = full_path,
    plot     = p,
    width    = width,
    height   = height
  )
  
  cat(sprintf("Saved: %s\n", full_path))
  return(p)
}

# ============================================================================
# RUN
# ============================================================================

plot_domain_scores(df.domain, output_file = "domain_scores_box_plot.pdf")