# PLOT THE FIGURE 1 - 5.
#####
library(tidyverse)
library(haven)
library(Hmisc) # for Lag command
library(tidyr)
library(ggplot2)
library(rjson)
library(purrr)
library(forcats)
library(jsonlite)
library(dplyr)
library(purrr)
library(stargazer)
library(lfe)




summarize <- dplyr::summarize
#####load data #### 
# change as needed 
#filepath <- '/Users/sggold/Library/CloudStorage/Dropbox/Shared-Project-Folders/Privacy-Experiment/pilots/Summer 2024 Pilot/'
#setwd(paste0(filepath,'./code/analysis/outputs_2/')) # Changed output directory to outputs_2


filepath <- "/Users/markli/Library/CloudStorage/Dropbox/descriptive_analysis_of_policies/"


#filepath = "/Users/markli/Library/CloudStorage/Dropbox/descriptive_analysis_of_policies/"
#df.domain <- read.csv(paste0(filepath,'data/policy_data_full_0217.csv'))
#df.domain <- df.domain %>% rename(privacy_category = category)
df.domain = read.csv("/Users/markli/Library/CloudStorage/Dropbox/spring2025experiment/data/final_extension_data/privacy_info.csv")
privacy_info <- read.csv(paste0(filepath, 'data/privacy_info_desc.csv'))
privacy_info <- privacy_info %>% mutate(html_key = paste0(tolower(feature), "-", tolower(field)))
setwd(paste0(filepath, "figures/"))

# Source plot_rules for consistent styling
source("/Users/markli/Library/CloudStorage/Dropbox/spring2025experiment/code/utils/plot_rules.R")

#privacy_label_mappings <- read.csv(paste0(filepath,'data/cleaned_privacy_label_mappings.csv'))
#df.domain <- df.domain %>% mutate(html_key = paste0(tolower(feature), "-", tolower(field)))
#df.domain <- df.domain %>% left_join(privacy_label_mappings, by="html_key")
#bad_keys <- c("collection-collection", "data-collect", "sharing-sharing", "purpose-purpose")
#df.domain <- df.domain %>% filter(!(html_key %in% bad_keys))
#df.domain <- df.domain %>% filter(html_key %in% unique(privacy_info_exp$html_key))


#privacy_info <- privacy_info %>% mutate(html_key = paste0(tolower(feature), "-", tolower(field)))




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
df.domain <- df.domain %>% mutate(val_mean = as.numeric(rating == "Yes"))
#df.domain <- df.domain %>% mutate(val_mean = ifelse(privacy_category == "Control", val_mean, 1 - val_mean)) 
df.domain <- df.domain %>% mutate(value = val_mean)
# Mark: join on the df.domain {top domain category, location, and number of employees}
df.domain <- df.domain %>% mutate(domain = ifelse(domain == "www.aol.com", "aol.com", domain))
df.domain <- df.domain %>% mutate(domain = ifelse(domain == "kidshealth.org", "kidshealth.com", domain))
df.domain <- df.domain %>% mutate(domain = ifelse(domain == "wiktionary.org", "wiktionary.com", domain))
df.domain <- df.domain %>% mutate(domain = ifelse(domain == "www.deviantart.com", "deviantart.com", domain))
df.domain <- df.domain %>% mutate(domain = ifelse(domain == "www.nhs.uk", "nhs", domain))


safe_fromJSON <- safely(fromJSON)
source(paste0(filepath,"utils/time_usage_helpers.R"))
domain_classification = rbind(read.csv(paste0(filepath,'data/domain_classification_2.csv')), read.csv(paste0(filepath,'data/domain_classification_extra.csv')))
domain_classification <- domain_classification %>% group_by(name) %>% slice(1)
df.domain = map_domain_3(df.domain, domain_classification)
df.domain = df.domain %>% mutate(category = category_level_1)
# readr::write_csv(domain_n_classification, "domain_n_classification.csv")


policy_complexity_scores <- read.csv(paste0(filepath, "data/policy_complexity_scores_old.csv"))
policy_complexity_scores |>
  filter(Perplexity <= 30)
policy_complexity_scores <- policy_complexity_scores %>% group_by(domain) %>% slice(1)
policy_complexity_scores <- policy_complexity_scores %>% rename(domain_cleaned = domain)
df.domain_with_scores <- df.domain %>% mutate(domain_cleaned = sub("\\..*$", "", domain)) %>%
  left_join(
    policy_complexity_scores,
    by = "domain_cleaned"
  )


get_domain_score <- function(df) {
  domain_score <- df %>%
    group_by(category_level_1, domain) %>%
    summarise(
      score          = mean(val_mean, na.rm = TRUE),
      revenue        = first(revenue),
      employeesRange = first(employeesRange),
      countryCode    = first(countryCode),
      stateCode      = first(stateCode),
      .groups        = "drop"
    ) %>%
    mutate(score = (score - min(score, na.rm = TRUE)) / 
             (max(score, na.rm = TRUE) - min(score, na.rm = TRUE)))
  return(domain_score)
}




disagg_reg_flesch <- felm(val_mean ~ Flesch.Reading.Ease.Score | 0 | 0 | domain, data = df.domain_with_scores %>% filter(!is.na(Flesch.Reading.Ease.Score)))
disagg_reg_gunning <- felm(val_mean ~ Gunning.Fog.Index | 0 | 0 | domain, data = df.domain_with_scores %>% filter(!is.na(Gunning.Fog.Index)))
disagg_reg_flesch_by_category <- felm(val_mean ~ Flesch.Reading.Ease.Score + privacy_category + Flesch.Reading.Ease.Score:privacy_category | 0 | 0 | domain, data = df.domain_with_scores %>% filter(!is.na(Flesch.Reading.Ease.Score)))
disagg_reg_gunning_by_category <- felm(val_mean ~ Gunning.Fog.Index + privacy_category + Gunning.Fog.Index:privacy_category | 0 | 0 | domain, data = df.domain_with_scores %>% filter(!is.na(Gunning.Fog.Index)))
library(texreg)
texreg(
  list(
    disagg_reg_flesch,
    disagg_reg_gunning
  ),
  label = "tab:readability_regression",  # LaTeX label
  caption = "Regression of Valence on Readability Scores and Privacy Categories",
  stars = c(0.01, 0.05, 0.1),  # significance stars
  digits = 3  # number of decimal places
)


domain_score <- get_domain_score(df.domain_with_scores) %>% group_by(domain) %>% slice(1) %>% ungroup() %>% select(domain, score)
df.domain_with_overall_score <- df.domain_with_scores %>% group_by(domain) %>% summarise(Flesch.Reading.Ease.Score = first(Flesch.Reading.Ease.Score), Gunning.Fog.Index = first(Gunning.Fog.Index), .groups='drop') %>% left_join(domain_score, by="domain")


agg_reg_flesch <- felm(score ~ Flesch.Reading.Ease.Score | 0 | 0 | 0, data =df.domain_with_overall_score)
agg_reg_gunning <- felm(score ~ Gunning.Fog.Index | 0 | 0 | 0, data = df.domain_with_overall_score)


stargazer(disagg_reg_flesch, disagg_reg_gunning, agg_reg_flesch, agg_reg_gunning)




plot_domain_scores_mean_variance <- function(
    data,
    privacy_cat = NULL,
    min_websites = 8,
    output_file = NULL,
    width = 10,
    height = 6
) {
  # Optionally filter by privacy_category
  df <- data
  if (!is.null(privacy_cat)) {
    
    df <- df %>% filter(privacy_category == privacy_cat)
  }
  
  # Compute domain-level scores
  domain_score <- get_domain_score(df)
  
  # Aggregate to category level
  category_score <- domain_score %>%
    group_by(category_level_1) %>%
    summarise(
      num_websites = n_distinct(domain),
      mean_score   = mean(score),
      sd_score    = sd(score),
      .groups      = "drop"
    ) %>%
    filter(category_level_1 != "", num_websites > min_websites) %>%
    mutate(category_level_1 = fct_reorder(category_level_1, mean_score))
  
  # Compute scaling factor for secondary axis
  factor <- max(category_score$mean_score) / max(category_score$sd_score)
  
  # Build the plot
  p <- ggplot(category_score, aes(x = category_level_1)) +
    geom_col(aes(y = mean_score, fill = "Mean"), width = 0.7) +
    geom_line(aes(y = sd_score * factor, color = "Standard Deviation", group = 1), size = 1.2) +
    geom_point(aes(y = sd_score * factor, color = "Standard Deviation"), size = 2) +
    scale_y_continuous(
      name     = "Mean score",
      sec.axis = sec_axis(~ . / factor, name = "Standard Deviation")
    ) +
    scale_fill_manual("", values = c(Mean = "steelblue")) +
    scale_color_manual("", values = c(`Standard Deviation` = "firebrick")) +
    labs(
      x = ifelse(is.null(privacy_cat),
                 "Website Category",
                 paste0("Website Category (privacy_category: ", privacy_cat, ")"))
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  # Determine output file name if not provided
  if (is.null(output_file)) {
    base_name <- if (is.null(privacy_cat)) {
      "domain_scores_all"
    } else {
      safe_name <- gsub("[^A-Za-z0-9]+", "_", privacy_cat)
      paste0("domain_scores_", safe_name)
    }
    output_file <- paste0(base_name, ".pdf")
  }
  
  # Full path for saving
  full_path <- file.path(output_file)
  print(p)
  # Save the plot to the figures folder
  ggsave(
    filename = full_path,
    plot     = p,
    width    = width,
    height   = height
  )
  
  return(p)
}




plot_privacy_fields <- function(
    df,
    variable = "val_mean",
    y_label = "Mean Value",
    output_file = "privacy_field_plot.pdf",
    width = 10,
    height = 6
) {
  var_sym <- sym(variable)
  
  # Step 1: Aggregate the variable across domain
  means_df <- df %>%
    filter(!is.na(privacy_category)) %>%
    group_by(privacy_category, domain, field, html_key) %>%
    summarise(val = mean(!!var_sym, na.rm = TRUE), .groups = "drop") %>%
    group_by(privacy_category, field, html_key) %>%
    summarise(mean_value = mean(val, na.rm = TRUE), .groups = "drop")
  
  # Step 2: Join short_desc and filter
  labeled_df <- means_df %>%
    left_join(privacy_info %>% distinct(html_key, short_desc), by = "html_key") %>%
    #filter(!is.na(short_desc) & !(html_key %in% exclude_keys)) %>%
    mutate(label = short_desc)
  
  # Step 3: Facet-specific reordering
  labeled_df <- labeled_df %>%
    group_by(privacy_category) %>%
    mutate(label = fct_reorder(label, mean_value)) %>%
    ungroup()
  
  # Step 4: Create plot with empty title and custom y-axis label
  p <- ggplot(labeled_df, aes(x = label, y = mean_value)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    facet_wrap(~privacy_category, scales = "free_y") +
    labs(
      y = y_label,
      title = ""
    ) +
    theme_minimal()
  
  # Step 5: Save and return plot
  ggsave(output_file, plot = p, width = width, height = height)
  return(p)
}




plot_correlation_by_category <- function(
    df,
    value_col = "val_mean",
    output_dir = "correlation_plots_by_category",
    privacy_labels = privacy_info,
    width = 8,
    height = 8
) {
  dir.create(output_dir, showWarnings = FALSE)
  
  var_value <- sym(value_col)
  
  # Get unique categories
  categories <- df %>%
    filter(!is.na(privacy_category)) %>%
    distinct(privacy_category) %>%
    pull()
  
  for (cat in categories) {
    message("Processing category: ", cat)
    
    # Step 1: Filter (df.domain already has short_desc)
    corr_data <- df %>%
      filter(privacy_category == cat) %>%
      filter(!is.na(short_desc)) %>%
      select(domain, short_desc, value = !!var_value) %>%
      group_by(domain, short_desc) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = short_desc, values_from = value) %>%
      filter(rowSums(!is.na(across(where(is.numeric)))) > 1)  # retain only domains with multiple non-NA fields
    
    if (nrow(corr_data) < 2) next  # skip if not enough data
    
    # Step 2: Correlation matrix
    corr_matrix <- corr_data %>%
      select(-domain) %>%
      cor(use = "pairwise.complete.obs") %>%
      as.data.frame()
    
    corr_df <- corr_matrix %>%
      rownames_to_column("row") %>%
      pivot_longer(cols = -row, names_to = "col", values_to = "correlation") %>%
      mutate(correlation = replace_na(correlation, 0))
    
    # Step 3: Plot
    p <- ggplot(corr_df, aes(x = row, y = col, fill = correlation)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradientn(
        colors = c("white", "#E0F0FF", "#B0D8FF", "#6AB8FF", "#1F90FF", "#0072B2", "#00558C", "#003A66"),
        limits = c(0, 1),
        breaks = c(0, 0.25, 0.5, 0.75, 1.0),
        name = "Correlation",
        na.value = "white"
      ) +
      coord_equal() +
      labs(title = NULL, x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold", hjust = 1),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.5, "cm"),
        panel.grid = element_blank(),
        plot.margin = margin(15, 15, 15, 15)
      ) +
      guides(fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 15,
        barheight = 1,
        frame.colour = "black",
        ticks.colour = "black",
        direction = "horizontal"
      ))
    
    # Step 4: Save
    print(p)
    output_file <- file.path(output_dir, paste0("correlation_", cat, ".pdf"))
    ggsave(output_file, plot = p, width = width, height = height)
  }
}


plot_correlation_by_category(
  df = df.domain,
  value_col = "val_mean",
  output_dir = "correlation_by_category",
  privacy_labels = privacy_info
)


plot_privacy_fields(df.domain, variable = "val_mean", y_label = "Mean Disclosure", output_file = "val_mean_plot.pdf")
# plot_privacy_fields(df.domain, variable = "avg_cert", y_label = "Mean Certainty", output_file = "certainty_plot.png")
# Calculate correlation matrix and reshape for plotting


plot_domain_scores <- function(
    data,
    min_websites = 10,
    output_file = NULL,
    width = 12,
    height = 6
) {
  # Compute domain-level scores by privacy_category
  domain_score <- data %>% 
    group_by(category, privacy_category, domain) %>%
    summarise(score = sum(val_mean) / n_distinct(html_key), .groups='drop') %>%
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
      y = "Fraction Engaging in Practice",
    ) +
    theme_privacy_experiment(base_size = 14, legend_position = "bottom") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Auto-generate output filename if not provided
  if (is.null(output_file)) {
    output_file <- "domain_scores_box_plot.pdf"
  }
  
  # Save plot
  ggsave(
    filename = file.path(output_file),
    plot     = p,
    width    = width,
    height   = height
  )
  
  return(p)
}


plot_domain_scores(df.domain, output_file="domain_scores_box_plot.pdf")