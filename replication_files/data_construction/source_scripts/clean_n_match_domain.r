# clean_tracker_109.R
# Based on clean_tracker.R with minimal path changes for full dataset
# Working directory: /yen/projects/faculty/sggold-privacy-policy/trakers

# -------------------------------------------------------------------------------------------------------
# 0. Load Libraries and Utils

library(dplyr)
library(data.table)
library(jsonlite)

source("utils/time_usage_helpers.R")

# -------------------------------------------------------------------------------------------------------
# Override functions with hardcoded paths (minimal change - only paths updated)

get_domain_classification_local <- function() {
  domain_classfication = rbind(
    read.csv("domain_classification_2.csv"), 
    read.csv("domain_classification_extra.csv")
  )
  twitter <- domain_classfication %>% filter(name == "twitter.com")
  twitter <- twitter %>% mutate(name = ifelse(name == "twitter.com", "x.com", name))
  domain_classfication <- rbind(domain_classfication, twitter)
  # handle double postfix
  domain_classfication$name <- sub("\\.gov\\.(au|in|uk)$", ".gov", domain_classfication$name)
  domain_classfication$name <- sub("\\.org\\.(au|in|uk)$", ".org", domain_classfication$name)
  domain_classfication$name <- sub("\\.co\\.(nz|uk|za|jp|th|id|au|kr|us|ec|ke)$", ".co", domain_classfication$name)
  domain_classfication$name <- sub("\\.com\\.(au|ar|br|bn|pa|sg|ec|mx|hk|my|cn|kw|tr|tw|gh)$", ".com", domain_classfication$name)
  # for duplicated websites, keep the newest record.
  domain_classfication = domain_classfication |>
    select(name, date, categories, category_length) |>
    arrange(name, desc(date)) |>
    group_by(name) |>
    slice(1) |>
    ungroup()
  return(domain_classfication)
}

get_privacy_info_wide_local <- function() {
  privacy_info <- fread("privacy_info.csv")
  privacy_info <- privacy_info %>% mutate(numeric_rating = ifelse(rating == "Yes", 1, 0))
  privacy_info <- privacy_info %>% group_by(html_key, domain) %>% summarise(provided_privacy = mean(numeric_rating)) %>% ungroup()
  privacy_info_wide <- privacy_info %>%
    pivot_wider(names_from = html_key, values_from = provided_privacy) %>%
    rename_with(~ paste0("p_", .), -domain)
  privacy_info_wide$domain = gsub('^(open\\.|en\\.|www?\\d*\\.)', '', privacy_info_wide$domain) # remove www.
  privacy_info_wide <- privacy_info_wide %>%
    mutate(domain = case_when(
      str_detect(tolower(domain), "twitter\\.com") ~ "x.com",
      TRUE ~ domain
    ))
  privacy_info_wide$domain <- gsub("betterhealth.vic.gov.au", "betterhealth.vic.gov", privacy_info_wide$domain) # gov.au -> gov
  privacy_info_wide$domain <- sub("\\.co\\.uk$", ".co", privacy_info_wide$domain) # co.uk -> .co
  privacy_info_wide <- high_level_aggregate(aggregate_time_data(privacy_info_wide, field="domain"), field="domain_aggregated")
  privacy_info_wide <- privacy_info_wide %>% group_by(domain_aggregated_high_level) %>% slice(1) %>% ungroup()
  return(privacy_info_wide)
}

# -------------------------------------------------------------------------------------------------------
# 1. Load the trackers dataset

cat("Loading trackers.csv...\n")
trackers_raw <- fread("trackers.csv")
cat("Loaded", nrow(trackers_raw), "rows\n")

# -------------------------------------------------------------------------------------------------------
# 2. Remove bad urls

trackers = trackers_raw |>
  filter(!grepl("translate.google|translate.googleapis", third_party_domain))
cat("After removing translate.google:", nrow(trackers), "rows\n")

# -------------------------------------------------------------------------------------------------------
# 3. Clean (aggregate) the domains

trackers_webs = trackers |>
  select(`domain`, `third_party_domain`, `child_domain`)

trackers_webs_aggregated = aggregate_time_data_trackers(trackers_webs)

trackers$domain_updated = trackers_webs_aggregated$domain_updated
trackers$third_party_domain_updated = trackers_webs_aggregated$third_party_domain_updated
trackers$child_domain_updated = trackers_webs_aggregated$child_domain_updated

# -------------------------------------------------------------------------------------------------------
# 4. Alignment ads domains, for example, "safeframe.googlesyndication" -> "google"

trackers_clean = trackers |>
  select(domain_updated, third_party_domain_updated, child_domain_updated)
trackers_clean$third_party_domain_updated = standardize_ad_domains_trackers(trackers_clean$third_party_domain_updated)
trackers_clean$child_domain_updated = standardize_ad_domains_trackers(trackers_clean$child_domain_updated)

# -------------------------------------------------------------------------------------------------------
# 5. Remove useless patterns

prefixes <- c(
  "csync\\.", "static\\.", "statics", "sync\\.", "track\\.", "gtrack\\.", "cdn\\.", 
  "cdn1\\.", "cdn2\\.", "tag\\.", "ag\\.", "t\\.", "l\\.", "p\\.", "s\\.", "a2\\.", 
  "s1\\.", "c1\\.", "c\\.", "q\\.", "us\\.", "rt\\.", "g\\.", "go\\.", "video\\.", 
  "creative\\.", "assets\\.", "serve\\.", "a1\\.", "vip\\.", "jobs\\.", "a\\.", 
  "cm\\.", "i\\.", "api\\.", "ads\\.", "ssl\\.", "tags\\.", "g\\.", "a\\.", "sc\\.", 
  "pxl\\.", "sp\\."
)
prefix_pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")(.*)")
remove_prefixes <- function(url) {
  sub(prefix_pattern, "\\2", url)
}
# domains don't need to do that
trackers_clean$third_party_domain_updated = remove_prefixes(trackers_clean$third_party_domain_updated)
trackers_clean$child_domain_updated = remove_prefixes(trackers_clean$child_domain_updated)

postfix <- c("vip")
postfix_pattern <- paste0("\\.(", paste(postfix, collapse = "|"), ")$")
trackers_clean$third_party_domain_updated <- sub(postfix_pattern, "", trackers_clean$third_party_domain_updated)
trackers_clean$child_domain_updated <- sub(postfix_pattern, "", trackers_clean$child_domain_updated)
trackers_clean$domain_updated <- sub(postfix_pattern, "", trackers_clean$domain_updated)

# push to trackers from trackers_clean
trackers$third_party_domain_updated = trackers_clean$third_party_domain_updated
trackers$child_domain_updated = trackers_clean$child_domain_updated
trackers$domain_updated = trackers_clean$domain_updated

# -------------------------------------------------------------------------------------------------------
# 6. High level aggregations for privacy mapping and analysis

trackers$domain_updated_high_level = high_level_aggregate_trackers(trackers$domain_updated)
trackers$child_domain_updated_high_level = high_level_aggregate_trackers(trackers$child_domain_updated)
trackers$third_party_domain_updated_high_level = high_level_aggregate_trackers(trackers$third_party_domain_updated)

# -------------------------------------------------------------------------------------------------------
# 7. Re-order the variable name to make it more readable

trackers <- trackers %>%
  select(id, 
         domain, 
         domain_updated, 
         domain_updated_high_level, 
         third_party_domain, 
         third_party_domain_updated, 
         third_party_domain_updated_high_level,
         child_domain, 
         child_domain_updated, 
         child_domain_updated_high_level, 
         tstamp_first, 
         tstamp_last)

cat("Writing tracker_cleaned.csv...\n")
write.csv(trackers, "tracker_cleaned.csv", row.names = FALSE)
cat("Saved tracker_cleaned.csv with", nrow(trackers), "rows\n")

# -------------------------------------------------------------------------------------------------------
# 8. Map Domain Classification

cat("Loading domain classification...\n")
domain_classfication = get_domain_classification_local()
cat("Loaded", nrow(domain_classfication), "domain classifications\n")

# it's kinda slow
cat("Mapping domain classification (this may take a while)...\n")
trackers_domain_class = map_domain_trackers(trackers, domain_classfication)

# -------------------------------------------------------------------------------------------------------
# 9. Map Privacy Information

cat("Loading privacy info...\n")
privacy_info <- get_privacy_info_wide_local()
privacy_info = privacy_info %>% rename(domain_privacy = domain)
cat("Loaded privacy info for", nrow(privacy_info), "domains\n")

cat("Mapping privacy data...\n")
trackers_domain_class_privacy = map_privacy_data_trackers(trackers_domain_class, privacy_info)

# -------------------------------------------------------------------------------------------------------
# 10. Save final output

cat("Writing tracker_cleaned_final.csv...\n")
write.csv(trackers_domain_class_privacy, "tracker_cleaned_final.csv", row.names = FALSE)
cat("Done! Saved tracker_cleaned_final.csv with", nrow(trackers_domain_class_privacy), "rows\n")

# -------------------------------------------------------------------------------------------------------
# Summary statistics

cat("\n=== Summary ===\n")
cat("Input rows:", nrow(trackers_raw), "\n")
cat("Output rows:", nrow(trackers_domain_class_privacy), "\n")
cat("Unique domain:", length(unique(trackers$domain_updated_high_level)), "\n")
cat("Unique third_party_domain:", length(unique(trackers$third_party_domain_updated_high_level)), "\n")