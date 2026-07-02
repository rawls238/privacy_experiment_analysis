setwd("~/Dropbox/spring2025experiment/code_github")
library(jsonlite); library(data.table); library(fst); library(fixest)
source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
select <- dplyr::select
BAD_USERS <- union(BAD_USERS, c("6ccc7d5","7d6864c"))

panel <- read_fst("../data/tracker_panel/panel_merged_CLEAN.fst", as.data.table=TRUE)
ec <- fread("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
ec_clean <- ec[in_experiment=="true" & !experiment_id %in% BAD_USERS]
ec_clean[wave_id==3, wave_id:=2L]
drop_cols <- intersect(c("wave_id","treatment","experiment_condition"), names(panel))
if(length(drop_cols)>0) panel[, (drop_cols):=NULL]
panel <- panel[experiment_id %in% ec_clean$experiment_id]
panel <- merge(panel, ec_clean[,.(experiment_id,wave_id,experiment_condition)], by="experiment_id", all.x=TRUE)
panel[wave_id==3, wave_id:=2L]
panel[, c1_anchor := fifelse(wave_id==1L, as.Date("2025-07-26"), as.Date("2025-08-09"))]
panel[, tau := as.integer(date - c1_anchor)]

site_df <- data.frame(website=unique(panel$website), stringsAsFactors=FALSE)
site_df <- aggregate_time_data(site_df, field="website")
site_df <- high_level_aggregate(site_df, field="website_aggregated")
site_lookup <- as.data.table(unique(site_df[,c("website","website_aggregated_high_level")]))
panel <- merge(panel, site_lookup, by="website", all.x=TRUE)
panel <- panel[!(tolower(website_aggregated_high_level) %in% SURVEY_WEBSITES)]

# checkpoint A: BEFORE category merge
t1_A <- panel[tau>=-7 & tau<=6 & !is.na(visit_count) & visit_count>0]
cat("A) rows in panel after SURVEY filter:", nrow(panel), "\n")
cat("A) t1 rows BEFORE category merge:", nrow(t1_A), "\n")

# category merge (the new step)
domain_class <- get_domain_classification(); setDT(domain_class)
dcs <- unique(domain_class[,.(website_agg=name_aggregated_high_level, category=category_level_1)])
cat("B) domain_class_slim rows:", nrow(dcs),
    " | unique website_agg:", uniqueN(dcs$website_agg),
    " | DUP?", nrow(dcs) != uniqueN(dcs$website_agg), "\n")

panel2 <- merge(panel, dcs, by.x="website_aggregated_high_level", by.y="website_agg", all.x=TRUE)
t1_B <- panel2[tau>=-7 & tau<=6 & !is.na(visit_count) & visit_count>0]
cat("C) panel rows AFTER category merge:", nrow(panel2), "\n")
cat("C) t1 rows AFTER category merge:", nrow(t1_B), "\n")