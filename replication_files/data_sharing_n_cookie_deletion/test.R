# =============================================================================
# test_appendix_checks.R   (THROWAWAY diagnostic -- do NOT commit)
#
# Two checks requested before touching Overleaf. Writes NOTHING to disk.
#
#   Part A: data_sharing_3 has no is.na() guard (unlike _2 / _4). Does this
#           change Column (3) "Withheld data" and the NEW Column (5)
#           "Any Behavior" in Table C.9?  Compares current vs guarded.
#   Part C: are the 4 category figures + baseline all drawing the SAME set of
#           categories as the shared big_cats target, and does cat_dt (which
#           drives \nCpvCategories) match?  Reports any category dropped in any
#           subsample, plus whether any "significant" CPV category is positive
#           (prose claims "significant negative").
#
# Run:  Rscript ~/Downloads/test_appendix_checks.R 2>&1 | tee /tmp/test_out.txt
# (setwd below makes paths resolve regardless of where this file lives.)
# =============================================================================

setwd("~/Dropbox/spring2025experiment/code_github")

# =============================================================================
# PART A -- data_sharing_3 NA asymmetry (Table C.9, columns 3 and 5)
#           Mirrors other_survey_regressions.R exactly (unweighted pass).
# =============================================================================
suppressMessages({ library(tidyverse); library(fixest) })

cat("\n", strrep("=", 70),
    "\nPART A: data_sharing_3 NA guard -> Column (3) 'Withheld' and (5) 'Any'\n",
    strrep("=", 70), "\n", sep = "")

survey_merged <- read.csv("../data/Survey/survey_merged_final.csv",
                          stringsAsFactors = FALSE)
experiment_user_info <- read.csv(
  "../data/final_extension_data/experiment_conditions_pilot_july_2024.csv",
  stringsAsFactors = FALSE)

survey_merged <- survey_merged %>%
  left_join(
    experiment_user_info %>%
      select(email, experiment_id_aux = experiment_id, experiment_condition,
             wave_id, block_idx),
    by = c("emailid" = "email"))

ds <- survey_merged %>%
  filter(completed_both == TRUE & !is.na(data_sharing_1) &
           !is.na(experiment_condition) & experiment_condition != "")

cat(sprintf("Analysis sample after filter: %d participants\n\n", nrow(ds)))

cat("NA counts in the four raw data_sharing columns (post-filter):\n")
for (v in paste0("data_sharing_", 1:4)) {
  cat(sprintf("  %-16s %d NA / %d rows\n", v, sum(is.na(ds[[v]])), nrow(ds)))
}
cat("  (_1 is covered by the filter; _2/_4 are NA-guarded in the script;\n",
    "   _3 is the unguarded one under test.)\n", sep = "")

build <- function(df, guard3) {
  df %>% mutate(
    b1 = ifelse(data_sharing_1 != "No", 1, 0),
    b2 = ifelse(data_sharing_2 != "No" & !is.na(data_sharing_2), 1, 0),
    b3 = if (guard3) ifelse(data_sharing_3 != "No" & !is.na(data_sharing_3), 1, 0)
    else        ifelse(data_sharing_3 != "No", 1, 0),
    b4 = ifelse(data_sharing_4 != "No" & !is.na(data_sharing_4), 1, 0),
    total_shared = b1 + b2 + b3 + b4,
    any_shared   = ifelse(total_shared > 0, 1, 0),
    experiment_condition = factor(experiment_condition,
                                  levels = c("control", "saliency", "info")),
    wave_id       = ifelse(wave_id == 3, 2, wave_id),
    block_by_wave = paste(wave_id, block_idx, sep = "_")) %>%
    rename(experiment_id = experiment_id_aux)
}

cur <- build(ds, guard3 = FALSE)   # current script behavior
fix <- build(ds, guard3 = TRUE)    # proposed fix: guard _3 like _2 / _4

cat(sprintf("\nany_shared NA count  ->  current: %d    fixed: %d\n",
            sum(is.na(cur$any_shared)), sum(is.na(fix$any_shared))))

fit <- function(df, y) {
  feols(as.formula(paste0(y, " ~ experiment_condition | block_by_wave")),
        data = df, cluster = ~experiment_id, notes = FALSE)
}

report <- function(label, y) {
  cat(sprintf("\n%s\n", label))
  for (nm in c("current", "fixed")) {
    d <- if (nm == "current") cur else fix
    m <- fit(d, y)
    cat(sprintf("  %-8s  N = %-5d  info = %+.4f (se %.4f)   saliency = %+.4f (se %.4f)\n",
                nm, nobs(m),
                coef(m)["experiment_conditioninfo"],     se(m)["experiment_conditioninfo"],
                coef(m)["experiment_conditionsaliency"], se(m)["experiment_conditionsaliency"]))
  }
}

report("Column (3) 'Withheld data':", "b3")
report("Column (5) 'Any Behavior' (NEW column):", "any_shared")

cat("\n-> Identical N & coefs across current/fixed => missing guard is cosmetic\n",
    "   (data_sharing_3 has no NAs). Different => it moves the paper numbers and\n",
    "   _3 needs the guard.\n", sep = "")


# =============================================================================
# PART C -- category-count consistency across the 4 category figures
#           Mirrors cookie_deletion.R Section 0 data prep + the two fit helpers.
# =============================================================================
suppressMessages({ library(jsonlite); library(data.table); library(fst) })
source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/number_format_helpers.R")
select <- dplyr::select   # prevent data.table masking inside get_domain_classification

cat("\n\n", strrep("=", 70),
    "\nPART C: category-count consistency (big_cats vs each figure)\n",
    strrep("=", 70), "\n", sep = "")

BAD_USERS <- union(BAD_USERS, c("6ccc7d5", "7d6864c"))
TAU_MIN <- -7; TAU_MAX <- 6

panel <- read_fst("../data/tracker_panel/panel_merged_CLEAN.fst", as.data.table = TRUE)
ec <- fread("../data/final_extension_data/experiment_conditions_pilot_july_2024.csv")
ec_clean <- ec[in_experiment == "true" & !experiment_id %in% BAD_USERS]
ec_clean[wave_id == 3, wave_id := 2L]

drop_cols <- intersect(c("wave_id", "treatment", "experiment_condition"), names(panel))
if (length(drop_cols) > 0) panel[, (drop_cols) := NULL]
panel <- panel[experiment_id %in% ec_clean$experiment_id]
panel <- merge(panel, ec_clean[, .(experiment_id, wave_id, experiment_condition)],
               by = "experiment_id", all.x = TRUE)
panel[wave_id == 3, wave_id := 2L]
panel[, c1_anchor := fifelse(wave_id == 1L, as.Date("2025-07-26"), as.Date("2025-08-09"))]
panel[, tau := as.integer(date - c1_anchor)]

site_df <- data.frame(website = unique(panel$website), stringsAsFactors = FALSE)
site_df <- aggregate_time_data(site_df, field = "website")
site_df <- high_level_aggregate(site_df, field = "website_aggregated")
site_lookup <- as.data.table(unique(
  site_df[, c("website", "website_aggregated_high_level")]))
panel <- merge(panel, site_lookup, by = "website", all.x = TRUE)
panel <- panel[!(tolower(website_aggregated_high_level) %in% SURVEY_WEBSITES)]

domain_class <- get_domain_classification(); setDT(domain_class)
domain_class_slim <- unique(domain_class[, .(
  website_agg = name_aggregated_high_level, category = category_level_1)])
panel <- merge(panel, domain_class_slim,
               by.x = "website_aggregated_high_level", by.y = "website_agg",
               all.x = TRUE)

t1 <- panel[tau >= TAU_MIN & tau <= TAU_MAX & !is.na(visit_count) & visit_count > 0]
t1[, cpv_3p     := n_cookies_third_party / visit_count]
t1[, log_cpv_3p := log(1 + cpv_3p)]
t1[, log_time   := log(1 + time_spent)]
t1[, treated    := as.integer(cookie_treatment_idx == 1)]
t1[, post       := as.integer(tau >= 0)]
t1[, post_treated := post * treated]
t1[, dow        := factor(weekdays(date))]

events <- fread("../data/final_extension_data/event_logs.csv",
                select = c("experiment_id", "event"))
del_users <- unique(events[grepl("^AUTOMATIC_COOKIE_DELETION", event), experiment_id])
t1[, has_log := as.integer(experiment_id %in% del_users)]
rm(events); gc(verbose = FALSE)

t1_cat <- t1[!is.na(category) & category != ""]
big_cats <- t1_cat[, .(n_obs = .N, n_users = uniqueN(experiment_id)),
                   by = category][n_obs >= 500 & n_users >= 50, category]

# --- the two subgroup-DiD helpers, copied verbatim from cookie_deletion.R ----
fit_by_group <- function(data, groups, group_col, yvar, fe_with_website = TRUE) {
  fe <- if (fe_with_website) "experiment_id + website + dow" else "experiment_id + dow"
  fml <- as.formula(sprintf("%s ~ post_treated | %s", yvar, fe))
  out <- lapply(groups, function(g) {
    sub <- data[get(group_col) == g]
    m <- tryCatch(feols(fml, data = sub, cluster = ~experiment_id, notes = FALSE),
                  error = function(e) NULL)
    if (is.null(m) || !"post_treated" %in% names(coef(m))) return(NULL)
    ci <- confint(m, "post_treated", level = 0.95)
    data.table(grp_raw = as.character(g),
               coef = coef(m)["post_treated"], se = se(m)["post_treated"],
               p = pvalue(m)["post_treated"],
               ci_lo = as.numeric(ci[1, 1]), ci_hi = as.numeric(ci[1, 2]))
  })
  rbindlist(out)
}

fit_by_group_logstatus <- function(data, groups, group_col, yvar, fe_with_website = TRUE) {
  fe <- if (fe_with_website) "experiment_id + website + dow" else "experiment_id + dow"
  fml <- as.formula(sprintf("%s ~ post_treated | %s", yvar, fe))
  out <- list()
  for (g in groups) for (lg in c(0, 1)) {
    sub <- data[get(group_col) == g & has_log == lg]
    m <- tryCatch(feols(fml, data = sub, cluster = ~experiment_id, notes = FALSE),
                  error = function(e) NULL)
    if (is.null(m) || !"post_treated" %in% names(coef(m))) next
    ci <- confint(m, "post_treated", level = 0.95)
    out[[paste(g, lg)]] <- data.table(
      grp_raw = as.character(g),
      log_status = if (lg == 1) "Has Log" else "No Log",
      coef = coef(m)["post_treated"], se = se(m)["post_treated"],
      p = pvalue(m)["post_treated"],
      ci_lo = as.numeric(ci[1, 1]), ci_hi = as.numeric(ci[1, 2]))
  }
  res <- rbindlist(out)
  res[, log_status := factor(log_status, levels = c("No Log", "Has Log"))]
  res
}

cat_dt       <- fit_by_group(t1_cat, sort(big_cats), "category", "log_cpv_3p")
cat_time     <- fit_by_group(t1_cat, sort(big_cats), "category", "log_time")
cat_log      <- fit_by_group_logstatus(t1_cat, sort(big_cats), "category", "log_cpv_3p")
cat_time_log <- fit_by_group_logstatus(t1_cat, sort(big_cats), "category", "log_time")

# baseline figure 1.6 uses `category %in% big_cats` directly (post == 0)
site_base <- t1[post == 0 & category %in% big_cats,
                .(site_mean_cpv = mean(cpv_3p, na.rm = TRUE)),
                by = .(website, category)]

M <- length(big_cats)
cat(sprintf("\nbig_cats (shared target)                          : %d\n", M))
cat(sprintf("fig 2.4  CPV category   [drives \\nCpvCategories]   : %d\n", uniqueN(cat_dt$grp_raw)))
cat(sprintf("fig 3.4  TIME category                             : %d\n", uniqueN(cat_time$grp_raw)))
cat(sprintf("fig 2.7  CPV cat x log-status  (distinct cats)      : %d\n", uniqueN(cat_log$grp_raw)))
cat(sprintf("fig 3.7  TIME cat x log-status (distinct cats)      : %d\n", uniqueN(cat_time_log$grp_raw)))
cat(sprintf("fig 1.6  baseline (site_base)                       : %d\n", uniqueN(site_base$category)))

report_missing <- function(dt, label) {
  miss <- setdiff(big_cats, unique(dt$grp_raw))
  if (length(miss)) cat(sprintf("  [%s] dropped: %s\n", label, paste(miss, collapse = ", ")))
}
cat("\nCategories in big_cats but DROPPED from a figure:\n")
report_missing(cat_dt,       "2.4 CPV cat")
report_missing(cat_time,     "3.4 TIME cat")
report_missing(cat_log,      "2.7 CPV cat x log")
report_missing(cat_time_log, "3.7 TIME cat x log")

one_shape <- function(dt, label) {
  tab <- dt[, .(n_shapes = uniqueN(log_status)), by = grp_raw][n_shapes < 2]
  if (nrow(tab)) cat(sprintf("  [%s] only ONE log-status shape: %s\n",
                             label, paste(tab$grp_raw, collapse = ", ")))
}
cat("\nLog-status figures: categories drawn for only ONE shape (has vs no log):\n")
one_shape(cat_log,      "2.7 CPV cat x log")
one_shape(cat_time_log, "3.7 TIME cat x log")

# prose says "significant NEGATIVE": flag any significant-and-POSITIVE CPV cat
sig_pos <- cat_dt[ci_lo > 0, .N]
cat(sprintf("\ncat_dt significant-AND-POSITIVE count (prose claims 'negative'): %d\n", sig_pos))
cat("  (\\nSigCpvCategories counts 2-sided significance; if this is > 0 the\n",
    "   'significant negative' wording is wrong.)\n", sep = "")

cat("\n=== DONE ===\n")