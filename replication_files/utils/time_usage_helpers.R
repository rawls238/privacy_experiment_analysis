# =============================================================================
# TIME USAGE HELPERS
# =============================================================================
#
# Functions used in production scripts to:
#   - Load and clean browsing time data
#   - Map domains to categories and privacy info
#   - Compute privacy scores (full, exposed, personalized)
#   - Load conjoint utility weights (individual or population)
#   - Load endline survey personalized info
#
# Caller scripts (verified via grep):
#   replication_files/time_use_analysis/time_usage_treatment_effects_SG.R
#   replication_files/survey_analysis/top_sites_beliefs_analysis.R
#   replication_files/privacy_descriptives/privacy_char_summary_stats_3.R
#   replication_files/utils/info_acq_helpers.R
#   code/cookie_deletion/cookie_deletion_checks*.R              (stage 1)
#
# Data paths assume the calling script setwd()'s to `code_github/`.
#
# Note: Previous version of this file also defined 11 functions (now removed
# as dead code; zero call sites across all .R scripts in the repo):
#   - clean_urls
#   - aggregate_time_data_trackers
#   - standardize_ad_domains_trackers
#   - high_level_aggregate_trackers
#   - map_domain_trackers
#   - map_domain
#   - map_domain_improved
#   - map_privacy_data_old
#   - map_privacy_data_trackers
#   - map_privacy_data_trackers_2
#   - map_privacy_data_trackers_SG
# Removing them dropped this file from ~2900 lines to ~1400 lines.
# =============================================================================

# Load necessary libraries
library(tidyverse)
library(stringdist)
library(data.table)
library(parallel)

# ----------------------------------------------------------------------------
# Data path constants (relative to code_github/)
# ----------------------------------------------------------------------------
DATA_DIR     <- "../data/"
EXT_DATA_DIR <- "../data/final_extension_data/"
SURVEY_DIR   <- "../data/Survey/"
CONJOINT_DIR <- "../data/Conjoint/"

# =============================================================================
# Core data loading
# =============================================================================

get_clean_time_data <- function() {
  time_data_2 <- fread(paste0(EXT_DATA_DIR, "time_data_2.csv"))
  privacy_info <- get_privacy_info_wide()
  exp_conditions <- fread(paste0(EXT_DATA_DIR, "experiment_conditions_pilot_july_2024.csv"))
  experiment_users <- exp_conditions[experiment_condition != "" & in_experiment == 'true']
  
  experiment_users <- experiment_users %>% mutate(wave_id = ifelse(wave_id == 3, 2, wave_id)) %>%
    mutate(block_by_wave = paste(wave_id,block_idx,sep = '_'))
  
  time_data_2 <- time_data_2 %>% left_join(experiment_users %>% select(experiment_id, wave_id, experiment_condition, block_by_wave), by="experiment_id")
  
  
  target_wave_ids <- exp_conditions[experiment_condition != "" & in_experiment == 'true', experiment_id]
  # Filter time data
  filtered_time_data <- time_data_2[experiment_id %in% target_wave_ids]
  filtered_time_data <- filtered_time_data  %>% filter((wave_id == 1 & mdy(date) >= START_DATE_WAVE_1 & mdy(date) < END_DATE_WAVE_1) |
                                                         (wave_id == 2 & mdy(date) >= START_DATE_WAVE_2 & mdy(date) < END_DATE_WAVE_2))
  # Clean data following pipeline
  time_data_2_dropna <- filtered_time_data %>%
    na.omit() %>%
    filter(!grepl("http", website))
  
  time_data_2_dropna <- time_data_2_dropna %>%
    mutate(website = case_when(
      str_detect(tolower(website), "twitter\\.com") ~ "x.com",
      TRUE ~ website
    ))
  
  # Aggregate following the pipeline
  time_data_2_aggregated <- aggregate_time_data(time_data_2_dropna)
  time_data_2_aggregated_high <- high_level_aggregate(time_data_2_aggregated)
  
  # Privacy matching
  privacy_unique <- unique(privacy_info, by = "domain")
  time_data_2_aggregated_high_privacy <- map_privacy_data(time_data_2_aggregated_high, privacy_unique)
  
  # Select final columns and map twitter to X
  final_data <- time_data_2_aggregated_high_privacy %>%
    select(tstamp, date, user_id, experiment_id, website, website_aggregated,
           website_aggregated_high_level, privacy_exist, time_spent, timezone,
           elicitation_count, visit_count, wave_id, experiment_condition) %>%
    mutate(
      date = mdy(date),
      # Map twitter to X in all website columns
      website = case_when(
        str_detect(tolower(website), "twitter") ~ str_replace_all(tolower(website), "twitter", "x"),
        TRUE ~ website
      ),
      website_aggregated = case_when(
        str_detect(tolower(website_aggregated), "twitter") ~ str_replace_all(tolower(website_aggregated), "twitter", "x"),
        TRUE ~ website_aggregated
      ),
      website_aggregated_high_level = case_when(
        str_detect(tolower(website_aggregated_high_level), "twitter") ~ str_replace_all(tolower(website_aggregated_high_level), "twitter", "x"),
        TRUE ~ website_aggregated_high_level
      )
    )
  final_data <- final_data %>%
    mutate(
      post = case_when(
        wave_id == 1 ~ date >= TREATMENT_DATE_WAVE_1,
        wave_id != 1 ~ date >= TREATMENT_DATE_WAVE_2
      ),
      treatment_date = case_when(
        wave_id == 1 ~ TREATMENT_DATE_WAVE_1,
        wave_id != 1 ~ TREATMENT_DATE_WAVE_2
      ),
      weeks_since_intervention = as.integer(floor((date - ymd(treatment_date)) / 7))
    )
  return(final_data)
}

get_privacy_info_raw <- function () {
  privacy_info <- fread(paste0(EXT_DATA_DIR, "privacy_info.csv"))
  privacy_info <- privacy_info %>% mutate(numeric_rating = ifelse(rating == "Yes", 1, 0))
  privacy_info$domain = gsub('^(open\\.|en\\.|www?\\d*\\.)', '',   privacy_info$domain) # remove www.
  privacy_info <- privacy_info %>%
    mutate(domain = case_when(
      str_detect(tolower(domain), "twitter\\.com") ~ "x.com",
      TRUE ~ domain
    ))
  privacy_info$domain <- gsub("betterhealth.vic.gov.au", "betterhealth.vic.gov", privacy_info$domain) # gov.au -> gov
  privacy_info$domain <- sub("\\.co\\.uk$", ".co", privacy_info$domain) # co.uk -> .co
  privacy_info <- high_level_aggregate(aggregate_time_data(privacy_info, field="domain"), field="domain_aggregated")
  return(privacy_info)
}


get_domain_classification <- function() {
  domain_classfication = rbind(read.csv("auxiliary_data/domain_classification_2.csv"), read.csv("auxiliary_data/domain_classification_extra.csv"))
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
  domain_classfication <- aggregate_time_data(domain_classfication, field="name")
  domain_classfication <- high_level_aggregate(domain_classfication, field="name_aggregated")
  
  domain_classfication<-domain_classfication %>%
    mutate(
      is_com = str_detect(name, "\\.com$")
    ) %>%
    group_by(name_aggregated_high_level) %>%
    slice_max(is_com, with_ties = FALSE) %>%  # keep .com if exists, else another
    ungroup()
  
  domain_classfication <- domain_classfication %>% mutate(category = map_chr(categories, ~ {
    if (is.na(.x)) {
      NA_character_  # Return NA if the value is missing
    } else {
      parsed <- fromJSON(.x)
      if (length(parsed) == 0) {
        NA_character_  # Handle empty lists
      } else {
        parsed %>% bind_rows() %>% arrange(desc(confidence)) %>% slice(1) %>% pull(name)
      }
    }
  }))
  domain_classfication <- domain_classfication %>% mutate(category = ifelse(is.na(category), "", sub("^/", "", category))) %>%
    separate(category, into = c("category_level_1", "category_level_2", "category_level_3"), sep = "/", fill = "right", extra = "merge")
  return(domain_classfication)
}

get_personalized_scores <- function(time_dat) {
  privacy_info <- get_privacy_info_wide()
  #privacy_info <- privacy_info %>% left_join(domain_classfication, by=c("domain_aggregated_high_level"="name_aggregated_high_level"))
  
  beta_cols <- names(time_dat)[startsWith(names(time_dat), "beta_")]
  
  # 0) Explicit aliases for known naming mismatches
  #    Add entries here as needed.
  
  # 1) Build the beta->p mapping (beta_ -> p_, '.' -> '-'), then apply aliases
  beta_map <- tibble(beta = beta_cols) %>%
    mutate(p = sub("^beta_", "p_", beta),
           p = gsub("\\.", "-", p))
  
  # 2) Figure out which p_* columns actually exist in privacy_info
  p_needed <- beta_map$p
  p_present <- intersect(p_needed, names(privacy_info))
  p_missing <- setdiff(p_needed, names(privacy_info))
  
  if (length(p_missing)) {
    message("Dropping beta features with no matching p_* in privacy_info: ",
            paste(unique(beta_map$beta[beta_map$p %in% p_missing]), collapse = ", "))
  }
  
  # Keep only overlapping features
  beta_map_use <- beta_map %>% filter(p %in% p_present)
  
  # 3) Build P matrix (websites × features)
  site_key <- "domain_aggregated_high_level"
  P <- privacy_info %>%
    select(all_of(site_key), all_of(beta_map_use$p))
  site_ids <- P[[site_key]]
  P_mat <- as.matrix(P[, beta_map_use$p, drop = FALSE])
  storage.mode(P_mat) <- "double"
  
  # 4) Build B matrix (users × features), renaming beta_* to p_* and keeping only overlapping p_*
  B <- time_dat %>% group_by(experiment_id) %>% slice(1) %>% ungroup() %>%
    select(experiment_id, all_of(beta_map_use$beta)) %>%
    rename_with(~ beta_map_use$p[match(.x, beta_map_use$beta)],
                .cols = all_of(beta_map_use$beta))
  
  B <- B %>%
    mutate(across(-experiment_id, ~ suppressWarnings(as.numeric(.)))) %>%
    mutate(across(-experiment_id, ~ coalesce(., 0)))
  
  # --- Align columns between P and B (defensive: intersect & reorder)
  p_cols_P <- colnames(P_mat)
  p_cols_B <- setdiff(colnames(B), "experiment_id")
  p_cols   <- intersect(p_cols_P, p_cols_B)
  
  
  # If you expect full overlap, assert here:
  # stopifnot(setequal(p_cols_P, p_cols_B))
  
  # Rebuild aligned matrices
  P_mat_aligned <- P_mat[, p_cols, drop = FALSE]
  B_mat_aligned <- as.matrix(B[, p_cols, drop = FALSE])
  storage.mode(B_mat_aligned) <- "double"
  
  # --- Personalized scores: S = P %*% t(B)
  W   <- abs(B_mat_aligned)                       # |beta|
  D   <- ifelse(B_mat_aligned >= 0,  1, -1)       # +1 for >=0, -1 for <0
  NEG <- ifelse(B_mat_aligned <  0,  1,  0)       # 1 for <0,  0 otherwise
  
  # term 1: sum_k |beta| * sign * p  ==  P %*% t(|beta| * sign)
  T1 <- P_mat_aligned %*% t(W * D)                # (n_sites × n_users)
  
  # term 2: sum_k |beta| * 1(beta<0)  -- independent of site, per user scalar
  C  <- rowSums(W * NEG, na.rm = TRUE)            # length n_users
  N  <- T1 + matrix(C, nrow = nrow(P_mat_aligned), ncol = length(C), byrow = TRUE)
  
  # denominator: sum_k |beta|
  denom <- rowSums(W, na.rm = TRUE)               # length n_users
  denom[denom == 0] <- NA_real_                   # avoid divide-by-zero → NA
  
  # final scores: in [0,1]
  S <- sweep(N, 2, denom, "/")
  
  # ---- Single aggregated score for requested attributes (q1_html_key & q2_html_key) ----
  user_keys <- time_dat %>%
    dplyr::group_by(experiment_id) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
    dplyr::select(experiment_id, q1_html_key, q2_html_key)
  
  # reorder to match columns/users of B_mat_aligned
  user_keys <- user_keys[match(B$experiment_id, user_keys$experiment_id), ]
  
  to_p_col <- function(x) {
    x <- as.character(x)
    x <- ifelse(startsWith(x, "p_"), x, paste0("p_", x))
    gsub("\\.", "-", x)
  }
  
  p_colnames <- colnames(P_mat_aligned)  # same as colnames(B_mat_aligned)
  idx_q1 <- match(to_p_col(user_keys$q1_html_key), p_colnames)  # length = n_users
  idx_q2 <- match(to_p_col(user_keys$q2_html_key), p_colnames)
  
  n_sites <- nrow(P_mat_aligned)
  n_users <- nrow(B_mat_aligned)
  
  # Helper to accumulate contributions for one attr vector of column indices
  accum_attr <- function(idx_vec) {
    # replace NA indices with 1 (dummy col), we'll zero-weight them via mask
    idx_fill <- ifelse(is.na(idx_vec), 1L, idx_vec)
    
    # p-values per (site,user) for this attr:
    # selects each user's column -> matrix n_sites x n_users
    P_attr <- P_mat_aligned[, idx_fill, drop = FALSE]
    
    # user-specific betas for that attr (vector length n_users)
    b_vec <- B_mat_aligned[cbind(seq_len(n_users), idx_fill)]
    w_vec <- abs(b_vec)
    rp_mat <- ifelse(matrix(b_vec, nrow = n_sites, ncol = n_users, byrow = TRUE) >= 0,
                     P_attr, 1 - P_attr)
    
    # expand weights to site rows and zero out users with NA idx
    w_mat <- matrix(w_vec, nrow = n_sites, ncol = n_users, byrow = TRUE)
    mask  <- as.numeric(!is.na(idx_vec))
    w_mat <- sweep(w_mat, 2, mask, `*`)
    
    list(num = w_mat * rp_mat, den = w_mat)
  }
  
  a1 <- accum_attr(idx_q1)
  a2 <- accum_attr(idx_q2)
  
  num_req <- a1$num + a2$num
  den_req <- a1$den + a2$den
  
  score_req <- num_req / den_req
  score_req[!is.finite(score_req)] <- NA_real_
  
  # Tidy to long and merge into your output
  scores_req_wide <- as.data.frame(score_req)
  names(scores_req_wide) <- as.character(B$experiment_id)
  scores_req_wide[["domain_aggregated_high_level"]] <- site_ids
  
  scores_req_long <- tidyr::pivot_longer(
    scores_req_wide,
    cols = -domain_aggregated_high_level,
    names_to = "experiment_id",
    values_to = "personalized_score_requested_attr"
  )
  
  # --- Tidy long table
  scores_long <- as.data.frame(S)
  names(scores_long) <- as.character(B$experiment_id)
  scores_long[["domain_aggregated_high_level"]] <- site_ids
  
  personalized_scores <- tidyr::pivot_longer(
    scores_long,
    cols = -domain_aggregated_high_level,
    names_to = "experiment_id",
    values_to = "personalized_score"
  )
  personalized_scores <- personalized_scores %>%
    left_join(scores_req_long, by = c("domain_aggregated_high_level", "experiment_id"))
  domain_classfication <- get_domain_classification()
  personalized_scores <- personalized_scores %>% left_join(
    domain_classfication %>% select(name_aggregated_high_level, category_level_1),  by=c("domain_aggregated_high_level"="name_aggregated_high_level"))
  
  # Patch: filter out invalid matches based on original website
  # - go, mail: do not match (semantically different websites)
  # - tips: only word.tips should match
  # - x: only Twitter/X URLs should match (not x.ai, etc.)
  invalid_combinations <- time_dat %>%
    select(experiment_id, website, website_aggregated_high_level) %>%
    distinct() %>%
    filter(
      website_aggregated_high_level %in% c("go", "mail") |
        (website_aggregated_high_level == "tips" & !grepl("^word\\.tips$", website, ignore.case = TRUE)) |
        (website_aggregated_high_level == "x" & !grepl("^x\\.com$|^x$|^t\\.co$|\\.x\\.com$", website, ignore.case = TRUE))
    ) %>%
    mutate(invalid_key = paste(experiment_id, website_aggregated_high_level, sep = "___")) %>%
    pull(invalid_key)
  
  personalized_scores <- personalized_scores %>%
    mutate(key = paste(experiment_id, domain_aggregated_high_level, sep = "___")) %>%
    filter(!(key %in% invalid_combinations)) %>%
    select(-key)
  
  return(personalized_scores)
}

get_privacy_info_wide <- function() {
  privacy_info <- fread(paste0(EXT_DATA_DIR, "privacy_info.csv"))
  privacy_info <- privacy_info %>% mutate(numeric_rating = ifelse(rating == "Yes", 1, 0))
  privacy_info <- privacy_info %>% group_by(html_key, domain) %>% summarise(provided_privacy = mean(numeric_rating)) %>% ungroup()
  privacy_info_wide <- privacy_info %>%
    pivot_wider(names_from = html_key, values_from = provided_privacy) %>%
    rename_with(~ paste0("p_", .), -domain)
  privacy_info_wide$domain = gsub('^(open\\.|en\\.|www?\\d*\\.)', '',   privacy_info_wide$domain) # remove www.
  privacy_info_wide <- privacy_info_wide %>%
    mutate(domain = case_when(
      str_detect(tolower(domain), "twitter\\.com") ~ "x.com",
      TRUE ~ domain
    ))
  privacy_info_wide$domain <- gsub("betterhealth.vic.gov.au", "betterhealth.vic.gov", privacy_info_wide$domain) # gov.au -> gov
  privacy_info_wide$domain <- sub("\\.co\\.uk$", ".co", privacy_info_wide$domain) # co.uk -> .co
  privacy_info_wide <- high_level_aggregate(aggregate_time_data(privacy_info_wide, field="domain"), field="domain_aggregated")
  privacy_info_wide <- privacy_info_wide %>% group_by(domain_aggregated_high_level) %>% slice(1) %>% ungroup()
  
  # patch：twitter -> x
  privacy_info_wide <- privacy_info_wide %>%
    mutate(domain_aggregated_high_level = case_when(
      domain_aggregated_high_level == "twitter" ~ "x",
      TRUE ~ domain_aggregated_high_level
    ))
  return(privacy_info_wide)
}

compute_privacy_scores <- function(df) {
  
  id_cols = c("experiment_id", "website_aggregated_high_level")
  
  p_cols    <- names(df)[startsWith(names(df), "p_")]
  beta_cols <- setdiff(names(df)[startsWith(names(df), "beta_")],
                       "beta_website_advertising")
  if (length(p_cols) == 0 || length(beta_cols) == 0) {
    return(df %>% mutate(privacy_full_beta_p = NA_real_))
  }
  
  p_long <- df %>%
    select(all_of(id_cols), all_of(p_cols)) %>%
    pivot_longer(cols = all_of(p_cols),
                 names_to = "attr",
                 values_to = "p",
                 names_prefix = "p_") %>%
    mutate(p = suppressWarnings(as.numeric(p)))
  
  beta_long <- df %>% group_by(experiment_id) %>% slice(1) %>% ungroup() %>%
    select("experiment_id", all_of(beta_cols)) %>%
    pivot_longer(cols = all_of(beta_cols),
                 names_to = "attr",
                 values_to = "beta",
                 names_prefix = "beta_") %>%
    mutate(beta = suppressWarnings(as.numeric(beta)))
  
  joined <- p_long %>%
    inner_join(beta_long, by = c("experiment_id", "attr")) %>%
    mutate(
      rp = if_else(beta >= 0, p, 1 - p, missing = NA_real_),
      w  = abs(beta)
    )
  
  scores <- joined %>%
    group_by(across(all_of(id_cols))) %>%
    summarise(
      privacy_full_beta_p = if (sum(w, na.rm = TRUE) > 0)
        sum(w * rp, na.rm = TRUE) / sum(w, na.rm = TRUE)
      else NA_real_,
      .groups = "drop"
    )
  
  df %>%
    left_join(scores, by = id_cols)
}

map_domain_3 <- function(df.domain, domain_classification_3) {
  # remove useless and convert inputs to data.table
  domain_classification_3 <- domain_classification_3 %>%
    select(-date)
  setDT(df.domain)
  setDT(domain_classification_3)
  
  # Extract and format websites
  web_in <- df.domain$domain
  web_ex <- domain_classification_3$name
  
  Formatting <- function(webs) {
    unique_webs <- unique(webs)
    cleaned_webs <- gsub('^(open\\.|en\\.|www?\\d*\\.)', '', unique_webs)
    return(cleaned_webs)
  }
  
  Split <- function(webs_formatted) {
    strsplit(webs_formatted, "\\.")
  }
  
  Split_remove_last <- function(webs_formatted) {
    sapply(strsplit(webs_formatted, "\\."), function(x) if(length(x) > 1){x[-length(x)]} else {x[length(x)]})
  }
  
  Makepair <- function(webs_in, webs_ex) {
    webs_inter <- Formatting(webs_in)
    webs_exter <- Formatting(webs_ex)
    
    exter_splits <- lapply(webs_exter, Split_remove_last)
    exter_domains <- unlist(exter_splits)
    exter_dt <- data.table(
      domain = exter_domains,
      exter = rep(webs_exter, sapply(exter_splits, length))
    )
    setkey(exter_dt, domain)
    matched_pairs <- vector("list", length(webs_inter))
    names(matched_pairs) <- webs_inter
    
    for (inter in webs_inter) {
      inter_domains <- unlist(Split(inter))
      matches <- unique(exter_dt[domain %in% inter_domains, exter])
      matched_pairs[[inter]] <- matches
    }
    
    return(matched_pairs)
  }
  
  map_matched <- Makepair(web_in, web_ex)
  
  get_top_match <- function(map_matched) {
    top_matches <- character(length(map_matched))
    names(top_matches) <- names(map_matched)
    
    for (a in names(map_matched)) {
      candidates <- map_matched[[a]]
      
      if (length(candidates) == 0) {
        top_matches[a] <- NA_character_
        next
      }
      
      a_parts <- unlist(strsplit(a, "\\."))
      
      best_match <- candidates[which.max(sapply(candidates, function(b) {
        b_parts <- unlist(strsplit(b, "\\."))
        matched_parts <- length(intersect(a_parts, b_parts))
        return(matched_parts * 1000 + nchar(b))  # Prioritize matched parts, then length
      }))]
      
      top_matches[a] <- best_match
    }
    
    return(top_matches)
  }
  
  top1_matches <- get_top_match(map_matched)
  
  top1_matches_dt <- data.table(
    left = names(top1_matches),
    right = unlist(top1_matches)
  )
  
  top1_matches_dt <- top1_matches_dt[!(right == "english-heritage.org.uk" & !grepl("english-heritage", left))]
  
  # Remove 'www' prefix using data.table
  remove_prefix <- function(domain) {
    gsub('^(open\\.|en\\.|www?\\d*\\.)', '', domain)
  }
  
  domain_classification_3[, name := remove_prefix(name)]
  
  join_and_count_matches <- function(df.domain, top1_matches_dt, domain_classification_3) {
    # NOTE: map_domain_3 expects df.domain to have columns 'domain' (the website
    # identifier) and 'value' (a non-empty data column used to filter out NA
    # rows produced by the full outer join). This differs from map_domain (which
    # uses 'website_aggregated' + 'time_spent') because map_domain_3 is used for
    # privacy-attribute data (privacy_char_summary_stats_3.R), not time data.
    setkey(df.domain, domain)
    setkey(top1_matches_dt, left)
    setkey(domain_classification_3, name)
    
    # First full join
    result_dt <- merge(df.domain, top1_matches_dt, by.x = "domain", by.y = "left", all = TRUE)
    # Second full join
    result_dt <- result_dt %>% mutate(index = row_number())
    result_dt <- merge(result_dt, domain_classification_3, by.x = "right", by.y = "name", all = TRUE)
    # Remove useless rows created by Full join.
    result_dt <- result_dt[!is.na(value) & value != ""]
    # Count and report
    original_count <- nrow(df.domain)
    final_count <- nrow(result_dt)
    matched_count <- sum(!is.na(result_dt$right) & !is.na(result_dt$domain))
    
    cat("Original number of rows in df.domain:", original_count, "\n")
    cat("Number of rows with a match:", matched_count, "\n")
    cat("Percentage of original rows with a match:", (matched_count / original_count) * 100, "%\n")
    return(result_dt)
  }
  
  enriched_dt <- join_and_count_matches(df.domain, top1_matches_dt, domain_classification_3)
  enriched_df = as.tibble(enriched_dt)
  
  enriched_df <- enriched_df %>% mutate(category = map_chr(categories, ~ {
    if (is.na(.x)) {
      NA_character_  # Return NA if the value is missing
    } else {
      parsed <- fromJSON(.x)
      if (length(parsed) == 0) {
        NA_character_  # Handle empty lists
      } else {
        parsed %>% bind_rows() %>% arrange(desc(confidence)) %>% slice(1) %>% pull(name)
      }
    }
  }))
  enriched_df <- enriched_df %>% mutate(category = ifelse(is.na(category), "", sub("^/", "", category))) %>%
    separate(category, into = c("category_level_1", "category_level_2", "category_level_3"), sep = "/", fill = "right", extra = "merge")
  # remove fields that we are not interested.
  enriched_df <- enriched_df |>
    select(-right, -categories)
  return(enriched_df)
}

map_privacy_data <- function(time_data_2_domain_class_high_level, privacy) {
  # Match user browsing data to privacy information using exact matching
  # Input: time_data with website columns, privacy from get_privacy_info_wide()
  # Output: original data with p_* columns and privacy_exist flag added
  
  enriched_data_1 = time_data_2_domain_class_high_level
  setDT(enriched_data_1)
  setDT(privacy)
  
  # Patch: handle special domain mappings
  # - go, mail: do not match (user visits go.dev, mail.com are different from privacy_info's abcnews.go.com, mail.aol.com)
  # - tips: only match word.tips (not smartfinance.tips, myvegas.tips)
  # - x: only match Twitter/X URLs (not t.me=Telegram, x.ai=xAI)
  enriched_data_1 <- enriched_data_1 %>%
    mutate(
      matching_key = case_when(
        website_aggregated_high_level %in% c("go", "mail") ~ NA_character_,
        website_aggregated_high_level == "tips" & !grepl("^word\\.tips$", website, ignore.case = TRUE) ~ NA_character_,
        website_aggregated_high_level == "x" & !grepl("^x\\.com$|^x$|^t\\.co$|\\.x\\.com$", website, ignore.case = TRUE) ~ NA_character_,
        TRUE ~ website_aggregated_high_level
      )
    )
  
  setDT(enriched_data_1)
  
  # Exact matching (replaces previous complex Makepair_1/get_top_match logic)
  result_dt <- merge(
    enriched_data_1,
    privacy,
    by.x = "matching_key",
    by.y = "domain_aggregated_high_level",
    all.x = TRUE
  )
  
  # Check if match was successful by looking at a p_* column
  p_cols <- names(privacy)[startsWith(names(privacy), "p_")]
  first_p_col <- p_cols[1]
  result_dt[, privacy_exist := !is.na(get(first_p_col))]
  result_dt[, matching_key := NULL]
  
  # Report matching statistics
  original_count <- nrow(enriched_data_1)
  matched_count <- sum(result_dt$privacy_exist)
  cat("Original number of rows:", original_count, "\n")
  cat("Number of rows with a match:", matched_count, "\n")
  cat("Percentage of rows with a match:", round((matched_count / original_count) * 100, 2), "%\n")
  
  enriched_df = as_tibble(result_dt)
  return(enriched_df)
}

# Refactored aggregate_time_data with field parameter
aggregate_time_data <- function(df, field = "website") {
  
  out_field <- paste0(field, "_aggregated")
  webs <- df[[field]]
  
  # --- Dedup: only process unique values ---
  unique_vals <- unique(webs)
  
  ## Formatting_1 (VECTORIZED)
  webs1 <- unique_vals
  
  is_translate <- grepl("\\.translate\\.goog$", webs1)
  
  idx1 <- is_translate & grepl("--", webs1)
  webs1[idx1] <- gsub("--", "@", webs1[idx1])
  webs1[idx1] <- gsub("-", ".", webs1[idx1])
  webs1[idx1] <- sub("\\.translate\\.goog$", "", webs1[idx1])
  webs1[idx1] <- gsub("@", "-", webs1[idx1])
  
  idx2 <- is_translate & !grepl("--", webs1)
  webs1[idx2] <- gsub("-", ".", webs1[idx2])
  webs1[idx2] <- sub("\\.translate\\.goog$", "", webs1[idx2])
  
  webs1 <- sub("^www-", "www.", webs1)
  webs1 <- sub("-com", ".com", webs1)
  
  ## Formatting_2 (.gov anywhere)
  webs2 <- sub("\\.gov", "", webs1)
  
  ## Formatting_3 (double postfix)
  patterns3 <- c("\\.net", "\\.com", "\\.co", "\\.org", "\\.edu", "\\.io",
                 "\\.live", "\\.ac", "\\.go")
  pattern3 <- paste(patterns3, collapse = "|")
  webs3 <- gsub(paste0("(", pattern3, ")(\\..*)?$"), "", webs2)
  
  ## Formatting_4 (PREFIX + SUFFIX LIST)
  cleaned_webs <- gsub('^(open\\.|en\\.|www?\\d*\\.)', '', webs3)
  
  suffixes <- c(
    '\\.xzy$', '\\.ai$', '\\.news$', '\\.dev$', '\\.eu$', '\\.it$', '\\.my$', '\\.sc$', '\\.so$', '\\.ag',
    '\\.live$', '\\.de$', '\\.fr$', '\\.bz$', '\\.us$', '\\.lk$', '\\.icu$', '\\.app$', '\\.app\\.box',
    '\\.online$', '\\.ee$', '\\.tv$', '\\.nl$', '\\.syf$', '\\.vip$', '\\.gg$', '\\.me$', '\\.is$',
    '\\.max$', '\\.sdge$', '\\.pch$', '\\.pl$', '\\.info$', '\\.tech$', '\\.fm$', '\\.nz$', '\\.lt$', '\\.rs$',
    '\\.pe$', '\\.iu$', '\\.chat$', '\\.uk$', '\\.at$', '\\.mi$', '\\.st$', '\\.ai$', '\\.cc$',
    '\\.es$', '\\.to$', '\\.il$', '\\.shop$', '\\.su$', '\\.re$', '\\.xxx$', '\\.ax$', '\\.ci$', '\\.ws$',
    '\\.lv$', '\\.bio$', '\\.pub$', '\\.md$', '\\.gs$', '\\.hr$', '\\.sx$', '\\.cz$', '\\.lc$', "\\.ro$", "\\.so$",
    '\\.ru$', '\\.cn$', '\\.jp$', '\\.in$', '\\.br$', '\\.za$', '\\.kr$', '\\.mx$', '\\.se$', '\\.no$',
    '\\.fi$', '\\.be$', '\\.ch$', '\\.dk$', '\\.pl$', '\\.gr$', '\\.tr$', '\\.ar$', '\\.cl$', '\\.hk$', '\\.tw$',
    '\\.sg$', '\\.ae$', '\\.sa$', '\\.il$', '\\.pt$', '\\.ie$', '\\.lu$', '\\.my$', '\\.ph$', '\\.vn$', '\\.id$',
    '\\.bd$', '\\.pk$', '\\.ng$', '\\.ke$', '\\.ug$', '\\.tz$', '\\.rw$', '\\.et$', '\\.nyc$', '\\.au$', '\\.fco.gov.uk$',
    '\\.jobs$', '\\.ly$', '\\.li$', '\\.si$', '\\.biz$', '\\.uk\\.com$', '\\.qa$', '\\.ae\\.org$', '\\.int$', '\\.nl\\.ca$',
    '\\.ad$', '\\.ae$', '\\.af$', '\\.ag$', '\\.ai$', '\\.am$', '\\.ao$', '\\.as$', '\\.at$', '\\.au$', '\\.aw$', '\\.ax$',
    '\\.ba$', '\\.bb$', '\\.bd$', '\\.be$', '\\.bf$', '\\.bg$', '\\.bh$', '\\.bi$', '\\.bj$', '\\.bl$', '\\.bm$', '\\.bn$',
    '\\.bo$', '\\.bq$', '\\.br$', '\\.bs$', '\\.bt$', '\\.bv$', '\\.bw$', '\\.by$', '\\.bz$', '\\.cc$', '\\.cd$', '\\.cf$',
    '\\.cg$', '\\.ch$', '\\.ci$', '\\.ck$', '\\.cl$', '\\.cm$', '\\.cn$', '\\.co$', '\\.cr$', '\\.cu$', '\\.cv$', '\\.cw$',
    '\\.cx$', '\\.cy$', '\\.cz$', '\\.de$', '\\.dj$', '\\.dk$', '\\.dm$', '\\.do$', '\\.dz$', '\\.ec$', '\\.ee$', '\\.eg$',
    '\\.eh$', '\\.er$', '\\.es$', '\\.et$', '\\.fi$', '\\.fj$', '\\.fm$', '\\.fo$', '\\.fr$', '\\.ga$', '\\.gb$', '\\.gd$',
    '\\.ge$', '\\.gf$', '\\.gg$', '\\.gh$', '\\.gi$', '\\.gl$', '\\.gm$', '\\.gn$', '\\.gp$', '\\.gq$', '\\.gr$', '\\.gt$',
    '\\.gu$', '\\.gw$', '\\.gy$', '\\.hk$', '\\.hm$', '\\.hn$', '\\.hr$', '\\.ht$', '\\.hu$', '\\.id$', '\\.ie$', '\\.il$',
    '\\.im$', '\\.io$', '\\.iq$', '\\.ir$', '\\.is$', '\\.it$', '\\.je$', '\\.jm$', '\\.jo$', '\\.in$', '\\.jp$', '\\.ke$',
    '\\.kg$', '\\.kh$', '\\.ki$', '\\.kj$', '\\.km$', '\\.kn$', '\\.kp$', '\\.kr$', '\\.kw$', '\\.ky$', '\\.kz$', '\\.la$',
    '\\.lb$', '\\.lc$', '\\.li$', '\\.lk$', '\\.lr$', '\\.ls$', '\\.lt$', '\\.lu$', '\\.lv$', '\\.ly$', '\\.ma$', '\\.mc$',
    '\\.md$', '\\.me$', '\\.mf$', '\\.mg$', '\\.mh$', '\\.mk$', '\\.ml$', '\\.mm$', '\\.mn$', '\\.mo$', '\\.mp$', '\\.mq$',
    '\\.mr$', '\\.ms$', '\\.mt$', '\\.mu$', '\\.mv$', '\\.mw$', '\\.mx$', '\\.my$', '\\.mz$', '\\.na$', '\\.nc$', '\\.ne$',
    '\\.ng$', '\\.ni$', '\\.nl$', '\\.no$', '\\.np$', '\\.nr$', '\\.nu$', '\\.nz$', '\\.om$', '\\.pa$', '\\.pe$', '\\.pf$',
    '\\.pg$', '\\.ph$', '\\.pk$', '\\.pl$', '\\.pm$', '\\.pn$', '\\.pr$', '\\.ps$', '\\.pt$', '\\.pw$', '\\.py$', '\\.qa$',
    '\\.re$', '\\.ro$', '\\.rs$', '\\.ru$', '\\.rw$', '\\.sa$', '\\.sb$', '\\.sc$', '\\.sd$', '\\.se$', '\\.sg$', '\\.sh$',
    '\\.si$', '\\.sj$', '\\.sk$', '\\.sl$', '\\.sm$', '\\.sn$', '\\.so$', '\\.sr$', '\\.ss$', '\\.st$', '\\.su$', '\\.sv$',
    '\\.sx$', '\\.sy$', '\\.sz$', '\\.tc$', '\\.td$', '\\.tf$', '\\.tg$', '\\.th$', '\\.tj$', '\\.tk$', '\\.tl$', '\\.tm$',
    '\\.tn$', '\\.to$', '\\.tr$', '\\.tt$', '\\.tv$', '\\.tz$', '\\.ua$', '\\.ug$', '\\.uk$', '\\.us$', '\\.uy$', '\\.uz$',
    '\\.vc$', '\\.ve$', '\\.vg$', '\\.vi$', '\\.vn$', '\\.vu$', '\\.wf$', '\\.ws$', '\\.ye$', '\\.yt$', '\\.za$', '\\.zm$',
    '\\.zw$'
  )
  
  suffix_pattern <- paste(suffixes, collapse = "|")
  
  webs4 <- ifelse(
    grepl("www\\.gov\\.uk$", webs3),
    "gov",
    gsub(suffix_pattern, "", cleaned_webs)
  )
  
  ## Aggregation rules
  webs5 <- webs4
  webs5 <- gsub(".*qualtrics.*", "qualtrics", webs5)
  webs5 <- gsub("l\\.facebook|lm\\.facebook|m\\.facebook\\.com", "facebook", webs5)
  webs5 <- gsub(".*cloudresearch.*", "cloudresearch", webs5)
  webs5 <- ifelse(grepl("^(www\\.)?t\\.me$", unique_vals, ignore.case = TRUE), "telegram", webs5)
  webs5 <- gsub("^(x|twitter|t)$", "twitter", webs5)
  webs5 <- gsub("^old\\.reddit.*$", "reddit", webs5)
  webs5 <- gsub("^new\\.reddit.*$", "reddit", webs5)
  webs5 <- gsub(".*\\.(microsoftonline|microsoft365)(\\..*)?$", "microsoft", webs5, perl = TRUE)
  webs5 <- gsub(".*\\.(amazonaws|amazoncognito)(\\..*)?$", "amazon", webs5, perl = TRUE)
  webs5 <- gsub(".*\\.(news.yahoo)(\\..*)?$", "news.yahoo", webs5, perl = TRUE)
  
  ## id.me override (needs original values)
  webs5[grepl("\\.id\\.me$", unique_vals)] <- "id.me"
  
  # --- Map back to full data ---
  lookup <- setNames(webs5, unique_vals)
  df[[out_field]] <- as.character(lookup[webs])
  
  if (field == "website") {
    df$website_aggregated <- df[[out_field]]
  }
  
  df
}

# Refactored high_level_aggregate with field parameter
high_level_aggregate <- function(df, field = "website_aggregated") {
  
  websites <- df[[field]]
  
  # --- Dedup: only process unique values ---
  unique_vals <- unique(websites)
  
  patterns <- c(
    "\\.xyz$", "\\.cloud$", "\\.eu$", "\\.live$", "\\.link$", "\\.app.link$", "\\.site$",
    "\\.my.site$", "\\.top$", "\\.com___$", "\\.com$", "\\.com\\.$", "\\.com.pa$",
    "\\.com.ua$", "\\.community$", "\\.it$", "\\.web$", "\\.website$", "\\.surveyrouter$",
    "\\.co$", "\\.so$", "\\.show$", "\\.club$", "\\.art$", "\\.org$", "\\.go$",
    "\\.edu$", "\\.health$", "\\.auth0$", "\\.us.auth0$", "\\.life$", "\\.coop$",
    "\\.ec$", "\\.k12.fl$", "\\.best$", "\\.si$", "\\.blogspot$", "\\.blog$",
    "\\.pages$", "\\.pub$"
  )
  
  webs_cleaned <- gsub(paste(patterns, collapse = "|"), "", unique_vals, perl = TRUE)
  webs_cleaned <- tolower(webs_cleaned)
  
  # --- Step 1: Extract last token after final dot (vectorized) ---
  formatted_unique <- sub(".*\\.", "", webs_cleaned)
  no_dot <- !grepl("\\.", webs_cleaned)
  formatted_unique[no_dot] <- webs_cleaned[no_dot]
  formatted_unique[is.na(webs_cleaned)] <- NA_character_
  
  # --- Step 2: Keep last 2 segments for known TLDs/gTLDs ---
  KEEP_LAST_2 <- c(
    # Canada
    "ca",
    # gTLDs
    "pro", "store", "run", "space", "studio", "page", "games", "bank", "one",
    "fun", "game", "lol", "work", "zone", "church", "media", "plus", "fit",
    "cafe", "ninja", "wtf", "click", "help", "today", "guide", "video",
    "global", "care", "guru", "design", "earth", "finance", "law", "travel",
    "host", "ink", "social", "win", "rocks", "stream", "academy", "digital",
    "exchange", "inc", "city", "codes", "coffee", "events", "heart", "baby",
    "casino", "careers", "gallery", "audio", "bot", "support", "works",
    "home", "group", "menu", "report", "fan", "land", "market", "name",
    "party", "school", "tube", "deals", "download", "dad", "money",
    "services", "software", "science", "email", "cards", "dental", "fish",
    "order", "pics", "reviews", "service", "trip", "vegas", "watch", "blue",
    "farm", "beer", "bet", "cam", "cat", "style", "team", "case", "cash",
    "bar", "bible", "center", "compare", "education", "place", "radio",
    "sale", "town", "trade", "training", "supply", "love", "mom", "pet",
    "red", "vote", "time", "restaurant", "mobi", "aero", "house", "senate",
    "zip", "tools", "world", "network", "force",
    # US state full names
    "virginia", "texas", "ohio", "utah", "hawaii", "indiana", "maryland",
    "colorado", "arizona", "georgia", "arkansas", "oregon", "idaho",
    "michigan", "wisconsin", "nebraska", "vermont", "alabama", "tennessee",
    "louisiana", "alaska", "delaware", "nevada", "iowa", "missouri", "maine",
    # US state/territory abbreviations & gov domains
    "ny", "va", "wa", "il", "in", "or", "nd", "nj", "mi", "mn", "dc",
    "fl", "nc", "sc", "al", "nv", "nh", "wi", "tx", "vt", "ct", "ks",
    "oh", "az", "ga", "ma", "wv", "nm", "ne", "ed", "mil", "dot", "gob",
    "europa", "k12", "illinois", "bc",
    # Other gTLDs
    "africa", "asia", "broadway", "company", "cyou", "day", "fyi", "gay",
    "homes", "how", "london", "ltd", "me", "onl", "paris", "partners",
    "points", "porn", "quest", "scot", "solutions", "systems", "tokyo",
    "wiki", "lnk", "moe", "as", "cap", "kit", "mail"
  )
  
  is_tld <- formatted_unique %in% KEEP_LAST_2 & grepl("\\.", webs_cleaned) & !is.na(formatted_unique)
  formatted_unique[is_tld] <- sub(".*\\.([^.]+\\.[^.]+)$", "\\1", webs_cleaned[is_tld])
  
  # --- Step 3: x -> twitter ---
  formatted_unique[formatted_unique == "x" & !is.na(formatted_unique)] <- "twitter"
  
  # --- Step 4: Mark junk as NA ---
  is_junk <- grepl("(com|edu):\\d+|org%3a", webs_cleaned) & !is.na(webs_cleaned)
  formatted_unique[is_junk] <- NA_character_
  
  # --- Step 5: Merge brand variants (e.g. amazon.ca -> amazon) ---
  MERGE_BRANDS <- c(
    # multi-TLD
    "amazon", "1tamilmv", "kemono", "brokensilenze", "bunkr", "foundryvtt",
    "icims", "kustomer", "minecraft", "monopolygo", "thepiratebay", "trivago",
    "uma", "yelp",
    # .ca brands
    "activia", "adameve", "affirm", "amsoil", "avery", "babycenter",
    "bathandbodyworks", "bbcearth", "bestbuy", "broadbandmap", "brother",
    "capterra", "carfax", "cargurus", "cheapflights", "cheapoair", "chevrolet",
    "columbiasportswear", "confirmit", "costco", "craftsman", "davidsbridal",
    "dennys", "drdrone", "ebay", "essie", "eventbrite", "expedia",
    "eyebuydirect", "facegood", "flightcentre", "ford", "frigidaire", "fruugo",
    "gettyimages", "giftcards", "glassdoor", "globalgolf", "gnc", "google",
    "hiringplatform", "hobbii", "homedepot", "honestreporting", "inaturalist",
    "intuit", "jerseymikes", "joolca", "knix", "medallia", "meineke",
    "michelin", "myprize", "naturalizer", "nbc", "ninjakitchen",
    "penguinrandomhouse", "petsmart", "pineconeresearch", "poshmark",
    "potterybarn", "questionpro", "realtor", "remax", "rentals", "rentbyowner",
    "revlon", "simplyhired", "sportsnet", "starbucks", "stubhub",
    "surveymonkey", "tacobell", "tantaly", "thetravelagentnextdoor",
    "ticketmaster", "toyota", "triggertech", "tripadvisor", "tyndale",
    "underarmour", "visa", "walmart", "wayfair", "weddingwire", "wingstop",
    "zohopublic",
    # .wiki (all)
    "7daystodie", "abioticfactor", "ashesofcreation", "atlyss", "avid", "bg3",
    "bluearchive", "dateeverything", "deadbydaylight", "deeprockgalactic",
    "deltarune", "enshrouded", "fantasylife", "helldivers", "kodi",
    "limbuscompany", "mechabreak", "namu", "palia", "runescape", "sca",
    "seaofthieves", "southpark", "terraria", "thebazaar", "unbound", "xat",
    # .lnk (all)
    "falloutboy", "flo", "googoodolls", "nin", "richiekotzen",
    "sabrinacarpenter", "skidrow", "stephenwilsonjr", "taylor", "taylorswift",
    # .help (all)
    "affinity", "bandcamp", "civicplus", "crisp", "crunch", "dixa", "gorgias",
    "intercom", "pixiv", "submittable", "toloka",
    # .store (safe)
    "dropkickmurphys", "katuchef", "leadpower", "melanawellness", "nectar",
    "philwickham", "ritdye", "ryzesuperfoods", "shopwanderous",
    "urbandictionary",
    # .careers (safe)
    "bju", "commonspirit", "github", "instacart", "literati", "tractorsupply",
    "vasculardynamicsinc",
    # .pro (safe)
    "ad-blocker", "adblockerproshield", "beermoney", "heroadblocker",
    "humanizeai", "keenetic", "labelpeelers", "popupblockermax", "promptify",
    # .kit (safe)
    "agentsgrowthacademy", "alliescraps", "builtwith", "lifeovercs",
    "makeanddocrew", "marketingwords", "natashaskitchen", "ruralmeanings",
    # other confirmed
    "apkmody", "bollyflix", "coinmaster", "coomer", "coursera", "credamo",
    "dafont", "daveandbusters", "discourse", "disneystore", "dominos",
    "doritedonuts", "elsevier", "eporner", "erome", "flixhq", "flixtor",
    "fmhy", "fmovies", "gdflix", "getyourguide", "hardrock", "hdtoday",
    "hdzog", "hotmovs", "incestflix", "libgen", "liquidweb", "mangago",
    "mcdonalds", "moviesjoy", "mugshots", "mybkexperience", "nvidia",
    "oregonstate", "osmows", "pornpics", "primewire", "soap2day",
    "solarmovies", "spankbang", "stream2watch", "streamflix", "thestreameast",
    "thewiki", "txxx", "umamusume", "upornia", "watchwrestling", "weakauras",
    "winz", "xmind", "xvideos", "y2mate", "yt1s", "ziprecruiter",
    "beardpapas", "bigbowl", "themeltdown", "publicmarketemeryville",
    "acosta", "netpeak", "revcontent", "beeg", "befuck", "10minutemail",
    "123moviesfree", "1tamilblasters", "blackbaud", "charlixcx", "clickworker",
    "meditech", "discoversouthcarolina"
  )
  
  brand_base <- sub("\\..*", "", formatted_unique)
  is_brand <- brand_base %in% MERGE_BRANDS & grepl("\\.", formatted_unique) & !is.na(formatted_unique)
  formatted_unique[is_brand] <- brand_base[is_brand]
  
  # --- Map back to full data ---
  lookup <- setNames(formatted_unique, unique_vals)
  df[[paste0(field, "_high_level")]] <- as.character(lookup[websites])
  
  if (field == "website_aggregated") {
    df$website_aggregated_high_level <- df[[paste0(field, "_high_level")]]
  }
  
  df
}

get_aggregated_time_data_with_privacy_info <- function() {
  privacy_info <- get_privacy_info_wide()
  
  time_exposure_data <- get_clean_time_data()
  exposed_websites <- time_exposure_data %>% filter(privacy_exist) %>%
    group_by(experiment_id, post) %>%
    mutate(
      total_time_spent = sum(time_spent),
      total_visits = sum(visit_count),
      num_websites = n_distinct(website_aggregated_high_level),
    ) %>%
    ungroup() %>%
    group_by(experiment_id, website_aggregated_high_level, post) %>%
    summarise(
      time_spent = sum(time_spent),
      time_share = time_spent / first(total_time_spent),
      visit_count = sum(visit_count),
      visit_share = visit_count / first(total_visits),
    ) %>% ungroup()
  
  exposed_norm <- exposed_websites %>%
    mutate(domain_aggregated_high_level = str_to_lower(website_aggregated_high_level))
  
  time_data_with_privacy_info <- exposed_norm %>% left_join(privacy_info, by="domain_aggregated_high_level")
  return(time_data_with_privacy_info)
  
}

compute_exposed_privacy_scores <- function(df) {
  
  set.seed(1992)
  # === FIX: Remove pre-existing columns to avoid .x .y conflict ===
  df <- df %>% select(-any_of(c("exposed_privacy_score", "n_info_exposures")))
  
  event_logs <- get_event_logs()
  id_cols <- c("experiment_id", "website_aggregated_high_level", "date")
  
  # --- Determine cutoff date for when dialog logging started ---
  dialog_logging_start <- as_date("2025-07-21")
  # --- Prepare p_ and beta_ long format ---
  p_cols <- names(df)[startsWith(names(df), "p_")]
  beta_cols <- setdiff(names(df)[startsWith(names(df), "beta_")], "beta_website_advertising")
  
  if (length(p_cols) == 0 || length(beta_cols) == 0) {
    return(df %>% mutate(exposed_privacy_score = NA_real_, n_info_exposures = NA_integer_))
  }
  
  p_long <- df %>%
    select(experiment_id, website_aggregated_high_level, all_of(p_cols)) %>%
    distinct(experiment_id, website_aggregated_high_level, .keep_all = TRUE) %>%
    pivot_longer(cols = all_of(p_cols), names_to = "attr", values_to = "p", names_prefix = "p_") %>%
    mutate(p = suppressWarnings(as.numeric(p)))
  
  beta_long <- df %>%
    group_by(experiment_id) %>%
    slice(1) %>%
    ungroup() %>%
    select(experiment_id, all_of(beta_cols)) %>%
    pivot_longer(cols = all_of(beta_cols), names_to = "attr", values_to = "beta", names_prefix = "beta_") %>%
    mutate(beta = suppressWarnings(as.numeric(beta)))
  
  # --- Pre-compute random attribute scores per experiment-site (excluding q1, q2) ---
  all_attr_scores <- p_long %>%
    inner_join(beta_long, by = c("experiment_id", "attr")) %>%
    mutate(
      rp = if_else(beta >= 0, p, 1 - p, missing = NA_real_),
      w = abs(beta)
    )
  
  # --- Process SHOWING_DIALOG events ---
  dialog_events <- event_logs %>%
    filter(event == "SHOWING_DIALOG") %>%
    select(experiment_id, tstamp, domain) %>%
    mutate(date = as_date(as_datetime(tstamp)))
  dialog_events <- aggregate_time_data(dialog_events, field = "domain")
  dialog_events <- high_level_aggregate(dialog_events, field = "domain_aggregated")
  dialog_events <- dialog_events %>%
    rename(website_aggregated_high_level = domain_aggregated_high_level) %>%
    group_by(experiment_id, date, website_aggregated_high_level) %>%
    summarise(n_dialog_events = n(), .groups = "drop")
  
  # --- Process RANDOM_INFO events ---
  random_info <- event_logs %>%
    filter(str_detect(event, "RANDOM_INFO")) %>%
    select(experiment_id, tstamp, domain, event) %>%
    mutate(
      exposed_field = str_remove(event, "^RANDOM_INFO\\s*"),
      date = as_date(as_datetime(tstamp))
    )
  random_info <- aggregate_time_data(random_info, field = "domain")
  random_info <- high_level_aggregate(random_info, field = "domain_aggregated")
  random_info <- random_info %>%
    rename(website_aggregated_high_level = domain_aggregated_high_level)
  
  # Compute random info scores per experiment-date-site
  random_info_scores <- random_info %>%
    inner_join(all_attr_scores, by = c("experiment_id", "website_aggregated_high_level", "exposed_field" = "attr")) %>%
    group_by(experiment_id, date, website_aggregated_high_level) %>%
    summarise(
      exposure_privacy_score = if (sum(w, na.rm = TRUE) > 0)
        sum(w * rp, na.rm = TRUE) / sum(w, na.rm = TRUE)
      else NA_real_,
      n_random_info = n(),
      .groups = "drop"
    )
  
  # --- Pre-compute average random attribute score per experiment-site (for saliency/control) ---
  random_attr_avg <- df %>%
    select(experiment_id, website_aggregated_high_level, q1_html_key, q2_html_key) %>%
    distinct() %>%
    left_join(all_attr_scores, by = c("experiment_id", "website_aggregated_high_level")) %>%
    filter(attr != q1_html_key & attr != q2_html_key) %>%
    group_by(experiment_id, website_aggregated_high_level) %>%
    summarise(
      random_attr_rp = mean(rp, na.rm = TRUE),
      random_attr_w = mean(w, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- Build df_augmented ---
  df_augmented <- df %>%
    mutate(.row_id = row_number()) %>%
    left_join(dialog_events, by = id_cols) %>%
    left_join(random_info_scores, by = id_cols) %>%
    left_join(random_attr_avg, by = c("experiment_id", "website_aggregated_high_level")) %>%
    mutate(
      n_dialog_events = replace_na(n_dialog_events, 0L),
      n_random_info = replace_na(n_random_info, 0L),
      has_dialog_logging = date >= dialog_logging_start
    )
  
  # --- Compute dialog privacy score ---
  df_augmented <- df_augmented %>%
    mutate(
      q1_rp = if_else(q1_preference_weight >= 0, q1_realized_privacy, 1 - q1_realized_privacy),
      q2_rp = if_else(q2_preference_weight >= 0, q2_realized_privacy, 1 - q2_realized_privacy),
      q1_w = abs(q1_preference_weight),
      q2_w = abs(q2_preference_weight),
      
      dialog_privacy_score = if_else(
        q1_w + q2_w > 0,
        (q1_w * q1_rp + q2_w * q2_rp) / (q1_w + q2_w),
        NA_real_
      )
    )
  
  # --- Determine effective n_dialog_events ---
  df_augmented <- df_augmented %>%
    mutate(
      effective_n_dialog = case_when(
        # Info with logging: use actual dialog events
        has_dialog_logging & experiment_condition == "info" & post == TRUE ~ n_dialog_events,
        
        # Info without logging: infer from random_info
        !has_dialog_logging & experiment_condition == "info" & post == TRUE ~ n_random_info,
        
        # Saliency with logging: use actual dialog events
        has_dialog_logging & experiment_condition == "saliency" & post == TRUE ~ n_dialog_events,
        
        # Everything else: simulate
        TRUE ~ rbinom(n(), visit_count, 0.5)
      )
    )
  
  # --- Compute n_info_exposures and exposed_privacy_score ---
  df_augmented <- df_augmented %>%
    mutate(
      n_info_exposures = effective_n_dialog,
      
      exposed_privacy_score = case_when(
        # INFO post: dialog score + random info exposure score
        experiment_condition == "info" & post == TRUE ~ {
          n_dialog_prefs <- effective_n_dialog * 2
          if_else(
            n_dialog_prefs + n_random_info > 0,
            (dialog_privacy_score * n_dialog_prefs + replace_na(exposure_privacy_score, 0) * n_random_info) /
              (n_dialog_prefs + n_random_info),
            NA_real_
          )
        },
        
        # SALIENCY post with logging: dialog score + random draws
        has_dialog_logging & experiment_condition == "saliency" & post == TRUE ~ {
          total_w <- q1_w * effective_n_dialog + q2_w * effective_n_dialog + random_attr_w * effective_n_dialog
          if_else(
            effective_n_dialog > 0 & total_w > 0,
            (q1_w * effective_n_dialog * q1_rp + q2_w * effective_n_dialog * q2_rp +
               random_attr_w * effective_n_dialog * random_attr_rp) / total_w,
            NA_real_
          )
        },
        
        # Everything else: simulated exposure
        TRUE ~ {
          total_w <- q1_w * effective_n_dialog + q2_w * effective_n_dialog + random_attr_w * effective_n_dialog
          if_else(
            effective_n_dialog > 0 & total_w > 0,
            (q1_w * effective_n_dialog * q1_rp + q2_w * effective_n_dialog * q2_rp +
               random_attr_w * effective_n_dialog * random_attr_rp) / total_w,
            NA_real_
          )
        }
      )
    )
  
  # --- Return original df with new columns added ---
  df %>%
    mutate(.row_id = row_number()) %>%
    left_join(
      df_augmented %>% select(.row_id, exposed_privacy_score, n_info_exposures),
      by = ".row_id"
    ) %>%
    select(-.row_id)
}

get_personalized_info_only <- function() {
  wave1_info <- read.csv(paste0(DATA_DIR, "endline_survey_top_sites_and_info/endline_survey_info_without_imputation_wave_1.csv"),
                         stringsAsFactors = FALSE)
  wave2_info <- read.csv(paste0(DATA_DIR, "endline_survey_top_sites_and_info/endline_survey_info_without_imputation_wave_2.csv"),
                         stringsAsFactors = FALSE)
  
  personalized_info <- rbind(wave1_info, wave2_info)
  
  privacy_info_desc <- read.csv(paste0(EXT_DATA_DIR, "privacy_info_desc.csv"), stringsAsFactors = FALSE)
  
  
  #Join personalized_info with privacy_info_desc to get features/fields for all 3 questions
  personalized_info <- personalized_info %>%
    # Q1 mapping
    left_join(privacy_info_desc %>% select(short_desc, feature, field),
              by = c("short_desc_1" = "short_desc")) %>%
    rename(q1_feature = feature, q1_field = field) %>%
    # Q2 mapping
    left_join(privacy_info_desc %>% select(short_desc, feature, field),
              by = c("short_desc_2" = "short_desc")) %>%
    rename(q2_feature = feature, q2_field = field) %>%
    # Random question mapping
    left_join(privacy_info_desc %>% select(short_desc, feature, field),
              by = c("short_desc_rand_1" = "short_desc")) %>%
    rename(qrand_feature = feature, qrand_field = field) %>%
    mutate(qrand_feature = tolower(qrand_feature), qrand_field = tolower(qrand_field), q2_feature = tolower(q2_feature), q2_field = tolower(q2_field), q1_field = tolower(q1_field), q1_feature = tolower(q1_feature))
  return(personalized_info)
}

get_personalized_info_long <- function() {
  personalized_info <- get_personalized_info_only()
  cat("  Loaded personalized info from both waves:", nrow(personalized_info), "participants\n")
  
  # Load in site privacy information
  privacy_info <- get_privacy_info_wide()
  
  # Privacy info to description map
  
  
  # STEP 4: Create long format - one row per TRUE website shown
  
  personalized_info_long <- personalized_info %>%
    select(experiment_id, email, experiment_condition,
           privacy_true_1, privacy_true_2, privacy_true_3, privacy_true_4, privacy_true_5,
           q1_feature, q1_field, q2_feature, q2_field, qrand_feature, qrand_field) %>%
    pivot_longer(
      cols = starts_with("privacy_true_"),
      names_to = "website_position",
      values_to = "website_high_level"
    ) %>%
    filter(!is.na(website_high_level) & website_high_level != "") %>%
    mutate(
      website_num = as.numeric(gsub("privacy_true_", "", website_position)),
      q1_html_key = paste0(q1_feature, "-", q1_field),
      q2_html_key = paste0(q2_feature, "-", q2_field),
      qrand_html_key = paste0(qrand_feature, "-", qrand_field)
      # MARK - website_num is 1-5, matching privacy_true_1 through privacy_true_5
    )
  return(personalized_info_long)
}

get_privacy_attribute_weights_by_individual <- function() {
  conjoint_utilities <- read.csv(paste0(CONJOINT_DIR, "Conjoint-Finalized/tables/individual_parameters_wide_means.csv"))
  
  conjoint_to_belief_id <- c(
    collection_log = "collect-log",
    collection_sensitive = "collect-sensitive",
    collection_financial = "collect-financial",
    collection_offsite = "collect-offsite",
    collection_location = "collect-location",
    collection_social = "collect-social",
    
    collection_bio = "collect-bio",
    use_social = "share-social",
    use_financial = "share-finance",
    use_advertising = "share-ads",
    use_law = "share-law",
    use_service = "share-service",
    usel_anonymized = "anonymized-anonymized",
    use_partners = "share-partners",
    use_personalization = "personalization-personalization",
    control_change = "change-change",
    control_automated = "automated-automated",
    control_delete = "deletion-deletion",
    control_storage = "storage-storage",
    website_advertising = "website_advertising"
  )
  
  
  conjoint_utilities_long <- conjoint_utilities %>%
    pivot_longer(
      cols = -c(RespondentId, category, N_id, category_name, website, category, category_name, website_cat, price_linear),
      names_to = "attribute_name",
      values_to = "beta_ij"
    ) %>%
    select(-c(category, N_id, category_name, website, category, category_name, website_cat, price_linear))
  
  all_utilities_processed <- conjoint_utilities_long %>%
    select(RespondentId, attribute_name, beta_ij)
  
  all_utilities_processed <- all_utilities_processed %>%
    mutate(attribute_name = conjoint_to_belief_id[attribute_name])
  
  utilities_wide <- all_utilities_processed %>%
    select(RespondentId, attribute_name, beta_ij) %>%
    pivot_wider(
      names_from = attribute_name,
      values_from = beta_ij,
      names_prefix = "beta_"
    ) %>%
    arrange(RespondentId)
  
  # baseline cleaning
  baseline_survey <- read.csv(paste0(SURVEY_DIR, "final_baseline_survey.csv"))
  
  # Dedup (remove missing)
  baseline_survey <- baseline_survey %>%
    filter(
      sys_RespStatus == 5,                                     # Must be 'Completed'
      !is.na(emailid) & trimws(emailid) != "",                 # Must have valid email
      !is.na(favoritewebsite) & trimws(favoritewebsite) != "", # Must have website
      !is.na(conjcat_conjCategory) & trimws(conjcat_conjCategory) != "", # Must have category
      Age != 5,                                                # Remove invalid age (5)
      !is.na(Age),                                             # Must have Age
      !is.na(Gender),                                          # Must have Gender
      !is.na(Education),                                       # Must have Education
      !is.na(Income),                                          # Must have Income
      (RaceSimple_1 == 1 | RaceSimple_2 == 1 | RaceSimple_3 == 1 | # At least one race == 1
         RaceSimple_4 == 1 | RaceSimple_5 == 1 | RaceSimple_6 == 1)
    )
  
  # Dedup (keep FIRST valid submission)
  baseline_survey <- baseline_survey %>%
    mutate(emailid = tolower(trimws(emailid))) %>%     # Normalize email
    group_by(emailid) %>%
    arrange(sys_EndTimeStamp) %>%                      # Sort by time (Oldest first)
    slice(1) %>%                                       # Keep the FIRST valid submission
    ungroup()
  
  baseline_survey <- baseline_survey %>%
    select(sys_RespNum, emailid) %>%
    rename(RespondentId = sys_RespNum)
  
  utilities_wide <-  utilities_wide %>% left_join(baseline_survey, by="RespondentId")
  meta_data <- read.csv(paste0(EXT_DATA_DIR, "experiment_conditions_pilot_july_2024.csv"))
  meta_data <- meta_data %>%  mutate(email = tolower(email)) %>% group_by(email) %>% arrange(tstamp) %>% slice(1) %>% ungroup()
  meta_data <- meta_data %>% select(email, experiment_id) %>% rename(emailid = email)
  utilities_wide <-  utilities_wide %>% left_join(meta_data, by="emailid")
  return(utilities_wide)
  
}


# ==============================================================================
# Get population-level privacy attribute weights from conjoint analysis
# ==============================================================================
# Uses population average parameters instead of individual-level estimates
# Returns a single row of weights that can be applied to all users
#
# Parameters:
#   average_across_categories: If TRUE (default), average weights across all 4
#                              website categories. If FALSE, returns category-specific weights.
#   category: If average_across_categories=FALSE, specify which category (1-4) to use.
#             If NULL, returns all categories.
# ==============================================================================

get_privacy_attribute_weights_population <- function(
    average_across_categories = TRUE,
    category = NULL,
    pop_params_path = paste0(CONJOINT_DIR, "Conjoint-01092026/tables/population_parameters_summary.csv")
) {
  
  # Read population parameters
  pop_params <- read.csv(pop_params_path)
  
  # Filter to beta_mean_cat (population means, not tau heterogeneity params)
  beta_means <- pop_params %>%
    filter(grepl("^beta_mean_cat", variable)) %>%
    select(category, feature_id, feature_name, mean)
  
  # Mapping from conjoint feature names to belief IDs (same as individual function)
  conjoint_to_belief_id <- c(
    collection_log = "collect-log",
    collection_sensitive = "collect-sensitive",
    collection_financial = "collect-financial",
    collection_offsite = "collect-offsite",
    collection_location = "collect-location",
    collection_social = "collect-social",
    collection_bio = "collect-bio",
    use_social = "share-social",
    use_financial = "share-finance",
    use_advertising = "share-ads",
    use_law = "share-law",
    use_service = "share-service",
    usel_anonymized = "anonymized-anonymized",
    use_partners = "share-partners",
    use_personalization = "personalization-personalization",
    control_change = "change-change",
    control_automated = "automated-automated",
    control_delete = "deletion-deletion",
    control_storage = "storage-storage",
    website_advertising = "website_advertising"
  )
  
  # Map feature names to belief IDs
  beta_means <- beta_means %>%
    mutate(
      belief_id = conjoint_to_belief_id[feature_name]
    ) %>%
    filter(!is.na(belief_id))  # Remove features not in mapping (e.g., price_linear)
  
  if (average_across_categories) {
    # Average across all 4 categories
    pop_weights <- beta_means %>%
      group_by(feature_name, belief_id) %>%
      summarise(
        beta = mean(mean, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Pivot to wide format
    weights_wide <- pop_weights %>%
      select(belief_id, beta) %>%
      pivot_wider(
        names_from = belief_id,
        values_from = beta,
        names_prefix = "beta_"
      )
    
    # Add identifier
    weights_wide <- weights_wide %>%
      mutate(weight_type = "population_average") %>%
      select(weight_type, everything())
    
  } else if (!is.null(category)) {
    # Use specific category
    pop_weights <- beta_means %>%
      filter(category == !!category) %>%
      select(belief_id, beta = mean)
    
    # Pivot to wide format
    weights_wide <- pop_weights %>%
      pivot_wider(
        names_from = belief_id,
        values_from = beta,
        names_prefix = "beta_"
      )
    
    # Add identifier
    weights_wide <- weights_wide %>%
      mutate(
        weight_type = "population",
        category = !!category
      ) %>%
      select(weight_type, category, everything())
    
  } else {
    # Return all categories separately
    pop_weights <- beta_means %>%
      select(category, belief_id, beta = mean)
    
    # Pivot to wide format
    weights_wide <- pop_weights %>%
      pivot_wider(
        names_from = belief_id,
        values_from = beta,
        names_prefix = "beta_"
      )
    
    # Add identifier
    weights_wide <- weights_wide %>%
      mutate(weight_type = "population") %>%
      select(weight_type, category, everything())
  }
  
  return(weights_wide)
}


# ==============================================================================
# Get population weights and expand to all experiment IDs
# ==============================================================================
# Creates a data frame with population weights replicated for each experiment_id
# This allows the population weights to be used in place of individual weights
# in downstream analyses that expect user-level weight data
#
# Parameters:
#   experiment_ids: Vector of experiment IDs to include. If NULL, gets all IDs
#                   from the metadata file.
#   average_across_categories: Passed to get_privacy_attribute_weights_population()
# ==============================================================================

get_privacy_attribute_weights_population_by_user <- function(
    experiment_ids = NULL,
    average_across_categories = TRUE,
    pop_params_path = paste0(CONJOINT_DIR, "Conjoint-01092026/tables/population_parameters_summary.csv")
) {
  
  # Get population weights (single row)
  pop_weights <- get_privacy_attribute_weights_population(
    average_across_categories = average_across_categories,
    pop_params_path = pop_params_path
  )
  
  # Get experiment IDs if not provided
  if (is.null(experiment_ids)) {
    meta_data <- read.csv(paste0(EXT_DATA_DIR, "experiment_conditions_pilot_july_2024.csv"))
    meta_data <- meta_data %>%
      mutate(email = tolower(email)) %>%
      group_by(email) %>%
      arrange(tstamp) %>%
      slice(1) %>%
      ungroup()
    experiment_ids <- unique(meta_data$experiment_id)
  }
  
  # Expand population weights to all users
  weights_by_user <- tibble(experiment_id = experiment_ids) %>%
    crossing(pop_weights %>% select(-weight_type))
  
  return(weights_by_user)
}