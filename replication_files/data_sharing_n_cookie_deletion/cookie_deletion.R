# =============================================================================
# cookie_deletion.R   (replication_files pipeline version)
#
# Produces the Cookie Deletion analyses for the paper appendix
# (writeup_v4.tex, "Analysis of Cookie Deletion Intervention").
#
#   SECTION 1  Did deletion happen, and what are the overall effects?
#     1.1 Cookie-deletion percentage changes                 -> fig:cookie_deletion_percentage_change
#     1.2 CPV DiD regression (2 cols: CPV + UC)             -> tab:cookie_deletion_did
#     1.3 Time DiD regression                                -> tab:time_did_regression
#     1.4 Survey-platform sites (excluded from the MAIN sample in Section 0)
#         1.4a survey-platform shares, incl. sample         -> tab:..._unaffected_sites_summary
#         1.4b robustness: DiD INCLUDING survey platforms   -> tab:..._did_excluding_unaffected
#         (table filenames / \cookieCpvExclCoef kept for tex compatibility;
#          semantics flipped: main excludes, robustness includes)
#     1.5 Baseline CPV by website category                   -> fig:cpv_baseline_by_category
#
#   SECTION 2  Is deletion plausibly random? (CPV outcome)
#     2.1 By event-log status                               -> tab:cookie_deletion_by_log_status
#     2.1b UC log-status models (feed \uc* macros)
#     2.2 By user time quintile                             -> fig:deletion_by_quintile
#     2.3 By site (top 15)                                  -> fig:deletion_by_site
#     2.4 By site category (VERTICAL)                       -> fig:deletion_by_category
#     2.4b UC by site category (VERTICAL)                    -> fig:deletion_uc_by_category
#     2.5 Quintile x log status (MAR)                       -> fig:deletion_by_quintile_log_status
#     2.6 Top-15 sites x log status (MAR)                   -> fig:deletion_by_site_log_status
#     2.7 Category x log status (MAR, VERTICAL)             -> fig:deletion_by_category_log_status
#
#   SECTION 3  Browsing-time heterogeneity
#     3.2 Time by quintile, TWO MARGINS                     -> fig:time_extensive_by_quintile
#                                                              fig:time_intensive_by_quintile
#     3.3 Time by site (top 15)                             -> fig:time_by_site
#     3.4 Time by category (VERTICAL)                       -> fig:time_by_category
#     3.5 Time quintile x log status (MAR)                  -> fig:time_by_quintile_log_status
#     3.6 Time sites x log status (MAR)                     -> fig:time_by_site_log_status
#     3.7 Time category x log status (MAR, VERTICAL)        -> fig:time_by_category_log_status
#
#   Inline scalars cited in E prose:
#     output/values/data_sharing_cookie_values.tex
#       \cookieCpvCoef \cookieCpvPct \cookieCpvExclCoef
#       \cookieTimeCoef \cookieTimePct
#       \cookieHasLogCoef \cookieNoLogCoef \noLogPct
#       \unaffCookieSharePre \unaffCookieSharePost
#       \unaffVisitSharePre \unaffVisitSharePost
#       \unaffTimeSharePre \unaffTimeSharePost
#       \timeExtQOneCoef \timeExtQOnePval
#       \timeIntQOneCoef \timeIntQOnePval
#       \qOneZeroDayPct \baseCatTopMedCpv \baseCatBottomMedCpv
#       \ucMainCoef \ucMainPct \ucHasLogCoef \ucNoLogCoef
#       \nSigCpvCategories \nSigNegCpvCategories
#       \nSigPosCpvCategories
#       \nSigNegUcCategories \nSigPosUcCategories
#       \nCpvCategories \nSigTimeSites \nNegTimeSites
#       \nTimeSites \nSigTimeCategories
#
#     output/values/data_sharing_cookie_str_values.tex
#       \sigPosCpvCategory
#
# DESIGN:
#   Two 2x2 DiDs combined through event-time alignment.
#   The early deletion group is compared with the late deletion group during
#   the one-week interval before the late group receives deletion.
#
#   Event time:
#     tau = date - early-deletion date for the participant's experimental wave
#
#   Window:
#     tau in [-7, 6]
#
# CHANGES this version:
#   - Replaced the four separate early/late CPV and browsing-time trajectory
#     figures with one event-study figure showing daily effects on CPV and
#     recorded browsing time.
#   - Both experimental waves enter the same early-versus-late specification.
#   - The figure reports estimated percentage changes rather than separate
#     treatment-group trajectories.
#   - Cookie outcomes use the rebuilt cookie panel.
#   - CPV and unique-cookie specifications use participant and website fixed
#     effects without day-of-week fixed effects.
#   - Browsing-time specifications retain day-of-week fixed effects.
# =============================================================================

library(jsonlite)
library(data.table)
library(fst)
library(fixest)
library(ggplot2)
library(savetexvalue)

setwd("~/Dropbox/spring2025experiment/code_github")

source("replication_files/utils/values.R")
source("replication_files/utils/time_usage_helpers.R")
source("replication_files/utils/number_format_helpers.R")
source("replication_files/utils/tex_helpers.R")
source("replication_files/utils/plot_rules.R")

select <- dplyr::select

FIGURES_DIR <- "output/figures/"
TABLES_DIR  <- "output/tables/"
VALUES_DIR  <- "output/values/"

# Cookie-deletion-specific bad users.
BAD_USERS <- union(
  BAD_USERS,
  c("6ccc7d5", "7d6864c")
)


# =============================================================================
# CONSTANTS
# =============================================================================

TAU_MIN     <- -7
TAU_MAX     <- 6
VLINE_X     <- -0.5
N_TOP_SITES <- 15

FIG_W                <- 8
FIG_H_QUINT          <- 5
FIG_H_CAT_VERT       <- 5
FIG_H_WIDE_SINGLE    <- 6.5
FIG_H_WIDE_LOGSTATUS <- 8.5

POINT_COLOR <- "gray30"

LOG_SHAPES <- c(
  "No Log"  = 16,
  "Has Log" = 17
)

rot_x <- theme(
  axis.text.x = element_text(
    angle = 45,
    hjust = 1
  )
)


# =============================================================================
# PLOT HELPERS
# =============================================================================

plot_coef <- function(
    dt,
    mode = c("vertical", "horizontal"),
    value_lab,
    group_lab
) {
  mode <- match.arg(mode)
  
  if (mode == "vertical") {
    ggplot(dt, aes(x = grp, y = coef)) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "gray50"
      ) +
      geom_errorbar(
        aes(ymin = ci_lo, ymax = ci_hi),
        width = ERRORBAR_WIDTH,
        linewidth = LINE_WIDTH,
        color = POINT_COLOR
      ) +
      geom_point(
        size = POINT_SIZE,
        color = POINT_COLOR
      ) +
      labs(
        x = group_lab,
        y = value_lab
      ) +
      theme_privacy_experiment(
        show_grid_x = FALSE,
        show_grid_y = TRUE
      )
  } else {
    ggplot(dt, aes(x = coef, y = grp)) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        color = "gray50"
      ) +
      geom_errorbarh(
        aes(xmin = ci_lo, xmax = ci_hi),
        height = 0.25,
        linewidth = LINE_WIDTH,
        color = POINT_COLOR
      ) +
      geom_point(
        size = POINT_SIZE,
        color = POINT_COLOR
      ) +
      labs(
        x = value_lab,
        y = group_lab
      ) +
      theme_privacy_experiment(
        show_grid_x = TRUE,
        show_grid_y = FALSE
      )
  }
}

plot_coef_logstatus <- function(
    dt,
    mode = c("vertical", "horizontal"),
    value_lab,
    group_lab
) {
  mode <- match.arg(mode)
  
  if (mode == "vertical") {
    ggplot(
      dt,
      aes(
        x = grp,
        y = coef,
        shape = log_status,
        group = log_status
      )
    ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "gray50"
      ) +
      geom_errorbar(
        aes(ymin = ci_lo, ymax = ci_hi),
        width = ERRORBAR_WIDTH,
        linewidth = LINE_WIDTH,
        color = POINT_COLOR,
        position = position_dodge(width = DODGE_WIDTH_2)
      ) +
      geom_point(
        size = POINT_SIZE,
        color = POINT_COLOR,
        position = position_dodge(width = DODGE_WIDTH_2)
      ) +
      scale_shape_manual(values = LOG_SHAPES) +
      labs(
        x = group_lab,
        y = value_lab,
        shape = NULL
      ) +
      theme_privacy_experiment(
        show_grid_x = FALSE,
        show_grid_y = TRUE
      )
  } else {
    ggplot(
      dt,
      aes(
        x = coef,
        y = grp,
        shape = log_status,
        group = log_status
      )
    ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        color = "gray50"
      ) +
      geom_errorbarh(
        aes(xmin = ci_lo, xmax = ci_hi),
        height = 0.25,
        linewidth = LINE_WIDTH,
        color = POINT_COLOR,
        position = position_dodge(width = DODGE_WIDTH_2)
      ) +
      geom_point(
        size = POINT_SIZE,
        color = POINT_COLOR,
        position = position_dodge(width = DODGE_WIDTH_2)
      ) +
      scale_shape_manual(values = LOG_SHAPES) +
      labs(
        x = value_lab,
        y = group_lab,
        shape = NULL
      ) +
      theme_privacy_experiment(
        show_grid_x = TRUE,
        show_grid_y = FALSE
      )
  }
}

DICT_CPV <- c(
  post_treated = "Post $\\times$ Cookie Deletion",
  experiment_id = "Participant FE",
  website = "Website FE",
  dow = "Day-of-Week FE",
  log_cpv_3p = "log(CPV)",
  log_uc = "log(Unique 3rd Party Cookies)"
)

DICT_TIME <- c(
  post_treated = "Post $\\times$ Cookie Deletion",
  experiment_id = "Participant FE",
  website = "Website FE",
  dow = "Day-of-Week FE",
  log_time = "log(Daily Browsing Time)"
)

SIGNIF <- c(
  "***" = 0.01,
  "**" = 0.05,
  "*" = 0.1
)

TIME_LAB <- "Estimated Effect on log(Daily Browsing Time)"

QUINT_LAB <- paste0(
  "User Time Quintile ",
  "(Q1 = Lowest pre-period time spent)"
)


# =============================================================================
# 0. DATA PREPARATION
# =============================================================================

panel <- read_fst(
  "../data/tracker_panel/panel_merged_CLEAN.fst",
  as.data.table = TRUE
)

panel[, date := as.Date(date)]

panel[
  ,
  c(
    "n_cookies_third_party",
    "n_trackers_third_party",
    "cookies_per_visit"
  ) := NULL
]

panel <- unique(
  panel,
  by = c(
    "experiment_id",
    "website",
    "date"
  )
)

cookies <- read_fst(
  "../data/processed_data/panel_cookies_v2.fst",
  as.data.table = TRUE
)

panel <- merge(
  panel,
  cookies[
    ,
    .(
      experiment_id,
      website,
      date,
      cookie_events_3rd_p,
      unique_cookies_3rd_p
    )
  ],
  by = c(
    "experiment_id",
    "website",
    "date"
  ),
  all.x = TRUE
)

panel[
  is.na(cookie_events_3rd_p),
  cookie_events_3rd_p := 0L
]

panel[
  is.na(unique_cookies_3rd_p),
  unique_cookies_3rd_p := 0L
]

rm(cookies)
gc(verbose = FALSE)

ec <- fread(
  paste0(
    "../data/final_extension_data/",
    "experiment_conditions_pilot_july_2024.csv"
  )
)

ec_clean <- ec[
  in_experiment == "true" &
    !experiment_id %in% BAD_USERS
]

ec_clean[
  wave_id == 3,
  wave_id := 2L
]

drop_cols <- intersect(
  c(
    "wave_id",
    "treatment",
    "experiment_condition"
  ),
  names(panel)
)

if (length(drop_cols) > 0) {
  panel[, (drop_cols) := NULL]
}

panel <- panel[
  experiment_id %in% ec_clean$experiment_id
]

panel <- merge(
  panel,
  ec_clean[
    ,
    .(
      experiment_id,
      wave_id,
      experiment_condition
    )
  ],
  by = "experiment_id",
  all.x = TRUE
)

panel[
  wave_id == 3,
  wave_id := 2L
]

panel[
  ,
  wave_start := fifelse(
    wave_id == 1L,
    as.Date("2025-06-14"),
    as.Date("2025-06-28")
  )
]

panel[
  ,
  c1_anchor := fifelse(
    wave_id == 1L,
    as.Date("2025-07-26"),
    as.Date("2025-08-09")
  )
]

panel[
  ,
  c2_anchor := fifelse(
    wave_id == 1L,
    as.Date("2025-08-02"),
    as.Date("2025-08-16")
  )
]

panel[
  ,
  tau := as.integer(date - c1_anchor)
]

# Identify survey-platform websites.
site_df <- data.frame(
  website = unique(panel$website),
  stringsAsFactors = FALSE
)

site_df <- aggregate_time_data(
  site_df,
  field = "website"
)

site_df <- high_level_aggregate(
  site_df,
  field = "website_aggregated"
)

site_lookup <- as.data.table(
  unique(
    site_df[
      ,
      c(
        "website",
        "website_aggregated_high_level"
      )
    ]
  )
)

panel <- merge(
  panel,
  site_lookup,
  by = "website",
  all.x = TRUE
)

panel[
  ,
  is_survey :=
    tolower(website_aggregated_high_level) %in%
    SURVEY_WEBSITES
]

panel_full <- copy(panel)

# Main analysis excludes survey-platform websites.
panel <- panel[
  is_survey == FALSE
]

# Website categories.
domain_class <- get_domain_classification()
setDT(domain_class)

domain_class_slim <- unique(
  domain_class[
    ,
    .(
      website_agg = name_aggregated_high_level,
      category = category_level_1
    )
  ]
)

panel <- merge(
  panel,
  domain_class_slim,
  by.x = "website_aggregated_high_level",
  by.y = "website_agg",
  all.x = TRUE
)

# Main analysis sample.
t1 <- panel[
  tau >= TAU_MIN &
    tau <= TAU_MAX &
    !is.na(visit_count) &
    visit_count > 0
]

# Full sample including survey-platform websites.
t_full <- panel_full[
  tau >= TAU_MIN &
    tau <= TAU_MAX &
    !is.na(visit_count) &
    visit_count > 0
]

t_full[
  ,
  cpv_3p := cookie_events_3rd_p / visit_count
]

t_full[
  ,
  log_cpv_3p := log(1 + cpv_3p)
]

t_full[
  ,
  treated := as.integer(cookie_treatment_idx == 1)
]

t_full[
  ,
  post := as.integer(tau >= 0)
]

t_full[
  ,
  post_treated := post * treated
]

t1[
  ,
  cpv_3p := cookie_events_3rd_p / visit_count
]

t1[
  ,
  log_cpv_3p := log(1 + cpv_3p)
]

t1[
  ,
  log_uc := log(1 + unique_cookies_3rd_p)
]

t1[
  ,
  log_time := log(1 + time_spent)
]

t1[
  ,
  treated := as.integer(cookie_treatment_idx == 1)
]

t1[
  ,
  post := as.integer(tau >= 0)
]

t1[
  ,
  post_treated := post * treated
]

t1[
  ,
  dow := factor(weekdays(date))
]

# Whether a participant has any automatic-cookie-deletion log.
events <- fread(
  "../data/final_extension_data/event_logs.csv",
  select = c(
    "experiment_id",
    "event"
  )
)

del_users <- unique(
  events[
    grepl("^AUTOMATIC_COOKIE_DELETION", event),
    experiment_id
  ]
)

t1[
  ,
  has_log := as.integer(
    experiment_id %in% del_users
  )
]

rm(events)
gc(verbose = FALSE)

# Categories used consistently across category figures.
t1_cat <- t1[
  !is.na(category) &
    category != ""
]

big_cats <- t1_cat[
  ,
  .(
    n_obs = .N,
    n_users = uniqueN(experiment_id)
  ),
  by = category
][
  n_obs >= 500 &
    n_users >= 50,
  category
]

# Top 15 websites used across site-level analyses.
site_pre_time <- t1[
  post == 0 &
    !is.na(time_spent) &
    time_spent > 0,
  .(
    total_time_sec = sum(time_spent)
  ),
  by = website
][
  order(-total_time_sec)
]

site_8cell <- t1[
  ,
  .(
    c1 = uniqueN(
      experiment_id[
        has_log == 0 &
          post == 0 &
          treated == 0
      ]
    ),
    c2 = uniqueN(
      experiment_id[
        has_log == 0 &
          post == 0 &
          treated == 1
      ]
    ),
    c3 = uniqueN(
      experiment_id[
        has_log == 0 &
          post == 1 &
          treated == 0
      ]
    ),
    c4 = uniqueN(
      experiment_id[
        has_log == 0 &
          post == 1 &
          treated == 1
      ]
    ),
    c5 = uniqueN(
      experiment_id[
        has_log == 1 &
          post == 0 &
          treated == 0
      ]
    ),
    c6 = uniqueN(
      experiment_id[
        has_log == 1 &
          post == 0 &
          treated == 1
      ]
    ),
    c7 = uniqueN(
      experiment_id[
        has_log == 1 &
          post == 1 &
          treated == 0
      ]
    ),
    c8 = uniqueN(
      experiment_id[
        has_log == 1 &
          post == 1 &
          treated == 1
      ]
    )
  ),
  by = website
]

site_8cell[
  ,
  min_cell := pmin(
    c1,
    c2,
    c3,
    c4,
    c5,
    c6,
    c7,
    c8
  )
]

site_8cell[
  ,
  pass_8cell := min_cell >= 5
]

ranked_sites <- merge(
  site_pre_time,
  site_8cell,
  by = "website",
  all.x = TRUE
)[
  order(-total_time_sec)
]

top15_sites <- ranked_sites[
  pass_8cell == TRUE
][
  1:N_TOP_SITES,
  website
]


# =============================================================================
# SECTION 1: DID DELETION HAPPEN, AND WHAT ARE THE OVERALL EFFECTS?
# =============================================================================

# --- 1.1 Cookie-deletion percentage changes --------------------------------
#         -> fig:cookie_deletion_percentage_change
#
# Both experimental waves enter one event-time specification. The early
# deletion group is treated and the late deletion group is the clean control
# during tau in [-7, 6]. The figure reports daily treatment effects on CPV and
# recorded browsing time as percentage changes relative to tau = -1.
#
# The completed participant-day panel preserves days on which the server
# received no browsing records.

panel_time <- panel[
  !is.na(time_spent) &
    time_spent >= 0 &
    !is.na(visit_count) &
    visit_count > 0
]

ud_obs <- t1[
  ,
  .(
    total_time = sum(
      time_spent,
      na.rm = TRUE
    )
  ),
  by = .(
    experiment_id,
    tau
  )
]

ud_meta <- unique(
  t1[
    ,
    .(
      experiment_id,
      treated,
      c1_anchor
    )
  ]
)

ud <- merge(
  CJ(
    experiment_id = ud_meta$experiment_id,
    tau = TAU_MIN:TAU_MAX
  ),
  ud_meta,
  by = "experiment_id"
)

ud <- merge(
  ud,
  ud_obs,
  by = c(
    "experiment_id",
    "tau"
  ),
  all.x = TRUE
)

ud[
  ,
  total_time := fifelse(
    is.na(total_time),
    0,
    total_time
  )
]

ud[
  ,
  post := as.integer(tau >= 0)
]

ud[
  ,
  post_treated := post * treated
]

ud[
  ,
  any_browse := as.integer(total_time > 0)
]

ud[
  ,
  log_daily_time := log1p(total_time)
]

ud[
  ,
  cal_date := c1_anchor + tau
]

m_cpv_event <- feols(
  log_cpv_3p ~
    i(tau, treated, ref = -1) |
    experiment_id +
    website +
    date,
  data = t1,
  cluster = ~experiment_id,
  notes = FALSE
)

m_time_event <- feols(
  log_daily_time ~
    i(tau, treated, ref = -1) |
    experiment_id +
    cal_date,
  data = ud,
  cluster = ~experiment_id,
  notes = FALSE
)

extract_event_effects <- function(
    model,
    outcome_label
) {
  terms <- grep(
    "^tau::-?[0-9]+:treated$",
    names(coef(model)),
    value = TRUE
  )
  
  out <- data.table(
    tau = as.integer(
      sub(
        "^tau::(-?[0-9]+):treated$",
        "\\1",
        terms
      )
    ),
    coefficient = unname(
      coef(model)[terms]
    ),
    outcome = outcome_label
  )
  
  out[
    ,
    percent_change :=
      100 * (exp(coefficient) - 1)
  ]
  
  # tau = -1 is the omitted reference period.
  out <- rbind(
    out,
    data.table(
      tau = -1L,
      coefficient = 0,
      outcome = outcome_label,
      percent_change = 0
    )
  )
  
  out[
    order(tau)
  ]
}

outcome_order <- c(
  "Third-Party Cookies per Visit",
  "Daily Browsing Time"
)

event_plot <- rbind(
  extract_event_effects(
    m_cpv_event,
    outcome_order[1]
  ),
  extract_event_effects(
    m_time_event,
    outcome_order[2]
  )
)

event_plot[
  ,
  outcome := factor(
    outcome,
    levels = outcome_order
  )
]

outcome_lines <- c(
  "Third-Party Cookies per Visit" = "solid",
  "Daily Browsing Time" = "dashed"
)

g_cookie_event <- ggplot(
  event_plot,
  aes(
    x = tau,
    y = percent_change,
    group = outcome,
    linetype = outcome
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_vline(
    xintercept = VLINE_X,
    linetype = "solid",
    color = "gray50"
  ) +
  annotate(
    "text",
    x = VLINE_X + 0.1,
    y = Inf,
    label = "Deletion begins",
    hjust = 0,
    vjust = 1.5,
    size = 4.2,
    color = TEXT_COLOR
  ) +
  geom_line(
    linewidth = LINE_WIDTH,
    color = POINT_COLOR
  ) +
  scale_x_continuous(
    breaks = TAU_MIN:TAU_MAX
  ) +
  scale_y_continuous(
    labels = function(x) {
      paste0(
        scales::number(
          x,
          accuracy = 1
        ),
        "%"
      )
    }
  ) +
  scale_linetype_manual(
    name = "Outcome",
    values = outcome_lines,
    breaks = outcome_order
  ) +
  labs(
    x = paste0(
      "Days Relative to Treatment-Group ",
      "Deletion Start"
    ),
    y = "Estimated Percent Change"
  ) +
  theme_privacy_experiment(
    show_grid_x = TRUE,
    show_grid_y = TRUE
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = grid::unit(
      1.5,
      "cm"
    )
  )

ggsave(
  paste0(
    FIGURES_DIR,
    "cookie_deletion_percentage_change.pdf"
  ),
  g_cookie_event,
  width = FIG_W,
  height = 5
)

cat(
  "Saved: cookie_deletion_percentage_change.pdf\n"
)


# --- 1.2 CPV DiD regression -------------------------------------------------
#         -> tab:cookie_deletion_did

m_pool <- feols(
  log_cpv_3p ~ post_treated |
    experiment_id +
    website,
  data = t1,
  cluster = ~experiment_id,
  notes = FALSE
)

m_uc <- feols(
  log_uc ~ post_treated |
    experiment_id +
    website,
  data = t1,
  cluster = ~experiment_id,
  notes = FALSE
)

write_tabular_only(
  etable(
    m_pool,
    m_uc,
    dict = DICT_CPV,
    digits = 3,
    signif.code = SIGNIF,
    tex = TRUE
  ),
  file = paste0(
    TABLES_DIR,
    "cookie_deletion_did_regression.tex"
  )
)


# --- 1.3 Time DiD regression ------------------------------------------------
#         -> tab:time_did_regression

did_time <- panel_time[
  tau >= TAU_MIN &
    tau <= TAU_MAX
]

did_time[
  ,
  `:=`(
    treated = as.integer(
      cookie_treatment_idx == 1L
    ),
    post = as.integer(tau >= 0)
  )
]

did_time[
  ,
  post_treated := post * treated
]

did_time[
  ,
  log_time := log(1 + time_spent)
]

did_time[
  ,
  dow := factor(weekdays(date))
]

m_time_pool <- feols(
  log_time ~ post_treated |
    experiment_id +
    website +
    dow,
  data = did_time,
  cluster = ~experiment_id,
  notes = FALSE
)

write_tabular_only(
  etable(
    m_time_pool,
    dict = DICT_TIME,
    digits = 3,
    signif.code = SIGNIF,
    tex = TRUE
  ),
  file = paste0(
    TABLES_DIR,
    "time_did_regression.tex"
  )
)


# --- 1.4a Survey-platform shares --------------------------------------------

share_pp <- t_full[
  ,
  .(
    total_visits = sum(visit_count),
    unaff_visits = sum(
      visit_count[is_survey]
    ),
    total_time = sum(
      time_spent,
      na.rm = TRUE
    ),
    unaff_time = sum(
      time_spent[is_survey],
      na.rm = TRUE
    ),
    total_cookies = sum(
      cookie_events_3rd_p,
      na.rm = TRUE
    ),
    unaff_cookies = sum(
      cookie_events_3rd_p[is_survey],
      na.rm = TRUE
    )
  ),
  by = post
]

share_pp[
  ,
  `:=`(
    visit_share =
      100 * unaff_visits / total_visits,
    time_share =
      100 * unaff_time / total_time,
    cookie_share =
      100 * unaff_cookies / total_cookies
  )
]

get_share <- function(metric, p) {
  share_pp[
    post == p,
    get(metric)
  ]
}

share_rows <- data.table(
  Metric = c(
    "Visit Share",
    "Time Share",
    "Cookie Share"
  ),
  Pre = c(
    get_share("visit_share", 0),
    get_share("time_share", 0),
    get_share("cookie_share", 0)
  ),
  Post = c(
    get_share("visit_share", 1),
    get_share("time_share", 1),
    get_share("cookie_share", 1)
  )
)

share_rows[
  ,
  Change := Post - Pre
]

share_lines <- c(
  "\\begin{tabular}{lccc}",
  "\\tabularnewline \\midrule \\midrule",
  "Metric & Pre & Post & Change \\\\",
  "\\midrule",
  vapply(
    seq_len(nrow(share_rows)),
    function(i) {
      r <- share_rows[i]
      
      sprintf(
        "%s & %.2f\\%% & %.2f\\%% & %+.2fpp \\\\",
        r$Metric,
        r$Pre,
        r$Post,
        r$Change
      )
    },
    character(1)
  ),
  "\\midrule \\midrule",
  "\\end{tabular}"
)

writeLines(
  share_lines,
  paste0(
    TABLES_DIR,
    "cookie_deletion_unaffected_sites_summary.tex"
  )
)

cat(
  "Saved (tabular only):",
  paste0(
    TABLES_DIR,
    "cookie_deletion_unaffected_sites_summary.tex"
  ),
  "\n"
)


# --- 1.4b Robustness including survey platforms -----------------------------

m_pool_excl <- feols(
  log_cpv_3p ~ post_treated |
    experiment_id +
    website,
  data = t_full,
  cluster = ~experiment_id,
  notes = FALSE
)

write_tabular_only(
  etable(
    m_pool_excl,
    dict = DICT_CPV,
    digits = 3,
    signif.code = SIGNIF,
    tex = TRUE
  ),
  file = paste0(
    TABLES_DIR,
    "cookie_deletion_did_excluding_unaffected.tex"
  )
)


# --- 1.5 Baseline CPV by website category ----------------------------------

site_base <- t1[
  post == 0 &
    category %in% big_cats,
  .(
    site_mean_cpv = mean(
      cpv_3p,
      na.rm = TRUE
    )
  ),
  by = .(
    website,
    category
  )
]

base_stats <- site_base[
  ,
  .(
    p25 = quantile(
      site_mean_cpv,
      0.25
    ),
    med = median(site_mean_cpv),
    p75 = quantile(
      site_mean_cpv,
      0.75
    )
  ),
  by = category
][
  order(med)
]

base_stats[
  ,
  grp := factor(
    category,
    levels = category
  )
]

p_base <- ggplot(
  base_stats,
  aes(
    x = grp,
    y = med
  )
) +
  geom_errorbar(
    aes(
      ymin = p25,
      ymax = p75
    ),
    width = ERRORBAR_WIDTH,
    linewidth = LINE_WIDTH,
    color = POINT_COLOR
  ) +
  geom_point(
    size = POINT_SIZE,
    color = POINT_COLOR
  ) +
  labs(
    x = NULL,
    y = paste0(
      "Baseline Third-Party ",
      "Cookies per Visit"
    )
  ) +
  theme_privacy_experiment(
    show_grid_x = FALSE,
    show_grid_y = TRUE
  ) +
  rot_x

ggsave(
  paste0(
    FIGURES_DIR,
    "cpv_baseline_by_category.pdf"
  ),
  p_base,
  width = FIG_W,
  height = FIG_H_CAT_VERT
)

cat(
  "Saved: cpv_baseline_by_category.pdf\n"
)


# =============================================================================
# SECTION 2: IS COOKIE DELETION PLAUSIBLY RANDOM?
# =============================================================================

# --- 2.1 By event-log status ------------------------------------------------

m_all <- feols(
  log_cpv_3p ~ post_treated |
    experiment_id +
    website,
  data = t1,
  cluster = ~experiment_id,
  notes = FALSE
)

m_has <- feols(
  log_cpv_3p ~ post_treated |
    experiment_id +
    website,
  data = t1[has_log == 1],
  cluster = ~experiment_id,
  notes = FALSE
)

m_no <- feols(
  log_cpv_3p ~ post_treated |
    experiment_id +
    website,
  data = t1[has_log == 0],
  cluster = ~experiment_id,
  notes = FALSE
)

write_tabular_only(
  etable(
    m_all,
    m_has,
    m_no,
    headers = c(
      "All Users",
      "Has Deletion Log",
      "No Deletion Log"
    ),
    dict = DICT_CPV,
    digits = 3,
    signif.code = SIGNIF,
    tex = TRUE
  ),
  file = paste0(
    TABLES_DIR,
    "cookie_deletion_by_log_status.tex"
  )
)


# --- 2.1b Unique cookies by event-log status --------------------------------

m_uc_has <- feols(
  log_uc ~ post_treated |
    experiment_id +
    website,
  data = t1[has_log == 1],
  cluster = ~experiment_id,
  notes = FALSE
)

m_uc_no <- feols(
  log_uc ~ post_treated |
    experiment_id +
    website,
  data = t1[has_log == 0],
  cluster = ~experiment_id,
  notes = FALSE
)


# --- 2.2 CPV by user time quintile ------------------------------------------

user_pre_time <- t1[
  post == 0,
  .(
    pre_total_time = sum(
      time_spent,
      na.rm = TRUE
    )
  ),
  by = experiment_id
][
  !is.na(pre_total_time) &
    pre_total_time > 0
]

user_pre_time[
  ,
  time_quintile := cut(
    pre_total_time,
    breaks = quantile(
      pre_total_time,
      0:5 / 5,
      na.rm = TRUE
    ),
    labels = paste0("Q", 1:5),
    include.lowest = TRUE
  )
]

t1_q <- merge(
  t1,
  user_pre_time[
    ,
    .(
      experiment_id,
      time_quintile
    )
  ],
  by = "experiment_id",
  all.x = TRUE
)[
  !is.na(time_quintile)
]


# --- Subgroup-estimation helpers --------------------------------------------

fit_by_group <- function(
    data,
    groups,
    group_col,
    yvar,
    fe_with_website = TRUE,
    fe_dow = TRUE
) {
  fe <- if (fe_with_website) {
    "experiment_id + website"
  } else {
    "experiment_id"
  }
  
  if (fe_dow) {
    fe <- paste(
      fe,
      "+ dow"
    )
  }
  
  fml <- as.formula(
    sprintf(
      "%s ~ post_treated | %s",
      yvar,
      fe
    )
  )
  
  out <- lapply(
    groups,
    function(g) {
      sub <- data[
        get(group_col) == g
      ]
      
      m <- tryCatch(
        feols(
          fml,
          data = sub,
          cluster = ~experiment_id,
          notes = FALSE
        ),
        error = function(e) NULL
      )
      
      if (
        is.null(m) ||
        !"post_treated" %in% names(coef(m))
      ) {
        return(NULL)
      }
      
      ci <- confint(
        m,
        "post_treated",
        level = 0.95
      )
      
      data.table(
        grp_raw = as.character(g),
        coef = coef(m)["post_treated"],
        se = se(m)["post_treated"],
        p = pvalue(m)["post_treated"],
        ci_lo = as.numeric(ci[1, 1]),
        ci_hi = as.numeric(ci[1, 2])
      )
    }
  )
  
  rbindlist(out)
}

fit_by_group_logstatus <- function(
    data,
    groups,
    group_col,
    yvar,
    fe_with_website = TRUE,
    fe_dow = TRUE
) {
  fe <- if (fe_with_website) {
    "experiment_id + website"
  } else {
    "experiment_id"
  }
  
  if (fe_dow) {
    fe <- paste(
      fe,
      "+ dow"
    )
  }
  
  fml <- as.formula(
    sprintf(
      "%s ~ post_treated | %s",
      yvar,
      fe
    )
  )
  
  out <- list()
  
  for (g in groups) {
    for (lg in c(0, 1)) {
      sub <- data[
        get(group_col) == g &
          has_log == lg
      ]
      
      m <- tryCatch(
        feols(
          fml,
          data = sub,
          cluster = ~experiment_id,
          notes = FALSE
        ),
        error = function(e) NULL
      )
      
      if (
        is.null(m) ||
        !"post_treated" %in% names(coef(m))
      ) {
        next
      }
      
      ci <- confint(
        m,
        "post_treated",
        level = 0.95
      )
      
      out[[paste(g, lg)]] <- data.table(
        grp_raw = as.character(g),
        log_status = if (lg == 1) {
          "Has Log"
        } else {
          "No Log"
        },
        coef = coef(m)["post_treated"],
        se = se(m)["post_treated"],
        p = pvalue(m)["post_treated"],
        ci_lo = as.numeric(ci[1, 1]),
        ci_hi = as.numeric(ci[1, 2])
      )
    }
  }
  
  res <- rbindlist(out)
  
  res[
    ,
    log_status := factor(
      log_status,
      levels = c(
        "No Log",
        "Has Log"
      )
    )
  ]
  
  res
}


# --- CPV by user time quintile ----------------------------------------------

q_dt <- fit_by_group(
  t1_q,
  paste0("Q", 1:5),
  "time_quintile",
  "log_cpv_3p",
  fe_dow = FALSE
)

q_dt[
  ,
  grp := factor(
    grp_raw,
    levels = paste0("Q", 1:5)
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "cpv_heterogeneity_by_user_quintile.pdf"
  ),
  plot_coef(
    q_dt,
    "vertical",
    "Estimated Effect on log(CPV)",
    QUINT_LAB
  ),
  width = FIG_W,
  height = FIG_H_QUINT
)


# --- 2.3 CPV by website -----------------------------------------------------

site_dt <- fit_by_group(
  t1[website %in% top15_sites],
  top15_sites,
  "website",
  "log_cpv_3p",
  fe_with_website = FALSE,
  fe_dow = FALSE
)

site_dt[
  ,
  display_name := gsub(
    "^www\\.",
    "",
    grp_raw
  )
]

site_dt <- site_dt[
  order(coef)
]

site_dt[
  ,
  grp := factor(
    display_name,
    levels = rev(display_name)
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "cpv_did_by_site.pdf"
  ),
  plot_coef(
    site_dt,
    "horizontal",
    "Estimated Effect on log(CPV)",
    NULL
  ),
  width = FIG_W,
  height = FIG_H_WIDE_SINGLE
)


# --- 2.4 CPV by website category -------------------------------------------

cat_dt <- fit_by_group(
  t1_cat,
  sort(big_cats),
  "category",
  "log_cpv_3p",
  fe_dow = FALSE
)

cat_dt <- cat_dt[
  order(coef)
]

cat_dt[
  ,
  grp := factor(
    grp_raw,
    levels = grp_raw
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "cpv_heterogeneity_by_website_category.pdf"
  ),
  plot_coef(
    cat_dt,
    "vertical",
    "Estimated Effect on log(CPV)",
    NULL
  ) +
    rot_x,
  width = FIG_W,
  height = FIG_H_CAT_VERT
)


# --- 2.4b Unique cookies by website category --------------------------------

cat_uc <- fit_by_group(
  t1_cat,
  sort(big_cats),
  "category",
  "log_uc",
  fe_dow = FALSE
)

cat_uc <- cat_uc[
  order(coef)
]

cat_uc[
  ,
  grp := factor(
    grp_raw,
    levels = grp_raw
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "uc_heterogeneity_by_website_category.pdf"
  ),
  plot_coef(
    cat_uc,
    "vertical",
    paste0(
      "Estimated Effect on log",
      "(Unique 3rd Party Cookies)"
    ),
    NULL
  ) +
    rot_x,
  width = FIG_W,
  height = FIG_H_CAT_VERT
)


# --- 2.5 CPV quintile by event-log status ----------------------------------

q_log <- fit_by_group_logstatus(
  t1_q,
  paste0("Q", 1:5),
  "time_quintile",
  "log_cpv_3p",
  fe_dow = FALSE
)

q_log[
  ,
  grp := factor(
    grp_raw,
    levels = paste0("Q", 1:5)
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "cpv_heterogeneity_by_user_quintile_log_status.pdf"
  ),
  plot_coef_logstatus(
    q_log,
    "vertical",
    "Estimated Effect on log(CPV)",
    QUINT_LAB
  ),
  width = FIG_W,
  height = FIG_H_QUINT
)


# --- 2.6 CPV by website and event-log status --------------------------------

site_log <- fit_by_group_logstatus(
  t1[website %in% top15_sites],
  top15_sites,
  "website",
  "log_cpv_3p",
  fe_with_website = FALSE,
  fe_dow = FALSE
)

site_log[
  ,
  display_name := gsub(
    "^www\\.",
    "",
    grp_raw
  )
]

order_site <- site_log[
  log_status == "Has Log"
][
  order(coef),
  display_name
]

site_log[
  ,
  grp := factor(
    display_name,
    levels = rev(order_site)
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "cpv_did_by_site_log_status.pdf"
  ),
  plot_coef_logstatus(
    site_log,
    "horizontal",
    "Estimated Effect on log(CPV)",
    NULL
  ),
  width = FIG_W,
  height = FIG_H_WIDE_LOGSTATUS
)


# --- 2.7 CPV by category and event-log status -------------------------------

cat_log <- fit_by_group_logstatus(
  t1_cat,
  sort(big_cats),
  "category",
  "log_cpv_3p",
  fe_dow = FALSE
)

order_cat <- cat_log[
  log_status == "No Log"
][
  order(coef),
  grp_raw
]

cat_log[
  ,
  grp := factor(
    grp_raw,
    levels = order_cat
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "cpv_heterogeneity_by_website_category_log_status.pdf"
  ),
  plot_coef_logstatus(
    cat_log,
    "vertical",
    "Estimated Effect on log(CPV)",
    NULL
  ) +
    rot_x,
  width = FIG_W,
  height = FIG_H_CAT_VERT
)


# =============================================================================
# SECTION 3: BROWSING-TIME HETEROGENEITY
# =============================================================================

# --- 3.2 Browsing time by user quintile: two margins ------------------------

ud[
  ,
  dow := factor(weekdays(cal_date))
]

ud <- merge(
  ud,
  user_pre_time[
    ,
    .(
      experiment_id,
      time_quintile
    )
  ],
  by = "experiment_id"
)[
  !is.na(time_quintile)
]

fit_margin <- function(data, yvar) {
  out <- lapply(
    paste0("Q", 1:5),
    function(q) {
      m <- tryCatch(
        feols(
          as.formula(
            paste(
              yvar,
              "~ post_treated | experiment_id + dow"
            )
          ),
          data = data[
            time_quintile == q
          ],
          cluster = ~experiment_id,
          notes = FALSE
        ),
        error = function(e) NULL
      )
      
      if (
        is.null(m) ||
        !"post_treated" %in% names(coef(m))
      ) {
        return(NULL)
      }
      
      ci <- confint(
        m,
        "post_treated",
        level = 0.95
      )
      
      data.table(
        grp_raw = q,
        coef = coef(m)["post_treated"],
        p = pvalue(m)["post_treated"],
        ci_lo = as.numeric(ci[1, 1]),
        ci_hi = as.numeric(ci[1, 2])
      )
    }
  )
  
  res <- rbindlist(out)
  
  res[
    ,
    grp := factor(
      grp_raw,
      levels = paste0("Q", 1:5)
    )
  ]
  
  res
}

ext_dt <- fit_margin(
  ud,
  "any_browse"
)

ggsave(
  paste0(
    FIGURES_DIR,
    "time_extensive_by_quintile.pdf"
  ),
  plot_coef(
    ext_dt,
    "vertical",
    "Estimated Effect on P(Browsed That Day)",
    QUINT_LAB
  ),
  width = FIG_W,
  height = FIG_H_QUINT
)

udi <- ud[
  total_time > 0
]

udi[
  ,
  log_time_pos := log(total_time)
]

int_dt <- fit_margin(
  udi,
  "log_time_pos"
)

ggsave(
  paste0(
    FIGURES_DIR,
    "time_intensive_by_quintile.pdf"
  ),
  plot_coef(
    int_dt,
    "vertical",
    paste0(
      "Estimated Effect on log(Daily Browsing Time), ",
      "Days with Browsing"
    ),
    QUINT_LAB
  ),
  width = FIG_W,
  height = FIG_H_QUINT
)

cat(
  paste0(
    "Saved: time_extensive_by_quintile.pdf, ",
    "time_intensive_by_quintile.pdf\n"
  )
)

q1_zero_pct <- 100 * ud[
  time_quintile == "Q1",
  mean(total_time == 0)
]


# --- 3.3 Browsing time by website -------------------------------------------

site_time <- fit_by_group(
  t1[website %in% top15_sites],
  top15_sites,
  "website",
  "log_time",
  fe_with_website = FALSE
)

site_time[
  ,
  display_name := gsub(
    "^www\\.",
    "",
    grp_raw
  )
]

site_time <- site_time[
  order(coef)
]

site_time[
  ,
  grp := factor(
    display_name,
    levels = rev(display_name)
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "time_did_by_site.pdf"
  ),
  plot_coef(
    site_time,
    "horizontal",
    TIME_LAB,
    NULL
  ),
  width = FIG_W,
  height = FIG_H_WIDE_SINGLE
)


# --- 3.4 Browsing time by website category ----------------------------------

cat_time <- fit_by_group(
  t1_cat,
  sort(big_cats),
  "category",
  "log_time"
)

cat_time <- cat_time[
  order(coef)
]

cat_time[
  ,
  grp := factor(
    grp_raw,
    levels = grp_raw
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "time_heterogeneity_by_website_category.pdf"
  ),
  plot_coef(
    cat_time,
    "vertical",
    TIME_LAB,
    NULL
  ) +
    rot_x,
  width = FIG_W,
  height = FIG_H_CAT_VERT
)


# --- 3.5 Browsing-time quintile by event-log status -------------------------

q_time_log <- fit_by_group_logstatus(
  t1_q,
  paste0("Q", 1:5),
  "time_quintile",
  "log_time"
)

q_time_log[
  ,
  grp := factor(
    grp_raw,
    levels = paste0("Q", 1:5)
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "time_heterogeneity_by_user_quintile_log_status.pdf"
  ),
  plot_coef_logstatus(
    q_time_log,
    "vertical",
    TIME_LAB,
    QUINT_LAB
  ),
  width = FIG_W,
  height = FIG_H_QUINT
)


# --- 3.6 Browsing time by website and event-log status ----------------------

site_time_log <- fit_by_group_logstatus(
  t1[website %in% top15_sites],
  top15_sites,
  "website",
  "log_time",
  fe_with_website = FALSE
)

site_time_log[
  ,
  display_name := gsub(
    "^www\\.",
    "",
    grp_raw
  )
]

order_site_t <- site_time_log[
  log_status == "Has Log"
][
  order(coef),
  display_name
]

site_time_log[
  ,
  grp := factor(
    display_name,
    levels = rev(order_site_t)
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "time_did_by_site_log_status.pdf"
  ),
  plot_coef_logstatus(
    site_time_log,
    "horizontal",
    TIME_LAB,
    NULL
  ),
  width = FIG_W,
  height = FIG_H_WIDE_LOGSTATUS
)


# --- 3.7 Browsing time by category and event-log status ---------------------

cat_time_log <- fit_by_group_logstatus(
  t1_cat,
  sort(big_cats),
  "category",
  "log_time"
)

order_cat_t <- cat_time_log[
  log_status == "No Log"
][
  order(coef),
  grp_raw
]

cat_time_log[
  ,
  grp := factor(
    grp_raw,
    levels = order_cat_t
  )
]

ggsave(
  paste0(
    FIGURES_DIR,
    "time_heterogeneity_by_website_category_log_status.pdf"
  ),
  plot_coef_logstatus(
    cat_time_log,
    "vertical",
    TIME_LAB,
    NULL
  ) +
    rot_x,
  width = FIG_W,
  height = FIG_H_CAT_VERT
)

cat(
  "Saved: 16 figures to",
  FIGURES_DIR,
  "\n"
)

cat(
  "Saved: 5 tables to",
  TABLES_DIR,
  "\n"
)


# =============================================================================
# INLINE SCALARS
# =============================================================================

no_log_pct <- 100 * (
  1 -
    uniqueN(
      t1[has_log == 1]$experiment_id
    ) /
    uniqueN(t1$experiment_id)
)

cookie_values_file <- file.path(
  VALUES_DIR,
  "data_sharing_cookie_values.tex"
)

suppressWarnings(
  file.remove(cookie_values_file)
)

save_tex_value(
  format_coef(
    coef(m_pool)["post_treated"]
  ),
  name = "cookieCpvCoef",
  file = cookie_values_file
)

save_tex_value(
  format_pct(
    100 * abs(
      exp(
        coef(m_pool)["post_treated"]
      ) - 1
    )
  ),
  name = "cookieCpvPct",
  file = cookie_values_file
)

save_tex_value(
  format_coef(
    coef(m_pool_excl)["post_treated"]
  ),
  name = "cookieCpvExclCoef",
  file = cookie_values_file
)

save_tex_value(
  format_coef(
    coef(m_time_pool)["post_treated"]
  ),
  name = "cookieTimeCoef",
  file = cookie_values_file
)

save_tex_value(
  format_pct(
    100 * abs(
      exp(
        coef(m_time_pool)["post_treated"]
      ) - 1
    )
  ),
  name = "cookieTimePct",
  file = cookie_values_file
)

save_tex_value(
  format_coef(
    coef(m_has)["post_treated"]
  ),
  name = "cookieHasLogCoef",
  file = cookie_values_file
)

save_tex_value(
  format_coef(
    coef(m_no)["post_treated"]
  ),
  name = "cookieNoLogCoef",
  file = cookie_values_file
)

save_tex_value(
  format_pct(no_log_pct),
  name = "noLogPct",
  file = cookie_values_file
)

save_tex_value(
  format_pct(
    share_pp[
      post == 0,
      cookie_share
    ]
  ),
  name = "unaffCookieSharePre",
  file = cookie_values_file
)

save_tex_value(
  format_pct(
    share_pp[
      post == 1,
      cookie_share
    ]
  ),
  name = "unaffCookieSharePost",
  file = cookie_values_file
)

save_tex_value(
  format_pct(
    share_pp[
      post == 0,
      visit_share
    ]
  ),
  name = "unaffVisitSharePre",
  file = cookie_values_file
)

save_tex_value(
  format_pct(
    share_pp[
      post == 1,
      visit_share
    ]
  ),
  name = "unaffVisitSharePost",
  file = cookie_values_file
)

save_tex_value(
  format_pct(
    share_pp[
      post == 0,
      time_share
    ]
  ),
  name = "unaffTimeSharePre",
  file = cookie_values_file
)

save_tex_value(
  format_pct(
    share_pp[
      post == 1,
      time_share
    ]
  ),
  name = "unaffTimeSharePost",
  file = cookie_values_file
)

save_tex_value(
  format_coef(
    ext_dt[
      grp_raw == "Q1",
      coef
    ]
  ),
  name = "timeExtQOneCoef",
  file = cookie_values_file
)

save_tex_value(
  format_pvalue(
    ext_dt[
      grp_raw == "Q1",
      p
    ]
  ),
  name = "timeExtQOnePval",
  file = cookie_values_file
)

save_tex_value(
  format_coef(
    int_dt[
      grp_raw == "Q1",
      coef
    ]
  ),
  name = "timeIntQOneCoef",
  file = cookie_values_file
)

save_tex_value(
  format_pvalue(
    int_dt[
      grp_raw == "Q1",
      p
    ]
  ),
  name = "timeIntQOnePval",
  file = cookie_values_file
)

save_tex_value(
  format_pct(q1_zero_pct),
  name = "qOneZeroDayPct",
  file = cookie_values_file
)

save_tex_value(
  format_count(
    round(
      base_stats[, max(med)]
    )
  ),
  name = "baseCatTopMedCpv",
  file = cookie_values_file
)

save_tex_value(
  format_count(
    round(
      base_stats[, min(med)]
    )
  ),
  name = "baseCatBottomMedCpv",
  file = cookie_values_file
)

save_tex_value(
  format_coef(
    coef(m_uc)["post_treated"]
  ),
  name = "ucMainCoef",
  file = cookie_values_file
)

save_tex_value(
  format_pct(
    100 * abs(
      exp(
        coef(m_uc)["post_treated"]
      ) - 1
    )
  ),
  name = "ucMainPct",
  file = cookie_values_file
)

save_tex_value(
  format_coef(
    coef(m_uc_has)["post_treated"]
  ),
  name = "ucHasLogCoef",
  file = cookie_values_file
)

save_tex_value(
  format_coef(
    coef(m_uc_no)["post_treated"]
  ),
  name = "ucNoLogCoef",
  file = cookie_values_file
)

n_sig <- function(dt) {
  sum(
    dt$ci_lo > 0 |
      dt$ci_hi < 0
  )
}

n_sig_neg <- function(dt) {
  sum(dt$ci_hi < 0)
}

n_sig_pos <- function(dt) {
  sum(dt$ci_lo > 0)
}

n_neg <- function(dt) {
  sum(dt$coef < 0)
}

save_tex_value(
  as.character(n_sig(cat_dt)),
  name = "nSigCpvCategories",
  file = cookie_values_file
)

save_tex_value(
  as.character(n_sig_neg(cat_dt)),
  name = "nSigNegCpvCategories",
  file = cookie_values_file
)

save_tex_value(
  as.character(n_sig_pos(cat_dt)),
  name = "nSigPosCpvCategories",
  file = cookie_values_file
)

save_tex_value(
  as.character(nrow(cat_dt)),
  name = "nCpvCategories",
  file = cookie_values_file
)

save_tex_value(
  as.character(n_sig_neg(cat_uc)),
  name = "nSigNegUcCategories",
  file = cookie_values_file
)

save_tex_value(
  as.character(n_sig_pos(cat_uc)),
  name = "nSigPosUcCategories",
  file = cookie_values_file
)

sig_pos_vec <- cat_dt[
  ci_lo > 0,
  grp_raw
]

if (length(sig_pos_vec) == 0) {
  sig_pos_cats <- "none"
  
  cat(
    paste0(
      "WARNING: no significant-positive CPV category ",
      "under the corrected data.\n"
    )
  )
  
  cat(
    paste0(
      "         \\sigPosCpvCategory written as 'none' -- ",
      "the paper sentence MUST be revised.\n"
    )
  )
} else {
  sig_pos_cats <- paste(
    gsub(
      "&",
      "\\\\&",
      sig_pos_vec
    ),
    collapse = ", "
  )
}

cookie_str_values_file <- file.path(
  VALUES_DIR,
  "data_sharing_cookie_str_values.tex"
)

writeLines(
  sprintf(
    "\\newcommand{\\sigPosCpvCategory}{%s}",
    sig_pos_cats
  ),
  cookie_str_values_file
)

cat(
  sprintf(
    paste0(
      "Wrote \\sigPosCpvCategory = %s -> %s\n"
    ),
    sig_pos_cats,
    cookie_str_values_file
  )
)

save_tex_value(
  as.character(n_sig(site_time)),
  name = "nSigTimeSites",
  file = cookie_values_file
)

save_tex_value(
  as.character(n_neg(site_time)),
  name = "nNegTimeSites",
  file = cookie_values_file
)

save_tex_value(
  as.character(nrow(site_time)),
  name = "nTimeSites",
  file = cookie_values_file
)

save_tex_value(
  as.character(n_sig(cat_time)),
  name = "nSigTimeCategories",
  file = cookie_values_file
)

cat(
  "Saved macros to",
  cookie_values_file,
  "\n"
)


# =============================================================================
# DIAGNOSTIC OUTPUT FOR WRITING
# =============================================================================

print_sig <- function(
    dt,
    label,
    keycol = "grp_raw"
) {
  cat(
    "\n[",
    label,
    "]\n",
    sep = ""
  )
  
  d <- copy(dt)
  
  d[
    ,
    sig := ci_lo > 0 |
      ci_hi < 0
  ]
  
  d[
    ,
    dir := ifelse(
      coef < 0,
      "neg",
      "pos"
    )
  ]
  
  print(
    d[
      ,
      c(
        keycol,
        "coef",
        "ci_lo",
        "ci_hi",
        "sig",
        "dir"
      ),
      with = FALSE
    ]
  )
  
  cat(
    sprintf(
      "  -> %d of %d significant; %d negative\n",
      sum(d$sig),
      nrow(d),
      sum(d$coef < 0)
    )
  )
}

cat(
  "\n",
  strrep("=", 60),
  "\nSIGNIFICANCE DIAGNOSTICS (for prose; not in paper)\n",
  strrep("=", 60),
  "\n",
  sep = ""
)

print_sig(
  q_dt,
  "CPV quintile"
)

print_sig(
  ext_dt,
  "TIME extensive margin by quintile"
)

print_sig(
  int_dt,
  "TIME intensive margin by quintile"
)

print_sig(
  site_dt,
  "CPV site"
)

print_sig(
  cat_dt,
  "CPV category"
)

print_sig(
  cat_uc,
  "UC category"
)

print_sig(
  site_time,
  "TIME site"
)

print_sig(
  cat_time,
  "TIME category"
)

cat(
  "\n",
  strrep("-", 60),
  paste0(
    "\nCPV CATEGORY signed breakdown ",
    "(for the significant-negative sentence):\n"
  ),
  sep = ""
)

cat(
  sprintf(
    "  significant NEGATIVE : %d\n",
    n_sig_neg(cat_dt)
  )
)

cat(
  sprintf(
    "  significant POSITIVE : %d  -> %s\n",
    n_sig_pos(cat_dt),
    paste(
      cat_dt[
        ci_lo > 0,
        grp_raw
      ],
      collapse = ", "
    )
  )
)

cat(
  sprintf(
    "  total categories     : %d\n",
    nrow(cat_dt)
  )
)

cat("=== DONE ===\n")