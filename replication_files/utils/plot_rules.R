# ============================================================================
# PLOT RULES: Unified Visual Style for Privacy Experiment
# ============================================================================
#
# STYLE GUIDE:
# - Treatment colors: Control=#2196F3 (blue), Saliency=#FFC93C (marigold), Information=#EF5350 (light red)
# - Treatment labels: Always use "Control", "Saliency", "Information" (capitalized)
# - Factor order: Control → Saliency → Information
# - Privacy Category colors: control=#C8E6C9 (light green), use=#90CAF9 (light blue), collect=#FFAB91 (light coral)
# - Privacy Category order: control → use → collect
# - Benchmark colors: Survey=#64B0DD (fog blue), Extension/Experiment=#DF776E (muted coral), Benchmark=#7DC27D (sage green)
# - Benchmark usage: ACS/PEW comparisons (C.1 demographics, C.4 PEW attitudes)
# - Benchmark order: Survey → Extension → Benchmark (our data first, external reference last)
# - Binary colors: #EF5350 (light red, active) vs #2196F3 (blue, baseline) — for non-exposure two-level comparisons
# - Ordinal/exposure colors: #4DD0E1 → #2196F3 → #283593 (cyan → blue → deep indigo)
# - Exposure (any levels): always use ordinal gradient (ordinal_2 or ordinal_3)
# - Font: system default (sans-serif), base size 12
# - Background: white (both panel and plot)
# - Text color: #1C1C1E
# - Grid: both directions, light grey (#E5E5EA)
# - NO alpha on data geoms; use inherently lighter colors instead
# - Zero/reference lines: #C7C7CC with alpha=0.5 (use only when 0 is meaningful)
#     NOTE: When grid is visible, use color="gray50" for better visibility
# - Intervention line: red dashed
# - Boxplot border: #636366
# - Legend: bottom by default
# - Legend order: Saliency first, then Information (when only showing treatment effects)
#
# PALETTE USAGE:
#   Treatment (categorical, unordered):  scale_color_treatment() / scale_fill_treatment()
#   Privacy Category (policy analysis):  scale_color_privacy_category() / scale_fill_privacy_category()
#   Benchmark (ACS/PEW comparisons):     scale_fill_benchmark()
#   Binary (non-exposure two-level):     scale_color_binary()
#   Exposure 2-level (median split):     scale_color_ordinal_2()
#   Exposure 3-level (terciles):         scale_color_ordinal_3()
#
# SEMANTIC RULES FOR BINARY PALETTE:
#   Light red (#EF5350) = active / noteworthy / high condition
#   Blue (#2196F3) = baseline / default / low condition
#   In analysis scripts, set factor levels so ACTIVE label comes FIRST.
#
# USAGE:
#   source("code/utils/plot_rules.R")
#   
#   # Treatment plot
#   ggplot(...) + scale_color_treatment() + theme_privacy_experiment()
#   
#   # Binary comparison (e.g., Did/Didn't acquire info)
#   ggplot(...) + scale_color_binary() + theme_privacy_experiment()
#   
#   # Benchmark comparison (e.g., Survey vs ACS/PEW)
#   ggplot(...) + scale_fill_benchmark() + theme_privacy_experiment()
#   
#   # Exposure levels
#   ggplot(...) + scale_color_ordinal_2() + theme_privacy_experiment()
#   ggplot(...) + scale_color_ordinal_3() + theme_privacy_experiment()
#   
#   # With reference lines
#   ggplot(...) + geom_hline_zero() + geom_vline_intervention()
#
# ============================================================================


library(ggplot2)


# ============================================================================
# TREATMENT DEFINITIONS (Categorical — unordered groups)
# ============================================================================


# Canonical color palette (Apple-inspired)
TREATMENT_COLORS <- c(
  "Control"     = "#8BC48B",
  "Saliency"    = "#2196F3",
  "Information" = "#EF5350"
)


# Map raw data labels to display labels
TREATMENT_LABELS <- c(
  "control"     = "Control",
  "saliency"    = "Saliency",
  "info"        = "Information",
  "Control"     = "Control",
  "Saliency"    = "Saliency",
  "Information" = "Information"
)


# Canonical factor ordering for plots
TREATMENT_ORDER <- c("Control", "Saliency", "Information")


# ============================================================================
# PRIVACY CATEGORY DEFINITIONS (for policy analysis)
# ============================================================================
# Used in descriptive_analysis_of_policies scripts
# Order: control → use → collect
# Light pastel tones for large-area fills (violin, bar, etc.)

PRIVACY_CATEGORY_COLORS <- c(
  "control" = "#C8E6C9",
  "use"     = "#90CAF9",
  "collect" = "#FFAB91"
)

PRIVACY_CATEGORY_ORDER <- c("control", "use", "collect")

PRIVACY_CATEGORY_LABELS <- c(
  "control" = "Control",
  "use"     = "Use",
  "collect" = "Collect"
)


# ============================================================================
# BENCHMARK COMPARISON DEFINITIONS (Survey vs External benchmarks)
# ============================================================================
# Used in demographic balance plots (C.1) and PEW attitude comparisons (C.4)
# Mid-saturation tones with distinct hues: blue / red / green
# Order: Survey → Extension/Experiment → External benchmark (ACS/PEW)
#
# NOTE: These hues are deliberately different from Treatment colors:
#   Treatment uses Apricot/Blue/Red; Benchmark uses FogBlue/Coral/SageGreen.
#   Once old Treatment plots are updated to use TREATMENT_COLORS from this file,
#   there will be no overlap between the two palettes.

BENCHMARK_COLORS <- c(
  "Survey"         = "#64B0DD",
  "Extension"      = "#DF776E",
  "Pew benchmark"  = "#7DC27D",
  "ACS"            = "#7DC27D"
)

BENCHMARK_ORDER <- c("Survey", "Extension", "Pew benchmark")
BENCHMARK_ORDER_ACS <- c("Survey", "Extension", "ACS")


# ============================================================================
# BINARY PALETTE (Two-level comparison, non-exposure)
# Light red (active/high) → blue (baseline/low)
# scale_color_binary() assigns colors positionally:
#   First factor level  → Light red (#EF5350) = active / noteworthy / high
#   Second factor level → Blue      (#2196F3) = baseline / default / low
# ============================================================================

BINARY_COLORS <- c(
  "active"   = "#E05A5A",   
  "baseline" = "#2196F3"    
)

# ============================================================================
# ORDINAL / EXPOSURE PALETTE (ordered levels)
# Blue-green → Blue → Deep purple — high contrast, distinct from treatment
# Use for ALL exposure variables regardless of number of levels
# ============================================================================


ORDINAL_COLORS_3 <- c(
  "low"    = "#80DEEA",
  "mid"    = "#2196F3",
  "high"   = "#283593"
)

# 2-level variant for median splits (same family, endpoints)
ORDINAL_COLORS_2 <- c(
  "low"    = "#80DEEA",
  "high"   = "#283593"
)

# Single accent color for non-grouped ordinal plots
ORDINAL_ACCENT <- "#283593"

# ============================================================================
# COLOR CONSTANTS
# ============================================================================


BG_COLOR         <- "white"
TEXT_COLOR       <- "#1C1C1E"
GRID_COLOR       <- "#E5E5EA"
ZERO_LINE_COLOR  <- "#C7C7CC"
BOX_BORDER_COLOR <- "#636366"


# ============================================================================
# GLOBAL AESTHETICS
# ============================================================================


ZERO_LINE_ALPHA <- 0.5    # reference lines only; NO alpha on data geoms
LINE_WIDTH      <- 1      # geom_line, geom_errorbar, stat_ecdf
POINT_SIZE      <- 3      # geom_point
ERRORBAR_WIDTH  <- 0.3    # geom_errorbar whisker cap width
DODGE_WIDTH_2   <- 0.5    # position_dodge for 2-group plots
DODGE_WIDTH_3   <- 0.6    # position_dodge for 3-group plots


# ============================================================================
# THEME FUNCTION
# ============================================================================


# Unified theme for privacy experiment plots
# Usage: ggplot(...) + theme_privacy_experiment()
theme_privacy_experiment <- function(
    base_size = 12,
    base_family = "",
    legend_position = "bottom",
    show_grid_x = TRUE,
    show_grid_y = TRUE
) {
  grid_x <- if (show_grid_x) element_line(color = GRID_COLOR, linewidth = 0.5) else element_blank()
  grid_y <- if (show_grid_y) element_line(color = GRID_COLOR, linewidth = 0.5) else element_blank()
  
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Background
      panel.background = element_rect(fill = BG_COLOR, color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Grid
      panel.grid.major.x = grid_x,
      panel.grid.major.y = grid_y,
      panel.grid.minor = element_blank(),
      
      # Text
      text = element_text(family = base_family, color = TEXT_COLOR),
      axis.title = element_text(family = base_family, color = TEXT_COLOR, size = base_size),
      axis.text = element_text(family = base_family, color = TEXT_COLOR, size = base_size - 2),
      
      # Legend
      legend.position = legend_position,
      legend.background = element_rect(fill = alpha("white", 0.95), color = NA),
      legend.key = element_rect(fill = NA, color = NA),
      
      # Title
      plot.title = element_text(family = base_family, hjust = 0.5, face = "bold", size = base_size + 2)
    )
}


# ============================================================================
# SCALE FUNCTIONS — TREATMENT (categorical)
# ============================================================================


scale_color_treatment <- function(...) {
  scale_color_manual(values = TREATMENT_COLORS, ...)
}

scale_fill_treatment <- function(...) {
  scale_fill_manual(values = TREATMENT_COLORS, ...)
}


# ============================================================================
# SCALE FUNCTIONS — PRIVACY CATEGORY (for policy analysis)
# ============================================================================


scale_fill_privacy_category <- function(...) {
  scale_fill_manual(values = PRIVACY_CATEGORY_COLORS, ...)
}

scale_color_privacy_category <- function(...) {
  scale_color_manual(values = PRIVACY_CATEGORY_COLORS, ...)
}


# ============================================================================
# SCALE FUNCTIONS — BENCHMARK (Survey vs ACS/PEW comparisons)
# ============================================================================


scale_fill_benchmark <- function(...) {
  scale_fill_manual(values = BENCHMARK_COLORS, ...)
}

scale_color_benchmark <- function(...) {
  scale_color_manual(values = BENCHMARK_COLORS, ...)
}


# ============================================================================
# SCALE FUNCTIONS — BINARY (two-level comparison, non-exposure)
# ============================================================================


scale_color_binary <- function(colors = BINARY_COLORS, ...) {
  scale_color_manual(values = unname(colors), ...)
}


# ============================================================================
# SCALE FUNCTIONS — ORDINAL (exposure levels)
# ============================================================================


scale_color_ordinal_3 <- function(colors = ORDINAL_COLORS_3, ...) {
  scale_color_manual(values = unname(colors), ...)
}

scale_color_ordinal_2 <- function(colors = ORDINAL_COLORS_2, ...) {
  scale_color_manual(values = unname(colors), ...)
}


# ============================================================================
# DATA FORMATTING
# ============================================================================


# Standardize treatment column to canonical labels and order
format_treatment_factor <- function(df, col_name = "experiment_condition") {
  char_values <- as.character(df[[col_name]])
  df[[col_name]] <- factor(
    TREATMENT_LABELS[char_values],
    levels = TREATMENT_ORDER
  )
  return(df)
}


# ============================================================================
# REFERENCE LINE HELPERS
# ============================================================================


geom_hline_zero <- function(
    color = ZERO_LINE_COLOR,
    alpha = ZERO_LINE_ALPHA,
    linewidth = 1,
    ...
) {
  geom_hline(yintercept = 0, color = color, alpha = alpha, linewidth = linewidth, ...)
}


geom_vline_intervention <- function(
    xintercept = 0,
    color = "red",
    linetype = "dashed",
    ...
) {
  geom_vline(xintercept = xintercept, color = color, linetype = linetype, ...)
}


# ============================================================================
# COEFFICIENT PLOTS
# ============================================================================
# For regression coefficient visualizations (treatment effects, DiD, etc.)
#
# KEY RULES:
# - Use geom_errorbar() + geom_point(), NOT geom_pointrange()
# - Use scale_color_treatment() for colors
# - Factor levels: c("Saliency", "Information") — Saliency first
# - Use constants: ERRORBAR_WIDTH, LINE_WIDTH, POINT_SIZE
# - Zero line: use geom_hline_zero(linetype = "dashed", color = "gray50")
#   (color="gray50" makes it visible against grid)
#
# EXAMPLE:
#   coefs <- coefs %>%
#     mutate(
#       Treatment = case_when(
#         grepl("info", term) ~ "Information",
#         grepl("saliency", term) ~ "Saliency"
#       ),
#       Treatment = factor(Treatment, levels = c("Saliency", "Information"))
#     )
#
#   ggplot(coefs, aes(x = Treatment, y = estimate, color = Treatment)) +
#     geom_hline_zero(linetype = "dashed", color = "gray50") +
#     geom_errorbar(
#       aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
#       width = ERRORBAR_WIDTH,
#       linewidth = LINE_WIDTH
#     ) +
#     geom_point(size = POINT_SIZE) +
#     facet_wrap(~outcome, scales = "free_y") +
#     scale_color_treatment() +
#     theme_privacy_experiment() +
#     labs(x = "", y = "Coefficient (95% CI)", color = "Treatment")
#
# ============================================================================


# ============================================================================
# END OF FILE
# ============================================================================