# =============================================================================
# number_format_helpers.R
#
# Number-formatting utilities for the privacy experiment paper outputs.
#
# Two purposes:
#
#   1. Workaround for savetexvalue's broken `accuracy` parameter.
#      savetexvalue's default formatter is scales::number() which is supposed
#      to honor an `accuracy = 0.1` / `accuracy = 1` argument, but in the
#      installed package version the argument is silently ignored and values
#      are always written at full floating-point precision. By pre-formatting
#      every numeric value to a character string here, we bypass the
#      formatter entirely: savetexvalue detects the input is already a
#      string and writes it verbatim to the .tex file.
#
#   2. Single source of truth for the paper's number-formatting rules.
#      Every inline scalar and every table cell that appears in the paper
#      passes through one of these helpers. Changing how (say) shares are
#      formatted across the entire paper is a one-line edit in the
#      format_share alias below, not a hunt across a dozen scripts.
#
# -----------------------------------------------------------------------------
# Formatting rules
# -----------------------------------------------------------------------------
#
#   Data type               Rule
#   ---------               ----
#   Integer counts (N)      Bare integer. Output "8168", not "8,168" or
#                           "8168.00".
#
#   Percentages             One decimal place. 61.04 -> "61", 35.19 -> "35.2",
#                           59.95 -> "60". Trailing zeros dropped.
#
#   Shares (0-1 proportion) Three decimal places. 0.378 -> "0.378",
#                           0.400 -> "0.4". Trailing zeros dropped.
#                           Three decimals on a 0-1 scale carries the same
#                           information as one decimal on a 0-100 scale.
#
#   Dollar amounts          Integer if the value is a whole number; two
#                           decimal places otherwise. 20 -> "20", 19.97 ->
#                           "19.97", 41.44 -> "41.44". Dollar amounts are
#                           usually small (single to triple digits) and a
#                           whole-dollar input should display as such.
#
#   Hours                   Two significant figures. Used because hours
#                           values span two orders of magnitude in our data:
#                           ~28 total baseline hours on the high end, ~0.02
#                           daily hours on the low end. A fixed-decimal rule
#                           would either lose information at the small end
#                           (0.0204 -> 0.0) or carry false precision at the
#                           large end (28 -> 28.31).
#                           Output length is not fixed:
#                             1234   -> "1200"
#                             123.4  -> "120"
#                             28.31  -> "28"
#                             3.138  -> "3.1"
#                             1.495  -> "1.5"
#                             0.757  -> "0.76"
#                             0.0501 -> "0.05"
#                             0.0214 -> "0.021"
#                             0.0204 -> "0.02"
#                           Note: formatC(..., format = "fg") can insert a
#                           leading space when the rounded value renders as
#                           a 2-digit integer ("28" with a leading space).
#                           We trimws() to strip it.
#
#   Age                     One decimal place.
#
#   Income                  Bare integer. Income values are large (5-figure
#                           and up) and reported on census-bracket midpoints
#                           which are already whole numbers, so the integer
#                           rule is always exact and never loses information.
#
#   Regression coefficients Three decimal places. Inline use only; etable()
#                           output is not routed through this file.
#
#   P-values                If p >= 0.001: three decimal places ("0.918",
#                           "0.001"). If p < 0.001: scientific notation with
#                           one significant figure ("1.0e-03", "1.0e-16").
#                           We use scientific instead of "< 0.001" because
#                           scientific preserves the actual magnitude of the
#                           p-value, which matters for very small p-values
#                           in regression output.
#
#   Beliefs (0-100 scale)   One decimal place. Same rule as percentages
#                           since the variable is on the same 0-100 scale.
#
# -----------------------------------------------------------------------------
# Trailing zeros
# -----------------------------------------------------------------------------
#
# All decimal output drops trailing zeros, so 0.400 -> "0.4". This may
# leave LaTeX table columns visually unaligned. If you need fixed-width
# output at a specific table site, use formatC(x, format = "f", digits = N)
# at that site instead of routing through these helpers.
#
# -----------------------------------------------------------------------------
# NA / Inf / NaN
# -----------------------------------------------------------------------------
#
# These helpers do not special-case NA, Inf, or NaN. R coerces them to the
# strings "NA", "Inf", "NaN" via as.character(). Callers that need a
# different display (e.g. "--" for missing cells) must substitute downstream.
#
# -----------------------------------------------------------------------------
# Usage
# -----------------------------------------------------------------------------
#
#   source("replication_files/utils/format_helpers.R")
#
#   save_tex_value(values = format_pct(59.92),     names = "femalePct", ...)
#   save_tex_value(values = format_dollar(19.97),  names = "wtaMean",   ...)
#   save_tex_value(values = format_count(8168),    names = "surveyN",   ...)
#
# Adding a new data type:
#   Add one line in the "Semantic aliases" section that dispatches to fmt()
#   with the appropriate mode and digit count. Do not duplicate the
#   rounding logic; route everything through fmt().
# =============================================================================


# -----------------------------------------------------------------------------
# Core dispatcher
# -----------------------------------------------------------------------------
# fmt(x, mode, n) is the only function that contains formatting logic. All
# semantic aliases below call this. Four modes:
#
#   mode = "sig"        Round to n significant figures, drop trailing zeros.
#                       trimws() strips leading space from formatC output.
#   mode = "decimal"    Round to n decimal places, drop trailing zeros.
#   mode = "int"        Round to integer; n is ignored.
#   mode = "dollar"     Integer if whole, otherwise n decimal places.
#   mode = "pvalue"     If x >= 0.001, n decimal places; if x < 0.001,
#                       scientific notation with 1 significant figure.
#
# Returns a character vector the same length as x.
fmt <- function(x, mode = "decimal", n = 1) {
  if (mode == "sig") {
    return(vapply(x,
                  function(v) trimws(formatC(signif(v, n), format = "fg", digits = n)),
                  character(1)))
  }
  if (mode == "decimal") {
    return(as.character(round(x, n)))
  }
  if (mode == "int") {
    return(as.character(round(x)))
  }
  if (mode == "dollar") {
    return(ifelse(x == round(x),
                  as.character(as.integer(round(x))),
                  as.character(round(x, n))))
  }
  if (mode == "pvalue") {
    return(ifelse(x >= 0.001,
                  as.character(round(x, n)),
                  formatC(x, format = "e", digits = 1)))
  }
  stop("fmt(): unknown mode '", mode,
       "'. Expected one of 'sig', 'decimal', 'int', 'dollar', 'pvalue'.")
}


# -----------------------------------------------------------------------------
# Semantic aliases
# -----------------------------------------------------------------------------
# Caller code reads naturally:
#
#   format_age(39.42)         instead of fmt(39.42, "decimal", 1)
#   format_dollar(19.97)      instead of fmt(19.97, "dollar", 2)
#
# This is the public API of the file. Prefer adding a new alias here over
# calling fmt() directly from caller scripts.

# Percentages on a 0-100 scale.
format_pct <- function(x) fmt(x, "decimal", 1)

# Shares on a 0-1 scale (probabilities, fractions, indicator means).
format_share <- function(x) fmt(x, "decimal", 3)

# Hours. Two significant figures, not a fixed decimal count.
format_hours <- function(x) fmt(x, "sig", 2)

# Age in years.
format_age <- function(x) fmt(x, "decimal", 1)

# Dollar amounts. Integer if whole, otherwise two decimals. The dollar sign
# is added in paper LaTeX, not here, so output is "20" / "19.97".
format_dollar <- function(x) fmt(x, "dollar", 2)

# Household income. Bare integer.
format_income <- function(x) fmt(x, "int")

# Belief responses, scaled 0-100. Same rule as percentages.
format_belief <- function(x) fmt(x, "decimal", 1)

# Regression coefficients reported inline.
format_coef <- function(x) fmt(x, "decimal", 3)

# P-values reported inline. Three decimals if p >= 0.001, scientific
# notation with one significant figure otherwise.
format_pvalue <- function(x) fmt(x, "pvalue", 3)

# Sample sizes, counts, anything that should appear as a bare integer.
format_count <- function(x) fmt(x, "int")