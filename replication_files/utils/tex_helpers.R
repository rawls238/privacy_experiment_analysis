# =============================================================================
# utils/tex_helpers.R
# =============================================================================
# Helpers for post-processing LaTeX output before it is written to
# output/tables/ and \input{} into the manuscript.
#
# Sourced by analysis scripts that write regression tables, e.g.:
#   source("replication_files/utils/tex_helpers.R")
# =============================================================================

# -----------------------------------------------------------------------------
# write_tabular_only(): save ONLY the \begin{tabular}...\end{tabular} block.
#
# fixest::etable(tex = TRUE) wraps its output in \begingroup\centering ...
# \par\endgroup. When the manuscript then does \scalebox{..}{\input{this file}},
# the \par inside the resulting hbox throws "There's no line here to end". We
# strip everything outside the tabular so the saved artifact is a clean box:
# scalebox/resizebox/bare \input all work, and \caption/\label/notes live in the
# float wrapper in the manuscript.
#
# `etable_output` is the character vector returned by etable(..., tex = TRUE)
# WITHOUT a `file=` argument (so it returns the string instead of writing).
#
# Tested in code_github/testing.R against a real etable fixture (17/17 checks).
# -----------------------------------------------------------------------------
write_tabular_only <- function(etable_output, file) {
  lines <- unlist(strsplit(paste(etable_output, collapse = "\n"), "\n"))
  start <- grep("\\\\begin\\{tabular\\}", lines)[1]
  end   <- grep("\\\\end\\{tabular\\}",   lines)[1]
  if (is.na(start) || is.na(end)) {
    stop("write_tabular_only(): no \\begin{tabular}/\\end{tabular} found.")
  }
  writeLines(lines[start:end], file)
  cat("Saved (tabular only):", file, "\n")
}