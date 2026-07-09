# =============================================================================
# test.R -- ROUND 10: robustness DV = log(1 + n_cookies_third_party), no denominator
# Same spec as Table E.1 (+ log-status split), only the DV changes.
# =============================================================================
stopifnot(exists("t1"))
hr <- function(x) cat("\n", strrep("=", 70), "\n[", x, "]\n", strrep("=", 70), "\n", sep = "")
hr("ROUND 10: undivided cookie count DV")

t1[, log_ncookies := log(1 + n_cookies_third_party)]

# Main DiD (Table E.1 analog)
m_n <- feols(log_ncookies ~ post_treated | experiment_id + website + dow,
             data = t1, cluster = ~experiment_id, notes = FALSE)
cat(sprintf("MAIN:    coef = %+.4f  (se %.4f, p = %.4g)   [CPV version: -0.329]\n",
            coef(m_n)["post_treated"], se(m_n)["post_treated"],
            pvalue(m_n)["post_treated"]))

# Log-status split (Table E.3 analog)
for (lg in c(1, 0)) {
  m <- feols(log_ncookies ~ post_treated | experiment_id + website + dow,
             data = t1[has_log == lg], cluster = ~experiment_id, notes = FALSE)
  cat(sprintf("%s: coef = %+.4f  (p = %.4g)   [CPV version: %s]\n",
              ifelse(lg == 1, "HAS-LOG", "NO-LOG "),
              coef(m)["post_treated"], pvalue(m)["post_treated"],
              ifelse(lg == 1, "-0.230", "-0.369")))
}
cat("\n=== ROUND 10 DONE ===\n")