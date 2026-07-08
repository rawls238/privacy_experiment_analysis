# =============================================================================
# test.R -- ROUND 6: pre-production checks
# =============================================================================
hr <- function(x) cat("\n", strrep("=", 70), "\n[", x, "]\n", strrep("=", 70), "\n", sep = "")

# ---- Check 1: 5-column etable rendering --------------------------------------
hr("CHECK 1: 5-column etable (needs dsa + m5 from round 3 in memory,")
cat("re-run round 3 block first if session was restarted)\n")

# Rebuild the four originals on the SAME dsa sample used for m5
m1 <- feols(data_sharing_1_b ~ experiment_condition | block_by_wave,
            cluster = ~experiment_id, data = dsa)
m2 <- feols(data_sharing_2_b ~ experiment_condition | block_by_wave,
            cluster = ~experiment_id, data = dsa)
m3 <- feols(data_sharing_3_b ~ experiment_condition | block_by_wave,
            cluster = ~experiment_id, data = dsa)
m4 <- feols(data_sharing_4_b ~ experiment_condition | block_by_wave,
            cluster = ~experiment_id, data = dsa)

dict <- c("experiment_conditioninfo"     = "Information Treatment",
          "experiment_conditionsaliency" = "Saliency Treatment",
          "block_by_wave" = "Block FE")

# Console version first: check headers, model numbers, and N row
etable(m1, m2, m3, m4, m5,
       headers = c("Deleted Cookies", "Opted out", "Withheld data",
                   "Changed privacy settings", "Any Behavior"),
       dict = dict, depvar = FALSE,
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1), digits = 3)

cat("\nN per model (should all match):\n")
cat(sapply(list(m1, m2, m3, m4, m5), nobs), "\n")

# ---- Check 2: baseline category figure numbers (freeze for notes) ------------
hr("CHECK 2: category baseline stats (final, for figure notes)")
final_stats <- site_base[, .(
  n_sites = .N,
  p25 = round(quantile(site_mean_cpv, .25), 1),
  med = round(median(site_mean_cpv), 1),
  p75 = round(quantile(site_mean_cpv, .75), 1)
), by = category][order(-med)]
print(final_stats, nrow = 20)

cat("\n=== ROUND 6 DONE ===\n")