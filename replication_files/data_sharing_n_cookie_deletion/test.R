# ROUND 17: conservation test -- do user-day TOTALS match even when
# website-level cells don't? (legacy + panel_new from round 14 in memory)
ud_new <- panel_new[, .(tot = sum(n_cookies_third_party)),
                    by = .(experiment_id, date)]
ud_old <- legacy[, .(tot = sum(n_cookies_third_party)),
                 by = .(experiment_id, date)]
ud <- merge(ud_new, ud_old, by = c("experiment_id", "date"),
            suffixes = c("_new", "_old"))
cat(sprintf("user-day cells compared: %s\n", format(nrow(ud), big.mark = ",")))
cat(sprintf("exact match: %.1f%% | within 5%%: %.1f%%\n",
            100 * ud[, mean(tot_new == tot_old)],
            100 * ud[, mean(abs(tot_new - tot_old) <= 0.05 * pmax(tot_old, 1))]))
cat("\nAug-only (09_16-covered period, no rotation loss):\n")
uda <- ud[format(date, "%m") == "08"]
cat(sprintf("  exact: %.1f%% | within 5%%: %.1f%%\n",
            100 * uda[, mean(tot_new == tot_old)],
            100 * uda[, mean(abs(tot_new - tot_old) <= 0.05 * pmax(tot_old, 1))]))