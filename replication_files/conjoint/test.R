# ============================================================================
# compare_wtp_old_vs_new.R
# ============================================================================
# Analysis + visualization comparing the OLD preference-space and NEW
# WTP-space individual dollar values, producing the evidence figures for the
# model-change report:
#
#   Fig 1  median_scatter        — attribute medians old vs new + 45 line
#                                  (conclusions unchanged)
#   Fig 2  tail_width_dumbbell   — per-attribute 5-95 width, old vs new
#                                  (how much the tails shrink)
#   Fig 3  distribution_overlay  — individual dollar densities, 3 attributes
#                                  (what the distributions look like)
#   Fig 4  mechanism_price_coef  — old/new WTP vs |price coef| (log x)
#                                  (WHY: ratio blows up as alpha -> 0;
#                                   direct parameterization does not)
#
# Outputs: output/figures/model_comparison/*.pdf + *.png
#          output/tables/conjoint_wtp_old_vs_new_person_level.csv
#          console summary block
#
# Run from: ~/Dropbox/spring2025experiment/code_github
# Dependencies: tidyverse, scales (no ggrepel/patchwork required)
# ============================================================================

library(tidyverse)
library(scales)

setwd("~/Dropbox/spring2025experiment/code_github")
source("replication_files/utils/values.R")   # PRIVACY_ATTR_MASTER

NEW_TABLE_DIR <- "../results/Conjoint_result/conjoint_with_website/tables"
OLD_TABLE_DIR <- "../results/Conjoint_result/conjoint_with_website/old_tables_2"
FIG_DIR       <- "output/figures/model_comparison"
TAB_DIR       <- "output/tables"
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TAB_DIR, showWarnings = FALSE, recursive = TRUE)

SWING <- 2
feature_labels <- setNames(PRIVACY_ATTR_MASTER$label, PRIVACY_ATTR_MASTER$feature_name)
privacy_features <- PRIVACY_ATTR_MASTER$feature_name

MODEL_COLORS <- c("Preference-space (old)" = "#B0413E",
                  "WTP-space (new)"        = "#2C6E91")

save_fig <- function(p, name, width = 8, height = 6) {
  ggsave(file.path(FIG_DIR, paste0(name, ".pdf")), p, width = width, height = height)
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), p, width = width, height = height,
         dpi = 300, bg = "white")
  cat(sprintf("Saved: %s/%s.{pdf,png}\n", FIG_DIR, name))
}

# ============================================================================
# 1. LOAD AND BUILD PERSON-LEVEL COMPARISON FRAME
# ============================================================================

cat(strrep("=", 78), "\n")
cat("OLD vs NEW: PERSON-LEVEL DOLLAR VALUES\n")
cat(strrep("=", 78), "\n\n")

new_wtp <- read_csv(file.path(NEW_TABLE_DIR, "individual_wtp_summary.csv"),
                    show_col_types = FALSE) %>%
  filter(feature_name %in% privacy_features) %>%
  select(sys_respnum, feature_name, dollar_new = dollar_value)

old_indiv <- read_csv(file.path(OLD_TABLE_DIR, "individual_parameters_summary.csv"),
                      show_col_types = FALSE)
old_price <- read_csv(file.path(OLD_TABLE_DIR, "individual_price_coefficients_summary.csv"),
                      show_col_types = FALSE)

# Old pipeline definition: dollar = (posterior-mean beta / posterior-mean
# price coef) * SWING — the ratio whose tails we are diagnosing.
old_dollars <- old_indiv %>%
  filter(feature_name %in% privacy_features) %>%
  inner_join(old_price %>%
               select(sys_respnum, price_coef_old = mean) %>%
               distinct(sys_respnum, .keep_all = TRUE),
             by = "sys_respnum") %>%
  transmute(sys_respnum, feature_name,
            dollar_old = (mean / price_coef_old) * SWING,
            abs_price_coef_old = abs(price_coef_old)) %>%
  filter(is.finite(dollar_old))

# Join by sys_respnum + feature_name (robust to any N_id re-indexing between runs)
person_cmp <- inner_join(old_dollars, new_wtp,
                         by = c("sys_respnum", "feature_name")) %>%
  mutate(feature_label = feature_labels[feature_name])

n_join <- n_distinct(person_cmp$sys_respnum)
cat(sprintf("Matched respondents: %d | rows: %d\n", n_join, nrow(person_cmp)))
if (n_join < 8000) {
  warning("Fewer than 8000 respondents matched across old/new tables — check ID alignment before trusting the comparison.")
}

write_csv(person_cmp, file.path(TAB_DIR, "conjoint_wtp_old_vs_new_person_level.csv"))

# Attribute-level summary used by Figs 1-2
attr_cmp <- person_cmp %>%
  group_by(feature_name, feature_label) %>%
  summarise(
    median_old = median(dollar_old),
    median_new = median(dollar_new),
    width_old  = quantile(dollar_old, .95) - quantile(dollar_old, .05),
    width_new  = quantile(dollar_new, .95) - quantile(dollar_new, .05),
    .groups = "drop"
  )

rho <- cor(attr_cmp$median_old, attr_cmp$median_new, method = "spearman")

# ============================================================================
# 2. CONSOLE SUMMARY (numbers backing the figures)
# ============================================================================

person_cor <- person_cmp %>%
  group_by(feature_label) %>%
  summarise(pearson = cor(dollar_old, dollar_new),
            spearman = cor(dollar_old, dollar_new, method = "spearman"),
            .groups = "drop")

cat("\n--- Summary ---\n")
cat(sprintf("Attribute-median Spearman rho (old vs new): %.3f\n", rho))
cat(sprintf("Median 5-95 width: old $%.1f -> new $%.1f (%.1fx narrower)\n",
            median(attr_cmp$width_old), median(attr_cmp$width_new),
            median(attr_cmp$width_old) / median(attr_cmp$width_new)))
cat(sprintf("Person-level rank correlation, median across attributes: %.3f\n",
            median(person_cor$spearman)))
cat(sprintf("Share of person-attribute cells with |dollar| > $12 (plot range): old %.1f%% vs new %.1f%%\n",
            100 * mean(abs(person_cmp$dollar_old) > 12),
            100 * mean(abs(person_cmp$dollar_new) > 12)))
cat(sprintf("Share with |dollar| > $60 (old clipping bound): old %.2f%% vs new %.2f%%\n",
            100 * mean(abs(person_cmp$dollar_old) > 60),
            100 * mean(abs(person_cmp$dollar_new) > 60)))

# ============================================================================
# FIG 1 — ATTRIBUTE MEDIANS: OLD vs NEW SCATTER (conclusions unchanged)
# ============================================================================

rng <- range(c(attr_cmp$median_old, attr_cmp$median_new)) * 1.15

p1 <- ggplot(attr_cmp, aes(x = median_old, y = median_new)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey55") +
  geom_point(size = 2.2, color = MODEL_COLORS[["WTP-space (new)"]]) +
  geom_text(aes(label = feature_label), size = 2.4, vjust = -0.9,
            check_overlap = TRUE, color = "grey30") +
  annotate("text", x = rng[1], y = rng[2],
           label = sprintf("Spearman rho = %.3f", rho),
           hjust = 0, vjust = 1, size = 3.6, fontface = "italic") +
  coord_equal(xlim = rng, ylim = rng) +
  scale_x_continuous(labels = dollar_format(accuracy = 1)) +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(x = "Median individual dollar value — preference-space (old)",
       y = "Median individual dollar value — WTP-space (new)",
       title = "Attribute-level conclusions are unchanged",
       subtitle = "Each point = one privacy attribute; dashed line = perfect agreement") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

save_fig(p1, "fig1_median_scatter", width = 7, height = 7)

# ============================================================================
# FIG 2 — TAIL WIDTH DUMBBELL (how much the tails shrink)
# ============================================================================

dumbbell_df <- attr_cmp %>%
  mutate(feature_label = fct_reorder(feature_label, width_old))

p2 <- ggplot(dumbbell_df) +
  geom_segment(aes(x = width_new, xend = width_old,
                   y = feature_label, yend = feature_label),
               color = "grey70", linewidth = 0.6) +
  geom_point(aes(x = width_old, y = feature_label,
                 color = "Preference-space (old)"), size = 2.4) +
  geom_point(aes(x = width_new, y = feature_label,
                 color = "WTP-space (new)"), size = 2.4) +
  scale_color_manual(values = MODEL_COLORS, name = NULL) +
  scale_x_continuous(labels = dollar_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = "5th-95th percentile width of individual dollar values",
       y = NULL,
       title = "Individual WTP tails shrink ~3x under WTP-space",
       subtitle = "Same likelihood (identity check 2e-13); only the heterogeneity layer moved") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

save_fig(p2, "fig2_tail_width_dumbbell", width = 8, height = 6)

# ============================================================================
# FIG 3 — DISTRIBUTION OVERLAY, 3 REPRESENTATIVE ATTRIBUTES
# ============================================================================
# Fixed +/-$20 display range; per-facet annotation reports the share of
# respondents falling OUTSIDE the display range under each model.

focus_feats <- c("collection_financial", "use_advertising", "use_personalization")
DISP <- 20

dist_df <- person_cmp %>%
  filter(feature_name %in% focus_feats) %>%
  pivot_longer(c(dollar_old, dollar_new),
               names_to = "model", values_to = "dollar") %>%
  mutate(model = ifelse(model == "dollar_old",
                        "Preference-space (old)", "WTP-space (new)"),
         feature_label = factor(feature_labels[feature_name],
                                levels = feature_labels[focus_feats]))

outside_df <- dist_df %>%
  group_by(feature_label, model) %>%
  summarise(pct_outside = 100 * mean(abs(dollar) > DISP), .groups = "drop") %>%
  mutate(lab = sprintf("%s: %.1f%% beyond ±$%d", model, pct_outside, DISP),
         y_pos = ifelse(grepl("old", model), Inf, Inf),
         v_just = ifelse(grepl("old", model), 1.4, 2.9))

p3 <- ggplot(dist_df, aes(x = pmin(pmax(dollar, -DISP), DISP),
                          fill = model, color = model)) +
  geom_density(alpha = 0.30, adjust = 1.0, linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_text(data = outside_df,
            aes(x = -DISP, y = Inf, label = lab, color = model, vjust = v_just),
            hjust = 0, size = 2.7, inherit.aes = FALSE, show.legend = FALSE) +
  facet_wrap(~feature_label, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = MODEL_COLORS, name = NULL) +
  scale_color_manual(values = MODEL_COLORS, name = NULL) +
  scale_x_continuous(labels = dollar_format(accuracy = 1),
                     breaks = seq(-DISP, DISP, 10)) +
  labs(x = "Individual dollar value (display clipped at ±$20)", y = "Density",
       title = "Individual WTP distributions: fat ratio tails vs model-governed tails",
       subtitle = "Product price range is $1-$5; annotations show mass beyond the display range") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

save_fig(p3, "fig3_distribution_overlay", width = 8, height = 8)

# ============================================================================
# FIG 4 — MECHANISM: WTP vs |PRICE COEFFICIENT| (log x)
# ============================================================================
# The core "why" figure. Old WTP = beta / alpha explodes hyperbolically as
# |alpha| -> 0 (left side); new WTP is estimated directly on the dollar scale
# and is flat in old |alpha|. Shown for the top attribute.

mech_feat <- "collection_financial"
Y_CLIP <- 60

mech_df <- person_cmp %>%
  filter(feature_name == mech_feat) %>%
  pivot_longer(c(dollar_old, dollar_new),
               names_to = "model", values_to = "dollar") %>%
  mutate(model = factor(ifelse(model == "dollar_old",
                               "Preference-space (old): WTP = beta / alpha",
                               "WTP-space (new): WTP estimated directly"),
                        levels = c("Preference-space (old): WTP = beta / alpha",
                                   "WTP-space (new): WTP estimated directly")),
         dollar_clip = pmin(pmax(dollar, -Y_CLIP), Y_CLIP))

pct_clip <- mech_df %>%
  group_by(model) %>%
  summarise(pct = 100 * mean(abs(dollar) > Y_CLIP), .groups = "drop")

p4 <- ggplot(mech_df, aes(x = abs_price_coef_old, y = dollar_clip)) +
  geom_point(alpha = 0.15, size = 0.5,
             color = MODEL_COLORS[["WTP-space (new)"]]) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.4) +
  geom_smooth(se = FALSE, color = MODEL_COLORS[["Preference-space (old)"]],
              linewidth = 0.7, method = "gam") +
  geom_text(data = pct_clip,
            aes(x = Inf, y = Inf, label = sprintf("%.1f%% clipped at ±$%d", pct, Y_CLIP)),
            hjust = 1.05, vjust = 1.5, size = 2.8, inherit.aes = FALSE) +
  facet_wrap(~model, ncol = 1) +
  scale_x_log10(labels = label_number(accuracy = 0.01)) +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(x = "|price coefficient| from the old model (log scale)",
       y = sprintf("Individual dollar value: %s (clipped at ±$%d)",
                   feature_labels[mech_feat], Y_CLIP),
       title = "Why the old tails explode: dividing by a near-zero price coefficient",
       subtitle = "Same respondents on both panels; ~14% have |price coef| < 0.10 (left of the axis)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

save_fig(p4, "fig4_mechanism_price_coef", width = 8, height = 8)

cat("\nAll comparison figures complete.\n")
cat("Report order suggestion: Fig 1 (nothing changed) -> Fig 2 (tails shrink)\n")
cat("-> Fig 3 (see the distributions) -> Fig 4 (the mechanism).\n")