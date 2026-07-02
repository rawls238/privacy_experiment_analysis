setwd("~/Dropbox/spring2025experiment/code_github")
library(tidyverse)
source("replication_files/utils/values.R")
source("replication_files/utils/plot_rules.R")

# 风险3: plot_rules.R 里这些常量到底是什么？
cat("=== PRIVACY_CATEGORY_ORDER ===\n"); print(PRIVACY_CATEGORY_ORDER)
cat("\n=== exists scale_fill_privacy_category ===\n"); print(exists("scale_fill_privacy_category"))

# 风险1: master 的 feature_name 跟 conjoint 数据的 feature_name 对得上吗？
ord <- get_privacy_attr_order()
cat("\n=== master feature_name (19) ===\n"); print(ord$feature_name)

CONJOINT_TABLE_DIR <- "../results/Conjoint_result/conjoint_with_website/tables"
ind <- read_csv(file.path(CONJOINT_TABLE_DIR, "individual_parameters_summary.csv"),
                show_col_types = FALSE)
data_fn <- sort(unique(ind$feature_name))
cat("\n=== conjoint data feature_name values ===\n"); print(data_fn)

# 关键对账：master 的 19 个 feature_name 有没有全部在数据里？
cat("\n=== master feature_name NOT in data (should be empty) ===\n")
print(setdiff(ord$feature_name, data_fn))
cat("=== data feature_name not in master (price/intercept etc, ok) ===\n")
print(setdiff(data_fn, ord$feature_name))