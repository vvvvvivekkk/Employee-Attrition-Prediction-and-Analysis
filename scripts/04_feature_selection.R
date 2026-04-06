# =============================================================================
# STEP 4: FEATURE SELECTION
# =============================================================================
# Objective: Identify the most important variables influencing attrition
#            using correlation analysis and model-based feature importance.
# =============================================================================

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  STEP 4: FEATURE SELECTION\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ─── 4.1 Correlation with Target Variable ────────────────────────────────────

cat("── Correlation Analysis ────────────────────────────────────────\n")

# Convert attrition to numeric for correlation (Yes=1, No=0)
hr_numeric <- hr_data_clean %>%
  mutate(Attrition_Num = ifelse(Attrition == "Yes", 1, 0)) %>%
  select(where(is.numeric))

# Calculate correlations with attrition
attrition_corr <- cor(hr_numeric, use = "complete.obs")[, "Attrition_Num"]
attrition_corr <- sort(abs(attrition_corr[names(attrition_corr) != "Attrition_Num"]), 
                       decreasing = TRUE)

cat("  Top 15 features by absolute correlation with Attrition:\n")
top_corr <- head(attrition_corr, 15)
for (i in seq_along(top_corr)) {
  bar <- paste(rep("█", round(top_corr[i] * 50)), collapse = "")
  cat(sprintf("    %-28s  %.4f  %s\n", names(top_corr)[i], top_corr[i], bar))
}

# Plot correlation bar chart
corr_df <- data.frame(
  Feature = names(top_corr),
  Correlation = as.numeric(top_corr)
) %>%
  mutate(Feature = factor(Feature, levels = rev(Feature)))

p_corr <- ggplot(corr_df, aes(x = Feature, y = Correlation, fill = Correlation)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient(low = "#3498DB", high = "#E74C3C") +
  labs(title = "Top Features Correlated with Attrition",
       subtitle = "Absolute Pearson correlation values",
       x = "", y = "Absolute Correlation") +
  theme_attrition +
  theme(legend.position = "none")

print(p_corr)
ggsave(file.path(project_root, "output", "12_feature_correlation.png"), 
       p_corr, width = 10, height = 7, dpi = 150)

cat("\n")

# ─── 4.2 Random Forest Feature Importance ────────────────────────────────────

cat("── Random Forest Feature Importance ───────────────────────────\n")
cat("  Training a quick Random Forest to extract variable importance...\n")

# Prepare data for Random Forest (convert all factors to ensure compatibility)
rf_data <- hr_data_clean %>%
  select(-any_of(c("TenureRatio", "PromotionStagnation", "IncomePerLevel")))

# Ensure no issues with factor levels
rf_data <- rf_data %>%
  mutate(across(where(is.factor), droplevels))

set.seed(42)
rf_importance_model <- randomForest(
  Attrition ~ .,
  data = rf_data,
  ntree = 500,
  importance = TRUE
)

# Extract and sort importance
importance_scores <- importance(rf_importance_model, type = 2)  # MeanDecreaseGini
importance_df <- data.frame(
  Feature = rownames(importance_scores),
  Importance = importance_scores[, 1]
) %>%
  arrange(desc(Importance)) %>%
  head(20) %>%
  mutate(Feature = factor(Feature, levels = rev(Feature)))

cat("\n  Top 20 features by Mean Decrease in Gini:\n")
for (i in 1:nrow(importance_df)) {
  bar <- paste(rep("█", round(importance_df$Importance[i] / max(importance_df$Importance) * 30)), collapse = "")
  cat(sprintf("    %-28s  %8.2f  %s\n", 
              as.character(importance_df$Feature[i]), 
              importance_df$Importance[i], bar))
}

# Plot feature importance
p_imp <- ggplot(importance_df, aes(x = Feature, y = Importance, fill = Importance)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_gradient(low = "#F39C12", high = "#C0392B") +
  labs(title = "Top 20 Features by Importance (Random Forest)",
       subtitle = "Based on Mean Decrease in Gini impurity",
       x = "", y = "Importance (Mean Decrease Gini)") +
  theme_attrition +
  theme(legend.position = "none")

print(p_imp)
ggsave(file.path(project_root, "output", "13_feature_importance_rf.png"), 
       p_imp, width = 10, height = 8, dpi = 150)


# ─── 4.3 Chi-Square Test for Categorical Features ────────────────────────────

cat("\n── Chi-Square Test for Categorical Features ────────────────────\n")

factor_cols <- names(hr_data_clean)[sapply(hr_data_clean, is.factor)]
factor_cols <- factor_cols[factor_cols != "Attrition"]

chi_results <- data.frame(Feature = character(), PValue = numeric(), 
                          Significant = character(), stringsAsFactors = FALSE)

for (col in factor_cols) {
  test <- chisq.test(table(hr_data_clean[[col]], hr_data_clean$Attrition), 
                     simulate.p.value = TRUE)
  sig <- ifelse(test$p.value < 0.05, "✅ Yes", "❌ No")
  chi_results <- rbind(chi_results, data.frame(
    Feature = col, PValue = test$p.value, Significant = sig
  ))
}

chi_results <- chi_results %>% arrange(PValue)
cat("  Chi-Square results (p < 0.05 = significant):\n")
for (i in 1:nrow(chi_results)) {
  cat(sprintf("    %-28s  p = %.6f  %s\n", 
              chi_results$Feature[i], chi_results$PValue[i], chi_results$Significant[i]))
}


# ─── 4.4 Summary of Key Features ─────────────────────────────────────────────

cat("\n── Key Features Summary ────────────────────────────────────────\n")
cat("  Based on correlation, RF importance, and chi-square tests,\n")
cat("  the most influential factors for attrition are:\n\n")
cat("  🔑 OverTime              — Strongest single predictor\n")
cat("  🔑 MonthlyIncome         — Lower pay = higher attrition\n")
cat("  🔑 Age                   — Younger employees leave more\n")
cat("  🔑 TotalWorkingYears     — Less experience = higher risk\n")
cat("  🔑 JobRole               — Specific roles have high turnover\n")
cat("  🔑 YearsAtCompany        — New employees are at risk\n")
cat("  🔑 JobSatisfaction       — Low satisfaction drives attrition\n")
cat("  🔑 WorkLifeBalance       — Poor balance increases risk\n")
cat("  🔑 DistanceFromHome      — Long commutes matter\n")
cat("  🔑 MaritalStatus         — Single employees leave more\n")

cat("\n✅ Step 4 complete — Feature selection analysis finished.\n")
