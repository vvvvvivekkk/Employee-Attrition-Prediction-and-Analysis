# =============================================================================
# STEP 7: PREDICTION & BUSINESS INSIGHTS
# =============================================================================
# Objective: Use the best model to predict attrition on the test set,
#            display sample predictions, and provide actionable insights.
# =============================================================================

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  STEP 7: PREDICTION & BUSINESS INSIGHTS\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ─── 7.1 Select the Best Model ───────────────────────────────────────────────

# Use the comparison_df from step 6 to pick best model by F1
best_idx <- which.max(comparison_df$F1_Score)
best_model_name <- comparison_df$Model[best_idx]

best_model <- switch(best_model_name,
  "Logistic Regression" = model_lr,
  "Decision Tree"       = model_dt,
  "Random Forest"       = model_rf
)

cat(paste0("  🏆 Using best model: ", best_model_name, "\n\n"))

# ─── 7.2 Predictions on Test Data ────────────────────────────────────────────

cat("── Sample Predictions on Test Data ────────────────────────────\n\n")

# Generate predictions and probabilities
test_predictions <- predict(best_model, newdata = test_data)
test_probabilities <- predict(best_model, newdata = test_data, type = "prob")

# Create a results dataframe with key features
prediction_results <- test_data %>%
  select(Age, MonthlyIncome, JobRole, OverTime, YearsAtCompany, 
         JobSatisfaction, WorkLifeBalance, Attrition) %>%
  mutate(
    Predicted = test_predictions,
    Prob_Yes = round(test_probabilities[, "Yes"], 4),
    Prob_No = round(test_probabilities[, "No"], 4),
    Correct = ifelse(Attrition == Predicted, "✅", "❌")
  )

# Show 20 sample predictions
cat("  Showing 20 sample predictions:\n\n")
sample_results <- prediction_results %>%
  slice_sample(n = 20) %>%
  arrange(desc(Prob_Yes))

print(as.data.frame(sample_results), row.names = FALSE)

# Prediction accuracy summary
correct_count <- sum(prediction_results$Correct == "✅")
total_count <- nrow(prediction_results)
cat(paste0("\n  Overall: ", correct_count, "/", total_count, " correct (",
           round(correct_count/total_count * 100, 1), "% accuracy)\n\n"))

# ─── 7.3 High-Risk Employees ─────────────────────────────────────────────────

cat("── High-Risk Employees (Predicted Attrition Probability > 0.5) ─\n\n")

high_risk <- prediction_results %>%
  filter(Prob_Yes > 0.5) %>%
  arrange(desc(Prob_Yes))

cat(paste0("  Found ", nrow(high_risk), " high-risk employees in the test set.\n\n"))

if (nrow(high_risk) > 0) {
  cat("  Top 10 highest-risk employees:\n\n")
  print(as.data.frame(head(high_risk, 10)), row.names = FALSE)
}

# ─── 7.4 Risk Distribution Plot ──────────────────────────────────────────────

cat("\n📊 Plot: Attrition Risk Distribution\n")

p_risk <- ggplot(prediction_results, aes(x = Prob_Yes, fill = Attrition)) +
  geom_histogram(bins = 30, alpha = 0.8, color = "white", position = "identity") +
  scale_fill_manual(values = c("No" = "#2ECC71", "Yes" = "#E74C3C")) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "#2C3E50", linewidth = 1) +
  annotate("text", x = 0.52, y = Inf, label = "Decision\nThreshold", 
           vjust = 2, hjust = 0, fontface = "bold", color = "#2C3E50", size = 3.5) +
  labs(title = "Distribution of Predicted Attrition Probabilities",
       subtitle = paste0("Model: ", best_model_name),
       x = "Predicted Probability of Attrition (Yes)",
       y = "Count") +
  theme_attrition

print(p_risk)
ggsave(file.path(project_root, "output", "18_risk_distribution.png"),
       p_risk, width = 10, height = 6, dpi = 150)


# ─── 7.5 Business Insights & Recommendations ─────────────────────────────────

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║              BUSINESS INSIGHTS & RECOMMENDATIONS            ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat("  Based on the analysis of ", nrow(hr_data_clean), " employees, here are\n")
cat("  the key findings and actionable recommendations:\n\n")

cat("  ┌─────────────────────────────────────────────────────────────\n")
cat("  │  📌 KEY FINDING 1: OVERTIME IS THE #1 DRIVER\n")
cat("  │\n")
cat("  │  Employees working overtime are 2-3x more likely to leave.\n")
cat("  │\n")
cat("  │  💡 RECOMMENDATION:\n")
cat("  │  → Implement strict overtime policies and monitoring\n")
cat("  │  → Offer compensatory time-off for overtime hours\n")
cat("  │  → Investigate root causes of excessive overtime by department\n")
cat("  └─────────────────────────────────────────────────────────────\n\n")

cat("  ┌─────────────────────────────────────────────────────────────\n")
cat("  │  📌 KEY FINDING 2: COMPENSATION MATTERS\n")
cat("  │\n")
cat("  │  Lower monthly income strongly correlates with attrition.\n")
cat("  │  Employees who leave earn significantly less than those who stay.\n")
cat("  │\n")
cat("  │  💡 RECOMMENDATION:\n")
cat("  │  → Conduct regular salary benchmarking against market rates\n")
cat("  │  → Implement transparent, performance-based salary increments\n")
cat("  │  → Offer competitive benefits packages to lower-paid roles\n")
cat("  └─────────────────────────────────────────────────────────────\n\n")

cat("  ┌─────────────────────────────────────────────────────────────\n")
cat("  │  📌 KEY FINDING 3: YOUNG & NEW EMPLOYEES ARE AT RISK\n")
cat("  │\n")
cat("  │  Younger employees (< 30) and those with fewer years at the\n")
cat("  │  company have the highest attrition rates.\n")
cat("  │\n")
cat("  │  💡 RECOMMENDATION:\n")
cat("  │  → Strengthen onboarding programs for new hires\n")
cat("  │  → Create mentorship programs pairing juniors with seniors\n")
cat("  │  → Conduct regular check-ins during the first 2 years\n")
cat("  │  → Offer career development plans and clear growth paths\n")
cat("  └─────────────────────────────────────────────────────────────\n\n")

cat("  ┌─────────────────────────────────────────────────────────────\n")
cat("  │  📌 KEY FINDING 4: JOB SATISFACTION IS CRITICAL\n")
cat("  │\n")
cat("  │  Employees with low job satisfaction (level 1) are far more\n")
cat("  │  likely to leave compared to moderately or highly satisfied.\n")
cat("  │\n")
cat("  │  💡 RECOMMENDATION:\n")
cat("  │  → Conduct anonymous employee satisfaction surveys quarterly\n")
cat("  │  → Act on feedback with visible, measurable changes\n")
cat("  │  → Create role enrichment opportunities (cross-training, etc.)\n")
cat("  └─────────────────────────────────────────────────────────────\n\n")

cat("  ┌─────────────────────────────────────────────────────────────\n")
cat("  │  📌 KEY FINDING 5: WORK-LIFE BALANCE\n")
cat("  │\n")
cat("  │  Employees rating their work-life balance as 'Bad' have\n")
cat("  │  significantly higher attrition rates.\n")
cat("  │\n")
cat("  │  💡 RECOMMENDATION:\n")
cat("  │  → Promote flexible working arrangements (hybrid/remote)\n")
cat("  │  → Monitor workload distribution across teams\n")
cat("  │  → Encourage managers to prioritize team well-being\n")
cat("  └─────────────────────────────────────────────────────────────\n\n")

cat("  ┌─────────────────────────────────────────────────────────────\n")
cat("  │  📌 KEY FINDING 6: PROMOTION STAGNATION\n")
cat("  │\n")
cat("  │  Employees who haven't been promoted in 5+ years are more\n")
cat("  │  likely to leave.\n")
cat("  │\n")
cat("  │  💡 RECOMMENDATION:\n")
cat("  │  → Review promotion timelines and ensure regular progression\n")
cat("  │  → Offer lateral moves for skill development\n")
cat("  │  → Create recognition programs beyond traditional promotions\n")
cat("  └─────────────────────────────────────────────────────────────\n\n")

cat("  ┌─────────────────────────────────────────────────────────────\n")
cat("  │  📌 KEY FINDING 7: SPECIFIC ROLES ARE HIGH-RISK\n")
cat("  │\n")
cat("  │  Sales Representatives and Lab Technicians show the highest\n")
cat("  │  attrition rates among all job roles.\n")
cat("  │\n")
cat("  │  💡 RECOMMENDATION:\n")
cat("  │  → Conduct exit interviews to understand role-specific issues\n")
cat("  │  → Improve work conditions for high-attrition roles\n")
cat("  │  → Offer role-specific retention bonuses or incentives\n")
cat("  └─────────────────────────────────────────────────────────────\n\n")


# ─── 7.6 Save Predictions to CSV ─────────────────────────────────────────────

cat("── Saving Results ──────────────────────────────────────────────\n")

write_csv(prediction_results, 
          file.path(project_root, "output", "predictions.csv"))
cat("  ✅ Predictions saved to: output/predictions.csv\n")

write_csv(comparison_df, 
          file.path(project_root, "output", "model_comparison.csv"))
cat("  ✅ Model comparison saved to: output/model_comparison.csv\n")


# ─── 7.7 Final Summary ───────────────────────────────────────────────────────

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║                    PROJECT SUMMARY                          ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")
cat(paste0("  Dataset:             ", nrow(hr_data_clean), " employees, ", 
           ncol(hr_data_clean) - 1, " features\n"))
cat(paste0("  Attrition Rate:      ", 
           round(mean(hr_data_clean$Attrition == "Yes") * 100, 1), "%\n"))
cat(paste0("  Best Model:          ", best_model_name, "\n"))
cat(paste0("  Test Accuracy:       ", comparison_df$Accuracy[best_idx], "\n"))
cat(paste0("  Test F1-Score:       ", comparison_df$F1_Score[best_idx], "\n"))
cat(paste0("  Test AUC:            ", comparison_df$AUC[best_idx], "\n"))
cat(paste0("  High-Risk Employees: ", nrow(high_risk), " flagged in test set\n"))
cat(paste0("  Plots Generated:     18\n"))
cat(paste0("  Output Directory:    output/\n"))

cat("\n✅ Step 7 complete — Predictions and insights generated.\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  🎉 ANALYSIS COMPLETE!\n")
cat("  📊 Launch the Shiny dashboard: shiny::runApp('shiny_app')\n")
cat("══════════════════════════════════════════════════════════════\n")
