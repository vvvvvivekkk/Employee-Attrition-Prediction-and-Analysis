# =============================================================================
# STEP 6: MODEL EVALUATION
# =============================================================================
# Objective: Evaluate all three models on the held-out test set using
#            accuracy, confusion matrix, precision, recall, and F1-score.
#            Compare and visualize results.
# =============================================================================

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  STEP 6: MODEL EVALUATION\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ─── Helper Function: Compute All Metrics ─────────────────────────────────────

compute_metrics <- function(model, model_name, test_data) {
  # Predict classes
  pred_class <- predict(model, newdata = test_data)
  
  # Predict probabilities (for ROC curve)
  pred_prob <- predict(model, newdata = test_data, type = "prob")
  
  # Confusion matrix (positive class = "Yes")
  cm <- confusionMatrix(pred_class, test_data$Attrition, positive = "Yes")
  
  # Extract metrics
  accuracy  <- cm$overall["Accuracy"]
  precision <- cm$byClass["Pos Pred Value"]
  recall    <- cm$byClass["Sensitivity"]
  specificity <- cm$byClass["Specificity"]
  f1        <- cm$byClass["F1"]
  
  # ROC AUC
  roc_obj <- roc(test_data$Attrition, pred_prob[, "Yes"], quiet = TRUE)
  auc_val <- auc(roc_obj)
  
  list(
    name = model_name,
    predictions = pred_class,
    probabilities = pred_prob,
    confusion_matrix = cm,
    roc = roc_obj,
    metrics = data.frame(
      Model = model_name,
      Accuracy = round(accuracy, 4),
      Precision = round(precision, 4),
      Recall = round(recall, 4),
      Specificity = round(specificity, 4),
      F1_Score = round(f1, 4),
      AUC = round(as.numeric(auc_val), 4),
      row.names = NULL
    )
  )
}

# ─── 6.1 Evaluate Each Model ─────────────────────────────────────────────────

cat("── Evaluating models on test set ──────────────────────────────\n\n")

eval_lr <- compute_metrics(model_lr, "Logistic Regression", test_data)
eval_dt <- compute_metrics(model_dt, "Decision Tree", test_data)
eval_rf <- compute_metrics(model_rf, "Random Forest", test_data)

# ─── 6.2 Print Confusion Matrices ────────────────────────────────────────────

models_eval <- list(eval_lr, eval_dt, eval_rf)

for (eval in models_eval) {
  cat(paste0("┌─────────────────────────────────────────────────────────────\n"))
  cat(paste0("│  ", eval$name, "\n"))
  cat(paste0("└─────────────────────────────────────────────────────────────\n"))
  print(eval$confusion_matrix)
  cat("\n")
}

# ─── 6.3 Comparison Table ────────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════════\n")
cat("  MODEL COMPARISON TABLE\n")
cat("══════════════════════════════════════════════════════════════\n\n")

comparison_df <- bind_rows(eval_lr$metrics, eval_dt$metrics, eval_rf$metrics)
print(comparison_df)

# Identify the best model
best_idx <- which.max(comparison_df$F1_Score)
best_model <- comparison_df$Model[best_idx]
cat(paste0("\n  🏆 Best Model (by F1-Score): ", best_model, "\n"))
cat(paste0("     Accuracy:   ", comparison_df$Accuracy[best_idx], "\n"))
cat(paste0("     Precision:  ", comparison_df$Precision[best_idx], "\n"))
cat(paste0("     Recall:     ", comparison_df$Recall[best_idx], "\n"))
cat(paste0("     F1-Score:   ", comparison_df$F1_Score[best_idx], "\n"))
cat(paste0("     AUC:        ", comparison_df$AUC[best_idx], "\n\n"))

# ─── 6.4 Visualization: Metric Comparison Bar Chart ──────────────────────────

cat("📊 Plot: Model Comparison\n")

comparison_long <- comparison_df %>%
  pivot_longer(cols = c(Accuracy, Precision, Recall, F1_Score, AUC),
               names_to = "Metric", values_to = "Value")

model_colors <- c("Logistic Regression" = "#3498DB",
                  "Decision Tree"       = "#E67E22",
                  "Random Forest"       = "#27AE60")

p_compare <- ggplot(comparison_long, 
                    aes(x = Metric, y = Value, fill = Model)) +
  geom_col(position = "dodge", alpha = 0.9, width = 0.7) +
  geom_text(aes(label = round(Value, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.3, size = 3, fontface = "bold") +
  scale_fill_manual(values = model_colors) +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2)) +
  labs(title = "Model Performance Comparison",
       subtitle = "Evaluated on held-out test set",
       x = "Evaluation Metric", y = "Score") +
  theme_attrition

print(p_compare)
ggsave(file.path(project_root, "output", "15_model_comparison.png"),
       p_compare, width = 12, height = 7, dpi = 150)


# ─── 6.5 ROC Curves ──────────────────────────────────────────────────────────

cat("📊 Plot: ROC Curves\n")

png(file.path(project_root, "output", "16_roc_curves.png"), 
    width = 900, height = 700, res = 120)

plot(eval_rf$roc, col = "#27AE60", lwd = 2.5, 
     main = "ROC Curves — Model Comparison",
     cex.main = 1.2, legacy.axes = TRUE)
lines(eval_lr$roc, col = "#3498DB", lwd = 2.5)
lines(eval_dt$roc, col = "#E67E22", lwd = 2.5)
abline(a = 0, b = 1, col = "gray60", lty = 2, lwd = 1.5)

legend("bottomright",
       legend = c(
         paste0("Random Forest (AUC = ", round(auc(eval_rf$roc), 3), ")"),
         paste0("Logistic Reg. (AUC = ", round(auc(eval_lr$roc), 3), ")"),
         paste0("Decision Tree (AUC = ", round(auc(eval_dt$roc), 3), ")")
       ),
       col = c("#27AE60", "#3498DB", "#E67E22"),
       lwd = 2.5, cex = 0.9, bty = "n")

dev.off()
cat("  Saved: output/16_roc_curves.png\n")

# ─── 6.6 Confusion Matrix Heatmaps ───────────────────────────────────────────

cat("📊 Plot: Confusion Matrix Heatmaps\n")

plot_confusion <- function(eval_result) {
  cm <- eval_result$confusion_matrix$table
  cm_df <- as.data.frame(cm)
  names(cm_df) <- c("Predicted", "Actual", "Count")
  
  ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Count)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = Count), size = 8, fontface = "bold", color = "white") +
    scale_fill_gradient(low = "#85C1E9", high = "#1A5276") +
    labs(title = paste0(eval_result$name, " — Confusion Matrix"),
         x = "Actual", y = "Predicted") +
    theme_attrition +
    theme(legend.position = "none",
          panel.grid = element_blank())
}

cm_plots <- lapply(models_eval, plot_confusion)
p_cm_grid <- grid.arrange(grobs = cm_plots, ncol = 3)

ggsave(file.path(project_root, "output", "17_confusion_matrices.png"),
       p_cm_grid, width = 16, height = 5.5, dpi = 150)

cat("\n✅ Step 6 complete — Model evaluation finished.\n")
