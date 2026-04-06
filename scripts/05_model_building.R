# =============================================================================
# STEP 5: MODEL BUILDING
# =============================================================================
# Objective: Split data into train/test sets, then build three classifiers:
#            1. Logistic Regression
#            2. Decision Tree (rpart)
#            3. Random Forest
# =============================================================================

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  STEP 5: MODEL BUILDING\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ─── 5.1 Train/Test Split ────────────────────────────────────────────────────

cat("── Train/Test Split ────────────────────────────────────────────\n")

set.seed(42)

# Use 80/20 split with stratification on the target variable
train_index <- createDataPartition(hr_data_clean$Attrition, p = 0.8, list = FALSE)
train_data  <- hr_data_clean[train_index, ]
test_data   <- hr_data_clean[-train_index, ]

cat(paste0("  Training set:  ", nrow(train_data), " observations (",
           round(nrow(train_data)/nrow(hr_data_clean)*100, 1), "%)\n"))
cat(paste0("  Testing set:   ", nrow(test_data), " observations (",
           round(nrow(test_data)/nrow(hr_data_clean)*100, 1), "%)\n"))
cat(paste0("  Train Attrition: Yes = ", sum(train_data$Attrition == "Yes"),
           ", No = ", sum(train_data$Attrition == "No"), "\n"))
cat(paste0("  Test Attrition:  Yes = ", sum(test_data$Attrition == "Yes"),
           ", No = ", sum(test_data$Attrition == "No"), "\n\n"))

# ─── 5.2 Define Cross-Validation Control ─────────────────────────────────────

cat("── Cross-Validation Setup ──────────────────────────────────────\n")

# 10-fold cross-validation repeated 3 times for robust estimates
train_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,                    # Needed for ROC/AUC
  summaryFunction = twoClassSummary,    # Optimize for ROC
  savePredictions = "final"
)

cat("  Method:    10-fold cross-validation, repeated 3 times\n")
cat("  Metric:    ROC (AUC)\n")
cat("  classProbs enabled for probability predictions\n\n")

# ─── 5.3 Model 1: Logistic Regression ────────────────────────────────────────

cat("── Model 1: Logistic Regression ───────────────────────────────\n")
cat("  Training logistic regression model...\n")

set.seed(42)
model_lr <- train(
  Attrition ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = train_control,
  metric = "ROC"
)

cat("  ✅ Logistic Regression trained.\n")
cat(paste0("     CV ROC (AUC): ", round(max(model_lr$results$ROC), 4), "\n"))
cat(paste0("     CV Sensitivity: ", round(max(model_lr$results$Sens), 4), "\n"))
cat(paste0("     CV Specificity: ", round(max(model_lr$results$Spec), 4), "\n\n"))

# ─── 5.4 Model 2: Decision Tree ──────────────────────────────────────────────

cat("── Model 2: Decision Tree (rpart) ─────────────────────────────\n")
cat("  Training decision tree with hyperparameter tuning...\n")

set.seed(42)
model_dt <- train(
  Attrition ~ .,
  data = train_data,
  method = "rpart",
  trControl = train_control,
  metric = "ROC",
  tuneLength = 10    # Try 10 different complexity parameters
)

cat("  ✅ Decision Tree trained.\n")
cat(paste0("     Best cp:        ", round(model_dt$bestTune$cp, 6), "\n"))
cat(paste0("     CV ROC (AUC):   ", round(max(model_dt$results$ROC), 4), "\n"))
cat(paste0("     CV Sensitivity: ", round(model_dt$results$Sens[which.max(model_dt$results$ROC)], 4), "\n"))
cat(paste0("     CV Specificity: ", round(model_dt$results$Spec[which.max(model_dt$results$ROC)], 4), "\n\n"))

# Plot the decision tree
cat("  📊 Visualizing decision tree...\n")
png(file.path(project_root, "output", "14_decision_tree.png"), 
    width = 1200, height = 800, res = 120)
rpart.plot(model_dt$finalModel, 
           type = 4, 
           extra = 104,
           under = TRUE,
           fallen.leaves = TRUE,
           roundint = FALSE,
           main = "Decision Tree for Employee Attrition",
           box.palette = c("#2ECC71", "#E74C3C"),
           shadow.col = "gray70")
dev.off()
cat("  Saved: output/14_decision_tree.png\n\n")

# ─── 5.5 Model 3: Random Forest ──────────────────────────────────────────────

cat("── Model 3: Random Forest ─────────────────────────────────────\n")
cat("  Training random forest with mtry tuning...\n")

set.seed(42)
model_rf <- train(
  Attrition ~ .,
  data = train_data,
  method = "rf",
  trControl = train_control,
  metric = "ROC",
  tuneLength = 5,           # Try 5 different mtry values
  ntree = 500,              # 500 trees
  importance = TRUE
)

cat("  ✅ Random Forest trained.\n")
cat(paste0("     Best mtry:      ", model_rf$bestTune$mtry, "\n"))
cat(paste0("     CV ROC (AUC):   ", round(max(model_rf$results$ROC), 4), "\n"))
cat(paste0("     CV Sensitivity: ", round(model_rf$results$Sens[which.max(model_rf$results$ROC)], 4), "\n"))
cat(paste0("     CV Specificity: ", round(model_rf$results$Spec[which.max(model_rf$results$ROC)], 4), "\n\n"))

# ─── 5.6 Quick Comparison ────────────────────────────────────────────────────

cat("── Cross-Validation Performance Summary ───────────────────────\n\n")

cv_results <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "Random Forest"),
  ROC_AUC = c(
    round(max(model_lr$results$ROC), 4),
    round(max(model_dt$results$ROC), 4),
    round(max(model_rf$results$ROC), 4)
  ),
  Sensitivity = c(
    round(max(model_lr$results$Sens), 4),
    round(model_dt$results$Sens[which.max(model_dt$results$ROC)], 4),
    round(model_rf$results$Sens[which.max(model_rf$results$ROC)], 4)
  ),
  Specificity = c(
    round(max(model_lr$results$Spec), 4),
    round(model_dt$results$Spec[which.max(model_dt$results$ROC)], 4),
    round(model_rf$results$Spec[which.max(model_rf$results$ROC)], 4)
  )
)

print(cv_results)

# Identify best model based on ROC
best_model_name <- cv_results$Model[which.max(cv_results$ROC_AUC)]
cat(paste0("\n  🏆 Best model (by CV ROC): ", best_model_name, "\n"))

cat("\n✅ Step 5 complete — All three models trained.\n")
