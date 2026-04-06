# =============================================================================
# STEP 2: DATA PREPROCESSING
# =============================================================================
# Objective: Clean the dataset, handle missing values, encode categorical
#            variables, remove redundant features, and scale numeric columns.
# =============================================================================

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  STEP 2: DATA PREPROCESSING\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ─── 2.1 Check for Missing Values ────────────────────────────────────────────

cat("── Missing Values Per Column ───────────────────────────────────\n")
missing_values <- colSums(is.na(hr_data))
if (any(missing_values > 0)) {
  cat("Columns with missing values:\n")
  print(missing_values[missing_values > 0])
  
  # Impute numeric columns with median, categorical with mode
  cat("\n🔧 Imputing missing values...\n")
  hr_data <- hr_data %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))
  
  cat("✅ Missing values imputed.\n\n")
} else {
  cat("✅ No missing values found in the dataset.\n\n")
}

# ─── 2.2 Remove Redundant/Constant Columns ───────────────────────────────────
# Columns like EmployeeCount, Over18, StandardHours have constant values
# and EmployeeNumber is just an ID — none are predictive.

cat("── Removing Redundant Columns ─────────────────────────────────\n")

# Identify constant columns (columns with only 1 unique value)
constant_cols <- names(hr_data)[sapply(hr_data, function(x) length(unique(x)) <= 1)]
cat("  Constant columns: ", paste(constant_cols, collapse = ", "), "\n")

# Identify ID-like columns
id_cols <- c("EmployeeNumber")
cat("  ID columns:       ", paste(id_cols, collapse = ", "), "\n")

# Remove these columns
cols_to_remove <- unique(c(constant_cols, id_cols))
cols_to_remove <- cols_to_remove[cols_to_remove %in% names(hr_data)]

hr_data_clean <- hr_data %>% select(-any_of(cols_to_remove))

cat("  Removed:          ", paste(cols_to_remove, collapse = ", "), "\n")
cat(paste0("  Remaining:        ", ncol(hr_data_clean), " columns\n\n"))

# ─── 2.3 Convert Categorical Variables to Factors ─────────────────────────────

cat("── Converting Categorical Variables to Factors ────────────────\n")

# Identify character columns and convert to factors
char_cols <- names(hr_data_clean)[sapply(hr_data_clean, is.character)]
cat("  Character columns: ", paste(char_cols, collapse = ", "), "\n")

hr_data_clean <- hr_data_clean %>%
  mutate(across(all_of(char_cols), as.factor))

# Also convert ordinal-like integer columns to factors
ordinal_cols <- c("Education", "EnvironmentSatisfaction", "JobInvolvement",
                  "JobLevel", "JobSatisfaction", "PerformanceRating",
                  "RelationshipSatisfaction", "StockOptionLevel", 
                  "WorkLifeBalance")
ordinal_cols <- ordinal_cols[ordinal_cols %in% names(hr_data_clean)]

cat("  Ordinal columns:  ", paste(ordinal_cols, collapse = ", "), "\n")

hr_data_clean <- hr_data_clean %>%
  mutate(across(all_of(ordinal_cols), as.factor))

cat("✅ Categorical conversion complete.\n\n")

# ─── 2.4 Feature Engineering ─────────────────────────────────────────────────

cat("── Feature Engineering ─────────────────────────────────────────\n")

# Create some useful derived features
hr_data_clean <- hr_data_clean %>%
  mutate(
    # Ratio of years at company to total working years
    TenureRatio = ifelse(TotalWorkingYears > 0, 
                         YearsAtCompany / TotalWorkingYears, 0),
    # Whether it's been a while since last promotion
    PromotionStagnation = ifelse(YearsSinceLastPromotion >= 5, 1, 0),
    # Income per job level (proxy for whether someone is underpaid)
    IncomePerLevel = MonthlyIncome / as.numeric(as.character(JobLevel))
  )

cat("  ➕ Created: TenureRatio, PromotionStagnation, IncomePerLevel\n")
cat("✅ Feature engineering complete.\n\n")

# ─── 2.5 Encode the Target Variable ──────────────────────────────────────────

cat("── Target Variable Encoding ────────────────────────────────────\n")

# Ensure Attrition is a factor with "Yes" as the positive class
hr_data_clean$Attrition <- factor(hr_data_clean$Attrition, levels = c("No", "Yes"))

cat("  Target: Attrition\n")
cat("  Levels: ", levels(hr_data_clean$Attrition), "\n")
cat("  Reference (Positive) class: Yes\n\n")

# ─── 2.6 Feature Scaling (for numeric columns) ───────────────────────────────

cat("── Feature Scaling ─────────────────────────────────────────────\n")

# Identify numeric columns (excluding target)
numeric_cols <- names(hr_data_clean)[sapply(hr_data_clean, is.numeric)]
cat("  Numeric columns to scale (", length(numeric_cols), " total):\n")
cat("    ", paste(numeric_cols[1:min(8, length(numeric_cols))], collapse = ", "), "\n")
if (length(numeric_cols) > 8) {
  cat("    ", paste(numeric_cols[9:length(numeric_cols)], collapse = ", "), "\n")
}

# Create a scaled version for models that need it
hr_data_scaled <- hr_data_clean
hr_data_scaled[numeric_cols] <- scale(hr_data_clean[numeric_cols])

cat("✅ Feature scaling complete (stored in hr_data_scaled).\n\n")

# ─── 2.7 Data Summary After Preprocessing ────────────────────────────────────

cat("── Preprocessed Data Summary ───────────────────────────────────\n")
cat(paste0("  Total observations: ", nrow(hr_data_clean), "\n"))
cat(paste0("  Total features:     ", ncol(hr_data_clean) - 1, " (+ 1 target)\n"))
cat(paste0("  Numeric features:   ", length(numeric_cols), "\n"))
cat(paste0("  Factor features:    ", sum(sapply(hr_data_clean, is.factor)) - 1, "\n"))
cat(paste0("  Attrition (Yes):    ", sum(hr_data_clean$Attrition == "Yes"), 
           " (", round(mean(hr_data_clean$Attrition == "Yes") * 100, 1), "%)\n"))
cat(paste0("  Attrition (No):     ", sum(hr_data_clean$Attrition == "No"),
           " (", round(mean(hr_data_clean$Attrition == "No") * 100, 1), "%)\n"))

cat("\n✅ Step 2 complete — Data is clean and ready for analysis.\n")
