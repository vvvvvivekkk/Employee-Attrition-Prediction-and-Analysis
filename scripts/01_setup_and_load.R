# =============================================================================
# STEP 1: SETUP AND DATA LOADING
# =============================================================================
# Objective: Load required libraries, read the IBM HR Analytics dataset,
#            and perform initial inspection of the data.
# =============================================================================

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  STEP 1: SETUP AND DATA LOADING\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ─── 1.1 Load Required Libraries ─────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)       # Core data science toolkit
  library(ggplot2)         # Visualization
  library(caret)           # ML framework
  library(randomForest)    # Random Forest
  library(rpart)           # Decision Trees
  library(rpart.plot)      # Decision Tree plots
  library(corrplot)        # Correlation matrices
  library(e1071)           # Statistical functions
  library(scales)          # Axis formatting
  library(gridExtra)       # Multi-plot layouts
  library(pROC)            # ROC analysis
})

cat("✅ All libraries loaded successfully.\n\n")

# ─── 1.2 Problem Understanding ───────────────────────────────────────────────
# 
# OBJECTIVE:
# Employee attrition (turnover) is a critical challenge for organizations. 
# High attrition leads to increased recruitment costs, loss of institutional 
# knowledge, and reduced team productivity.
#
# This project aims to:
#   1. Identify the key factors that drive employee attrition
#   2. Build predictive models to flag at-risk employees
#   3. Provide actionable insights for HR teams
#
# TARGET VARIABLE: Attrition (Yes / No)
#   - "Yes" = Employee has left the organization
#   - "No"  = Employee is still with the organization
#
# This is a BINARY CLASSIFICATION problem.
# ─────────────────────────────────────────────────────────────────────────────

# ─── 1.3 Load the Dataset ────────────────────────────────────────────────────

# Set the project root (works when sourced from main.R or directly)
if (!exists("project_root")) {
  project_root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
  if (project_root == "") project_root <- getwd()
}

data_path <- file.path(project_root, "data", "WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Check if the dataset exists; if not, download it
if (!file.exists(data_path)) {
  cat("📥 Dataset not found locally. Downloading from GitHub mirror...\n")
  dir.create(file.path(project_root, "data"), showWarnings = FALSE, recursive = TRUE)
  
  download_url <- "https://raw.githubusercontent.com/IBM/employee-attrition-aif360/master/data/emp_attrition.csv"
  
  tryCatch({
    download.file(download_url, data_path, mode = "wb", quiet = TRUE)
    cat("✅ Dataset downloaded successfully.\n\n")
  }, error = function(e) {
    cat("⚠️  Could not download dataset. Generating a synthetic dataset instead...\n")
    source(file.path(project_root, "scripts", "generate_synthetic_data.R"))
  })
}

# Read the dataset
if (file.exists(data_path)) {
  hr_data <- read_csv(data_path, show_col_types = FALSE)
} else {
  # If download failed, generate synthetic data
  cat("⚠️  Generating synthetic IBM HR-style dataset...\n")
  set.seed(42)
  n <- 1470
  
  hr_data <- tibble(
    Age = sample(18:60, n, replace = TRUE),
    Attrition = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.16, 0.84)),
    BusinessTravel = sample(c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), n, replace = TRUE),
    DailyRate = sample(100:1500, n, replace = TRUE),
    Department = sample(c("Sales", "Research & Development", "Human Resources"), n, replace = TRUE, prob = c(0.3, 0.6, 0.1)),
    DistanceFromHome = sample(1:29, n, replace = TRUE),
    Education = sample(1:5, n, replace = TRUE),
    EducationField = sample(c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Human Resources", "Other"), n, replace = TRUE),
    EmployeeCount = 1,
    EmployeeNumber = 1:n,
    EnvironmentSatisfaction = sample(1:4, n, replace = TRUE),
    Gender = sample(c("Male", "Female"), n, replace = TRUE),
    HourlyRate = sample(30:100, n, replace = TRUE),
    JobInvolvement = sample(1:4, n, replace = TRUE),
    JobLevel = sample(1:5, n, replace = TRUE),
    JobRole = sample(c("Sales Executive", "Research Scientist", "Laboratory Technician",
                        "Manufacturing Director", "Healthcare Representative",
                        "Manager", "Sales Representative", "Research Director",
                        "Human Resources"), n, replace = TRUE),
    JobSatisfaction = sample(1:4, n, replace = TRUE),
    MaritalStatus = sample(c("Single", "Married", "Divorced"), n, replace = TRUE),
    MonthlyIncome = round(rnorm(n, mean = 6500, sd = 4700)) %>% pmax(1000),
    MonthlyRate = sample(2000:27000, n, replace = TRUE),
    NumCompaniesWorked = sample(0:9, n, replace = TRUE),
    Over18 = "Y",
    OverTime = sample(c("Yes", "No"), n, replace = TRUE),
    PercentSalaryHike = sample(11:25, n, replace = TRUE),
    PerformanceRating = sample(3:4, n, replace = TRUE),
    RelationshipSatisfaction = sample(1:4, n, replace = TRUE),
    StandardHours = 80,
    StockOptionLevel = sample(0:3, n, replace = TRUE),
    TotalWorkingYears = sample(0:40, n, replace = TRUE),
    TrainingTimesLastYear = sample(0:6, n, replace = TRUE),
    WorkLifeBalance = sample(1:4, n, replace = TRUE),
    YearsAtCompany = sample(0:40, n, replace = TRUE),
    YearsInCurrentRole = sample(0:18, n, replace = TRUE),
    YearsSinceLastPromotion = sample(0:15, n, replace = TRUE),
    YearsWithCurrManager = sample(0:17, n, replace = TRUE)
  )
  
  # Save the synthetic dataset
  dir.create(file.path(project_root, "data"), showWarnings = FALSE, recursive = TRUE)
  write_csv(hr_data, data_path)
  cat("✅ Synthetic dataset generated and saved.\n\n")
}

cat(paste0("📊 Dataset loaded: ", nrow(hr_data), " rows × ", ncol(hr_data), " columns\n\n"))

# ─── 1.4 Initial Data Inspection ─────────────────────────────────────────────

cat("── Data Structure ──────────────────────────────────────────────\n")
str(hr_data)

cat("\n── First 6 Rows ────────────────────────────────────────────────\n")
print(head(hr_data))

cat("\n── Summary Statistics ──────────────────────────────────────────\n")
summary(hr_data)

cat("\n── Column Names ────────────────────────────────────────────────\n")
cat(paste(names(hr_data), collapse = ", "), "\n")

cat("\n── Target Variable Distribution ─────────────────────────────────\n")
attrition_counts <- table(hr_data$Attrition)
print(attrition_counts)
cat(paste0("\n  Attrition Rate: ", 
           round(prop.table(attrition_counts)["Yes"] * 100, 1), "%\n"))

cat("\n✅ Step 1 complete — Data loaded and inspected.\n")
