# Employee Attrition Prediction and Analysis

## 📋 Project Overview

This project analyzes employee data from the IBM HR Analytics dataset to identify key factors
influencing attrition and builds machine learning models to predict whether an employee will
leave or stay. The analysis uses R with tidyverse, ggplot2, caret, randomForest, and rpart.

## 🎯 Objective

- Understand patterns behind employee attrition
- Identify the most influential factors driving turnover
- Build and compare predictive models (Logistic Regression, Decision Tree, Random Forest)
- Provide actionable business recommendations

## 📂 Project Structure

```
Employee_Attrition_R/
├── README.md                          # This file
├── data/
│   └── WA_Fn-UseC_-HR-Employee-Attrition.csv  # IBM HR Analytics dataset
├── scripts/
│   ├── 01_setup_and_load.R            # Install packages & load dataset
│   ├── 02_data_preprocessing.R        # Clean, encode, and scale data
│   ├── 03_eda.R                       # Exploratory Data Analysis with ggplot2
│   ├── 04_feature_selection.R         # Feature importance analysis
│   ├── 05_model_building.R            # Train Logistic Regression, Decision Tree, Random Forest
│   ├── 06_model_evaluation.R          # Evaluate models with metrics & comparison
│   ├── 07_prediction_and_insights.R   # Predict on test data & business insights
│   └── main.R                         # Master script — runs all steps sequentially
├── shiny_app/
│   └── app.R                          # Interactive Shiny dashboard
├── output/
│   └── (generated plots saved here)
└── install_packages.R                 # One-time package installer
```

## 🚀 How to Run

### Prerequisites
- R (version 4.0+ recommended)
- RStudio (recommended IDE)

### Step 1: Install Required Packages
```r
source("install_packages.R")
```

### Step 2: Run the Full Analysis
```r
source("scripts/main.R")
```

### Step 3: Launch the Shiny Dashboard (Optional)
```r
shiny::runApp("shiny_app")
```

## 📦 Libraries Used

| Library        | Purpose                                |
|----------------|----------------------------------------|
| tidyverse      | Data manipulation and piping           |
| ggplot2        | Data visualization                     |
| caret          | Model training, tuning, evaluation     |
| randomForest   | Random Forest classifier               |
| rpart          | Decision Tree classifier               |
| rpart.plot     | Decision Tree visualization            |
| corrplot       | Correlation matrix visualization       |
| e1071          | Support functions for caret            |
| scales         | Axis formatting for plots              |
| gridExtra      | Arranging multiple ggplots             |
| shinydashboard | Shiny dashboard layout                 |
| plotly         | Interactive plots in Shiny             |
| DT             | Interactive data tables in Shiny       |

## 📊 Models Compared

| Model               | Description                               |
|----------------------|-------------------------------------------|
| Logistic Regression  | Baseline linear classifier                |
| Decision Tree        | Interpretable rule-based classifier       |
| Random Forest        | Ensemble of decision trees (best performer)|

## 📈 Key Findings (Typical Results)

- **Overtime**, **Monthly Income**, **Job Satisfaction**, and **Years at Company** are
  among the strongest predictors of attrition.
- Employees who work overtime are significantly more likely to leave.
- Lower job satisfaction and lower monthly income correlate with higher attrition.
- Random Forest typically achieves the highest accuracy (~85-87%).

## 📜 License

This project is for educational purposes. The IBM HR Analytics dataset is publicly available
on Kaggle under the Open Database License.
