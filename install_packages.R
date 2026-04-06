# =============================================================================
# Employee Attrition Prediction and Analysis
# Package Installer — Run this once before starting the project
# =============================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║    Employee Attrition Prediction — Package Installer        ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# List of required packages
required_packages <- c(
  "tidyverse",       # Data manipulation (dplyr, tidyr, readr, etc.)
  "ggplot2",         # Data visualization
  "caret",           # Machine learning framework
  "randomForest",    # Random Forest algorithm
  "rpart",           # Decision Trees
  "rpart.plot",      # Decision Tree visualization
  "corrplot",        # Correlation plots
  "e1071",           # SVM, Naive Bayes, and caret dependencies
  "scales",          # Axis formatting
  "gridExtra",       # Grid arrangement of ggplots
  "pROC",            # ROC curves
  "shiny",           # Shiny web framework
  "shinydashboard",  # Dashboard layout for Shiny
  "plotly",          # Interactive plots
  "DT"               # Interactive data tables
)

# Install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("  📦 Installing: ", pkg, "\n"))
    install.packages(pkg, dependencies = TRUE, repos = "https://cran.r-project.org")
  } else {
    cat(paste0("  ✅ Already installed: ", pkg, "\n"))
  }
}

cat("Checking and installing required packages...\n\n")
invisible(sapply(required_packages, install_if_missing))

cat("\n✅ All packages are installed and ready!\n")
cat("   You can now run: source('scripts/main.R')\n")
