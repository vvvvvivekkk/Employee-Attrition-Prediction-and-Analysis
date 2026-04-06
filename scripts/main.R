# =============================================================================
#
#    ███████╗███╗   ███╗██████╗ ██╗      ██████╗ ██╗   ██╗███████╗███████╗
#    ██╔════╝████╗ ████║██╔══██╗██║     ██╔═══██╗╚██╗ ██╔╝██╔════╝██╔════╝
#    █████╗  ██╔████╔██║██████╔╝██║     ██║   ██║ ╚████╔╝ █████╗  █████╗  
#    ██╔══╝  ██║╚██╔╝██║██╔═══╝ ██║     ██║   ██║  ╚██╔╝  ██╔══╝  ██╔══╝  
#    ███████╗██║ ╚═╝ ██║██║     ███████╗╚██████╔╝   ██║   ███████╗███████╗
#    ╚══════╝╚═╝     ╚═╝╚═╝     ╚══════╝ ╚═════╝    ╚═╝   ╚══════╝╚══════╝
#
#     █████╗ ████████╗████████╗██████╗ ██╗████████╗██╗ ██████╗ ███╗   ██╗
#    ██╔══██╗╚══██╔══╝╚══██╔══╝██╔══██╗██║╚══██╔══╝██║██╔═══██╗████╗  ██║
#    ███████║   ██║      ██║   ██████╔╝██║   ██║   ██║██║   ██║██╔██╗ ██║
#    ██╔══██║   ██║      ██║   ██╔══██╗██║   ██║   ██║██║   ██║██║╚██╗██║
#    ██║  ██║   ██║      ██║   ██║  ██║██║   ██║   ██║╚██████╔╝██║ ╚████║
#    ╚═╝  ╚═╝   ╚═╝      ╚═╝   ╚═╝  ╚═╝╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝
#
#    PREDICTION & ANALYSIS — Master Pipeline Script
#    Author: Data Science Project
#    Description: Runs the complete end-to-end analysis pipeline
#
# =============================================================================

# ─── Set Project Root ─────────────────────────────────────────────────────────
# Detect whether we're running from RStudio or the command line
project_root <- tryCatch({
  dirname(rstudioapi::getActiveDocumentContext()$path) %>% dirname()
}, error = function(e) {
  # Fallback: assume we're running from the project root or scripts dir
  if (file.exists("scripts/main.R")) {
    getwd()
  } else if (file.exists("main.R")) {
    dirname(getwd())
  } else {
    getwd()
  }
})

# Set working directory to project root
setwd(project_root)
cat(paste0("📂 Project root: ", project_root, "\n"))

# ─── Start Timer ──────────────────────────────────────────────────────────────
pipeline_start <- Sys.time()

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║    Employee Attrition Prediction & Analysis                 ║\n")
cat("║    Complete End-to-End Pipeline                             ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║    Steps:                                                   ║\n")
cat("║    1. Setup & Data Loading                                  ║\n")
cat("║    2. Data Preprocessing                                    ║\n")
cat("║    3. Exploratory Data Analysis (EDA)                       ║\n")
cat("║    4. Feature Selection                                     ║\n")
cat("║    5. Model Building                                        ║\n")
cat("║    6. Model Evaluation                                      ║\n")
cat("║    7. Prediction & Business Insights                        ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")

# ─── Run Each Step Sequentially ───────────────────────────────────────────────

cat("\n⏳ Running Step 1...\n")
source(file.path(project_root, "scripts", "01_setup_and_load.R"))

cat("\n⏳ Running Step 2...\n")
source(file.path(project_root, "scripts", "02_data_preprocessing.R"))

cat("\n⏳ Running Step 3...\n")
source(file.path(project_root, "scripts", "03_eda.R"))

cat("\n⏳ Running Step 4...\n")
source(file.path(project_root, "scripts", "04_feature_selection.R"))

cat("\n⏳ Running Step 5...\n")
source(file.path(project_root, "scripts", "05_model_building.R"))

cat("\n⏳ Running Step 6...\n")
source(file.path(project_root, "scripts", "06_model_evaluation.R"))

cat("\n⏳ Running Step 7...\n")
source(file.path(project_root, "scripts", "07_prediction_and_insights.R"))

# ─── Done ─────────────────────────────────────────────────────────────────────

pipeline_end <- Sys.time()
elapsed <- round(difftime(pipeline_end, pipeline_start, units = "mins"), 2)

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║                  🎉 PIPELINE COMPLETE 🎉                    ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat(paste0("║  Total time: ", elapsed, " minutes", 
           paste(rep(" ", max(0, 39 - nchar(as.character(elapsed)))), collapse = ""), "║\n"))
cat("║                                                             ║\n")
cat("║  Next steps:                                                ║\n")
cat("║  → Review output/ folder for all plots and CSVs             ║\n")
cat("║  → Launch Shiny: shiny::runApp('shiny_app')                 ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
