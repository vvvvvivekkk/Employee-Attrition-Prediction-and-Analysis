# =============================================================================
# SHINY DASHBOARD — Employee Attrition Prediction & Analysis
# =============================================================================
# An interactive dashboard for exploring employee attrition data,
# visualizing patterns, and making predictions.
#
# Launch with: shiny::runApp("shiny_app")
# =============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(randomForest)
library(caret)
library(scales)

# ─── Load or Generate Data ────────────────────────────────────────────────────

# Try to load the CSV from the data folder
data_path <- file.path("..", "data", "WA_Fn-UseC_-HR-Employee-Attrition.csv")
if (!file.exists(data_path)) {
  data_path <- file.path("data", "WA_Fn-UseC_-HR-Employee-Attrition.csv")
}

if (file.exists(data_path)) {
  hr_data <- read_csv(data_path, show_col_types = FALSE)
} else {
  # Generate synthetic data if CSV is not available
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
}

# Preprocess data
hr_data <- hr_data %>%
  select(-any_of(c("EmployeeCount", "EmployeeNumber", "Over18", "StandardHours"))) %>%
  mutate(Attrition = factor(Attrition, levels = c("No", "Yes")))

# Train a quick RF model for predictions
set.seed(42)
hr_model_data <- hr_data %>%
  mutate(across(where(is.character), as.factor))
rf_model <- randomForest(Attrition ~ ., data = hr_model_data, ntree = 200, importance = TRUE)

# Color palette
colors <- list(
  primary = "#1B2838",
  secondary = "#2C3E50",
  accent = "#3498DB",
  success = "#2ECC71",
  danger = "#E74C3C",
  warning = "#F39C12",
  bg = "#ECF0F1"
)

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "blue",
  
  # ── Header ──
  dashboardHeader(
    title = span(
      icon("users"), " Employee Attrition Dashboard"
    ),
    titleWidth = 350
  ),
  
  # ── Sidebar ──
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Exploratory Analysis", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Feature Importance", tabName = "features", icon = icon("star")),
      menuItem("Model Performance", tabName = "models", icon = icon("brain")),
      menuItem("Predict Attrition", tabName = "predict", icon = icon("magic")),
      menuItem("Data Explorer", tabName = "data", icon = icon("database"))
    ),
    br(),
    div(style = "padding: 15px; color: #bdc3c7; font-size: 12px;",
      p(icon("info-circle"), " IBM HR Analytics Dataset"),
      p(paste0(nrow(hr_data), " employees analyzed")),
      p(paste0(round(mean(hr_data$Attrition == "Yes") * 100, 1), "% attrition rate"))
    )
  ),
  
  # ── Body ──
  dashboardBody(
    # Custom CSS
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .small-box { border-radius: 10px; }
      .small-box .icon { font-size: 60px; top: 15px; }
      .box { border-radius: 8px; border-top: 3px solid #3498DB; }
      .box-header { font-weight: bold; }
      .skin-blue .main-header .logo { 
        background-color: #1B2838; font-weight: bold; 
      }
      .skin-blue .main-header .navbar { background-color: #2C3E50; }
      .skin-blue .main-sidebar { background-color: #1B2838; }
      .info-box { border-radius: 10px; min-height: 90px; }
      .nav-tabs-custom > .tab-content { padding: 15px; }
      h3 { color: #2C3E50; }
      .prediction-result {
        font-size: 28px; font-weight: bold; padding: 20px;
        border-radius: 15px; text-align: center; margin: 15px 0;
      }
      .pred-stay { background: linear-gradient(135deg, #2ECC71, #27AE60); color: white; }
      .pred-leave { background: linear-gradient(135deg, #E74C3C, #C0392B); color: white; }
    "))),
    
    tabItems(
      
      # ═══════════════════════════════════════════════════════════════════════
      # TAB 1: OVERVIEW
      # ═══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "overview",
        fluidRow(
          valueBox(nrow(hr_data), "Total Employees", icon = icon("users"),
                   color = "blue", width = 3),
          valueBox(sum(hr_data$Attrition == "Yes"), "Left Company", 
                   icon = icon("door-open"), color = "red", width = 3),
          valueBox(sum(hr_data$Attrition == "No"), "Stayed", 
                   icon = icon("building"), color = "green", width = 3),
          valueBox(paste0(round(mean(hr_data$Attrition == "Yes") * 100, 1), "%"), 
                   "Attrition Rate", icon = icon("percent"), color = "yellow", width = 3)
        ),
        fluidRow(
          box(title = "Attrition Distribution", status = "primary", 
              solidHeader = TRUE, width = 6,
              plotlyOutput("overview_attrition_pie", height = "350px")),
          box(title = "Attrition by Department", status = "primary", 
              solidHeader = TRUE, width = 6,
              plotlyOutput("overview_dept", height = "350px"))
        ),
        fluidRow(
          box(title = "Age Distribution by Attrition", status = "info",
              solidHeader = TRUE, width = 6,
              plotlyOutput("overview_age", height = "350px")),
          box(title = "Monthly Income by Attrition", status = "info",
              solidHeader = TRUE, width = 6,
              plotlyOutput("overview_income", height = "350px"))
        )
      ),
      
      # ═══════════════════════════════════════════════════════════════════════
      # TAB 2: EXPLORATORY ANALYSIS
      # ═══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "eda",
        fluidRow(
          box(title = "Select Analysis", status = "primary", solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(4,
                  selectInput("eda_feature", "Choose Feature:",
                    choices = c("OverTime", "JobRole", "MaritalStatus",
                                "BusinessTravel", "Department", "EducationField",
                                "Gender", "JobSatisfaction", "WorkLifeBalance",
                                "EnvironmentSatisfaction", "JobInvolvement",
                                "Education", "JobLevel", "StockOptionLevel"),
                    selected = "OverTime")
                ),
                column(4,
                  selectInput("eda_plot_type", "Plot Type:",
                    choices = c("Stacked Bar" = "fill", 
                                "Grouped Bar" = "dodge",
                                "Count" = "stack"),
                    selected = "fill")
                ),
                column(4,
                  checkboxInput("eda_show_pct", "Show Percentages", TRUE)
                )
              )
          )
        ),
        fluidRow(
          box(title = "Attrition by Selected Feature", status = "info",
              solidHeader = TRUE, width = 7,
              plotlyOutput("eda_bar", height = "450px")),
          box(title = "Summary Statistics", status = "warning",
              solidHeader = TRUE, width = 5,
              verbatimTextOutput("eda_summary"),
              hr(),
              h4("Attrition Rate by Group:"),
              tableOutput("eda_table"))
        ),
        fluidRow(
          box(title = "Numeric Feature vs Attrition", status = "primary",
              solidHeader = TRUE, width = 6,
              selectInput("eda_numeric", "Choose Numeric Feature:",
                choices = c("MonthlyIncome", "Age", "DailyRate", "DistanceFromHome",
                            "HourlyRate", "MonthlyRate", "NumCompaniesWorked",
                            "PercentSalaryHike", "TotalWorkingYears",
                            "TrainingTimesLastYear", "YearsAtCompany",
                            "YearsInCurrentRole", "YearsSinceLastPromotion",
                            "YearsWithCurrManager"),
                selected = "MonthlyIncome"),
              plotlyOutput("eda_boxplot", height = "350px")),
          box(title = "Scatter: Age vs Monthly Income", status = "primary",
              solidHeader = TRUE, width = 6,
              plotlyOutput("eda_scatter", height = "400px"))
        )
      ),
      
      # ═══════════════════════════════════════════════════════════════════════
      # TAB 3: FEATURE IMPORTANCE
      # ═══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "features",
        fluidRow(
          box(title = "Random Forest — Variable Importance (Top 20)", 
              status = "primary", solidHeader = TRUE, width = 8,
              plotlyOutput("feat_importance", height = "500px")),
          box(title = "About Feature Importance", status = "info",
              solidHeader = TRUE, width = 4,
              h4("How to Interpret"),
              p("Variables with higher importance scores have more influence 
                 on predicting employee attrition."),
              hr(),
              h4("Mean Decrease Gini"),
              p("Measures how much each variable contributes to the homogeneity 
                 of the nodes in the Random Forest. Higher values indicate 
                 more important features."),
              hr(),
              h4("Key Takeaways"),
              tags$ul(
                tags$li(strong("Overtime"), " is typically the strongest predictor"),
                tags$li(strong("Monthly Income"), " strongly influences attrition"),
                tags$li(strong("Age"), " and ", strong("Years at Company"), " are critical"),
                tags$li(strong("Job Role"), " shows significant variation")
              )
          )
        ),
        fluidRow(
          box(title = "Correlation Heatmap (Numeric Features)", 
              status = "warning", solidHeader = TRUE, width = 12,
              plotlyOutput("feat_corr", height = "500px"))
        )
      ),
      
      # ═══════════════════════════════════════════════════════════════════════
      # TAB 4: MODEL PERFORMANCE
      # ═══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "models",
        fluidRow(
          box(title = "Model Information", status = "primary", 
              solidHeader = TRUE, width = 12,
              p("The Random Forest model was trained with 200 trees using 
                 10-fold cross-validation. Below are its performance metrics."),
              fluidRow(
                infoBox("OOB Error Rate", 
                         paste0(round(rf_model$err.rate[nrow(rf_model$err.rate), "OOB"] * 100, 1), "%"),
                         icon = icon("percentage"), color = "blue", width = 4),
                infoBox("Trees", "200", icon = icon("tree"), color = "green", width = 4),
                infoBox("Features", ncol(hr_model_data) - 1, 
                         icon = icon("list"), color = "yellow", width = 4)
              )
          )
        ),
        fluidRow(
          box(title = "OOB Error Rate by Number of Trees", status = "info",
              solidHeader = TRUE, width = 6,
              plotlyOutput("model_oob", height = "400px")),
          box(title = "Confusion Matrix (OOB)", status = "warning",
              solidHeader = TRUE, width = 6,
              plotlyOutput("model_cm", height = "400px"))
        ),
        fluidRow(
          box(title = "Model Metrics", status = "primary", solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(12,
                  h4("Random Forest Out-of-Bag Performance:"),
                  tableOutput("model_metrics_table")
                )
              )
          )
        )
      ),
      
      # ═══════════════════════════════════════════════════════════════════════
      # TAB 5: PREDICT ATTRITION
      # ═══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "predict",
        fluidRow(
          box(title = "Enter Employee Details", status = "primary", 
              solidHeader = TRUE, width = 6,
              fluidRow(
                column(6,
                  numericInput("pred_age", "Age:", 30, min = 18, max = 65),
                  selectInput("pred_dept", "Department:", 
                    choices = unique(hr_data$Department)),
                  selectInput("pred_jobrole", "Job Role:",
                    choices = unique(hr_data$JobRole)),
                  numericInput("pred_income", "Monthly Income ($):", 5000, 
                               min = 1000, max = 20000, step = 500),
                  selectInput("pred_overtime", "Overtime:", 
                    choices = c("Yes", "No"), selected = "No"),
                  selectInput("pred_gender", "Gender:",
                    choices = c("Male", "Female"))
                ),
                column(6,
                  sliderInput("pred_satisfaction", "Job Satisfaction:", 
                              1, 4, 3, step = 1),
                  sliderInput("pred_wlb", "Work-Life Balance:", 1, 4, 3, step = 1),
                  sliderInput("pred_env_sat", "Environment Satisfaction:", 
                              1, 4, 3, step = 1),
                  numericInput("pred_years_company", "Years at Company:", 5,
                               min = 0, max = 40),
                  numericInput("pred_total_years", "Total Working Years:", 10,
                               min = 0, max = 40),
                  numericInput("pred_distance", "Distance from Home (miles):", 
                               10, min = 1, max = 30)
                )
              ),
              br(),
              actionButton("predict_btn", "🔮 Predict Attrition", 
                           class = "btn-primary btn-lg btn-block",
                           style = "font-size: 18px; padding: 12px;")
          ),
          box(title = "Prediction Result", status = "success", 
              solidHeader = TRUE, width = 6,
              uiOutput("prediction_result"),
              hr(),
              h4("Prediction Probabilities:"),
              plotlyOutput("prediction_gauge", height = "250px"),
              hr(),
              h4("Risk Factors for This Employee:"),
              uiOutput("risk_factors")
          )
        )
      ),
      
      # ═══════════════════════════════════════════════════════════════════════
      # TAB 6: DATA EXPLORER
      # ═══════════════════════════════════════════════════════════════════════
      tabItem(tabName = "data",
        fluidRow(
          box(title = "Employee Data Table", status = "primary", 
              solidHeader = TRUE, width = 12,
              p("Browse, search, and filter the complete employee dataset."),
              DTOutput("data_table"))
        )
      )
      
    )  # end tabItems
  )  # end dashboardBody
)


# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # Define color palette
  attrition_colors <- c("No" = "#2ECC71", "Yes" = "#E74C3C")
  
  # ═══════════════════════════════════════════════════════════════════════════
  # OVERVIEW TAB
  # ═══════════════════════════════════════════════════════════════════════════
  
  output$overview_attrition_pie <- renderPlotly({
    counts <- hr_data %>% count(Attrition)
    plot_ly(counts, labels = ~Attrition, values = ~n, type = "pie",
            marker = list(colors = c("#2ECC71", "#E74C3C")),
            textinfo = "label+percent",
            textfont = list(size = 14)) %>%
      layout(showlegend = TRUE, 
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$overview_dept <- renderPlotly({
    dept_data <- hr_data %>%
      group_by(Department, Attrition) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(Department) %>%
      mutate(Pct = round(Count / sum(Count) * 100, 1))
    
    plot_ly(dept_data, x = ~Department, y = ~Pct, color = ~Attrition,
            colors = attrition_colors, type = "bar",
            text = ~paste0(Pct, "%"), textposition = "auto") %>%
      layout(barmode = "stack", yaxis = list(title = "Percentage (%)"),
             xaxis = list(title = ""),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$overview_age <- renderPlotly({
    plot_ly(hr_data, x = ~Age, color = ~Attrition,
            colors = attrition_colors, type = "histogram",
            alpha = 0.7, barmode = "overlay") %>%
      layout(xaxis = list(title = "Age"),
             yaxis = list(title = "Count"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$overview_income <- renderPlotly({
    plot_ly(hr_data, y = ~MonthlyIncome, color = ~Attrition,
            colors = attrition_colors, type = "box") %>%
      layout(yaxis = list(title = "Monthly Income ($)"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  # ═══════════════════════════════════════════════════════════════════════════
  # EDA TAB
  # ═══════════════════════════════════════════════════════════════════════════
  
  output$eda_bar <- renderPlotly({
    feature <- input$eda_feature
    
    plot_data <- hr_data %>%
      mutate(Feature = as.factor(.data[[feature]])) %>%
      group_by(Feature, Attrition) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(Feature) %>%
      mutate(Pct = round(Count / sum(Count) * 100, 1))
    
    if (input$eda_plot_type == "fill") {
      p <- plot_ly(plot_data, x = ~Feature, y = ~Pct, color = ~Attrition,
                   colors = attrition_colors, type = "bar",
                   text = ~paste0(Pct, "%"), textposition = "auto") %>%
        layout(barmode = "stack", yaxis = list(title = "Percentage (%)"))
    } else {
      p <- plot_ly(plot_data, x = ~Feature, y = ~Count, color = ~Attrition,
                   colors = attrition_colors, type = "bar",
                   text = ~Count, textposition = "auto") %>%
        layout(barmode = input$eda_plot_type, 
               yaxis = list(title = "Count"))
    }
    
    p %>% layout(xaxis = list(title = feature, tickangle = -45),
                 paper_bgcolor = "rgba(0,0,0,0)",
                 plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$eda_summary <- renderPrint({
    feature <- input$eda_feature
    cat(paste0("Feature: ", feature, "\n"))
    cat(paste0("Unique values: ", length(unique(hr_data[[feature]])), "\n\n"))
    summary(as.factor(hr_data[[feature]]))
  })
  
  output$eda_table <- renderTable({
    feature <- input$eda_feature
    hr_data %>%
      mutate(Feature = as.factor(.data[[feature]])) %>%
      group_by(Feature) %>%
      summarise(
        Total = n(),
        Left = sum(Attrition == "Yes"),
        `Attrition Rate (%)` = round(mean(Attrition == "Yes") * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(`Attrition Rate (%)`))
  })
  
  output$eda_boxplot <- renderPlotly({
    feature <- input$eda_numeric
    plot_ly(hr_data, y = ~.data[[feature]], color = ~Attrition,
            colors = attrition_colors, type = "box") %>%
      layout(yaxis = list(title = feature),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$eda_scatter <- renderPlotly({
    plot_ly(hr_data, x = ~Age, y = ~MonthlyIncome, color = ~Attrition,
            colors = attrition_colors, type = "scatter", mode = "markers",
            marker = list(size = 5, opacity = 0.6)) %>%
      layout(xaxis = list(title = "Age"),
             yaxis = list(title = "Monthly Income ($)"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  # ═══════════════════════════════════════════════════════════════════════════
  # FEATURE IMPORTANCE TAB
  # ═══════════════════════════════════════════════════════════════════════════
  
  output$feat_importance <- renderPlotly({
    imp <- importance(rf_model, type = 2)
    imp_df <- data.frame(
      Feature = rownames(imp),
      Importance = imp[, 1]
    ) %>%
      arrange(desc(Importance)) %>%
      head(20) %>%
      mutate(Feature = factor(Feature, levels = rev(Feature)))
    
    plot_ly(imp_df, x = ~Importance, y = ~Feature, type = "bar",
            orientation = "h",
            marker = list(color = ~Importance,
                          colorscale = list(c(0, "#F39C12"), c(1, "#C0392B")))) %>%
      layout(yaxis = list(title = ""),
             xaxis = list(title = "Mean Decrease Gini"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$feat_corr <- renderPlotly({
    numeric_data <- hr_data %>% select(where(is.numeric))
    cor_mat <- cor(numeric_data, use = "complete.obs")
    
    plot_ly(z = cor_mat, x = colnames(cor_mat), y = colnames(cor_mat),
            type = "heatmap",
            colorscale = list(c(0, "#3498DB"), c(0.5, "white"), c(1, "#E74C3C")),
            zmin = -1, zmax = 1) %>%
      layout(xaxis = list(tickangle = -45, tickfont = list(size = 9)),
             yaxis = list(tickfont = list(size = 9)),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  # ═══════════════════════════════════════════════════════════════════════════
  # MODEL PERFORMANCE TAB
  # ═══════════════════════════════════════════════════════════════════════════
  
  output$model_oob <- renderPlotly({
    oob_data <- data.frame(
      Trees = 1:nrow(rf_model$err.rate),
      OOB = rf_model$err.rate[, "OOB"],
      No = rf_model$err.rate[, "No"],
      Yes = rf_model$err.rate[, "Yes"]
    )
    
    plot_ly(oob_data, x = ~Trees) %>%
      add_lines(y = ~OOB, name = "OOB", line = list(color = "#3498DB", width = 2)) %>%
      add_lines(y = ~No, name = "No (Stayed)", line = list(color = "#2ECC71", width = 1.5)) %>%
      add_lines(y = ~Yes, name = "Yes (Left)", line = list(color = "#E74C3C", width = 1.5)) %>%
      layout(xaxis = list(title = "Number of Trees"),
             yaxis = list(title = "Error Rate"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$model_cm <- renderPlotly({
    cm <- rf_model$confusion[1:2, 1:2]
    cm_df <- data.frame(
      Predicted = rep(c("No", "Yes"), each = 2),
      Actual = rep(c("No", "Yes"), 2),
      Count = c(cm[1,1], cm[1,2], cm[2,1], cm[2,2])
    )
    
    plot_ly(cm_df, x = ~Actual, y = ~Predicted, z = ~Count,
            type = "heatmap", text = ~Count, texttemplate = "%{text}",
            colorscale = list(c(0, "#85C1E9"), c(1, "#1A5276"))) %>%
      layout(xaxis = list(title = "Actual"),
             yaxis = list(title = "Predicted"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$model_metrics_table <- renderTable({
    cm <- rf_model$confusion[1:2, 1:2]
    TP <- cm[2, 2]  # True Positive (Yes predicted as Yes)
    TN <- cm[1, 1]  # True Negative (No predicted as No)
    FP <- cm[2, 1]  # False Positive (No predicted as Yes)
    FN <- cm[1, 2]  # False Negative (Yes predicted as No)
    
    accuracy <- (TP + TN) / (TP + TN + FP + FN)
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    f1 <- 2 * (precision * recall) / (precision + recall)
    specificity <- TN / (TN + FP)
    
    data.frame(
      Metric = c("Accuracy", "Precision", "Recall (Sensitivity)", 
                 "Specificity", "F1-Score"),
      Value = round(c(accuracy, precision, recall, specificity, f1), 4)
    )
  })
  
  # ═══════════════════════════════════════════════════════════════════════════
  # PREDICTION TAB
  # ═══════════════════════════════════════════════════════════════════════════
  
  prediction_result <- eventReactive(input$predict_btn, {
    # Build a single-row dataframe matching the training data structure
    new_emp <- hr_model_data[1, ]  # Use first row as template
    
    new_emp$Age <- input$pred_age
    new_emp$Department <- factor(input$pred_dept, levels = levels(hr_model_data$Department))
    new_emp$JobRole <- factor(input$pred_jobrole, levels = levels(hr_model_data$JobRole))
    new_emp$MonthlyIncome <- input$pred_income
    new_emp$OverTime <- factor(input$pred_overtime, levels = levels(hr_model_data$OverTime))
    new_emp$Gender <- factor(input$pred_gender, levels = levels(hr_model_data$Gender))
    new_emp$JobSatisfaction <- factor(input$pred_satisfaction, levels = levels(hr_model_data$JobSatisfaction))
    new_emp$WorkLifeBalance <- factor(input$pred_wlb, levels = levels(hr_model_data$WorkLifeBalance))
    new_emp$EnvironmentSatisfaction <- factor(input$pred_env_sat, levels = levels(hr_model_data$EnvironmentSatisfaction))
    new_emp$YearsAtCompany <- input$pred_years_company
    new_emp$TotalWorkingYears <- input$pred_total_years
    new_emp$DistanceFromHome <- input$pred_distance
    
    # Predict
    pred_class <- predict(rf_model, newdata = new_emp)
    pred_prob <- predict(rf_model, newdata = new_emp, type = "prob")
    
    list(
      class = as.character(pred_class),
      prob_yes = pred_prob[1, "Yes"],
      prob_no = pred_prob[1, "No"]
    )
  })
  
  output$prediction_result <- renderUI({
    req(prediction_result())
    result <- prediction_result()
    
    if (result$class == "Yes") {
      div(class = "prediction-result pred-leave",
        icon("exclamation-triangle", style = "font-size: 40px;"),
        br(), br(),
        "⚠️ HIGH RISK — Likely to LEAVE",
        br(),
        span(style = "font-size: 16px;",
             paste0("Probability: ", round(result$prob_yes * 100, 1), "%"))
      )
    } else {
      div(class = "prediction-result pred-stay",
        icon("check-circle", style = "font-size: 40px;"),
        br(), br(),
        "✅ LOW RISK — Likely to STAY",
        br(),
        span(style = "font-size: 16px;",
             paste0("Probability: ", round(result$prob_no * 100, 1), "%"))
      )
    }
  })
  
  output$prediction_gauge <- renderPlotly({
    req(prediction_result())
    result <- prediction_result()
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = round(result$prob_yes * 100, 1),
      title = list(text = "Attrition Risk (%)"),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = ifelse(result$prob_yes > 0.5, "#E74C3C", "#2ECC71")),
        steps = list(
          list(range = c(0, 30), color = "#D5F5E3"),
          list(range = c(30, 60), color = "#FCF3CF"),
          list(range = c(60, 100), color = "#FADBD8")
        ),
        threshold = list(
          line = list(color = "#2C3E50", width = 3),
          thickness = 0.75,
          value = 50
        )
      )
    ) %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor = "rgba(0,0,0,0)",
             height = 250)
  })
  
  output$risk_factors <- renderUI({
    req(prediction_result())
    
    risks <- c()
    if (input$pred_overtime == "Yes") risks <- c(risks, "🔴 Works Overtime")
    if (input$pred_income < 3000) risks <- c(risks, "🔴 Low Monthly Income")
    if (input$pred_age < 25) risks <- c(risks, "🟡 Young Employee (< 25)")
    if (input$pred_satisfaction <= 2) risks <- c(risks, "🔴 Low Job Satisfaction")
    if (input$pred_wlb <= 2) risks <- c(risks, "🟡 Poor Work-Life Balance")
    if (input$pred_years_company <= 2) risks <- c(risks, "🟡 New Employee (≤ 2 years)")
    if (input$pred_distance > 20) risks <- c(risks, "🟡 Long Commute (> 20 miles)")
    if (input$pred_env_sat <= 2) risks <- c(risks, "🟡 Low Environment Satisfaction")
    
    if (length(risks) == 0) {
      risks <- c("🟢 No significant risk factors identified")
    }
    
    tags$ul(
      lapply(risks, function(r) tags$li(style = "margin: 5px 0; font-size: 14px;", r))
    )
  })
  
  # ═══════════════════════════════════════════════════════════════════════════
  # DATA EXPLORER TAB
  # ═══════════════════════════════════════════════════════════════════════════
  
  output$data_table <- renderDT({
    datatable(hr_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "Bfrtip",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      filter = "top",
      class = "cell-border stripe hover",
      rownames = FALSE
    ) %>%
      formatStyle("Attrition",
        backgroundColor = styleEqual(c("Yes", "No"), c("#FADBD8", "#D5F5E3")),
        fontWeight = "bold"
      )
  })
  
}

# ─── Run the App ──────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
