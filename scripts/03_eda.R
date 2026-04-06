# =============================================================================
# STEP 3: EXPLORATORY DATA ANALYSIS (EDA)
# =============================================================================
# Objective: Visualize the dataset to uncover patterns and relationships
#            between employee attributes and attrition.
# =============================================================================

cat("\n")
cat("══════════════════════════════════════════════════════════════\n")
cat("  STEP 3: EXPLORATORY DATA ANALYSIS (EDA)\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Create output directory for plots
dir.create(file.path(project_root, "output"), showWarnings = FALSE, recursive = TRUE)

# ─── Define a Custom Theme ────────────────────────────────────────────────────

theme_attrition <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5, color = "#2C3E50"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D"),
    axis.title = element_text(face = "bold", color = "#34495E"),
    axis.text = element_text(color = "#2C3E50"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA)
  )

# Color palette for Attrition
attrition_colors <- c("No" = "#2ECC71", "Yes" = "#E74C3C")

# ─── 3.1 Attrition Distribution ──────────────────────────────────────────────

cat("📊 Plot 1: Attrition Distribution\n")

p1 <- ggplot(hr_data_clean, aes(x = Attrition, fill = Attrition)) +
  geom_bar(width = 0.6, alpha = 0.9) +
  geom_text(stat = "count", aes(label = paste0(after_stat(count), "\n(", 
            round(after_stat(count)/sum(after_stat(count))*100, 1), "%)")),
            vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Employee Attrition Distribution",
       subtitle = "Imbalanced dataset — ~16% attrition rate",
       x = "Attrition Status", y = "Number of Employees") +
  theme_attrition +
  ylim(0, max(table(hr_data_clean$Attrition)) * 1.15)

print(p1)
ggsave(file.path(project_root, "output", "01_attrition_distribution.png"), 
       p1, width = 8, height = 6, dpi = 150)


# ─── 3.2 Attrition by Age ────────────────────────────────────────────────────

cat("📊 Plot 2: Attrition by Age\n")

p2 <- ggplot(hr_data_clean, aes(x = Age, fill = Attrition)) +
  geom_histogram(bins = 25, alpha = 0.8, position = "identity", color = "white") +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Age Distribution by Attrition",
       subtitle = "Younger employees tend to have higher attrition rates",
       x = "Age", y = "Count") +
  theme_attrition

print(p2)
ggsave(file.path(project_root, "output", "02_attrition_by_age.png"), 
       p2, width = 10, height = 6, dpi = 150)


# ─── 3.3 Attrition by Monthly Income ─────────────────────────────────────────

cat("📊 Plot 3: Attrition by Monthly Income\n")

p3 <- ggplot(hr_data_clean, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.3) +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Monthly Income by Attrition Status",
       subtitle = "Employees who leave tend to earn less",
       x = "Attrition", y = "Monthly Income ($)") +
  theme_attrition

print(p3)
ggsave(file.path(project_root, "output", "03_attrition_by_income.png"), 
       p3, width = 8, height = 6, dpi = 150)


# ─── 3.4 Attrition by Job Role ───────────────────────────────────────────────

cat("📊 Plot 4: Attrition by Job Role\n")

role_attrition <- hr_data_clean %>%
  group_by(JobRole, Attrition) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(JobRole) %>%
  mutate(Percentage = Count / sum(Count) * 100)

p4 <- ggplot(role_attrition, aes(x = reorder(JobRole, -Percentage), 
                                   y = Percentage, fill = Attrition)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Attrition Rate by Job Role",
       subtitle = "Some roles experience significantly higher turnover",
       x = "Job Role", y = "Percentage (%)") +
  theme_attrition +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

print(p4)
ggsave(file.path(project_root, "output", "04_attrition_by_jobrole.png"), 
       p4, width = 12, height = 7, dpi = 150)


# ─── 3.5 Attrition by Work-Life Balance ──────────────────────────────────────

cat("📊 Plot 5: Attrition by Work-Life Balance\n")

wlb_labels <- c("1" = "Bad", "2" = "Good", "3" = "Better", "4" = "Best")

wlb_data <- hr_data_clean %>%
  mutate(WLB_Label = factor(wlb_labels[as.character(WorkLifeBalance)],
                            levels = c("Bad", "Good", "Better", "Best")))

p5 <- ggplot(wlb_data, aes(x = WLB_Label, fill = Attrition)) +
  geom_bar(position = "fill", alpha = 0.9) +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Attrition Rate by Work-Life Balance",
       subtitle = "Poor work-life balance correlates with higher attrition",
       x = "Work-Life Balance Rating", y = "Proportion") +
  theme_attrition

print(p5)
ggsave(file.path(project_root, "output", "05_attrition_by_wlb.png"), 
       p5, width = 8, height = 6, dpi = 150)


# ─── 3.6 Attrition by Years at Company ───────────────────────────────────────

cat("📊 Plot 6: Attrition by Years at Company\n")

p6 <- ggplot(hr_data_clean, aes(x = YearsAtCompany, fill = Attrition)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Years at Company — Density by Attrition",
       subtitle = "Employees with fewer years at the company are more likely to leave",
       x = "Years at Company", y = "Density") +
  theme_attrition

print(p6)
ggsave(file.path(project_root, "output", "06_attrition_by_years.png"), 
       p6, width = 10, height = 6, dpi = 150)


# ─── 3.7 Attrition by Overtime ───────────────────────────────────────────────

cat("📊 Plot 7: Attrition by Overtime\n")

p7 <- ggplot(hr_data_clean, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill", alpha = 0.9, width = 0.6) +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Attrition Rate by Overtime Status",
       subtitle = "Employees working overtime are significantly more likely to leave",
       x = "Works Overtime?", y = "Proportion") +
  theme_attrition

print(p7)
ggsave(file.path(project_root, "output", "07_attrition_by_overtime.png"), 
       p7, width = 8, height = 6, dpi = 150)


# ─── 3.8 Attrition by Department ─────────────────────────────────────────────

cat("📊 Plot 8: Attrition by Department\n")

p8 <- ggplot(hr_data_clean, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "fill", alpha = 0.9, width = 0.6) +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Attrition Rate by Department",
       subtitle = "Comparing turnover across organizational departments",
       x = "Department", y = "Proportion") +
  theme_attrition +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p8)
ggsave(file.path(project_root, "output", "08_attrition_by_department.png"), 
       p8, width = 9, height = 6, dpi = 150)


# ─── 3.9 Correlation Matrix (Numeric Features) ──────────────────────────────

cat("📊 Plot 9: Correlation Heatmap\n")

numeric_data <- hr_data_clean %>% select(where(is.numeric))

if (ncol(numeric_data) >= 2) {
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  png(file.path(project_root, "output", "09_correlation_matrix.png"), 
      width = 1000, height = 900, res = 100)
  corrplot(cor_matrix, method = "color", type = "lower",
           tl.cex = 0.7, tl.col = "#2C3E50",
           addCoef.col = "black", number.cex = 0.5,
           col = colorRampPalette(c("#3498DB", "white", "#E74C3C"))(200),
           title = "Correlation Matrix of Numeric Features",
           mar = c(0, 0, 2, 0))
  dev.off()
}


# ─── 3.10 Age vs. Monthly Income by Attrition ────────────────────────────────

cat("📊 Plot 10: Age vs Monthly Income Scatter\n")

p10 <- ggplot(hr_data_clean, aes(x = Age, y = MonthlyIncome, color = Attrition)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_manual(values = attrition_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Age vs. Monthly Income by Attrition",
       subtitle = "Younger, lower-income employees have higher attrition",
       x = "Age", y = "Monthly Income ($)") +
  theme_attrition

print(p10)
ggsave(file.path(project_root, "output", "10_age_vs_income.png"), 
       p10, width = 10, height = 7, dpi = 150)


# ─── 3.11 Job Satisfaction by Attrition ───────────────────────────────────────

cat("📊 Plot 11: Job Satisfaction Distribution\n")

p11 <- ggplot(hr_data_clean, aes(x = JobSatisfaction, fill = Attrition)) +
  geom_bar(position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Job Satisfaction by Attrition Status",
       subtitle = "1 = Low, 2 = Medium, 3 = High, 4 = Very High",
       x = "Job Satisfaction Level", y = "Count") +
  theme_attrition

print(p11)
ggsave(file.path(project_root, "output", "11_job_satisfaction.png"), 
       p11, width = 9, height = 6, dpi = 150)

cat("\n📁 All plots saved to: output/\n")
cat("✅ Step 3 complete — Exploratory Data Analysis finished.\n")
