library(tidyverse)

hosp_data <- read_csv("data/hospitalization_rates.csv", show_col_types = FALSE)

ga_hosp <- hosp_data %>%
  filter(State == "Georgia") %>%
  filter(AgeCategory_Legend == "All" & Sex_Label == "All" & Race_Label == "All") %>%
  mutate(week = as.Date(`_WeekendDate`)) %>%
  dplyr::select(week, WeeklyRate) %>%
  arrange(week) %>%
  mutate(WeeklyRate = as.numeric(WeeklyRate))

train_data <- ga_hosp %>%
  filter(week >= "2023-01-01" & week <= "2024-12-31") %>%
  mutate(hosp_rate_target = lead(WeeklyRate, 2)) %>%
  drop_na()

test_data <- ga_hosp %>%
  filter(week >= "2025-01-01") %>%
  mutate(hosp_rate_target = lead(WeeklyRate, 2)) %>%
  drop_na()

ar1_model <- lm(hosp_rate_target ~ WeeklyRate, data = train_data)
ar1_test_pred <- predict(ar1_model, newdata = test_data)
ar1_test_r2 <- 1 - sum((test_data$hosp_rate_target - ar1_test_pred)^2) / sum((test_data$hosp_rate_target - mean(test_data$hosp_rate_target))^2)

linear_model_results <- read_csv("linear_regression/analysis_output/model_accuracy_metrics.csv", show_col_types = FALSE)
linear_model_test_r2 <- linear_model_results %>%
  filter(Metric == "R_squared_2025") %>%
  pull(Value)

arx_model_results <- read_csv("arx_model/analysis_output/model_accuracy_metrics.csv", show_col_types = FALSE)
arx_model_test_r2 <- arx_model_results %>%
  filter(Metric == "R_squared_2025") %>%
  pull(Value)

comparison_data <- data.frame(
  Model = c("AR1 Model", "Linear Regression Model", "ARX Model"),
  Test_R2 = c(ar1_test_r2, linear_model_test_r2, arx_model_test_r2)
)

png("ar1_vs_wastewater_comparison.png", width = 800, height = 600)
barplot(comparison_data$Test_R2, 
        names.arg = comparison_data$Model,
        main = "Model Performance Comparison (2025 Test Data)",
        ylab = "Test R²",
        col = c("lightblue", "lightgreen", "lightyellow"),
        ylim = c(0, max(comparison_data$Test_R2) * 1.1))

text(x = 0.7, y = ar1_test_r2 + 0.01, 
     labels = paste0("R² = ", round(ar1_test_r2, 3)), 
     pos = 3, cex = 1.2)  
text(x = 1.9, y = linear_model_test_r2 + 0.01, 
     labels = paste0("R² = ", round(linear_model_test_r2, 3)), 
     pos = 3, cex = 1.2)
text(x = 3.1, y = arx_model_test_r2 + 0.01, 
     labels = paste0("R² = ", round(arx_model_test_r2, 3)), 
     pos = 3, cex = 1.2)

dev.off()