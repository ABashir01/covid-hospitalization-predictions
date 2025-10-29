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

compute_ar1_metrics <- function(train_data, test_data, ar1_model, ar1_test_pred) {
  train_pred <- predict(ar1_model)
  train_r2 <- 1 - sum((train_data$hosp_rate_target - train_pred)^2) /
              sum((train_data$hosp_rate_target - mean(train_data$hosp_rate_target))^2)
  train_rmse <- sqrt(mean((train_data$hosp_rate_target - train_pred)^2))
  train_mae <- mean(abs(train_data$hosp_rate_target - train_pred))
  test_rmse <- sqrt(mean((test_data$hosp_rate_target - ar1_test_pred)^2))
  test_mae <- mean(abs(test_data$hosp_rate_target - ar1_test_pred))
  test_r2 <- 1 - sum((test_data$hosp_rate_target - ar1_test_pred)^2) /
             sum((test_data$hosp_rate_target - mean(test_data$hosp_rate_target))^2)
  c(
    RMSE_Training = train_rmse,
    MAE_Training = train_mae,
    R_squared_Training = train_r2,
    RMSE_2025 = test_rmse,
    MAE_2025 = test_mae,
    R_squared_2025 = test_r2
  )
}

get_metrics_vector <- function(df, metric_order) {
  sapply(metric_order, function(m) df %>% filter(Metric == m) %>% pull(Value))
}

metric_order <- c(
  "RMSE_Training", "MAE_Training", "R_squared_Training",
  "RMSE_2025", "MAE_2025", "R_squared_2025"
)

ar1_stats <- compute_ar1_metrics(train_data, test_data, ar1_model, ar1_test_pred)
linear_metrics <- read_csv("linear_regression/analysis_output/model_accuracy_metrics.csv", show_col_types = FALSE)
arx_metrics <- read_csv("arx_model/analysis_output/model_accuracy_metrics.csv", show_col_types = FALSE)

comparison_metrics <- data.frame(
  Metric = metric_order,
  AR1_Model = as.numeric(ar1_stats[metric_order]),
  Linear_Model = get_metrics_vector(linear_metrics, metric_order),
  ARX_Model = get_metrics_vector(arx_metrics, metric_order)
)

write_csv(comparison_metrics, "model_comparison_metrics.csv")

comparison_data <- data.frame(
  Model = c("AR1 Model", "Linear Regression Model", "ARX Model"),
  Test_R2 = c(ar1_test_r2, linear_model_test_r2, arx_model_test_r2)
)

png("auto_regression_comparison.png", width = 800, height = 600)
barplot(comparison_data$Test_R2, 
        names.arg = comparison_data$Model,
        main = "Model Performance Comparison (2025 Test Data)",
        ylab = "Test R²",
        col = c("#31A1B3", "#EF6F6A", "#A39FC9"),
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

linear_predictions <- read_csv("linear_regression/analysis_output/2025_predictions.csv", show_col_types = FALSE)
arx_predictions <- read_csv("arx_model/analysis_output/2025_predictions.csv", show_col_types = FALSE)

ar1_predictions_df <- test_data %>%
  mutate(ar1_pred = ar1_test_pred) %>%
  select(week, ar1_pred)

comparison_df <- linear_predictions %>%
  select(week, actual_rate, predicted_rate) %>%
  rename(linear_pred = predicted_rate) %>%
  inner_join(
    arx_predictions %>%
      select(week, predicted_rate) %>%
      rename(arx_pred = predicted_rate),
    by = "week"
  ) %>%
  inner_join(ar1_predictions_df, by = "week") %>%
  arrange(week) %>%
  mutate(week = as.Date(week))

png("2025_predictions_comparison.png", width = 1400, height = 500)
par(mfrow = c(1, 3))

plot_predictions <- function(model_name, predicted_rate, r2, color) {  
  plot(comparison_df$actual_rate, predicted_rate,
      xlab = "Actual 2-Week Ahead Rate", 
      ylab = "Predicted 2-Week Ahead Rate",
      main = paste(model_name, "- 2025 Predictions"), 
      pch = 16, col = color)
  abline(0, 1, col = "black", lwd = 2)
  text(x = max(comparison_df$actual_rate) * 0.1,
      y = max(predicted_rate) * 0.9,
      pos = 4, cex = 1.2)
}

plot_predictions("AR1 Model", comparison_df$ar1_pred, ar1_test_r2, "#31A1B3")
plot_predictions("Linear Model", comparison_df$linear_pred, linear_model_test_r2, "#EF6F6A")
plot_predictions("ARX Model", comparison_df$arx_pred, arx_model_test_r2, "#A39FC9")

dev.off()
