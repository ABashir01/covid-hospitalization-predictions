library(tidyverse)

load_and_prepare_data <- function() {
  covid_data <- read_csv("data/hospitalization_rates.csv")
  wastewater_data <- read_csv("data/wastewater_trends.csv")
  
  ga_hosp <- covid_data %>%
    filter(State == "Georgia") %>%
    filter(`_WeekendDate` >= "2023-01-01") %>%
    filter(`_WeekendDate` <= "2024-12-31") %>%
    filter(AgeCategory_Legend == "All" & 
           Sex_Label == "All" & 
           Race_Label == "All") %>%
    mutate(week = as.Date(`_WeekendDate`)) %>%
    select(week, WeeklyRate) %>%
    arrange(week)
  
  ga_wastewater <- wastewater_data %>%
    filter(`State/Territory` == "Georgia") %>%
    filter(Week_Ending_Date >= "2023-01-01") %>%
    filter(Week_Ending_Date <= "2024-12-31") %>%
    mutate(week = as.Date(Week_Ending_Date)) %>%
    select(week, `State/Territory_WVAL`) %>%
    arrange(week)
  
  model_data <- ga_hosp %>%
    inner_join(ga_wastewater, by = "week") %>%
    arrange(week)
  
  return(model_data)
}

create_features <- function(data) {
  data %>%
    mutate(
      hosp_rate_target = lead(WeeklyRate, 2),
      wastewater_current = `State/Territory_WVAL`,
      wastewater_change_2w = wastewater_current - lag(wastewater_current, 2),
      wastewater_seasonal = sin(2 * pi * as.numeric(format(week, "%U")) / 52),
      wastewater_cos_seasonal = cos(2 * pi * as.numeric(format(week, "%U")) / 52),
      wastewater_ma_2w_temp = (wastewater_current + lag(wastewater_current, 1)) / 2,
      wastewater_vol_2w = sqrt((wastewater_current - wastewater_ma_2w_temp)^2 + 
                              (lag(wastewater_current, 1) - wastewater_ma_2w_temp)^2) / sqrt(2),
      wastewater_ma_3w_temp = (wastewater_current + 
                               lag(wastewater_current, 1) + 
                               lag(wastewater_current, 2)) / 3,
      wastewater_vol_3w = sqrt(((wastewater_current - wastewater_ma_3w_temp)^2 + 
                               (lag(wastewater_current, 1) - wastewater_ma_3w_temp)^2 + 
                               (lag(wastewater_current, 2) - wastewater_ma_3w_temp)^2) / 3),
      wastewater_ma_4w_temp = (wastewater_current + 
                               lag(wastewater_current, 1) + 
                               lag(wastewater_current, 2) + 
                               lag(wastewater_current, 3)) / 4,
      wastewater_vol_4w = sqrt(((wastewater_current - wastewater_ma_4w_temp)^2 + 
                               (lag(wastewater_current, 1) - wastewater_ma_4w_temp)^2 + 
                               (lag(wastewater_current, 2) - wastewater_ma_4w_temp)^2 + 
                               (lag(wastewater_current, 3) - wastewater_ma_4w_temp)^2) / 4)
    ) %>%
    select(-wastewater_ma_2w_temp, 
           -wastewater_ma_3w_temp, 
           -wastewater_ma_4w_temp) %>%
    drop_na()
}

train_model <- function(data) {
  lm(hosp_rate_target ~ WeeklyRate + wastewater_current + wastewater_change_2w + 
     wastewater_seasonal + wastewater_cos_seasonal + 
     wastewater_vol_2w + wastewater_vol_3w + wastewater_vol_4w, 
     data = data)
}

calculate_metrics <- function(actual, predicted) {
  list(
    rmse = sqrt(mean((actual - predicted)^2)),
    mae = mean(abs(actual - predicted)),
    r_squared = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  )
}

create_plot <- function(actual, predicted, title, filename) {
  png(filename, width = 800, height = 600)
  plot(actual, predicted, 
       xlab = "Actual 2-Week Ahead Rate", 
       ylab = "Predicted 2-Week Ahead Rate",
       main = title, pch = 16, col = "blue")
  abline(0, 1, col = "red", lwd = 2)
  r_squared <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  text(x = max(actual) * 0.1, y = max(predicted) * 0.9,
       labels = paste0("R² = ", round(r_squared, 3)), 
       pos = 4, cex = 1.2)
  dev.off()
}

load_2025_data <- function() {
  covid_data_2025 <- read_csv("data/hospitalization_rates.csv")
  wastewater_data_2025 <- read_csv("data/wastewater_trends.csv")
  
  ga_hosp_2025 <- covid_data_2025 %>%
    filter(State == "Georgia") %>%
    filter(`_WeekendDate` >= "2025-01-01") %>%
    filter(AgeCategory_Legend == "All" & 
           Sex_Label == "All" & 
           Race_Label == "All") %>%
    mutate(week = as.Date(`_WeekendDate`)) %>%
    select(week, WeeklyRate) %>%
    arrange(week) %>%
    mutate(WeeklyRate = as.numeric(WeeklyRate))
  
  ga_wastewater_2025 <- wastewater_data_2025 %>%
    filter(`State/Territory` == "Georgia") %>%
    filter(Week_Ending_Date >= "2025-01-01") %>%
    filter(Week_Ending_Date <= "2025-12-31") %>%
    mutate(week = as.Date(Week_Ending_Date)) %>%
    select(week, `State/Territory_WVAL`) %>%
    arrange(week)
  
  model_data_2025 <- ga_hosp_2025 %>%
    inner_join(ga_wastewater_2025, by = "week") %>%
    arrange(week)
  
  return(model_data_2025)
}

main <- function() {
  model_data <- load_and_prepare_data()
  model_data_lagged <- create_features(model_data)
  
  lm_model <- train_model(model_data_lagged)
  predictions <- predict(lm_model)
  
  saveRDS(lm_model, 
          "arx_model/output/covid_prediction_model.rds")
  write_csv(model_data_lagged, 
            "arx_model/output/model_data.csv")
  
  metrics <- calculate_metrics(model_data_lagged$hosp_rate_target, predictions)
  
  model_data_2025 <- load_2025_data()
  
#   Null check for safety
  if (nrow(model_data_2025) > 0) {
    model_data_2025_lagged <- create_features(model_data_2025)
    
    if (nrow(model_data_2025_lagged) > 0) {
      predictions_2025 <- predict(lm_model, newdata = model_data_2025_lagged)
      actual_2025 <- model_data_2025_lagged$hosp_rate_target
      
      metrics_2025 <- calculate_metrics(actual_2025, predictions_2025)
      
      png("arx_model/analysis_output/2025_prediction_comparison.png", 
          width = 1000, height = 600)
      par(mfrow = c(1, 2))
      
      plot(model_data_lagged$hosp_rate_target, predictions, 
           xlab = "Actual 2-Week Ahead Rate", 
           ylab = "Predicted 2-Week Ahead Rate",
           main = "Training (2023-2024)", pch = 16, col = "blue")
      abline(0, 1, col = "red", lwd = 2)
      text(x = max(model_data_lagged$hosp_rate_target) * 0.1, 
           y = max(predictions) * 0.9,
           labels = paste0("R² = ", round(metrics$r_squared, 3)), 
           pos = 4, cex = 1.2)
      
      plot(actual_2025, predictions_2025, 
           xlab = "Actual 2-Week Ahead Rate", 
           ylab = "Predicted 2-Week Ahead Rate",
           main = "2025 Predictions", pch = 16, col = "green")
      abline(0, 1, col = "red", lwd = 2)
      text(x = max(actual_2025) * 0.1, 
           y = max(predictions_2025) * 0.9,
           labels = paste0("R² = ", round(metrics_2025$r_squared, 3)), 
           pos = 4, cex = 1.2)
      
      dev.off()
      
      results_summary <- data.frame(
        Metric = c("RMSE_Training", "MAE_Training", "R_squared_Training", 
                  "RMSE_2025", "MAE_2025", "R_squared_2025"),
        Value = c(metrics$rmse, metrics$mae, metrics$r_squared, 
                  metrics_2025$rmse, metrics_2025$mae, metrics_2025$r_squared)
      )
      
      write_csv(results_summary, 
                "arx_model/analysis_output/model_accuracy_metrics.csv")
      
      predictions_df <- model_data_2025_lagged %>%
        mutate(
          actual_rate = hosp_rate_target,
          predicted_rate = predictions_2025,
          error = hosp_rate_target - predictions_2025,
          abs_error = abs(hosp_rate_target - predictions_2025)
        ) %>%
        select(week, wastewater_current, wastewater_seasonal, 
               wastewater_cos_seasonal, wastewater_vol_2w, 
               wastewater_vol_3w, wastewater_vol_4w, 
               actual_rate, predicted_rate, error, abs_error)
      
      write_csv(predictions_df, 
                "arx_model/analysis_output/2025_predictions.csv")
    }
  }
  
  coef_values <- coef(lm_model)
  coef_df <- data.frame(
    Variable = names(coef_values),
    Coefficient = as.numeric(coef_values),
    Non_Zero = as.numeric(coef_values) != 0
  )
  
  write_csv(coef_df, 
            "arx_model/analysis_output/model_coefficients.csv")
}

main()
