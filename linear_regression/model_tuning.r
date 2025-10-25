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

create_features <- function(data, feature_config) {
  result <- data %>%
    mutate(
      hosp_rate_target = lead(WeeklyRate, 2),
      wastewater_current = `State/Territory_WVAL`
    )
  
  if (feature_config["change_1w"]) {
    result <- result %>% mutate(wastewater_change_1w = wastewater_current - lag(wastewater_current, 1))
  }
  if (feature_config["change_2w"]) {
    result <- result %>% mutate(wastewater_change_2w = wastewater_current - lag(wastewater_current, 2))
  }
  if (feature_config["ma_2w"]) {
    result <- result %>% mutate(wastewater_ma_2w = (wastewater_current + lag(wastewater_current, 1)) / 2)
  }
  if (feature_config["ma_3w"]) {
    result <- result %>% mutate(wastewater_ma_3w = (wastewater_current + lag(wastewater_current, 1) + lag(wastewater_current, 2)) / 3)
  }
  if (feature_config["ma_4w"]) {
    result <- result %>% mutate(wastewater_ma_4w = (wastewater_current + lag(wastewater_current, 1) + lag(wastewater_current, 2) + lag(wastewater_current, 3)) / 4)
  }
  if (feature_config["seasonal"]) {
    result <- result %>% mutate(wastewater_seasonal = sin(2 * pi * as.numeric(format(week, "%U")) / 52))
  }
  if (feature_config["cos_seasonal"]) {
    result <- result %>% mutate(wastewater_cos_seasonal = cos(2 * pi * as.numeric(format(week, "%U")) / 52))
  }
  if (feature_config["vol_2w"]) {
    result <- result %>% mutate(
      ma_2w_temp = (wastewater_current + lag(wastewater_current, 1)) / 2,
      wastewater_vol_2w = sqrt((wastewater_current - ma_2w_temp)^2 + (lag(wastewater_current, 1) - ma_2w_temp)^2) / sqrt(2)
    ) %>% select(-ma_2w_temp)
  }
  if (feature_config["vol_3w"]) {
    result <- result %>% mutate(
      ma_3w_temp = (wastewater_current + lag(wastewater_current, 1) + lag(wastewater_current, 2)) / 3,
      wastewater_vol_3w = sqrt(((wastewater_current - ma_3w_temp)^2 + 
                                (lag(wastewater_current, 1) - ma_3w_temp)^2 + 
                                (lag(wastewater_current, 2) - ma_3w_temp)^2) / 3)
    ) %>% select(-ma_3w_temp)
  }
  if (feature_config["vol_4w"]) {
    result <- result %>% mutate(
      ma_4w_temp = (wastewater_current + lag(wastewater_current, 1) + lag(wastewater_current, 2) + lag(wastewater_current, 3)) / 4,
      wastewater_vol_4w = sqrt(((wastewater_current - ma_4w_temp)^2 + 
                                (lag(wastewater_current, 1) - ma_4w_temp)^2 + 
                                (lag(wastewater_current, 2) - ma_4w_temp)^2 + 
                                (lag(wastewater_current, 3) - ma_4w_temp)^2) / 4)
    ) %>% select(-ma_4w_temp)
  }
  
  result %>% drop_na()
}

build_formula <- function(feature_config) {
  features <- c("wastewater_current")
  
  if (feature_config["change_1w"]) features <- c(features, "wastewater_change_1w")
  if (feature_config["change_2w"]) features <- c(features, "wastewater_change_2w")
  if (feature_config["ma_2w"]) features <- c(features, "wastewater_ma_2w")
  if (feature_config["ma_3w"]) features <- c(features, "wastewater_ma_3w")
  if (feature_config["ma_4w"]) features <- c(features, "wastewater_ma_4w")
  if (feature_config["seasonal"]) features <- c(features, "wastewater_seasonal")
  if (feature_config["cos_seasonal"]) features <- c(features, "wastewater_cos_seasonal")
  if (feature_config["vol_2w"]) features <- c(features, "wastewater_vol_2w")
  if (feature_config["vol_3w"]) features <- c(features, "wastewater_vol_3w")
  if (feature_config["vol_4w"]) features <- c(features, "wastewater_vol_4w")
  
  formula_str <- paste("hosp_rate_target ~", paste(features, collapse = " + "))
  return(as.formula(formula_str))
}

train_and_evaluate <- function(train_data, test_data, feature_config) {
  formula <- build_formula(feature_config)
  model <- lm(formula, data = train_data)
  
  train_pred <- predict(model)
  test_pred <- predict(model, newdata = test_data)
  
  train_r2 <- 1 - sum((train_data$hosp_rate_target - train_pred)^2) / 
              sum((train_data$hosp_rate_target - mean(train_data$hosp_rate_target))^2)
  
  test_r2 <- 1 - sum((test_data$hosp_rate_target - test_pred)^2) / 
             sum((test_data$hosp_rate_target - mean(test_data$hosp_rate_target))^2)
  
  list(train_r2 = train_r2, test_r2 = test_r2, model = model)
}

generate_feature_combinations <- function() {
  base_features <- c("change_1w", "change_2w", "ma_2w", "ma_3w", "ma_4w", 
                     "seasonal", "cos_seasonal", "vol_2w", "vol_3w", "vol_4w")
  
  combinations <- list()
  for (i in 1:length(base_features)) {
    combs <- combn(base_features, i, simplify = FALSE)
    for (comb in combs) {
      config <- setNames(rep(FALSE, length(base_features)), base_features)
      config[comb] <- TRUE
      combinations <- append(combinations, list(config))
    }
  }
  return(combinations)
}

main <- function() {
  train_data_raw <- load_and_prepare_data()
  test_data_raw <- load_2025_data()
  
  if (nrow(test_data_raw) == 0) {
    stop("No 2025 test data available")
  }
  
  feature_combinations <- generate_feature_combinations()
  results <- data.frame()
  
  for (i in seq_along(feature_combinations)) {
    config <- feature_combinations[[i]]
    
    train_data <- create_features(train_data_raw, config)
    test_data <- create_features(test_data_raw, config)
    
    if (nrow(test_data) == 0) next
    
    evaluation <- train_and_evaluate(train_data, test_data, config)
    
    feature_names <- paste(names(config)[config], collapse = ", ")
    
    results <- rbind(results, data.frame(
      config_id = i,
      features = feature_names,
      train_r2 = evaluation$train_r2,
      test_r2 = evaluation$test_r2,
      n_train = nrow(train_data),
      n_test = nrow(test_data),
      n_features = sum(config) + 1
    ))
  }
  
  results <- results %>%
    arrange(desc(test_r2)) %>%
    mutate(rank = row_number())
  
  dir.create("linear_regression/tuning_results", showWarnings = FALSE)
  write_csv(results, "linear_regression/tuning_results/tuning_results.csv")
  
  best_config <- feature_combinations[[results$config_id[1]]]
  best_features <- paste(names(best_config)[best_config], collapse = ", ")
  
  summary_stats <- results %>%
    summarise(
      total_models_tested = n(),
      best_test_r2 = max(test_r2, na.rm = TRUE),
      best_features = best_features,
      mean_test_r2 = mean(test_r2, na.rm = TRUE),
      median_test_r2 = median(test_r2, na.rm = TRUE)
    )
  
  write_csv(summary_stats, "linear_regression/tuning_results/summary_stats.csv")
    
  return(results)
}

main()
