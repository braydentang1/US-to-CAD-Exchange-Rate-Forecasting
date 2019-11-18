library(tidyverse)
library(forecast)
library(caret)
library(parallel)
library(smooth)
library(lubridate)
library(alfred)

#......................Fit the ensemble model...............................#

inner_train = function(train_fold_index, data, iterations, xreg) {
  
  #' Validates the inner train set using walk forward cross validation.
  #'
  #' @param train_fold_index A vector of indices to use as the training fold
  #' @param data The complete time series as a ts object
  #' @param iterations The number of iterations of random grid search to use
  #' @param xreg A matrix 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  
  set.seed(200350623)
  random_grid <- tibble(
    rwd_weight = sample.int(10000, size = iterations, replace = TRUE), 
    tbats_weight = sample.int(10000, size = iterations, replace = TRUE), 
    arima_weight = sample.int(10000, size = iterations, replace = TRUE),
    ces_weight = sample.int(10000, size = iterations, replace = TRUE)) %>% 
    mutate(sum_of_weights = rowSums(.)) 
  
  outer_train_time <- time(data)[train_fold_index]
  outer_train <- window(
    data,
    start = outer_train_time[1], 
    end = outer_train_time[length(outer_train_time)])
  
  all_inner_sets <- createTimeSlices(
    y = outer_train, 
    initialWindow = 150,
    horizon = 3, 
    fixedWindow = FALSE)
  
  all_inner_train <- all_inner_sets$train
  all_validation <- all_inner_sets$test
  
  scores <- vector("list", length(all_inner_train))
  
  for (k in 1:length(all_inner_train)) {
    
    train_time <- time(data)[all_inner_train[[k]]]
    train <- window(
      data,
      start = train_time[1],
      end = train_time[length(train_time)])
    
    xreg_train <- xreg[1:length(train_time), ]
    
    validation_time <- time(data)[all_validation[[k]]]
    
    validation <- window(
      data,
      start = validation_time[1],
      end = validation_time[length(validation_time)])
    
    xreg_validation <- xreg[1:length(validation_time), ]
    
    all_predictions <- grab_predictions(
      train = train,
      horizon = 3, 
      xreg_train = xreg_train, 
      xreg_newdata = xreg_validation) %>% 
      reduce(ts.intersect) %>% 
      as_tibble(.) %>%
      set_names(c("rwd", "tbats", "arima", "ces"))
    
    scores[[k]] <- random_grid_search(
      grid = random_grid, 
      predictions = all_predictions,
      validation = validation)
    
  }
  
  final_grid <- scores %>% 
    reduce(cbind) %>% 
    rowMeans(.) %>% 
    bind_cols(random_grid, score = .)
  
  final_parameters <- final_grid[which.min(final_grid$score), ]
  
  final_parameters
  
}

outer_fold <- function(data, train_folds_index, test_folds_index, parameters, xreg) {
  
  outer_train_time <- time(data)[train_folds_index]
  
  outer_train <- window(
    data, 
    start = outer_train_time[1], 
    end = outer_train_time[length(outer_train_time)])
  
  xreg_train <- xreg[1:length(outer_train_time), ]
  
  test_time <- time(data)[test_folds_index]
  test <- window(data, start = test_time[1], end = test_time[length(test_time)])
  xreg_test <- xreg[1:length(test_time), ]
  
  final_predictions <- grab_predictions(
    train = outer_train, 
    horizon = 3, 
    xreg_train = xreg_train, 
    xreg_newdata = xreg_test) %>% 
    reduce(ts.intersect) %>%
    as_tibble(.) %>%
    set_names(c("rwd", "tbats", "arima", "ces"))
  
  score <- random_grid_search(
    grid = parameters,
    predictions = final_predictions,
    validation = test)
  
  score
  
}


grab_predictions <- function(train, horizon, xreg_train, xreg_newdata) {
  
  model_average = vector("list", 4)
  
  forecast_rwd <- rwf(y = train, h = horizon, drift = TRUE)
  model_average[[1]] <- forecast_rwd$mean
  
  model_tbats <- tbats(y = train, num.cores = 1)
  forecast_tbats <- forecast(model_tbats, h = horizon)
  model_average[[2]] <- forecast_tbats$mean
  
  arima_model <- auto.arima(
    y = train, 
    ic = "aicc",
    stationary = TRUE,
    xreg = xreg_train)
  
  forecast_arima <- forecast(arima_model, h = horizon, xreg = xreg_newdata)
  model_average[[3]] <- forecast_arima$mean
  
  forecast_ces <- auto.ces(y = train, h = horizon)
  model_average[[4]] <- forecast_ces$forecast
  
  model_average
}


random_grid_search <- function(grid, predictions, validation) {
  
  score <- vector("numeric", nrow(grid))
  constant <- which(colnames(grid) == "sumofWeights")

  for (k in 1:nrow(grid)) {
    
    predictions_tmp <-
      predictions %>% 
      transmute(
        weighted_average =
          rwd * as.numeric(grid[k, 1] / grid[k, constant]) +
          tbats * as.numeric(grid[k, 2] / grid[k, constant]) +
          arima * as.numeric(grid[k, 3] / grid[k, constant]) +
          ces * as.numeric(grid[k, 4] / grid[k, constant])
        )
    
    score[k] <- accuracy(f = predictions_tmp$weighted_average, x = validation)[2]
    
  }
  
  score
  
}

forecast_ensemble <- function(train, horizon, parameters, xreg_train, xreg_newdata) {
  
  predictions <- grab_predictions(
    train = train,
    horizon = horizon,
    xreg_train = xreg_train,
    xreg_newdata = xreg_newdata) %>%
    reduce(., ts.intersect) %>%
    as_tibble(.) %>%
    set_names(c("rwd", "tbats", "arima", "ces"))
  
  constant <- which(colnames(parameters) == "sum_of_weights")
  
  predictions_tmp <-
    predictions %>% 
    transmute(
      weighted_average = 
        rwd * as.numeric(parameters[1, 1] / parameters[1, constant]) +
        tbats * as.numeric(parameters[1, 2] / parameters[1, constant]) +
        arima * as.numeric(parameters[1, 3] / parameters[1, constant]) +
        ces * as.numeric(parameters[1, 4] / parameters[1, constant])
      )
  

}

