library(tidyverse)
library(forecast)
library(caret)
library(parallel)
library(zoo)
library(smooth)
library(lubridate)
library(alfred)

#......................Fit the ensemble model...............................#

inner_train <- function(train_fold_index, data, iterations, xreg) {
  
  #' Validates the inner train set using walk forward cross validation.
  #'
  #' @param train_fold_index A vector of indices to use as the training fold
  #' @param data The complete time series as a ts object
  #' @param iterations The number of iterations of random grid search to use
  #' @param xreg A matrix that provides additional variables to include in the ARIMA model. Must be the same length as 
  #'  the vector train_fold_index.
  #'
  #' @return A tibble containing the best weights to use for each model in the weighted average for
  #'  predictions.
  #'
  #' @export
  #'
  
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
    initialWindow = length(outer_train) - 7,
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

outer_fold <- function(data, train_fold_index, test_fold_index, parameters, xreg) {

  #' Trains all the entire training data, and evaluates on the test set using 
  #' walk forward cross validation.
  #'
  #' @param data The complete time series as a ts object
  #' @param train_fold_index A vector of indices to use as the training fold
  #' @param test_fold_index A vector of indices to use as the testing fold
  #' @param parameters A tibble of optimal parameters (weights)
  #'  to use in the weighted average calculation.
  #' @param xreg A matrix that provides additional variables to include in the ARIMA model. Must be the same length as 
  #'  the vector train_fold_index.
  #'
  #' @return A numeric vector of length equal to one containing the root mean squared error.
  #' @export
  #'

  outer_train_time <- time(data)[train_fold_index]
  
  outer_train <- window(
    data, 
    start = outer_train_time[1], 
    end = outer_train_time[length(outer_train_time)])
  
  xreg_train <- xreg[1:length(outer_train_time), ]
  
  test_time <- time(data)[test_fold_index]
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
  
  random_grid_search(
    grid = parameters,
    predictions = final_predictions,
    validation = test)
   
}


grab_predictions <- function(train, horizon, xreg_train, xreg_newdata) {
  
  #' Calculates the predictions for each of the level one learners.
  #'
  #' @param train The training time series
  #' @param horizon The number of steps ahead to forecast
  #' @param xreg_train A matrix that provides additional variables to include in the
  #'  ARIMA model. Must be equal in length to the time series "train".
  #' @param xreg_newdata A matrix that provides the same columns as that of "xreg_train" but 
  #'  for the observations to be predicted. Must be equal in length to that of "horizon".
  #'
  #' @return A list containing the predictions of each model to be used in ensembling.
  #' @export
  #'

  model_average <- vector("list", 4)
  
  forecast_rwd <- rwf(y = train, h = horizon, drift = TRUE)
  model_average[[1]] <- forecast_rwd$mean
  
  model_tbats <- tbats(y = train, num.cores = 1)
  forecast_tbats <- forecast(model_tbats, h = horizon)
  model_average[[2]] <- forecast_tbats$mean
  
  if (all(xreg_train[, "covid"] == 0)) {
    xreg_train <- xreg_train[, !colnames(xreg_train) %in% c("covid")]
    xreg_newdata <- xreg_newdata[, !colnames(xreg_newdata) %in% c("covid")]
  }
  
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

  #' Finds an optimal set of weights for each model via. random grid search.
  #'
  #' @param grid The grid of candidate weights, as a tibble,
  #'  to be used for each model. Should contain weights for the random walk, TBATS, ARIMA, 
  #'  complex exponential smoothing methods in that order, and then a column on the end that
  #'  representing the sums of each row named sum_of_weights.
  #' @param predictions A tibble of predictions from the level one base learners
  #' @param validation The time series object representing the validation set
  #'
  #' @return A list containing the predictions of each model to be used in ensembling.
  #' @export
  #'
  score <- vector("numeric", nrow(grid))
  constant <- which(colnames(grid) == "sum_of_weights")

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
    
    score[k] <- forecast::accuracy(f = predictions_tmp$weighted_average, x = validation)[2]
    
  }
  
  score
  
}

forecast_ensemble <- function(train, horizon, parameters, xreg_train, xreg_newdata) {

  #' Calculates the forecasts using the learned ensemble model.
  #'
  #' @param train The training time series, given as a ts object
  #' @param horizon An integer vector of length one that provides the number of steps ahead
  #'  to forecast.
  #' @param parameters A tibble that gives the optimal weights for each level one base learner
  #' @param xreg_train A matrix that provides additional variables to be used in the ARIMA model.
  #'  Must be of the same length to that of "train".
  #' @param xreg_newdata A matrix that contains the same columns as that of xreg_train, but 
  #'  for new observations. Must be of the same length to that of 'horizon'.
  #'
  #' @return A tibble containing the h-ahead forecast.
  #' @export
  #'
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

point_predictions <- function(x, iterations = 60, xreg, to = NULL) {
  
  #' Fits the ensemble model and then calculates three month forecasts.
  #'
  #' @param x The time series to use when fitting the ensemble models. Must be a ts object.
  #' @param iterations The number of iterations to use in random grid search when fitting the ensemble
  #' @param xreg The exogenous variables to be used for the ARIMAX model in the ensemble.
  #' @param to An optional vector specifying the "to" date as a date object.
  #'
  #' @return A time series of forecasted values three months into the future.
  #' @export
  #'
  
  # Find the final parameters using all of the data
  
  if (!is.null(to)) {
    month_to <- ifelse(month(to) - 1 == 0, 12, month(to) - 1)
    year_to <- ifelse(month(to) - 1 == 0, year(to) - 1, year(to))
    x <- window(x, end = c(year_to, month_to))
  }
  
  best_param_final <- inner_train(
    train_fold_index = 1:length(x),
    data = x, 
    iterations = iterations, 
    xreg = xreg[1:length(x), ]
  )
  
  # Fit the final model
  
  months_end <- month(zoo::as.Date(x))[length(x)]
  years_end <- year(zoo::as.Date(x))[length(x)]
  months_forecast <- c(months_end + 1, months_end + 2, months_end + 3) %>%
    if_else(. > 12, . - 12, .)
  
  xreg_newdata <- tibble(
    great_recession_ind = c(0, 0, 0),
    dotcom_bubble = c(0, 0, 0), 
    twothousandten_levelchange = c(1, 1, 1),
    holidays = if_else(months_forecast == 11 | months_forecast == 12, 1, 0), 
    twothousandtwo_levelchange = c(0, 0, 0)
  )
  
  if (to >= "2020-03-01") {
    xreg_newdata$covid = c(1, 1, 1)
  } 
  
  ts(forecast_ensemble(
    train = x,
    horizon = 3,
    parameters = best_param_final, 
    xreg_train = xreg[1:length(x), ],
    xreg_newdata = as.matrix(xreg_newdata)),
    start = c(
      if_else(months_end + 1 == 13, years_end + 1, years_end), 
      if_else(months_end + 1 == 13, 1, months_end + 1)), 
    frequency = 12)
  
}

msis <- function(lower, upper, alpha, future_obs, m = 1, data_ts) {
  
  #' This function calculates the mean scaled interval score, a loss function that
  #' assesses the quality of a prediction interval, given a significance level.
  #'
  #' @param lower The lower bound on the prediction interval, as a ts object
  #' @param upper The upper bound on the prediction interval, as a ts object
  #' @param alpha The significance level of the prediction interval
  #' @param future_obs The observed, true values of the time series. Must be a ts object with the same overlapping time intervals 
  #'  of the lower and upper bounds.
  #' @param m The seasonality of the time series when calculating the naive forecasts for scaling. Default = 1 (no seasonality).
  #'
  #' @return The mean scaled interval score.
  #' @export
  #'
  
  total_width <- sum(upper - lower)
  penalty <- sum((2 / alpha) * ((future_obs < lower) * (lower - future_obs) + (future_obs > upper) * (future_obs - upper)))
  numerator <- total_width + penalty
  
  year_to <- time(future_obs) - 1/12
  train_ts <- window(data_ts, end = year_to[1]) 
  
  naive_vs_real <- ts.intersect(train_ts, stats::lag(data_ts, -m), dframe = TRUE) %>%
    rename_at(., .vars = vars(2), ~"naive") %>%
    mutate(train_ts = as.numeric(train_ts),
           naive = as.numeric(naive))
  
  denominator <- mae(data = naive_vs_real, truth = train_ts, estimate = naive)$.estimate
  
  numerator / (length(lower) * denominator)
  
}

#' This function generates the xreg matrix for the ARIMAX model
#'
#' @param data A tibble containing two columns: DATE, which has the date is 
#' the format yyyy-mm-dd, and EXCAUS, which has the average exchange rate for the given month.
#'
#' @return Data.matrix object of features for the xreg argument in auto.arima
#' @export 
#'
#' @examples
#' xreg_feat_eng(tibble(DATE = c("1991-01-01", "1991-02-01"), EXCAUS = c(1.31, 1.32)))
xreg_feat_eng <- function(data) {

  data %>% 
    mutate(great_recession_ind = if_else(DATE >= ymd("2007-12-01") & DATE <= ymd("2009-06-01"), 1, 0)) %>%
    mutate(dotcom_bubble = if_else(DATE >= ymd("2001-03-01") & DATE <= ymd("2001-11-01"), 1,0)) %>%
    mutate(twothousandten_levelchange = if_else(year(DATE) >= 2008, 1,0)) %>%
    mutate(holidays = if_else(month(DATE) == 11 | month(DATE) == 12, 1,0)) %>%
    mutate(twothousandtwo_levelchange = if_else(year(DATE) >= 2002.2 & year(DATE) <= 2007.5, 1,0)) %>%
    mutate(covid = if_else(DATE >= "2020-03-01", 1, 0)) %>%
    filter(DATE >= ymd("2000-1-01")) %>%
    select(-EXCAUS, -DATE) %>%
    data.matrix(.)

}

#' Calculates prediction intervals using Taylor and Bunn (1999), using an adjusted regression
#' function as explained by Ivan Svetunkov (2017).
#'
#' @param forecast_date Starting date (inclusive) for which to provide three month ahead forecasts
#' @param predictions_combined A tibble containing all of the predictions. Must have
#' columns pred, observed, error, x (the predictors), and starting_dates (the forecast date, inclusive, for which
#' to provide three month ahead forecasts)
#' @param quantile The desired prediction interval level (ex: 0.95)
#'
#' @return A tibble with the desired lower and upper bounds on the forecast error
#' term.
#' @export
#'
#' @examples
#' get_intervals("2019-01-01", my_data, 0.95)
get_intervals <- function(forecast_date, predictions_combined, quantile) {
  
  error_data <- predictions_combined %>%
    filter(starting_dates < forecast_date)
  
  A <- c(1,1)
  quant <- (1 + quantile) / 2
  
  # https://github.com/trnnick/smooth/blob/master/R/ssfunctions.R
  quantfunc <- function(A, quant, data) {
    
    ee <- data$error - (A[1] * data$x^A[2])
    (1 - quant) * sum(abs(ee[which(ee<0)])) + quant*sum(abs(ee[which(ee>=0)]))
    
  }
  
  upper <- nlminb(A, quantfunc, quant = quant, data = error_data)$par
  lower <- nlminb(A, quantfunc, quant = 1 - quant, data = error_data)$par  
  
  upper_residuals <- upper[1] * c(1:3)^upper[2]
  lower_residuals <- lower[1] * c(1:3)^lower[2]
  
  tibble(
    lower = lower_residuals,
    upper = upper_residuals,
    period = c(ymd(forecast_date), ymd(forecast_date) %m+% months(1), ymd(forecast_date) %m+% months(2))
  )
  
}
