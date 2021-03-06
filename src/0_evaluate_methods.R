"This script evaluates a bunch of different time series forecasting methodologies
using walk forward cross validation.

Usage: 0_evaluate_methods.R --exclude_vector=<exclude_vector> --results_out=<results_out>

Options: 
--exclude_vector=<exclude_vector> A quoted string detailing what models to EXCLUDE, separated by `,`. Possible options include Bagged ETS, Arima, ETS, STL, CES, TBATS, RWD, and Theta
--results_out=<results_out> A file path that specifies where to store the list of results.
" -> doc

library(tidyverse)
library(forecast)
library(caret)
library(smooth)
library(lubridate)
library(yardstick)
library(alfred)
library(docopt)

opt <- docopt(doc)

source("src/all_functions.R")

#......................Evaluation of Various Methods......................................................................#
# Function that finds monthly RMSE one step ahead in time, with restimation of the model at every step.
# This is leave one out cross validation.
# Exclude bagged ETS by default since it takes a long time to run.
# Exclude is a character vector that will exclude the fitted models in the vector. Example: c("ETS", "Arima") excludes ETS and Arima from the results.

evaluate_models <- function(data, exclude = "Bagged ETS", time_slices, xreg) {
  
  #' Evaluates a bunch of time series forecasting methods based off RMSE for a given time series.
  #'
  #'@param data The training time series as a ts object
  #'@param exclude A character vector that contains any models to exclude from the 
  #'  evaluation. Possible elements include: "Bagged ETS", "Arima", "ETS", "STL", "CES", "TBATS",
  #'  "RWD", and "Theta". Default: "Bagged ETS".
  #'@param time_slices A list of train and test indices for each split of walk forward cross
  #'  validation.
  #'@param xreg A matrix that contains the exogenous variables for the ARIMAX/CES models. Must
  #'  be the same length of the time series given to the argument data. Must be a matrix.
  #'  
  #'@return A list of the RMSE for each of the models not given in "exclude: 
  #'
  #'@examples 
  #'evaluate_models(
  #' data = my_time_series,
  #' exclude = c("Bagged ETS", "ARIMA"),
  #' time_slices = caret::createTimeSlices(y = my_time_series, horizon = 3, fixedWindow = FALSE),
  #' xreg = my_exog_var
  #' )
  #'
  
  all_train <- time_slices$train
  all_test <- time_slices$test
  
  error_ets <- vector("numeric", length(all_test))
  error_arima <- vector("numeric", length(all_test))
  error_theta <- vector("numeric", length(all_test))
  error_rwd <- vector("numeric", length(all_test))
  error_tbats <- vector("numeric", length(all_test))
  error_baggedETS <- vector("numeric", length(all_test))
  error_stl <- vector("numeric", length(all_test))
  error_ces <- vector("numeric", length(all_test))
  error_ensemble <- vector("numeric", length(all_test))
  
  for (i in 1:length(all_test)) {
    
    model_average <- vector("list", 5)
    
    train_time <- time(data)[all_train[[i]]]
    test_time <- time(data)[all_test[[i]]]
    
    train <- window(
      data, 
      start = train_time[1], 
      end = train_time[length(train_time)]
    )
    
    test <- window(
      data,
      start = test_time[1], 
      end = test_time[length(test_time)]
    )
    
    # naive_forecasts <- ts.intersect(train, stats::lag(train, -1), dframe = TRUE) %>%
    #   rename_at(., .vars = vars(2), ~"naive") %>%
    #   mutate(naive = as.numeric(naive),
    #          train = as.numeric(train))
    # 
    # mae_train <- mae(naive_forecasts,
    #                  truth = train, estimate = naive).$estimate
    
    #ETS
    if (!"ETS" %in% exclude) {
      
      ets_model <- ets(train, ic = "aicc")
      forecast_ets <- forecast(ets_model, h = length(test))
      model_average[[1]] <- forecast_ets$mean
      error_ets[i] <- yardstick::mae_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_ets$mean))
      
    }
    
    #Auto.Arima
    if (!"ARIMA" %in% exclude) {
      
      xreg_train <- xreg[1:length(train), ]
      xreg_test <- xreg[1:length(test), ]  
      
      if (all(xreg_train[, c("covid")] == 0)) {
        
        xreg_train <- xreg_train[, !colnames(xreg_train) %in% c("covid")]
        xreg_test <- xreg_test[, !colnames(xreg_test) %in% c("covid")]
        
      } 
      
      arima_model <- auto.arima(
        train,
        stepwise = TRUE, 
        ic = "aicc", 
        stationary = TRUE, 
        xreg = xreg_train
      )
      
      forecast_arima <- forecast(arima_model, h = length(test), xreg = xreg_test)
      error_arima[i] <- yardstick::mae_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_arima$mean))
      
    }
    
    #Theta
    if (!"Theta" %in% exclude) {
      
      forecast_theta <- thetaf(y = train, h = length(test))
      model_average[[2]] <- forecast_theta$mean
      error_theta[i] <- yardstick::mae_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_theta$mean))
      
    }
    
    #Naive = Random Walk 
    if (!"RWD" %in% exclude) {
      
      forecast_rwd <- rwf(y = train, h = length(test), drift = TRUE)
      model_average[[3]] <- forecast_rwd$mean
      error_rwd[i] <- yardstick::mae_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_rwd$mean))
      
    }
    
    #TBATS/BATS
    if (!"TBATS" %in% exclude) {
      
      model_tbats <- tbats(y = train, num.cores = 2)
      forecast_tbats <- forecast(model_tbats, h = length(test))
      model_average[[4]] <- forecast_tbats$mean
      error_tbats[i] <- yardstick::mae_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_tbats$mean))
      
    }
    
    #Bagged ETS
    if (!"Bagged ETS" %in% exclude) {
      
      model_baggedETS <- baggedETS(y = train)
      forecast_baggedETS <- forecast(model_baggedETS, h = length(test))
      error_baggedETS[i] <- yardstick::mae_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_baggedETS$mean))
      
    }
    
    #STL
    if (!"STL" %in% exclude) {
      
      forecast_stl <- stlf(y = train, h = length(test), robust = TRUE)
      model_average[[5]] <- forecast_stl$mean
      error_stl[i] <- yardstick::mae_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_stl$mean))
      
    }
    
    #CES
    if (!"CES" %in% exclude) {
      
      ces_combined <- ts.union(train, test, dframe = TRUE) %>%
        gather(., na.rm = TRUE)
      
      forecast_ces <- auto.ces(y = ts(ces_combined$value, start = train_time[1], frequency = 12), h = length(test), holdout = TRUE)
      model_average[[6]] <- forecast_ces$forecast
      error_ces[i] <- yardstick::mae_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_ces$forecast))
      
    }
    
    #Process the ensemble, take a simple average for now
    model_average_process <- model_average %>%
      reduce(ts.intersect) %>%
      rowMeans(.)
    
    error_ensemble[i] <- yardstick::mae_vec(
      truth = as.numeric(test), 
      estimate = as.numeric(model_average_process))
    
  }
  
  list(
    ets = mean(error_ets, na.rm = TRUE), 
    auto_arima = mean(error_arima, na.rm = TRUE), 
    theta = mean(error_theta, na.rm = TRUE),
    rwd = mean(error_rwd, na.rm = TRUE),
    tbats = mean(error_tbats, na.rm = TRUE), 
    baggedETS = mean(error_baggedETS, na.rm = TRUE),
    stl = mean(error_stl, na.rm = TRUE), 
    ces = mean(error_ces, na.rm = TRUE),
    ensemble = mean(error_ensemble, na.rm = TRUE)
  )
  
}

#' "This script evaluates a bunch of different time series forecasting methodologies
#' using walk forward cross validation.
#'
#' @param exclude_vector A vector of models to NOT fit. Can contain elements Bagged ETS, Arima, ETS, STL, CES, TBATS, RWD, and Theta.
#' By default, excludes Bagged ETS.
#' @param results_out A directory specifying where to store a RDS object of results.
#'
#' @return None.
#' @export
#'
#' @examples
#' main(exclude_vector = "Bagged ETS, CES", results_out = "results")
main <- function(exclude_vector = "Bagged ETS", results_out) {

  # Parse the string into an R character vector
  exclude <- unlist(strsplit(exclude_vector[1], ","))
  #.......... .................................Import Data.....................................................................#
  data <- get_fred_series("EXCAUS", "EXCAUS") %>%
    filter(!is.na(EXCAUS)) %>%
    rename(DATE = date) 
  
  saveRDS(data, file = paste0(results_out, "/excaus.rds"))
  
  data_ts <- ts(
    data$EXCAUS,
     frequency = 12,
      start = c(year(data$DATE[1]), month(data$DATE[1])), 
      end = c(year(data$DATE[nrow(data)]), month(data$DATE[nrow(data)]))) %>%
    window(., start = c(2000, 1))
  
  # Get exogenous regressors
  data_FE <- xreg_feat_eng(data)
  
  #.............................Create the cross validation time slices.......................................................................# 
  time_slices <- createTimeSlices(
    y = data_ts, 
    initialWindow = length(data_ts) - 12, 
    horizon = 3, 
    fixedWindow = FALSE
    )
  
  #I exclude the ARIMA and Bagged ETS Models because they are significantly worse than the others
  results <- evaluate_models(
    data = data_ts, 
    exclude = exclude,
    time_slices = time_slices, 
    xreg = data_FE)
  
  if (!dir.exists(results_out))  {
    dir.create(results_out)
  }
  
  write_csv(
    tibble(model = names(unlist(results)), scores = unlist(results)),
    path = paste0(results_out, "/individual_validation.csv"))

}

main(
  results_out = opt$results_out,
  exclude_vector = opt$exclude_vector
)