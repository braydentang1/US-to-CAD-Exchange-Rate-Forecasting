library(tidyverse)
library(forecast)
library(caret)
library(smooth)
library(lubridate)
library(yardstick)
library(alfred)

#.......... .................................Import Data.....................................................................#

data <- get_fred_series("EXCAUS", "EXCAUS") %>%
  filter(!is.na(EXCAUS)) %>%
  rename(DATE = date) 

data_ts <- ts(
  data$EXCAUS,
   frequency = 12,
    start = c(year(data$DATE[1]), month(data$DATE[1])), 
    end = c(year(data$DATE[nrow(data)]), month(data$DATE[nrow(data)]))) %>%
  window(., start = c(2000,1))

#..........................................Feature Engineering..............................................................#

data_FE <- data %>% 
  mutate(great_recession_ind = if_else(DATE >= ymd("2007-12-01") & DATE <= ymd("2009-06-01"),1,0)) %>%
  mutate(dotcom_bubble = if_else(DATE >= ymd("2001-03-01") & DATE <= ymd("2001-11-01"), 1,0)) %>%
  mutate(twothousandten_levelchange = if_else(year(DATE) >= 2008, 1,0)) %>%
  mutate(holidays = if_else(month(DATE) == 11 | month(DATE) == 12, 1,0)) %>%
  mutate(twothousandtwo_levelchange = if_else(year(DATE) >= 2002.2 & year(DATE) <= 2007.5, 1,0)) %>%
  filter(DATE >= ymd("2000-1-01")) %>%
  select(-EXCAUS, -DATE) %>%
  data.matrix(.) 
#.......................................Plot the Time Series................................................................................#

autoplot(
  data_ts, ts.colour = "black", ts.size = 1) + 
  labs(
    title = "US/CAD Exchange Rate Over Time",
    subtitle = "Over The Period of January 1971 to April 2019",
    y = "$1 USD in CAD", 
    x = "Time"
  )

autoplot(
  diff(data_ts), 
  ts.colour = "black", ts.size = 1) +
  labs(
    title = "US/CAD Exchange Rate Over Time; 1st Order Differenced Series", 
    subtitle = "Over The Period of January 1971 to April 2019",
     y = "$1 USD in CAD; Difference",
      x = "Time"
)

#......................Evaluation of Various Methods......................................................................#
#Function that finds monthly RMSE one step ahead in time, with restimation of the model at every step.
#This is leave one out cross validation.
#Exclude bagged ETS by default since it takes a long time to run.
#Exclude is a character vector that will exclude the fitted models in the vector. Example: c("ETS", "Arima") excludes ETS and Arima from the results.

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
    
  naive_forecasts <- ts.intersect(train, stats::lag(train, -1), dframe = TRUE) %>%
    rename_at(., .vars = vars(2), ~"naive") %>%
    mutate(naive = as.numeric(naive),
           train = as.numeric(train))
  
  mae_train <- mae(naive_forecasts,
                   truth = train, estimate = naive)$.estimate

    #ETS
    if (!"ETS" %in% exclude) {
      
    ets_model <- ets(train, ic = "aicc")
    forecast_ets <- forecast(ets_model, h = length(test))
    model_average[[1]] <- forecast_ets$mean
    error_ets[i] <- yardstick::mase_vec(
      truth = as.numeric(test), 
      estimate = as.numeric(forecast_ets$mean),
      mae_train = mae_train)
    
    }
    
    #Auto.Arima
    if (!"ARIMA" %in% exclude) {
      
    xreg_train <- xreg[1:length(train), ]
    xreg_test <- xreg[1:length(test), ]  
      
    arima_model <- auto.arima(
      train,
      stepwise = TRUE, 
      ic = "aicc", 
      stationary = TRUE, 
      xreg = xreg_train
  )

    forecast_arima <- forecast(arima_model, h = length(test), xreg = xreg_test)
    error_arima[i] <- yardstick::mase_vec(
      truth = as.numeric(test), 
      estimate = as.numeric(forecast_arima$mean),
      mae_train = mae_train)
    
    }
    
    #Theta
    if (!"Theta" %in% exclude) {
      
    forecast_theta <- thetaf(y = train, h = length(test))
    model_average[[2]] <- forecast_theta$mean
    error_theta[i] <- yardstick::mase_vec(
      truth = as.numeric(test), 
      estimate = as.numeric(forecast_theta$mean),
      mae_train = mae_train)
    
    }
    
    #Naive = Random Walk 
    if (!"RWD" %in% exclude) {
      
    forecast_rwd <- rwf(y = train, h = length(test), drift = TRUE)
    model_average[[3]] <- forecast_rwd$mean
    error_rwd[i] <- yardstick::mase_vec(
      truth = as.numeric(test), 
      estimate = as.numeric(forecast_rwd$mean),
      mae_train = mae_train)
    
    }
    
    #TBATS/BATS
    if (!"TBATS" %in% exclude) {
      
      model_tbats <- tbats(y = train, num.cores = 2)
      forecast_tbats <- forecast(model_tbats, h = length(test))
      model_average[[4]] <- forecast_tbats$mean
      error_tbats[i] <- yardstick::mase_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_tbats$mean),
        mae_train = mae_train)
      
    }
    
    #Bagged ETS
    if (!"Bagged ETS" %in% exclude) {
      
      model_baggedETS <- baggedETS(y = train)
      forecast_baggedETS <- forecast(model_baggedETS, h = length(test))
      error_baggedETS[i] <- yardstick::mase_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_baggedETS$mean),
        mae_train = mae_train)
      
    }
    
    #STL
    if (!"STL" %in% exclude) {
      
      forecast_stl <- stlf(y = train, h = length(test), robust = TRUE)
      model_average[[5]] <- forecast_stl$mean
      error_stl[i] <- yardstick::mase_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_stl$mean),
        mae_train = mae_train)
      
    }
    
    #CES
    if (!"CES" %in% exclude) {
      
      ces_combined <- ts.union(train, test, dframe = TRUE) %>%
        gather(., na.rm = TRUE)
      
      forecast_ces <- auto.ces(y = ts(ces_combined$value, start = train_time[1], frequency = 12), h = length(test), holdout = TRUE)
      model_average[[6]] <- forecast_ces$forecast
      error_ces[i] <- yardstick::mase_vec(
        truth = as.numeric(test), 
        estimate = as.numeric(forecast_ces$forecast),
        mae_train = mae_train)
      
    }
    
    #Process the ensemble, take a simple average for now
    model_average_process <- model_average %>%
     reduce(ts.intersect) %>%
      rowMeans(.)
    
    error_ensemble[i] <- yardstick::mase_vec(
      truth = as.numeric(test), 
      estimate = as.numeric(model_average_process),
      mae_train = mae_train)
    
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

#.............................Create the cross validation time slices.......................................................................# 
time_slices <- createTimeSlices(
  y = data_ts, 
  initialWindow = 200, 
  horizon = 3, 
  fixedWindow = FALSE
  )

#I exclude the ARIMA and Bagged ETS Models because they are significantly worse than the others
results <- evaluate_models(
  data = data_ts, 
  exclude = c("Bagged ETS", "ETS", "STL"),
  time_slices = time_slices, 
  xreg = data_FE)

#Tried using this but model is much worse. Don't use.
#results_bagged_theta <- evaluate_bagged_theta(n = 5000, data = data_ts)