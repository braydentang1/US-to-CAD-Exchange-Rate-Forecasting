library(tidyverse)
library(forecast)
library(caret)
library(smooth)
library(lubridate)
library(alfred)

#Best single method:
#RWD: 0.02575
#...........................................Import Data.....................................................................#

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

    #ETS
    if (!"ETS" %in% exclude) {
      
    ets_model <- ets(train, ic = "aicc")
    forecast_ets <- forecast(ets_model, h = length(test))
    model_average[[1]] <- forecast_ets$mean
    error_ets[i] <- accuracy(forecast_ets, x = test)[2,2]
    
    }
    
    #Auto.Arima
    if (!"Arima" %in% exclude) {
      
    train_time <- time(data)[all_train[[i]]]
    test_time <- time(data)[all_test[[i]]]
      
    train <- window(data, start = train_time[1], end = train_time[length(train_time)])
    test <- window(data, start = test_time[1], end = test_time[length(test_time)])
      
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
    error_arima[i] <- accuracy(forecast_arima, x = test)[2,2]
    
    }
    
    #Theta
    if (!"Theta" %in% exclude) {
      
    forecast_theta <- thetaf(y = train, h = length(test))
    model_average[[2]] <- forecast_theta$mean
    error_theta[i] <- accuracy(forecast_theta, x = test)[2,2]
    
    }
    
    #Naive = Random Walk 
    if (!"RWD" %in% exclude) {
      
    forecast_rwd <- rwf(y = train, h = length(test), drift = TRUE)
    model_average[[3]] <- forecast_rwd$mean
    error_rwd[i] <- accuracy(forecast_rwd, x = test)[2,2]
    
    }
    
    #TBATS/BATS
    if (!"TBATS" %in% exclude) {
      
      model_tbats <- tbats(y = train, num.cores = 4)
      forecast_tbats <- forecast(model_tbats, h = length(test))
      model_average[[4]] <- forecast_tbats$mean
      error_tbats[i] <- accuracy(forecast_tbats, x = test)[2,2]
      
    }
    
    #Bagged ETS
    if (!"Bagged ETS" %in% exclude) {
      
      model_baggedETS <- baggedETS(y = train)
      forecast_baggedETS <- forecast(model_baggedETS, h = length(test))
      error_baggedETS[i] <- accuracy(forecast_baggedETS, x = test)[2,2]
      
    }
    
    #STL
    if (!"STL" %in% exclude) {
      
      forecast_stl <- stlf(y = train, h = length(test), robust = TRUE)
      model_average[[5]] <- forecast_stl$mean
      error_stl[i] <- accuracy(forecast_stl, x = test)[2,2]
      
    }
    
    #CES
    if (!"CES" %in% exclude) {
      
      forecast_ces <- auto.ces(y = train, h = length(test))
      model_average[[6]] <- forecast_ces$forecast
      error_ces[i] <- accuracy(x = test, f = forecast_ces$forecast)[2]
      
    }
    
    #Process the ensemble, take a simple average for now
    model_average_process <- model_average %>%
     reduce(ts.intersect) %>%
      rowMeans(.)
    
    error_ensemble[i] <- accuracy(model_average_process, x = test)[2]
    
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

#......................Custom Bagging Function for the Theta Method...................................................#
# Not used because it leads to a worse model.
# Probably could have used a premade function already in the forecast package.

evaluate_bagged_theta <- function(n, data, time_slices) {
  
  all_train <- time_slices$train
  all_test <- time_slices$test
  error <- vector("numeric", length(test))
  
  for (i in 1:length(all_test)) {
    
    train_time <- time(data)[all_train[[i]]]
    test_time <- time(data)[all_test[[i]]]
    
    train <- window(data, start = train_time[1], end = train_time[length(train_time)])
    test <- window(data, start = test_time[1], end = test_time[length(test_time)])
    
    set.seed(200350623)
    bootstrap_train <- bld.mbb.bootstrap(x = train, num = n)
    
    predictions <- map_dfr(
      bootstrap_train,
      forecast_bagged_theta,
      h = length(test)
    )

    model_average_process <- predictions %>% 
    reduce(ts.intersect) %>% 
    rowMeans(.)

    error[i] <- accuracy(model_average_process, x = test)[2]
    
  }
  
  mean(error)
  
}

#..................................Wrapper to make calls to forecasting function easier when all of the bootstrapped time series are in a list..............# 
#Not used because it leads to a worse model.
forecast_bagged_theta <- function(x, h) {
  
  forecast_theta <- thetaf(y = x, h = h)$mean

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
  exclude = c("Bagged ETS", "ETS", "Theta", "STL"),
  time_slices = time_slices, 
  xreg = data_FE)

#Tried using this but model is much worse. Don't use.
#results_bagged_theta <- evaluate_bagged_theta(n = 5000, data = data_ts)