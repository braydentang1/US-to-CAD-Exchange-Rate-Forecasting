library(tidyverse)
library(forecast)
library(caret)
library(lubridate)
library(alfred)

#Best single method:
#RWD: 0.02362
#...........................................Import Data.....................................................................#

data = get_fred_series("EXCAUS", "EXCAUS") %>%
  filter(!is.na(EXCAUS)) %>%
  rename(DATE = date) 

data.ts = ts(data$EXCAUS, frequency = 12, start = c(year(data$DATE[1]), month(data$DATE[1])), end = c(year(data$DATE[nrow(data)]), month(data$DATE[nrow(data)]))) %>%
  window(., start = c(2000,1))

#..........................................Feature Engineering..............................................................#

data.FE = data %>% 
  mutate(GreatRecession.Ind = ifelse(DATE >= ymd("2007-12-01") & DATE <= ymd("2009-06-01"),1,0)) %>%
  mutate(DotCom_Recession.Ind = ifelse(DATE >= ymd("2001-03-01") & DATE <= ymd("2001-11-01"), 1,0)) %>%
  mutate(TwoThousandTen.LevelChange = ifelse(year(DATE) >= 2008, 1,0)) %>%
  mutate(HOLIDAYS = ifelse(month(DATE) == 11 | month(DATE) == 12, 1,0)) %>%
  mutate(TwoThousandTwo.LevelChange = ifelse(year(DATE) >= 2002.2 & year(DATE) <= 2007.5, 1,0)) %>%
  filter(DATE >= ymd("2000-1-01")) %>%
  select(-EXCAUS, -DATE) %>%
  data.matrix(.) 
#.......................................Plot the Time Series................................................................................#

autoplot(data.ts, ts.colour = "black", ts.size = 1) + labs(title = "US/CAD Exchange Rate Over Time", subtitle = "Over The Period of January 1971 to April 2019", y = "$1 USD in CAD", x = "Time")
autoplot(diff(data.ts), ts.colour = "black", ts.size = 1) +labs(title = "US/CAD Exchange Rate Over Time; 1st Order Differenced Series", subtitle = "Over The Period of January 1971 to April 2019", y = "$1 USD in CAD; Difference", x = "Time")

#......................Evaluation of Various Methods......................................................................#
#Function that finds monthly RMSE one step ahead in time, with restimation of the model at every step.
#This is leave one out cross validation.
#Exclude bagged ETS by default since it takes a long time to run.
#Exclude is a character vector that will exclude the fitted models in the vector. Example: c("ETS", "Arima") excludes ETS and Arima from the results.

evaluate.Models = function(data, exclude = "Bagged ETS", timeSlices, xreg){
  
  allTrain = timeSlices$train
  allTest = timeSlices$test
  
  error.ETS = vector("numeric", length(allTest))
  error.Arima = vector("numeric", length(allTest))
  error.theta = vector("numeric", length(allTest))
  error.rwd = vector("numeric", length(allTest))
  error.tbats = vector("numeric", length(allTest))
  error.baggedETS = vector("numeric", length(allTest))
  error.STL = vector("numeric", length(allTest))
  
  error.Ensemble = vector("numeric", length(allTest))

  for(i in 1:length(allTest)){
  
    model.Average = vector("list", 5)
    
    train.time = time(data)[allTrain[[i]]]
    test.time = time(data)[allTest[[i]]]
    
    train = window(data, start = train.time[1], end = train.time[length(train.time)])
    test = window(data, start = test.time[1], end = test.time[length(test.time)])

    #ETS
    if(!"ETS" %in% exclude){
      
    ets.model = ets(train, ic = "aicc")
    forecast.ets = forecast(ets.model, h = length(test))
    model.Average[[1]] = forecast.ets$mean
    error.ETS[i] = accuracy(forecast.ets, x = test)[2,2]
    
    }
    
    #Auto.Arima
    if(!"Arima" %in% exclude){
      
    train.time = time(data)[allTrain[[i]]]
    test.time = time(data)[allTest[[i]]]
      
    train = window(data, start = train.time[1], end = train.time[length(train.time)])
    test = window(data, start = test.time[1], end = test.time[length(test.time)])
      
    xreg.train = xreg[1:length(train),]
    xreg.test = xreg[1:length(test),]  
      
    arima.model = auto.arima(train, stepwise = TRUE, ic = "aicc", stationary = TRUE, xreg = xreg.train)
    forecast.arima = forecast(arima.model, h = length(test), xreg = xreg.test)
    error.Arima[i] = accuracy(forecast.arima, x = test)[2,2]
    
    }
    
    #Theta
    if(!"Theta" %in% exclude){
      
    forecast.theta = thetaf(y = train, h = length(test))
    model.Average[[2]] = forecast.theta$mean
    error.theta[i] = accuracy(forecast.theta, x = test)[2,2]
    
    }
    
    #Naive = Random Walk 
    if(!"RWD" %in% exclude){
      
    forecast.rwd = rwf(y = train, h = length(test), drift = TRUE)
    model.Average[[3]] = forecast.rwd$mean
    error.rwd[i] = accuracy(forecast.rwd, x = test)[2,2]
    
    }
    
    #TBATS/BATS
    if(!"TBATS" %in% exclude){
      
      model.tbats = tbats(y = train, num.cores = 4)
      forecast.tbats = forecast(model.tbats, h=length(test))
      model.Average[[4]] = forecast.tbats$mean
      error.tbats[i] = accuracy(forecast.tbats, x = test)[2,2]
      
    }
    
    #Bagged ETS
    if(!"Bagged ETS" %in% exclude){
      
      model.baggedETS = baggedETS(y = train)
      forecast.baggedETS = forecast(model.baggedETS, h=length(test))
      error.baggedETS[i] = accuracy(forecast.baggedETS, x = test)[2,2]
      
    }
    
    #STL
    if(!"STL" %in% exclude){
      
      forecast.STL = stlf(y = train, h = length(test), robust = TRUE)
      model.Average[[5]] = forecast.STL$mean
      error.STL[i] = accuracy(forecast.STL, x = test)[2,2]
      
    }
    
    #Process the ensemble, take a simple average for now
    model.Average.Process = model.Average %>% reduce(ts.intersect) %>% rowMeans(.)
    
    error.Ensemble[i] = accuracy(model.Average.Process, x = test)[2]
    
  }
  
  list(ETS = mean(error.ETS, na.rm = TRUE), Auto.Arima = mean(error.Arima, na.rm = TRUE), 
       Theta = mean(error.theta, na.rm = TRUE), RWD = mean(error.rwd, na.rm = TRUE),
       TBATS = mean(error.tbats, na.rm = TRUE), baggedETS = mean(error.baggedETS, na.rm = TRUE),
       STL = mean(error.STL, na.rm = TRUE), Ensemble = mean(error.Ensemble, na.rm = TRUE))
  
}

#......................Custom Bagging Function for the Theta Method...................................................#
#Not used because it leads to a worse model.
#Probably could have used a premade function already in the forecast package.

evaluate.baggedTheta = function(n, data, timeSlices){
  
  allTrain = timeSlices$train
  allTest = timeSlices$test
  error= vector("numeric", length(test))
  
  for(i in 1:length(allTest)){
    
    train.time = time(data)[allTrain[[i]]]
    test.time = time(data)[allTest[[i]]]
    
    train = window(data, start = train.time[1], end = train.time[length(train.time)])
    test = window(data, start = test.time[1], end = test.time[length(test.time)])
    
    set.seed(200350623)
    bootstrap.train = bld.mbb.bootstrap(x = train, num = n)
    
    predictions = lapply(bootstrap.train, FUN = forecast.BaggedTheta, h = length(test))
    model.Average.Process = predictions %>% reduce(ts.intersect) %>% rowMeans(.)
    error[i] = accuracy(model.Average.Process, x = test)[2]
    
  }
  
  mean(error)
  
}

#..................................Wrapper to make calls to forecasting function easier when all of the bootstrapped time series are in a list..............# 
#Not used because it leads to a worse model.
forecast.BaggedTheta = function(x, h){
  
  forecast.theta = thetaf(y = x, h = h)$mean

}

#.............................Create the cross validation time slices.......................................................................# 
timeSlices = createTimeSlices(y = data.ts, initialWindow = 210, horizon = 3, fixedWindow = FALSE)

#I exclude the ARIMA and Bagged ETS Models because they are significantly worse than the others
results = evaluate.Models(data = data.ts, exclude = c("Bagged ETS", "ETS", "Theta", "STL"), timeSlices = timeSlices, xreg = data.FE)

#Tried using this but model is much worse. Don't use.
#results.BaggedTheta = evaluate.baggedTheta(n = 5000, data = data.ts)