library(tidyverse)
library(forecast)
library(caret)
library(lubridate)

#Best single method:
#RWD: 0.02133881 
#...........................................Import Data.....................................................................#

data = read_csv("C:/Users/Brayden/Documents/GitHub/US-to-CAD-Exchange-Rate-Forecasting/Data/EXCAUS.csv") %>% 
  mutate(DATE = as.Date(DATE)) %>%
  .[349:nrow(.), ]

data.ts = ts(data$EXCAUS, frequency = 12, start = c(2000,1), end = c(2019,4))

#..........................................Feature Engineering..............................................................#

data.FE = data %>% 
  mutate(GreatRecession.Ind = ifelse(DATE >= ymd("2007-12-01") & DATE <= ymd("2009-06-01"),1,0)) %>%
  mutate(DotCom_Recession.Ind = ifelse(DATE >= ymd("2001-03-01") & DATE <= ymd("2001-11-01"), 1,0)) %>%
  mutate(TwoThousandTen.LevelChange = ifelse(year(DATE) >= 2008, 1,0)) %>%
  mutate(HOLIDAYS = ifelse(month(DATE) == 11 | month(DATE) == 12, 1,0)) %>%
  mutate(TwoThousandTwo.LevelChange = ifelse(year(DATE) >= 2002.2 & year(DATE) <= 2007.5, 1,0)) %>%
  select(-DATE, -EXCAUS) %>%
  data.matrix(.)
#.......................................Plot the Time Series................................................................................#

autoplot(data.ts, ts.colour = "black", ts.size = 1) + labs(title = "US/CAD Exchange Rate Over Time", subtitle = "Over The Period of January 2000 to April 2019", y = "$1 USD in CAD", x = "Time")
autoplot(diff(data.ts), ts.colour = "black", ts.size = 1) +labs(title = "US/CAD Exchange Rate Over Time; 1st Order Differenced Series", subtitle = "Over The Period of January 1971 to April 2019", y = "$1 USD in CAD; Difference", x = "Time")

#......................Evaluation of Various Methods......................................................................#
#Function that finds monthly RMSE one step ahead in time, with restimation of the model at every step.
#This is leave one out cross validation.
#Exclude bagged ETS by default since it takes a long time to run.
#Exclude is a character vector that will exclude the fitted models in the vector. Example: c("ETS", "Arima") excludes ETS and Arima from the results.

evaluate.Arima = function(data, timeSlices, xreg){
  
  allTrain = timeSlices$train
  allTest = timeSlices$test
  
  error.Arima = vector("numeric", length(allTest))
  error.RWD = vector("numeric", length(allTest))

  for(i in 1:length(allTest)){
  
    model.Average = vector("list", 5)
    
    train.time = time(data)[allTrain[[i]]]
    test.time = time(data)[allTest[[i]]]
  
    train = window(data, start = train.time[1], end = train.time[length(train.time)])
    test = window(data, start = test.time[1], end = test.time[length(test.time)])
    
    xreg.train = xreg[1:length(train),]
    xreg.test = xreg[1:length(test),]

    arima.model = auto.arima(train, stepwise = TRUE, ic = "aicc", stationary = TRUE, xreg = xreg.train)
    forecast.rwd = rwf(y = train, h = length(test), drift = TRUE)
    
    checkresiduals(arima.model)
    forecast.arima = forecast(arima.model, h = length(test), xreg = xreg.test)
    
    error.Arima[i] = accuracy(forecast.arima, x = test)[2,2]
    error.RWD[i] = accuracy(forecast.rwd, x = test)[2,2]
 
  }
  
  list(Auto.Arima = mean(error.Arima, na.rm = TRUE),
       RWD = mean(error.RWD, na.rm = TRUE))
  
}

#.............................Create the cross validation time slices.......................................................................# 
timeSlices = createTimeSlices(y = data.ts, initialWindow = 202, horizon = 3, fixedWindow = FALSE)

#I exclude the ARIMA and Bagged ETS Models because they are significantly worse than the others
results = evaluate.Arima(data = data.ts, timeSlices = timeSlices, xreg = data.FE)

#Tried using this but model is much worse. Don't use.
#results.BaggedTheta = evaluate.baggedTheta(n = 5000, data = data.ts)