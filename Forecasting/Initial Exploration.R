library(tidyverse)
library(forecast)

#Will rename once we settle on some sort of methodology

#......................Import Data..........................................#

data = read_csv("C:/Users/Brayden/Documents/GitHub/US-to-CAD-Exchange-Rate-Forecasting/Data/EXCAUS.csv")
data.ts = ts(data$EXCAUS, frequency = 12, start = c(1971,1), end = c(2019,4)) 

#......................Plot the Time Series.................................#

autoplot(data.ts, ts.colour = "black", ts.size = 1) + labs(title = "US/CAD Exchange Rate Over Time", subtitle = "Over The Period of January 1971 to April 2019", y = "$1 USD in CAD", x = "Time")
autoplot(diff(data.ts), ts.colour = "black", ts.size = 1) +labs(title = "US/CAD Exchange Rate Over Time; 1st Order Differenced Series", subtitle = "Over The Period of January 1971 to April 2019", y = "$1 USD in CAD; Difference", x = "Time")

#......................Monthly Predictions.............................#
#Function that finds monthly RMSE one step ahead in time, with restimation of the model

evaluate.Models = function(data){
  
  test = window(data, start = 2018 + 4/12)
  error.ETS = vector("numeric", length(test))
  error.Arima = vector("numeric", length(test))

  for(i in 1:length(test)){
  
    train = window(data, end = 2018 + (2+i)/12)
    
    #ETS
    ets.model = ets(train, ic = "aicc")
    forecast.ets = forecast(ets.model, h = 1)$mean
    error.ETS[i] = (forecast.ets - test[i])^2 
    
    
    #Auto.Arima
    arima.model = auto.arima(train, stepwise = TRUE, ic = "aicc", stationary = TRUE)
    forecast.arima = forecast(arima.model, h = 1)$mean
    error.Arima[i] = (forecast.arima - test[i])^2 
  
  }
  
  list(ETS = mean(error.ETS)^0.5, Auto.Arima = mean(error.Arima)^0.5)
  
}

