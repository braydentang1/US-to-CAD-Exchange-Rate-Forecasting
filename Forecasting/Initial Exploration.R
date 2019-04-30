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
#Exclude is a character vector that will exclude the fitted models in the vector. Example: c("ETS", "Arima") excludes ETS and Arima from the results.

evaluate.Models = function(data, exclude = NULL){
  
  test = window(data, start = 2018)
  
  error.ETS = vector("numeric", length(test))
  error.Arima = vector("numeric", length(test))
  error.theta = vector("numeric", length(test))
  error.rwd = vector("numeric", length(test))
  
  model.Ensemble = vector("list", length(test))

  for(i in 1:length(test)){
  
    model.Average = vector("numeric", 3)
    train = window(data, end = 2017 + (10+i)/12)
    
    #ETS
    if(!"ETS" %in% exclude){
      
    ets.model = ets(train, ic = "aicc")
    forecast.ets = forecast(ets.model, h = 1)$mean
    model.Average[1] = forecast.ets
    error.ETS[i] = (forecast.ets - test[i])^2 
    
    }
    
    #Auto.Arima
    if(!"Arima" %in% exclude){
    arima.model = auto.arima(train, stepwise = TRUE, ic = "aicc", stationary = TRUE)
    forecast.arima = forecast(arima.model, h = 1)$mean
    error.Arima[i] = (forecast.arima - test[i])^2 
    }
    
    #Theta
    if(!"Theta" %in% exclude){
    forecast.theta = thetaf(y = train, h = 1)$mean
    model.Average[2] = forecast.theta
    error.theta[i] = (forecast.theta - test[i])^2
    }
    
    #Naive = Random Walk 
    if(!"RWD" %in% exclude){
    forecast.rwd = rwf(y = train, h = 1, drift = TRUE)$mean
    model.Average[3] = forecast.rwd
    error.rwd[i] = (forecast.rwd - test[i])^2
    }
    
    model.Ensemble[[i]] = model.Average
    
  }
  
  #Process the ensembling method; take a simple average.
  
  Ensemble.Process = unlist(lapply(model.Ensemble, mean)) 
  
  list(ETS = mean(error.ETS, na.rm = TRUE)^0.5, Auto.Arima = mean(error.Arima, na.rm = TRUE)^0.5, 
       Theta = mean(error.theta, na.rm = TRUE)^0.5, RWD = mean(error.rwd, na.rm = TRUE)^0.5,
       Ensemble = (mean((Ensemble.Process - test)^2))^0.5)
  
}

results = evaluate.Models(data = data.ts, exclude = c("Arima"))
