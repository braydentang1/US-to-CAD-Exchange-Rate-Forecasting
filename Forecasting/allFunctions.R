library(tidyverse)
library(forecast)
library(caret)
library(parallel)
library(smooth)
library(lubridate)
library(alfred)

#......................Fit the ensemble model...............................#

innerTrain = function(trainFoldIndex, data, iterations, xreg){
  
  set.seed(200350623)
  randomGrid = tibble(RWD.Weight = sample.int(10000, size = iterations, replace = TRUE), 
                      TBATS.Weight = sample.int(10000, size = iterations, replace = TRUE), 
                      ARIMA.Weight = sample.int(10000, size = iterations, replace = TRUE),
                      CES.Weight = sample.int(10000, size = iterations, replace = TRUE)) %>% 
    mutate(sumofWeights = rowSums(.)) 
  
  outerTrain.time = time(data)[trainFoldIndex]
  outerTrain = window(data, start = outerTrain.time[1], end = outerTrain.time[length(outerTrain.time)])
  
  allInnerSets = createTimeSlices(y = outerTrain, initialWindow = 150, horizon = 3, fixedWindow = FALSE)
  allInnerTrain = allInnerSets$train
  allValidation = allInnerSets$test
  
  scores = vector("list", length(allInnerTrain))
  
  for(k in 1:length(allInnerTrain)){
    
    train.time = time(data)[allInnerTrain[[k]]]
    train = window(data, start = train.time[1], end = train.time[length(train.time)])
    xreg.train = xreg[1:length(train.time), ]
    
    validation.time = time(data)[allValidation[[k]]]
    validation = window(data, start = validation.time[1], end = validation.time[length(validation.time)])
    xreg.validation = xreg[1:length(validation.time), ]
    
    allPredictions = grabPredictions(train = train, horizon = 3, xreg.train = xreg.train, xreg.newdata = xreg.validation) %>% 
      reduce(ts.intersect) %>% 
      as_tibble(.) %>%
      set_names(c("RWD", "TBATS", "ARIMA", "CES"))
    
    scores[[k]] = randomGridSearch(grid = randomGrid, predictions = allPredictions, validation = validation)
    
  }
  
  finalGrid = scores %>% reduce(cbind) %>% rowMeans(.) %>% bind_cols(randomGrid, Score = .)
  finalParameters = finalGrid[which.min(finalGrid$Score), ]
  
  finalParameters
}

outerFold = function(data, trainFoldsIndex, testFoldsIndex, parameters, xreg){
  
  outerTrain.time = time(data)[trainFoldsIndex]
  outerTrain = window(data, start = outerTrain.time[1], end = outerTrain.time[length(outerTrain.time)])
  xreg.train = xreg[1:length(outerTrain.time), ]
  
  test.time = time(data)[testFoldsIndex]
  test = window(data, start = test.time[1], end = test.time[length(test.time)])
  xreg.test = xreg[1:length(test.time), ]
  
  finalPredictions = grabPredictions(train = outerTrain, horizon = 3, xreg.train = xreg.train, xreg.newdata = xreg.test) %>% 
    reduce(ts.intersect) %>%
    as_tibble(.) %>%
    set_names(c("RWD", "TBATS", "ARIMA", "CES"))
  
  score = randomGridSearch(grid = parameters, predictions = finalPredictions, validation = test)
  
  score
}



grabPredictions = function(train, horizon, xreg.train, xreg.newdata){
  
  model.Average = vector("list", 4)
  
  forecast.rwd = rwf(y = train, h = horizon, drift = TRUE)
  model.Average[[1]] = forecast.rwd$mean
  
  model.tbats = tbats(y = train, num.cores = 1)
  forecast.tbats = forecast(model.tbats, h = horizon)
  model.Average[[2]] = forecast.tbats$mean
  
  arima.model = auto.arima(y = train, ic = "aicc", stationary = TRUE, xreg = xreg.train)
  forecast.arima = forecast(arima.model, h = horizon, xreg = xreg.newdata)
  model.Average[[3]] = forecast.arima$mean
  
  forecast.CES = auto.ces(y = train, h = horizon)
  model.Average[[4]] = forecast.CES$forecast
  
  model.Average
}


randomGridSearch = function(grid, predictions, validation){
  
  score = vector("numeric", nrow(grid))
  constant = which(colnames(grid) == "sumofWeights")

  for (k in 1:nrow(grid)){
    
    predictions.tmp = predictions %>% transmute(Weighted.Average = RWD * as.numeric(grid[k,1]/grid[k, constant]) +
                                                  TBATS * as.numeric(grid[k,2]/grid[k, constant]) +
                                                  ARIMA * as.numeric(grid[k,3]/grid[k, constant]) +
                                                  CES * as.numeric(grid[k,4]/grid[k, constant]))
    
    score[k] = accuracy(f = predictions.tmp$Weighted.Average, x = validation)[2]
    
  }
  
  score
  
}

forecast.ensemble = function(train, horizon, parameters, xreg.train, xreg.newdata){
  
  predictions = grabPredictions(train = train, horizon = 3, xreg.train = xreg.train, xreg.newdata = xreg.newdata) %>%
    reduce(., ts.intersect) %>%
    as_tibble(.) %>%
    set_names(c("RWD", "TBATS", "ARIMA", "CES"))
  
  constant = which(colnames(parameters) == "sumofWeights")
  predictions.tmp = predictions %>% transmute(Weighted.Average = RWD * as.numeric(parameters[1, 1]/parameters[1, constant]) +
                                                TBATS * as.numeric(parameters[1, 2]/parameters[1, constant]) +
                                                ARIMA * as.numeric(parameters[1, 3]/parameters[1, constant]) +
                                                CES * as.numeric(parameters[1, 4]/parameters[1, constant]))
  

}

