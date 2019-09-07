library(tidyverse)
library(forecast)
library(caret)
library(parallel)
library(lubridate)
library(alfred)

#......................Fit the ensemble model...............................#

innerTrain = function(trainFoldIndex, data, iterations, xreg){
  
  set.seed(200350623)
  randomGrid = tibble(RWD.Weight = sample.int(10000, size = iterations, replace = TRUE), TBATS.Weight = sample.int(10000, size = iterations, replace = TRUE), 
                      ARIMA.Weight = sample.int(10000, size = iterations, replace = TRUE)) %>% 
    mutate(sumofWeights = rowSums(.)) 
  
  outerTrain.time = time(data)[trainFoldIndex]
  outerTrain = window(data, start = outerTrain.time[1], end = outerTrain.time[length(outerTrain.time)])
  
  allInnerSets = createTimeSlices(y = outerTrain, initialWindow = 160, horizon = 3, fixedWindow = FALSE)
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
    
    allPredictions = grabPredictions(train = train, newdata = validation, xreg.train = xreg.train, xreg.newdata = xreg.validation) %>% 
      reduce(ts.intersect) %>% 
      as_tibble(.) %>%
      set_names(c("RWD", "TBATS", "ARIMA"))
    
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
  
  finalPredictions = grabPredictions(train = outerTrain, newdata = test, xreg.train = xreg.train, xreg.newdata = xreg.test) %>% 
    reduce(ts.intersect) %>%
    as_tibble(.) %>%
    set_names(c("RWD", "TBATS", "ARIMA"))
  
  score = randomGridSearch(grid = parameters, predictions = finalPredictions, validation = test)
  
  score
}



grabPredictions = function(train, newdata, xreg.train, xreg.newdata){
  
  model.Average = vector("list", 3)
  
  forecast.rwd = rwf(y = train, h = length(newdata), drift = TRUE)
  model.Average[[1]] = forecast.rwd$mean
  
  model.tbats = tbats(y = train, num.cores = 1)
  forecast.tbats = forecast(model.tbats, h=length(newdata))
  model.Average[[2]] = forecast.tbats$mean
  
  arima.model = auto.arima(y = train, ic = "aicc", stationary = TRUE, xreg = xreg.train)
  forecast.arima = forecast(arima.model, h = length(newdata), xreg = xreg.newdata)
  model.Average[[3]] = forecast.arima$mean
  
  model.Average
}


randomGridSearch = function(grid, predictions, validation){
  
  score = vector("numeric", nrow(grid))
  constant = which(colnames(grid) == "sumofWeights")

  for (k in 1:nrow(grid)){
    
    predictions.tmp = predictions %>% transmute(Weighted.Average = RWD * as.numeric(grid[k,1]/grid[k, constant]) + TBATS * as.numeric(grid[k,2]/grid[k, constant]) 
                                                + ARIMA * as.numeric(grid[k,3]/grid[k, constant]))
    
    score[k] = accuracy(f = predictions.tmp$Weighted.Average, x = validation)[2]
    
  }
  
  score
  
}