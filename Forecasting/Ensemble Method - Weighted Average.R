library(tidyverse)
library(forecast)
library(caret)

#......................Import Data..........................................#

data = read_csv("C:/Users/Brayden/Documents/GitHub/US-to-CAD-Exchange-Rate-Forecasting/Data/EXCAUS.csv")
data.ts = ts(data$EXCAUS, frequency = 12, start = c(1971,1), end = c(2019,4)) 

#......................Fit the ensemble model...............................#

innerTrain = function(trainFoldIndex, data, iterations){
  
  set.seed(200350623)
  randomGrid = tibble(ETS.Weight = sample.int(10000, size = iterations, replace = TRUE), Theta.Weight = sample.int(10000, size = iterations, replace = TRUE),
                      RWD.Weight = sample.int(10000, size = iterations, replace = TRUE), TBATS.Weight = sample.int(10000, size = iterations, replace = TRUE),
                      STL.Weight = sample.int(10000, size = iterations, replace = TRUE)) %>% 
    mutate(sumofWeights = rowSums(.)) 
  
  outerTrain.time = time(data)[trainFoldIndex]
  outerTrain = window(data, start = outerTrain.time[1], end = outerTrain.time[length(outerTrain.time)])
  
  allInnerSets = createTimeSlices(y = outerTrain, initialWindow = 558, horizon = 3, fixedWindow = FALSE)
  allInnerTrain = allInnerSets$train
  allValidation = allInnerSets$test
  
  scores = vector("list", length(allInnerTrain))
  
  for(k in 1:length(allInnerTrain)){
    
    train.time = time(data)[allInnerTrain[[k]]]
    train = window(data, start = train.time[1], end = train.time[length(train.time)])
    
    validation.time = time(data)[allValidation[[k]]]
    validation = window(data, start = validation.time[1], end = validation.time[length(validation.time)])
    
    allPredictions = grabPredictions(train = train, newdata = validation) %>% 
      reduce(ts.intersect) %>% 
      as_tibble(.) %>%
      set_names(c("ETS", "Theta", "RWD", "TBATS", "STL"))
    
    scores[[k]] = randomGridSearch(grid = randomGrid, predictions = allPredictions, validation = validation)
    
  }
  
  finalGrid = scores %>% reduce(cbind) %>% rowMeans(.) %>% bind_cols(randomGrid, Score = .)
  finalParameters = finalGrid[which.min(finalGrid$Score), ]
  
  finalParameters
}

outerFold = function(data, trainFoldsIndex, testFoldsIndex, parameters){
  
  outerTrain.time = time(data)[trainFoldsIndex]
  outerTrain = window(data, start = outerTrain.time[1], end = outerTrain.time[length(outerTrain.time)])
  
  test.time = time(data)[testFoldsIndex]
  test = window(data, start = test.time[1], end = test.time[length(test.time)])
  
  finalPredictions = grabPredictions(train = outerTrain, newdata = test) %>% 
    reduce(ts.intersect) %>%
    as_tibble(.) %>%
    set_names(c("ETS", "Theta", "RWD", "TBATS", "STL"))
  
  score = randomGridSearch(grid = parameters, predictions = finalPredictions, validation = test)
  
  score
}



grabPredictions = function(train, newdata){
  
  model.Average = vector("list", 5)
  
    ets.model = ets(train, ic = "aicc")
    forecast.ets = forecast(ets.model, h = length(newdata))
    model.Average[[1]] = forecast.ets$mean

    forecast.theta = thetaf(y = train, h = length(newdata))
    model.Average[[2]] = forecast.theta$mean
    
    forecast.rwd = rwf(y = train, h = length(newdata), drift = TRUE)
    model.Average[[3]] = forecast.rwd$mean
    
    model.tbats = tbats(y = train, num.cores = 4)
    forecast.tbats = forecast(model.tbats, h=length(newdata))
    model.Average[[4]] = forecast.tbats$mean
    
    forecast.STL = stlf(y = train, h = length(newdata), robust = TRUE)
    model.Average[[5]] = forecast.STL$mean
    
    model.Average
}


randomGridSearch = function(grid, predictions, validation){
  
  score = vector("numeric", nrow(grid))
  
  for (k in 1:nrow(grid)){
    
    predictions.tmp = predictions %>% transmute(Weighted.Average = ETS * as.numeric(grid[k,1]/grid[k,6]) + Theta * as.numeric(grid[k,2]/grid[k,6]) +
                                               RWD * as.numeric(grid[k,3]/grid[k,6]) + TBATS * as.numeric(grid[k,4]/grid[k,6]) + STL * as.numeric(grid[k,5]/grid[k,6]))
    
    score[k] = accuracy(f = predictions.tmp$Weighted.Average, x = validation)[2]
    
  }
  
  score
  
}

timeSlices = createTimeSlices(y = data.ts, initialWindow = 565, horizon = 3, fixedWindow = FALSE)
bestParameters = lapply(timeSlices$train, FUN = innerTrain, data = data.ts, iterations = 3)
finalResults = mapply(FUN = outerFold, trainFoldsIndex = timeSlices$train, testFoldsIndex = timeSlices$test, parameters = bestParameters, MoreArgs = list(data = data.ts), SIMPLIFY = FALSE)
mean(unlist(finalResults))