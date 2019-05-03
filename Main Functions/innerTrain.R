innerTrain <-
function(trainFoldIndex, data, iterations){
  
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
