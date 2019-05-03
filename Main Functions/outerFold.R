outerFold <-
function(data, trainFoldsIndex, testFoldsIndex, parameters){
  
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
