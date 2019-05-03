randomGridSearch <-
function(grid, predictions, validation){
  
  score = vector("numeric", nrow(grid))
  
  for (k in 1:nrow(grid)){
    
    predictions.tmp = predictions %>% transmute(Weighted.Average = ETS * as.numeric(grid[k,1]/grid[k,6]) + Theta * as.numeric(grid[k,2]/grid[k,6]) +
                                               RWD * as.numeric(grid[k,3]/grid[k,6]) + TBATS * as.numeric(grid[k,4]/grid[k,6]) + STL * as.numeric(grid[k,5]/grid[k,6]))
    
    score[k] = accuracy(f = predictions.tmp$Weighted.Average, x = validation)[2]
    
  }
  
  score
  
}
