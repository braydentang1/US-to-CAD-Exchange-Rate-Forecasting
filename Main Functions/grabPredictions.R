grabPredictions <-
function(train, newdata){
  
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
