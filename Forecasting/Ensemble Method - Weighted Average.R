library(tidyverse)
library(forecast)
library(caret)
library(parallel)
library(lubridate)
library(alfred)
library(parallel)

source("/home/brayden/GitHub/US-to-CAD-Exchange-Rate-Forecasting/Forecasting/allFunctions.R")

#0.02384 RMSE
#Worse than best single method RWD.
#......................Import Data..........................................#

data = get_fred_series("EXCAUS", "EXCAUS") %>%
  filter(!is.na(EXCAUS)) %>%
  rename(DATE = date) 

data.ts = ts(data$EXCAUS, frequency = 12, start = c(year(data$DATE[1]), month(data$DATE[1])), end = c(year(data$DATE[nrow(data)]), month(data$DATE[nrow(data)]))) %>%
  window(., start = c(2000,1))

#..........................................Feature Engineering..............................................................#

data.FE = data %>% 
  mutate(GreatRecession.Ind = ifelse(DATE >= ymd("2007-12-01") & DATE <= ymd("2009-06-01"),1,0)) %>%
  mutate(DotCom_Recession.Ind = ifelse(DATE >= ymd("2001-03-01") & DATE <= ymd("2001-11-01"), 1,0)) %>%
  mutate(TwoThousandTen.LevelChange = ifelse(year(DATE) >= 2008, 1,0)) %>%
  mutate(HOLIDAYS = ifelse(month(DATE) == 11 | month(DATE) == 12, 1,0)) %>%
  mutate(TwoThousandTwo.LevelChange = ifelse(year(DATE) >= 2002.2 & year(DATE) <= 2007.5, 1,0)) %>%
  filter(DATE >= ymd("2000-1-01")) %>%
  select(-EXCAUS, -DATE) %>%
  data.matrix(.) 

#..........................................Fit model.........................................................................#

timeSlices = createTimeSlices(y = data.ts, initialWindow = 210, horizon = 3, fixedWindow = FALSE)

bestParameters = mclapply(X = timeSlices$train, FUN = innerTrain, data = data.ts, iterations = 100, xreg = data.FE, mc.cores = 4)

#bestParameters = lapply(timeSlices$train, FUN = innerTrain, data = data.ts, iterations = 75)

finalResults = mapply(FUN = outerFold, trainFoldsIndex = timeSlices$train, testFoldsIndex = timeSlices$test, parameters = bestParameters, MoreArgs = list(data = data.ts, xreg = data.FE), SIMPLIFY = FALSE)
mean(unlist(finalResults))