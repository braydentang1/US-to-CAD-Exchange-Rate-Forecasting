library(tidyverse)
library(forecast)
library(caret)
library(parallel)
library(lubridate)
library(smooth)
library(alfred)
library(parallel)

source("src/all_functions.R")

#0.02558 RMSE
#Slightly better than the RWD
#......................Import Data..........................................#

data <- get_fred_series("EXCAUS", "EXCAUS") %>%
  filter(!is.na(EXCAUS)) %>%
  rename(DATE = date) 

data_ts <- ts(
  data$EXCAUS,
   frequency = 12, 
   start = c(year(data$DATE[1]), month(data$DATE[1])),
    end = c(year(data$DATE[nrow(data)]), month(data$DATE[nrow(data)]))) %>%
  window(., start = c(2000, 1)
)

#..........................................Feature Engineering..............................................................#

data_FE <- data %>% 
  mutate(great_recession_ind = if_else(DATE >= ymd("2007-12-01") & DATE <= ymd("2009-06-01"), 1, 0)) %>%
  mutate(dotcom_bubble = if_else(DATE >= ymd("2001-03-01") & DATE <= ymd("2001-11-01"), 1, 0)) %>%
  mutate(twothousandten_levelchange = if_else(year(DATE) >= 2008, 1, 0)) %>%
  mutate(holidays = if_else(month(DATE) == 11 | month(DATE) == 12, 1, 0)) %>%
  mutate(twothousandtwo_levelchange = if_else(year(DATE) >= 2002.2 & year(DATE) <= 2007.5, 1, 0)) %>%
  filter(DATE >= ymd("2000-1-01")) %>%
  select(-EXCAUS, -DATE) %>%
  data.matrix(.) 

#..........................................Fit model.........................................................................#

time_slices <- createTimeSlices(
  y = data_ts, 
  initialWindow = 200,
   horizon = 3, 
   fixedWindow = FALSE
)

best_parameters <- mclapply(
  X = time_slices$train,
  FUN = inner_train, 
  data = data_ts,
  iterations = 100, 
  xreg = data_FE, 
  mc.cores = detectCores()
)

# For single threaded
# best_parameters <- lapply(
# time_slices$train,
# FUN = inner_train,
# xreg = xreg,
# data = data_ts,
# iterations = 30
# )

final_results <- pmap_dbl(
  list(time_slices$train, time_slices$test, best_parameters),
   ~outer_fold(data = data_ts, ..1, ..2, ..3, xreg = data_FE)) %>%
  mean(.)

#.........................................Forecast three months ahead.......................................................#

# Find the final parameters using all of the data
best_param_final <- inner_train(
  train_fold_index = 1:length(data_ts),
  data = data_ts, 
  iterations = 100, 
  xreg = data_FE
)
# Fit the final model

months_end <- month(data$DATE)[length(data$DATE)]
years_end <- year(data$DATE)[length(data$DATE)]
months_forecast <- c(months_end + 1, months_end + 2, months_end + 3) %>%
  if_else(. > 12, . - 12, .)

xreg_newdata <- tibble(
  great_recession_ind = c(0, 0, 0),
  dotcom_bubble = c(0, 0, 0), 
  twothousandten_levelchange = c(1, 1, 1),
  holidays = if_else(months_forecast == 11 | months_forecast == 12, 1, 0), 
  twothousandtwo_levelchange = c(0, 0, 0)
  ) 

final_predictions  <- ts(forecast_ensemble(
  train = data_ts,
  horizon = 3,
  parameters = best_param_final, 
  xreg_train = data_FE,
  xreg_newdata = as.matrix(xreg_newdata)),
start = c(
  if_else(months_end + 1 == 13, years_end + 1, years_end), 
  if_else(months_end + 1 == 13, 1, months_end + 1)), 
frequency = 12)

