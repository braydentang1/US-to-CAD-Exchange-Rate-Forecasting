library(tidyverse)
library(forecast)
library(caret)
library(parallel)
library(lubridate)
library(smooth)
library(alfred)
library(parallel)

source("src/all_functions.R")

#0.02463 RMSE
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
  initialWindow = length(data_ts) - 12,
   horizon = 3, 
   fixedWindow = FALSE
)

best_parameters <- mclapply(
  X = time_slices$train,
  FUN = inner_train, 
  data = data_ts,
  iterations = 60, 
  xreg = data_FE, 
  mc.cores = 4
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