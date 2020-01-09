library(tidyverse)
library(forecast)
library(zoo)
library(caret)
library(parallel)
library(lubridate)
library(smooth)
library(alfred)
library(parallel)

source("src/all_functions.R")

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

# Get all forecasts
starting_dates <- seq.Date(from = date("2018-01-01"), to = data$DATE[nrow(data)] %m+% months(1), by = "month")

# To generate all predictions from the very start:

# all_predictions <- pmap(list(starting_dates), .f = ~point_predictions(x = data_ts, xreg = data_FE, to = ..1))
# saveRDS(all_predictions, file = "data/rds/predictions.rds")

# To generate updated predictions

current_predictions <- readRDS("data/rds/predictions.rds")
current_predictions[[length(current_predictions) + 1]] <- point_predictions(x = data_ts, xreg = data_FE, to = starting_dates[length(starting_dates)])
saveRDS(current_predictions, "data/rds/predictions.rds")

# To pull in the saved predictions in predictions.rds

# all_predictions <- readRDS("data/rds/predictions.rds")
# actual_forecast <- all_predictions[[length(all_predictions)]]

# Generate bootstrap forecasts, and then save.

#   set.seed(200350623)
#   all_bootstraps <- forecast::bld.mbb.bootstrap(
#     x = window(data_ts, end = c(2017, 9)),
#     num = 3500)
# 
#   # Takes a long time to run. Only run if you have to!
# 
#   residuals_sim <- mclapply(
#     X = all_bootstraps,
#     FUN = simulated_residuals,
#     xreg = data_FE,
#     data_ts = data_ts,
#     forecast_latest = actual_forecast,
#     mc.cores = 4
#   )
# 
# saveRDS(residuals_sim, file = "data/rds/residuals_sim.rds")

# residuals_sim <- readRDS("data/rds/simulated_residuals.rds")

residuals_sim_df <- bind_rows(residuals_sim)

upper <- map(residuals_sim_df, ~quantile(., 0.975)) %>% 
  unlist(.) %>%
  ts(., start = c(year(zoo::as.Date(actual_forecast))[1], month(zoo::as.Date(actual_forecast))[1]), frequency = 12) + actual_forecast

lower <- map(residuals_sim_df, ~quantile(., 0.025)) %>% 
  unlist(.) %>%
  ts(., start = c(year(zoo::as.Date(actual_forecast))[1], month(zoo::as.Date(actual_forecast))[1]), frequency = 12) + actual_forecast
