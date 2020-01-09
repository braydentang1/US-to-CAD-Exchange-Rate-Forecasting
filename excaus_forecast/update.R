library(rsconnect)
library(alfred)
library(tidyverse)

data <- get_fred_series("EXCAUS", "EXCAUS") %>% 
  filter(!is.na(EXCAUS)) %>%
  rename(DATE = date) 
                
saveRDS(data, "data/rds/excaus.rds")

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

# To generate updated predictions

current_predictions <- readRDS("data/rds/predictions.rds")
current_predictions[[length(current_predictions) + 1]] <- point_predictions(x = data_ts, xreg = data_FE, to = starting_dates[length(starting_dates)])
saveRDS(current_predictions, "data/rds/predictions.rds")

rsconnect::deployApp("../excaus_forecast", forceUpdate = TRUE, launch.browser = FALSE)

