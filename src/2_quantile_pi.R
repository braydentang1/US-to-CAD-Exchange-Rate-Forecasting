"This script generates training data for the quantile regression method used to
formulate prediction intervals as described by Taylor and Bunn (1999) and Svetunkov (2017)

Usage: 2_quantile_pi.R --excaus_in=<excaus_in> --results_out=<results_out>

Options:
--excaus_in=<excaus_in> A file path that specifies where the saved EXCAUS time series is (as an .rds file).
--results_out=<results_out> A file path that describes where to store results.
" -> doc

library(tidyverse)
library(forecast)
library(zoo)
library(caret)
library(parallel)
library(lubridate)
library(smooth)
library(alfred)
library(parallel)
library(docopt)

source("src/all_functions.R")

opt <- docopt(doc)

#' Generates training data for the quantile regression method used to formulate prediction intervals
#' as described by Taylor and Bunn (1999) and Svetunkov (2017).
#'
#' @param excaus_in A file path that specifies where the EXCAUS data is stored.
#' @param results_out A file path that describes where to store results.
#'
#' @return None.
#' @export
#'
#' @examples
#' main("results")
main <- function(excaus_in, results_out) {

  # Import data
  data <- readRDS(excaus_in)
  
  data_ts <- ts(
    data$EXCAUS,
    frequency = 12, 
    start = c(year(data$DATE[1]), month(data$DATE[1])),
    end = c(year(data$DATE[nrow(data)]), month(data$DATE[nrow(data)]))) %>%
    window(., start = c(2000, 1)
    )
  
  # Feature engineering for ARIMAX
  data_FE <- xreg_feat_eng(data)
  
  # Get all forecasts
  starting_dates <- seq.Date(
    from = date("2009-01-01"),
    to = data$DATE[nrow(data)] %m+% months(1),
    by = "month"
    )
  
  all_predictions <- map(starting_dates, .f = function(a) point_predictions(x = data_ts, xreg = data_FE, to = a)) %>%
    set_names(starting_dates)
  
  saveRDS(all_predictions, file = paste0(results_out, "/predictions.rds"))
  
  predictions_combined <- tibble(
   pred = reduce(all_predictions[1:(length(all_predictions) - 3)], .f = function(x, y) c(x, y)),
   observed = reduce(map(all_predictions[1:(length(all_predictions) - 3)], .f = function(x) ts.intersect(x, data_ts)[, 2]),
                     .f = function(x, y) c(x, y)),
   error = observed - pred,
   x = rep(1:3, length(all_predictions) - 3),
   starting_dates = rep(starting_dates[1:(length(starting_dates) - 3)], each = 3)
  )
  
  saveRDS(predictions_combined, file = paste0(results_out, "/quantile-training-data.rds"))

}

main(
  excaus_in = opt$excaus_in,
  results_out = opt$results_out
)