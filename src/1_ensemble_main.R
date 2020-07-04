"This script evaluates the proposed ensemble method.

Usage: 1_ensemble_main.R --iterations=<iterations> --results_out=<results_out> --excaus_in=<excaus_in>

Options: 

--iterations=<iterations> An integer that specifies the number of iterations of random search to use.
--results_out=<results_out> A file path that specifies where to output the ensemble validation score.
--excaus_in=<excaus_in> A file path that specifies where the stored EXCAUS data is (as an .rds file)
" -> doc

library(tidyverse)
library(forecast)
library(caret)
library(parallel)
library(lubridate)
library(smooth)
library(alfred)
library(parallel)
library(docopt)

opt <- docopt(doc)

source("src/all_functions.R")

main <- function(iterations, results_out, excaus_in) {
  #......................Import Data..........................................#
  
  data <- readRDS(excaus_in)
  
  data_ts <- ts(
    data$EXCAUS,
     frequency = 12, 
     start = c(year(data$DATE[1]), month(data$DATE[1])),
      end = c(year(data$DATE[nrow(data)]), month(data$DATE[nrow(data)]))) %>%
    window(., start = c(2000, 1)
  )
  
data_FE <- xreg_feat_eng(data)
  
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
    iterations = iterations, 
    xreg = data_FE, 
    mc.cores = detectCores() - 2
  )
  
  final_results <- pmap_dbl(
    list(time_slices$train, time_slices$test, best_parameters),
     ~outer_fold(data = data_ts, ..1, ..2, ..3, xreg = data_FE)) %>%
    mean(.)
  
  write_csv(final_results, paste0(results_out, "/ensemble_validation.csv"))

}

main(
  iterations = opt$iterations
)