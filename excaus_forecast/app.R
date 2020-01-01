library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(ggplot2)
library(alfred)
library(shinythemes)
library(shinyhelper)
library(shinyBS)
library(yardstick)

source("../src/all_functions.R")

msis <- function(lower, upper, alpha, future_obs, m = 1) {

    total_width <- sum(upper - lower)
    penalty <- sum((2 / alpha) * ((future_obs < lower) * (lower - future_obs) + (future_obs > upper) * (future_obs - upper)))
    numerator <- total_width + penalty

    year_to <- time(future_obs) - 1/12
    train_ts <- window(data_ts, end = year_to[1]) 
    
    naive_vs_real <- ts.intersect(train_ts, stats::lag(data_ts, -m), dframe = TRUE) %>%
        rename_at(., .vars = vars(2), ~"naive") %>%
        mutate(train_ts = as.numeric(train_ts),
               naive = as.numeric(naive))
    
    denominator <- mae(data = naive_vs_real, truth = train_ts, estimate = naive)$.estimate
    
    numerator / (length(lower) * denominator)
    
}

data <- get_fred_series("EXCAUS", "EXCAUS") %>% 
    filter(!is.na(EXCAUS)) %>%
    rename(DATE = date) 

data_ts <- ts(
    data$EXCAUS,
    frequency = 12, 
    start = c(year(data$DATE[1]), month(data$DATE[1])),
    end = c(year(data$DATE[nrow(data)]), month(data$DATE[nrow(data)]))) %>%
    window(., start = c(2000, 1))

data_FE <- data %>% 
    mutate(great_recession_ind = if_else(DATE >= ymd("2007-12-01") & DATE <= ymd("2009-06-01"), 1, 0)) %>%
    mutate(dotcom_bubble = if_else(DATE >= ymd("2001-03-01") & DATE <= ymd("2001-11-01"), 1, 0)) %>%
    mutate(twothousandten_levelchange = if_else(year(DATE) >= 2008, 1, 0)) %>%
    mutate(holidays = if_else(month(DATE) == 11 | month(DATE) == 12, 1, 0)) %>%
    mutate(twothousandtwo_levelchange = if_else(year(DATE) >= 2002.2 & year(DATE) <= 2007.5, 1, 0)) %>%
    filter(DATE >= ymd("2000-1-01")) %>%
    select(-EXCAUS, -DATE) %>%
    data.matrix(.) 

residuals_sim_df <- bind_rows(readRDS("../data/rds/residuals_sim.rds"))
all_predictions <- readRDS("../data/rds/predictions.rds")

ui <- fluidPage(

    # Application title
    titlePanel("Forecasting the US to Canadian Exchange Rate"),
    theme = shinytheme("slate"),
    sidebarLayout(
        sidebarPanel(
            sliderTextInput(
                inputId = "time_window",
                label = "Select a start time:",
                choices = as.Date(window(time(data_ts), start = c(2010, 1))),
                selected = as.Date("2010-01-01")
                ),
            sliderTextInput(
                inputId = "forecast_window",
                label = "Select a forecast period:",
                choices = seq.Date(from = date("2018-01-01"), to = date(data$DATE[nrow(data)]) %m+% months(1), by = "month"),
                selected = as.Date("2019-11-01")
            ),
            sliderInput(
                inputId = "confidence_level",
                label = "Prediction Interval Confidence Level:",
                min = 70,
                max = 99,
                value = 90, 
                step = 1
            ),
            selectInput(
                inputId = "model",
                label = "Select a Model to View:",
                choices = c(
                    "Ensemble",
                    "Random Walk with Drift (RWD)",
                    "Complex Exponential Smoothing (CES)",
                    "ARIMAX",
                    "TBATS"),
                multiple = FALSE,
                selected = "Ensemble"
            ) %>% helper(),
            helpText("Click icon to the right for description...") %>% helper(size = "l", icon = "info", content = "description", type = "markdown", buttonLabel = "Close")
        ),
        
        mainPanel(
           fluidRow(plotlyOutput("forecast_plot")),
           fluidRow(
               column(tableOutput("evaluation_metrics") %>% helper(size = "l", content = "helper_evaluation", type = "markdown", buttonLabel = "Close"),
                      width = 11))
        )
    )
)

server <- function(input, output, session) {

    observeEvent(input$time_window, {
        if (input$time_window > input$forecast_window) {
            updateSliderTextInput(session, inputId = "time_window", selected = input$forecast_window)
        }
    })
    
    actual_forecast <- reactive({
        
        month_to <- ifelse(month(input$forecast_window) - 1 == 0, 12, month(input$forecast_window) - 1)
        year_to <- ifelse(month(input$forecast_window) - 1 == 0, year(input$forecast_window) - 1, year(input$forecast_window))
        
        if (input$model == "Ensemble") {
            
            mean_forecast <- all_predictions[[which(seq.Date(from = date("2018-01-01"), to = date(data$DATE[nrow(data)]) %m+% months(1), by = "month") == input$forecast_window)]]
            
            lower <-  map(residuals_sim_df, ~quantile(., 0.5 - input$confidence_level / 200)) %>% 
                unlist(.) %>%
                ts(., start = c(year(zoo::as.Date(mean_forecast))[1], month(zoo::as.Date(mean_forecast))[1]), frequency = 12) + mean_forecast
            
            upper <- map(residuals_sim_df, ~quantile(., input$confidence_level / 200 + 0.5)) %>% 
                unlist(.) %>%
                ts(., start = c(year(zoo::as.Date(mean_forecast))[1], month(zoo::as.Date(mean_forecast))[1]), frequency = 12) + mean_forecast
            
            list(average = mean_forecast, lower = lower, upper = upper)
            
        } else if (input$model == "Random Walk with Drift (RWD)") {
            
            model <- rwf(
                y = window(data_ts, end = c(year_to, month_to)),
                h = 3, 
                drift = TRUE,
                level = input$confidence_level
                )
            
            list(average = model$mean, lower = model$lower[, 1], upper = model$upper[, 1])
            
        } else if (input$model == "Complex Exponential Smoothing (CES)") {
            
            model <- auto.ces(
                y = window(data_ts, end = c(year_to, month_to)),
                h = 3, 
                level = input$confidence_level / 100,
                interval = "nonparametric"
            )
            
            list(average = model$forecast, lower = model$lower, upper = model$upper)
        
        } else if (input$model == "ARIMAX") {

            train_ts <- window(data_ts, end = c(year_to, month_to))
            months_forecast <- c(month(input$forecast_window), month(input$forecast_window) + 1, month(input$forecast_window) + 2)
            months_forecast <- ifelse(months_forecast > 12, months_forecast - 12, months_forecast)
            
            xreg_newdata <- tibble(
                great_recession_ind = c(0, 0, 0),
                dotcom_bubble = c(0, 0, 0), 
                twothousandten_levelchange = c(1, 1, 1),
                holidays = if_else(months_forecast == 11 | months_forecast == 12, 1, 0), 
                twothousandtwo_levelchange = c(0, 0, 0)
            ) %>%
                data.matrix(.)
            
            model <- auto.arima(y = train_ts, xreg = data_FE[1:length(train_ts), ])
            forecasts_ARIMAX <- forecast(model, h = 3, xreg = xreg_newdata, level = input$confidence_level)
            
            list(average = forecasts_ARIMAX$mean, lower = forecasts_ARIMAX$lower[, 1], upper = forecasts_ARIMAX$upper[, 1])
            
        } else {
            
            train_ts <- window(data_ts, end = c(year_to, month_to))
            model <- tbats(y = train_ts)
            forecasts_TBATS <- forecast(model, h = 3, level = input$confidence_level)
            
            list(average = forecasts_TBATS$mean, lower = forecasts_TBATS$lower[, 1], upper = forecasts_TBATS$upper[, 1])
            
        }  
    })
    
    output$evaluation_metrics <- renderTable({
        
        month_to <- ifelse(month(input$forecast_window) - 1 == 0, 12, month(input$forecast_window) - 1)
        year_to <- ifelse(month(input$forecast_window) - 1 == 0, year(input$forecast_window) - 1, year(input$forecast_window))
        
          if (sum(as.Date(time(actual_forecast()$average)) %in% as.Date(time(data_ts))) == 0) {
              
            rmse_obs <- NA
            mae_obs <- NA
            mase_obs <- NA
            msis_obs <- NA
            
        } else {
            
            naive_forecasts <- stats::lag(data_ts, -1)
            train_ts <- window(data_ts, end = c(year_to, month_to))
            naive_vs_real <- ts.intersect(train_ts, naive_forecasts, dframe = TRUE) %>%
                mutate(train_ts = as.numeric(train_ts), 
                       naive_forecasts = as.numeric(naive_forecasts)) 
            
            forecast_vs_real <- ts.intersect(data_ts, actual_forecast()$average, dframe = TRUE) %>%
                rename_at(., vars(2), ~"predicted") %>%
                mutate(data_ts = as.numeric(data_ts),
                       predicted = as.numeric(predicted))

            naive_mae <- mae(naive_vs_real, truth = train_ts, estimate = naive_forecasts)$.estimate
            rmse_obs <- rmse(forecast_vs_real, truth = data_ts, estimate = predicted)$.estimate
            mae_obs <- mae(forecast_vs_real, truth = data_ts, estimate = predicted)$.estimate
            mase_obs <- mase(forecast_vs_real, truth = data_ts, estimate = predicted, mae_train = naive_mae)$.estimate
            msis_obs <- msis(
                lower = actual_forecast()$lower, 
                upper = actual_forecast()$upper, 
                alpha = 1 - input$confidence_level/100,
                future_obs = ts(forecast_vs_real$data_ts, frequency = 12, start = c(year(input$forecast_window), month(input$forecast_window))))
         }
        tibble(
            "RMSE of Forecast (in cents)" = rmse_obs,
             "MAE of Forecast (in cents)" = mae_obs,
            "MASE of Forecast" = mase_obs,
            "MSIS of Prediction Interval" = msis_obs
            )
        }, digits = 5, align = "c")

    output$forecast_plot <- renderPlotly({

        int_plot <- tibble(
                time = as_date(time(actual_forecast()$average)),
                lower = as.numeric(actual_forecast()$lower),
                upper = as.numeric(actual_forecast()$upper)
            )
            
        points <- tibble(
            time = as.Date(time(data_ts)),
            points = as.numeric(data_ts),
            set = rep("Actual", length(data_ts))
        ) %>%
            filter(time >= as.Date(input$time_window), time <= as.Date(time(actual_forecast()$average))[length(time(actual_forecast()$average))]) 
        
        combined <- tibble(
            time = as_date(time(actual_forecast()$average)),
            points = as.numeric(actual_forecast()$average),
            set = rep("Forecast", length(actual_forecast()$average))
        ) %>% 
            bind_rows(points, .)
        
        p <- ggplot() + 
            geom_ribbon(data = int_plot, aes(x = time, ymin = lower, ymax = upper), alpha = 0.3) +
            geom_line(data = combined, aes(x = time , y = points, colour = set)) +
            scale_colour_manual(breaks = c("Actual", "Forecast"), values = c("black", "blue")) +
            scale_x_date(breaks = points$time[seq(1, nrow(points), by = 3)], date_labels = "%Y-%B") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(
                x = "Time", 
                colour = "Set",
                y = "EXCAUS")
        
        ggplotly(p) %>%
            layout(
                autosize = TRUE,
                title = list(text = paste0(
                    "US to CAD Exchange Rate From: ",
                    input$time_window,
                    "<br>",
                    "<sup>",
                    input$confidence_level,
                    "% Prediction Intervals in Grey",
                    "</sup>")),
                margin = list(t = 80))
        
    })
    
    observe_helpers(withMathJax = TRUE)
    
}

# Run the application   
shinyApp(ui = ui, server = server)
