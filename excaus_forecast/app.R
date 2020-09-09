library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(ggplot2)
library(alfred)
library(shinythemes)
library(shinyhelper)
library(shinyBS)
library(DT)
library(yardstick)

source("src/all_functions.R")

data <- readRDS("results/excaus.rds")

data_ts <- ts(
    data$EXCAUS,
    frequency = 12, 
    start = c(year(data$DATE[1]), month(data$DATE[1])),
    end = c(year(data$DATE[nrow(data)]), month(data$DATE[nrow(data)]))) %>%
    window(., start = c(2000, 1))

data_FE <- xreg_feat_eng(data)
all_predictions <- readRDS("results/predictions.rds")
all_residuals <- readRDS("results/quantile-training-data.rds")

ui <- fluidPage(
    
    # Application title
    titlePanel("Forecasting the US to Canadian Exchange Rate"),
    theme = shinytheme("darkly"),
    includeCSS("style.css"),
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
                selected = as.Date(data$DATE[nrow(data)]) %m+% months(1)
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
            ) %>% helper(size = "l", content = "model_descriptions", type = "markdown", buttonLabel = "Close"),
            bsTooltip("model", "For more information on the chosen model, click on the ? icon.", "right", options = list(container = "body")),
            helpText("For a description of the app, click the information icon...") %>% helper(size = "l", icon = "info", content = "description", type = "markdown", buttonLabel = "Close")
        ),
        mainPanel(
           fluidRow(plotlyOutput("forecast_plot")),
           fluidRow(
               column(tableOutput("evaluation_metrics") %>% helper(size = "l", content = "helper_evaluation", type = "markdown", buttonLabel = "Close"),
                      width = 11),
               tags$style(HTML(
               ".dataTables_wrapper .dataTables_paginate .paginate_button {
                    color: #ffffff !important
               }")),
               column(tabsetPanel(type = "tabs",
                                  selected = "Observed",
                                  tabPanel(
                                      "Forecasts and Intervals", dataTableOutput("forecasts_intervals")),
                                  tabPanel(
                                      "Observed", dataTableOutput("observed")
                                  )
                                 ),
                      width = 10))
        )
    )
)

server <- function(input, output, session) {

    observeEvent(input$time_window, {
        
        if (as.Date(input$time_window) < "2018-01-01") {
            from_date <- "2018-01-01"
        } else {
            from_date <- input$time_window
        }
        
        updateSliderTextInput(session, inputId = "forecast_window", choices = seq.Date(from = as.Date(from_date), to = date(data$DATE[nrow(data)]) %m+% months(1), by = "month"),
                              selected = input$forecast_window)
    })
    
    actual_forecast <- reactive({
        
        month_to <- ifelse(month(input$forecast_window) - 1 == 0, 12, month(input$forecast_window) - 1)
        year_to <- ifelse(month(input$forecast_window) - 1 == 0, year(input$forecast_window) - 1, year(input$forecast_window))
        
        if (input$model == "Ensemble") {
            
            mean_forecast <- all_predictions[[which(seq.Date(from = date("2009-01-01"), to = date(data$DATE[nrow(data)]) %m+% months(1), by = "month") == input$forecast_window)]]
            
            prediction_intervals <- get_intervals(forecast_date = input$forecast_window, predictions_combined = all_residuals, quantile = input$confidence_level / 100)
            
            lower <- prediction_intervals$lower %>%
                ts(., start = c(year(zoo::as.Date(mean_forecast))[1], month(zoo::as.Date(mean_forecast))[1]), frequency = 12) + mean_forecast
            
            upper <- prediction_intervals$upper %>%
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
                twothousandtwo_levelchange = c(0, 0, 0),
                covid = c(0, 0, 0)
            ) %>%
                data.matrix(.)
            
            xreg_temp <- data_FE[1:length(train_ts), ]
            
            if (all(xreg_temp[, "covid"] == 0)) {
                xreg_temp <- xreg_temp[, !colnames(xreg_temp) %in% "covid"]
                xreg_newdata <- xreg_newdata[, !colnames(xreg_newdata) %in% "covid"]
            } else {
                xreg_newdata[, "covid"] <- c(1, 1, 1) 
            }
            
            model <- auto.arima(y = train_ts, xreg = xreg_temp)
            forecasts_ARIMAX <- forecast(model, h = 3, xreg = xreg_newdata, level = input$confidence_level)
            
            list(average = forecasts_ARIMAX$mean, lower = forecasts_ARIMAX$lower[, 1], upper = forecasts_ARIMAX$upper[, 1])
            
        } else {
            
            train_ts <- window(data_ts, end = c(year_to, month_to))
            model <- tbats(y = train_ts)
            forecasts_TBATS <- forecast(model, h = 3, level = input$confidence_level)
            
            list(average = forecasts_TBATS$mean, lower = forecasts_TBATS$lower[, 1], upper = forecasts_TBATS$upper[, 1])
            
        }  
    })
    
    tables <- reactive({
        
        int_plot <- tibble(
            time = zoo::as.Date(time(actual_forecast()$average)),
            lower = as.numeric(actual_forecast()$lower),
            upper = as.numeric(actual_forecast()$upper)
        )
        
        points <- tibble(
            time = zoo::as.Date(time(data_ts)),
            points = as.numeric(data_ts),
            set = rep("Actual", length(data_ts))
        ) %>%
            filter(time >= as.Date(input$time_window), time <= as.Date(time(actual_forecast()$average))[length(time(actual_forecast()$average))]) 
        
        combined <- tibble(
            time = zoo::as.Date(time(actual_forecast()$average)),
            points = as.numeric(actual_forecast()$average),
            set = rep("Forecast", length(actual_forecast()$average))
        ) %>% 
            bind_rows(points, .)
        
        list(intervals = int_plot, points = points, combined = combined)
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
                future_obs = ts(forecast_vs_real$data_ts, frequency = 12, start = c(year(input$forecast_window), month(input$forecast_window))),
                data_ts = data_ts)
         }
        tibble(
            "RMSE of Forecast (in cents)" = rmse_obs,
             "MAE of Forecast (in cents)" = mae_obs,
            "MASE of Forecast" = mase_obs,
            "MSIS of Prediction Interval" = msis_obs
            )
        }, digits = 5, align = "c")
    
    addPopover(session, "evaluation_metrics", title = NULL, content = "For more information regarding these evaluation metrics, click on the ? icon to the right.", trigger = 'hover')

    output$forecast_plot <- renderPlotly({

        int_plot <- tables()$intervals
        points <- tables()$points
        combined <- tables()$combined
        
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
    
    output$forecasts_intervals <- renderDataTable({
        DT::datatable(bind_cols(tables()$intervals %>% rename(Time = time, Upper = upper, Lower = lower), tibble(Mean = actual_forecast()$average)),
                      extensions = "Buttons", rownames = FALSE, options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"))) %>%
            formatStyle(columns = c("Time", "Upper", "Lower", "Mean"), color = "#00bc8c", backgroundColor = "#222") %>%
            formatRound(columns = c("Upper", "Lower", "Mean"), digits = 4)
    })
    
    output$observed <- renderDataTable({
        DT::datatable(tables()$points %>%
                rename(Time = time, Observed = points) %>%
                select(-set),
            extensions = "Buttons",
            rownames = FALSE,
            options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"))
            ) %>%
            formatStyle(columns = c("Time", "Observed"), color = "#00bc8c", backgroundColor = "#222") %>%
            formatRound(columns = c("Observed"), digits = 4)
    })
    
    observe_helpers(withMathJax = TRUE)
    
}

# Run the application   
shinyApp(ui = ui, server = server)
