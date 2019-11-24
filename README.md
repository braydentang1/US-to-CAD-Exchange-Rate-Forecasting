# US-to-CAD-Exchange-Rate-Forecasting
An attempt to forecast the US to CAD Exchange Rate- Inspired by a school project

Currently, the best model differs depending on the movement of the exchange over time. Right now, a simple random walk with drift appears to be the most favourable model (sits at around 0.025 RMSE), though at times is beat by a more complicated ensemble method that utilizes a [TBATS](https://pkg.robjhyndman.com/forecast/reference/tbats.html), [complex exponential smoothing](https://kourentzes.com/forecasting/2016/10/22/complex-exponential-smoothing/), and an [ARIMAX](https://robjhyndman.com/hyndsight/arimax/) (ARIMA with exogenous regressors) model in addition to the random walk with drift.

This project is inspired by what my friend (Riley Peters) and I did in our term project at the University of Regina, however, with a much greater emphasis on:

1) Forecasting and forecasting performance over general time series analysis,
2) Utilizing cross validation in a time series context in which models are dynamically refit per each slice of time.
3) Shorter term forecasts; the term project we did prioritized 12 month ahead forecasts which were unlikely to be accurate given how long the time frame is.

I decide to push the boundaries past what we were "allowed" to do in the term project. In this case, I use nested cross validation (in the context of time series, that is, order is maintained) to determine weights to use when combining forecasts from various methods.

Ultimately, I also just want to brush up on my time series skills in R. 
