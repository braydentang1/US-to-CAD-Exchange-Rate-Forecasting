# US-to-CAD-Exchange-Rate-Forecasting
An attempt to forecast the US to CAD Exchange Rate- Inspired by a school project

This project is basically what my friend (Riley Peters) and I did in our term project at the University of Regina, however, with a much greater emphasis on:

1) Forecasting and forecasting performance over general time series analysis,
2) Utilizing cross validation in a time series context in which models are dynamically refit per each slice of time.
3) Shorter term forecasts; the term project we did prioritized 12 month ahead forecasts which were unlikely to be accurate given how long the time frame is (if it was, we would we rich!).

During the project, we were heavily restricted due to space constraints and also because we did not learn about many of the methods that are commonplace in modern forecasting today; methods like the TBATS or the theta method for instance. This is probably because the course we took was a course rooted in carefully analyzing time series and not necessarily forecasting performance. Furthermore, we didn't talk at all about cross validation which is incredibly important in any forecasting/prediction problem.

I decide to push the boundaries past what we were "allowed" to do in the term project. In this case, I use nested cross validation (in the context of time series, that is, order is maintained) to determine weights to use when combining forecasts from various methods.

Ultimately, I also just want to brush up on my time series skills in R. 