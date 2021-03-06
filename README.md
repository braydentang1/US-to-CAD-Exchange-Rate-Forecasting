# US-to-CAD-Exchange-Rate-Forecasting

## Introduction:

An attempt to forecast the US to CAD Exchange Rate.

One can view a Shiny app [here,](https://braydentang1.shinyapps.io/excaus_forecast/) that summarizes the results of this repository.

This project is inspired by what my friend (Riley Peters) and I did in our term project at the University of Regina with a much greater emphasis on:

1) Forecasting and forecasting performance over general time series analysis.
2) Utilizing cross validation in a time series context in which models are dynamically refit per each slice of time.
3) Larger emphasis on accurate prediction intervals, including using methods proposed by [Taylor and Bunn (1999)](https://www.jstor.org/stable/2634872?seq=1) and [Svetunkov (2017)](https://forecasting.svetunkov.ru/en/2017/06/11/smooth-package-for-r-prediction-intervals/).
4) Shorter term forecasts; the term project we did prioritized 12 month ahead forecasts which were unlikely to be accurate given how long the time frame is.

## Instructions:

Requires Docker. You may need sudo privileges in order to run the following docker commands.

To recreate this repository from scratch (and to run the Shiny app locally):

1) Clone this repository.
2) In the root directory of this repository, run:

```docker run --rm -v "/$(pwd):/excaus" btang101/excaus_app make -C /excaus all```

This might take about one hour to run. 

3) Run, from the root directory of this repository:

```docker run --rm -v "/$(pwd):/excaus" btang101/excaus_app Rscript -e "shiny::runApp('excaus_forecast')"```

Then visit the address printed in the terminal using any web browser. 

To reset the repository without any intermediate results, run:

```docker run --rm -v "/$(pwd):/excaus" btang101/excaus_app make -C /excaus clean```

Windows users, you may need to replace:

```"/$(pwd):/excaus"``` with ```"${pwd}://excaus"```

## Dependencies:

- R (tested on 3.6):
	- alfred==0.1.1
	- caret==6.0-86
	- forecast==8.12
	- zoo==1.8-7
	- smooth==2.5.6
	- lubridate==1.7.8
	- shiny==1.4.0.2
	- shinyBS==0.61
	- shinythemes==1.1.2
	- shinyWidgets==0.5.1
	- plotly==4.9.2.1
	- DT==0.13
	- yardstick==0.0.6
	- tidyverse==1.3.0
	
I recommend just using the Docker container.



		
