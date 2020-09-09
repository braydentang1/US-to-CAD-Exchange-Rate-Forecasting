## Model Descriptions

#### Ensemble

- This is a weighted average of all of the other forecasting methods given, that is:

$$\hat y_{t} = \sum_{i = 1}^{M} \alpha_{i} \hat y_{it},$$ 

subject to 

$$\sum_{i = 1}^{M} \alpha_{i} = 1$$

where

$$\hat y_{it} = \text{the forecasted value from the ith model at time t}, $$
$$\alpha_{i} = \text{weight given to the ith model and}, $$
$$M = \text{total number of models}.$$

- The most optimal $\alpha$ are found using random grid search. In the future, I would like to replace this with a true Bayesian model combination.

- Prediction intervals are found using quantile regression as proposed by Taylor and Bunn (1999) with modifications as proposed by Svetunkov (2017). 

#### Random Walk with Drift:

- The classic model described below:

$$Y_{t} = \delta + Y_{t-1} + W_{t},$$

where

$$\delta = \text{drift and},$$
$$W_{t} \ \text{follows a white noise process.}$$

- Prediction intervals are generated using the closed form expressions for random walks.

#### Complex Exponential Smoothing (CES)

- A proposed improvement over exponential smoothing models.
- Overall idea is to circumvent the need to decompose a series and instead use the "information potential" of a series $p_{t}$ and its observed values $y_{t}$. 
- Modelling for both of these components is done simultaneously using a complex variable.
- Please see [this paper by Svetunkov et. al](https://mpra.ub.uni-muenchen.de/69394/1/MPRA_paper_69394.pdf) for a technical description of this method.
- Prediction intervals given are estimated using the same quantile regression method as the ensemble.
- Parameters chosen using AICc.

#### ARIMAX

- An ARIMA model with exogenous regressors, as described [here](https://robjhyndman.com/hyndsight/arimax/):

$$y_t = \beta x_t + \phi_1 y_{t-1} + \cdots + \phi_p y_{t-p} - \theta_1 z_{t-1} - \dots - \theta_q z_{t-q} + z_t,$$

- The exogenous regressors in this case are time series themselves that are tied to specific events (such as the 2009 recession, or if the month is during the holiday period of November/December). They take on the value 1 for specific periods of time where the event is active, and 0 otherwise. 

- All parameters chosen using AICc.

- Prediction intervals provided are the theoretical ones that can be derived in closed form.

#### TBATS

- A highly flexible framework for forecasting time series. Incorporates Box-Cox transformations, seasonality with Fourier terms (though this is ignored in this case since there is no seasonality) and ARMA errors; all automatically. Each of these parameters are chosen using AICc.

- Prediction intervals given are the theoretical ones based on a Gaussian assumption.

