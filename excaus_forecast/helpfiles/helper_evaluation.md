## Evaluation Metric Definitions:

#### RMSE (Root Mean Squared Error)

- The average squared difference between the actual observed value of the exchange rate and the forecasted rate given by the model selected. The square root of this average is taken to return the error on the original units.
Mathematically,

$$\sqrt{\frac{1}{h}\sum_{t = 1}^{h} (y_{t} - \hat y_{t})^2}, $$

$\text{where} \ \hat y_{t} \  \text{is the forecasted value of the exchange rate at time t for h = 3 point forecasts.}$ 

#### MAE (Mean Absolute Error)

- The average absolute difference between the actual observed value of the exchange rate and the forecasted rate given by the model selected. Mathematically,

$$\frac{1}{h}\sum_{t = 1}^{h} \left|y_{t} - \hat y_{t}\right| $$

$\text{where} \ \hat y_{t} \  \text{is the forecasted value of the exchange rate at time t for h point forecasts.}$

#### MASE (Mean Absolute Scaled Error)

- The percentage of error a proposed forecasting model has when compared to that of a naive forecasting method (a random walk with no drift, which sets $\hat y_{t} = y_{t-1} \ \text{for all t}$). Values < 1 imply that the proposed method has less error than that of the naive method, whereas values > 1 imply that the proposed method has error worse than that of the naive method. If we have a non-seasonal time series, then define

$$q_{j} = \frac{\displaystyle e_{j}}
    {\displaystyle\frac{1}{T-1}\sum_{t=2}^T |y_{t}-y_{t-1}|}.$$

Then,

$$\text{MASE} = \text{mean}(|q_{j}|).$$

- For more information, please see this [link by Rob J. Hyndman.](https://otexts.com/fpp2/accuracy.html)

#### MSIS (Mean Scaled Interval Score)

- A loss function that penalizes prediction intervals that do not succesfully contain the actual, observed values of a particular forecast period as well as the size of the interval itself (since intervals that are extremely wide are not very useful). Mathematically,

$$MSIS = \frac{1}{h} \times \frac{\sum_{t = n+1}^{n + h} (U_{t} - L_{t}) + \frac{2}{\alpha}(L_{t} - {y_{t}}) \times ind(y_{t} < L_{t}) + \frac{2}{\alpha}(y_{t} - U_{t}) \times ind(y_{t} > U_{t})}{\displaystyle\frac{1}{T-1}\sum_{t=2}^T |y_{t}-y_{t-1}|},$$

where 

$$U_{t} = \text{prediction interval upper bound at time t}, $$
$$L_{t} = \text{prediction interval lower bound at time t}, $$
$$h = \text{forecast horizon} = 3, $$
$$\alpha = \text{significance level of the prediction interval, and } $$
$$ind(\text{event}) \ \text{is the indicator function.}$$

- Please see [this paper here](https://www.sciencedirect.com/science/article/pii/S0169207019301128) for more information.