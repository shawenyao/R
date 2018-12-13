# the intercept value returned by `arima` function is actually the mean
# the true intercept needs to be derived

#==== case 1: AR(1) ====
set.seed(1)

n <- 10000
n.ahead <- 10

ts <- arima.sim(model = list(order = c(1, 0, 0), ar = 0.7), n = n) + 10

model <- arima(ts, order = c(1, 0, 0))

coef(model)
intercept <- (1 - coef(model)[1]) * coef(model)[2]
coef_ar1 <- coef(model)[1]

forecast <- NULL
forecast[1] <- ts[n]
for(i in seq_len(n.ahead)){
  forecast[i+1] <- intercept + coef_ar1 * forecast[i]
}

data.frame(
  # predict using `predict` function
  predict = c(ts[n], predict(model, n.ahead = n.ahead)$pred),
  # manual forecast
  forecast = forecast
)


#==== case 2: AR(1) with external regressor (xreg) ====
rm(list = ls())
set.seed(1)

n <- 10000
n.ahead <- 10

xreg <- rnorm(n + n.ahead)
ts <- arima.sim(model = list(order = c(1, 0, 0), ar = 0.7), n = n) + 10 + 2 * xreg[1:n]

model <- arima(ts, order = c(1, 0, 0), xreg = xreg[1:n])

coef(model)

intercept <- (1 - coef(model)[1]) * coef(model)[2]
coef_ar1 <- coef(model)[1]
coef_xreg <- coef(model)[3]

forecast <- NULL
forecast[1] <- ts[n]
for(i in seq_len(n.ahead)){
  forecast[i+1] <- intercept + coef_ar1 * forecast[i] + coef_xreg * (xreg[n+i] - xreg[n+i-1] * coef_ar1)
}

data.frame(
  # predict using `predict` function
  predict = c(ts[n], predict(model, n.ahead = n.ahead, newxreg = xreg[(n+1):(n+n.ahead)])$pred),
  # manual forecast
  forecast = forecast
)

