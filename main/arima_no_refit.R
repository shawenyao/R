library(astsa)
library(forecast)

data <- sarima.sim(
  ar = c(0.9, - 0.5),
  ma = c(-0.3, 0.3),
  n = 1000
)

fit1 <- sarima(xdata = data[1:900], p = 2, d = 0, q = 2)
fit2 <- arima(data[1:900], order = c(2, 0, 2))
fit2_refit <- Arima(y = data[901:1000], model = fit2) 

fit1$fit$coef == fit2_refit$coef

prediction_without_newdata <- predict(fit2, n.ahead = 200)$pred
prediction_with_newdata <- predict(fit2_refit, n.ahead = 100)$pred

par(mar=c(1,1,1,1))
par(mfrow = c(2, 1))
plot(c(data[1:900], prediction_without_newdata), type = "l")
lines(901:1100, prediction_without_newdata, col = "dodgerblue")
plot(c(data[1:1000], predict(fit2_refit, n.ahead = 100)$pred), type = "l")
lines(901:1000, data[901:1000], col = "tomato")
lines(1001:1100, prediction_with_newdata, col = "dodgerblue")
