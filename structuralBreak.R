## F-test for structual break
fit <- arima(train1, order = order_arima, seasonal = order_arima_Seasonal, include.mean = FALSE)


rss <- sum(residuals(fit)^2)
sigma2 <- fit$sigma2
stats <- rep(NA, cut_off)
for (i in seq.int(20, 180))
{
  fit1 <- arima(train1[seq(1,i)], order = order_arima, seasonal = order_arima_Seasonal, include.mean = FALSE)
  fit2 <- arima(train1[seq(i+1,cut_off)], order = order_arima, seasonal = order_arima_Seasonal, include.mean = FALSE)
  ess <- sum(c(residuals(fit1), residuals(fit2))^2)
  stats[i] <- (rss - ess)/sigma2
}

plot(stats, ylim = c(-10, 10))
abline(h = qchisq(0.05, df = length(coef(fit)), lower.tail = FALSE), lty = 2, col = "red")
