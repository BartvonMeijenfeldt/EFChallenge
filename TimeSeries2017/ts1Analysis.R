source("constants.R")
source("functions.R")
library(reshape2)
library(xts)
library(readr)
library(forecast)
library(tseries)

path_time_serie1 <- "TimeSeries2017/ts1.csv"

train1 <- getTrain(path_time_serie1, cut_off)


introductoryPlots(train1)

adf.test(train1, alternative = "stationary")

ar1 <- sarima(train1, p = 1, d = 0, q = 0)
ar1Sar1 <- sarima(train1, p = 1, d = 0, q = 0, P = 1, D = 0, Q = 0, S = 12)
ar1Sma1 <- sarima(train1, p = 1, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 12)
#lm(train1 ~ ar1$fit, ar1Sar1$fit, ar1Sma1$fit)


train1 <- ts(train1, frequency = 12)
test1  <- ts(test1, frequency = 12, start = c(18, 7))

#Forecast's Arima function saves fitted values
ar1 <- Arima(train1, order = c(1, 0, 0))
ar1Sma1 <- Arima(train1, order = c(1, 0, 0), seasonal = c(0, 0, 1))

##very important to find the seasonality yourself first
##I can probably also write a function in which I loop
##the standard choices and put them in an auto.arima
##so seasonality 7/4/12

##Automatic Arima model
##  -Selects the number of differences d via unit root tests
##  -Select p and q by minimizing AICc
##  -stepwise search is used to traverse model space, but can be turned of 
fit_arima <- auto.arima(train1, stepwise = F, trace = T, approximation = F) #.parallel = T, num.cores = 4)
summary(fit_arima)

## Exponentional smoothing can not compare AICs
## need to check with CVs
fit_ets <- ets(train1)
checkresiduals(fit_ets)
summary(fit_ets)

##still got to try PROPHET,
##it is from facebook, so might be best


##benchmarking
ts1 <- getTs(path_time_serie1)
ts1 <- ts1 %>% ts(frequency = 12)

# naiveT1 <-  naive(ts1, h = length(test1))
# snaiveT1 <- snaive(ts1, h = length(test1))
# 
# accuracy(naive, test1)[, 'RMSE']
# accuracy(snaive, test1)[, 'RMSE']

##Rolling windows
#Naive
sq <- function(u){u^2}

e_naive <- rep(NA, 10)
for (h in 1 : 10) {
  e          <- (ts1 %>% tsCV(forecastfunction = naive, h = h))[(cut_off + 1) : 250] 
  e_naive[h] <- e %>% sq() %>% mean(na.rm = TRUE) 
}

#ETS
model_ets <- Reduce(paste0, fit_ets$components[1 : 3])
fets <- function(x, h) {
  forecast(ets(x, model = model_ets), h = h)
}

e_ets <- rep(NA, 10)
for (h in 1 : 10) {
  e        <- ( ts1 %>% tsCV(forecastfunction = fets, h = h) )[(cut_off + 1) : 250] 
  e_ets[h] <- e %>% sq() %>% mean(na.rm = TRUE)
}


#Arima
#need to check this part of the code
#I might not pass the right paramets
order_arima          <- c(fit_arima$arma[1], fit_arima$arma[6], fit_arima$arma[2])
order_arima_Seasonal <- c(fit_arima$arma[3], fit_arima$arma[7], fit_arima$arma[4])

farima <- function(x, h) {
  forecast(arima(x, order = order_arima, seasonal = order_arima_Seasonal, include.mean = F), h = h)
}

e_arima <- rep(NA, 10)
for (h in 1 : 10) {
  e          <- (ts1 %>% tsCV(forecastfunction = farima, h = h))[(cut_off + 1) : 250] 
  e_arima[h] <- e %>% sq() %>% mean(na.rm = TRUE) 
}

e_mixed <- rep(NA, 10)
for (h in 1 : 10) {
  e_ets      <- ( ts1 %>% tsCV(forecastfunction = fets, h = h) )[(cut_off + 1) : 250]  
  e_arm      <- (ts1 %>% tsCV(forecastfunction = farima, h = h))[(cut_off + 1) : 250] 
  e_mixed[h] <- ( (e_ets + e_arm) / 2 ) %>% sq() %>% mean(na.rm = TRUE) 
}

simplearima <- function(x, h) {
  forecast(arima(x, order = c(2, 0, 1)), h = h)
}

e_simplearima <- rep(NA, 10)
for (h in 1 : 10) {
  e          <- (ts1 %>% tsCV(forecastfunction = simplearima, h = h))[(cut_off + 1) : 250] 
  e_simplearima[h] <- e %>% sq() %>% mean(na.rm = TRUE)  
}

##Plotting the results
mse <- tibble(h = 1 : 10, e_naiv = e_naive / e_naive, e_ets = e_ets / e_naive, 
              e_arima = e_arima / e_naive, e_mixed = e_mixed / e_naive, 
              e_simplearima = e_simplearima / e_naive)
mse_melt <- melt(mse, id = "h")
ggplot(data = mse_melt, aes(x = h, y = value, colour = variable)) + geom_line() + geom_point()

mse_melt %>% group_by(variable) %>% summarise(rmse = mean(value))
