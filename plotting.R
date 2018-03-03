source("constants.R")
source("functions.R")
library(forecast)
library(ggplot2)

path_time_serie1 <- "TimeSeries2017/ts1.csv"

train1 <- getTrain(path_time_serie1, cut_off)
test1  <- getTest(path_time_serie1, cut_off)

introductoryPlots(train1)

p_ap      <- autoplot(train1)
p_diff_ap <- autoplot(diff(train1))
p_acf     <- ggAcf(train1)
p_pacf    <- ggPacf(train1)

multiplot(p_ap, p_diff_ap, p_acf, p_pacf, cols = 2)
