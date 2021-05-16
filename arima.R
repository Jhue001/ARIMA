### ARIMA with R ###
#Forecasting Johnson & Johnson stock price with ARIMA
###############################################################################

myData <- read.csv("JNJ.csv")
dates <- as.Date(myData$Date)
myData <- cbind(myData, dates)

#load libraries
library(MASS)
library(tseries)
library(forecast)

#Plot and log trainprice (90% of data)
lnprice <- log(myData$Close[1:(nrow(myData)*0.9)])

#ACF PACF Dickey-fuller test
acf(lnprice, lag.max = 20)
pacf(lnprice, lag.max = 20)
adf.test(lnprice)

#non-stationarity detected
difflnprice <- diff(lnprice, lag = 1, difference = 1)
adf.test(difflnprice)
acf(difflnprice, lag.max = 20)
pacf(difflnprice, lag.max = 20)

# Likely MA(1) model
# change to timeseries, autoarima
myts <- ts(lnprice, start = c(2020, as.numeric(format(myData$dates[1], "%j"))),
           frequency = 365)
fitlnprice <- auto.arima(myts)
# Xi+1=Xi+ϵi

#plot
plot(myts, type='l')
title('JNJ price')
exp(lnprice)

#forecast
forecasted <- forecast(fitlnprice, h= (0.1*nrow(myData)))
plot(forecasted)

forecast_values <- as.numeric(forecasted$mean)
finalforecast <- exp(forecast_values)

#evaluation
df <- data.frame(myData$Close[226:250], finalforecast)
names(df) <- c("Actual", "Forecasted")
percentage_error = ((df$Actual-df$Forecasted)/df$Actual)
mean( percentage_error)

#Ljung-box _ serial correlation test
Box.test(fitlnprice$residuals, lag=5, type = "Ljung-Box")

