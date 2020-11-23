##Air Passenger Project 

## USE FORECAST LIBRARY.

install.packages("forecast")
library(forecast)
library(zoo)

## CREATE DATA FRAME. 
#Grocery.data <- read.csv("673_case1.csv")
AirPassenger.data <- read.csv(file.choose(), header = TRUE)
# See the first 6 records of the file.
head(AirPassenger.data)
Summary(AirPassenger.data)


# 1. Plot the data and visualize time series components.

## 1-a Create time series data set in R using the ts() function.

AirPassenger.ts <- ts(AirPassenger.data$Passengers, 
                      start = c(1949, 1), end = c(1960, 12), freq = 12)

## 1-b Apply the plot() function to create a data plot with the historical data, provide it in 
##your report, and explain what time series components can be visualized in this plot.

plot(AirPassenger.ts, 
     xlab = "Time", ylab = "AirPassenger (in $M)", 
     ylim = c(0, 700),main = "AirPassenger", col = "blue")

autocor <- Acf(AirPassenger.ts, lag.max = 12, main = "Autocorrelation for AirPassenger")
AirPassenger.stl <- stl(AirPassenger.ts, s.window = "periodic")
autoplot(AirPassenger.stl, main = "Airpassenger Time Series Components")


# 2. Apply five regression models using data partition.

## 2-a Develop data partition with the validation partition of 16 periods and the rest for the
##training partition.

nValid <- 44
nTrain <- length(AirPassenger.ts) - nValid
train.ts <- window(AirPassenger.ts, start = c(1949, 1), end = c(1949, nTrain))
valid.ts <- window(AirPassenger.ts, start = c(1949, nTrain + 1), 
                   end = c(1949, nTrain + nValid))

## 2-b. Use the tslm() function for the training partition to develop each of the 5 regression
##models from the above list. Apply the summary() function to identify the model structure
##and parameters for each regression model, show them in your report, and also present
##the respective model equation. Use each model to forecast AirPassengers for the validation
##period using the forecast() function.

##2-b-i Regression model with linear trend

train.lin <- tslm(train.ts ~ trend)
summary(train.lin)
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)

##2-b-ii Regression mode with quadratic trend

train.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(train.quad)
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)

##2-b-iii Regression model with seasonality

train.season <- tslm(train.ts ~ season)
summary(train.season)
train.season.pred <- forecast(train.season, h = nValid, level = 0)

##2-b-iv Regression model with linear trend and seasonality

train.lin.season <- tslm(train.ts ~ trend + season)
summary(train.lin.season)
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)

##2-b-v Regression model with quadratic trend and seasonality

train.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.trend.season)
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)

## 2-c. Apply the accuracy() function to compare performance measure of the 5 forecasts you
##developed in 2b. Present the accuracy measures in your report, compare them, and, using
##MAPE and RMSE, identify the two most accurate regression models for forecasting.

round(accuracy(train.lin.pred, valid.ts), 3)
round(accuracy(train.quad.pred, valid.ts), 3)
round(accuracy(train.season.pred, valid.ts), 3)
round(accuracy(train.lin.season.pred, valid.ts), 3)
round(accuracy(train.trend.season.pred, valid.ts), 3)

# 3. Employ the entire data set to make time series forecast.

##3-a - ## Based on above question, 2 most accurate models are- 

## Model-1: Regression model with linear trend and seasonality for entire dataset

AirPassenger.lin.season <- tslm(AirPassenger.ts ~ trend + season)
summary(AirPassenger.lin.season)
AirPassenger.lin.season.pred <- forecast(AirPassenger.lin.season, h = 4, level = 0)

## Model-2: Regression model with quadratic trend and seasonality for entire dataset

AirPassenger.trend.season <- tslm(AirPassenger.ts ~ trend + I(trend^2) + season)
summary(AirPassenger.trend.season)
AirPassenger.trend.season.pred <- forecast(AirPassenger.trend.season, h = 4, level = 0)

## 3-b

# Performance measures for regression model with linear trend and seasonality
round(accuracy(AirPassenger.lin.season.pred$fitted, AirPassenger.ts), 3)

# Performance measures for regression model with quadratic trend and seasonality
round(accuracy(AirPassenger.trend.season.pred$fitted, AirPassenger.ts), 3)

# Performance measures using naive forecast
round(accuracy((naive(AirPassenger.ts))$fitted, AirPassenger.ts), 3)

# Performance measures using seasonal naive forecast
round(accuracy((snaive(AirPassenger.ts))$fitted, AirPassenger.ts), 3)
