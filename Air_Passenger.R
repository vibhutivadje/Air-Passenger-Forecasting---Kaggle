## USE FORECAST LIBRARY.

library(forecast)
library(ggplot2)

## 1. GET DATA

## Create data frame.
AirPassenger.data <- read.csv(file.choose(), header = TRUE)

## See the first 6 records of the file.
head(AirPassenger.data)

## Create time series data set in R using the ts() function.

AirPassenger.ts <- ts(AirPassenger.data$Passengers, 
                      start = c(1949, 1), end = c(1960, 12), freq = 12)

## 2. TEST PREDICTABILITY

## Approach 1 

## 2-a: Use Arima() function to fit AR(1) model 
AirPassenger.ar1<- Arima(AirPassenger.ts, order = c(1,0,0))
summary(AirPassenger.ar1)

## Approach 2 

## 2-b: Create differenced Airpassenger data using (lag-1).
diff.AirPassenger <- diff(AirPassenger.ts, lag = 1)
diff.AirPassenger

## 2-c: Use Acf() function to identify autocorrealtion for differenced Airpassenger, and plot autocorrelation for different lags 

Acf(diff.AirPassenger, lag.max = 12, 
    main = "Autocorrelation for Differenced Air Passenger Data")

## 3. EXPLORE AND VISUALIZE SERIES

## 3-a: Apply the plot() function to create a data plot with the historical data
plot(AirPassenger.ts, 
     xlab = "Time", ylab = "AirPassenger (in $M)", 
     ylim = c(0, 700),main = "AirPassenger", col = "blue")

## 3-b: Auto correlation on time series data
Acf(AirPassenger.ts, lag.max = 12, main = "Autocorrelation for AirPassenger")

## 3-c: Use stl() function to plot times series components of the original data.
AirPassenger.stl <- stl(AirPassenger.ts, s.window = "periodic")

autoplot(AirPassenger.stl, main = "Airpassenger Time Series Components")

## 3-d: Box plot
boxplot(AirPassenger.ts~cycle(AirPassenger.ts), xlab="Passenger Numbers ('000)", ylab="Months", col=rgb(0.1,0.9,0.3,0.4), 
        main="Monthly Air Passengers Boxplot from 1950 to 1960", notch=FALSE)


## 4. DATA PREPROCESSING

## 4-a: Check for missing values
sum(is.na(AirPassenger.data))

## 4-b: Test frequency
frequency(AirPassenger.ts)

## 4-c: Test cycle 
cycle(AirPassenger.ts)

## 4-d: Dataset summary
summary(AirPassenger.ts)

## 5. DATA PARTITION

## 5-a: Develop data partition with the validation partition of 25 periods and the rest for the training partition.

nValid <- 25
nTrain <- length(AirPassenger.ts) - nValid
train.ts <- window(AirPassenger.ts, start = c(1949, 1), end = c(1949, nTrain))
valid.ts <- window(AirPassenger.ts, start = c(1949, nTrain + 1), 
                   end = c(1949, nTrain + nValid))

# Plot the time series data and visualize partitions. 
plot(train.ts, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xaxt = "n", xlim = c(1949, 1962.25), main = "Air Passenger data partition", lwd = 2) 
axis(1, at = seq(1949, 1962.25, 1), labels = format(seq(1949, 1962.25, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(1958.9, 1958.9), c(0, 720))
lines(c(1960.95, 1960.9), c(0, 720))
text(1954, 720, "Training", cex = 0.75)
text(1959.9, 720, "Validation", cex = 0.75)
text(1961.8, 720, "Future", cex = 0.75)
arrows(1958.7, 670, 1948.7, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
## 6. APPLY FORECASTING METHODS

## 6-a: REGRESSION BASED MODELS 

## Apply Regression Models with trend and seasonality and apply forecast() for validataion period.

## i.  Regression model with linear trend and seasonality

train.lin.season <- tslm(train.ts ~ trend + season)
summary(train.lin.season)
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)

# Plot predictions for linear trend and seasonality forecast.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xlim = c(1949, 1962.25), main = "Linear Trend and Seasonality Forecast", 
     col = "blue", lwd =2) 
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(1958.9, 1958.9), c(0, 720))
lines(c(1960.95, 1960.9), c(0, 720))
text(1954, 720, "Training", cex = 0.75)
text(1959.9, 720, "Validation", cex = 0.75)
#text(1961.8, 720, "Future", cex = 0.75)
arrows(1958.7, 670, 1948.7, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
#      lwd = 1, angle = 30)

## ii. Regression model with quadratic trend and seasonality 

train.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.trend.season)
train.trend.season.pred <- forecast(train.trend.season, h = nValid, level = 0)

# Plot predictions for quadratic trend and seasonality forecast.
plot(train.trend.season.pred$mean, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xlim = c(1949, 1962.25), main = "Quadratic Trend and Seasonality Forecast", 
     col = "blue", lwd =2) 
lines(train.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(1958.9, 1958.9), c(0, 720))
lines(c(1960.95, 1960.9), c(0, 720))
text(1954, 720, "Training", cex = 0.75)
text(1959.9, 720, "Validation", cex = 0.75)
#text(1961.8, 720, "Future", cex = 0.75)
arrows(1958.7, 670, 1948.7, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
#       lwd = 1, angle = 30)

## 6-b HOLT-WINTER'S MODEL

hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot predictions for HOLT-WINTER'S model forecast
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Air Passengers", ylim = c(50, 720), bty = "l",
     xlim = c(1949, 1962.25), main = "Holt-winter's model Forecast", 
     col = "blue", lwd =2) 
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(1958.9, 1958.9), c(0, 720))
lines(c(1960.95, 1960.9), c(0, 720))
text(1954, 720, "Training", cex = 0.75)
text(1959.9, 720, "Validation", cex = 0.75)
#text(1961.8, 720, "Future", cex = 0.75)
arrows(1958.7, 670, 1948.7, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1959.1, 670, 1960.8, 670, code = 3, length = 0.1,
       lwd = 1, angle = 30)
#arrows(1961.1, 670, 1962.5, 670, code = 3, length = 0.1,
#       lwd = 1, angle = 30)

## 7. Apply the accuracy() function to compare performance measure of the 3 forecasts developed above. 

round(accuracy(train.lin.season.pred, valid.ts), 3)
round(accuracy(train.trend.season.pred, valid.ts), 3)
round(accuracy(hw.ZZZ.pred, valid.ts), 3)

## 8. CALCULATE THE RESIDUAL OF BEST MODEL TO IMPLEMENT 2-LEVEL FORECASTING 

## 8-a: Level 1: For Quad trend+Seasonality
train.trend.season.pred$residuals
Acf(train.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Training Residuals for Quadratic regression model")

## 8-b: Level 2: Apply AR(1),AR(2),AR(3) and AR(12) on Quad trend+seasonality residual
res.ar1 <- Arima(train.trend.season.pred$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

