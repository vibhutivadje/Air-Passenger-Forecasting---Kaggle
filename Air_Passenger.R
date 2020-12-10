##Air Passenger Project 

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

## 4-d: Box plot
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


