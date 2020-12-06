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


