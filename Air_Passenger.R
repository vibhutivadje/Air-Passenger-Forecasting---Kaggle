##Air Passenger Project 

## USE FORECAST LIBRARY.

install.packages("forecast")
library(forecast)
library(zoo)

## CREATE DATA FRAME. 
#Grocery.data <- read.csv("673_case1.csv")
Air_Passenger.data <- read.csv(file.choose(), header = TRUE)
# See the first 6 records of the file.
head(Air_Passenger.data)

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET.
Air_Passenger.ts <- ts(Air_Passenger.data$Passengers, 
               start = c(1949, 1), end = c(1960, 12), freq = 12)

Air_Passenger.ts

## Use plot() to plot time series data  
plot(Air_Passenger.ts, 
     xlab = "Time", ylab = "Passengers (in Hundreds) ", 
     ylim = c(100, 700), main = "Monthly Air Passenger", col = "blue")

# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
Air_Passenger.stl <- stl(Air_Passenger.ts, s.window = "periodic")
Air_Passenger.stl
autoplot(Air_Passenger.stl, main = "Gorcery Store Time Series Components")
