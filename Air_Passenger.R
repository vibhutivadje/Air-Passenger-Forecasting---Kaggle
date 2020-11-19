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
