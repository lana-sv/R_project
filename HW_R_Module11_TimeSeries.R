#R for Data Science - class August-Sept 2022 with Prateek
#Homework

#Module 11, Assignment 02: 
#1) Arima model for AirPassangers dataset, store result in mod_time

#2) Predict values for the nnext 20 years and store in pred_time

#3) plot actual values and predicted values

install.packages("forecast")
install.packages("tseries")
install.packages("patchwork")
install.packages("fpp2")
install.packages("ggfortify")

library(forecast) 
library(tseries)
library(ggfortify)

# Libraries for Time Series Analysis
library(tidyverse) # metapackage of all tidyverse packages
library(forecast)  # meta package for Time Series Modeling
library(patchwork) # wrapper package for visualizations
library(fpp2) # meta package for Time series Modeling


data(AirPassengers)
AP <- AirPassengers
# Take a look at the class of the dataset AirPassengers

sum(is.na(AP))
class(AP)
frequency(AP)
cycle(AP)
# Review the table summary
summary(AP)
# Plot the raw data using the base plot function
plot(AP,xlab="Date", ylab = "Passenger numbers (1000's)",main="Air Passenger numbers from 1949 to 1961")

autoplot(AP) + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 

boxplot(AP~cycle(AP),xlab="Date", ylab = "Passenger Numbers (1000's)" ,main ="Monthly Air Passengers Boxplot from 1949 to 1961")


decomposeAP <- decompose(AP,"multiplicative")
autoplot(decomposeAP)

adf.test(AP) 

autoplot(acf(AP,plot=FALSE))+ labs(title="Correlogram of Air Passengers from 1949 to 1961") 

# Review random time series for any missing values
decomposeAP$random

# Autoplot the random time series from 7:138 which exclude the NA values
autoplot(acf(decomposeAP$random[7:138],plot=FALSE))+ labs(title="Correlogram of Air Passengers Random Component from 1949 to 1961")

#autoplot(AP) + geom_smooth(method="lm")+ labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 

arimaAP <- auto.arima(AP)
arimaAP

ggtsdiag(arimaAP)

#plot a forecast of the time series using the forecast function, again from the forecast R package, 
#with a 95% confidence interval where h is the forecast horizon periods in months: 10 years = 120 months
forecastAP <- forecast(arimaAP, level = c(95), h = 120)
autoplot(forecastAP)


