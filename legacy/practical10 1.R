rm(list = ls(all = TRUE))

library(tseries)
library(forecast)
library(TTR)
library(lmtest)

# Step 1: Data Calling
data("AirPassengers")
AirPassengers

plot(AirPassengers,
     main = "Monthly Airline Passengers Data")

# Step 2: Time Series Decomposition
AirPassengers_decompose <- decompose(AirPassengers, type = "multiplicative")
AirPassengers_decompose
plot(AirPassengers_decompose)

# Step 3: Checking Stationarity and Non-stationarity
acf(AirPassengers,
    main = "ACF plot for AirPassengers Data")

pacf(AirPassengers,
     main = "PACF plot for AirPassengers Data")

kpss.test(AirPassengers)

# Step 4: Using Transformation
Air_Log <- log(AirPassengers)
plot(Air_Log)

# Step 5: Model Identification

# Double Exponential Smoothing (No Seasonality)
AirPassengers_Holtwinter2 <- HoltWinters(log(AirPassengers), gamma = FALSE)
AirPassengers_Holtwinter2
AirPassengers_Holtwinter2$SSE
plot(AirPassengers_Holtwinter2)

# Triple Exponential Smoothing (Holt-Winters with Seasonality)
AirPassengers_Holtwinter3 <- HoltWinters(log(AirPassengers))
AirPassengers_Holtwinter3
AirPassengers_Holtwinter3$SSE
plot(AirPassengers_Holtwinter3)

# Step 6: Model Estimation
AirPassengers_Holtwinter3 <- HoltWinters(log(AirPassengers))
AirPassengers_Holtwinter3

# Step 7: Forecasting using Triple Exponential Smoothing
ForecastFinal <- forecast(AirPassengers_Holtwinter3, h = 10)
ForecastFinal
plot(ForecastFinal)

# Comparison Forecast (Double Smoothing)
ForecastDummy <- forecast(AirPassengers_Holtwinter2, h = 10)
plot(ForecastDummy)