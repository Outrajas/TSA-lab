rm(list = ls())

# (f) Convert the data into a time series object
data("AirPassengers")   # built-in dataset
air_ts <- AirPassengers

# (g) Plot the data to identify dominant component
plot(air_ts,
     main = "AirPassengers Data",
     xlab = "Year",
     ylab = "Number of Passengers",
     lwd = 2)

# (h) Decompose the data to observe components
decomp <- decompose(air_ts)
plot(decomp)

# (i) Check stationarity using ACF/PACF
acf(air_ts, main = "ACF of AirPassengers Data")
pacf(air_ts, main = "PACF of AirPassengers Data")

# (j) Check stationarity using KPSS test
library(tseries)
kpss_test <- kpss.test(air_ts)
print(kpss_test)

# (k) If non-stationary, difference the data
diff_air_ts <- diff(log(air_ts))   # log transform + differencing
plot(diff_air_ts,
     main = "Log-Differenced AirPassengers Data",
     xlab = "Year",
     ylab = "Differenced Log(Passengers)",
     lwd = 2)

# Re-run KPSS test on transformed data
kpss_test_diff <- kpss.test(diff_air_ts)
print(kpss_test_diff)

# (l) Based on components, fit ARIMA model
# Candidate models
model_ar   <- arima(log(air_ts), order = c(1,1,0), method = "ML")
model_ma   <- arima(log(air_ts), order = c(0,1,1), method = "ML")
model_arma <- arima(log(air_ts), order = c(1,1,1), method = "ML")

# Compare AIC
AIC(model_ar, model_ma, model_arma)

# Suppose ARIMA(1,1,1) chosen
final_model <- arima(log(air_ts), order = c(1,1,1), method = "ML")
summary(final_model)

# (n) Goodness of fit diagnostics
tsdiag(final_model)

# Ljung-Box test
Box.test(final_model$residuals, lag = 10, type = "Ljung-Box")

# Residual plots
plot(final_model$residuals,
     main = "Residuals of Final Model",
     ylab = "Residuals")
acf(final_model$residuals, main = "ACF of Residuals")
