rm(list = ls(all = TRUE))

library(tseries)
library(forecast)
library(readr)

# Load data
bank_case <- read_csv("C:/Users/maths/Desktop/tsalab_ojas/lab5/bank_case.txt",col_names = FALSE)
bank_case <- bank_case$X1
bank_case

# Step 1: Convert dataset into time series object
bank_data_ts <- as.ts(bank_case)
bank_data_ts

# Step 2: Plot the data
plot(bank_data_ts,
     main = "Commercial Bank Real Estate Loan Data",
     xlab = "Months",
     ylab = "Loans in Billions in Dollar",
     lwd = 2,
     xaxt = "n")
axis(1, seq(0, 70, 5), seq(0, 70, 5))

# Step 3: Check Stationarity
acf(bank_data_ts,
    main = "ACF Plot of Commercial Bank Real Estate Loan Data",
    lag.max = 10,
    ylim = c(-1,1))

pacf(bank_data_ts,
     main = "PACF Plot of Commercial Bank Real Estate Loan Data",
     lag.max = 10,
     ylim = c(-1,1))

adf.test(bank_data_ts)

# Step 4: Convert Non-Stationary to Stationary (Differencing)
bank_data_ts_d1 <- diff(bank_data_ts)
adf.test(bank_data_ts_d1)

bank_data_ts_d2 <- diff(bank_data_ts_d1)
adf.test(bank_data_ts_d2)

acf(bank_data_ts_d2,
    main = "ACF Plot of Commercial Bank Real Estate Loan Data",
    lag.max = 10,
    ylim = c(-1,1))

pacf(bank_data_ts_d2,
     main = "PACF Plot of Commercial Bank Real Estate Loan Data",
     lag.max = 10,
     ylim = c(-1,1))

# Step 5: Model Identification (Try different ARIMA orders)

bank_fit1 <- arima(bank_data_ts, order = c(0,2,0))
bank_fit1$aic

bank_fit2 <- arima(bank_data_ts, order = c(1,2,1))
bank_fit2$aic

bank_fit3 <- arima(bank_data_ts, order = c(0,2,2))
bank_fit3$aic

bank_fit4 <- arima(bank_data_ts, order = c(1,2,2))
bank_fit4$aic

bank_fit5 <- arima(bank_data_ts, order = c(1,2,0))
bank_fit5$aic

bank_fit6 <- arima(bank_data_ts, order = c(0,2,1))
bank_fit6$aic

# Minimum AIC obtained for ARIMA(0,2,1)

# Step 6: Final Model Estimation
bank_fit_Est <- arima(bank_data_ts,
                      order = c(0,2,1),
                      method = "ML")
bank_fit_Est

# Step 7: Diagnostic Checking

res <- bank_fit_Est$residuals

plot(res,
     main = "Residuals of Commercial Bank Real Estate Loan Data",
     type = "l")
abline(h = 0)

# 1. Normality Check
shapiro.test(res)
qqnorm(res)
qqline(res, col = "red")

# 2. Serial Autocorrelation Check
Box.test(res, lag = 10, type = "Box-Pierce")
acf(res)
pacf(res)

# 3. Heteroscedasticity Check
white.test(res)
acf(res^2)
pacf(res^2)

# Step 8: Forecasting
bank_fit_Forecast <- forecast(bank_fit_Est, h = 20)
bank_fit_Forecast

plot(bank_fit_Forecast,
     main = "Forecast of Commercial Bank Real Estate Loan Data for Next 20 Months",
     xlab = "Months",
     ylab = "Loans in Billions in Dollar",
     lwd = 2)

axis(1, seq(0, 90, 5), seq(0, 90, 5))