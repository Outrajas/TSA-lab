rm(list=ls())
#1
bank_data<- scan("C:/Users/maths/Desktop/tsalab_ojas/lab4/bank_case.txt")
#2
bank_data_<-ts(bank_data)
#3
plot(bank_data, 
     main = "Commercial Bank Real Estate Loan Data", 
     xaxt = "n", 
     xlab = "Months", 
     ylab = "Loans in billions of dollars", 
     lwd = 2)
axis(1, seq(0, 70, 5), seq(0, 70, 5))
#4

acf(bank_data, 
    main = "ACF of Commercial Bank Real Estate Loan Data", 
    lag.max = 10, 
    ylim = c(-1, 1))


pacf(bank_data, 
     main = "PACF of Commercial Bank Real Estate Loan Data", 
     lag.max = 10, 
     ylim = c(-1, 1))

# 4
library(tseries)
adf_test_result <- adf.test(bank_data)
cat("\nAugmented Dickey-Fuller Test\n")
cat("=============================\n")
cat("data: bank_data\n")
cat("Dickey-Fuller =", round(adf_test_result$statistic, 5), 
    ", Lag order =", adf_test_result$parameter, 
    ", p-value =", adf_test_result$p.value, "\n")

#5
# First difference to remove trend

diff2_bank_data <- diff(bank_data, differences = 2)

# Plot second differenced data
plot(diff2_bank_data,
     main = "Second Differenced Bank Loan Data",
     xlab = "Months",
     ylab = "Differenced Loans",
     lwd = 2)

# ADF test on second differenced data
adf_test_diff2 <- adf.test(diff2_bank_data)
print(adf_test_diff2)

#6

# Plot ACF and PACF of differenced data
acf(diff2_bank_data, main = "ACF of Differenced Data")
pacf(diff2_bank_data, main = "PACF of Differenced Data")

# Fit candidate ARIMA models and compare AIC
model_ar <- arima(bank_data, order = c(1,1,0), method = "ML")  # AR(1)
model_ma <- arima(bank_data, order = c(0,1,1), method = "ML")  # MA(1)
model_arma <- arima(bank_data, order = c(1,1,1), method = "ML") # ARMA(1,1)

AIC(model_ar, model_ma, model_arma)



#7
# Fit ARIMA(0,1,1) model (based on earlier AIC choice)
final_model <- arima(bank_data, order = c(0,1,1), method = "ML")

# Display parameter estimates
summary(final_model)

# Extract coefficients and variance explicitly
final_model$coef      # model coefficients (e.g., MA term)
final_model$sigma2    # estimated variance of residuals

#8
# Diagnostic plots: residuals, ACF of residuals, Ljung-Box p-values
tsdiag(final_model)

# Ljung-Box test for autocorrelation in residuals
Box.test(final_model$residuals, lag = 10, type = "Ljung-Box")

# Plot residuals
plot(final_model$residuals,
     main = "Residuals of Final Model",
     ylab = "Residuals")

# ACF of residuals
acf(final_model$residuals, main = "ACF of Residuals")

#9
# Fit ARIMA(1,1,1) model
better_model <- arima(bank_data, order = c(1,1,1), method = "ML")

# Check parameter estimates
summary(better_model)

# Residual diagnostics again
tsdiag(better_model)
Box.test(better_model$residuals, lag = 10, type = "Ljung-Box")
acf(better_model$residuals, main = "ACF of Residuals (Better Model)")

