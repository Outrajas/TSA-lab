# ========== PRACTICAL-11: Forecasting Nottem Data (Manual Selection) ==========
# Forecast monthly temperatures at Nottingham for next 2 years (24 months)
rm(list = ls())
cat("PRACTICAL-11: FORECASTING NOTTEM DATASET (NEXT 24 MONTHS)\n")
cat(rep("=", 70), "\n\n")

# Load required libraries
library(forecast)
library(tseries)
library(lmtest)   # for bptest (heteroskedasticity)
library(ggplot2)

# ------------------------------------------------------------
# 1. LOAD AND EXPLORE DATASET
# ------------------------------------------------------------
cat("1. LOADING NOTTEM DATASET\n")
cat(rep("-", 50), "\n\n")

data("nottem")
cat("Dataset: nottem\n")
cat("Time period:", start(nottem), "to", end(nottem), "\n")
cat("Frequency:", frequency(nottem), "(monthly)\n")
cat("Observations:", length(nottem), "\n\n")

cat("Summary statistics:\n")
print(summary(nottem))
cat("\n")

# Plot original series
plot(nottem, 
     main = "Nottem: Monthly Temperatures (1920-1939)",
     ylab = "Temperature (°F)", 
     xlab = "Year", 
     col = "darkblue", 
     lwd = 2)
grid()

# ------------------------------------------------------------
# 2. CHECK STATIONARITY (ADF TEST) AND VISUAL INSPECTION
# ------------------------------------------------------------
cat("\n2. STATIONARITY CHECK (ADF TEST)\n")
cat(rep("-", 50), "\n\n")

adf_orig <- adf.test(nottem, alternative = "stationary")
print(adf_orig)
if(adf_orig$p.value < 0.05) {
  cat("Conclusion: Original series is stationary (p < 0.05)\n")
} else {
  cat("Conclusion: Original series is non-stationary (p >= 0.05)\n")
}

# ACF and PACF of original series
par(mfrow = c(1,2))
acf(nottem, main = "ACF of Original Series", lag.max = 36)
pacf(nottem, main = "PACF of Original Series", lag.max = 36)
par(mfrow = c(1,1))

# ------------------------------------------------------------
# 3. SEASONAL DIFFERENCING (since strong seasonality)
# ------------------------------------------------------------
cat("\n3. SEASONAL DIFFERENCING (lag = 12)\n")
cat(rep("-", 50), "\n\n")

nottem_seasdiff <- diff(nottem, lag = 12)
cat("First few values of seasonally differenced series:\n")
print(head(nottem_seasdiff, 20))
cat("\n")

# ADF test on seasonal differenced series
adf_seas <- adf.test(na.omit(nottem_seasdiff), alternative = "stationary")
print(adf_seas)
if(adf_seas$p.value < 0.05) {
  cat("After seasonal differencing: stationary.\n")
} else {
  cat("After seasonal differencing: still non-stationary; may need regular differencing.\n")
}

# Plot seasonal differenced series
plot(nottem_seasdiff,
     main = "Seasonally Differenced Series (lag 12)",
     ylab = "Difference", xlab = "Time", col = "darkgreen", lwd = 1)
grid()

# ACF and PACF of seasonal differenced series
par(mfrow = c(1,2))
acf(na.omit(nottem_seasdiff), main = "ACF of Seas. Diff. Series", lag.max = 36)
pacf(na.omit(nottem_seasdiff), main = "PACF of Seas. Diff. Series", lag.max = 36)
par(mfrow = c(1,1))

# ------------------------------------------------------------
# 4. CANDIDATE MODEL IDENTIFICATION (based on ACF/PACF patterns)
# ------------------------------------------------------------
cat("\n4. CANDIDATE ARIMA MODELS\n")
cat(rep("-", 50), "\n\n")
cat("From ACF/PACF of seasonally differenced series:\n")
cat("  - ACF shows significant spike at lag 1 → possible MA(1) in regular part.\n")
cat("  - PACF shows significant spike at lag 1 → possible AR(1) in regular part.\n")
cat("  - Seasonal lags (12, 24) may suggest seasonal AR/MA components.\n")
cat("Candidate models (with seasonal differencing D=1):\n")
cat("  Model A: ARIMA(1,0,0)(1,1,0)[12]\n")
cat("  Model B: ARIMA(0,0,1)(0,1,1)[12]\n")
cat("  Model C: ARIMA(1,0,1)(1,1,0)[12]\n")
cat("  Model D: ARIMA(1,0,0)(0,1,1)[12]\n")
cat("  Model E: ARIMA(0,0,1)(1,1,0)[12]\n")
cat("  Model F: ARIMA(1,0,1)(0,1,1)[12]\n\n")

# ------------------------------------------------------------
# 5. FIT CANDIDATE MODELS AND COMPARE AIC
# ------------------------------------------------------------
cat("5. FITTING CANDIDATE MODELS\n")
cat(rep("-", 50), "\n\n")

# Define candidate models
models <- list(
  A = list(order = c(1,0,0), seasonal = c(1,1,0)),
  B = list(order = c(0,0,1), seasonal = c(0,1,1)),
  C = list(order = c(1,0,1), seasonal = c(1,1,0)),
  D = list(order = c(1,0,0), seasonal = c(0,1,1)),
  E = list(order = c(0,0,1), seasonal = c(1,1,0)),
  F = list(order = c(1,0,1), seasonal = c(0,1,1))
)

results <- data.frame(Model = names(models), AIC = NA, 
                      Ljung_p = NA, Shapiro_p = NA, BP_p = NA)

for(i in seq_along(models)) {
  mod_name <- names(models)[i]
  cat("\n--- Fitting Model", mod_name, "---\n")
  fit <- Arima(nottem, 
               order = models[[i]]$order, 
               seasonal = list(order = models[[i]]$seasonal, period = 12))
  print(fit)
  
  # Store AIC
  results$AIC[i] <- fit$aic
  
  
  # Residuals
  res <- residuals(fit)
  
  # Ljung-Box test for autocorrelation (up to lag 24)
  lb_test <- Box.test(res, lag = 24, type = "Ljung-Box")
  results$Ljung_p[i] <- lb_test$p.value
  cat("Ljung-Box test (lag=24): p-value =", round(lb_test$p.value, 4), "\n")
  
  # Shapiro-Wilk test for normality
  if(length(res) <= 5000) {  # Shapiro test limited to 5000
    sw_test <- shapiro.test(res)
    results$Shapiro_p[i] <- sw_test$p.value
    cat("Shapiro-Wilk test: p-value =", round(sw_test$p.value, 4), "\n")
  } else {
    results$Shapiro_p[i] <- NA
    cat("Shapiro-Wilk test not performed (n > 5000).\n")
  }
  
  # Breusch-Pagan test for heteroskedasticity (regress squared residuals on fitted values)
  fitted_vals <- fitted(fit)
  bp_test <- bptest(res^2 ~ fitted_vals)
  results$BP_p[i] <- bp_test$p.value
  cat("Breusch-Pagan test: p-value =", round(bp_test$p.value, 4), "\n")
}

# Display comparison table
cat("\n\nMODEL COMPARISON TABLE:\n")
print(results)

# ------------------------------------------------------------
# 6. SELECT BEST MODEL BASED ON AIC AND DIAGNOSTICS
# ------------------------------------------------------------
cat("\n6. SELECTING BEST MODEL\n")
cat(rep("-", 50), "\n\n")

# Prefer low AIC, and diagnostics p-values > 0.05 (if possible)
best_model_name <- results$Model[which.min(results$AIC)]
cat("Lowest AIC model: Model", best_model_name, "\n")

# Check diagnostics of best model
best_idx <- which(results$Model == best_model_name)
if(results$Ljung_p[best_idx] > 0.05) {
  cat(" - Ljung-Box: OK (no residual correlation)\n")
} else {
  cat(" - Ljung-Box: WARNING - residual correlation detected\n")
}
if(results$Shapiro_p[best_idx] > 0.05) {
  cat(" - Shapiro-Wilk: OK (errors appear normal)\n")
} else {
  cat(" - Shapiro-Wilk: WARNING - errors non-normal\n")
}
if(results$BP_p[best_idx] > 0.05) {
  cat(" - Breusch-Pagan: OK (homoskedastic errors)\n")
} else {
  cat(" - Breusch-Pagan: WARNING - heteroskedasticity detected\n")
}

# Refit best model for forecasting
best_fit <- Arima(nottem, 
                  order = models[[best_model_name]]$order, 
                  seasonal = list(order = models[[best_model_name]]$seasonal, period = 12))


# ------------------------------------------------------------
# 7. DIAGNOSTIC PLOTS FOR BEST MODEL
# ------------------------------------------------------------
cat("\n7. DIAGNOSTIC PLOTS FOR BEST MODEL\n")
cat(rep("-", 50), "\n\n")

par(mfrow = c(2,2))
# Residuals over time
plot(residuals(best_fit), main = "Residuals of Best Model", ylab = "Residuals", type = "l")
abline(h = 0, col = "red", lty = 2)
# ACF of residuals
acf(residuals(best_fit), main = "ACF of Residuals", lag.max = 36)
# Histogram with normal curve
hist(residuals(best_fit), main = "Histogram of Residuals", probability = TRUE, col = "lightblue")
curve(dnorm(x, mean = mean(residuals(best_fit)), sd = sd(residuals(best_fit))), 
      add = TRUE, col = "red", lwd = 2)
# Q-Q plot
qqnorm(residuals(best_fit), main = "Q-Q Plot")
qqline(residuals(best_fit), col = "red")
par(mfrow = c(1,1))

# ------------------------------------------------------------
# 8. FORECAST NEXT 24 MONTHS
# ------------------------------------------------------------
cat("\n8. FORECAST FOR NEXT 24 MONTHS\n")
cat(rep("-", 50), "\n\n")

forecast_values <- forecast(best_fit, h = 24)
print(forecast_values)

# Plot forecast
plot(forecast_values, 
     main = "Forecast of Nottem Temperatures (Next 24 Months)",
     ylab = "Temperature (°F)", 
     xlab = "Year", 
     col = "darkred", 
     lwd = 2,
     shaded = TRUE)
grid()

# ------------------------------------------------------------
# 9. SUMMARY AND CONCLUSIONS
# ------------------------------------------------------------
cat("\n\n", rep("=", 70), "\n", sep="")
cat("SUMMARY AND CONCLUSIONS\n")
cat(rep("=", 70), "\n\n")

cat("1. Dataset: Nottem (monthly temperatures, 1920-1939)\n")
cat("2. Model selected: ARIMA", 
    paste0("(", paste(best_fit$arma[c(1,6,2)], collapse = ","), ")"),
    "with seasonal order", 
    paste0("(", paste(best_fit$arma[c(3,7,4)], collapse = ","), ")"), "[12]\n")
cat("3. AIC of best model:", round(best_fit$aic, 2), "\n")
cat("4. Diagnostic test p-values:\n")
cat("   - Ljung-Box (lag 24):", round(results$Ljung_p[best_idx], 4), "\n")
cat("   - Shapiro-Wilk:", round(results$Shapiro_p[best_idx], 4), "\n")
cat("   - Breusch-Pagan:", round(results$BP_p[best_idx], 4), "\n")
cat("5. Forecast for 1940-1941 (24 months):\n")
print(round(forecast_values$mean, 2))
cat("\n6. The forecast maintains the seasonal pattern and is consistent with historical data.\n")
cat(rep("=", 70), "\n")