# ========== PRACTICAL-4: US Population Analysis ==========
# Load required libraries
library(ggplot2)
library(forecast)

cat("PRACTICAL-4: US POPULATION ANALYSIS (1970-1990)\n")
cat(rep("=", 70), "\n\n")

# ------------------------------------------------------------
# (a) IMPORT DATA AS TIME SERIES OBJECT
# ------------------------------------------------------------
cat("(a) IMPORTING DATA AS TIME SERIES OBJECT\n")
cat(rep("-", 50), "\n\n")

# Create the dataset from given values
us_population <- data.frame(
  Year = 1970:1990,
  Population = c(3929214, 5308483, 7239881, 9638453, 12860702, 
                 17063353, 23191876, 31443321, 38558371, 50189209, 
                 62979666, 76212168, 92228496, 106021537, 123202624, 
                 132164569, 151325798, 179323175, 203302031, 
                 226542203, 248709873)
)

cat("1. Dataset created with 21 observations (1970-1990)\n")
cat("2. First few observations:\n")
print(head(us_population))
cat("\n3. Last few observations:\n")
print(tail(us_population))

# Create time series object (annual data, frequency = 1)
us_pop_ts <- ts(us_population$Population, start = 1970, frequency = 1)

cat("\n4. Time series object created:\n")
cat("   - Start year:", start(us_pop_ts), "\n")
cat("   - End year:", end(us_pop_ts), "\n")
cat("   - Frequency:", frequency(us_pop_ts), "(annual data, no seasonality)\n")
cat("   - Observations:", length(us_pop_ts), "\n\n")

# Plot original time series
cat("5. Plotting original time series...\n")
plot(us_pop_ts, 
     main = "US Population (1970-1990)",
     ylab = "Population",
     xlab = "Year",
     col = "darkgreen", 
     lwd = 2,
     type = "o",
     pch = 16)
grid()

# ------------------------------------------------------------
# (b) IDENTIFY DOMINATING COMPONENT(S)
# ------------------------------------------------------------
cat("\n\n(b) IDENTIFYING DOMINATING COMPONENT(S)\n")
cat(rep("-", 50), "\n\n")

cat("1. Analyzing time series components...\n")
cat("   Note: Annual data has no seasonal component (frequency = 1)\n")
cat("   We analyze for trend vs. random components only\n\n")

# For annual data, we manually analyze trend vs random
# Calculate simple moving average as trend approximation
window_size <- 3  # 3-year moving average
if (length(us_pop_ts) >= window_size) {
  trend_approx <- filter(us_pop_ts, rep(1/window_size, window_size), sides = 2)
  random_approx <- us_pop_ts - trend_approx
}

# Calculate variances
pop_var_total <- var(us_pop_ts, na.rm = TRUE)

# For annual data, fit linear model to estimate trend variance
time_index <- 1:length(us_pop_ts)
linear_model <- lm(us_pop_ts ~ time_index)
fitted_trend <- fitted(linear_model)
trend_variance <- var(fitted_trend, na.rm = TRUE)
residual_variance <- var(residuals(linear_model), na.rm = TRUE)

cat("2. VARIANCE ANALYSIS:\n")
cat("   Total variance:", format(pop_var_total, scientific = FALSE, big.mark = ","), "\n")
cat("   Trend variance:", format(trend_variance, scientific = FALSE, big.mark = ","), "\n")
cat("   Residual variance:", format(residual_variance, scientific = FALSE, big.mark = ","), "\n\n")

cat("3. PERCENTAGE OF VARIANCE EXPLAINED:\n")
cat("   Trend:", round((trend_variance/pop_var_total)*100, 1), "%\n")
cat("   Residual:", round((residual_variance/pop_var_total)*100, 1), "%\n\n")

cat("4. DOMINATING COMPONENT IDENTIFICATION:\n")
if (trend_variance > residual_variance) {
  cat("   DOMINATING COMPONENT: TREND\n")
  cat("   Reason: Trend explains", round((trend_variance/pop_var_total)*100, 1), "% of total variance\n")
} else {
  cat("   DOMINATING COMPONENT: RANDOM/RESIDUAL\n")
}

# ------------------------------------------------------------
# (c) SQUARE ROOT TRANSFORMATION
# ------------------------------------------------------------
cat("\n\n(c) APPLYING SQUARE ROOT TRANSFORMATION\n")
cat(rep("-", 50), "\n\n")

# Apply square root transformation
us_pop_sqrt <- sqrt(us_pop_ts)

cat("1. Square root transformation applied: sqrt(Population)\n")
cat("2. Comparison of first 5 values:\n")
cat("   Year  Original  Square_Root\n")
for(i in 1:5) {
  cat("   ", 1969+i, "  ", 
      format(us_pop_ts[i], big.mark = ","), "  ", 
      round(us_pop_sqrt[i], 2), "\n", sep="")
}

# Plot comparison
cat("\n3. Plotting comparison...\n")
par(mfrow = c(1, 2))
plot(us_pop_ts, 
     main = "Original US Population",
     ylab = "Population",
     xlab = "Year",
     col = "darkgreen", 
     lwd = 2,
     type = "o")
grid()

plot(us_pop_sqrt, 
     main = "Square Root Transformed",
     ylab = "sqrt(Population)",
     xlab = "Year",
     col = "purple", 
     lwd = 2,
     type = "o")
grid()
par(mfrow = c(1, 1))

# ------------------------------------------------------------
# (d) ESTIMATE LINEAR TREND
# ------------------------------------------------------------
cat("\n\n(d) ESTIMATING LINEAR TREND\n")
cat(rep("-", 50), "\n\n")

cat("1. Linear regression model fitted:\n")
cat("   Population = β₀ + β₁ × Time\n\n")

cat("2. MODEL COEFFICIENTS:\n")
cat("   Intercept (β₀):", format(coef(linear_model)[1], scientific = FALSE, big.mark = ","), "\n")
cat("   Slope (β₁):", format(coef(linear_model)[2], scientific = FALSE, big.mark = ","), "\n")
cat("   Interpretation: Population increases by", 
    format(round(coef(linear_model)[2]), big.mark = ","), 
    "per year\n\n")

cat("3. MODEL STATISTICS:\n")
cat("   R-squared:", round(summary(linear_model)$r.squared, 4), "\n")
cat("   Adjusted R-squared:", round(summary(linear_model)$adj.r.squared, 4), "\n")
cat("   F-statistic:", round(summary(linear_model)$fstatistic[1], 2), "\n")
cat("   p-value:", format.pval(pf(summary(linear_model)$fstatistic[1],
                                  summary(linear_model)$fstatistic[2],
                                  summary(linear_model)$fstatistic[3],
                                  lower.tail = FALSE)), "\n\n")

# Plot with trend line
cat("4. Plotting data with linear trend line...\n")
plot(us_pop_ts, 
     main = "US Population with Linear Trend",
     ylab = "Population",
     xlab = "Year",
     col = "darkgreen", 
     lwd = 2,
     type = "o",
     pch = 16)
lines(time_index + 1969, fitted_trend, 
      col = "red", 
      lwd = 3,
      lty = 1)
legend("topleft", 
       legend = c("Original Data", "Linear Trend"), 
       col = c("darkgreen", "red"), 
       lwd = c(2, 3), 
       lty = 1,
       bg = "white")
grid()

# ------------------------------------------------------------
# (e) REMOVE ESTIMATED LINEAR TREND
# ------------------------------------------------------------
cat("\n\n(e) REMOVING ESTIMATED LINEAR TREND\n")
cat(rep("-", 50), "\n\n")

# Detrend the data
us_pop_detrended <- us_pop_ts - fitted_trend

cat("1. Linear trend removed from data\n")
cat("2. First 5 detrended values:\n")
for(i in 1:5) {
  cat("   Year", 1969+i, ":", round(us_pop_detrended[i]), "\n")
}

cat("\n3. Variance comparison:\n")
cat("   Original variance:", format(pop_var_total, scientific = FALSE, big.mark = ","), "\n")
cat("   Detrended variance:", format(var(us_pop_detrended), scientific = FALSE, big.mark = ","), "\n")
cat("   Variance reduction:", round((1 - var(us_pop_detrended)/pop_var_total)*100, 1), "%\n\n")

# Plot detrended data
cat("4. Plotting detrended data...\n")
plot(us_pop_detrended, 
     main = "US Population (Detrended)",
     ylab = "Population - Trend",
     xlab = "Year",
     col = "blue", 
     lwd = 2,
     type = "o",
     pch = 16)
abline(h = 0, 
       col = "red", 
       lty = 2, 
       lwd = 1.5)
grid()

# ------------------------------------------------------------
# SUMMARY COMPARISON
# ------------------------------------------------------------
cat("\n\n", rep("=", 70), "\n", sep="")
cat("SUMMARY COMPARISON\n")
cat(rep("=", 70), "\n\n")

cat("COMPARISON TABLE:\n")
cat(rep("-", 70), "\n")
cat("Analysis Step              | Key Result/Value\n")
cat(rep("-", 70), "\n")
cat("Total Observations         | 21 years (1970-1990)\n")
cat("Population Growth          | 3.9M → 248.7M\n")
cat("Dominant Component         | TREND (", round((trend_variance/pop_var_total)*100, 1), "% variance)\n", sep="")
cat("Linear Trend Equation      | Population = ", format(round(coef(linear_model)[1]), big.mark = ","), 
    " + ", format(round(coef(linear_model)[2]), big.mark = ","), " × Year\n", sep="")
cat("Annual Growth Rate         | ", format(round(coef(linear_model)[2]), big.mark = ","), " people/year\n", sep="")
cat("Model R-squared            | ", round(summary(linear_model)$r.squared, 4), " (", 
    round(summary(linear_model)$r.squared*100, 1), "% variance explained)\n", sep="")
cat("Variance after Detrending  | Reduced by ", round((1 - var(us_pop_detrended)/pop_var_total)*100, 1), "%\n", sep="")
cat(rep("-", 70), "\n\n")

cat("KEY OBSERVATIONS:\n")
cat("1. Population Growth Pattern:\n")
cat("   - Strong linear growth trend (", round((trend_variance/pop_var_total)*100, 1), "% of variance)\n", sep="")
cat("   - Minimal random variation (", round((residual_variance/pop_var_total)*100, 1), "% of variance)\n", sep="")
cat("   - No seasonal component (annual data)\n\n")

cat("2. Statistical Analysis:\n")
cat("   - Linear model provides excellent fit (R² = ", round(summary(linear_model)$r.squared, 4), ")\n", sep="")
cat("   - Highly significant trend (p < 0.001)\n")
cat("   - Square root transformation successfully applied\n")
cat("   - Detrending removes ", round((1 - var(us_pop_detrended)/pop_var_total)*100, 1), "% of variance\n\n", sep="")

cat("3. Practical Implications:\n")
cat("   - Consistent population growth during 1970-1990\n")
cat("   - Linear model suitable for short-term forecasting\n")
cat("   - Transformation useful for variance stabilization\n")
cat("   - Trend analysis reveals underlying growth patterns\n")
cat(rep("=", 70), "\n")