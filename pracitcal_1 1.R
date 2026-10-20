# ========== PRACTICAL-1: US Population Analysis from Excel ==========
cat("PRACTICAL-1: US POPULATION ANALYSIS FROM EXCEL\n")
cat(rep("=", 70), "\n\n")

# ------------------------------------------------------------
# (a) Data entered into Excel file (assumed already done)
# ------------------------------------------------------------
cat("(a) DATA ENTRY INTO EXCEL FILE\n")
cat(rep("-", 50), "\n\n")
cat("1. Data entered into Excel file: 'us_population.xlsx'\n")
cat("2. Excel file contains:\n")
cat("   - Column A: Year (1790 to 1990, every 10 years)\n")
cat("   - Column B: US_Population (in millions)\n")
cat("   - 21 observations total\n\n")

# ------------------------------------------------------------
# (b) Import data from Excel file
# ------------------------------------------------------------
cat("(b) IMPORTING DATA FROM EXCEL FILE\n")
cat(rep("-", 50), "\n\n")

# Install and load readxl package (commented after first run)
cat("1. Loading required package for Excel import...\n")
# install.packages("readxl")  # Run this once if package not installed
library(readxl)

cat("2. Setting working directory...\n")
setwd("C:/Users/maths/Desktop/tsalab_ojas/lab1")

cat("3. Importing data from Excel file...\n")
us_pop_data <- read_excel("us_population.xlsx")

cat("4. Displaying imported data structure:\n")
str(us_pop_data)
cat("\n5. First few observations:\n")
print(head(us_pop_data))
cat("\n6. Last few observations:\n")
print(tail(us_pop_data))

# ------------------------------------------------------------
# (c) Call suitable package for time series analysis
# ------------------------------------------------------------
cat("\n\n(c) LOADING TIME SERIES ANALYSIS PACKAGES\n")
cat(rep("-", 50), "\n\n")

cat("1. Loading time series analysis packages...\n")
library(forecast)
library(ggplot2)

cat("2. Packages loaded successfully:\n")
cat("   - forecast: For time series analysis and forecasting\n")
cat("   - ggplot2: For data visualization\n")
cat("   - stats: Built-in R package for statistical functions\n")

# ------------------------------------------------------------
# (d) Convert to time series object and explain frequency
# ------------------------------------------------------------
cat("\n\n(d) CONVERTING TO TIME SERIES OBJECT\n")
cat(rep("-", 50), "\n\n")

cat("1. Creating time series object...\n")
us_pop_ts <- ts(us_pop_data$US_Population, 
                start = 1790, 
                frequency = 0.1)  # Once every 10 years

cat("2. Time series object details:\n")
cat("   - Start year:", start(us_pop_ts), "\n")
cat("   - End year:", end(us_pop_ts), "\n")
cat("   - Frequency:", frequency(us_pop_ts), "\n")
cat("   - Observations:", length(us_pop_ts), "\n\n")

cat("3. EXPLANATION OF SAMPLING FREQUENCY:\n")
cat("   - Frequency = 0.1 means data is collected once every 10 years\n")
cat("   - This is DECENNIAL data (every 10 years)\n")
cat("   - Data points: 1790, 1800, 1810, ..., 1990\n")
cat("   - Total of 21 observations over 200 years\n")
cat("   - No seasonal component (annual data would have frequency = 1)\n")
cat("   - No monthly patterns (monthly data would have frequency = 12)\n\n")

cat("4. Data summary statistics:\n")
cat("   - Minimum population:", format(min(us_pop_ts), big.mark = ","), "\n")
cat("   - Maximum population:", format(max(us_pop_ts), big.mark = ","), "\n")
cat("   - Mean population:", format(round(mean(us_pop_ts)), big.mark = ","), "\n")
cat("   - Median population:", format(median(us_pop_ts), big.mark = ","), "\n")

# ------------------------------------------------------------
# (e) Plot data and identify dominating components
# ------------------------------------------------------------
cat("\n\n(e) PLOTTING AND ANALYSIS\n")
cat(rep("-", 50), "\n\n")

cat("1. Plotting time series data...\n")
plot(us_pop_ts, 
     main = "US Population (1790-1990)",
     ylab = "Population (in millions)",
     xlab = "Year",
     col = "darkblue", 
     lwd = 2,
     type = "o",
     pch = 16)
grid()

cat("2. Performing basic time series decomposition...\n")
# For decennial data, we analyze trend component
time_index <- 1:length(us_pop_ts)
linear_model <- lm(us_pop_ts ~ time_index)
fitted_trend <- fitted(linear_model)

cat("3. Fitting linear trend model...\n")
cat("   Linear equation: Population = ", 
    format(round(coef(linear_model)[1]), big.mark = ","), 
    " + ", 
    format(round(coef(linear_model)[2]), big.mark = ","), 
    " × Year\n", sep="")

cat("4. Plotting with trend line...\n")
plot(us_pop_ts, 
     main = "US Population with Linear Trend (1790-1990)",
     ylab = "Population (millions)",
     xlab = "Year",
     col = "darkblue", 
     lwd = 2,
     type = "o",
     pch = 16)
lines(time_index*10 + 1780, fitted_trend, 
      col = "red", 
      lwd = 3,
      lty = 1)
legend("topleft", 
       legend = c("Original Data", "Linear Trend"), 
       col = c("darkblue", "red"), 
       lwd = c(2, 3), 
       lty = 1,
       bg = "white")
grid()

cat("\n5. DOMINATING COMPONENT IDENTIFICATION:\n")

# Calculate variance explained by trend
total_variance <- var(us_pop_ts)
trend_variance <- var(fitted_trend)
residual_variance <- var(residuals(linear_model))

cat("   - Total variance:", format(total_variance, scientific = FALSE, big.mark = ","), "\n")
cat("   - Trend variance:", format(trend_variance, scientific = FALSE, big.mark = ","), "\n")
cat("   - Residual variance:", format(residual_variance, scientific = FALSE, big.mark = ","), "\n\n")

cat("   PERCENTAGE OF VARIANCE EXPLAINED:\n")
cat("   - Trend:", round((trend_variance/total_variance)*100, 1), "%\n")
cat("   - Random/Residual:", round((residual_variance/total_variance)*100, 1), "%\n\n")

cat("   FINAL CONCLUSION:\n")
if (trend_variance > residual_variance) {
  cat("   ✅ DOMINATING COMPONENT: TREND\n")
  cat("   Reason: Trend explains", round((trend_variance/total_variance)*100, 1), "% of total variance\n")
} else {
  cat("   ✅ DOMINATING COMPONENT: RANDOM/RESIDUAL\n")
}

# ------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------
cat("\n\n", rep("=", 70), "\n", sep="")
cat("SUMMARY\n")
cat(rep("=", 70), "\n\n")

cat("KEY FINDINGS:\n")
cat("1. Data Import Success:\n")
cat("   - Successfully imported from Excel file\n")
cat("   - 21 observations of US population (1790-1990)\n\n")

cat("2. Time Series Characteristics:\n")
cat("   - Frequency: 0.1 (decennial data - every 10 years)\n")
cat("   - No seasonal patterns (not monthly/quarterly data)\n")
cat("   - Strong trend component visible\n\n")

cat("3. Statistical Analysis:\n")
cat("   - Population growth from 3.9M to 248.7M over 200 years\n")
cat("   - Average growth rate: ~", format(round(coef(linear_model)[2]), big.mark = ","), " per decade\n", sep="")
cat("   - Trend explains ", round((trend_variance/total_variance)*100, 1), "% of variance\n", sep="")
cat("   - Minimal random variation\n\n")

cat("4. Visual Observations:\n")
cat("   - Exponential-like growth pattern\n")
cat("   - Acceleration in growth rate over time\n")
cat("   - Consistent upward trend with minor fluctuations\n")
cat(rep("=", 70), "\n")