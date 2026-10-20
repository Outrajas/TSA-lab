# Method 2: Create PDF directly without rmarkdown
setwd("C:/Users/maths/Desktop/tsalab_ojas/lab2")

# Start PDF device
pdf("practical3_output.pdf", width = 11, height = 8.5, paper = "a4")

# Load libraries
library(ggplot2)
library(forecast)
library(ggfortify)

# Execute your code
cat("\n===== PRACTICAL 3 OUTPUT =====\n")
cat("Date:", format(Sys.Date(), "%Y-%m-%d"), "\n\n")

# Your original code goes here - copy and paste it:
# Step 1: Set working directory
setwd("C:/Users/maths/Desktop/tsalab_ojas/lab2")

# Step 2: Load packages
library(ggplot2)
library(forecast)
library(ggfortify)

# Step 3: Nottem Data Analysis
cat("\n===== NOTTEM DATA ANALYSIS =====\n")

# Load Nottem dataset
data("nottem")
cat("Nottem dataset loaded\n")
cat("Time period:", start(nottem), "to", end(nottem), "\n")
cat("Frequency:", frequency(nottem), "\n")
cat("Observations:", length(nottem), "\n\n")

# Plot original data
plot(nottem, 
     main = "Nottem Dataset: Monthly Temperatures (1920-1939)",
     ylab = "Temperature (F)", 
     col = "blue", 
     lwd = 2)

# Decompose using classical decomposition
nottem_decomp <- decompose(nottem)

# Plot decomposition
plot(nottem_decomp, 
     col = "darkblue", 
     lwd = 1.5)

# Check component dominance
nottem_var <- var(nottem, na.rm = TRUE)
trend_var <- var(nottem_decomp$trend, na.rm = TRUE)
seasonal_var <- var(nottem_decomp$seasonal, na.rm = TRUE)
residual_var <- var(nottem_decomp$random, na.rm = TRUE)

cat("Variance Analysis for Nottem data:\n")
cat("  Total variance:", round(nottem_var, 3), "\n")
cat("  Trend variance:", round(trend_var, 3), "\n")
cat("  Seasonal variance:", round(seasonal_var, 3), "\n")
cat("  Residual variance:", round(residual_var, 3), "\n\n")

cat("Percentage of variance explained:\n")
cat("  Trend:", round((trend_var/nottem_var)*100, 1), "%\n")
cat("  Seasonal:", round((seasonal_var/nottem_var)*100, 1), "%\n")
cat("  Residual:", round((residual_var/nottem_var)*100, 1), "%\n\n")

# Determine dominating component
if (seasonal_var > trend_var && seasonal_var > residual_var) {
  cat("DOMINATING COMPONENT: SEASONAL (Strong seasonal patterns in temperature)\n")
} else if (trend_var > seasonal_var && trend_var > residual_var) {
  cat("DOMINATING COMPONENT: TREND\n")
} else {
  cat("DOMINATING COMPONENT: RANDOM/RESIDUAL\n")
}

# Step 4: AirPassengers Data Analysis
cat("\n===== AIRPASSENGERS DATA ANALYSIS =====\n")

# Load AirPassengers dataset
data("AirPassengers")
cat("AirPassengers dataset loaded\n")
cat("Time period:", start(AirPassengers), "to", end(AirPassengers), "\n")
cat("Frequency:", frequency(AirPassengers), "\n")
cat("Observations:", length(AirPassengers), "\n\n")

# Plot original data
plot(AirPassengers, 
     main = "AirPassengers Dataset: Monthly Totals (1949-1960)",
     ylab = "Passengers (in thousands)", 
     col = "darkred", 
     lwd = 2)

# Decompose using classical decomposition
air_decomp <- decompose(AirPassengers)

# Plot decomposition
plot(air_decomp, 
     col = "darkred", 
     lwd = 1.5)

# Alternative decomposition using STL
air_stl <- stl(AirPassengers, s.window = "periodic")

# Plot STL decomposition
plot(air_stl, 
     main = "STL Decomposition of AirPassengers Data")

# Check component dominance
air_var <- var(AirPassengers, na.rm = TRUE)
air_trend_var <- var(air_decomp$trend, na.rm = TRUE)
air_seasonal_var <- var(air_decomp$seasonal, na.rm = TRUE)
air_residual_var <- var(air_decomp$random, na.rm = TRUE)

cat("Variance Analysis for AirPassengers data:\n")
cat("  Total variance:", round(air_var, 3), "\n")
cat("  Trend variance:", round(air_trend_var, 3), "\n")
cat("  Seasonal variance:", round(air_seasonal_var, 3), "\n")
cat("  Residual variance:", round(air_residual_var, 3), "\n\n")

cat("Percentage of variance explained:\n")
cat("  Trend:", round((air_trend_var/air_var)*100, 1), "%\n")
cat("  Seasonal:", round((air_seasonal_var/air_var)*100, 1), "%\n")
cat("  Residual:", round((air_residual_var/air_var)*100, 1), "%\n\n")

# Determine dominating component
if (air_trend_var > air_seasonal_var && air_trend_var > air_residual_var) {
  cat("DOMINATING COMPONENT: TREND (Strong upward trend in passenger numbers)\n")
} else if (air_seasonal_var > air_trend_var && air_seasonal_var > air_residual_var) {
  cat("DOMINATING COMPONENT: SEASONAL\n")
} else {
  cat("DOMINATING COMPONENT: RANDOM/RESIDUAL\n")
}

# Summary comparison
cat("\n===== SUMMARY COMPARISON =====\n")
cat("Nottem: Strong SEASONAL component (temperature patterns)\n")
cat("AirPassengers: Strong TREND component (growth in air travel)\n")

# Close PDF
dev.off()

cat("PDF created: practical3_output.pdf\n")