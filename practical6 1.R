# ============================================
# PRACTICAL-6: AirPassengers Data Analysis
# ============================================

rm(list = ls(all = TRUE))

library(tseries)
library(forecast)
library(TTR)
library(lmtest)

# (a) Load and convert data
data("AirPassengers")
AirPassengers_ts <- AirPassengers

# Print data
print("AirPassengers Dataset:")
print(AirPassengers)

# (b) PLOT 1: Time series plot
dev.new()
plot(AirPassengers_ts,
     main = "Monthly Airline Passengers Data",
     xlab = "Time",
     ylab = "Passengers",
     col = "darkred",
     lwd = 2)

# (c) Decompose and plot
AirPassengers_decomposed <- stl(AirPassengers_ts, s.window = "periodic")

# PLOT 2: Decomposition plot
dev.new()
plot(AirPassengers_decomposed, 
     main = "Decomposition of AirPassengers Data")

# (d) PLOT 3 & 4: ACF and PACF plots
dev.new()
par(mfrow = c(1, 2))
acf(AirPassengers_ts, 
    main = "ACF of AirPassengers Data",
    lag.max = 20,
    ylim = c(-1, 1))

pacf(AirPassengers_ts, 
     main = "PACF of AirPassengers Data",
     lag.max = 20,
     ylim = c(-1, 1))
par(mfrow = c(1, 1))

# (e) Statistical tests
adf_air_result <- adf.test(AirPassengers_ts)
cat("\nAugmented Dickey-Fuller Test\n")
cat("=============================\n")
cat("data: AirPassengers\n")
cat("Dickey-Fuller =", round(adf_air_result$statistic, 5), 
    ", Lag order =", adf_air_result$parameter, 
    ", p-value =", adf_air_result$p.value, "\n")
cat("alternative hypothesis: stationary\n\n")

kpss_result <- kpss.test(AirPassengers_ts)
cat("KPSS Test for Level Stationarity\n")
cat("================================\n")
cat("data: AirPassengers_ts\n")
cat("KPSS Level =", kpss_result$statistic, 
    ", Truncation lag parameter =", kpss_result$parameter, 
    ", p-value =", kpss_result$p.value, "\n")