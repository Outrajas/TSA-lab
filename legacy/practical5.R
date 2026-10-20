# ============================================
# PRACTICAL-5: Alternative - Separate Plots like PDF
# ============================================

# (a) Import data
bank_data <- scan("C:/Users/maths/Desktop/tsalab_ojas/lab3/bank_case.txt")

# (b) Convert to time series
bank_ts <- ts(bank_data)

# (c) PLOT 1: Time series plot (as in PDF page 2)
dev.new()  # Open new window for first plot
plot(bank_ts, 
     main = "Commercial Bank Real Estate Loan Data", 
     xaxt = "n", 
     xlab = "Months", 
     ylab = "Loans in billions of dollars", 
     lwd = 2)
axis(1, seq(0, 70, 5), seq(0, 70, 5))

# (d) PLOT 2: ACF plot (as in PDF page 2-3)
dev.new()  # Open new window for second plot
acf(bank_ts, 
    main = "ACF of Commercial Bank Real Estate Loan Data", 
    lag.max = 10, 
    ylim = c(-1, 1))

# (d) PLOT 3: PACF plot (as in PDF page 3)
dev.new()  # Open new window for third plot
pacf(bank_ts, 
     main = "PACF of Commercial Bank Real Estate Loan Data", 
     lag.max = 10, 
     ylim = c(-1, 1))

# (e) ADF Test
library(tseries)
adf_test_result <- adf.test(bank_ts)
cat("\nAugmented Dickey-Fuller Test\n")
cat("=============================\n")
cat("data: bank_data\n")
cat("Dickey-Fuller =", round(adf_test_result$statistic, 5), 
    ", Lag order =", adf_test_result$parameter, 
    ", p-value =", adf_test_result$p.value, "\n")
cat("alternative hypothesis: stationary\n")