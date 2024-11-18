# Load Libraries
install.packages("tidyverse")
install.packages("zoo")
install.packages("lubridate")
install.packages("tseries")
install.packages("vars")
library(tidyverse)
library(zoo)
library(lubridate)
library(readr)
library(ggplot2)
library(dplyr)
library(tseries)
library(vars)

# Import Data
ngdp_data <- read.csv("Data/GDP.csv")
DFF_data <- read.csv("Data/DFF.csv")
UNRATE_data <- read.csv("Data/UNRATE.csv")
BOGMBASE_data <- read_csv("Data/BOGMBASE.csv")
M2SL_data <- read.csv("Data/M2SL.csv")
M1SL_data <- read.csv("Data/M1SL.csv")
PCEPI_data <- read.csv("Data/PCEPI.csv")

# Date format
ngdp_data$DATE <- as.Date(ngdp_data$DATE, format="%Y-%m-%d")
DFF_data$DATE <- as.Date(DFF_data$DATE, format="%Y-%m-%d")
UNRATE_data$DATE <- as.Date(UNRATE_data$DATE, format="%Y-%m-%d")
BOGMBASE_data$DATE <- as.Date(BOGMBASE_data$DATE, format="%Y-%m-%d")
M2SL_data$DATE <- as.Date(M2SL_data$DATE, format="%Y-%m-%d")
M1SL_data$DATE <- as.Date(M1SL_data$DATE, format="%Y-%m-%d")
PCEPI_data$DATE <- as.Date(PCEPI_data$DATE, format="%Y-%m-%d")

# Convert all data to the same frequency (to quarterly)
DFF_quarterly <- DFF_data %>%
  mutate(quarter = floor_date(DATE, "quarter")) %>%
  group_by(quarter) %>%
  summarize(DFF = mean(DFF, na.rm = TRUE))

UNRATE_quarterly <- UNRATE_data %>%
  mutate(quarter = floor_date(DATE, "quarter")) %>%
  group_by(quarter) %>%
  summarize(UNRATE = mean(UNRATE, na.rm = TRUE))

BOGMBASE_quarterly <- BOGMBASE_data %>%
  mutate(quarter = floor_date(DATE, "quarter")) %>%
  group_by(quarter) %>%
  summarize(BOGMBASE = first(BOGMBASE, order_by = DATE))

M2SL_quarterly <- M2SL_data %>%
  mutate(quarter = floor_date(DATE, "quarter")) %>%
  group_by(quarter) %>%
  summarize(M2SL = first(M2SL, order_by = DATE))

M1SL_quarterly <- M1SL_data %>%
  mutate(quarter = floor_date(DATE, "quarter")) %>%
  group_by(quarter) %>%
  summarize(M1SL = first(M1SL, order_by = DATE))

# Rename quarter to DATE
DFF_quarterly <- DFF_quarterly %>% rename(DATE = quarter)
UNRATE_quarterly <- UNRATE_quarterly %>% rename(DATE = quarter)
BOGMBASE_quarterly <- BOGMBASE_quarterly %>% rename(DATE = quarter)
M2SL_quarterly <- M2SL_quarterly %>% rename(DATE = quarter)
M1SL_quarterly <- M1SL_quarterly %>% rename(DATE = quarter)

# Combine data
combined_data <- ngdp_data %>%
  left_join(DFF_quarterly, by = "DATE") %>%
  left_join(UNRATE_quarterly, by = "DATE") %>%
  left_join(BOGMBASE_quarterly, by = "DATE") %>%
  left_join(M2SL_quarterly, by = "DATE") %>%
  left_join(M1SL_quarterly, by = "DATE") %>%
  left_join(PCEPI_data, by = "DATE")

# Remove incomplete rows
combined_data_complete <- combined_data[complete.cases(combined_data), ]

# Keep Data from 2000
combined_data_complete <- combined_data_complete[-c(1:159), ]

# ggplot2
ggplot(combined_data_complete, aes(x = DATE)) +
  geom_line(aes(y = GDP), color = "blue") +
  geom_line(aes(y = BOGMBASE), color = "purple") +
  geom_line(aes(y = M2SL), color = "orange") +
  geom_line(aes(y = M1SL), color = "yellow") +
  labs(title = "Time Series Plots of Key Economic Variables")

ggplot(combined_data_complete, aes(x = DATE)) +
  geom_line(aes(y = DFF), color = "red") +
  geom_line(aes(y = UNRATE), color = "green") +
  geom_line(aes(y = PCEPI_PC1), color = "black") +
  labs(title = "Time Series Plots of Key Economic Variables")

# Augmented Dickey-Fuller (ADF) test
adf.test(combined_data_complete$GDP)
adf.test(combined_data_complete$DFF)
adf.test(combined_data_complete$UNRATE)
adf.test(combined_data_complete$BOGMBASE)
adf.test(combined_data_complete$M2SL)
adf.test(combined_data_complete$M1SL)
adf.test(combined_data_complete$PCEPI_PC1)

# Difference the NGDP data
diff_GDP <- diff(combined_data_complete$GDP, differences = 1)

# Difference the DFF data
diff_DFF <- diff(combined_data_complete$DFF, differences = 1)

# Difference the UNRATE data
diff_UNRATE <- diff(combined_data_complete$UNRATE, differences = 1)

# Polynomial Detrending (M2SL data)
# Create a time index
time_index_M2SL <- 1:length(combined_data_complete$M2SL)

# Fit a polynomial trend model
poly_detrend_M2SL <- lm(combined_data_complete$M2SL ~ poly(time_index_M2SL, degree = 2))
detrended_M2SL <- residuals(poly_detrend_M2SL)

###############
# Plot original data
plot(time_index_M2SL, combined_data_complete$M2SL, type = "l", col = "blue", lwd = 2, 
     ylab = "M2SL", xlab = "Time", main = "Polynomial Detrending")

# Add polynomial trend line
lines(time_index_M2SL, fitted(poly_detrend_M2SL), col = "red", lwd = 2, lty = 2)

# Create a separate plot for detrended data
plot(time_index_M2SL, detrended_M2SL, type = "l", col = "green", lwd = 2, 
     ylab = "Detrended M2SL", xlab = "Time", main = "Detrended Data")

# Assign the detrended data to M2SL
combined_data_complete$diff_M2SL <- detrended_M2SL

# Difference the M1SL data
diff_M1SL <- diff(combined_data_complete$M1SL, differences = 1)

# Difference the PCEPI_PC1 data
diff_PCEPI_PC1 <- diff(combined_data_complete$PCEPI_PC1, differences = 1)

# Remove the first row of the original dataframe
combined_data_complete <- combined_data_complete[-1, ]

# Assign the differenced series to the dataframe
combined_data_complete$diff_GDP <- diff_GDP
combined_data_complete$diff_DFF <- diff_DFF
combined_data_complete$diff_UNRATE <- diff_UNRATE
combined_data_complete$diff_BOGMBASE <- combined_data_complete$BOGMBASE
combined_data_complete$diff_M1SL <- diff_M1SL
combined_data_complete$diff_PCEPI_PC1 <- diff_PCEPI_PC1

# Check stationarity again
adf.test(combined_data_complete$diff_GDP)
adf.test(combined_data_complete$diff_DFF)
adf.test(combined_data_complete$diff_UNRATE)
adf.test(combined_data_complete$diff_M2SL)
adf.test(combined_data_complete$diff_M1SL)
adf.test(combined_data_complete$diff_PCEPI_PC1)

# VAR
# Selecting optimal lag length for VAR model
lag_selection <- VARselect(combined_data_complete[, c("diff_GDP", "diff_DFF", "diff_UNRATE", "diff_BOGMBASE", "diff_M2SL", "diff_M1SL", "diff_PCEPI_PC1")], lag.max = 6, type = "const")

# Displaying the selection criteria
lag_selection$criteria
optimal_lag <- lag_selection$selection["AIC(n)"]

# Fitting the VAR Model
var_model <- VAR(combined_data_complete[, c("diff_GDP", "diff_DFF", "diff_UNRATE", "diff_BOGMBASE", "diff_M2SL", "diff_M1SL", "diff_PCEPI_PC1")], p = optimal_lag, type = "const")

# Diagnostics
# Residual analysis
serial.test(var_model, lags.pt = 15, type = "PT.asymptotic")

# Stability Check
stability_check <- roots(var_model)
roots(var_model)

# Scenario Adjustments and Generating the forecast
n_steps <- 8
scenario_adjustment <- data.frame(
  diff_GDP = rep(0, n_steps),  
  diff_DFF = rep(10.2, n_steps), 
  diff_UNRATE = rep(10, n_steps),  
  diff_BOGMBASE = rep(90, n_steps),
  diff_M2SL = rep(0, n_steps),
  diff_M1SL = rep(0, n_steps),
  diff_PCEPI_PC1 = rep(0, n_steps)
)

# Forecasts
baseline_forecast <- predict(var_model, n.ahead = n_steps)
baseline_diff_GDP <- baseline_forecast$fcst$diff_GDP[, "fcst"]

scenario_diff_GDP <- baseline_diff_GDP + scenario_adjustment$diff_DFF + scenario_adjustment$diff_UNRATE + scenario_adjustment$diff_GDP + scenario_adjustment$diff_BOGMBASE + scenario_adjustment$diff_M2SL + scenario_adjustment$diff_M1SL + scenario_adjustment$diff_PCEPI_PC1

print(scenario_diff_GDP)
print(baseline_diff_GDP)

# Compare scenario forecast with baseline
plot(1:n_steps, scenario_diff_GDP, type = "l", col = "red", xlab = "Steps", ylab = "NGDP", main = "Scenario Analysis")
lines(1:n_steps, baseline_diff_GDP, col = "blue")
legend("topright", legend = c("Scenario", "Baseline"), col = c("red", "blue"), lty = 1)
