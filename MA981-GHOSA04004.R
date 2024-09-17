# Install and load necessary libraries
install.packages(c("tidyverse","xts", "lubridate", "forecast", "prophet", "ggplot2", "imputeTS"))
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)
library(tseries)
library(xts)
library(lubridate)


data<- read.csv("C:/Users/Dell/Downloads/manjiri/energy_dataset.csv")
data

# Step 2: Exploratory Data Analysis (EDA)
# Info of the dataset
str(data)

# Describe the dataset
summary(data)

# Checking for missing values
is_null <- sapply(data, function(x) sum(is.na(x)))
print("Missing values per column:")
print(is_null)

# Length of the dataset
length_of_dataset <- nrow(data)
print(paste("Length of the dataset:", length_of_dataset))

# Shape of the dataset
shape_of_dataset <- dim(data)
print(paste("Shape of the dataset: Rows:", shape_of_dataset[1], "Columns:", shape_of_dataset[2]))

# Columns of the dataset
columns_of_dataset <- colnames(data)
print("Columns of the dataset:")
print(columns_of_dataset)

# Data types of the dataset
data_types <- sapply(data, class)
print("Data types of the dataset:")
print(data_types)

# Number of unique values per column
num_unique_values <- sapply(data, function(x) length(unique(x)))
print("Number of unique values per column:")
print(num_unique_values)

# Check for duplicate rows
duplicate_rows <- sum(duplicated(data))
print(paste("Number of duplicate rows:", duplicate_rows))

# Correlation matrix of the dataset
numeric_data <- data[sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data, use = "complete.obs")

print("Correlation matrix:")
print(correlation_matrix)

# Convert correlation matrix to a long format data frame
correlation_long <- as.data.frame(as.table(correlation_matrix))

# Visualize the correlation matrix
ggplot(data = correlation_long, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  coord_fixed()

# Step 3: Data Preprocessing

# Handling missing data by imputing with mean (you can choose other methods as well)
data_imputed <- data %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
data_imputed

# Convert the time column to a proper datetime format
data_imputed$time <- as.POSIXct(data_imputed$time, format="%Y-%m-%d %H:%M:%S")

# Print column names to verify
print(colnames(data_imputed))

# Step 4: Time Series Data Preparation
ts_data <- data_imputed
ts_data$time <- as.POSIXct(ts_data$time, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
na_time <- sum(is.na(ts_data$time))
if (na_time > 0) {
  cat("Number of NA values in the time column:", na_time, "\n")
}
ts_data <- ts_data[!is.na(ts_data$time), ]
ts_data_xts <- xts(ts_data[ , -which(names(ts_data) == "time")], order.by = ts_data$time)
print(head(ts_data_xts))

# Step 5: Data Visualization
# Ensure the time series data is in xts format
ts_data_xts <- xts(ts_data[, -which(names(ts_data) == "time")], order.by = ts_data$time)

# Plot the individual time series
plot(ts_data_xts$total.load.actual, main="Total Load Actual Over Time", ylab="Total Load", xlab="Time")
plot(ts_data_xts$generation.solar, main="Solar Generation Over Time", ylab="Solar Generation", xlab="Time")
plot(ts_data_xts$generation.wind.onshore, main="Wind Onshore Generation Over Time", ylab="Wind Onshore Generation", xlab="Time")
plot(ts_data_xts$price.actual, main="Actual Price Over Time", ylab="Price", xlab="Time")

# Combine multiple time series into one plot using ggplot2 and ggfortify
autoplot(ts_data_xts) +
  labs(title = "Multiple Time Series Visualization", x = "Time", y = "Values") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green", "purple"))

# Step 6: Time Series Analysis
# Convert 'total.load.actual' to a numeric vector (if it's not already)
total_load_actual_numeric <- as.numeric(ts_data_xts$total.load.actual)

# Ensure time series data is in ts format
# Adjust frequency based on your data's periodicity (e.g., 365 for daily data with yearly seasonality)
ts_total_load_actual <- ts(total_load_actual_numeric, frequency = 365)

# Decompose the time series using stl
decomposed_ts <- stl(ts_total_load_actual, s.window = "periodic")

# Plot the decomposed time series
plot(decomposed_ts)

# Model 1: ARIMA Model

# Ensure 'total_load_actual' is numeric and handle missing values
total_load_actual_numeric <- as.numeric(ts_data_xts$total.load.actual)
total_load_actual_numeric <- na.omit(total_load_actual_numeric)

# Convert to time series object
ts_total_load_actual <- ts(total_load_actual_numeric, frequency = 365)

# Check if the time series is stationary
adf_result <- adf.test(ts_total_load_actual)
print(adf_result)

# Differencing to make the series stationary
diff_ts <- diff(ts_total_load_actual)
adf_result_diff <- adf.test(diff_ts)
print(adf_result_diff)

# Fit ARIMA model
fit_arima <- auto.arima(ts_total_load_actual)
summary(fit_arima)

# Forecasting with ARIMA
forecast_arima <- forecast(fit_arima, h = 30)
autoplot(forecast_arima) +
  labs(title = "ARIMA Forecast", x = "Time", y = "Total Load")


# Model 2: Exponential Smoothing (ETS)

# Ensure 'total_load_actual' is numeric and handle missing values
total_load_actual_numeric <- as.numeric(ts_data_xts$total.load.actual)
total_load_actual_numeric <- na.omit(total_load_actual_numeric)

# Convert to time series object
ts_total_load_actual <- ts(total_load_actual_numeric, frequency = 365)

# Fit ETS model
fit_ets <- ets(ts_total_load_actual)
summary(fit_ets)

# Forecasting with ETS
forecast_ets <- forecast(fit_ets, h = 30)
autoplot(forecast_ets) +
  labs(title = "ETS Forecast", x = "Time", y = "Total Load")

# Step 7: Evaluation of Models
# Plot the time series and forecasts
autoplot(ts_total_load_actual, series = "Actual") +
  autolayer(forecast_arima$mean, series = "ARIMA Forecast", PI = FALSE) +
  autolayer(forecast_ets$mean, series = "ETS Forecast", PI = FALSE) +
  labs(title = "Comparison of ARIMA and ETS Forecasts", x = "Time", y = "Total Load") +
  guides(colour = guide_legend(title = "Forecast")) +
  theme_minimal()