install.packages("stringr")
install.packages("ace_tools")
install.packages("tseries")
install.packages("forecast")
install.packages("car")
install.packages("dplyr")
install.packages("tseries")
install.packages("Metrics") 
install.packages("caret")    
install.packages("ggplot2")
install.packages("lmtest")
install.packages("MLmetrics")
install.packages("tidyr")
install.packages("tidyverse")


library(MLmetrics)
library(Metrics)
library(caret)
library(ggplot2)
library(lmtest)
library(stringr)
library(readxl)
library(dplyr)
library(lubridate)
library(ace_tools)
library(ggplot2)
library(zoo)
library(tseries)
library(forecast)
library(corrplot)
library(tseries)
library(car)
library(xgboost)
library(prophet)
library(tidyr)
library(tidyverse)
library(prophet)

## Paths and reading to data
bitcoin_data_path <- "/Users/onur/Desktop/UNIPD_NOTES/Bussiness and Financial Data/Project/Usefull/isoformat-2017.xlsx"
google_trends_path <- "/Users/onur/Desktop/UNIPD_NOTES/Bussiness and Financial Data/Project/Usefull/btc_search.xlsx"
onchain_data_path <- "/Users/onur/Desktop/UNIPD_NOTES/Bussiness and Financial Data/Project/Usefull/btc_onchain.xlsx"
bitcoin_data <- read_excel(bitcoin_data_path)
google_trends <- read_excel(google_trends_path)
onchain_data <- read_excel(onchain_data_path)

# Necessary adjustments such as converting date into date format, changing column names, char columns into numeric etc.
str(bitcoin_data)
bitcoin_data <- bitcoin_data %>%
  mutate(time = as.Date(time))

str(google_trends)
colnames(google_trends) <- c("Date", "TrendScore")

google_trends <- google_trends %>%
  mutate(Date = as.Date(paste0(Date, "-01")),
         TrendScore = as.numeric(TrendScore))

onchain_data <- onchain_data %>%
  mutate(Date = as.Date(Date))
char_cols <- sapply(onchain_data, is.character) 
onchain_data[, char_cols] <- lapply(onchain_data[, char_cols], function(x) as.numeric(str_replace_all(x, ",", ".")))

google_trends <- google_trends[-c(1,2), ] 

sum(is.na(bitcoin_data))
sum(is.na(google_trends))
sum(is.na(onchain_data))

# Dealing with missing values 
total_missing <- sum(is.na(onchain_data))
total_values <- prod(dim(onchain_data))

# Bitcoin prices are highly volatile therefore missing values are dropped
missing_percentage <- (total_missing / total_values) * 100
cat("Total Missing Values:", total_missing, "\n")
cat("Percentage of Missing Data in onchain_data:", round(missing_percentage, 2), "%\n")

# Since bitcoin is a highly volatile and unpredictable we dropped missing values since they are only 4.1% of total data
onchain_data_clean <- na.omit(onchain_data)
sum(is.na(onchain_data_clean))

cat("Original rows:", nrow(onchain_data), "\n")
cat("Rows after removing missing values:", nrow(onchain_data_clean), "\n")

# Plotting search trends and price data based on close values 
# Btc close prices
ggplot(bitcoin_monthly, aes(x = YearMonth, y = Close)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Bitcoin Closing Price Over Time",
       x = "Year-Month",
       y = "Close Price (USD)") +
  theme_minimal()

# Google search trends 
ggplot(google_trends, aes(x = YearMonth, y = TrendScore)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Google Search Trends for Bitcoin Over Time",
       x = "Year-Month",
       y = "Search Trend Score") +
  theme_minimal()

# Before merging google trends with bitcoin price, make sure the same format 
google_trends <- google_trends %>%
  rename(YearMonth = YearMonth) %>%  
  mutate(YearMonth = as.Date(as.yearmon(YearMonth, "%Y-%m")))

bitcoin_monthly <- bitcoin_monthly %>%
  mutate(YearMonth = as.Date(YearMonth))

plot_data <- bitcoin_monthly %>%
  left_join(google_trends, by = "YearMonth")

str(plot_data)
sum(is.na(plot_data))

# To plot search trends and bitcoin price, we need to normalize both since their scale is different 

plot_data <- plot_data %>%
  mutate(
    Close_Scaled = (Close - min(Close, na.rm = TRUE)) / (max(Close, na.rm = TRUE) - min(Close, na.rm = TRUE)),
    TrendScore_Scaled = (TrendScore - min(TrendScore, na.rm = TRUE)) / (max(TrendScore, na.rm = TRUE) - min(TrendScore, na.rm = TRUE))
  )

# Create bitcoin_monthly again 

# Convert 'time' column to Date format
bitcoin_data <- bitcoin_data %>%
  mutate(time = as.Date(time))

# Aggregate to Monthly Level
bitcoin_monthly <- bitcoin_data %>%
  mutate(YearMonth = as.yearmon(time)) %>%
  group_by(YearMonth) %>%
  summarize(
    Open = first(open),  
    High = max(high, na.rm = TRUE), 
    Low = min(low, na.rm = TRUE), 
    Close = last(close),  
    Volume = sum(Volume, na.rm = TRUE)  # Total trading volume
  ) %>%
  ungroup() %>%
  mutate(YearMonth = as.Date(as.yearmon(YearMonth, "%Y-%m-%d")))

str(bitcoin_monthly)

# Boxplots 

# Based on the daily graph 
bitcoin_long_daily <- bitcoin_data %>%
  pivot_longer(cols = c(open, high, low, close, Volume), 
               names_to = "Variable", 
               values_to = "Value")

ggplot(bitcoin_long_daily, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Outliers in Daily Bitcoin Price and Volume Data",
       x = "Variable",
       y = "Value") +
  theme_minimal()

# Google boxplot
ggplot(google_trends, aes(x = "", y = TrendScore)) +  # Empty x-axis for single variable boxplot
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Outliers in Google Search Trends for Bitcoin",
       x = "Google Search Trends",
       y = "Trend Score") +
  theme_minimal()

# Log transformations 

bitcoin_data <- bitcoin_data %>%
  mutate(log_Volume = log(Volume + 1))

bitcoin_monthly <- bitcoin_monthly %>%
  mutate(log_Volume = log(Volume + 1))

# Log transformation and monthly aggregation resulted in less outliers 

# Daily btc volume boxplot 

ggplot(bitcoin_data, aes(x = "", y = log_Volume)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of Log-Transformed Bitcoin Trading Volume (Daily)",
       x = "Bitcoin Data",
       y = "Log(Volume)") +
  theme_minimal()

# Monhtly btc volume boxtplot

ggplot(bitcoin_monthly, aes(x = "", y = log_Volume)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of Log-Transformed Bitcoin Trading Volume (Monthly)",
       x = "Bitcoin Monthly Data",
       y = "Log(Volume)") +
  theme_minimal()

# To check if any na values 
sum(is.na(onchain_data_clean$DiffMean))
sum(is.na(onchain_data_clean$FeeTotUSD))
sum(is.na(onchain_data_clean$HashRate))
sum(is.na(onchain_data_clean$TxCnt))

# Boxplot some features of onchain data
onchain_long <- onchain_data_clean %>%
  select(DiffMean, FeeTotUSD, HashRate, TxCnt) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(onchain_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Outliers in On-Chain Data (DiffMean, FeeTotUSD, HashRate, TxCnt)",
       x = "Variable",
       y = "Value") +
  theme_minimal()

# Stationary test on btc closing price (adf)
adf_result <- adf.test(bitcoin_monthly$Close, alternative = "stationary")
print(adf_result)

close_ts <- ts(bitcoin_monthly$Close, start = c(year(min(bitcoin_monthly$YearMonth)), month(min(bitcoin_monthly$YearMonth))), frequency = 12)

# Multiplicative decomposition
decomposed_close <- decompose(close_ts, type = "multiplicative")

# Seperating components
seasonality <- decomposed_close$seasonal
trend <- decomposed_close$trend
residual <- decomposed_close$random

# To save components in the dataset
bitcoin_monthly <- bitcoin_monthly %>%
  mutate(
    Seasonal = as.numeric(seasonality),
    Trend = as.numeric(trend),
    Residual = as.numeric(residual)
  )

plot(decomposed_close)

#Check na values after decomposition
sum(is.na(bitcoin_monthly$Trend))
sum(is.na(bitcoin_monthly$Residual))

# Plot residual

ggplot(bitcoin_monthly, aes(x = YearMonth, y = Residual)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Residual Component Over Time",
       x = "Year-Month",
       y = "Residual") +
  theme_minimal()

# ACF - Pacf of residual 
par(mfrow = c(1,2))
acf(na.omit(bitcoin_monthly$Residual), main = "ACF of Residuals")
pacf(na.omit(bitcoin_monthly$Residual), main = "PACF of Residuals")
par(mfrow = c(1,1))

# First order differencing to residual and adf test
merged_data <- merged_data[!is.na(merged_data$Residual), ]
merged_data$Diff_Residual <- c(NA, diff(merged_data$Residual))
merged_data <- merged_data[!is.na(merged_data$Diff_Residual), ]

adf_test_result <- adf.test(merged_data$Diff_Residual, alternative = "stationary")
print(adf_test_result)

# Log transformation to residual
adf_log_residual <- adf.test(na.omit(bitcoin_monthly$log_Residual), alternative = "stationary")
print(adf_log_residual)

# z-score normalization and plotting
bitcoin_monthly <- bitcoin_monthly %>%
  mutate(Deseasonalized_Price = Trend + Residual)

bitcoin_monthly <- bitcoin_monthly %>%
  mutate(Normalized_Deseasonalized_Price = scale(Deseasonalized_Price))

summary(bitcoin_monthly$Normalized_Deseasonalized_Price)

ggplot(bitcoin_monthly, aes(x = YearMonth, y = Deseasonalized_Price)) +
  geom_line(color = "blue") +
  labs(title = "Deseasonalized Bitcoin Price Over Time",
       x = "Year-Month",
       y = "Deseasonalized Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Merging datasets. Check time intervals 
min(bitcoin_monthly$YearMonth)
max(bitcoin_monthly$YearMonth)

min(google_trends$YearMonth)  
max(google_trends$YearMonth)

min(onchain_data_clean$Date)  
max(onchain_data_clean$Date) 

# Merge the data
bitcoin_monthly <- bitcoin_monthly %>%
  mutate(YearMonth = as.Date(YearMonth))
google_trends <- google_trends %>%
  mutate(YearMonth = as.Date(as.yearmon(YearMonth, "%Y-%m")))
onchain_data_monthly <- onchain_data_monthly %>%
  mutate(YearMonth = as.Date(as.yearmon(YearMonth, "%Y-%m-01")))

# Common date interval
min(bitcoin_monthly$YearMonth)
max(bitcoin_monthly$YearMonth)
min(google_trends$YearMonth)
max(google_trends$YearMonth)
min(onchain_data_monthly$YearMonth)
max(onchain_data_monthly$YearMonth)
start_date <- as.Date("2017-08-01")
end_date <- as.Date("2021-12-31")


# Merge 
merged_data <- data.frame(YearMonth = seq(from = start_date, to = end_date, by = "month"))
merged_data <- merged_data %>%
  left_join(bitcoin_monthly, by = "YearMonth")
merged_data <- merged_data %>%
  left_join(google_trends, by = "YearMonth")
merged_data <- merged_data %>%
  left_join(onchain_data_monthly, by = "YearMonth")

str(merged_data)



# Extract features

selected_features <- c(
  "YearMonth", "Close", "AdrActCnt", "TxCnt", "HashRate", "AdrBalUSD10KCnt",
  "TrendScore", "DiffMean", "FeeTotUSD", "Volume", "log_Volume",
  "AdrBal1in100KCnt", "AdrBal1in100MCnt", "AdrBal1in10BCnt", "AdrBal1in10KCnt", 
  "AdrBal1in10MCnt", "AdrBal1in1BCnt", "AdrBal1in1KCnt", "AdrBal1in1MCnt", "AdrBalCnt", 
  "AdrBalNtv0.001Cnt", "AdrBalNtv0.01Cnt", "AdrBalNtv0.1Cnt", "AdrBalNtv100Cnt", 
  "AdrBalNtv100KCnt", "AdrBalNtv10Cnt", "AdrBalNtv10KCnt", "AdrBalNtv1Cnt", 
  "AdrBalNtv1KCnt", "AdrBalNtv1MCnt", "AdrBalUSD100Cnt", "AdrBalUSD100KCnt", 
  "AdrBalUSD10Cnt", "AdrBalUSD10KCnt", "AdrBalUSD10MCnt", "AdrBalUSD1Cnt", 
  "AdrBalUSD1KCnt", "AdrBalUSD1MCnt","FeeMeanUSD"
)

selected_data <- merged_data %>% select(all_of(selected_features))
str(selected_data)
colSums(is.na(selected_data))

selected_data <- selected_data %>%
  mutate(log_DiffMean = log(DiffMean + 1))


# Add new feature diff_close--> difference between close prices of btc
bitcoin_monthly <- bitcoin_monthly %>%
  arrange(YearMonth) %>%
  mutate(diff_close = Close - lag(Close)) 
selected_data <- selected_data %>%
  left_join(bitcoin_monthly %>% select(YearMonth, diff_close), by = "YearMonth")


# Add new features; small-medium-large holders
selected_data <- selected_data %>%
  mutate(
    Large_Holders = AdrBalNtv1KCnt,    # Holders --> more than 1k BTC
    Small_Holders = AdrBalUSD1KCnt,    # Holders --> <= 1k USD worth of BTC
    Medium_Holders = AdrBalCnt - (AdrBalNtv1KCnt + AdrBalUSD1KCnt)  # Remaining
  )
colnames(selected_data)

# Plot holders over time
holders_long <- selected_data %>%
  select(YearMonth, Large_Holders, Medium_Holders, Small_Holders) %>%
  pivot_longer(cols = c(Large_Holders, Medium_Holders, Small_Holders),
               names_to = "Holder_Type",
               values_to = "Count")

ggplot(holders_long, aes(x = YearMonth, y = Count, color = Holder_Type)) +
  geom_line(size = 1) +
  labs(title = "Bitcoin Holder Distribution Over Time",
       x = "Year",
       y = "Number of Holders",
       color = "Holder Type") +
  theme_minimal()


#Correlation matrix using pearson

selected_features <- selected_data[, c("diff_close", "TrendScore", "log_Volume", 
                                       "TxCnt", "HashRate", "FeeMeanUSD", 
                                       "AdrActCnt", "AdrBal1in1KCnt", 
                                       "AdrBalNtv10Cnt", "AdrBalUSD10KCnt")]
cor_matrix <- cor(selected_features, use = "complete.obs", method = "pearson")

melted_cor_matrix <- melt(cor_matrix)
cor_matrix[upper.tri(cor_matrix)] <- NA
melted_cor_matrix <- melt(cor_matrix, na.rm = TRUE)

# Define custom color scale to match your image
custom_palette <- scale_fill_gradientn(colors = c("darkred", "white", "darkblue"),
                                       values = c(0, 0.5, 1),
                                       limits = c(-1, 1),
                                       name = "Correlation")

ggplot(melted_cor_matrix, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  custom_palette +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Half Correlation Matrix") +
  coord_fixed()


# Add new feature as diff_close, it was deleted
selected_data <- selected_data %>%
  arrange(YearMonth) %>%  # Ensure data is sorted by date
  mutate(diff_close = Close - lag(Close))

# Multicolinearity test (vif) 
vif_data <- selected_data %>% select(-YearMonth)
vif_data <- na.omit(vif_data)

vif_model <- lm(Close ~ ., data = vif_data)
vif_results <- vif(vif_model)

vif_df <- data.frame(Variable = names(vif_results), VIF = vif_results)
high_vif <- vif_df %>% filter(VIF > 5)
print(high_vif)

# Select variables for models 
selected_data_vif <- selected_data %>%
  select(YearMonth, Close, TxCnt, AdrActCnt, TrendScore,HashRate,log_Volume)

# Models

# 1)MLR  model 

mlr_data <- selected_data %>%
  select(YearMonth, Close, AdrActCnt, TxCnt, HashRate, AdrBalUSD10KCnt, TrendScore)
mlr_data$YearMonth <- as.Date(mlr_data$YearMonth)
mlr_data <- na.omit(mlr_data)
mlr_data <- mlr_data %>% arrange(YearMonth)

# Sliding window train-test-validaiton ratios 
train_ratio <- 0.7
test_ratio <- 0.2
validation_ratio <- 0.1

# indexes
total_rows <- nrow(mlr_data)
train_index <- round(total_rows * train_ratio)
test_index <- round(total_rows * (train_ratio + test_ratio))

# creating data for train-test-validation
train_data <- mlr_data[1:train_index, ]
test_data <- mlr_data[(train_index + 1):test_index, ]
validation_data <- mlr_data[(test_index + 1):total_rows, ]

# model formula 
mlr_model <- lm(Close ~ AdrActCnt + TxCnt + HashRate + AdrBalUSD10KCnt + TrendScore, data = train_data)

#Predicitons 
test_data$Predicted_Close <- predict(mlr_model, newdata = test_data)
validation_data$Predicted_Close <- predict(mlr_model, newdata = validation_data)
rmse_mlr_test <- sqrt(mean((test_data$Close - test_data$Predicted_Close)^2))
rmse_mlr_validation <- sqrt(mean((validation_data$Close - validation_data$Predicted_Close)^2))

# MAPE
mape_mlr_test <- mean(abs((test_data$Close - test_data$Predicted_Close) / test_data$Close)) * 100
mape_mlr_validation <- mean(abs((validation_data$Close - validation_data$Predicted_Close) / validation_data$Close)) * 100

# R² (R-squared)
r2_mlr_test <- cor(test_data$Close, test_data$Predicted_Close)^2
r2_mlr_validation <- cor(validation_data$Close, validation_data$Predicted_Close)^2

cat("MLR Model Performance:\n")
cat("Test RMSE:", round(rmse_mlr_test, 2), "\n")
cat("Validation RMSE:", round(rmse_mlr_validation, 2), "\n")
cat("Test MAPE:", round(mape_mlr_test, 2), "%\n")
cat("Validation MAPE:", round(mape_mlr_validation, 2), "%\n")
cat("Test R²:", round(r2_mlr_test, 4), "\n")
cat("Validation R²:", round(r2_mlr_validation, 4), "\n")


# 2) Auto Arima model
bitcoin_monthly <- bitcoin_monthly %>% drop_na(Residual, Trend, Seasonal)

train_size <- floor(0.8 * nrow(bitcoin_monthly))
train_data <- bitcoin_monthly[1:train_size, ]
test_data <- bitcoin_monthly[(train_size + 1):nrow(bitcoin_monthly), ]

residual_ts <- ts(train_data$Residual, frequency = 12)

best_arima_model <- auto.arima(residual_ts)
forecast_residuals <- forecast(best_arima_model, h = nrow(test_data), level = c(80, 95))

final_forecast <- data.frame(
  YearMonth = test_data$YearMonth,
  Predicted_Residual = forecast_residuals$mean,
  Predicted_Close = test_data$Trend + test_data$Seasonal + forecast_residuals$mean,
  Lower_80 = test_data$Trend + test_data$Seasonal + forecast_residuals$lower[,1],
  Upper_80 = test_data$Trend + test_data$Seasonal + forecast_residuals$upper[,1],
  Lower_95 = test_data$Trend + test_data$Seasonal + forecast_residuals$lower[,2],
  Upper_95 = test_data$Trend + test_data$Seasonal + forecast_residuals$upper[,2]
)

p1 <- ggplot() +
  # Historical data
  geom_line(data = bitcoin_monthly, aes(x = YearMonth, y = Close), color = "black", size = 1) +
  geom_line(data = test_data, aes(x = YearMonth, y = Close), color = "blue", size = 1) +
  geom_line(data = final_forecast, aes(x = YearMonth, y = Predicted_Close), color = "red", size = 1, linetype = "dashed") +
  
  geom_ribbon(data = final_forecast, aes(x = YearMonth, ymin = Lower_80, ymax = Upper_80), fill = "pink", alpha = 0.5) +
  geom_ribbon(data = final_forecast, aes(x = YearMonth, ymin = Lower_95, ymax = Upper_95), fill = "lightgray", alpha = 0.5) +
  
  # Labels & Titles
  labs(title = "Bitcoin Price Forecast (ARIMA on Residuals with Prediction Interval)",
       x = "Date",
       y = "Price (USD)") +
  
  theme_minimal() +
  
  # Legend
  scale_color_manual(name = "Legend",
                     values = c("black" = "black", "blue" = "blue", "red" = "red"),
                     labels = c("Historical Data", "Actual Price", "Predicted Price")) +
  theme(legend.position = "top")

p2 <- ggplot(final_forecast, aes(x = YearMonth, y = Predicted_Close)) +
  geom_line(data = test_data, aes(x = YearMonth, y = Close), color = "blue", size = 1) +
  geom_line(aes(y = Predicted_Close), color = "red", size = 1, linetype = "dashed") +
  
  geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80), fill = "pink", alpha = 0.5) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "lightgray", alpha = 0.5) +
  
  labs(title = "Bitcoin Price Prediction Interval",
       x = "Date",
       y = "Price (USD)") +
  
  theme_minimal() +
  theme(legend.position = "none")

print(p1)  
print(p2)

## 3) Grid Search Arima Model

residual_ts <- ts(train_data$Residual, frequency = 12)

# ARIMA (p,d,q)  (0,0,0) - (3,3,3)
best_aic <- Inf  
best_order <- c(0, 0, 0)

for (p in 0:3) {
  for (d in 0:3) {
    for (q in 0:3) {
      model_try <- tryCatch({
        fit <- arima(residual_ts, order = c(p, d, q), method = "ML")
        aic_value <- fit$aic
        if (aic_value < best_aic) {
          best_aic <- aic_value
          best_order <- c(p, d, q)
        }
      }, error = function(e) NULL)  # Skip fail models 
    }
  }
}

best_arima_model <- arima(residual_ts, order = best_order, method = "ML")
forecast_residuals <- forecast(best_arima_model, h = nrow(test_data), level = c(80, 95))
# Adding seasonality 
final_forecast <- data.frame(
  YearMonth = test_data$YearMonth,
  Predicted_Residual = forecast_residuals$mean,
  Predicted_Close = test_data$Trend + test_data$Seasonal + forecast_residuals$mean,
  Lower_80 = test_data$Trend + test_data$Seasonal + forecast_residuals$lower[,1],
  Upper_80 = test_data$Trend + test_data$Seasonal + forecast_residuals$upper[,1],
  Lower_95 = test_data$Trend + test_data$Seasonal + forecast_residuals$lower[,2],
  Upper_95 = test_data$Trend + test_data$Seasonal + forecast_residuals$upper[,2]
)

# Plotting historical price
p1 <- ggplot() +

  geom_line(data = bitcoin_monthly, aes(x = YearMonth, y = Close), color = "black", size = 1) +
  geom_line(data = test_data, aes(x = YearMonth, y = Close), color = "blue", size = 1) +
  
  geom_line(data = final_forecast, aes(x = YearMonth, y = Predicted_Close), color = "red", size = 1, linetype = "dashed") +
  
  geom_ribbon(data = final_forecast, aes(x = YearMonth, ymin = Lower_80, ymax = Upper_80), fill = "pink", alpha = 0.5) +
  geom_ribbon(data = final_forecast, aes(x = YearMonth, ymin = Lower_95, ymax = Upper_95), fill = "lightgray", alpha = 0.5) +
  
  labs(title = paste("Bitcoin Price Forecast (ARIMA(", paste(best_order, collapse=","), ") on Residuals with Prediction Interval)"),
       x = "Date",
       y = "Price (USD)") +
  
  theme_minimal() +
  
  scale_color_manual(name = "Legend",
                     values = c("black" = "black", "blue" = "blue", "red" = "red"),
                     labels = c("Historical Data", "Actual Price", "Predicted Price")) +
  theme(legend.position = "top")

# Prediciton time interval
p2 <- ggplot(final_forecast, aes(x = YearMonth, y = Predicted_Close)) +
  # Actual prices in test set
  geom_line(data = test_data, aes(x = YearMonth, y = Close), color = "blue", size = 1) +
  
  # Predicted Prices
  geom_line(aes(y = Predicted_Close), color = "red", size = 1, linetype = "dashed") +

  geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80), fill = "pink", alpha = 0.5) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "lightgray", alpha = 0.5) +
  
  labs(title = paste("Bitcoin Price Prediction Interval (ARIMA(", paste(best_order, collapse=","), "))"),
       x = "Date",
       y = "Price (USD)") +
  
  theme_minimal() +
  theme(legend.position = "none")

# Both plots
print(p1)  # Full historical data with forecast
print(p2)  # Prediction interval only

cat("Best ARIMA Order (p,d,q):", paste(best_order, collapse = ","), "\n")
cat("Best AIC:", best_aic, "\n")

# 4) Sarmax Model

sarimax_data <- selected_data %>%
  select(YearMonth, Close, AdrActCnt, TxCnt, HashRate, AdrBalUSD10KCnt, TrendScore) %>%
  drop_na()

sarimax_data$YearMonth <- as.Date(sarimax_data$YearMonth)

train_size <- floor(0.8 * nrow(sarimax_data))
train_data <- sarimax_data[1:train_size, ]
test_data <- sarimax_data[(train_size + 1):nrow(sarimax_data), ]

close_ts <- ts(train_data$Close, frequency = 12)

best_aic <- Inf  # Start with a high AIC value
best_order <- c(0,0,0,0,0,0)  # Best SARIMAX order placeholder

#Grid search
for (p in 0:3) {
  for (d in 0:2) {
    for (q in 0:3) {
      for (P in 0:2) {
        for (D in 0:2) {
          for (Q in 0:2) {
            model_try <- tryCatch({
              fit <- Arima(close_ts, order = c(p,d,q), seasonal = c(P,D,Q), xreg = as.matrix(train_data[,3:7]), method = "ML")
              aic_value <- fit$aic
              if (aic_value < best_aic) {
                best_aic <- aic_value
                best_order <- c(p, d, q, P, D, Q)
              }
            }, error = function(e) NULL)  # Skip models that fail
          }
        }
      }
    }
  }
}


best_sarimax <- Arima(close_ts, order = best_order[1:3], seasonal = best_order[4:6], 
                      xreg = as.matrix(train_data[,3:7]), method = "ML")


future_xreg <- as.matrix(test_data[, 3:7])
sarimax_forecast <- forecast(best_sarimax, h = nrow(test_data), xreg = future_xreg, level = c(80, 95))


forecast_df <- data.frame(
  YearMonth = test_data$YearMonth,
  Actual_Close = test_data$Close,
  Predicted_Close = sarimax_forecast$mean,
  Lower_80 = sarimax_forecast$lower[,1],
  Upper_80 = sarimax_forecast$upper[,1],
  Lower_95 = sarimax_forecast$lower[,2],
  Upper_95 = sarimax_forecast$upper[,2]
)


rmse_value <- sqrt(mean((forecast_df$Actual_Close - forecast_df$Predicted_Close)^2))
mape_value <- mean(abs((forecast_df$Actual_Close - forecast_df$Predicted_Close) / forecast_df$Actual_Close)) * 100
r_squared <- 1 - (sum((forecast_df$Actual_Close - forecast_df$Predicted_Close)^2) / sum((forecast_df$Actual_Close - mean(forecast_df$Actual_Close))^2))

cat("Best SARMAX Order (p,d,q)(P,D,Q):", paste(best_order, collapse = ","), "\n")
cat("Best AIC:", best_aic, "\n")
cat("RMSE:", round(rmse_value, 2), "\n")
cat("MAPE:", round(mape_value, 2), "%\n")
cat("R²:", round(r_squared, 4), "\n")

# Plots 1-2  

# Historical and predicitons 
p1 <- ggplot() +
  geom_line(data = sarimax_data, aes(x = YearMonth, y = Close), color = "black", size = 1) +
  geom_line(data = test_data, aes(x = YearMonth, y = Close), color = "blue", size = 1) +
  geom_line(data = forecast_df, aes(x = YearMonth, y = Predicted_Close), color = "red", size = 1, linetype = "dashed") +
  geom_ribbon(data = forecast_df, aes(x = YearMonth, ymin = Lower_80, ymax = Upper_80), fill = "pink", alpha = 0.5) +
  geom_ribbon(data = forecast_df, aes(x = YearMonth, ymin = Lower_95, ymax = Upper_95), fill = "lightgray", alpha = 0.5) +
  labs(title = paste("Bitcoin Price Forecast (SARMAX(", paste(best_order, collapse=","), "))"),
       x = "Date", y = "Price (USD)") +
  theme_minimal()

# Prediciton interval
p2 <- ggplot(forecast_df, aes(x = YearMonth, y = Predicted_Close)) +
  geom_line(data = test_data, aes(x = YearMonth, y = Close), color = "blue", size = 1) +
  geom_line(aes(y = Predicted_Close), color = "red", size = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80), fill = "pink", alpha = 0.5) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "lightgray", alpha = 0.5) +
  labs(title = paste("Bitcoin Price Prediction Interval (SARMAX(", paste(best_order, collapse=","), "))"),
       x = "Date", y = "Price (USD)") +
  theme_minimal()


print(p1) 
print(p2) 


# 5)XGBOOST model

# Feature Sets for Grid Search
feature_sets <- list(
  c("TxCnt", "AdrActCnt", "HashRate"),
  c("TxCnt", "AdrActCnt", "HashRate", "AdrBalUSD10KCnt"),
  c("TxCnt", "AdrActCnt", "HashRate", "AdrBalUSD10KCnt", "TrendScore"),
  c("TxCnt", "AdrActCnt", "HashRate", "AdrBalUSD10KCnt", "TrendScore", "FeeMeanUSD"),
  c("TxCnt", "AdrActCnt", "HashRate", "AdrBalUSD10KCnt", "TrendScore", "FeeMeanUSD", "log_Volume")
)

# Parameters
param_grid <- expand.grid(
  max_depth = c(3, 5, 7),
  eta = c(0.1, 0.3),
  subsample = c(0.6, 0.7, 1.0),
  booster = c("gbtree"),
  stringsAsFactors = FALSE
)

results <- data.frame()
train_size <- floor(0.8 * nrow(selected_data))
train_data <- selected_data[1:train_size, ]
test_data <- selected_data[(train_size + 1):nrow(selected_data), ]

# Grid search
for (features in feature_sets) {
  for (i in 1:nrow(param_grid)) {
    params <- as.list(param_grid[i, ])
    cat("\nTrying Parameters: MaxDepth=", params$max_depth, "| LearningRate=", params$eta, 
        "| Subsample=", params$subsample, "| Booster=", params$booster, "\n")
    cat("Features:", paste(features, collapse=", "), "\n")
    
    train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, features]), label = train_data$Close)
    test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, features]), label = test_data$Close)
    
    xgb_model <- xgb.train(
      params = params,
      data = train_matrix,
      nrounds = 100,
      verbose = 0
    )
    
    predictions <- predict(xgb_model, test_matrix)
    rmse <- sqrt(mean((test_data$Close - predictions)^2, na.rm = TRUE))
    mape <- mean(abs((test_data$Close - predictions) / test_data$Close), na.rm = TRUE) * 100
    r2 <- cor(test_data$Close, predictions, use = "complete.obs")^2
  
    results <- rbind(results, data.frame(
      Features = paste(features, collapse=", "),
      MaxDepth = params$max_depth,
      LearningRate = params$eta,
      Subsample = params$subsample,
      Booster = params$booster,
      RMSE = rmse,
      MAPE = mape,
      R2 = r2
    ))
  }
}

results <- results[order(results$RMSE), ]
print("Top 5 Best Models (Lowest RMSE):")
print(results[1:5, ])

best_model <- results[1, ]  # Top model
best_features <- unlist(strsplit(best_model$Features, ", "))

best_params <- list(
  max_depth = best_model$MaxDepth,
  eta = best_model$LearningRate,
  subsample = best_model$Subsample,
  booster = best_model$Booster,
  objective = "reg:squarederror"
)

train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, best_features]), label = train_data$Close)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, best_features]), label = test_data$Close)

final_xgb_model <- xgb.train(
  params = best_params,
  data = train_matrix,
  nrounds = 100,
  verbose = 0
)

final_predictions <- predict(final_xgb_model, test_matrix)
final_rmse <- sqrt(mean((test_data$Close - final_predictions)^2, na.rm = TRUE))
final_mape <- mean(abs((test_data$Close - final_predictions) / test_data$Close), na.rm = TRUE) * 100
final_r2 <- cor(test_data$Close, final_predictions, use = "complete.obs")^2

cat("Final XGBoost Model Metrics\n")
cat("RMSE:", final_rmse, "\n")
cat("MAPE:", final_mape, "\n")
cat("R^2:", final_r2, "\n")

# Plots
plot_data <- data.frame(
  YearMonth = test_data$YearMonth,
  Actual = test_data$Close,
  Predicted = final_predictions
)

plot_data$YearMonth <- as.Date(plot_data$YearMonth)
max_prediction_date <- max(plot_data$YearMonth, na.rm = TRUE)

bitcoin_actual_filtered <- bitcoin_monthly %>%
  filter(YearMonth <= max_prediction_date) %>%
  select(YearMonth, Close)

final_plot_data <- full_join(
  bitcoin_actual_filtered, 
  plot_data %>% select(YearMonth, Predicted),
  by = "YearMonth"
)
ggplot(final_plot_data, aes(x = YearMonth)) +
  geom_line(aes(y = Close), color = "blue", size = 1.2) +
  geom_line(aes(y = Predicted), color = "red", linetype = "dashed", size = 1.2) +
  labs(
    title = "Bitcoin Prices vs XGBoost Predictions (Aligned End Dates)",
    x = "Date",
    y = "Bitcoin Price (USD)",
    caption = "Blue: Actual Prices | Red Dashed: XGBoost Predictions"
  ) +
  theme_minimal()
importance_matrix <- xgb.importance(feature_names = best_features, model = final_xgb_model)
ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Horizontal Bar Chart
  labs(
    title = "XGBoost Feature Importance (Gain)",
    x = "Feature",
    y = "Importance Score",
    caption = "Feature importance based on Gain metric"
  ) +
  theme_minimal()





# 6) Prophet

prophet_data <- selected_data %>%
  select(YearMonth, Close, AdrActCnt, TxCnt, HashRate, TrendScore, AdrBalUSD10KCnt, log_Volume, 
         FeeMeanUSD, AdrBal1in1KCnt, AdrBalNtv10Cnt, AdrBalNtv10KCnt, diff_close, log_Close) %>%
  rename(ds = YearMonth, y = Close) 
prophet_data$ds <- as.Date(prophet_data$ds)

#remove na
prophet_data <- prophet_data %>%
  drop_na()

# train-test split
train_size <- floor(0.8 * nrow(prophet_data))
train_data <- prophet_data[1:train_size, ]
test_data <- prophet_data[(train_size + 1):nrow(prophet_data), ]

param_grid <- expand.grid(
  changepoint_prior_scale = c(0.001, 0.01, 0.1, 0.5, 1),
  seasonality_prior_scale = c(0.01, 0.1, 1, 10),
  stringsAsFactors = FALSE
)

# Features
feature_sets <- list(
  c("AdrActCnt", "TxCnt", "HashRate"),
  c("AdrActCnt", "TxCnt", "HashRate", "TrendScore"),
  c("AdrActCnt", "TxCnt", "HashRate", "TrendScore", "AdrBalUSD10KCnt"),
  c("AdrActCnt", "TxCnt", "HashRate", "TrendScore", "AdrBalUSD10KCnt", "log_Volume"),
  c("AdrActCnt", "TxCnt", "HashRate", "TrendScore", "AdrBalUSD10KCnt", "log_Volume", "FeeMeanUSD"),
  c("AdrActCnt", "TxCnt", "HashRate", "TrendScore", "AdrBalUSD10KCnt", "log_Volume", "FeeMeanUSD", "AdrBal1in1KCnt"),
  c("AdrActCnt", "TxCnt", "HashRate", "TrendScore", "AdrBalUSD10KCnt", "log_Volume", "FeeMeanUSD", "AdrBal1in1KCnt", "AdrBalNtv10Cnt"),
  c("AdrActCnt", "TxCnt", "HashRate", "TrendScore", "AdrBalUSD10KCnt", "log_Volume", "FeeMeanUSD", "AdrBal1in1KCnt", "AdrBalNtv10Cnt", "AdrBalNtv10KCnt"),
  c("AdrActCnt", "TxCnt", "HashRate", "TrendScore", "AdrBalUSD10KCnt", "log_Volume", "FeeMeanUSD", "AdrBal1in1KCnt", "AdrBalNtv10Cnt", "AdrBalNtv10KCnt", "diff_close"),
  c("AdrActCnt", "TxCnt", "HashRate", "TrendScore", "AdrBalUSD10KCnt", "log_Volume", "FeeMeanUSD", "AdrBal1in1KCnt", "AdrBalNtv10Cnt", "AdrBalNtv10KCnt", "diff_close", "log_Close")
)

results <- data.frame()

# Grid Search
for (features in feature_sets) {
  for (i in 1:nrow(param_grid)) {
    params <- as.list(param_grid[i, ])
    cat("\nTrying Parameters: changepoint_prior_scale =", params$changepoint_prior_scale, 
        "| seasonality_prior_scale =", params$seasonality_prior_scale, "\n")
    cat("Features:", paste(features, collapse=", "), "\n")
    
    train_data_prophet <- train_data %>%
      select(ds, y, all_of(features))
    
    m <- prophet(
      growth = "linear",
      changepoint.prior.scale = params$changepoint_prior_scale,
      seasonality.prior.scale = params$seasonality_prior_scale
    )
    for (feature in features) {
      m <- add_regressor(m, feature)
    }
    m <- fit.prophet(m, train_data_prophet)
    future <- test_data %>%
      select(ds, all_of(features))
    
    predictions <- predict(m, future)
    pred_values <- predictions$yhat[1:nrow(test_data)]
    rmse <- sqrt(mean((test_data$y - pred_values)^2, na.rm = TRUE))
    mape <- mean(abs((test_data$y - pred_values) / test_data$y), na.rm = TRUE) * 100
    r2 <- cor(test_data$y, pred_values, use = "complete.obs")^2
    
    results <- rbind(results, data.frame(
      Features = paste(features, collapse=", "),
      ChangepointPriorScale = params$changepoint_prior_scale,
      SeasonalityPriorScale = params$seasonality_prior_scale,
      RMSE = rmse,
      MAPE = mape,
      R2 = r2
    ))
  }
}

# Results
results <- results[order(results$RMSE), ]
print("Top 5 Best Models (Lowest RMSE):")
print(results[1:5, ])

# Best model parameters
best_model <- results[1, ]
best_features <- unlist(strsplit(best_model$Features, ", "))

best_params <- list(
  changepoint_prior_scale = best_model$ChangepointPriorScale,
  seasonality_prior_scale = best_model$SeasonalityPriorScale
)

final_m <- prophet(
  growth = "linear",
  changepoint.prior.scale = best_params$changepoint_prior_scale,
  seasonality.prior.scale = best_params$seasonality_prior_scale
)

for (feature in best_features) {
  final_m <- add_regressor(final_m, feature)
}
final_m <- fit.prophet(final_m, train_data %>% select(ds, y, all_of(best_features)))
future <- test_data %>%
  select(ds, all_of(best_features))

final_predictions <- predict(final_m, future)
test_data$Predicted <- final_predictions$yhat[1:nrow(test_data)]

max_test_date <- max(test_data$ds)
historical_filtered <- bitcoin_monthly %>%
  filter(YearMonth <= max_test_date)

# Plot 
ggplot() +
  geom_line(data = historical_filtered, aes(x = YearMonth, y = Close), color = "blue", size = 1) +  # Historical Prices
  geom_line(data = test_data, aes(x = ds, y = Predicted), color = "red", linetype = "dashed", size = 1) +  # Predictions
  labs(
    title = "Bitcoin Price Predictions vs. Historical Prices (Prophet)",
    x = "Date",
    y = "Bitcoin Price (USD)",
    caption = "Blue: Historical Prices | Red Dashed: Prophet Predictions"
  ) +
  theme_minimal()
