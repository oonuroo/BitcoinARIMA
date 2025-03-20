install.packages("stringr")
install.packages("ace_tools")

library(stringr)
library(readxl)
library(dplyr)
library(lubridate)
library(ace_tools)
library(ggplot2)
library(zoo)

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


