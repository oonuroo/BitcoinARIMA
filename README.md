# BitcoinARIMA

Forecasts Bitcoin prices over a 6-month horizon using ARIMA(2,0,3), optimized via grid search, achieving an RMSE of 4,586 USD. Combines historical price data, Google Trends, and on-chain metrics (0.28 correlation with active addresses) to predict BTC price trends using R.

## Overview

Developed as part of a study at Università di Padova, this project predicts Bitcoin (BTC) prices using time-series and machine learning models: Multiple Linear Regression (MLR), ARIMA, SARMAX, XGBoost, and Prophet. ARIMA(2,0,3) emerged as the best performer, leveraging monthly aggregated data from 08/2017 to 12/2021. It explores Bitcoin’s volatile, nonlinear behavior with preprocessing techniques like log transformations and stationarity adjustments.

## Features
- **Top Model**: ARIMA(2,0,3) with RMSE 4,586 USD and MAPE 8.43%.
- **Data Integration**: Historical BTC prices, Google Trends, and on-chain metrics (e.g., Active Addresses, Transaction Count).
- **Model Comparison**: Evaluates MLR, ARIMA, SARMAX, XGBoost, and Prophet with grid search optimization.

## Technologies Used
- **R**: Core language (tested with R 4.3.2).
- **Packages**:
  - `tidyverse`: Data manipulation and visualization (`dplyr`, `ggplot2`, etc.).
  - `forecast`: ARIMA and SARMAX modeling.
  - `xgboost`: Gradient boosting.
  - `prophet`: Time-series forecasting.
  - `tseries`: Stationarity tests (ADF).
  - `car`: VIF for multicollinearity.
  - `Metrics`, `MLmetrics`: Performance evaluation.

## Prerequisites
- R 4.0+ (RStudio recommended).
- Install dependencies:
  ```R
  install.packages(c("tidyverse", "forecast", "xgboost", "prophet", "tseries", "car", "Metrics", "MLmetrics"))

## Datasets :
- isoformat-2017.xlsx: Daily BTC prices from OpenDataSoft.
- btc_search.xlsx: Monthly Google Trends for "Bitcoin" from Google Trends.
- btc_onchain.xlsx: On-chain metrics from blockchain APIs.

!! Place datasets in data/ or update paths in main.R.

## Running the Project
# Clone the Repository:
- git clone https://github.com/oonuroo/BitcoinARIMA.git
- cd BitcoinARIMA
# Install Packages:
- source("install_packages.R")

# Run the Analysis:
Rscript main.R

# Outputs: Model predictions, metrics (RMSE, MAPE, R²), and plots in results/.

# Project Structure
- data/: Store input datasets (manually added).
- install_packages.R: Installs required R packages.
- main.R: Full analysis script (preprocessing, modeling, visualization).
- results/: Output plots (e.g., arima_plot.png, xgboost_importance.png).

## Methodology
# Data Preprocessing
- Aggregated daily prices and on-chain data to monthly intervals.
- Applied log transformations (e.g., log_Volume, DiffMean) to reduce skewness.
- Dropped 4.1% missing on-chain data; retained outliers for market insights.
- Decomposed prices into trend, seasonal, and residual components; differenced for stationarity (ADF p-value 0.03).

# Feature Selection
- Correlation analysis: Active Addresses (0.28), Transaction Count (0.21), Google Trends (0.12).
- VIF to remove multicollinearity, retaining TxCnt, AdrActCnt, TrendScore, etc.

# Models
- MLR: Baseline, RMSE 15,758 USD (poor generalization).
- ARIMA(2,0,3): Grid search optimized, RMSE 4,586 USD on test data.
- SARMAX: Grid search applied, but often reverted to ARIMA(0,0,0), failing to converge.
- XGBoost: Grid search over features and parameters (e.g., max_depth=3), RMSE ~17,418 USD.
- Prophet: Grid search with regressors, RMSE 9,197 USD, excels at long-term trends.

## Results
- ARIMA(2,0,3): Best performer (RMSE 4,586 USD, MAPE 8.43%), though it overestimates price declines.
- Key Predictors: Active Addresses and Transaction Count moderately enhance accuracy.
- Visualizations: See results/ for ARIMA forecasts, XGBoost feature importance, and more.

## Limitations
- Short-term volatility remains challenging.
- SARMAX struggles with weakly correlated external predictors.
- XGBoost underperforms without extensive tuning.

License
MIT License - free to use, modify, and share.
Contact
Onur Bacaksız | Email (mailto:onurobacaksiz@gmail.com)

