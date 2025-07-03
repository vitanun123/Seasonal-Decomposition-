# Seasonal-Decomposition-
Seasonal Decomposition with ARIMA-X12,BV.4.1 and Deseats packages
# Project 1: Seasonal Adjustment and Forecasting of US Personal Consumption Expenditures

This project analyzes the **US Personal Consumption Expenditures (PCE)** time series using several decomposition and forecasting techniques in R.

## Project Objectives

- Visualize the log-transformed PCE series
- Decompose the series using:
  - **X-12-ARIMA**
  - **BV4.1 base model**
  - **DeSeaTS** (trend-seasonal smoothing)
- Compare seasonal adjustment results across methods
- Analyze residuals for stationarity and seasonality
- Fit an ARMA model to DeSeaTS residuals
- Perform forecast and back-transform the results to original scale

## Key Techniques and Tools

- Time series decomposition (`seasonal`, `deseats`)
- Periodogram and ACF diagnostics
- ARMA model selection and residual analysis
- Forecasting with bootstrapped intervals
- Retransformation of log-forecast to original units

## R Packages Used

- `ggplot2`, `zoo`, `ggpubr` – plotting and time series
- `deseats` – smoothing, decomposition, ARMA model fitting
- `forecast`, `TSA`, `tseries` – diagnostics and modeling
- `seasonal` – X-12-ARIMA seasonal adjustment
- `tidyr` – data reshaping

## Key Outputs

- Comparison plots of seasonal adjustment methods  
- Stationary series and their periodograms  
- ARMA residual diagnostics and normality test  
- Point and interval forecasts (log + retransformed)

## Input Data

- `Personal Consumption Expenditures.csv` (quarterly, starting from 1960)

---

**Note:** All analysis is performed in log-scale for stationarity and comparability, and final forecasts are back-transformed to original scale for interpretation.
