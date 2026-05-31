# TÜİK Forecasting Project

Forecasting the next monthly observation of the **Non-Domestic Producer Price Index (2010=100)** using TÜİK data accessed through the `tuikr` R package.

## 1. Project Overview

This repository contains an R-based forecasting project for the Quantitative Analysis course. The objective is to forecast the next unpublished monthly value of the Non-Domestic Producer Price Index, compare ten required forecasting methods, and select the superior method based on both accuracy and suitability to the time series structure.

## 2. Data Source and TÜİK Connection

The data were accessed through the `tuikr` R package from the TÜİK Data Portal.

- **TÜİK data set name:** Non-Domestic Producer Price Index and Rate of Change [2010=100]
- **TÜİK theme/category:** Price Statistics
- **TÜİK table name:** Non-Domestic Producer Price Index and Rate of Change [2010=100]
- **TÜİK dataflow ID:** `TR,DF_YDUFE_EDO_V1,1.0`
- **Selected variable:** Non-Domestic Producer Price Index (2010=100)
- **Data frequency:** Monthly
- **Time coverage:** 2010-01 to 2026-02
- **Latest available observation:** 2026-02
- **Forecast target period:** 2026-03
- **Date of data access:** 2026-05-31
- **R package used for data access:** `tuikr`
- **Package source:** https://github.com/emraher/tuikr

## 3. Research Objective

The Non-Domestic Producer Price Index measures price changes in imported goods used in production. It is inflation-related because import prices and exchange-rate movements often pass through to domestic inflation.

## 4. Use of TÜİK Data in R

No manually downloaded, edited, copied, or separately created data file is used. The notebook:

1. attempts `tuikr::statistical_data()` with the registered SDMX dataflow ID,
2. if the SDMX endpoint is unavailable, uses `tuikr::statistical_tables()` to locate the official portal table,
3. downloads the TÜİK table programmatically inside R,
4. converts the imported data into a forecasting-ready monthly time series.

## 5. Exploratory Time Series Analysis

The series shows:

- a visible upward trend,
- monthly seasonality,
- short-run random variation around trend,
- no manually introduced missing periods in the filtered national index series.

See `forecasting_project.html` for the full exploratory discussion and plot.

## 6. Forecasting Methods Applied

The following methods are implemented in the notebook:

- Naïve Forecasting
- Moving Average
- Weighted Moving Average
- Exponential Smoothing
- Trend-Adjusted Exponential Smoothing
- Linear Trend Projection
- Seasonal Indices
- Additive Decomposition
- Multiplicative Decomposition
- Regression with Trend and Seasonal Dummy Variables

All methods are applicable because the selected series is a monthly numeric index with enough observations for seasonal analysis.

## 7. Forecast Accuracy Comparison

The project compares methods using:

- Bias / Mean Error
- MAD
- MSE
- MAPE
- RSFE
- Tracking Signal

The comparison table is saved in `outputs/tables/accuracy_comparison.csv`.

## 8. Selection of the Superior Method

The superior method is selected using both forecast accuracy and suitability to the monthly trend and seasonality of the Non-Domestic Producer Price Index. The final choice is reported in the notebook and in `outputs/tables/final_forecast.csv`.

## 9. Final Next-Period Forecast

- Selected Superior Method: Multiplicative Decomposition
- Date of Data Access: 2026-05-31
- Latest Available Observation: 2026-02
- Forecast Target Period: 2026-03
- Forecasted Value: 2994.10

## 10. Interpretation of Results

The selected model forecasts the Non-Domestic Producer Price Index for March 2026
at 2994.10, suggesting that imported producer price pressure remains on its
recent upward trajectory after adjusting for monthly seasonality.

## 11. Limitations

- exchange-rate shocks,
- possible TÜİK data revisions,
- structural breaks during high-inflation periods,
- no external explanatory variables,
- SDMX service availability may require the portal fallback path.

## 12. Reproducibility

### Requirements

- R (4.2 or newer recommended)
- RStudio
- Internet connection for live TÜİK data access

### Packages

```r
install.packages(c("remotes", "knitr", "dplyr", "kableExtra", "readxl", "crul"))
remotes::install_github("emraher/tuikr")
```

### Run the project

1. Clone the repository.
2. Set the working directory to the project root.
3. Open `forecasting_project.Rmd`.
4. Click **Knit**.

## 13. Repository Structure

```text
tuik-forecasting-project/
├── README.md
├── forecasting_project.Rmd
├── forecasting_project.html
├── outputs/
│   ├── tables/
│   └── figures/
├── R/
│   ├── data_import.R
│   ├── forecasting_methods.R
│   ├── accuracy_measures.R
│   └── plots.R
├── renv.lock
└── .gitignore
```

## 14. Author

- **Name:** Mehmet Zahid Kemik
- **Student Number:** 138722034
- **Course:** Quantitative Analysis

## Google Sheet Registration

| Field | Value |
|---|---|
| Student Name | Mehmet Zahid Kemik |
| Student Number | 138722034 |
| TÜİK Data Set Name | Non-Domestic Producer Price Index and Rate of Change [2010=100] |
| TÜİK Theme / Category | Price Statistics |
| TÜİK Table Name | Non-Domestic Producer Price Index and Rate of Change [2010=100] |
| tuikr Dataflow ID | TR,DF_YDUFE_EDO_V1,1.0 |
| Selected Variable | Non-Domestic Producer Price Index (2010=100) |
| Data Frequency | Monthly |
