# Time-Series Forecasting: SARIMAX vs Seasonal-Naive on Mauna Loa CO2

## Problem
Forecast monthly atmospheric CO2 concentration and evaluate whether a seasonal
SARIMAX model beats a seasonal-naive baseline under walk-forward validation.

## Dataset
Mauna Loa CO2 (weekly, 1958-2001) from `statsmodels.datasets.co2`,
resampled to monthly means (~522 observations, monthly frequency).
Bundled with `statsmodels`; no external download required.

## Method
- **Preprocessing:** weekly -> monthly mean, drop NaNs.
- **Models:**
  - Seasonal-naive baseline (repeat last 12-month cycle).
  - SARIMAX(1,1,1)(1,1,1,12).
- **Validation:** expanding-window walk-forward backtest, 4 folds, 12-month horizon.
- **Metrics:** MAE, RMSE, MAPE (averaged across folds).

## Results
Run `notebooks/01_exploration.ipynb` to regenerate. Metrics are written to
`results/metrics.csv`.

| Model              | MAE   | RMSE  | MAPE  |
|--------------------|-------|-------|-------|
| Seasonal Naive     | _run_ | _to_  | _fill_ |
| SARIMAX            | _run_ | _to_  | _fill_ |

## Project Structure
- data/data_loader.py - dataset loaders
- src/models.py - baseline + SARIMAX
- src/backtest.py - walk-forward validation
- src/evaluate.py - metrics + plots
- notebooks/01_exploration.ipynb - end-to-end demo
- results/ - metrics.csv, forecast.png
- legacy/ - original R practicals (practicals 1-11)

## How to Run
    pip install -r requirements.txt
    jupyter notebook notebooks/01_exploration.ipynb

## Legacy
The original R/RMarkdown lab practicals (decomposition, trend estimation,
detrending on Nottem and AirPassengers) are preserved in `legacy/` for reference.
