"""Forecasting models: Seasonal Naive baseline + SARIMAX."""
import numpy as np
from statsmodels.tsa.statespace.sarimax import SARIMAX


def seasonal_naive_forecast(train, horizon, season_length=12):
    last_season = np.asarray(train)[-season_length:]
    reps = int(np.ceil(horizon / season_length))
    return np.tile(last_season, reps)[:horizon]


def sarimax_forecast(train, horizon, order=(1, 1, 1), seasonal_order=(1, 1, 1, 12)):
    model = SARIMAX(
        train,
        order=order,
        seasonal_order=seasonal_order,
        enforce_stationarity=False,
        enforce_invertibility=False,
    )
    # Added maxiter=500 to fix ConvergenceWarning
    results = model.fit(disp=False, maxiter=500)
    return np.asarray(results.forecast(steps=horizon))
