"""Expanding-window walk-forward backtesting."""
import numpy as np
from sklearn.metrics import mean_absolute_error, mean_squared_error


def walk_forward_validate(series, forecast_fn, horizon=12, n_folds=4, **fn_kwargs):
    values = np.asarray(series)
    if len(values) <= horizon * n_folds:
        raise ValueError("Not enough data for the requested folds/horizon.")

    metrics = {"MAE": [], "RMSE": [], "MAPE": []}
    for i in range(n_folds):
        train_end = len(values) - horizon * (n_folds - i)
        train, test = values[:train_end], values[train_end:train_end + horizon]
        preds = forecast_fn(train, horizon, **fn_kwargs)

        metrics["MAE"].append(mean_absolute_error(test, preds))
        metrics["RMSE"].append(np.sqrt(mean_squared_error(test, preds)))
        safe_test = np.where(test == 0, 1e-9, test)
        metrics["MAPE"].append(np.mean(np.abs((test - preds) / safe_test)) * 100)

    return {k: float(np.mean(v)) for k, v in metrics.items()}
