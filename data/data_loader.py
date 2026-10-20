"""Dataset loaders for the TSA forecasting project."""
import pandas as pd
from statsmodels.datasets import co2


def get_co2_monthly():
    """Mauna Loa CO2 (weekly, 1958-2001) resampled to monthly means."""
    data = co2.load_pandas().data
    series = data["co2"].resample("MS").mean()
    # Interpolate missing months instead of dropping them, then force freq
    series = series.interpolate(method='time').asfreq('MS')
    series.name = "co2"
    return series


def get_airpassengers():
    """Classic AirPassengers monthly totals (1949-1960). Requires internet on first call."""
    from statsmodels.datasets import get_rdataset
    df = get_rdataset("AirPassengers", "datasets").data
    idx = pd.date_range(start="1949-01-01", periods=len(df), freq="MS")
    return pd.Series(df["value"].values, index=idx, name="passengers")
