"""Metrics table + forecast plot."""
import os
import pandas as pd
import matplotlib.pyplot as plt


def save_results(results_dict, output_path="results/metrics.csv"):
    df = pd.DataFrame(results_dict).T
    df.index.name = "Model"
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    df.round(3).to_csv(output_path)
    print(df.round(3))
    return df


def plot_forecast(actual, forecast, index, output_path="results/forecast.png"):
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    plt.figure(figsize=(12, 5))
    plt.plot(index, actual, label="Actual")
    plt.plot(index, forecast, label="Forecast", linestyle="--")
    plt.legend()
    plt.title("Forecast vs Actual")
    plt.tight_layout()
    plt.savefig(output_path)
    plt.close()
