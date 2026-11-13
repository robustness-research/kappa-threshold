"""Stage 5: kappa-loss threshold / critical-percentage computation.

Port of scripts/threshold_instance_finder.R. Aggregates all stage-4
per-dataset results, adds synthetic instance_level=0 -> kappa=1 baseline
rows, averages kappa across folds, computes kappa_loss = 1 - mean_kappa,
then for each (dataset, technique, noise_level, threshold) finds the largest
instance percentage ("critical_percentage") for which kappa_loss stays
<= threshold.

Noise levels are hardcoded to {0.1, 0.2, 0.3} here (matching the R script),
NOT the full noise_levels list in parameters.csv -- this restricted set is
also what stage 6 (train-noise) uses.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from scripts.utilities.paths import (
    THRESHOLD_RESULTS_CSV,
    ensure_parent,
    instances_combined_results_csv,
)

NOISE_LEVELS = (0.1, 0.2, 0.3)
KL_THRESHOLDS = (0.05, 0.10, 0.15)


def _load_combined_results(datasets: list[str]) -> pd.DataFrame:
    frames = []
    for dataset in datasets:
        path = instances_combined_results_csv(dataset)
        if path.exists():
            frames.append(pd.read_csv(path))
    if not frames:
        raise FileNotFoundError("No combined <dataset>_results.csv files found; run combiners first.")
    return pd.concat(frames, ignore_index=True)


def _baseline_rows(datasets: list[str], techniques: list[str]) -> pd.DataFrame:
    grid = pd.MultiIndex.from_product(
        [datasets, techniques, NOISE_LEVELS], names=["dataset", "method", "noise_levels"]
    ).to_frame(index=False)
    grid["instance_level"] = 0.0
    grid["fold"] = np.nan
    grid["accuracy"] = 1.0
    grid["kappa"] = 1.0
    return grid


def compute_kappa_loss_table(datasets: list[str], techniques: list[str]) -> pd.DataFrame:
    all_data = _load_combined_results(datasets)
    baseline = _baseline_rows(datasets, techniques)
    all_data = pd.concat([baseline, all_data], ignore_index=True)

    grouped = (
        all_data.groupby(["dataset", "method", "noise_levels", "instance_level"], as_index=False)
        .agg(kappa=("kappa", "mean"))
    )
    grouped["kappa_loss"] = 1.0 - grouped["kappa"]
    grouped = grouped.rename(
        columns={"dataset": "dataset_name", "method": "technique", "noise_levels": "noise", "instance_level": "percentage"}
    )
    grouped["noise"] = grouped["noise"] * 100
    grouped["percentage"] = grouped["percentage"] * 100
    return grouped


def find_threshold(data: pd.DataFrame, threshold: float) -> tuple[float | None, float | None]:
    if data.empty:
        return None, None
    data = data.sort_values("percentage")
    under_threshold = data["kappa_loss"] <= threshold
    if under_threshold.any():
        row = data[under_threshold].iloc[-1]
    else:
        row = data.iloc[-1]
    return float(row["percentage"]), float(row["kappa_loss"])


def find_all_thresholds(
    datasets: list[str],
    techniques: list[str],
    noise_levels: tuple[float, ...] = NOISE_LEVELS,
    kl_thresholds: tuple[float, ...] = KL_THRESHOLDS,
) -> pd.DataFrame:
    klc_data = compute_kappa_loss_table(datasets, techniques)

    rows = []
    for dataset in datasets:
        for technique in techniques:
            for noise in noise_levels:
                noise_pct = noise * 100
                subset = klc_data[
                    (klc_data.dataset_name == dataset)
                    & (klc_data.technique == technique)
                    & (klc_data.noise == noise_pct)
                ]
                for thresh in kl_thresholds:
                    critical_percentage, kappa_at_critical = find_threshold(subset, thresh)
                    rows.append(
                        {
                            "dataset_name": dataset,
                            "technique": technique,
                            "noise_level": noise_pct,
                            "threshold": thresh,
                            "critical_percentage": critical_percentage,
                            "kappa_at_critical": kappa_at_critical,
                        }
                    )
    return pd.DataFrame(rows)


def run_threshold_finder(datasets: list[str], techniques: list[str]) -> pd.DataFrame:
    result = find_all_thresholds(datasets, techniques)
    ensure_parent(THRESHOLD_RESULTS_CSV)
    result.to_csv(THRESHOLD_RESULTS_CSV, index=False)
    return result
