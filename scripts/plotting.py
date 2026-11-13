"""For each dataset and threshold: per-model side-by-side kappa-loss line plots (original vs
train_noise, vline at critical_percentage) arranged in a grid, and per-model
grouped accuracy bar charts (faceted by noise level) arranged in a grid.
Colors matching the R script: original="#42c0f6", train_noise="#2E86AB".
"""

from __future__ import annotations

import math

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd

from scripts.utilities.paths import (
    THRESHOLD_RESULTS_CSV,
    ensure_parent,
    instances_results_avg_csv,
    plot_accuracy_comparison_path,
    plot_kappa_loss_path,
    retrained_results_avg_csv,
)

TARGET_NOISE_LEVELS = (0.1, 0.2, 0.3)
DATASET_TYPE_COLORS = {"original": "#42c0f6", "train_noise": "#2E86AB"}


def _load_combined(dataset: str, threshold: float) -> pd.DataFrame | None:
    original_path = instances_results_avg_csv(dataset)
    train_noise_path = retrained_results_avg_csv(dataset, threshold)
    if not original_path.exists() or not train_noise_path.exists():
        return None

    original = pd.read_csv(original_path)
    original["dataset_type"] = "original"
    train_noise = pd.read_csv(train_noise_path)
    train_noise["dataset_type"] = "train_noise"

    combined = pd.concat([original, train_noise], ignore_index=True)
    return combined[combined["noise_levels"].isin(TARGET_NOISE_LEVELS)]


def _load_threshold_results(dataset: str, threshold: float) -> pd.DataFrame:
    df = pd.read_csv(THRESHOLD_RESULTS_CSV)
    df = df.rename(columns={"dataset_name": "dataset", "technique": "method", "noise_level": "noise_levels"})
    df["noise_levels"] = df["noise_levels"] / 100
    return df[(df.dataset == dataset) & (df.threshold == threshold) & df.noise_levels.isin(TARGET_NOISE_LEVELS)]


def _plot_kappa_loss_pair(ax_orig, ax_train, method_data: pd.DataFrame, model: str, threshold_data: pd.DataFrame):
    for ax, dtype, label in [(ax_orig, "original", "Original"), (ax_train, "train_noise", "Train Noise")]:
        sub = method_data[method_data.dataset_type == dtype]
        for noise in TARGET_NOISE_LEVELS:
            noise_sub = sub[sub.noise_levels == noise].sort_values("instance_level")
            if noise_sub.empty:
                continue
            ax.plot(
                noise_sub["instance_level"].astype(str),
                noise_sub["avg_kappa_loss"],
                marker="o",
                label=f"{noise}",
            )
        for _, row in threshold_data.iterrows():
            ax.axvline(str(row["critical_percentage"] / 100), linestyle=":", color="orange", alpha=0.7)
        ax.set_title(f"{model} - {label}")
        ax.set_xlabel("Instance Level")
        ax.set_ylabel("Kappa Loss")
        ax.set_ylim(0, 0.3)
        ax.legend(title="Noise Level", fontsize="small")


def plot_kappa_loss(dataset: str, threshold: float, methods: list[str]) -> None:
    combined = _load_combined(dataset, threshold)
    if combined is None:
        return
    threshold_results = _load_threshold_results(dataset, threshold)

    available_models = [m for m in methods if m in combined["method"].unique()]
    if not available_models:
        return

    n_cols = min(2, len(available_models))
    n_rows = math.ceil(len(available_models) / n_cols)
    fig, axes = plt.subplots(n_rows, n_cols * 2, figsize=(6 * n_cols * 2 / 2, 3 * n_rows), squeeze=False)

    for i, model in enumerate(available_models):
        row, col_pair = divmod(i, n_cols)
        ax_orig = axes[row][col_pair * 2]
        ax_train = axes[row][col_pair * 2 + 1]
        method_data = combined[combined.method == model]
        threshold_data = threshold_results[threshold_results.method == model]
        _plot_kappa_loss_pair(ax_orig, ax_train, method_data, model, threshold_data)

    fig.tight_layout()
    out_path = plot_kappa_loss_path(dataset, threshold)
    ensure_parent(out_path)
    fig.savefig(out_path, dpi=120)
    plt.close(fig)


def plot_accuracy_comparison(dataset: str, threshold: float, methods: list[str]) -> None:
    combined = _load_combined(dataset, threshold)
    if combined is None:
        return

    available_models = [m for m in methods if m in combined["method"].unique()]
    if not available_models:
        return

    n_cols = min(2, len(available_models))
    n_rows = math.ceil(len(available_models) / n_cols)
    fig, axes = plt.subplots(
        n_rows * len(TARGET_NOISE_LEVELS), n_cols, figsize=(6 * n_cols, 2 * n_rows * len(TARGET_NOISE_LEVELS)),
        squeeze=False,
    )

    for i, model in enumerate(available_models):
        row_block, col = divmod(i, n_cols)
        method_data = combined[combined.method == model]
        for j, noise in enumerate(TARGET_NOISE_LEVELS):
            ax = axes[row_block * len(TARGET_NOISE_LEVELS) + j][col]
            noise_data = method_data[method_data.noise_levels == noise]
            instance_levels = sorted(noise_data["instance_level"].unique())
            width = 0.35
            x = range(len(instance_levels))
            for k, dtype in enumerate(["original", "train_noise"]):
                sub = noise_data[noise_data.dataset_type == dtype].set_index("instance_level")
                values = [sub.loc[il, "avg_accuracy"] if il in sub.index else 0 for il in instance_levels]
                offset = (k - 0.5) * width
                ax.bar(
                    [xi + offset for xi in x], values, width=width,
                    color=DATASET_TYPE_COLORS[dtype], alpha=0.7, label=dtype.replace("_", " ").title(),
                )
            ax.set_xticks(list(x))
            ax.set_xticklabels([str(il) for il in instance_levels])
            ax.set_ylim(0, 1)
            ax.set_title(f"{model} (noise={noise}) - Accuracy Comparison", fontsize=9)
            ax.set_xlabel("Instance Level")
            ax.set_ylabel("Accuracy")
            if i == 0 and j == 0:
                ax.legend(fontsize="small")

    fig.tight_layout()
    out_path = plot_accuracy_comparison_path(dataset, threshold)
    ensure_parent(out_path)
    fig.savefig(out_path, dpi=120)
    plt.close(fig)


def plot_dataset(dataset: str, threshold: float, methods: list[str]) -> None:
    plot_kappa_loss(dataset, threshold, methods)
    plot_accuracy_comparison(dataset, threshold, methods)
