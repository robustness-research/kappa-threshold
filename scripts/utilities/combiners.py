"""Combine per-model resultds, average folds, and compare original vs retrained results."""

from __future__ import annotations

import pandas as pd

from scripts.utilities.paths import (
    MIA_CSV,
    MIA_UNIQUE_CSV,
    ensure_parent,
    mia_by_dataset_csv,
    mia_unique_by_dataset_csv,
    COMPARISON_CSV,
    SUMMARY_BY_DATASET_CSV,
    SUMMARY_BY_MODEL_CSV,
    SUMMARY_OVERALL_CSV,
    instances_combined_results_csv,
    instances_results_avg_csv,
    instances_results_csv,
    retrained_combined_results_csv,
    retrained_results_avg_csv,
    retrained_results_csv
)

GROUP_COLS = ["dataset", "method", "noise_levels", "instance_level"]


# --- Extract Most Important Attribute ---
def combine_mia(datasets: list[str]) -> None:
    all_results = [pd.read_csv(mia_by_dataset_csv(d)) for d in datasets if mia_by_dataset_csv(d).exists()]
    combined = pd.concat(all_results, ignore_index=True)
    ensure_parent(MIA_CSV)
    combined.to_csv(MIA_CSV, index=False)

    unique_results = [
        pd.read_csv(mia_unique_by_dataset_csv(d))
        for d in datasets
        if mia_unique_by_dataset_csv(d).exists()
    ]
    combined_unique = pd.concat(unique_results, ignore_index=True)
    ensure_parent(MIA_UNIQUE_CSV)
    combined_unique.to_csv(MIA_UNIQUE_CSV, index=False)


































def combine_original_by_dataset(dataset: str, techniques: list[str]) -> pd.DataFrame | None:
    shards = [instances_results_csv(dataset, m) for m in techniques]
    existing = [p for p in shards if p.exists()]
    if len(existing) != len(techniques):
        return None

    combined = pd.concat([pd.read_csv(p) for p in existing], ignore_index=True)
    out_path = instances_combined_results_csv(dataset)
    ensure_parent(out_path)
    combined.to_csv(out_path, index=False)
    for p in existing:
        p.unlink()
    return combined


def combine_train_noise_by_dataset(dataset: str, techniques: list[str], threshold: float) -> pd.DataFrame | None:
    shards = [retrained_results_csv(dataset, m, threshold) for m in techniques]
    existing = [p for p in shards if p.exists()]
    if len(existing) != len(techniques):
        return None

    combined = pd.concat([pd.read_csv(p) for p in existing], ignore_index=True)
    out_path = retrained_combined_results_csv(dataset, threshold)
    ensure_parent(out_path)
    combined.to_csv(out_path, index=False)
    for p in existing:
        p.unlink()
    return combined


def average_folds(df: pd.DataFrame) -> pd.DataFrame:
    grouped = df.groupby(GROUP_COLS, as_index=False).agg(
        avg_accuracy=("accuracy", "mean"),
        avg_kappa=("kappa", "mean"),
        sd_accuracy=("accuracy", "std"),
        sd_kappa=("kappa", "std"),
    )
    grouped["avg_kappa_loss"] = 1 - grouped["avg_kappa"]
    for col in ["avg_accuracy", "avg_kappa", "avg_kappa_loss", "sd_accuracy", "sd_kappa"]:
        grouped[col] = grouped[col].round(3)
    return grouped[
        ["dataset", "method", "noise_levels", "instance_level", "avg_accuracy", "avg_kappa",
         "avg_kappa_loss", "sd_accuracy", "sd_kappa"]
    ]


def average_folds_original(dataset: str) -> pd.DataFrame | None:
    path = instances_combined_results_csv(dataset)
    if not path.exists():
        return None
    averaged = average_folds(pd.read_csv(path))
    out_path = instances_results_avg_csv(dataset)
    ensure_parent(out_path)
    averaged.to_csv(out_path, index=False)
    return averaged


def average_folds_train_noise(dataset: str, threshold: float) -> pd.DataFrame | None:
    path = retrained_combined_results_csv(dataset, threshold)
    if not path.exists():
        return None
    averaged = average_folds(pd.read_csv(path))
    out_path = retrained_results_avg_csv(dataset, threshold)
    ensure_parent(out_path)
    averaged.to_csv(out_path, index=False)
    return averaged


def _load_avg(path, dataset_type: str) -> pd.DataFrame | None:
    if not path.exists():
        return None
    df = pd.read_csv(path)
    df["dataset_type"] = dataset_type
    return df


def compare_original_vs_train_noise(
    datasets: list[str], threshold: float, target_noise_levels: tuple[float, ...] = (0.1, 0.2, 0.3)
) -> pd.DataFrame:
    """The pipeline's core research answer: did noise-aware retraining reduce
    Kappa Loss, and by how much, per (dataset, method, noise_level, instance_level)?
    """
    frames = []
    for dataset in datasets:
        original = _load_avg(instances_results_avg_csv(dataset), "original")
        train_noise = _load_avg(retrained_results_avg_csv(dataset, threshold), "train_noise")
        if original is None or train_noise is None:
            continue
        frames.append(pd.concat([original, train_noise], ignore_index=True))

    if not frames:
        return pd.DataFrame()

    all_data = pd.concat(frames, ignore_index=True)
    all_data = all_data[all_data["noise_levels"].isin(target_noise_levels)]

    pivoted = all_data.pivot_table(
        index=GROUP_COLS, columns="dataset_type", values=["avg_kappa_loss", "avg_accuracy"]
    )
    comparison = pd.DataFrame(index=pivoted.index)
    comparison["original_kappa_loss"] = pivoted[("avg_kappa_loss", "original")].round(3)
    comparison["train_noise_kappa_loss"] = pivoted[("avg_kappa_loss", "train_noise")].round(3)
    comparison["kappa_loss_diff"] = (
        comparison["train_noise_kappa_loss"] - comparison["original_kappa_loss"]
    ).round(3)
    comparison["original_accuracy"] = pivoted[("avg_accuracy", "original")].round(3)
    comparison["train_noise_accuracy"] = pivoted[("avg_accuracy", "train_noise")].round(3)
    comparison["accuracy_diff"] = (
        comparison["train_noise_accuracy"] - comparison["original_accuracy"]
    ).round(3)
    comparison = comparison.reset_index().sort_values(GROUP_COLS).reset_index(drop=True)

    ensure_parent(COMPARISON_CSV)
    comparison.to_csv(COMPARISON_CSV, index=False)
    return comparison


def _summarize(all_data: pd.DataFrame, group_cols: list[str]) -> pd.DataFrame:
    agg = all_data.groupby(group_cols, as_index=False).agg(
        mean_kappa_loss=("avg_kappa_loss", "mean"),
        sd_kappa_loss=("avg_kappa_loss", "std"),
        min_kappa_loss=("avg_kappa_loss", "min"),
        max_kappa_loss=("avg_kappa_loss", "max"),
        mean_accuracy=("avg_accuracy", "mean"),
        sd_accuracy=("avg_accuracy", "std"),
        min_accuracy=("avg_accuracy", "min"),
        max_accuracy=("avg_accuracy", "max"),
    )
    numeric_cols = agg.select_dtypes(include="number").columns
    agg[numeric_cols] = agg[numeric_cols].round(3)
    return agg.sort_values(group_cols).reset_index(drop=True)


def summarize_results(datasets: list[str], threshold: float) -> dict[str, pd.DataFrame]:
    frames = []
    for dataset in datasets:
        original = _load_avg(instances_results_avg_csv(dataset), "original")
        train_noise = _load_avg(retrained_results_avg_csv(dataset, threshold), "train_noise")
        if original is None or train_noise is None:
            continue
        frames.append(pd.concat([original, train_noise], ignore_index=True))

    if not frames:
        return {}

    all_data = pd.concat(frames, ignore_index=True)
    all_data = all_data[all_data["noise_levels"].isin((0.1, 0.2, 0.3))]

    by_dataset = _summarize(all_data, ["dataset", "dataset_type"])
    by_model = _summarize(all_data, ["method", "dataset_type"])
    overall = _summarize(all_data, ["dataset_type"])

    ensure_parent(SUMMARY_BY_DATASET_CSV)
    by_dataset.to_csv(SUMMARY_BY_DATASET_CSV, index=False)
    by_model.to_csv(SUMMARY_BY_MODEL_CSV, index=False)
    overall.to_csv(SUMMARY_OVERALL_CSV, index=False)

    return {"by_dataset": by_dataset, "by_model": by_model, "overall": overall}
