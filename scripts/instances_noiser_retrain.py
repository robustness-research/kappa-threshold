"""Stage 6: train-noise experiment.

Port of scripts/instances_noiser_train_noise.R. For noise_levels restricted
to {0.1, 0.2, 0.3}: look up critical_percentage (from stage 5's
threshold_instance_results.csv) for (dataset, model, noise_level, threshold),
corrupt that fraction of TRAINING rows using the same noise-injection
mechanic as stage 3/4, retrain, then re-evaluate clean-vs-noisy-test
kappa/accuracy identically to stage 4 -- using the SAME noise_level for both
training-data corruption and test-time evaluation corruption (matching the
R script's single `noise` loop variable reused for both).

Critically, this stage MUST reconstruct the exact train/test partition that
stage 4 used, by reading back results/instances/original/vectors/<dataset>_
<model>_all_folds.csv, rather than re-splitting the data -- stage 4's saved
indices are the source of truth (see `fold_membership_from_vectors`).
"""

from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.metrics import accuracy_score, cohen_kappa_score

from scripts.utilities.data_io import load_dataset, split_features
from scripts.utilities.models import build_pipeline
from scripts.noise_injector import inject_noise
from scripts.utilities.paths import (
    MIA_CSV,
    THRESHOLD_RESULTS_CSV,
    ensure_parent,
    noise_injection_csv,
    instances_vectors_csv,
    retrained_results_csv,
)

N_FOLDS = 5
TRAIN_NOISE_LEVELS = (0.1, 0.2, 0.3)


def _lookup_mia(dataset: str, model: str) -> str:
    mia_df = pd.read_csv(MIA_CSV)
    match = mia_df[(mia_df.dataset_name == dataset) & (mia_df.technique == model)]
    if match.empty:
        raise ValueError(f"No MIA entry for dataset={dataset!r}, model={model!r}")
    return str(match["most_important_attribute"].iloc[0])


def _lookup_critical_percentage(dataset: str, model: str, noise_level: float, threshold: float) -> float:
    thresholds_df = pd.read_csv(THRESHOLD_RESULTS_CSV)
    noise_pct = noise_level * 100
    match = thresholds_df[
        (thresholds_df.dataset_name == dataset)
        & (thresholds_df.technique == model)
        & (np.isclose(thresholds_df.noise_level, noise_pct))
        & (np.isclose(thresholds_df.threshold, threshold))
    ]
    if match.empty:
        raise ValueError(
            f"No threshold entry for dataset={dataset!r}, model={model!r}, "
            f"noise_level={noise_pct}, threshold={threshold}"
        )
    return float(match["critical_percentage"].iloc[0]) / 100.0


def fold_membership_from_vectors(dataset: str, model: str, n_rows: int) -> dict[int, tuple[np.ndarray, np.ndarray]]:
    """Reconstruct {fold: (train_idx_0indexed, test_idx_0indexed)} from stage 4's
    saved vectors CSV -- the structural contract stage 6 depends on. Never
    re-split the data here.
    """
    vectors_path = instances_vectors_csv(dataset, model)
    if not vectors_path.exists():
        raise FileNotFoundError(
            f"{vectors_path} not found -- run stage 4 (instances_noiser) for "
            f"({dataset!r}, {model!r}) first."
        )
    vectors_df = pd.read_csv(vectors_path)

    membership = {}
    for fold_number in sorted(vectors_df["fold"].unique()):
        fold_rows = vectors_df[vectors_df["fold"] == fold_number]
        test_idx_1indexed = np.unique(fold_rows["original_test_index"].to_numpy())
        test_idx = test_idx_1indexed - 1
        train_idx = np.setdiff1d(np.arange(n_rows), test_idx, assume_unique=False)
        membership[int(fold_number)] = (train_idx, test_idx)
    return membership


def fold_permutation(vectors_df: pd.DataFrame, fold_number: int) -> np.ndarray:
    fold_rows = vectors_df[vectors_df["fold"] == fold_number]
    return fold_rows["index"].to_numpy()


def _kappa_and_accuracy(clean_pred: np.ndarray, noisy_pred: np.ndarray) -> tuple[float, float]:
    kappa = cohen_kappa_score(clean_pred, noisy_pred)
    if np.isnan(kappa):
        kappa = 1.0
    accuracy = accuracy_score(clean_pred, noisy_pred)
    return float(accuracy), float(kappa)


def run_train_noise(
    dataset: str,
    model: str,
    threshold: float,
    instance_levels: list[float],
    noise_levels: tuple[float, ...] = TRAIN_NOISE_LEVELS,
    seed: int = 1,
    resume: bool = True,
) -> pd.DataFrame:
    out_path = retrained_results_csv(dataset, model, threshold)
    if resume and out_path.exists():
        return pd.read_csv(out_path)

    df = load_dataset(dataset)
    X, y = split_features(df)
    mia = _lookup_mia(dataset, model)

    membership = fold_membership_from_vectors(dataset, model, n_rows=len(df))
    vectors_df = pd.read_csv(instances_vectors_csv(dataset, model))

    for noise in noise_levels:
        inject_noise(dataset, mia, noise, seed=seed, resume=resume)

    rng = np.random.default_rng(seed)
    results = []

    for fold_number, (train_idx, test_idx) in membership.items():
        train_X_clean = X.iloc[train_idx]
        train_y = y.iloc[train_idx]
        test_X_clean = X.iloc[test_idx].reset_index(drop=True)

        all_indices = fold_permutation(vectors_df, fold_number)
        n_test = len(test_idx)

        for noise in noise_levels:
            critical_percentage = _lookup_critical_percentage(dataset, model, noise, threshold)

            if critical_percentage == 0:
                noisy_train_X = train_X_clean
            else:
                noisy_full = pd.read_csv(noise_injection_csv(dataset, mia, noise))
                noisy_full = noisy_full.drop(
                    columns=["dataset_name", "altered_attribute", "noise_level"], errors="ignore"
                )
                noisy_train_source = noisy_full.iloc[train_idx].reset_index(drop=True).drop(columns=["class"])

                n_train = len(train_idx)
                n_instances = round(n_train * critical_percentage)
                alter_pos = rng.choice(n_train, size=n_instances, replace=False)

                noisy_train_X = train_X_clean.reset_index(drop=True).copy()
                noisy_train_X.iloc[alter_pos] = noisy_train_source.iloc[alter_pos]

            pipe = build_pipeline(model, noisy_train_X)
            pipe.fit(noisy_train_X, train_y.reset_index(drop=True) if critical_percentage != 0 else train_y)

            clean_pred = pipe.predict(test_X_clean)

            noisy_full_test = pd.read_csv(noise_injection_csv(dataset, mia, noise))
            noisy_full_test = noisy_full_test.drop(
                columns=["dataset_name", "altered_attribute", "noise_level"], errors="ignore"
            )
            noisy_test_source = noisy_full_test.iloc[test_idx].reset_index(drop=True).drop(columns=["class"])

            for percent in instance_levels:
                sample_size = round(n_test * percent)
                idx_to_use = all_indices[:sample_size] - 1

                if sample_size == 0:
                    noisy_pred = clean_pred
                else:
                    noisy_test = test_X_clean.copy()
                    noisy_test.iloc[idx_to_use] = noisy_test_source.iloc[idx_to_use]
                    noisy_pred = pipe.predict(noisy_test)

                accuracy, kappa = _kappa_and_accuracy(clean_pred, noisy_pred)
                results.append(
                    {
                        "dataset": dataset,
                        "fold": f"Fold_{fold_number}",
                        "method": model,
                        "noise_levels": noise,
                        "instance_level": percent,
                        "accuracy": accuracy,
                        "kappa": kappa,
                    }
                )

    result_df = pd.DataFrame(results)
    ensure_parent(out_path)
    result_df.to_csv(out_path, index=False)
    return result_df
