"""Stage 4: original instance-noise experiment.

Port of scripts/instances_noiser.R. 5-fold stratified CV; per fold, fit the
model once on the clean train fold (no internal tuning). For each
(noise_level, instance_level) combination, replace a random subset of test
rows with their noisy counterparts (from stage 3) and compare the clean-fit
model's predictions on clean-test vs partially-noised-test via Cohen's Kappa
and accuracy -- predictions vs predictions, NOT vs ground truth.

The fold/test-index assignments are saved to
results/instances/original/vectors/<dataset>_<model>_all_folds.csv with the
exact schema `fold,index,original_test_index` (1-indexed) so that stage 6
(instances_noiser_train_noise.py) can reconstruct identical train/test
partitions instead of re-splitting.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
from sklearn.metrics import accuracy_score, cohen_kappa_score
from sklearn.model_selection import StratifiedKFold

from scripts.utilities.data_io import load_dataset, split_features
from scripts.utilities.models import build_pipeline
from scripts.noise_injector import inject_noise
from scripts.utilities.paths import (
    MIA_CSV,
    ensure_parent,
    noise_injection_csv,
    instances_results_csv,
    instances_vectors_csv,
)

N_FOLDS = 5


def _lookup_mia(dataset: str, model: str) -> str:
    mia_df = pd.read_csv(MIA_CSV)
    match = mia_df[(mia_df.dataset_name == dataset) & (mia_df.technique == model)]
    if match.empty:
        raise ValueError(f"No MIA entry for dataset={dataset!r}, model={model!r} in {MIA_CSV}")
    return str(match["most_important_attribute"].iloc[0])


def compute_folds(dataset: str, seed: int = 1):
    """Returns list of (fold_number[1-indexed], train_idx, test_idx) with 0-indexed positional arrays."""
    df = load_dataset(dataset)
    X, y = split_features(df)
    skf = StratifiedKFold(n_splits=N_FOLDS, shuffle=True, random_state=seed)
    return [(i + 1, train_idx, test_idx) for i, (train_idx, test_idx) in enumerate(skf.split(X, y))]


def load_or_generate_vectors(dataset: str, model: str, folds, seed: int = 1) -> pd.DataFrame:
    """Load the saved fold/test-index CSV, generating any missing folds' shuffled
    within-test-set permutation and appending them, exactly mirroring the R script's
    incremental-generate-or-load behavior.
    """
    vectors_path = instances_vectors_csv(dataset, model)
    if vectors_path.exists():
        existing = pd.read_csv(vectors_path)
    else:
        existing = pd.DataFrame(columns=["fold", "index", "original_test_index"])

    rng = np.random.default_rng(seed)
    rows_to_add = []
    for fold_number, _train_idx, test_idx in folds:
        if not existing.empty and (existing["fold"] == fold_number).any():
            continue
        n_test = len(test_idx)
        shuffled = rng.permutation(np.arange(1, n_test + 1))  # 1-indexed positions within test set
        original_test_index_1indexed = test_idx[shuffled - 1] + 1  # 1-indexed row in original dataset
        rows_to_add.append(
            pd.DataFrame(
                {
                    "fold": fold_number,
                    "index": shuffled,
                    "original_test_index": original_test_index_1indexed,
                }
            )
        )

    if rows_to_add:
        existing = pd.concat([existing, *rows_to_add], ignore_index=True)
        ensure_parent(vectors_path)
        existing.to_csv(vectors_path, index=False)

    return existing


def fold_permutation(vectors_df: pd.DataFrame, fold_number: int) -> np.ndarray:
    """1-indexed shuffled positions within that fold's test set, in the saved order."""
    fold_rows = vectors_df[vectors_df["fold"] == fold_number]
    return fold_rows["index"].to_numpy()


def _kappa_and_accuracy(clean_pred: np.ndarray, noisy_pred: np.ndarray) -> tuple[float, float]:
    kappa = cohen_kappa_score(clean_pred, noisy_pred)
    if np.isnan(kappa):
        kappa = 1.0
    accuracy = accuracy_score(clean_pred, noisy_pred)
    return float(accuracy), float(kappa)


def run_instances_noiser(
    dataset: str,
    model: str,
    noise_levels: list[float],
    instance_levels: list[float],
    seed: int = 1,
    resume: bool = True,
) -> pd.DataFrame:
    out_path = instances_results_csv(dataset, model)
    if resume and out_path.exists():
        return pd.read_csv(out_path)

    df = load_dataset(dataset)
    X, y = split_features(df)
    mia = _lookup_mia(dataset, model)

    folds = compute_folds(dataset, seed=seed)
    vectors_df = load_or_generate_vectors(dataset, model, folds, seed=seed)

    # Ensure every (noise, mia) noisy-dataset variant exists.
    for noise in noise_levels:
        inject_noise(dataset, mia, noise, seed=seed, resume=resume)

    results = []
    for fold_number, train_idx, test_idx in folds:
        pipe = build_pipeline(model, X.iloc[train_idx])
        pipe.fit(X.iloc[train_idx], y.iloc[train_idx])

        test_X_clean = X.iloc[test_idx].reset_index(drop=True)
        clean_pred = pipe.predict(test_X_clean)

        n_test = len(test_idx)
        all_indices = fold_permutation(vectors_df, fold_number)  # 1-indexed positions within test set

        for noise in noise_levels:
            noisy_full = pd.read_csv(noise_injection_csv(dataset, mia, noise))
            noisy_full = noisy_full.drop(
                columns=["dataset_name", "altered_attribute", "noise_level"], errors="ignore"
            )
            noisy_test_source = noisy_full.iloc[test_idx].reset_index(drop=True)

            for percent in instance_levels:
                sample_size = round(n_test * percent)
                idx_to_use = all_indices[:sample_size] - 1  # 0-indexed positions within test set

                if noise == 0 or sample_size == 0:
                    noisy_pred = clean_pred
                else:
                    noisy_test = test_X_clean.copy()
                    noisy_test.iloc[idx_to_use] = noisy_test_source.drop(columns=["class"]).iloc[
                        idx_to_use
                    ]
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
