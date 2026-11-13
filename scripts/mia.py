from __future__ import annotations

import pandas as pd
from sklearn.inspection import permutation_importance

from scripts.utilities.data_io import load_dataset, split_features, scale_numeric
from scripts.utilities.models import build_pipeline, is_supported
from scripts.utilities.paths import (
    ensure_parent,
    mia_by_dataset_csv,
    mia_unique_by_dataset_csv,
)


def _compute_mia(fitted_pipeline, X: pd.DataFrame, y: pd.Series, seed: int = 123) -> str:
    scoring = "neg_log_loss" if hasattr(fitted_pipeline, "predict_proba") else "accuracy"
    try:
        result = permutation_importance(
            fitted_pipeline, X, y, scoring=scoring, n_repeats=5, random_state=seed
        )
    except (ValueError, AttributeError):
        result = permutation_importance(
            fitted_pipeline, X, y, scoring="accuracy", n_repeats=5, random_state=seed
        )
    importances = pd.Series(result.importances_mean, index=X.columns)
    return importances.idxmax()


def compute_mia_for_dataset(dataset: str, techniques: list[str], seed: int = 123) -> pd.DataFrame:
    df = load_dataset(dataset)
    X, y = split_features(df)
    X = scale_numeric(X)

    rows = []
    for technique in techniques:
        if not is_supported(technique):
            print(f"[skip] {technique}: no Python equivalent")
            continue
        pipe = build_pipeline(technique, X)
        pipe.fit(X, y)
        mia = _compute_mia(pipe, X, y, seed=seed)
        rows.append(
            {"dataset_name": dataset, "technique": technique, "most_important_attribute": mia}
        )

    return pd.DataFrame(rows)


def run_mia(dataset: str, techniques: list[str], seed: int = 123, resume: bool = True) -> pd.DataFrame:
    out_path = mia_by_dataset_csv(dataset)
    unique_path = mia_unique_by_dataset_csv(dataset)

    if resume and out_path.exists() and unique_path.exists():
        return pd.read_csv(out_path)

    dataset_results = compute_mia_for_dataset(dataset, techniques, seed=seed)

    ensure_parent(out_path)
    dataset_results.to_csv(out_path, index=False)

    unique_results = dataset_results.drop_duplicates(
        subset=["dataset_name", "most_important_attribute"]
    )[["dataset_name", "most_important_attribute"]]
    ensure_parent(unique_path)
    unique_results.to_csv(unique_path, index=False)

    return dataset_results