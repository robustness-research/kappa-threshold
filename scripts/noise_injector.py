"""Stage 3: generate noisy dataset variants for each (dataset, MIA, noise_level).

Port of scripts/noise_injector.R. Numeric MIA -> Gaussian jitter proportional
to the attribute's std dev. Categorical MIA -> a probability blend between
"keep current value" and "sample from the population's empirical frequency",
controlled by alpha = 1 - exp(-noise_level).
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from scripts.utilities.data_io import load_dataset
from scripts.utilities.paths import ensure_parent, noise_injection_csv


def _inject_numeric(df: pd.DataFrame, mia: str, noise_level: float, rng: np.random.Generator) -> pd.Series:
    std_dev = df[mia].std(ddof=1)
    dvar = std_dev * noise_level
    noisy = rng.normal(loc=df[mia].to_numpy(dtype=float), scale=dvar)
    return pd.Series(np.round(noisy, 2), index=df.index)


def _inject_categorical(df: pd.DataFrame, mia: str, noise_level: float, rng: np.random.Generator) -> pd.Series:
    categories = sorted(df[mia].dropna().unique())
    freq = df[mia].value_counts(normalize=True).reindex(categories).fillna(0.0).to_numpy()
    alpha = 1.0 - np.exp(-noise_level)

    cat_index = {cat: i for i, cat in enumerate(categories)}
    n_cat = len(categories)

    # Precompute one blended probability vector per possible current-value,
    # then look each row up by its current category (vectorized rewrite of
    # the R script's per-row loop).
    blended_by_category: dict = {}
    for cat in categories:
        one_hot = np.zeros(n_cat)
        one_hot[cat_index[cat]] = 1.0
        p1 = alpha * freq + (1.0 - alpha) * one_hot
        p1 = p1 / p1.sum()
        blended_by_category[cat] = p1

    new_values = [
        rng.choice(categories, p=blended_by_category[cur_val])
        for cur_val in df[mia]
    ]
    return pd.Series(new_values, index=df.index)


def inject_noise_at_level(df: pd.DataFrame, mia: str, noise_level: float, rng: np.random.Generator) -> pd.DataFrame:
    noisy = df.copy()
    if noise_level == 0:
        return noisy

    is_numeric = pd.api.types.is_numeric_dtype(df[mia])
    if is_numeric:
        noisy[mia] = _inject_numeric(df, mia, noise_level, rng)
    else:
        noisy[mia] = _inject_categorical(df, mia, noise_level, rng)
    return noisy


def inject_noise(
    dataset: str,
    mia: str,
    noise_level: float,
    seed: int = 1,
    resume: bool = True,
) -> None:
    out_path = noise_injection_csv(dataset, mia, noise_level)
    if resume and out_path.exists():
        return

    df = load_dataset(dataset)
    rng = np.random.default_rng(seed)
    noisy = inject_noise_at_level(df, mia, noise_level, rng)

    noisy["dataset_name"] = dataset
    noisy["altered_attribute"] = mia
    noisy["noise_level"] = float(noise_level)

    ensure_parent(out_path)
    noisy.to_csv(out_path, index=False)
