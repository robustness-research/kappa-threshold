from __future__ import annotations

import os

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
    """Resample each row from a blend of "keep my value" and the population frequencies.

    Fully vectorized (no per-row Python): row i's new category is drawn by
    inverse-CDF sampling from cdf[code_i], using one uniform draw per row.
    """
    values = df[mia]
    categories = sorted(values.dropna().unique())
    n_cat = len(categories)
    freq = values.value_counts(normalize=True).reindex(categories).fillna(0.0).to_numpy()
    alpha = 1.0 - np.exp(-noise_level)

    # probs[c] = distribution of the new value for a row currently in category c.
    probs = alpha * freq[None, :] + (1.0 - alpha) * np.eye(n_cat)
    cdf = np.cumsum(probs, axis=1)
    cdf[:, -1] = 1.0  # guard against floating-point drift

    codes = pd.Categorical(values, categories=categories).codes.astype(np.intp)
    u = rng.random(len(values))

    # Look up every row in its own cdf row with a single searchsorted: shift
    # row c of the cdf by +c so the flattened table is globally sorted, and
    # shift each uniform draw by the same +c.
    offsets = np.arange(n_cat)
    flat_cdf = (cdf + offsets[:, None]).ravel()
    flat_pos = np.searchsorted(flat_cdf, u + codes, side="right")
    new_codes = np.clip(flat_pos - codes * n_cat, 0, n_cat - 1)

    return pd.Series(np.asarray(categories, dtype=object)[new_codes], index=df.index)


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

    # Write to a temp file, then rename: an interrupted run must never leave a
    # truncated CSV that `resume` would treat as finished.
    ensure_parent(out_path)
    tmp_path = out_path.with_name(out_path.name + ".tmp")
    noisy.to_csv(tmp_path, index=False)
    os.replace(tmp_path, out_path)
