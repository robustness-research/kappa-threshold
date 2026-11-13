"""Process dataset before use in other scripts"""
from __future__ import annotations

import pandas as pd

from scripts.utilities.paths import dataset_csv

CLASS_COLUMN = "class"

def load_dataset(dataset: str) -> pd.DataFrame:
    """Load dataset and find class column. Rename to 'class' if necessary. Throw error if class column not found"""
    df = pd.read_csv(dataset_csv(dataset))
    class_col = next(
        (col for col in df.columns if col.lower() == CLASS_COLUMN),
        None,
    )
    if class_col is None:
        raise ValueError(
            f"No case-insensitive 'class' column found among {list(df.columns)}"
        )
    if class_col != CLASS_COLUMN:
        df = df.rename(columns={class_col: CLASS_COLUMN})
    return df


def split_features(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.Series]:
    """Split 'class' column from other dataset features."""
    X = df.drop(columns=[CLASS_COLUMN])
    y = df[CLASS_COLUMN]
    return X, y


def get_num_col(X: pd.DataFrame) -> list[str]:
    """Return names of numerical columns."""
    return list(X.select_dtypes(include="number").columns)


def get_cat_col(X: pd.DataFrame) -> list[str]:
    """Return names of catergorical columns."""
    return list(X.select_dtypes(exclude="number").columns)


def scale_numeric(X: pd.DataFrame) -> pd.DataFrame:
    """Scale numeric columns."""
    X = X.copy()
    num_cols = get_num_col(X)
    if num_cols:
        means = X[num_cols].mean()
        stds = X[num_cols].std(ddof=1).replace(0, 1)
        X[num_cols] = (X[num_cols] - means) / stds
    return X
