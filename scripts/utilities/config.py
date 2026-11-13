"""Loader for data/parameters.csv as an object."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import pandas as pd

from scripts.utilities.paths import PARAMETERS_CSV


@dataclass(frozen=True)
class Parameters:
    dataset_name: list[str]
    fold_name: list[str]
    technique_name: list[str]
    noise_levels: list[float]
    instance_level: list[float]
    threshold_level: list[float]

    @property
    def datasets(self) -> list[str]:
        return self.dataset_name

    @property
    def techniques(self) -> list[str]:
        return self.technique_name


def _split(value: str) -> list[str]:
    return [v.strip() for v in value.split("|")]


def load_parameters(path: Path = PARAMETERS_CSV) -> Parameters:
    df = pd.read_csv(path)
    values = dict(zip(df["parameter"], df["values"]))

    return Parameters(
        dataset_name=_split(values["dataset_name"]),
        fold_name=_split(values["fold_name"]),
        technique_name=_split(values["technique_name"]),
        noise_levels=[float(v) for v in _split(values["noise_levels"])],
        instance_level=[float(v) for v in _split(values["instance_level"])],
        threshold_level=[float(v) for v in _split(values["threshold_level"])],
    )
