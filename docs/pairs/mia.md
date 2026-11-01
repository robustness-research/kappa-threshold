# Stage 2: most important attribute (MIA)

## Purpose

For each dataset, fits every technique on the full z-scaled data (no tuning) and ranks features by permutation importance. The top feature per (dataset, technique) is the most important attribute (MIA). Its unique values per dataset decide which features get noise in stage 3.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/mia_extractor.R` | `Rscript scripts/mia_extractor.R <dataset>` (or no args for all) |
| Shell driver | `scripts/mia.sh` | Runs the R script per dataset with `nohup` |
| Shell combine | `scripts/mia_combine.sh` | Concatenates per-dataset CSVs |
| Python module | `src/kappa_threshold/mia.py` | `run_mia`, `compute_mia_for_dataset`, `combine_mia` |
| CLI | `src/kappa_threshold/cli.py` | `kt mia run [--dataset] [--jobs] [--resume/--no-resume]`, `kt mia combine` |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Dataset | `data/datasets/{d}.csv` | `DATASETS_DIR / {d}.csv` (`dataset_csv`) |
| Parameters | `data/parameters.csv` | `PARAMETERS_CSV` |
| Per-dataset result | `results/most_important_attr/by_dataset/{d}_mia.csv` | `mia_by_dataset_csv` |
| Per-dataset unique | `results/most_important_attr/by_dataset/{d}_miaUnique.csv` | `mia_unique_by_dataset_csv` |
| Combined | `results/most_important_attr/mia.csv`, `miaUnique.csv` | `MIA_CSV`, `MIA_UNIQUE_CSV` |

## Function map

| R (`mia_extractor.R`) | Python (`mia.py`) |
|---|---|
| `load_parameters` | `config.load_parameters` |
| `train_model` (caret, `trainControl(method="none")`) | `models.build_pipeline` |
| `calculate_feature_importance` (iml `FeatureImp`, loss `ce`) | `_compute_mia` (sklearn `permutation_importance`, `neg_log_loss`, 5 repeats, seed 1) |
| `process_method` | inner loop of `compute_mia_for_dataset` |
| `process_dataset` | `compute_mia_for_dataset` plus `run_mia` (writes both CSVs) |
| `mia_combine.sh` | `combine_mia` |

## Known differences

- **Method.** R uses iml cross-entropy permutation importance. Python uses sklearn log-loss permutation importance, and `models.py` substitutes sklearn estimators for 9 R techniques. MIA results are not expected to match R exactly.
- **Silent fallback.** `_compute_mia` falls back to `accuracy` scoring on `ValueError`/`AttributeError`, with no log entry.
- **Write timing.** R writes each dataset's CSVs only after all 21 models finish. One failing model loses the whole dataset.
- **Resume.** `run_mia` returns early if both per-dataset files exist. Rerunning over existing output needs `--no-resume`.
- **Driver lists.** `mia.sh` hardcodes five datasets. The CLI reads the dataset list from `parameters.csv`.
- **Stale stray files.** `mia_new.csv` and `miaUnique_new.csv` in `results/most_important_attr/` are not produced by `paths.py`.

## Status

- Outputs: see [most-important-attr.md](../results/most-important-attr.md) and [most-important-attr-by-dataset.md](../results/most-important-attr-by-dataset.md).
- `mfeat-factors` has no output. The R log stops at `gcvEarth` with `future.globals.maxSize` exceeded, and the log header is from an older script revision. See [checklist.md](../checklist.md#stage-2-most-important-attribute-mia).
- The Python port has not been run over all datasets. The backup `results/most_important_attr/r_legacy/` was made before that run was interrupted.
