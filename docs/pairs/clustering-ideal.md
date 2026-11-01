# Stage 9: dataset clustering (IDEAL holdout variant)

## Purpose

Same clustering as [clustering-26.md](clustering-26.md), but four datasets are held out as a test set. The model is fit on the remaining datasets and the holdout datasets are assigned to clusters afterwards.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/clustering/dataset_clustering_IDEAL.R` | `Rscript` (run directly) |
| Python function | `src/kappa_threshold/clustering.py` | `run_clustering_ideal`, `IDEAL_HOLDOUT_DATASETS` |
| CLI | `src/kappa_threshold/cli.py` | `kt cluster run --ideal` |

## Holdout datasets

`analcatdata_authorship`, `breast-w`, `cardiotocography`, `liver-disorders` (`paths.py`, `IDEAL_HOLDOUT_DATASETS`).

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Output folder | `data/results/clustering_IDEAL/` | `results/clustering_IDEAL/` (`CLUSTERING_IDEAL_DIR`) |
| Extra files | `train_set.csv`, `test_set.csv`, `train_dmatrix.csv`, `test_data.csv` | `CLUSTERING_IDEAL_TRAIN_SET_CSV`, `..._TEST_SET_CSV`, `..._TRAIN_DMATRIX_CSV`, `..._TEST_DATA_CSV` |

## Known differences

- **Output root.** R writes `data/results/`; Python writes `results/`.

## Status

- `results/clustering_IDEAL/` has 27 groups (31 minus 4 holdouts).
- `results/clustering_IDEAL.zip` is an older snapshot of the folder (29 Jun). See [clustering-ideal.md](../results/clustering-ideal.md) and [checklist.md](../checklist.md#stage-9-clustering).
