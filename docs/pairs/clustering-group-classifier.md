# Stage 9: assign a dataset to a cluster

## Purpose

Given a new dataset name and a K, computes its characteristics and assigns it to the nearest cluster centroid from the main clustering run.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/clustering/dataset_group_classifier.R` | `Rscript` with dataset and K arguments |
| Python function | `src/kappa_threshold/clustering.py` | `predict_cluster` |
| CLI | `src/kappa_threshold/cli.py` | `kt cluster predict <dataset> [--k]` |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Centroids | `data/results/clustering/centroids.csv` | `results/clustering/centroids.csv` (`CLUSTERING_CENTROIDS_CSV`) |
| Characteristics | `data/results/clustering/characteristics.csv` | `CLUSTERING_CHARACTERISTICS_CSV` |
| Output | `data/results/clustering/predicted_group.csv` | `CLUSTERING_PREDICTED_GROUP_CSV` |

## Known differences

- **Input and output root.** R reads and writes `data/results/clustering/`. Python uses `results/clustering/`.
- **Output shape.** R writes its single result row. The CLI writes `pred.to_frame().T` with a single row.

## Status

- `results/clustering/predicted_group.csv` has 5 rows, dated 29 Jun. It was written by the R script, not the current CLI. Treat it as stale. See [clustering.md](../results/clustering.md) and [checklist.md](../checklist.md#stage-9-clustering).
