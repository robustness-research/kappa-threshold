# Stage 9: dataset clustering (main variant)

## Purpose

Computes per-dataset characteristics (instance count, numeric and nominal attribute counts, dataset type), z-scales them, builds a distance matrix, and clusters datasets with hierarchical clustering (cut at 4, 5, 6 groups) and k-means (4 groups). The groups are used to split the plots by cluster.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/clustering/dataset_clustering_26.R` | `Rscript` (run directly) |
| Python module | `src/kappa_threshold/clustering.py` | `run_clustering`, `compute_characteristics`, `_cluster_and_save` |
| CLI | `src/kappa_threshold/cli.py` | `kt cluster run [--dataset]` |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Datasets | `data/datasets/{d}.csv` | `dataset_csv` |
| Parameters | `data/parameters.csv` | `PARAMETERS_CSV` |
| Output folder | `data/results/clustering/` | `results/clustering/` (`CLUSTERING_DIR`) |
| Files | `characteristics.csv`, `cl_scaled.csv`, `dmatrix.csv`, `centroids.csv`, `kcca_info.csv`, `groups.csv` | same names |

## Known differences

- **Output root.** R writes under `data/results/`. Python writes under `results/`. The current tree has only `results/`.
- **Output file names.** Same names in both.

## Status

- `results/clustering/` has 31 datasets in `characteristics.csv` and `groups.csv`, dated 17 Aug.
- `predicted_group.csv` (5 rows, 29 Jun) is stale. See [clustering.md](../results/clustering.md) and [checklist.md](../checklist.md#stage-9-clustering).
