# results/clustering/

## Producer

- Stage 9, main variant: `src/kappa_threshold/clustering.py` (`run_clustering`), constants in `paths.py` (`CLUSTERING_*_CSV`).
- R: `scripts/clustering/dataset_clustering_26.R` (writes `data/results/clustering/`, the wrong root), `dataset_group_classifier.R` (writes `predicted_group.csv`).

## Files (7)

| File | Contents |
|---|---|
| `characteristics.csv` | One row per dataset: instance count, numeric and nominal attribute counts, dataset type (31 rows) |
| `cl_scaled.csv` | Z-scaled characteristics |
| `dmatrix.csv` | Distance matrix |
| `centroids.csv` | Cluster centroids |
| `kcca_info.csv` | k-centroid run information (1 row) |
| `groups.csv` | Cluster assignment per dataset under `hclust_4/5/6` and `kmeans_4` (31 rows) |
| `predicted_group.csv` | Output of the group classifier for 5 datasets. **Dated 29 Jun, from the R script, so stale** |

## Status

- Complete for 31 datasets, dated 17 Aug, except `predicted_group.csv`.
- Checklist: [checklist.md](../checklist.md#stage-9-clustering).

## Pair docs

[clustering-26.md](../pairs/clustering-26.md), [clustering-group-classifier.md](../pairs/clustering-group-classifier.md).
