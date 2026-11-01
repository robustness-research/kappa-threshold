# results/clustering_IDEAL/

## Producer

- Stage 9, `--ideal` variant: `src/kappa_threshold/clustering.py` (`run_clustering_ideal`), constants `CLUSTERING_IDEAL_*` in `paths.py`.
- R: `scripts/clustering/dataset_clustering_IDEAL.R` (writes `data/results/clustering_IDEAL/`).

## Files (10)

| File | Contents |
|---|---|
| `characteristics.csv`, `cl_scaled.csv`, `dmatrix.csv`, `centroids.csv`, `kcca_info.csv`, `groups.csv` | As in [clustering.md](clustering.md), for the 27 non-holdout datasets |
| `train_dmatrix.csv` | Distance matrix for the training datasets |
| `train_set.csv`, `test_set.csv` | Training and holdout dataset lists |
| `test_data.csv` | Characteristics of the 4 holdout datasets |

Holdout datasets: `analcatdata_authorship`, `breast-w`, `cardiotocography`, `liver-disorders`.

## Status

- 27 groups, by design. Dated 17 Aug.
- `results/clustering_IDEAL.zip` (in `results/`) is an older snapshot. Remove or keep as an archive once the folder is confirmed.

## Pair docs

[clustering-ideal.md](../pairs/clustering-ideal.md). Checklist: [checklist.md](../checklist.md#stage-9-clustering).
