# results/plots/by_cluster/

## Producer

- Stage 8 by cluster: `plotting.py` (`plot_by_cluster_kappa_loss_path`, `plot_by_cluster_accuracy_path`). `kt plot run` does not call the by-cluster plots, so these were probably produced by R (`compare_results_by_cluster_processer.R`, lines 239–263, which writes to `data/results/`).

## Files (22)

Pattern: `cluster_{n}_threshold_{slug}_{kappa_loss|accuracy}.png`, clusters 1 to 4.

| Threshold | Files | Expected |
|---|---|---|
| 0_05 | 8 | 8 |
| 0_1 | 8 | 8 |
| 0_15 | 6 | 8. **Cluster 1 has no 0_15 files** |

## Completeness

- Incomplete at threshold 0.15.

## Pair docs

[plots-by-cluster.md](../pairs/plots-by-cluster.md). Checklist: [checklist.md](../checklist.md#stage-8-plots).
