# Stage 8: plots split by cluster

## Purpose

Draws the kappa-loss and accuracy plots for each cluster from stage 9, one set per cluster and threshold.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/utilities/compare_results_by_cluster_processer.R` | Sourced by the R pipeline |
| Python module | `src/kappa_threshold/plotting.py` | `plot_by_cluster_kappa_loss_path`, `plot_by_cluster_accuracy_path` (paths) |
| CLI | `src/kappa_threshold/cli.py` | `kt plot run` (no separate cluster command) |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Input | `data/results/...` (wrong root) | `results/instances/...` and `results/clustering/groups.csv` |
| Output | `data/results/plots/by_cluster/` | `results/plots/by_cluster/` (`PLOTS_BY_CLUSTER_DIR`) |
| File names | `cluster_{n}_threshold_{slug}_kappa_loss.png`, `..._accuracy.png` | same |

## Known differences

- **Output root.** R writes under `data/results/`; Python writes under `results/`.
- **Coverage.** The Python CLI does not generate the by-cluster plots as part of `kt plot run`, so they are presumably from the R run. Check `plotting.py` before relying on regeneration.

## Status

- 22 files. Thresholds 0_05 and 0_1 have 8 each; 0_15 has 6. Cluster 1 has no 0_15 files.
- See [plots-by-cluster.md](../results/plots-by-cluster.md) and [checklist.md](../checklist.md#stage-8-plots).
