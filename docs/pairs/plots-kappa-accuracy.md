# Stage 8: per-dataset kappa-loss and accuracy plots

## Purpose

For each dataset and threshold, draws two plots: kappa loss against instance percentage, and accuracy comparison between clean and noisy training.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/utilities/compare_results_processer.R` | Sourced by the R pipeline |
| Python module | `src/kappa_threshold/plotting.py` | `plot_dataset`, `plot_kappa_loss`, `plot_accuracy_comparison` |
| CLI | `src/kappa_threshold/cli.py` | `kt plot run [--dataset]` |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Input | `results/instances/*/by_dataset/{d}_results_avg.csv` | `original_results_avg_csv`, `train_noise_results_avg_csv` |
| Kappa loss | `results/plots/{d}_threshold_{0_05}_all_kappa_loss.png` | `plot_kappa_loss_path` |
| Accuracy | `results/plots/{d}_threshold_{0_05}_all_accuracy_comparison.png` | `plot_accuracy_comparison_path` |

Threshold slugs are `0_05`, `0_1`, `0_15`.

## Known differences

- **None in file names.** Names match between R and Python (checked against existing files).
- **Input dependency.** Plots need the averaged files from stage 7. Datasets without them get no plots.

## Status

- 28 datasets × 3 thresholds × 2 plots = 168 files present.
- Missing datasets: `eucalyptus`, `mfeat-factors`, `mfeat-pixel`. See [plots.md](../results/plots.md).
