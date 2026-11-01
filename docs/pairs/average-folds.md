# Stage 7 (part): average over folds

## Purpose

For each combined results file, averages accuracy and kappa over the 5 folds for each (dataset, method, noise level, instance level). Adds the kappa loss (1 − mean kappa) and standard deviations.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/utilities/average_folds_processer.R` | Sourced by the R pipeline |
| Python functions | `src/kappa_threshold/combiners.py` | `average_folds`, `average_folds_original`, `average_folds_train_noise` |
| CLI | `src/kappa_threshold/cli.py` | `kt combine run` |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Original input | `results/instances/original/by_dataset/{d}_results.csv` | `original_combined_results_csv` |
| Original output | `results/instances/original/by_dataset/{d}_results_avg.csv` | `original_results_avg_csv` |
| Train-noise input | `threshold_{t}/{d}_results.csv` | `train_noise_combined_results_csv` |
| Train-noise output | `threshold_{t}/{d}_results_avg.csv` | `train_noise_results_avg_csv` |

## Known differences

- **Rounding.** Python rounds the averages to 3 decimal places. Check the R script before comparing numbers exactly.

## Status

- Original `_results_avg.csv` exists for 29 of 31 datasets. Missing `eucalyptus` and `mfeat-factors`.
- Train-noise `_results_avg.csv` exists for 28 datasets at each threshold. See the train-noise folder docs.
