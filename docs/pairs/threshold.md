# Stage 5: kappa-loss threshold (critical percentage)

## Purpose

Combines the stage 4 results across models and folds, adds a baseline (0% instances, kappa 1), and computes kappa loss (1 − kappa). For each (dataset, model, noise level, threshold) it finds the largest percentage of corrupted instances whose kappa loss stays at or below the threshold. That percentage is the critical percentage used in stage 6.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/threshold_instance_finder.R` | Run directly; noise levels 0.1/0.2/0.3 and thresholds 0.05/0.10/0.15 hardcoded |
| Python module | `src/kappa_threshold/threshold_finder.py` | `compute_kappa_loss_table`, `find_threshold`, `find_all_thresholds`, `run_threshold_finder` |
| CLI | `src/kappa_threshold/cli.py` | `kt threshold find [--dataset]` |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Stage 4 combined | `results/instances/original/by_dataset/{d}_results.csv` | `original_combined_results_csv` |
| Output | `results/threshold_instance_results_new.csv` (quoted headers) | `results/threshold_instance_results.csv` (`THRESHOLD_INSTANCE_RESULTS_CSV`) |

## Function map

| R (`threshold_instance_finder.R`) | Python (`threshold_finder.py`) |
|---|---|
| (aggregation and baseline, inline) | `compute_kappa_loss_table`, `_baseline_rows` |
| (threshold search, inline) | `find_threshold`, `find_all_thresholds` |
| (write, inline) | `run_threshold_finder` |

## Known differences

- **Output file.** R writes `_new.csv`; Python writes the main file. Stage 6 (R) reads `_new.csv`; stage 6 (Python) reads the main file.
- **Subset overwrite.** `run_threshold_finder` writes the whole output file from the datasets passed in. Running it with `--dataset X` drops every other dataset's rows. (This is the bug that required the earlier restore.)
- **Placeholder rows.** When a dataset has no stage 4 combined file, `_baseline_rows` alone is used, so every critical percentage is 0. These rows look like valid output.
- **Noise scale.** `noise_level` is stored as 10, 20, 30 in the output; other files use 0.1, 0.2, 0.3.

## Status

- Output has 31 datasets × 189 rows (21 models × 3 noise levels × 3 thresholds).
- Placeholder rows: `eucalyptus` and `mfeat-factors` (all 189 each), and `monks-problems-2` × `fda`.
- See [results-root.md](../results/results-root.md) and [checklist.md](../checklist.md#stage-5-threshold).
