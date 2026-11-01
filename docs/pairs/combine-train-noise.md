# Stage 7 (part): combine train-noise shards per threshold

## Purpose

For each threshold, concatenates the per-model train-noise shards for a dataset into one file, then deletes the shards.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/utilities/dataset_noise_results_combiner.R` | Sourced by the R pipeline |
| Python function | `src/kappa_threshold/combiners.py` | `combine_train_noise_by_dataset` |
| CLI | `src/kappa_threshold/cli.py` | `kt combine run` |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Shards | `results/instances/train_noise/by_dataset/threshold_{t}/{d}_{model}_{t}_results.csv` | `train_noise_results_csv` |
| Combined | `results/instances/train_noise/by_dataset/threshold_{t}/{d}_results.csv` | `train_noise_combined_results_csv` |

## Known differences

- **Silent skip.** Python returns `None` when any shard for the dataset is missing. Partial datasets are therefore left out without a warning.
- **Shard deletion.** Shards are removed after the combined file is written. Any shard still on disk means that dataset has not been combined since its last model finished.

## Status

- Combined for 28 datasets at every threshold. Missing `eucalyptus`, `mfeat-factors`, `mfeat-pixel`. See the three train-noise folder docs.
