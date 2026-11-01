# Stage 7 (part): combine original per-model shards

## Purpose

Concatenates the 21 per-model result shards for a dataset into one file, then deletes the shards.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/utilities/dataset_results_combiner.R` | Sourced by the R pipeline |
| Python function | `src/kappa_threshold/combiners.py` | `combine_original_by_dataset` |
| CLI | `src/kappa_threshold/cli.py` | `kt combine run` (calls it for each dataset) |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Shards | `results/instances/original/by_dataset/{d}_{model}_results.csv` | `original_results_csv` |
| Combined | `results/instances/original/by_dataset/{d}_results.csv` | `original_combined_results_csv` |

## Known differences

- **Silent skip.** Python returns `None` and does nothing when any model shard is missing. R behaviour should be checked in the script before relying on it.
- **Shard deletion.** Python deletes the shards after writing the combined file. Leftover shards therefore indicate an incomplete combine.

## Status

- Combined for 29 datasets. Missing `eucalyptus` and `mfeat-factors`. See [instances-original-by-dataset.md](../results/instances-original-by-dataset.md).
