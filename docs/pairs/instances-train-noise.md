# Stage 6: train-noise experiment

## Purpose

Retrains each (dataset, model) on noisy training data. For each noise level in {0.1, 0.2, 0.3} and each kappa-loss threshold, it corrupts the MIA feature for the number of training instances given by that dataset's critical percentage (from stage 5), then tests on the same fold as stage 4. The output feeds stage 7's comparison of noisy against clean training.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/instances_noiser_train_noise.R` | `Rscript ... <dataset> <model>` (threshold read from the script) |
| Shell driver | `scripts/instances_train_noise.sh` | Max 8 jobs, hardcoded `wall-robot-navigation` × `rfRules` |
| Python module | `src/kappa_threshold/instances_noiser_train_noise.py` | `run_train_noise`, `_lookup_mia`, `_lookup_critical_percentage`, `fold_membership_from_vectors` |
| CLI | `src/kappa_threshold/cli.py` | `kt train-noise run [--dataset] [--model] [--jobs] [--resume/--no-resume]` |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| MIA table | `results/most_important_attr/mia.csv` | `MIA_CSV` |
| Critical percentages | `results/threshold_instance_results_new.csv` | `THRESHOLD_INSTANCE_RESULTS_CSV` = `threshold_instance_results.csv` (**mismatch**) |
| Fold vectors | `results/instances/original/vectors/...` | `original_vectors_csv` |
| Noisy inputs | `results/noise_injection/by_dataset/...` | `noise_injection_csv` |
| Per-model shard | `results/instances/train_noise/by_dataset/threshold_{t}/{d}_{model}_{t}_results.csv` | `train_noise_results_csv` |
| Combined | `threshold_{t}/{d}_results.csv` | `train_noise_combined_results_csv` |

## Function map

| R (`instances_noiser_train_noise.R`) | Python (`instances_noiser_train_noise.py`) |
|---|---|
| `train_model_noise` | `models.build_pipeline` |
| `calculate_predictions` | fit and predict inside `run_train_noise` |
| `process_instance` | per-noise and per-threshold loop in `run_train_noise` |
| `process_model` | `run_train_noise` |

## Known differences

- **Threshold input file.** R reads `threshold_instance_results_new.csv` (5 datasets, R-written, quoted headers). Python reads `threshold_instance_results.csv` (31 datasets). A run of the R stage would find almost no rows.
- **Noise-level scale.** The threshold file stores `noise_level` as 10, 20, 30. Python converts to the 0.1 to 0.3 scale when it looks up critical percentages.
- **Critical percentage 0.** `instances_noiser_train_noise.py` has a special case for a critical percentage of 0 (around line 138). Placeholder rows (see [threshold.md](threshold.md)) reach this case. The R handling was not checked.
- **Fixed noise levels.** Only 0.1, 0.2, 0.3 are run.

## Status

- Combined per threshold: 28 datasets. Missing `eucalyptus`, `mfeat-factors`, `mfeat-pixel`.
- Leftover shards (partial runs): `mfeat-pixel` at 0.05, 0.1, 0.15 (18/18/16 of 21). `banknote` `rf` and `wall-robot-navigation` `rfRules` at 0.05.
- `eucalyptus` and `mfeat-factors` fail with missing packages `RWeka`, `inTrees`, `arm`.
- See [instances-train-noise-0.05.md](../results/instances-train-noise-0.05.md), [instances-train-noise-0.1.md](../results/instances-train-noise-0.1.md), [instances-train-noise-0.15.md](../results/instances-train-noise-0.15.md), and [checklist.md](../checklist.md#stage-6-train-noise).
