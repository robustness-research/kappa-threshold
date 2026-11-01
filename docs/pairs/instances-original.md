# Stage 4: original instance-noise experiment

## Purpose

Trains each (dataset, model) pair on the clean data and on increasing fractions of noisy instances, using 5-fold cross-validation. It saves the fold test-set indices (`vectors`) so that stage 6 can reuse the same folds, and writes kappa and accuracy per noise level and instance level.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/instances_noiser.R` | `Rscript scripts/instances_noiser.R <dataset> <model>` |
| Shell driver | `scripts/instances.sh` | GNU parallel, `-j 16`, hardcoded `monks-problems-2` × `fda` |
| Python module | `src/kappa_threshold/instances_noiser.py` | `run_instances_noiser`, `compute_folds`, `load_or_generate_vectors`, `fold_permutation`, `_kappa_and_accuracy` |
| CLI | `src/kappa_threshold/cli.py` | `kt instances run [--dataset] [--model] [--jobs] [--resume/--no-resume]` |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Dataset | `data/datasets/{d}.csv` | `dataset_csv` |
| MIA table | `results/most_important_attr/mia.csv` | `MIA_CSV` |
| Noisy inputs | `results/noise_injection/by_dataset/...` | `noise_injection_csv` |
| Fold vectors | `results/instances/original/vectors/{d}_{model}_all_folds.csv` | `original_vectors_csv` |
| Per-model shard | `results/instances/original/by_dataset/{d}_{model}_results.csv` | `original_results_csv` |
| Combined (after stage 7) | `..._by_dataset/{d}_results.csv` | `original_combined_results_csv` |

## Function map

| R (`instances_noiser.R`) | Python (`instances_noiser.py`) |
|---|---|
| `train_model` | `models.build_pipeline` |
| `calculate_predictions` | fit and predict inside `run_instances_noiser` |
| `process_instance` | per-instance-level loop in `run_instances_noiser` |
| `process_fold` | fold loop in `run_instances_noiser` |
| `process_model` | `run_instances_noiser` |

## Known differences

- **Shard merging.** Python's `combine_original_by_dataset` only merges a dataset when all model shards exist. Otherwise it returns `None` without a warning.
- **Driver lists.** `instances.sh` runs only `monks-problems-2` × `fda`. The CLI reads all lists from `parameters.csv`.
- **Vectors.** Python reuses saved vectors if they exist (`load_or_generate_vectors`). R writes them during its run.

## Status

- Vectors: 651 of 651 (31 datasets × 21 models).
- Combined results: 29 of 31 datasets. Missing `eucalyptus` and `mfeat-factors`.
- `monks-problems-2`: 20 of 21 models. `fda` failed in R with `'from' must be a finite number`.
- Stray `iris_rf_results.csv`.
- See [instances-original-by-dataset.md](../results/instances-original-by-dataset.md), [instances-original-vectors.md](../results/instances-original-vectors.md), and [checklist.md](../checklist.md#stage-4-original-instance-noise).
