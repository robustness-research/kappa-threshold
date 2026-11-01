# Stage 3: noise injection

## Purpose

For each dataset, each MIA feature (from `miaUnique.csv`) and each noise level, corrupts that feature's values in the dataset and writes the result. These noisy copies are the inputs that stages 4 and 6 read when they train with noise.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/noise_injector.R` | `Rscript scripts/noise_injector.R <dataset>` |
| Shell driver | `scripts/noise.sh` | Runs the R script per dataset with `nohup` |
| Python module | `src/kappa_threshold/noise_injector.py` | `inject_noise` |
| CLI | `src/kappa_threshold/cli.py` | `kt noise inject [--dataset] [--jobs] [--resume/--no-resume]` |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Dataset | `data/datasets/{d}.csv` | `dataset_csv` |
| MIA features | `results/most_important_attr/miaUnique.csv` | `MIA_UNIQUE_CSV` |
| Output | `results/noise_injection/by_dataset/{d}_{feature}_noise{level}.csv` | `noise_injection_csv` |
| Level 1 filename | `..._noise1.csv` | `..._noise1.0.csv` (mismatch) |

## Function map

| R (`noise_injector.R`) | Python (`noise_injector.py`) |
|---|---|
| `inject_noise_at_level` | `inject_noise_at_level` |
| `process_attribute` | `inject_noise` (per feature) |
| `process_dataset` | `cli.noise_inject` loop |
| (numeric branch, inline) | `_inject_numeric` |
| (categorical branch, inline) | `_inject_categorical` |

## Known differences

- **Level-1 filename.** R writes `noise1.csv`. Python's `f"{1.0}"` gives `noise1.0.csv`. Resume checks look for the Python name, so the 139 R files for level 1 are not recognised and would be regenerated. Levels 0.1 to 0.9 match.
- **Level scale.** Levels run 0.1 to 1.0 in the parameters, but stages 5 and 6 only use 0.1, 0.2, 0.3, so 0.4 to 1.0 are never analysed.

## Status

- 139 features across 30 datasets, levels 0.1 to 0.9 present for all of them.
- Level 1 present as R-named files (139). One Python-named level-1 file exists (`iris_petallength_noise1.0.csv`).
- `mfeat-factors` has no files, because it has no MIA features.
- See [noise-injection.md](../results/noise-injection.md) and [checklist.md](../checklist.md#stage-3-noise-injection).
