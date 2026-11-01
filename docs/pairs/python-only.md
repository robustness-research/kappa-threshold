# Python-only modules and tests

These modules and tests have no R source. They replace duplicated R code or the shell drivers.

## Modules

| Module | Role | Replaces |
|---|---|---|
| `src/kappa_threshold/paths.py` | Single source of every path. Rooted at `results/` and `data/`. | Mixed `data/results/` and `results/` paths in R |
| `src/kappa_threshold/config.py` | `load_parameters` reads `data/parameters.csv` | `read.csv` plus `strsplit` in each R script |
| `src/kappa_threshold/data_io.py` | `load_dataset`, `find_class_column`, `zscale_numeric` | Inline dataset loading and `scale()` in R |
| `src/kappa_threshold/models.py` | `build_pipeline`, `PLSClassifier`, substitute estimators | Three duplicated `train_model` functions (`mia_extractor.R`, `instances_noiser.R`, `instances_noiser_train_noise.R`) |
| `src/kappa_threshold/parallel.py` | `run_parallel` with skip-if-exists | GNU parallel and `nohup` in the `.sh` drivers |
| `src/kappa_threshold/cli.py` | `kt` commands | `scripts/mia.sh`, `noise.sh`, `instances.sh`, `instances_train_noise.sh`, `mia_combine.sh` |
| `src/kappa_threshold/__init__.py` | Package marker | none |

## Tests

| Test | Covers | Reads files |
|---|---|---|
| `tests/test_config.py` | Parameter counts and technique names | `data/parameters.csv` |
| `tests/test_models.py` | Every technique builds and fits on numeric and mixed data | none |
| `tests/test_noise_injector.py` | Zero noise is identity; noise statistics; categorical behaviour | none |
| `tests/test_threshold_finder.py` | Known answers and edge cases for `find_threshold` | none |
| `tests/test_fold_reuse_contract.py` | Stage 6 rebuilds the stage 4 folds from saved vectors | `data/datasets/{d}.csv` |

No MIA test exists. Test pass status is not recorded in the repo; see [checklist.md](../checklist.md#tests).
