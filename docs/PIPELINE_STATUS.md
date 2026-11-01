# Pipeline analysis and run-status checklist

Snapshot taken 2026-11-01 from `results/`, `scripts/logs/`, `src/kappa_threshold/` and `data/parameters.csv`. Counts are against 31 datasets × 21 models unless noted.

Legend: [x] complete, [~] partial, [ ] not run or missing.

## 1. Pipeline

Config: `data/parameters.csv` defines 31 datasets, 21 models, 5 folds, noise levels 0.1–1, instance levels 0.1–1, thresholds 0.05 / 0.10 / 0.15.

| Stage | Command | Depends on | Output |
|---|---|---|---|
| 1 Datasets | (input) | — | `data/datasets/*.csv` |
| 2 Most important attribute | `kt mia run`, `kt mia combine` | 1 | `results/most_important_attr/` (`mia.csv`, `miaUnique.csv`) |
| 3 Noise injection | `kt noise inject` | 2 | `results/noise_injection/by_dataset/{ds}_{feat}_noise{lvl}.csv` |
| 4 Original instance-noise | `kt instances run` | 1 | `results/instances/original/{vectors,by_dataset}/` |
| 5 Kappa-loss threshold | `kt threshold find` | 4 (combined) | `results/threshold_instance_results.csv` |
| 6 Train-noise | `kt train-noise run` | 3, 4 (vectors), 5 | `results/instances/train_noise/by_dataset/threshold_*/` |
| 7 Combine / average / compare | `kt combine run` | 4, 6 | `results/summary_*.csv`, `results/comparison_original_vs_train_noise.csv` |
| 8 Plots | `kt plot run` | 7 | `results/plots/` |
| 9 Clustering | `kt cluster run [--ideal]` | dataset characteristics | `results/clustering/`, `results/clustering_IDEAL/` |

- `scripts/*.sh` and `scripts/*.R` are the original R pipeline. Several shell scripts hard-code subsets (for example `instances.sh` runs only `monks-problems-2 × fda`). Treat `data/parameters.csv` plus `kt` as the source of truth.
- Stages 5 and 6 use only noise levels 0.1, 0.2, 0.3 (hard-coded in `threshold_finder.py` and `combiners.py`). Levels 0.4–1.0 are generated in stage 3 but never analysed.

## 2. Defects and risks (ranked)

1. **Threshold finder overwrites with a subset.** `run_threshold_finder` writes the whole `threshold_instance_results.csv` from only the datasets passed. `kt threshold find --dataset X` drops every other dataset's rows. Fix: merge on `(dataset_name, technique, noise_level, threshold)`, or refuse partial writes.
2. **Placeholder rows look like real results.** With no combined stage-4 data, `_baseline_rows` injects instance_level 0 / kappa 1, so every critical_percentage is 0. Affected: `eucalyptus` and `mfeat-factors` (all 189 rows each) and `monks-problems-2 × fda` (one model). Nothing flags these.
3. **Combiners skip silently.** `combine_original_by_dataset` and `combine_train_noise_by_dataset` return `None` when any model shard is missing, with no warning. This is why `eucalyptus` and `mfeat-factors` have no combined file.
4. **Noise filename mismatch.** Python builds `..._noise1.0.csv`; the R run wrote `..._noise1.csv`. Resume checks in stage 3 will miss the R files for level 1. `iris_petallength` has both spellings on disk.
5. **Leftover shards mark partial runs.** Shards are unlinked only after a full combine. Remaining shards: `mfeat-pixel` (18 / 18 / 16 of 21 at 0.05 / 0.10 / 0.15), `banknote` `rf` and `wall-robot-navigation` `rfRules` at 0.05.
6. **Stale aggregates.** `summary_*.csv` and `comparison_original_vs_train_noise.csv` are dated 17 Aug, older than `threshold_instance_results.csv` (30 Oct). The summaries cover 28 datasets, not 31.
7. **Errors are not surfaced.** Logs show `monks-problems-2_fda` (R `seq.default` error), 42 train-noise logs failing on missing packages (`RWeka`, `inTrees`, `arm`) for `eucalyptus` and `mfeat-factors`, and an `mfeat-factors` MIA failure. Nothing parses logs.
8. **Stale or stray files.** `mia_new.csv`, `miaUnique_new.csv`, `threshold_instance_results_new.csv` (945 rows, 5 datasets), `results/instances/original/by_dataset/iris_rf_results.csv`, `results/clustering/predicted_group.csv` (29 Jun, 5 rows), and the `.zip` archives in `results/`.
9. **`eucalyptus` original stage unexplained.** Vectors exist for all 21 models, but there are no per-model or combined `by_dataset` CSVs and no error logs. Check whether the files were deleted or never written.
10. **Notebooks** (`notebooks/*.ipynb`, 4 files) are not verified against current outputs.

## 3. Run-status checklist

### Stage 1 – Datasets
- [x] All 31 datasets present in `data/datasets/`

### Stage 2 – Most important attribute
- [x] 30 of 31 datasets have `_mia.csv` and `_miaUnique.csv` in `by_dataset/`
- [x] Combined `mia.csv` and `miaUnique.csv` (139 features, 30 datasets)
- [ ] `mfeat-factors` (log shows an error)

### Stage 3 – Noise injection
- [x] 139 features × 10 levels on disk (R naming, `noise1.csv`)
- [ ] Python-named level 1 (`noise1.0.csv`) not generated for any feature (defect 4)
- [ ] `mfeat-factors` (no MIA)

### Stage 4 – Original instance-noise
- [x] Vectors: 651 of 651
- [x] Combined per-dataset results: 29 of 31
- [ ] `eucalyptus` combined results (vectors exist; defect 9)
- [ ] `mfeat-factors` combined results
- [~] `monks-problems-2`: 20 of 21 models (`fda` failed in R)
- [ ] Stray `iris_rf_results.csv` to remove or explain

### Stage 5 – Threshold
- [x] `threshold_instance_results.csv` has 31 × 189 rows
- [~] Valid rows for 29 datasets (`monks-problems-2 × fda` is a placeholder)
- [ ] `eucalyptus`, `mfeat-factors`: placeholder rows only (critical_percentage = 0); exclude or rerun after stage 4 is fixed

### Stage 6 – Train-noise (thresholds 0.05 / 0.10 / 0.15)
- [x] Combined results for 28 datasets at each threshold
- [~] `mfeat-pixel`: 18 / 18 / 16 of 21 shards
- [~] `banknote` (0.05): `rf` shard left over
- [~] `wall-robot-navigation` (0.05): `rfRules` shard left over
- [ ] `eucalyptus`, `mfeat-factors`: failing on missing R packages (RWeka, inTrees, arm)
- [ ] Confirm `monks-problems-2`, `steel-plates-fault`, `vehicle` have all 21 models (some logs show failures, though combined files exist)

### Stage 7 – Combine / average / compare
- [x] Summaries and comparison exist (dated 17 Aug)
- [ ] Regenerate after stages 4–6 are complete (stale)
- [ ] Summaries cover 28 datasets; `eucalyptus`, `mfeat-factors`, `mfeat-pixel` missing

### Stage 8 – Plots
- [x] Per-dataset plots (2 per threshold) for 28 datasets
- [x] `results/plots/by_cluster/` (22 files) and `by_cluster_IDEAL.zip`
- [ ] `eucalyptus`, `mfeat-factors`, `mfeat-pixel` per-dataset plots
- [ ] `results/summary_plots/` not generated

### Stage 9 – Clustering
- [x] Main clustering: 31 datasets (`groups.csv`, `characteristics.csv`), updated 17 Aug
- [x] IDEAL variant: 27 groups (4 holdout datasets excluded by design)
- [~] `predicted_group.csv` (5 rows, 29 Jun) is stale

### Notebooks
- [ ] Verify the four notebooks run and match current outputs

## 4. Regenerating this checklist

Re-run these checks against disk:

- Count files per stage directory and compare with the expected totals above.
- Find missing `(dataset, model)` pairs in `results/instances/original/vectors/` and combined `by_dataset/` files.
- For each threshold, list `results/instances/train_noise/by_dataset/threshold_*/` shards that remain without a combined file.
- Count rows per dataset in `results/threshold_instance_results.csv` and flag datasets whose `critical_percentage` is all 0.
- Compare modification times of `summary_*.csv` against `threshold_instance_results.csv`.
