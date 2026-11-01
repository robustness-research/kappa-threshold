# kappa-threshold documentation

Start here. The pipeline has two implementations: the original R scripts in `scripts/` and the Python port in `src/kappa_threshold/`, run through the `kt` command. Results are in `results/`.

## Stage map

| Stage | R | Python | Pair doc | Result docs |
|---|---|---|---|---|
| 2 MIA | `scripts/mia_extractor.R`, `mia.sh`, `mia_combine.sh` | `mia.py` | [mia.md](pairs/mia.md) | [most-important-attr.md](results/most-important-attr.md), [most-important-attr-by-dataset.md](results/most-important-attr-by-dataset.md), [most-important-attr-r-legacy.md](results/most-important-attr-r-legacy.md) |
| 3 Noise | `scripts/noise_injector.R`, `noise.sh` | `noise_injector.py` | [noise-injection.md](pairs/noise-injection.md) | [noise-injection.md](results/noise-injection.md) |
| 4 Original | `scripts/instances_noiser.R`, `instances.sh` | `instances_noiser.py` | [instances-original.md](pairs/instances-original.md) | [instances-original-by-dataset.md](results/instances-original-by-dataset.md), [instances-original-vectors.md](results/instances-original-vectors.md) |
| 5 Threshold | `scripts/threshold_instance_finder.R` | `threshold_finder.py` | [threshold.md](pairs/threshold.md) | [results-root.md](results/results-root.md) |
| 6 Train-noise | `scripts/instances_noiser_train_noise.R`, `instances_train_noise.sh` | `instances_noiser_train_noise.py` | [instances-train-noise.md](pairs/instances-train-noise.md) | [0.05](results/instances-train-noise-0.05.md), [0.1](results/instances-train-noise-0.1.md), [0.15](results/instances-train-noise-0.15.md) |
| 7 Combine | `scripts/utilities/dataset_results_combiner.R`, `dataset_noise_results_combiner.R`, `average_folds_processer.R`, `summarize_results.R` | `combiners.py` | [combine-original.md](pairs/combine-original.md), [combine-train-noise.md](pairs/combine-train-noise.md), [average-folds.md](pairs/average-folds.md), [summarize.md](pairs/summarize.md) | [results-root.md](results/results-root.md), [summary-plots.md](results/summary-plots.md) |
| 8 Plots | `scripts/utilities/compare_results_processer.R`, `compare_results_by_cluster_processer.R` | `plotting.py` | [plots-kappa-accuracy.md](pairs/plots-kappa-accuracy.md), [plots-by-cluster.md](pairs/plots-by-cluster.md) | [plots.md](results/plots.md), [plots-by-cluster.md](results/plots-by-cluster.md) |
| 9 Clustering | `scripts/clustering/dataset_clustering_26.R`, `dataset_clustering_IDEAL.R`, `dataset_group_classifier.R` | `clustering.py` | [clustering-26.md](pairs/clustering-26.md), [clustering-ideal.md](pairs/clustering-ideal.md), [clustering-group-classifier.md](pairs/clustering-group-classifier.md) | [clustering.md](results/clustering.md), [clustering-ideal.md](results/clustering-ideal.md) |

## Other files

- [pairs/analysis-rmd.md](pairs/analysis-rmd.md): R Markdown notebooks, no Python port
- [pairs/unported-utilities.md](pairs/unported-utilities.md): R utilities with no Python port
- [pairs/python-only.md](pairs/python-only.md): Python-only modules and tests
- [PIPELINE_STATUS.md](PIPELINE_STATUS.md): earlier status summary and defect list
- [checklist.md](checklist.md): what has and has not run, with the missing items

## Results folders

Each folder in `results/` has its own doc under [results/](results/). The top-level files are in [results-root.md](results/results-root.md). `summary-plots.md` describes a folder that does not exist yet.

## Conventions used in these docs

- Paths are relative to the repo root.
- Python paths come from `src/kappa_threshold/paths.py`.
- Threshold slugs: `0_05`, `0_1`, `0_15` in plot names; `0.05`, `0.1`, `0.15` in folder names.
- Noise levels: `0.1`–`0.3` in analysis files; `10`/`20`/`30` in `threshold_instance_results.csv`.
