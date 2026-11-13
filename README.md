# kappa-threshold documentation

Start here. The pipeline has two implementations: the original R scripts in `scripts/legacy/` and the Python port in `scripts/`, run through the `kt` command. Results will be in `results/`.

## Script pipeline

| Order | R | Python
|---|---|---
|  mia extraction | `legacy/mia_extractor.R`, `mia.sh`, `mia_combine.sh` | `mia.py`
| noise injection | `legacy/noise_injector.R`, `noise.sh` | `noise_injector.py` 
| instances alteration | `legacy/instances_noiser.R`, `instances.sh` | `instances_noiser.py` 
| threshold finding | `legacy/threshold_instance_finder.R` | `threshold_finder.py` 
| re-training with noise| `legacy/instances_noiser_train_noise.R`, `instances_train_noise.sh` | `instances_noiser_train_noise.py` 
| clustering | `legacy/clustering/dataset_clustering_26.R`, `dataset_clustering_IDEAL.R`, `dataset_group_classifier.R` | `clustering.py`

## Conventions used in these docs

- Paths are relative to the repo root.
- Python paths come from `scripts/paths.py`.
- Threshold slugs: `0_05`, `0_1`, `0_15` in plot names; `0.05`, `0.1`, `0.15` in folder names.
- Noise levels: `0.1`–`0.3` in analysis files; `10`/`20`/`30` in `threshold_instance_results.csv`.
