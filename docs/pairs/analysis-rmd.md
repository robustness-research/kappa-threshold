# Analysis R Markdown files (no Python equivalent)

## Purpose

Three R Markdown notebooks used to explore results interactively. None is run by the pipeline and none has a Python port.

## Files

| File | Purpose | Reads (as written) |
|---|---|---|
| `scripts/analysis/plots.Rmd` | Per-dataset plots and threshold tables | `../../data/files/parameters.csv`, `../../data/results/threshold_instance_results.csv`, `results_dir/{d}_results.csv` |
| `scripts/analysis/clustering_comparison.Rmd` | Compare new and IDEAL clustering | `data/results/clustering/*.csv`, `data/results/clustering_IDEAL/*.csv` |
| `scripts/analysis/threshold_instance_analysis.Rmd` | Threshold analysis | `../results/threshold_instances_results.csv` |
| `scripts/analysis/clustering_comparison.html` | Rendered output of the clustering notebook | n/a |

## Known problems

- `plots.Rmd` reads `data/files/parameters.csv`, a path that `file_names.R` created and nothing current writes.
- `threshold_instance_analysis.Rmd` uses the plural `threshold_instances_results.csv`. The pipeline writes `threshold_instance_results.csv`.
- Clustering paths use `data/results/`, not `results/`.

## Python counterparts

The equivalent computations live in the Python modules listed in the other pair docs. Notebooks under `notebooks/` (Python) are not checked against current outputs; see [checklist.md](../checklist.md#notebooks).
