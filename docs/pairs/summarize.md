# Stage 7 (part): summaries and original-versus-train-noise comparison

## Purpose

Builds the comparison of clean-trained against noise-trained results for noise levels 0.1 to 0.3, and summary tables by dataset, by model, and overall.

## Files

| Role | Path | Entry point |
|---|---|---|
| R script | `scripts/utilities/summarize_results.R` | Sourced by the R pipeline; also writes plots |
| Python functions | `src/kappa_threshold/combiners.py` | `compare_original_vs_train_noise`, `summarize_results` |
| CLI | `src/kappa_threshold/cli.py` | `kt combine run` (for each threshold) |

## Inputs and outputs

| | R | Python (`paths.py`) |
|---|---|---|
| Averaged inputs | R reads `data/results/instances/...` (wrong root) | `results/instances/...` |
| Comparison | `results/comparison_original_vs_train_noise.csv` | `COMPARISON_ORIGINAL_VS_TRAIN_NOISE_CSV` |
| Summary by dataset | `results/summary_by_dataset.csv` | `SUMMARY_BY_DATASET_CSV` |
| Summary by model | `results/summary_by_model.csv` | `SUMMARY_BY_MODEL_CSV` |
| Overall | `results/summary_overall.csv` | `SUMMARY_OVERALL_CSV` |
| Plots | `results/summary_plots/` | `SUMMARY_PLOTS_DIR` (defined, never written) |

## Known differences

- **Input root.** R reads `data/results/`; Python reads `results/`.
- **Plots.** R writes summary plots to `results/summary_plots/`. Python defines the path but writes nothing there.
- **Overwrites.** The comparison and summary files have fixed names that do not include the threshold. Running `kt combine run` for each threshold therefore leaves only the last threshold's data in them. Check this before reading the files as per-threshold results.

## Status

- Summaries cover 28 datasets. They are dated 17 Aug, older than the 30 Oct threshold file, so they should be regenerated.
- `summary_plots/` does not exist. See [results-root.md](../results/results-root.md) and [summary-plots.md](../results/summary-plots.md).
