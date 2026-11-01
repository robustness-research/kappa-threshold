# results/ (top level)

## Files

| File | Rows or size | Written by | Status |
|---|---|---|---|
| `summary_by_dataset.csv` | 56 (28 datasets × 2 types) | `combiners.summarize_results` (R: `summarize_results.R`) | 17 Aug. Stale; covers 28 datasets |
| `summary_by_model.csv` | 42 (21 models × 2 types) | same | 17 Aug. Stale |
| `summary_overall.csv` | 2 | same | 17 Aug. Stale |
| `comparison_original_vs_train_noise.csv` | 21,132 | `combiners.compare_original_vs_train_noise` (R: `summarize_results.R`) | 17 Aug. Fixed name, so only the last threshold survives (see [summarize.md](../pairs/summarize.md)) |
| `threshold_instance_results.csv` | 5,859 (31 × 189) | `threshold_finder.run_threshold_finder` | 30 Oct. Current. Contains placeholder rows for `eucalyptus`, `mfeat-factors`, and `monks-problems-2 × fda` |
| `threshold_instance_results_new.csv` | 945 (5 datasets) | R `threshold_instance_finder.R` | Stray. Still read by the R stage 6 script |
| `clustering_IDEAL.zip` | 14 KB | manual archive | 29 Jun. Snapshot of `clustering_IDEAL/`; not referenced by `paths.py` |

## Notes

- The `noise_level` column in `threshold_instance_results*.csv` is 10, 20, 30. Other files use 0.1, 0.2, 0.3.
- The main threshold file has unquoted headers (Python). The `_new` file has quoted headers (R).
- Summaries are older than the threshold file, so regenerate them after stages 4 to 6 finish.

## Checklist items

See [checklist.md](../checklist.md#stage-5-threshold) and [checklist.md](../checklist.md#stage-7-combine-summarize-compare).
