# results/instances/original/by_dataset/

## Producer

- Stage 4: `src/kappa_threshold/instances_noiser.py` writes per-model shards (`original_results_csv`). `combiners.py` merges them (`original_combined_results_csv`) and averages (`original_results_avg_csv`). Shards are deleted after merging.
- R: `scripts/instances_noiser.R`, `dataset_results_combiner.R`, `average_folds_processer.R`.

## Files (59)

| Pattern | Count | Meaning |
|---|---|---|
| `{d}_results.csv` | 29 | Combined per dataset |
| `{d}_results_avg.csv` | 29 | Fold-averaged per dataset |
| `iris_rf_results.csv` | 1 | **Stray.** One model's result for `iris`, not a pattern the pipeline writes |

## Completeness

- 29 of 31 datasets combined. Missing: `eucalyptus`, `mfeat-factors`.
- `monks-problems-2` is combined with 20 of 21 models (`fda` missing; the R run failed with `'from' must be a finite number`).
- Every combined file has 21 models except `monks-problems-2` (12,000 rows, against 12,600 for a complete dataset).

## Pair docs

[instances-original.md](../pairs/instances-original.md), [combine-original.md](../pairs/combine-original.md), [average-folds.md](../pairs/average-folds.md).

Checklist: [checklist.md](../checklist.md#stage-4-original-instance-noise).
