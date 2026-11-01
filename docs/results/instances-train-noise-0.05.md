# results/instances/train_noise/by_dataset/threshold_0.05/

## Producer

- Stage 6 for threshold 0.05: `instances_noiser_train_noise.py` (`train_noise_results_csv`), merged by `combiners.py` (`train_noise_combined_results_csv`, `train_noise_results_avg_csv`).
- R: `instances_noiser_train_noise.R` (lines 388–393), `dataset_noise_results_combiner.R`.

## Files (76)

| Pattern | Count | Meaning |
|---|---|---|
| `{d}_results.csv` | 28 | Combined per dataset |
| `{d}_results_avg.csv` | 28 | Fold-averaged |
| `{d}_{model}_0.05_results.csv` | 20 | **Leftover shards.** Show a partial run |

Leftover shards: `banknote` `rf`, `wall-robot-navigation` `rfRules`, and 18 for `mfeat-pixel`.

## Completeness

- Combined for 28 datasets. Missing: `eucalyptus`, `mfeat-factors`, `mfeat-pixel`.
- `eucalyptus` and `mfeat-factors` fail with missing packages (`RWeka`, `inTrees`, `arm`).

## Pair docs

[instances-train-noise.md](../pairs/instances-train-noise.md), [combine-train-noise.md](../pairs/combine-train-noise.md), [average-folds.md](../pairs/average-folds.md).

Checklist: [checklist.md](../checklist.md#stage-6-train-noise).
