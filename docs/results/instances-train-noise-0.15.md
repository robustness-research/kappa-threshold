# results/instances/train_noise/by_dataset/threshold_0.15/

## Producer

As in [instances-train-noise-0.05.md](instances-train-noise-0.05.md) with threshold 0.15.

## Files (72)

| Pattern | Count | Meaning |
|---|---|---|
| `{d}_results.csv` | 28 | Combined per dataset |
| `{d}_results_avg.csv` | 28 | Fold-averaged |
| `{d}_{model}_0.15_results.csv` | 16 | **Leftover shards.** All for `mfeat-pixel` (16 of 21) |

## Completeness

- Combined for 28 datasets. Missing: `eucalyptus`, `mfeat-factors`, `mfeat-pixel`.
- `mfeat-pixel` is still partial: 16 of 21 models, the least complete of the three thresholds.

## Pair docs

[instances-train-noise.md](../pairs/instances-train-noise.md).

Checklist: [checklist.md](../checklist.md#stage-6-train-noise).
