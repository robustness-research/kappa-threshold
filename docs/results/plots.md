# results/plots/ (per dataset)

## Producer

- Stage 8: `src/kappa_threshold/plotting.py` (`plot_dataset`, `plot_kappa_loss`, `plot_accuracy_comparison`), paths `plot_kappa_loss_path` and `plot_accuracy_comparison_path`.
- R: `scripts/utilities/compare_results_processer.R` (lines 204–226).

## Files (169)

| Pattern | Count |
|---|---|
| `{d}_threshold_{0_05\|0_1\|0_15}_all_kappa_loss.png` | 84 |
| `{d}_threshold_{0_05\|0_1\|0_15}_all_accuracy_comparison.png` | 84 |
| `by_cluster/` (folder, see [plots-by-cluster.md](plots-by-cluster.md)) | 22 files |
| `by_cluster_IDEAL.zip` | 1 (about 63 MB, 22 PNGs plus `__MACOSX`) |

## Completeness

- 28 datasets × 3 thresholds × 2 types = 168 PNGs present.
- Missing: `eucalyptus`, `mfeat-factors`, `mfeat-pixel`.
- `by_cluster_IDEAL.zip` is a stray archive, not referenced by `paths.py`.

## Pair docs

[plots-kappa-accuracy.md](../pairs/plots-kappa-accuracy.md). Checklist: [checklist.md](../checklist.md#stage-8-plots).
