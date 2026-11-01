# results/instances/original/vectors/

## Producer

- Stage 4: `instances_noiser.py` (`original_vectors_csv`). Read by stage 6 (`instances_noiser_train_noise.py`) to recover the exact folds.
- R: `instances_noiser.R` (line 196 reads them) and `instances_noiser_train_noise.R` (line 274).

## Files (651)

Pattern: `{dataset}_{model}_all_folds.csv`, one per (dataset, model).

## Completeness

- 651 of 651 (31 datasets × 21 models). Complete.
- These are the inputs the test `tests/test_fold_reuse_contract.py` checks against.

## Notes

- Eucalyptus and mfeat-factors have vectors for all 21 models, even though their combined results are missing. So their stage 4 run produced vectors but not results. This is open; see [checklist.md](../checklist.md#stage-4-original-instance-noise).

## Pair docs

[instances-original.md](../pairs/instances-original.md), [instances-train-noise.md](../pairs/instances-train-noise.md).
