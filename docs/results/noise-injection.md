# results/noise_injection/by_dataset/

## Producer

- Stage 3: `src/kappa_threshold/noise_injector.py` (`noise_injection_csv` in `paths.py`).
- R: `scripts/noise_injector.R` (line 152).

## Files (1,391)

Pattern: `{dataset}_{feature}_noise{level}.csv`.

| Level | Files | Naming |
|---|---|---|
| 0.1 to 0.9 | 139 each (1,251) | `noise0.1.csv` to `noise0.9.csv` (R and Python agree) |
| 1 | 139 | `noise1.csv` (R). **Python would write `noise1.0.csv`, so these are not recognised on resume** |
| 1.0 | 1 | `iris_petallength_noise1.0.csv` (Python form). The only Python-named level-1 file |

Features come from `miaUnique.csv` (139), over 30 datasets. `mfeat-factors` has none.

## Completeness

- Every (feature, level 0.1–0.9) pair is present. Level 1 is present under the R name.
- Expected total after the naming fix is 1,390, plus one duplicate for `iris_petallength`.

## Pair docs

[noise-injection.md](../pairs/noise-injection.md), [instances-original.md](../pairs/instances-original.md) (reads these files), [instances-train-noise.md](../pairs/instances-train-noise.md).

Checklist: [checklist.md](../checklist.md#stage-3-noise-injection).
