# results/most_important_attr/ (top level)

## Producer

- Stage 2: `src/kappa_threshold/mia.py` (`combine_mia`), constants `MIA_CSV` and `MIA_UNIQUE_CSV` in `paths.py`. Also `kt mia combine`.
- R: `scripts/mia_extractor.R`, `mia_combine.sh`.

## Files (4 at top level, plus folders)

| File | Rows | Written by | Status |
|---|---|---|---|
| `mia.csv` | 630 (30 datasets × 21 models) | `combine_mia` | Current R-era output. Read by stage 4 (R), stage 6 |
| `miaUnique.csv` | 139 features | `combine_mia` | Current R-era output. Read by stage 3 |
| `mia_new.csv` | 105 (5 datasets) | not in `paths.py` | **Stray** |
| `miaUnique_new.csv` | 28 | not in `paths.py` | **Stray** |

## Folders

- `by_dataset/`: per-dataset shards. See [most-important-attr-by-dataset.md](most-important-attr-by-dataset.md).
- `r_legacy/`: backup made before the interrupted Python MIA rerun. See [most-important-attr-r-legacy.md](most-important-attr-r-legacy.md).

## Completeness

- `mfeat-factors` is missing from `mia.csv` and `miaUnique.csv`. Its R run stops at `gcvEarth` with `future.globals.maxSize` exceeded.
- The Python port (`kt mia run --no-resume`) has not been run over all datasets. The results here are from R.

## Pair docs

[mia.md](../pairs/mia.md). Checklist: [checklist.md](../checklist.md#stage-2-most-important-attribute-mia).
