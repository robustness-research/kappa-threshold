# results/most_important_attr/by_dataset/

## Producer

- Stage 2: `mia.py` (`mia_by_dataset_csv`, `mia_unique_by_dataset_csv`). R: `mia_extractor.R` (lines 151, 156).

## Files (60)

| Pattern | Count | Meaning |
|---|---|---|
| `{d}_mia.csv` | 30 | One row per model: `dataset_name, technique, most_important_attribute` |
| `{d}_miaUnique.csv` | 30 | Unique features for the dataset |

## Completeness

- 30 of 31 datasets. Missing: `mfeat-factors`.

## Notes

- The files match the Python naming in `paths.py`.
- These are the files `mia_combine` merges into the top-level `mia.csv` and `miaUnique.csv`.

## Pair docs

[mia.md](../pairs/mia.md). Checklist: [checklist.md](../checklist.md#stage-2-most-important-attribute-mia).
