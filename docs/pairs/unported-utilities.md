# R utilities with no Python port

These R scripts have no Python equivalent. Each entry says what it does and whether the pipeline still depends on it.

| File | Purpose | Python equivalent | Still needed? |
|---|---|---|---|
| `scripts/utilities/arff_to_csv_converter.R` | Converts `.arff` files to `.csv` in `data/datasets/` | none | The pipeline reads only the `.csv` files, so this is needed once per new dataset |
| `scripts/utilities/rds_to_csv_converter.R` | Converts `.rds` to `.csv` in `data/datasets/` | none | Only if `.rds` inputs are used |
| `scripts/utilities/dataset_info.R` | Prints instance count, attribute types, binary or multiclass | partly `clustering.compute_characteristics` | Reads `data/datasets/arff/{d}.csv`, a path other scripts do not use |
| `scripts/utilities/file_names.R` | Writes `data/files/parameters.csv` and per-parameter `.rds` | `config.load_parameters` reads `data/parameters.csv` directly | No. The output path is stale and `plots.Rmd` still reads it |
| `scripts/utilities/package_installer.R` | Installs R packages | none | Environment setup only |

The dataset converters are the only ones in this list that may still be needed. Confirm which datasets were added from ARFF before deciding.
