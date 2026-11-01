# results/summary_plots/

## Status

**Does not exist.** Nothing has been written here.

## Expected producer

- `summarize_results.R` (line 15) writes summary plots such as `kappa_loss_by_dataset.png` into this folder.
- `paths.py` defines `SUMMARY_PLOTS_DIR`, but the Python `combiners.summarize_results` does not write plots here.

## Action

Either port the summary plots into Python, or remove `SUMMARY_PLOTS_DIR` and drop it from the R-era plan. Decide before regenerating stage 7.

## Pair docs

[summarize.md](../pairs/summarize.md). Checklist: [checklist.md](../checklist.md#stage-7-combine-summarize-compare).
