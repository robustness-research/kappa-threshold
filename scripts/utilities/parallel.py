"""ProcessPoolExecutor-based fan-out with skip-if-exists resumability."""

from __future__ import annotations

from collections.abc import Callable, Iterable
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path


def run_parallel(
    jobs: Iterable[tuple],
    fn: Callable,
    output_path_fn: Callable[..., Path] | None = None,
    max_workers: int = 8,
    resume: bool = True,
) -> dict[str, int]:
    """Run `fn(*args)` for each `args` tuple in `jobs`, in a process pool.

    If `output_path_fn` is given and `resume=True`, a job is skipped when
    `output_path_fn(*args).exists()` -- mirrors every pipeline stage's
    existing file-existence-check resumability.
    """
    jobs = list(jobs)
    to_run = []
    n_skipped = 0
    for args in jobs:
        if resume and output_path_fn is not None and output_path_fn(*args).exists():
            n_skipped += 1
            continue
        to_run.append(args)

    n_succeeded = 0
    n_failed = 0

    if max_workers == 1:
        for args in to_run:
            try:
                fn(*args)
                n_succeeded += 1
            except Exception as exc:  # noqa: BLE001
                n_failed += 1
                print(f"FAILED {args}: {exc}")
    else:
        with ProcessPoolExecutor(max_workers=max_workers) as executor:
            futures = {executor.submit(fn, *args): args for args in to_run}
            for future in as_completed(futures):
                args = futures[future]
                try:
                    future.result()
                    n_succeeded += 1
                except Exception as exc:  # noqa: BLE001
                    n_failed += 1
                    print(f"FAILED {args}: {exc}")

    summary = {"succeeded": n_succeeded, "failed": n_failed, "skipped": n_skipped}
    print(f"Done: {summary}")
    return summary
