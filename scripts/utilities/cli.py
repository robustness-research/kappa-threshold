"""Single CLI entry point for the kappa-threshold pipeline"""

from __future__ import annotations

from itertools import product

import typer

from scripts.utilities.combiners import (
    average_folds_original,
    average_folds_train_noise,
    combine_original_by_dataset,
    combine_train_noise_by_dataset,
    compare_original_vs_train_noise,
    summarize_results,
)
from scripts.utilities.config import load_parameters
from scripts.instances_noiser import run_instances_noiser
from scripts.instances_noiser_retrain import run_train_noise
from scripts.mia import run_mia
from scripts.utilities.combiners import combine_mia
from scripts.noise_injector import inject_noise
from scripts.utilities.paths import instances_results_csv, retrained_results_csv
from scripts.utilities.parallel import run_parallel
from scripts.plotting import plot_dataset
from scripts.threshold_finder import run_threshold_finder

app = typer.Typer(help="kappa-threshold pipeline CLI")
mia_app = typer.Typer(help="most-important-attribute extraction")
noise_app = typer.Typer(help="noise injection")
instances_app = typer.Typer(help="instance-noise experiment")
threshold_app = typer.Typer(help="kappa-loss threshold computation")
train_noise_app = typer.Typer(help="instance-noise experiment with retraining")
combine_app = typer.Typer(help="combine/average/compare")
cluster_app = typer.Typer(help="dataset clustering")
plot_app = typer.Typer(help="plots")

app.add_typer(mia_app, name="mia")
app.add_typer(noise_app, name="noise")
app.add_typer(instances_app, name="instances")
app.add_typer(threshold_app, name="threshold")
app.add_typer(train_noise_app, name="retrain")
app.add_typer(combine_app, name="combine")
app.add_typer(plot_app, name="plot")
app.add_typer(cluster_app, name="cluster")


def _filter(values: list[str], selected: list[str] | None) -> list[str]:
    return selected if selected else values


# --- Extract Most Important Attribute ---
@mia_app.command("run")
def mia_run(
    dataset: list[str] = typer.Option(None, help="Restrict to these datasets (default: all)"),
    jobs: int = typer.Option(8, help="Max parallel workers"),
    resume: bool = typer.Option(True, help="Skip datasets already computed"),
):
    params = load_parameters()
    datasets = _filter(params.dataset_name, dataset)
    from scripts.utilities.paths import mia_by_dataset_csv

    run_parallel(
        [(d, params.technique_name) for d in datasets],
        run_mia,
        output_path_fn=lambda d, _t: mia_by_dataset_csv(d),
        max_workers=jobs,
        resume=resume,
    )


@mia_app.command("combine")
def mia_combine(dataset: list[str] = typer.Option(None)):
    params = load_parameters()
    combine_mia(_filter(params.dataset_name, dataset))


# --- Noise injection ---
@noise_app.command("inject")
def noise_inject(
    dataset: list[str] = typer.Option(None),
    jobs: int = typer.Option(8),
    resume: bool = typer.Option(True),
):
    import pandas as pd

    from scripts.utilities.paths import MIA_UNIQUE_CSV, noise_injection_csv

    params = load_parameters()
    datasets = _filter(params.dataset_name, dataset)
    mia_unique = pd.read_csv(MIA_UNIQUE_CSV)

    jobs_list = []
    for d in datasets:
        for mia_feature in mia_unique[mia_unique.dataset_name == d]["most_important_attribute"].unique():
            for noise in params.noise_levels:
                jobs_list.append((d, mia_feature, noise))

    run_parallel(
        jobs_list,
        inject_noise,
        output_path_fn=noise_injection_csv,
        max_workers=jobs,
        resume=resume,
    )


@instances_app.command("run")
def instances_run(
    dataset: list[str] = typer.Option(None),
    model: list[str] = typer.Option(None),
    jobs: int = typer.Option(8),
    resume: bool = typer.Option(True),
):
    params = load_parameters()
    datasets = _filter(params.dataset_name, dataset)
    models = _filter(params.technique_name, model)

    def _job(d, m):
        run_instances_noiser(d, m, params.noise_levels, params.instance_level, resume=resume)

    run_parallel(
        list(product(datasets, models)),
        _job,
        output_path_fn=instances_results_csv,
        max_workers=jobs,
        resume=resume,
    )


@threshold_app.command("find")
def threshold_find(dataset: list[str] = typer.Option(None)):
    params = load_parameters()
    run_threshold_finder(_filter(params.dataset_name, dataset), params.technique_name)


@train_noise_app.command("run")
def train_noise_run(
    dataset: list[str] = typer.Option(None),
    model: list[str] = typer.Option(None),
    jobs: int = typer.Option(8),
    resume: bool = typer.Option(True),
):
    params = load_parameters()
    datasets = _filter(params.dataset_name, dataset)
    models = _filter(params.technique_name, model)

    def _job(d, m, t):
        run_train_noise(d, m, t, params.instance_level, resume=resume)

    def _out(d, m, t):
        return retrained_results_csv(d, m, t)

    run_parallel(
        list(product(datasets, models, params.threshold_level)),
        _job,
        output_path_fn=_out,
        max_workers=jobs,
        resume=resume,
    )


@combine_app.command("run")
def combine_run(dataset: list[str] = typer.Option(None)):
    params = load_parameters()
    datasets = _filter(params.dataset_name, dataset)

    for d in datasets:
        combine_original_by_dataset(d, params.technique_name)
        average_folds_original(d)
        for t in params.threshold_level:
            combine_train_noise_by_dataset(d, params.technique_name, t)
            average_folds_train_noise(d, t)

    for t in params.threshold_level:
        compare_original_vs_train_noise(datasets, t)
        summarize_results(datasets, t)


@plot_app.command("run")
def plot_run(dataset: list[str] = typer.Option(None)):
    params = load_parameters()
    datasets = _filter(params.dataset_name, dataset)
    for d in datasets:
        for t in params.threshold_level:
            plot_dataset(d, t, params.technique_name)


@cluster_app.command("run")
def cluster_run(dataset: list[str] = typer.Option(None), ideal: bool = typer.Option(False, help="Also run the IDEAL holdout variant")):
    from scripts.clustering import run_clustering, run_clustering_ideal

    params = load_parameters()
    datasets = _filter(params.dataset_name, dataset)
    run_clustering(datasets)
    if ideal:
        run_clustering_ideal(datasets)


@cluster_app.command("predict")
def cluster_predict(dataset: str, k: int = typer.Option(4)):
    from scripts.clustering import predict_cluster
    from scripts.utilities.paths import CLUSTERING_PREDICTED_GROUP_CSV, ensure_parent

    pred = predict_cluster(dataset, k=k)
    typer.echo(pred.to_string())
    ensure_parent(CLUSTERING_PREDICTED_GROUP_CSV)
    pred.to_frame().T.to_csv(CLUSTERING_PREDICTED_GROUP_CSV, index=False)


if __name__ == "__main__":
    app()
