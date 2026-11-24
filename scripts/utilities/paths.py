from __future__ import annotations

from pathlib import Path


def _GET_ROOT(start: Path) -> Path:
    for path in [start, *start.parents]:
        if (path / "pyproject.toml").exists():
            return path
    return start


ROOT_PATH = _GET_ROOT(Path(__file__).resolve())

DATA_DIR = ROOT_PATH / "data"
DATASETS_DIR = DATA_DIR / "datasets"
PARAMETERS_CSV = DATA_DIR / "parameters.csv"

RESULTS_DIR = ROOT_PATH / "results"

# --- Extract Most Important Attribute ---
MIA_DIR = RESULTS_DIR / "most_important_attr"
MIA_BY_DATASET_DIR = MIA_DIR / "by_dataset"
MIA_CSV = MIA_DIR / "mia.csv"
MIA_UNIQUE_CSV = MIA_DIR / "miaUnique.csv"


def mia_by_dataset_csv(dataset: str) -> Path:
    return MIA_BY_DATASET_DIR / f"{dataset}_mia.csv"


def mia_unique_by_dataset_csv(dataset: str) -> Path:
    return MIA_BY_DATASET_DIR / f"{dataset}_miaUnique.csv"


# --- Noise injection ---
NOISE_INJECTION_BY_DATASET_DIR = RESULTS_DIR / "noise_injection" / "by_dataset"


def noise_injection_csv(dataset: str, mia_feature: str, noise_level: float) -> Path:
    return NOISE_INJECTION_BY_DATASET_DIR / f"{dataset}_{mia_feature}_noise{noise_level:g}.csv"


# --- Instance-noise injection ---
INSTANCES_DIR = RESULTS_DIR / "instances" / "noised"
INSTANCES_BY_DATASET_DIR = INSTANCES_DIR / "by_dataset"
INSTANCES_VECTORS_DIR = INSTANCES_DIR / "vectors"


def instances_results_csv(dataset: str, model: str) -> Path:
    return INSTANCES_BY_DATASET_DIR / f"{dataset}_{model}_results.csv"


def instances_results_avg_csv(dataset: str) -> Path:
    return INSTANCES_BY_DATASET_DIR / f"{dataset}_results_avg.csv"


def instances_combined_results_csv(dataset: str) -> Path:
    return INSTANCES_BY_DATASET_DIR / f"{dataset}_results.csv"


def instances_vectors_csv(dataset: str, model: str) -> Path:
    return INSTANCES_VECTORS_DIR / f"{dataset}_{model}_all_folds.csv"


# --- Kappa-loss threshold computation ---
THRESHOLD_RESULTS_CSV = RESULTS_DIR / "threshold_results.csv"


# --- Instance-noise injection retrained with thresholds ---
INSTANCES_RETRAINED_DIR = RESULTS_DIR / "instances" / "retrained"
INSTANCES_RETRAINED_BY_DATASET_DIR = INSTANCES_RETRAINED_DIR / "by_dataset"


def _threshold_tag(threshold: float) -> str:
    return f"{threshold:g}"


def retrained_threshold_dir(threshold: float) -> Path:
    return INSTANCES_RETRAINED_BY_DATASET_DIR / f"threshold_{_threshold_tag(threshold)}"


def retrained_results_csv(dataset: str, model: str, threshold: float) -> Path:
    """Per-model shard, merged away by combiners.py into retrained_combined_results_csv."""
    return retrained_threshold_dir(threshold) / f"{dataset}_{model}_{_threshold_tag(threshold)}_results.csv"


def retrained_combined_results_csv(dataset: str, threshold: float) -> Path:
    return retrained_threshold_dir(threshold) / f"{dataset}_results.csv"


def retrained_results_avg_csv(dataset: str, threshold: float) -> Path:
    return retrained_threshold_dir(threshold) / f"{dataset}_results_avg.csv"


# --- Summary / comparison ---
SUMMARY_BY_DATASET_CSV = RESULTS_DIR / "summary_by_dataset.csv"
SUMMARY_BY_MODEL_CSV = RESULTS_DIR / "summary_by_model.csv"
SUMMARY_OVERALL_CSV = RESULTS_DIR / "summary_overall.csv"
COMPARISON_CSV = RESULTS_DIR / "comparison.csv"

# --- Plots ---
PLOTS_DIR = RESULTS_DIR / "plots"
PLOTS_BY_CLUSTER_DIR = PLOTS_DIR / "by_cluster"
SUMMARY_PLOTS_DIR = RESULTS_DIR / "summary_plots"


def _threshold_slug(threshold: float) -> str:
    """0.05 -> '0_05', 0.1 -> '0_1', 0.15 -> '0_15' (matches existing plot filenames)."""
    return _threshold_tag(threshold).replace(".", "_")


def plot_kappa_loss_path(dataset: str, threshold: float) -> Path:
    return PLOTS_DIR / f"{dataset}_threshold_{_threshold_slug(threshold)}_all_kappa_loss.png"


def plot_accuracy_comparison_path(dataset: str, threshold: float) -> Path:
    return PLOTS_DIR / f"{dataset}_threshold_{_threshold_slug(threshold)}_all_accuracy_comparison.png"


def plot_by_cluster_kappa_loss_path(cluster_id: int, threshold: float) -> Path:
    return PLOTS_BY_CLUSTER_DIR / f"cluster_{cluster_id}_threshold_{_threshold_slug(threshold)}_kappa_loss.png"


def plot_by_cluster_accuracy_path(cluster_id: int, threshold: float) -> Path:
    return PLOTS_BY_CLUSTER_DIR / f"cluster_{cluster_id}_threshold_{_threshold_slug(threshold)}_accuracy.png"


# --- Clustering ---
CLUSTERING_DIR = RESULTS_DIR / "clustering"
CLUSTERING_CHARACTERISTICS_CSV = CLUSTERING_DIR / "characteristics.csv"
CLUSTERING_SCALED_CSV = CLUSTERING_DIR / "cl_scaled.csv"
CLUSTERING_DMATRIX_CSV = CLUSTERING_DIR / "dmatrix.csv"
CLUSTERING_CENTROIDS_CSV = CLUSTERING_DIR / "centroids.csv"
CLUSTERING_KCCA_INFO_CSV = CLUSTERING_DIR / "kcca_info.csv"
CLUSTERING_GROUPS_CSV = CLUSTERING_DIR / "groups.csv"
CLUSTERING_PREDICTED_GROUP_CSV = CLUSTERING_DIR / "predicted_group.csv"

HOLDOUT_DATASETS = [
    "eucalyptus",
    "mfeat-pixel",
    "monks-problems-2",
    "steel-plates-fault",
    "wall-robot-navigation",
]


def dataset_csv(dataset: str) -> Path:
    return DATASETS_DIR / f"{dataset}.csv"


def ensure_parent(path: Path) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    return path
