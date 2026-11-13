"""Stage 9: dataset clustering.

Ports scripts/clustering/dataset_clustering_26.R (`run_clustering`) and
scripts/clustering/dataset_clustering_IDEAL.R (`run_clustering_ideal`).
Computes per-dataset characteristics (instance count, #numeric/#nominal
attributes, binary vs multiclass), z-scales them, builds a Euclidean
distance matrix, and runs hierarchical clustering (ward linkage, k=4/5/6),
k-means (k=4/5/6), and a 2D MDS embedding for visualization. Output schemas
mirror the R scripts' results/clustering{,_IDEAL}/*.csv files (canonicalized
under results/, not data/results/ as the R scripts mistakenly wrote).

The IDEAL variant clusters only on the 27 datasets NOT in IDEAL_HOLDOUT_DATASETS,
matching a paper's original held-out test split; clustering_comparison.ipynb
compares standard vs IDEAL cluster assignments on the datasets common to both.

MDS: R used MASS::isoMDS (non-metric MDS); sklearn.manifold.MDS defaults to
metric MDS. This is a documented approximation, not a faithful reproduction.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
from scipy.cluster.hierarchy import cut_tree, linkage
from scipy.spatial.distance import pdist, squareform
from sklearn.cluster import KMeans
from sklearn.manifold import MDS
from sklearn.preprocessing import StandardScaler

from scripts.utilities.data_io import load_dataset
from scripts.utilities.paths import (
    CLUSTERING_CENTROIDS_CSV,
    CLUSTERING_CHARACTERISTICS_CSV,
    CLUSTERING_DMATRIX_CSV,
    CLUSTERING_GROUPS_CSV,
    CLUSTERING_KCCA_INFO_CSV,
    CLUSTERING_SCALED_CSV,
    CLUSTERING_IDEAL_CENTROIDS_CSV,
    CLUSTERING_IDEAL_CHARACTERISTICS_CSV,
    CLUSTERING_IDEAL_DMATRIX_CSV,
    CLUSTERING_IDEAL_GROUPS_CSV,
    CLUSTERING_IDEAL_KCCA_INFO_CSV,
    CLUSTERING_IDEAL_SCALED_CSV,
    CLUSTERING_IDEAL_TEST_DATA_CSV,
    CLUSTERING_IDEAL_TEST_SET_CSV,
    CLUSTERING_IDEAL_TRAIN_DMATRIX_CSV,
    CLUSTERING_IDEAL_TRAIN_SET_CSV,
    IDEAL_HOLDOUT_DATASETS,
    ensure_parent,
)

K_VALUES = (4, 5, 6)
FEATURE_COLS = ["instances_n", "attributes_num", "attributes_nom", "dataset_type"]


def compute_characteristics(datasets: list[str]) -> pd.DataFrame:
    rows = []
    for dataset in datasets:
        df = load_dataset(dataset)
        predictors = df.drop(columns=["class"])
        n_numeric = predictors.select_dtypes(include="number").shape[1]
        n_nominal = predictors.shape[1] - n_numeric
        n_classes = df["class"].nunique()
        if n_classes == 2:
            dataset_type = 0
        elif n_classes > 2:
            dataset_type = 1
        else:
            raise ValueError(f"Invalid number of unique classes for {dataset}: {n_classes}")

        rows.append(
            {
                "dataset_name": dataset,
                "instances_n": len(df),
                "attributes_num": n_numeric,
                "attributes_nom": n_nominal,
                "dataset_type": dataset_type,
            }
        )
    return pd.DataFrame(rows)


def _cluster_and_save(
    dataset_names: pd.Series,
    scaled: np.ndarray,
    seed: int,
    centroids_path,
    kcca_info_path,
    groups_path,
) -> dict[str, pd.DataFrame]:
    """Hierarchical + k-means clustering (k=4/5/6) and 2D MDS on `scaled`
    features for the given dataset names, writing centroids/kcca_info/groups
    CSVs matching the R scripts' schemas.
    """
    groups_df = pd.DataFrame({"ID": np.arange(1, len(dataset_names) + 1), "dataset_name": dataset_names.values})

    condensed = pdist(scaled, metric="euclidean")
    Z = linkage(condensed, method="ward")
    for k in K_VALUES:
        groups_df[f"hclust_{k}"] = cut_tree(Z, n_clusters=k).flatten() + 1

    centroid_frames = []
    kcca_info = None
    for k in K_VALUES:
        km = KMeans(n_clusters=k, n_init=25, random_state=seed)
        labels = km.fit_predict(scaled)
        groups_df[f"kmeans_{k}"] = labels + 1

        centroids = pd.DataFrame(km.cluster_centers_, columns=FEATURE_COLS)
        centroids["K"] = k
        centroid_frames.append(centroids)

        if k == 4:
            sizes = pd.Series(labels).value_counts().sort_index()
            total_ss = float(np.sum((scaled - scaled.mean(axis=0)) ** 2))
            kcca_info = pd.DataFrame(
                [
                    {
                        "K": 4,
                        "num_centers": k,
                        "tot_withinss": km.inertia_,
                        "betweenss": total_ss - km.inertia_,
                        **{f"size_cluster_{i + 1}": int(sizes.get(i, 0)) for i in range(k)},
                    }
                ]
            )

    centroids_df = pd.concat(centroid_frames, ignore_index=True)
    ensure_parent(centroids_path)
    centroids_df.to_csv(centroids_path, index=False)
    ensure_parent(kcca_info_path)
    kcca_info.to_csv(kcca_info_path, index=False)

    dmatrix = squareform(condensed)
    mds = MDS(n_components=2, dissimilarity="precomputed", random_state=seed, normalized_stress="auto")
    coords = mds.fit_transform(dmatrix)
    groups_df["coordinate_1"] = coords[:, 0]
    groups_df["coordinate_2"] = coords[:, 1]

    ensure_parent(groups_path)
    groups_df.to_csv(groups_path, index=False)

    return {"centroids": centroids_df, "kcca_info": kcca_info, "groups": groups_df}


def run_clustering(datasets: list[str], seed: int = 1) -> dict[str, pd.DataFrame]:
    characteristics = compute_characteristics(datasets)
    ensure_parent(CLUSTERING_CHARACTERISTICS_CSV)
    characteristics.to_csv(CLUSTERING_CHARACTERISTICS_CSV, index=False)

    scaler = StandardScaler()
    scaled = scaler.fit_transform(characteristics[FEATURE_COLS].astype(float))
    scaled_df = pd.DataFrame(scaled, columns=FEATURE_COLS)
    scaled_df.insert(0, "dataset_name", characteristics["dataset_name"])
    ensure_parent(CLUSTERING_SCALED_CSV)
    scaled_df.to_csv(CLUSTERING_SCALED_CSV, index=False)

    dmatrix_df = pd.DataFrame(
        squareform(pdist(scaled, metric="euclidean")),
        index=characteristics["dataset_name"],
        columns=characteristics["dataset_name"],
    )
    ensure_parent(CLUSTERING_DMATRIX_CSV)
    dmatrix_df.to_csv(CLUSTERING_DMATRIX_CSV)

    clustered = _cluster_and_save(
        characteristics["dataset_name"], scaled, seed,
        CLUSTERING_CENTROIDS_CSV, CLUSTERING_KCCA_INFO_CSV, CLUSTERING_GROUPS_CSV,
    )

    return {"characteristics": characteristics, "scaled": scaled_df, "dmatrix": dmatrix_df, **clustered}


def run_clustering_ideal(
    datasets: list[str], seed: int = 1, holdout: list[str] = IDEAL_HOLDOUT_DATASETS
) -> dict[str, pd.DataFrame]:
    """Cluster only on datasets NOT in `holdout` (default: the paper's 4 held-out
    test datasets), matching scripts/clustering/dataset_clustering_IDEAL.R.
    """
    characteristics = compute_characteristics(datasets)
    ensure_parent(CLUSTERING_IDEAL_CHARACTERISTICS_CSV)
    characteristics.to_csv(CLUSTERING_IDEAL_CHARACTERISTICS_CSV, index=False)

    pd.DataFrame({"dataset_name": holdout}).to_csv(CLUSTERING_IDEAL_TEST_DATA_CSV, index=False)

    scaler = StandardScaler()
    scaled = scaler.fit_transform(characteristics[FEATURE_COLS].astype(float))
    scaled_df = pd.DataFrame(scaled, columns=FEATURE_COLS)
    scaled_df.insert(0, "dataset_name", characteristics["dataset_name"])
    ensure_parent(CLUSTERING_IDEAL_SCALED_CSV)
    scaled_df.to_csv(CLUSTERING_IDEAL_SCALED_CSV, index=False)

    dmatrix_df = pd.DataFrame(
        squareform(pdist(scaled, metric="euclidean")),
        index=characteristics["dataset_name"],
        columns=characteristics["dataset_name"],
    )
    ensure_parent(CLUSTERING_IDEAL_DMATRIX_CSV)
    dmatrix_df.to_csv(CLUSTERING_IDEAL_DMATRIX_CSV)

    is_holdout = characteristics["dataset_name"].isin(holdout)
    train_scaled_df = scaled_df[~is_holdout].reset_index(drop=True)
    test_scaled_df = scaled_df[is_holdout].reset_index(drop=True)
    ensure_parent(CLUSTERING_IDEAL_TRAIN_SET_CSV)
    train_scaled_df.to_csv(CLUSTERING_IDEAL_TRAIN_SET_CSV, index=False)
    test_scaled_df.to_csv(CLUSTERING_IDEAL_TEST_SET_CSV, index=False)

    train_scaled = train_scaled_df[FEATURE_COLS].to_numpy()
    train_dmatrix_df = pd.DataFrame(
        squareform(pdist(train_scaled, metric="euclidean")),
        index=train_scaled_df["dataset_name"],
        columns=train_scaled_df["dataset_name"],
    )
    ensure_parent(CLUSTERING_IDEAL_TRAIN_DMATRIX_CSV)
    train_dmatrix_df.to_csv(CLUSTERING_IDEAL_TRAIN_DMATRIX_CSV)

    clustered = _cluster_and_save(
        train_scaled_df["dataset_name"], train_scaled, seed,
        CLUSTERING_IDEAL_CENTROIDS_CSV, CLUSTERING_IDEAL_KCCA_INFO_CSV, CLUSTERING_IDEAL_GROUPS_CSV,
    )

    return {
        "characteristics": characteristics,
        "scaled": scaled_df,
        "dmatrix": dmatrix_df,
        "train_set": train_scaled_df,
        "test_set": test_scaled_df,
        **clustered,
    }


def predict_cluster(dataset: str, k: int = 4) -> pd.Series:
    """Assign `dataset` to its nearest k-means centroid (K=`k`), z-scaling its
    characteristics using the reference mean/sd stored in characteristics.csv
    from run_clustering(). Port of dataset_group_classifier.R.
    """
    reference = pd.read_csv(CLUSTERING_CHARACTERISTICS_CSV)
    centroids = pd.read_csv(CLUSTERING_CENTROIDS_CSV)

    means = reference[FEATURE_COLS].mean()
    sds = reference[FEATURE_COLS].std(ddof=1)
    if (sds == 0).any():
        raise ValueError("At least one feature has zero standard deviation in reference characteristics")

    characteristics = compute_characteristics([dataset]).iloc[0]
    feature_values = characteristics[FEATURE_COLS].astype(float)
    scaled_features = (feature_values - means) / sds

    centroids_k = centroids[centroids["K"] == k].reset_index(drop=True)
    if centroids_k.empty:
        raise ValueError(f"No centroids found for K={k}")

    diffs = centroids_k[FEATURE_COLS].astype(float).to_numpy() - scaled_features.to_numpy()
    distances = pd.Series(np.sqrt((diffs ** 2).sum(axis=1)))
    predicted_group = int(distances.idxmin()) + 1

    return pd.Series(
        {
            "dataset_name": dataset,
            "K": k,
            "predicted_group": predicted_group,
            "distance_to_group": float(distances.min()),
            **feature_values.to_dict(),
        }
    )
