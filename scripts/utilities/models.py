"""Default model settings for all tested models to be called upon"""

from __future__ import annotations

from typing import Callable

import numpy as np
import pandas as pd
from sklearn.base import BaseEstimator, ClassifierMixin
from sklearn.compose import ColumnTransformer
from sklearn.discriminant_analysis import (
    LinearDiscriminantAnalysis,
    QuadraticDiscriminantAnalysis,
)
from sklearn.ensemble import GradientBoostingClassifier, RandomForestClassifier
from sklearn.cluster import KMeans
from sklearn.cross_decomposition import PLSRegression
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder
from sklearn.svm import SVC
from sklearn.tree import DecisionTreeClassifier
from sklearn.multiclass import OneVsRestClassifier

from scripts.utilities.data_io import get_cat_col, get_num_col

SEED = 123

try:  # optional dependencies
    import wittgenstein
except ImportError:  # pragma: no cover
    wittgenstein = None
try:
    from pyearth import Earth
except ImportError:  # pragma: no cover
    Earth = None
try:
    from imodels import SkopeRulesClassifier
except ImportError:  # pragma: no cover
    SkopeRulesClassifier = None


class PLSClassifier(BaseEstimator, ClassifierMixin):
    """PLS regression conversion to classifier (analogue of caret 'simpls')."""

    def __init__(self, n_components: int = 3):
        self.n_components = n_components

    def fit(self, X, y):
        y = np.asarray(y)
        self.classes_ = np.unique(y)
        Y = (y[:, None] == self.classes_[None, :]).astype(float)
        n_comp = max(1, min(self.n_components, X.shape[1], X.shape[0] - 1))
        self.pls_ = PLSRegression(n_components=n_comp, scale=False).fit(X, Y)
        return self

    def predict(self, X):
        return self.classes_[np.argmax(self.pls_.predict(X), axis=1)]


class EarthClassifier(BaseEstimator, ClassifierMixin):
    """MARS (analogue of gcvEarth)."""

    def fit(self, X, y):
        y = np.asarray(y)
        self.classes_ = np.unique(y)
        Y = (y[:, None] == self.classes_[None, :]).astype(float)
        self.earth_ = Earth(max_degree=1).fit(X, Y)
        return self

    def predict(self, X):
        return self.classes_[np.argmax(self.earth_.predict(X), axis=1)]


class RuleClassifier(BaseEstimator, ClassifierMixin):
    """RIPPER and IREP."""

    def __init__(self, learner: str = "RIPPER", random_state: int = SEED):
        self.learner = learner
        self.random_state = random_state

    def _frame(self, X):
        return pd.DataFrame(np.asarray(X), columns=[f"x{i}" for i in range(X.shape[1])])

    def fit(self, X, y):
        y = np.asarray(y)
        self.classes_ = np.unique(y)
        df = self._frame(X)
        self.models_ = []
        for c in self.classes_:
            d = df.copy()
            d["_y"] = (y == c)
            m = getattr(wittgenstein, self.learner)(random_state=self.random_state)
            m.fit(d, class_feat="_y", pos_class=True)
            self.models_.append(m)
        return self

    def predict_proba(self, X):
        df = self._frame(X)
        scores = np.column_stack([m.predict_proba(df)[:, 1] for m in self.models_])
        scores = np.clip(scores, 1e-3, None)
        return scores / scores.sum(axis=1, keepdims=True)

    def predict(self, X):
        return self.classes_[np.argmax(self.predict_proba(X), axis=1)]


class LVQClassifier(BaseEstimator, ClassifierMixin):
    """Learning Vector Quantization 1 (caret's 'lvq')."""

    def __init__(self, prototypes_per_class: int = 3, alpha: float = 0.03,
                 epochs: int = 20, random_state: int = SEED):
        self.prototypes_per_class = prototypes_per_class
        self.alpha = alpha
        self.epochs = epochs
        self.random_state = random_state

    def fit(self, X, y):
        X = np.asarray(X, dtype=float)
        y = np.asarray(y)
        rng = np.random.default_rng(self.random_state)
        self.classes_ = np.unique(y)
        protos, labels = [], []
        for c in self.classes_:
            idx = np.flatnonzero(y == c)
            pick = rng.choice(idx, size=min(self.prototypes_per_class, len(idx)), replace=False)
            protos.append(X[pick])
            labels += [c] * len(pick)
        P = np.vstack(protos)
        labels = np.array(labels)
        n_iter = self.epochs * len(X)
        order = rng.integers(0, len(X), size=n_iter)
        for t, i in enumerate(order):
            a = self.alpha * (1 - t / n_iter)
            j = np.argmin(((P - X[i]) ** 2).sum(axis=1))
            sign = 1.0 if labels[j] == y[i] else -1.0
            P[j] += sign * a * (X[i] - P[j])
        self.prototypes_, self.proto_labels_ = P, labels
        return self

    def predict(self, X):
        X = np.asarray(X, dtype=float)
        d = ((X[:, None, :] - self.prototypes_[None, :, :]) ** 2).sum(axis=2)
        return self.proto_labels_[np.argmin(d, axis=1)]


class RBFClassifier(BaseEstimator, ClassifierMixin):
    """RBF network: k-means centres -> Gaussian features -> logistic output layer."""

    def __init__(self, n_centers: int = 20, random_state: int = SEED):
        self.n_centers = n_centers
        self.random_state = random_state

    def _features(self, X):
        d2 = ((np.asarray(X, dtype=float)[:, None, :] - self.centers_[None, :, :]) ** 2).sum(axis=2)
        return np.exp(-d2 / (2 * self.sigma_ ** 2))

    def fit(self, X, y):
        X = np.asarray(X, dtype=float)
        k = max(1, min(self.n_centers, len(X)))
        self.centers_ = KMeans(n_clusters=k, n_init=10, random_state=self.random_state).fit(X).cluster_centers_
        cd = np.sqrt(((self.centers_[:, None, :] - self.centers_[None, :, :]) ** 2).sum(axis=2))
        self.sigma_ = max(cd.max() / np.sqrt(2 * k), 1e-6)
        self.out_ = LogisticRegression(max_iter=2000).fit(self._features(X), y)
        self.classes_ = self.out_.classes_
        return self

    def predict_proba(self, X):
        return self.out_.predict_proba(self._features(X))

    def predict(self, X):
        return self.out_.predict(self._features(X))


# name -> estimator factory (analogue of `model_params` in the R script)
MODELS: dict[str, Callable[[], BaseEstimator]] = {
    "decision_tree_entropy": lambda: DecisionTreeClassifier(criterion="entropy", random_state=SEED),
    "decision_tree_unpruned": lambda: DecisionTreeClassifier(
        criterion="gini", min_samples_split=20, min_samples_leaf=7, random_state=SEED
    ),
    "lda": lambda: LinearDiscriminantAnalysis(),
    "gradient_boosting": lambda: GradientBoostingClassifier(
        n_estimators=100, max_depth=2, learning_rate=0.1, min_samples_leaf=10,
        random_state=SEED,
    ),
    "mlp": lambda: MLPClassifier(hidden_layer_sizes=(5,), max_iter=500, random_state=SEED),
    "logistic_regression": lambda: LogisticRegression(penalty=None, max_iter=2000),
    "gaussian_nb": lambda: GaussianNB(),
    "qda_shrunk": lambda: QuadraticDiscriminantAnalysis(reg_param=0.5),
    "random_forest": lambda: RandomForestClassifier(n_estimators=500, random_state=SEED),
    "decision_tree_gini": lambda: DecisionTreeClassifier(
        criterion="gini", min_samples_split=20, min_samples_leaf=7, ccp_alpha=0.01,
        random_state=SEED,
    ),
    "pls_da": lambda: PLSClassifier(n_components=3),
    "svm_linear": lambda: SVC(kernel="linear", C=1.0, random_state=SEED),
    "svm_rbf": lambda: SVC(kernel="rbf", C=1.0, random_state=SEED),
    "knn": lambda: KNeighborsClassifier(n_neighbors=5),
    "logistic_l2": lambda: LogisticRegression(penalty="l2", C=6.25, max_iter=2000),
    "lvq1": lambda: LVQClassifier(),
    "rbf_network": lambda: RBFClassifier(),
    "ripper": lambda: RuleClassifier("RIPPER"),
    "irep": lambda: RuleClassifier("IREP"),
    "mars": lambda: EarthClassifier(),
    "skope_rules": lambda: OneVsRestClassifier(SkopeRulesClassifier(random_state=SEED)),
}


# model -> (imported object or None, pip package, extra in pyproject.toml)
_REQUIRES = {
    "ripper": (wittgenstein, "wittgenstein", "rules"),
    "irep": (wittgenstein, "wittgenstein", "rules"),
    "mars": (Earth, "py-earth2 (CPython 3.13 only; imports as `pyearth`)", "earth"),
    "skope_rules": (SkopeRulesClassifier, "imodels", "rules"),
}


def is_supported(technique: str) -> bool:
    return technique in MODELS


def _preprocessor(X: pd.DataFrame) -> ColumnTransformer:
    return ColumnTransformer(
        [
            ("num", "passthrough", get_num_col(X)),
            ("cat", OneHotEncoder(handle_unknown="ignore", sparse_output=False), get_cat_col(X)),
        ]
    )


def build_pipeline(technique: str, X: pd.DataFrame) -> Pipeline:
    if technique not in MODELS:
        raise ValueError(f"Unsupported method: {technique}")
    dep, package, extra = _REQUIRES.get(technique, (True, None, None))
    if dep is None:
        raise ImportError(
            f"Model '{technique}' requires {package}, which is not installed. "
            f"Install it with: pip install -e '.[{extra}]'"
        )
    return Pipeline([("prep", _preprocessor(X)), ("model", MODELS[technique]())])
