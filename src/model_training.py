
from typing import Dict, Tuple
import numpy as np
from sklearn.linear_model import LinearRegression, RidgeCV, LassoCV
from sklearn.pipeline import Pipeline
from sklearn.decomposition import PCA
from sklearn.preprocessing import SplineTransformer
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score,root_mean_squared_error

def make_linear_regression_pipeline(preprocessor) -> Pipeline:
    """Build a pipeline with preprocessing and ordinary least squares regression.

    Parameters
    ----------
    preprocessor : sklearn.compose.ColumnTransformer
        Preprocessing steps to apply before modeling.

    Returns
    -------
    sklearn.pipeline.Pipeline
        End-to-end pipeline for linear regression.
    """
    return Pipeline(steps=[("pre", preprocessor), ("model", LinearRegression())])

def make_ridge_pipeline(preprocessor) -> Pipeline:
    """Build a pipeline with preprocessing and Ridge regression with CV over alphas.

    Returns
    -------
    sklearn.pipeline.Pipeline
    """
    alphas = np.logspace(-3, 3, 30)
    return Pipeline(steps=[("pre", preprocessor), ("model", RidgeCV(alphas=alphas, store_cv_values=False))])

def make_lasso_pipeline(preprocessor) -> Pipeline:
    """Build a pipeline with preprocessing and Lasso regression with internal CV.

    Returns
    -------
    sklearn.pipeline.Pipeline
    """
    alphas = np.logspace(-3, 1, 30)
    return Pipeline(steps=[("pre", preprocessor), ("model", LassoCV(alphas=alphas, max_iter=10000, n_jobs=None))])

def make_pca_linear_pipeline(preprocessor, n_components: int = 24) -> Pipeline:
    """Build a pipeline with preprocessing, PCA for dimensionality reduction, and Linear Regression.

    Parameters
    ----------
    n_components : int, default 24
        Number of principal components to keep.
    """
    return Pipeline(steps=[
        ("pre", preprocessor),
        ("pca", PCA(n_components=n_components, random_state=42)),
        ("model", LinearRegression())
    ])

def make_spline_ridge_pipeline(preprocessor, n_knots: int = 5, degree: int = 3) -> Pipeline:
    """Build a "GAM-like" pipeline using spline basis expansion + Ridge.

    Notes
    -----
    This mimics a univariate-spline GAM using scikit-learn's ``SplineTransformer``
    applied feature-wise, followed by Ridge regression. It captures smooth non-linearities
    without external dependencies.
    """
    spline = SplineTransformer(n_knots=n_knots, degree=degree, include_bias=False)
    return Pipeline(steps=[
        ("pre", preprocessor),
        ("spline", spline),
        ("model", RidgeCV(alphas=np.logspace(-3, 3, 30)))
    ])

def make_random_forest_pipeline(preprocessor, n_estimators: int = 300, random_state: int = 42) -> Pipeline:
    """Build a pipeline with preprocessing and a Random Forest regressor.

    Parameters
    ----------
    n_estimators : int, default 300
        Number of trees.
    random_state : int, default 42
        Random seed for reproducibility.
    """
    rf = RandomForestRegressor(
        n_estimators=n_estimators,
        random_state=random_state,
        n_jobs=-1,
        oob_score=False
    )
    return Pipeline(steps=[("pre", preprocessor), ("model", rf)])

def evaluate(y_true, y_pred) -> Dict[str, float]:
    """Compute regression metrics.

    Returns
    -------
    dict
        Dictionary containing RMSE, MAE, and R2 scores.
    """
    rmse = mean_squared_error(y_true, y_pred)
    mae = mean_absolute_error(y_true, y_pred)
    r2 = r2_score(y_true, y_pred)
    return {"rmse": float(rmse), "mae": float(mae), "r2": float(r2)}

def train_and_evaluate(pipeline: Pipeline, X_train, y_train, X_test, y_test) -> Tuple[Pipeline, Dict[str, float]]:
    """Fit a pipeline and evaluate it on a hold-out set.

    Parameters
    ----------
    pipeline : sklearn.pipeline.Pipeline
        Model pipeline to fit.
    X_train, y_train, X_test, y_test
        Train/test splits.

    Returns
    -------
    (fitted_pipeline, metrics) : tuple
        The trained pipeline and a metrics dictionary.
    """
    pipeline.fit(X_train, y_train)
    preds = pipeline.predict(X_test)
    return pipeline, evaluate(y_test, preds)
