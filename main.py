
import argparse
import os
import pandas as pd
from sklearn.model_selection import train_test_split

from src.data_loading import load_data, basic_clean, split_features_target
from src.feature_engineering import build_preprocessor
from src.model_training import (
    make_linear_regression_pipeline, make_ridge_pipeline, make_lasso_pipeline,
    make_pca_linear_pipeline, make_spline_ridge_pipeline, make_random_forest_pipeline,
    train_and_evaluate
)
from src.model_evaluation import compare_model_results
from src.utils import set_random_seed, infer_target_column, save_model

def run(data_path: str, target: str = None, test_size: float = 0.2, random_state: int = 42, outdir: str = "models") -> None:
    """Run the full training & evaluation pipeline across multiple models.

    Parameters
    ----------
    data_path : str
        Path to the CSV dataset.
    target : str | None, default None
        Target column name. If None, the function tries to infer it.
    test_size : float, default 0.2
        Proportion of data used for testing.
    random_state : int, default 42
        Random state for splitting.
    outdir : str, default "models"
        Directory where fitted models will be saved.
    """
    set_random_seed(random_state)
    df = load_data(data_path)
    df = basic_clean(df)
    if target is None:
        target = infer_target_column(df)

    X, y = split_features_target(df, target)
    pre = build_preprocessor(df, target)

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, random_state=random_state)

    models = {
        "LinearRegression": make_linear_regression_pipeline(pre),
        "RidgeCV": make_ridge_pipeline(pre),
        "LassoCV": make_lasso_pipeline(pre),
        "PCA+Linear": make_pca_linear_pipeline(pre, n_components=min(24, X_train.shape[1])),
        "SplineRidge": make_spline_ridge_pipeline(pre),
        "RandomForest": make_random_forest_pipeline(pre),
    }

    results = {}
    fitted = {}
    for name, pipe in models.items():
        model, metrics = train_and_evaluate(pipe, X_train, y_train, X_test, y_test)
        results[name] = metrics
        fitted[name] = model

    table = compare_model_results(results)
    print("\nModel comparison (lower RMSE is better):\n")
    print(table.to_string(float_format=lambda x: f"{x:.4f}"))

    os.makedirs(outdir, exist_ok=True)
    for name, model in fitted.items():
        save_model(model, os.path.join(outdir, f"{name}.joblib"))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Train and compare models for article popularity prediction.")
    parser.add_argument("--data", type=str, default="data/articles.csv", help="Path to CSV data.")
    parser.add_argument("--target", type=str, default=None, help="Target column name (default: infer).")
    parser.add_argument("--test_size", type=float, default=0.2, help="Test size fraction.")
    parser.add_argument("--seed", type=int, default=42, help="Random seed.")
    parser.add_argument("--outdir", type=str, default="models", help="Directory to save fitted models.")
    args = parser.parse_args()
    run(args.data, target=args.target, test_size=args.test_size, random_state=args.seed, outdir=args.outdir)
