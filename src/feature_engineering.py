
from typing import List, Optional
import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

def get_numeric_columns(df: pd.DataFrame, exclude: Optional[List[str]] = None) -> List[str]:
    """Return numeric columns, optionally excluding some columns.

    Parameters
    ----------
    df : pandas.DataFrame
        Input data.
    exclude : list[str] | None
        Column names to exclude from the result.

    Returns
    -------
    list[str]
        Names of numeric columns suitable for modeling.
    """
    exclude = set(exclude or [])
    numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
    return [c for c in numeric_cols if c not in exclude]

def build_numeric_pipeline() -> Pipeline:
    """Create a robust numeric preprocessing pipeline.

    The pipeline imputes missing values with the median and standardizes features.

    Returns
    -------
    sklearn.pipeline.Pipeline
        A preprocessing pipeline for numeric features.
    """
    return Pipeline(steps=[
        ("imputer", SimpleImputer(strategy="median")),
        ("scaler", StandardScaler(with_mean=True, with_std=True)),
    ])

def build_preprocessor(df: pd.DataFrame, target_col: str) -> ColumnTransformer:
    """Create a ColumnTransformer that preprocesses numeric features only.

    This project focuses on numeric features commonly found in the Mashable dataset.
    If you later add categorical features, extend this function with appropriate encoders.

    Parameters
    ----------
    df : pandas.DataFrame
        Input dataframe to infer columns.
    target_col : str
        Name of the target column (will be excluded).

    Returns
    -------
    sklearn.compose.ColumnTransformer
        Column transformer applying numeric preprocessing to numeric columns.
    """
    num_cols = get_numeric_columns(df, exclude=[target_col])
    numeric_pipeline = build_numeric_pipeline()
    return ColumnTransformer(
        transformers=[("num", numeric_pipeline, num_cols)],
        remainder="drop",
        verbose_feature_names_out=False
    )
