
import pandas as pd
from typing import Tuple

def load_data(filepath: str, low_memory: bool = True) -> pd.DataFrame:
    """Load dataset from a CSV file.

    Parameters
    ----------
    filepath : str
        Path to the CSV file.
    low_memory : bool, default True
        Forwarded to ``pandas.read_csv`` to reduce memory usage on wide files.

    Returns
    -------
    pandas.DataFrame
        Loaded dataframe.
    """
    return pd.read_csv(filepath, low_memory=low_memory)

def basic_clean(df: pd.DataFrame) -> pd.DataFrame:
    """Perform simple, safe cleaning operations (e.g., drop duplicates).

    Parameters
    ----------
    df : pandas.DataFrame
        Raw dataframe.

    Returns
    -------
    pandas.DataFrame
        Cleaned dataframe with duplicates removed and original index reset.
    """
    return df.drop_duplicates().reset_index(drop=True)

def split_features_target(df: pd.DataFrame, target_col: str) -> Tuple[pd.DataFrame, pd.Series]:
    """Split a dataframe into features and target.

    Parameters
    ----------
    df : pandas.DataFrame
        Input data.
    target_col : str
        Name of the target column.

    Returns
    -------
    (X, y) : tuple[pandas.DataFrame, pandas.Series]
        Feature matrix X and target vector y.
    """
    if target_col not in df.columns:
        raise ValueError(f"Target column '{target_col}' not found in dataframe.")
    y = df[target_col]
    X = df.drop(columns=[target_col])
    return X, y
