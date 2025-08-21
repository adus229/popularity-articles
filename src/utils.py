
import os
import random

import numpy as np
import pandas as pd

from joblib import dump

def set_random_seed(seed: int = 42) -> None:
    """Set global random seed for reproducibility across numpy & Python's RNGs.

    Parameters
    ----------
    seed : int, default 42
        Seed value.
    """
    random.seed(seed)
    np.random.seed(seed)

def infer_target_column(df: pd.DataFrame) -> str:
    """Infer the most likely target column name.

    Returns
    -------
    str
        The inferred target column. Raises ValueError if none was found.
    """
    candidates = ["shares", "target", "y"]
    for c in candidates:
        if c in df.columns:
            return c
    # fallback: last column if numeric and not an id-like
    numeric = df.select_dtypes(include=[np.number]).columns.tolist()
    if numeric:
        return numeric[-1]
    raise ValueError("Could not infer target column. Please pass it explicitly.")

def save_model(pipeline, path: str) -> None:
    """Persist a fitted pipeline to disk using joblib.

    Parameters
    ----------
    pipeline : sklearn.pipeline.Pipeline
        Trained pipeline.
    path : str
        Destination path ending with .joblib.
    """
    os.makedirs(os.path.dirname(path), exist_ok=True)
    dump(pipeline, path)
