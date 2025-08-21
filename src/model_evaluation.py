
from typing import Dict
import pandas as pd

def compare_model_results(results: Dict[str, Dict[str, float]]) -> pd.DataFrame:
    """Convert a dict of model metrics into a sorted DataFrame.

    Parameters
    ----------
    results : dict[str, dict[str, float]]
        Mapping from model name to its metrics (e.g., {'rmse': ..., 'mae': ..., 'r2': ...}).

    Returns
    -------
    pandas.DataFrame
        Table sorted by RMSE ascending.
    """
    df = pd.DataFrame(results).T
    return df.sort_values(by="rmse")
