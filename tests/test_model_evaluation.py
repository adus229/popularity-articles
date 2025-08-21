
from src.model_evaluation import compare_model_results

def test_compare_model_results_orders_by_rmse():
    results = {'A': {'rmse': 1.0, 'mae': 1.0, 'r2': 0.5},
               'B': {'rmse': 0.5, 'mae': 0.5, 'r2': 0.7}}
    table = compare_model_results(results)
    assert table.index[0] == 'B'
