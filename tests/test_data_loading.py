
import pandas as pd
from src.data_loading import load_data, basic_clean, split_features_target

def test_basic_clean_and_split(tmp_path):
    df = pd.DataFrame({
        'a': [1, 1, 2],
        'b': [3, 3, 4],
        'shares': [10, 10, 20],
    })
    p = tmp_path / 'toy.csv'
    df.to_csv(p, index=False)

    df_loaded = load_data(str(p))
    df_clean = basic_clean(df_loaded)
    assert len(df_clean) == 2  # duplicates removed

    X, y = split_features_target(df_clean, 'shares')
    assert 'shares' not in X.columns
    assert y.name == 'shares'
