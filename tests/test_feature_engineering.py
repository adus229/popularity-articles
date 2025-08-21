
import pandas as pd
from src.feature_engineering import get_numeric_columns, build_preprocessor

def test_get_numeric_columns():
    df = pd.DataFrame({'num': [1,2,3], 'cat': ['a','b','c'], 'shares':[1,2,3]})
    cols = get_numeric_columns(df, exclude=['shares'])
    assert cols == ['num']

def test_build_preprocessor_runs():
    df = pd.DataFrame({'x':[1.0,2.0,3.0], 'shares':[10,20,30]})
    pre = build_preprocessor(df, target_col='shares')
    # should provide a get_feature_names_out method after fitting
    pre.fit(df[['x']], df['shares'])
    names = pre.get_feature_names_out()
    assert len(names) == 1
