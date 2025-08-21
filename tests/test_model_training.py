
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split

from src.feature_engineering import build_preprocessor
from src.model_training import (
    make_linear_regression_pipeline, train_and_evaluate
)

def test_train_and_evaluate_linear():
    # simple linear relationship
    rng = np.random.RandomState(0)
    X = pd.DataFrame({'x': rng.randn(100)})
    y = 3.0 * X['x'] + 2.0 + rng.randn(100) * 0.1

    pre = build_preprocessor(pd.concat([X, y.rename('shares')], axis=1), target_col='shares')
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

    pipe = make_linear_regression_pipeline(pre)
    model, metrics = train_and_evaluate(pipe, X_train, y_train, X_test, y_test)
    assert metrics['rmse'] < 0.5
