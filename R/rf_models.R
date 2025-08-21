# Random Forest model
#' Fit a Random Forest and compute RMSE.
#'
#' @param df_train data.frame.
#' @param df_test data.frame.
#' @return list(model = randomForest, rmse = numeric)
fit_random_forest <- function(X_train, Y_train) {
  randomForest::randomForest(x=X_train, y=Y_train)
}
