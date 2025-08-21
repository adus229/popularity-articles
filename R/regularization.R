# Regularization models
#' Cross-validated Ridge and predictions on test.
#'
#' @param train_df data.frame (training).
#' @param test_df data.frame (test).
#' @return list(model = cv.glmnet, rmse_min = numeric, rmse_1se = numeric, lambdas = c(min,1se))
fit_ridge_cv <- function(train_set, Y_train) {
  glmnet::cv.glmnet(model.matrix(shares~., data=train_set),
                    Y_train, family="gaussian",
                    nfolds=10, alpha=0, keep=TRUE)
}


#' Cross-validated Lasso and predictions on test.
#'
#' @param train_df data.frame (training).
#' @param test_df data.frame (test).
#' @return list(model = cv.glmnet, rmse_min = numeric, rmse_1se = numeric, lambdas = c(min,1se))
fit_lasso_cv <- function(train_set, Y_train) {
  glmnet::cv.glmnet(model.matrix(shares~., data=train_set),
                    Y_train, family="gaussian",
                    nfolds=10, alpha=1, keep=TRUE)
}
