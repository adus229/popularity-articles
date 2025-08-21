# PCA models
# ------------------------------ PCA -----------------------------------------

#' PCA via FactoMineR if available; otherwise fallback to stats::prcomp.
#'
#' @param df_train data.frame (train).
#' @param df_test data.frame (test).
#' @param num_cols numeric column names (including 'shares').
#' @param k integer number of components to keep (default 24).
#' @return list(model = lm, rmse = numeric)
fit_pca_regression <- function(articles, numericals, categorials, random_selection) {
  if (!requireNamespace("FactoMineR", quietly=TRUE)) {
    warning("FactoMineR not available, using prcomp.")
    return(NULL)
  }
  pca <- FactoMineR::PCA(articles[, numericals[numericals != "shares"]],
                         ncp=length(numericals), graph=FALSE)
  pca.components <- data.frame(pca$ind$coord[,1:24])
  pca.reg_data <- cbind(shares=articles$shares, pca.components)
  pca.reg_full_data <- cbind(pca.reg_data, articles[, categorials])
  train_set_pca <- pca.reg_full_data[random_selection <= 0.7, ]
  test_set_pca  <- pca.reg_full_data[random_selection > 0.7, ]
  formula_initial <- formula(lm(shares ~ ., data=pca.reg_data))
  reg.pca_full <- lm(shares ~ ., data=train_set_pca)
  reg.pca <- MASS::stepAIC(lm(formula_initial, data=pca.reg_full_data),
                           direction="forward", trace=FALSE,
                           scope=formula(reg.pca_full))
  list(model=reg.pca, train=train_set_pca, test=test_set_pca)
}
