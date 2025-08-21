# GAM models

#' Build a GAM formula including categorical terms and smooths for numeric terms.
#'
#' @param cat_names character[] categorical column names to include (some may be absent).
#' @param num_names character[] numeric column names (including 'shares').
#' @param excluded_cats character[] categories to exclude (default c("weekday_is_sunday","is_weekend")).
#' @return formula
build_gam_formula <- function(categorials, numericals) {
  formule <- "shares ~"
  for (col in categorials[!(categorials %in% c("weekday_is_sunday","is_weekend"))]) {
    formule <- paste0(formule, "+", col)
  }
  for (col in numericals[numericals != "shares"]) {
    formule <- paste0(formule, "+s(", col, ",k=6)")
  }
  as.formula(formule)
}


#' Fit a GAM using mgcv if available; otherwise return NULL.
#'
#' @param df_train data.frame.
#' @param gam_formula formula from build_gam_formula().
#' @return list(model = gam, rmse = numeric) or NULL if mgcv is unavailable.
fit_gam_optional <- function(train_set, formula) {
  if (!requireNamespace("mgcv", quietly=TRUE)) {
    warning("mgcv not installed, skipping GAM.")
    return(NULL)
  }
  mgcv::gam(formula, data=train_set)
}
