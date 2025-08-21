# OLS models

#' Fit a full OLS model (shares ~ .).
#' @param df data.frame (training).
#' @return lm object.
fit_ols_full <- function(train_set) {
  lm(shares ~ ., data=train_set)
}


#' Fit forward stepwise OLS using MASS::stepAIC (fallback to stats::step).
#' @param df data.frame (training).
#' @param scope_formula formula defining the full model.
#' @return lm object.
fit_ols_forward <- function(train_set, scope) {
  if (!requireNamespace("MASS", quietly=TRUE)) {
    warning("MASS not installed, skipping stepAIC forward.")
    return(NULL)
  }
  MASS::stepAIC(lm(shares ~ 1, data=train_set), direction="forward", trace=FALSE, scope=scope)
}

#' Fit backward stepwise OLS using MASS::stepAIC (fallback to stats::step).
#' @param full_model lm object for the full model.
#' @return lm object.
fit_ols_backward <- function(train_set, full_model) {
  if (!requireNamespace("MASS", quietly=TRUE)) {
    warning("MASS not installed, skipping stepAIC backward.")
    return(NULL)
  }
  MASS::stepAIC(full_model, direction="backward", trace=FALSE)
}


#' Parse Autometrics variable list pasted as a table literal.
#'
#' @param txt character, OxMetrics-like table with header "Variables".
#' @return character[] of variable names to include (excluding constant).
parse_autometrics_vars <- function(autometrics_df) {
  vars <- c(autometrics_df$Variables)[-1]
  formule_auto <- paste("shares ~", paste(trimws(vars), collapse="+"))
  as.formula(formule_auto)
}

#' Fit an OLS model using a variable set (Autometrics-like).
#'
#' @param df data.frame (training).
#' @param vars character[] variable names to include.
#' @return lm object.
fit_autometrics_model <- function(train_set, autometrics_df) {
  formula <- parse_autometrics_vars(autometrics_df)
  lm(formula, data=train_set)
}
