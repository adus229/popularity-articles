test_that("GAM formula builds; fit optional", {
  # Try to infer feature names from data
  df <- make_mock_df(80)
  df <- filter_recent(df) |> drop_columns(c("url","timedelta","weekday_is_sunday","is_weekend"))
  # Categorical cols: those that are 0/1 and not shares
  cat_cols <- names(Filter(function(x) is.numeric(x) && all(x %in% c(0,1)), df))
  num_cols <- names(Filter(function(x) is.numeric(x), df))
  num_cols <- unique(c(num_cols, "shares"))
  fm <- build_gam_formula(cat_cols, num_cols)
  expect_s3_class(fm, "formula")
  testthat::skip_if_not_installed("mgcv")
  sp <- split_train_test(df)
  res <- fit_gam_optional(sp$train, sp$test, fm)
  expect_true(is.list(res))
  expect_true(is.numeric(res$rmse))
})
