test_that("PCA regression works (FactoMineR or prcomp)", {
  df <- make_mock_df(90)
  df <- filter_recent(df) |> drop_columns(c("url","timedelta","weekday_is_sunday","is_weekend"))
  sp <- split_train_test(df, seed = 11)
  # include 'shares' in num cols
  num_cols <- c(colnames(df)[sapply(df, is.numeric)])
  if (!"shares" %in% num_cols) num_cols <- c(num_cols, "shares")
  out <- fit_pca_regression(sp$train, sp$test, num_cols = num_cols, k = 10)
  expect_true(is.list(out))
  expect_s3_class(out$model, "lm")
  expect_true(is.numeric(out$rmse))
})
