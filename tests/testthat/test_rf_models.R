test_that("Random Forest fits and returns RMSE", {
  testthat::skip_if_not_installed("randomForest")
  df <- make_mock_df(90)
  df <- filter_recent(df) |> drop_columns(c("url","timedelta","weekday_is_sunday","is_weekend"))
  sp <- split_train_test(df)
  rf <- fit_random_forest(sp$train, sp$test)
  expect_true(is.list(rf))
  expect_true(is.numeric(rf$rmse))
})
