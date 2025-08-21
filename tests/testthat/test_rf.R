test_that("Random Forest works", {
  df <- make_mock_df(50)
  df <- filter_recent(df)
  sp <- split_train_test(df)
  model <- randomForest::randomForest(x = sp$train[, setdiff(names(sp$train), "shares")], y = sp$train$shares)
  expect_s3_class(model, "randomForest")
})