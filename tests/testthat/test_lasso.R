test_that("fit_lasso produces glmnet object", {
  df <- make_mock_df(50)
  df <- filter_recent(df)
  sp <- split_train_test(df)
  y <- sp$train$shares
  x <- as.matrix(sp$train[ , setdiff(names(sp$train), "shares")])
  model <- glmnet::glmnet(x, y, alpha = 1)
  expect_s3_class(model, "glmnet")
})