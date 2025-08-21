test_that("split_train_test returns data.frames", {
  df <- make_mock_df(50)
  sp <- split_train_test(df, seed = 42, p = 0.7)
  expect_s3_class(sp$train, "data.frame")
  expect_s3_class(sp$test, "data.frame")
  expect_equal(nrow(sp$train) + nrow(sp$test), nrow(df))
})

test_that("log_high_variance only transforms high-variance cols", {
  set.seed(1)
  df <- data.frame(x = c(-1000, -10, 0, 10, 1000), y = rnorm(5), shares = rnorm(5))
  out <- log_high_variance(df, num_cols = c("x","y","shares"), var_threshold = 100)
  expect_true("x" %in% out$transformed)
  expect_false("y" %in% out$transformed)
})
