test_that("OLS fits (full/forward/backward)", {
  df <- make_mock_df(80)
  df <- filter_recent(df) |> drop_columns(c("url","timedelta","weekday_is_sunday","is_weekend"))
  full <- fit_ols_full(df)
  expect_s3_class(full, "lm")
  fw <- fit_ols_forward(df, scope_formula = formula(full))
  expect_s3_class(fw, "lm")
  bw <- fit_ols_backward(full)
  expect_s3_class(bw, "lm")
  sp <- split_train_test(df)
  expect_true(is.numeric(rmse_on(bw, sp$test)))
})

test_that("Autometrics-like model builds from var list", {
  df <- make_mock_df(70)
  df <- filter_recent(df) |> drop_columns(c("url","timedelta","weekday_is_sunday","is_weekend"))
  txt <- "Variables  Coefficient  Std.Error  t-value  t-prob Part.R^2
Constant 1 1 1 0 0
num_hrefs 1 1 1 0 0
data_channel_is_tech 1 1 1 0 0"
  vars <- parse_autometrics_vars(txt)
  mdl <- fit_autometrics_model(df, vars)
  expect_s3_class(mdl, "lm")
})
