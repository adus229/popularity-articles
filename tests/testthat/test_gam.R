test_that("GAM fits correctly", {
  df <- make_mock_df(50)
  df <- filter_recent(df)
  feat <- get_feature_names()
  formule <- as.formula(paste("shares ~", paste0("s(", feat$numericals[1:5], ",k=6)", collapse = "+")))
  model <- mgcv::gam(formule, data = df)
  expect_s3_class(model, "gam")
})