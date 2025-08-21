test_that("PCA runs and returns object", {
  df <- make_mock_df(40)
  df <- filter_recent(df)
  feat <- get_feature_names()
  pca <- FactoMineR::PCA(df[, feat$numericals[-length(feat$numericals)]], ncp = 5, graph = FALSE)
  expect_true("PCA" %in% class(pca))
})