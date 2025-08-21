test_that("export_stargazer_optional writes file when available", {
  df <- make_mock_df(60)
  df <- filter_recent(df) |> drop_columns(c("url","timedelta","weekday_is_sunday","is_weekend"))
  m1 <- lm(shares ~ num_hrefs, data = df)
  m2 <- lm(shares ~ num_hrefs + num_imgs, data = df)
  out <- tempfile(fileext = ".tex")
  export_stargazer_optional(
    models = list(m1, m2),
    column_labels = c("m1", "m2"),
    add_lines = list(c("Note", "demo", "demo")),
    out_path = out
  )
  if (requireNamespace("stargazer", quietly = TRUE)) {
    expect_true(file.exists(out))
    expect_true(file.info(out)$size > 0)
  } else {
    succeed("stargazer not installed; export skipped as expected")
  }
})
