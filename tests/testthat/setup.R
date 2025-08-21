# Load project R functions (expects an R/ folder at repo root) and ensure testthat
rdir <- normalizePath(file.path("..", "..","..", "R"), mustWork = FALSE)
if (dir.exists(rdir)) {
  rfiles <- list.files(rdir, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(rfiles, source))
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Please install 'testthat' to run tests.")
}
