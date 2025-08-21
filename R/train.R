args <- commandArgs(trailingOnly = TRUE)
DATA_PATH <- if (length(args) >= 2 && args[1] == "--data") args[2] else "data/articles.csv"
if (!dir.exists('reports')) dir.create('reports', recursive = TRUE)
source(file.path('R','pipeline.R'), local = TRUE)