if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::init(bare = TRUE)

pkgs <- c("glmnet", "randomForest", "mgcv", "stargazer", "ggplot2",
"broom", "FactoMineR", "factoextra","data.table","vtable","ggcorrplot","lares","gridExtra","caret","scales")

renv::install(pkgs)
renv::snapshot(prompt = FALSE)
message("✓ renv full ready. Next time: renv::restore()")
