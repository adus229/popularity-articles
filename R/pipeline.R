# =============================
# Pipeline for Article Popularity Models
# =============================

# Load all modules
source("R/utils.R")
source("R/ols_models.R")
source("R/regularization.R")
source("R/pca_models.R")
source("R/gam_models.R")
source("R/rf_models.R")
source("R/exports.R")

suppressPackageStartupMessages({
  library(glmnet)
  library(MASS)
  library(stargazer)
  library(FactoMineR)
  library(factoextra)
  library(mgcv)
  library(randomForest)
  library(caret)
  library(data.table)
})

# -----------------------------
# CLI args
# -----------------------------
args <- commandArgs(trailingOnly=TRUE)
data_path <- "data/articles.csv"
seed <- 1
out_dir <- "reports"

if (length(args) > 0) {
  for (i in seq(1, length(args), by=2)) {
    if (args[i] == "--data") data_path <- args[i+1]
    if (args[i] == "--seed") seed <- as.numeric(args[i+1])
    if (args[i] == "--out")  out_dir <- args[i+1]
  }
}

dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)

# -----------------------------
# Load & preprocess
# -----------------------------
articles_all <- load_data(data_path)
articles_all <- filter_recent(articles_all)

articles <- drop_columns(articles_all, c("url","timedelta","weekday_is_sunday","is_weekend"))
feat <- get_feature_names()
articles <- log_high_variance(articles)

splits <- split_train_test(articles, seed)
train_set <- splits$train
test_set  <- splits$test
Y_train <- train_set$shares
X_train <- train_set[, !(colnames(train_set)=="shares")]

# -----------------------------
# Linear Models (OLS)
# -----------------------------
reg.model <- fit_ols_full(train_set)
reg.ols_fw <- fit_ols_forward(train_set, scope=formula(reg.model))
reg.ols_bw <- fit_ols_backward(train_set, reg.model)

# Autometrics (simulate OxMetrics import)
autometrics_df <- fread("data/autometrics.txt") # ⚠ fichier à fournir
reg.auto <- fit_autometrics_model(train_set, autometrics_df)

rmse_vals <- numeric(3)
nb_var_selected <- numeric(3)

rmse_vals[1] <- rmse_on(reg.ols_fw, test_set)
nb_var_selected[1] <- length(reg.ols_fw$coefficients) - 1

rmse_vals[2] <- rmse_on(reg.ols_bw, test_set)
nb_var_selected[2] <- length(reg.ols_bw$coefficients) - 1

rmse_vals[3] <- rmse_on(reg.auto, test_set)
nb_var_selected[3] <- length(reg.auto$coefficients) - 1

aic_vals <- AIC(reg.ols_fw, reg.ols_bw, reg.auto)$AIC

export_stargazer_optional(
  list(reg.ols_fw, reg.ols_bw, reg.auto),
  file=file.path(out_dir, "regression_lineaire_autometrics.tex"),
  col_labels=c("Forward","Backward","Autometrics"),
  add_lines=list(
    c("AIC", round(aic_vals,3)),
    c("RMSE", round(rmse_vals,3)),
    c("Nb var", nb_var_selected)
  )
)

# -----------------------------
# Ridge
# -----------------------------
ridge_cv <- fit_ridge_cv(train_set, Y_train)
pred.ridge <- predict(ridge_cv, newx=model.matrix(shares~., data=test_set),
                      s=c(ridge_cv$lambda.min, ridge_cv$lambda.1se))
mse_ridge <- c(
  sqrt(mean((pred.ridge[,1]-test_set$shares)^2)),
  sqrt(mean((pred.ridge[,2]-test_set$shares)^2))
)

ridge_comp_results <- data.frame(lambda=c(ridge_cv$lambda.min, ridge_cv$lambda.1se),
                                 RMSE=mse_ridge)
row.names(ridge_comp_results) <- c("Min rule","Std rule")

stargazer(ridge_comp_results,
          out=file.path(out_dir,"ridge_compare_results.tex"),
          summary=FALSE, table.placement="h", flip=TRUE)

reg.ridge_final <- glmnet(X_train, Y_train, family="gaussian",
                          alpha=0, lambda=ridge_cv$lambda.min, standardize=FALSE)
stargazer(reg.ridge_final$beta[,1],
          out=file.path(out_dir,"ridge_coefs.tex"),
          summary=FALSE, table.placement="h", flip=TRUE)

# -----------------------------
# Lasso
# -----------------------------
lasso_cv <- fit_lasso_cv(train_set, Y_train)
pred.lasso <- predict(lasso_cv, newx=model.matrix(shares~., data=test_set),
                      s=c(lasso_cv$lambda.min, lasso_cv$lambda.1se))
mse_lasso <- c(
  sqrt(mean((pred.lasso[,1]-test_set$shares)^2)),
  sqrt(mean((pred.lasso[,2]-test_set$shares)^2))
)

lasso_comp_results <- data.frame(lambda=c(lasso_cv$lambda.min, lasso_cv$lambda.1se),
                                 RMSE=mse_lasso)
row.names(lasso_comp_results) <- c("Min rule","Std rule")

stargazer(lasso_comp_results,
          out=file.path(out_dir,"lasso_compare_results.tex"),
          summary=FALSE, table.placement="h", flip=TRUE)

reg.lasso_final <- glmnet(X_train, Y_train, family="gaussian",
                          alpha=1, lambda=lasso_cv$lambda.min, standardize=FALSE)

lasso_results <- data.frame(Lasso=round(reg.lasso_final$beta[,1],3))
lasso_results$var <- rownames(lasso_results)
lasso_results <- lasso_results[lasso_results$Lasso != 0, , drop=FALSE]

ols_results <- data.frame(Ols=round(reg.ols_bw$coefficients,3))
ols_results$var <- rownames(ols_results)

results_ols_lasso <- merge(ols_results, lasso_results, by="var", all=TRUE)
results_ols_lasso <- rbind(results_ols_lasso,
                           c("===========","===========","==========="),
                           c("Number of var",nrow(ols_results)-1,nrow(lasso_results)))

stargazer(results_ols_lasso,
          out=file.path(out_dir,"ols_lasso_results.tex"),
          summary=FALSE, table.placement="h", rownames=FALSE)

# -----------------------------
# PCA
# -----------------------------
pca <- PCA(articles[,feat$numericals[feat$numericals != "shares"]],
           ncp=length(feat$numericals), graph=FALSE)
pca.percents <- get_eigenvalue(pca)
stargazer(pca.percents,
          out=file.path(out_dir,"pca_percents.tex"),
          summary=FALSE, table.placement="h")

pca.components <- data.frame(pca$ind$coord[,1:24])
pca.reg_data <- cbind(shares=articles$shares, pca.components)
pca.reg_full_data <- cbind(pca.reg_data, articles_all[,feat$categorials])

train_set_pca <- pca.reg_full_data[splits$train_idx, ]
test_set_pca  <- pca.reg_full_data[splits$test_idx, ]


reg.pca_full <- lm(shares~., data=train_set_pca)
formula_initial <- formula(lm(shares~., data=pca.reg_data))
reg.pca <- stepAIC(lm(formula_initial, data=pca.reg_full_data),
                   direction="forward", trace=FALSE,
                   scope=formula(reg.pca_full))
pca_mse <- rmse_on(reg.pca, test_set_pca)

stargazer(reg.pca,
          out=file.path(out_dir,"pca_regression.tex"),
          table.placement="h",
          add.lines=list(c("RMSE", round(pca_mse,3))))

# -----------------------------
# GAM
# -----------------------------
gam_formula <- build_gam_formula(feat$categorials, feat$numericals)
reg.gam <- gam(gam_formula, data=train_set)
gam_mse <- rmse_on(reg.gam, test_set)

stargazer(summary(reg.gam)$p.table,
          out=file.path(out_dir,"gam_lineaire.tex"),
          summary=FALSE, table.placement="h")
stargazer(summary(reg.gam)$s.table,
          out=file.path(out_dir,"gam_function.tex"),
          summary=FALSE, table.placement="h")

# -----------------------------
# Random Forest
# -----------------------------
reg.rf <- fit_random_forest(X_train, Y_train)
rmse_rf <- rmse_on(reg.rf, test_set)

# -----------------------------
# Final comparison
# -----------------------------
compare_all_final <- data.frame(
  models=c("OLS","Ridge","Lasso","PCA","GAM","Random Forest"),
  RMSE=c(rmse_vals[2], mse_ridge[1], mse_lasso[1], pca_mse, gam_mse, rmse_rf),
  var=c(length(reg.ols_bw$coefficients)-1,
        nrow(reg.ridge_final$beta),
        nrow(lasso_results),
        length(reg.pca$coefficients)-1,
        56,
        "-")
)

stargazer(compare_all_final,
          out=file.path(out_dir,"compare_all_final_models.tex"),
          summary=FALSE, table.placement="h", rownames=FALSE)

cat("✅ Pipeline finished. All models trained and results exported to", out_dir, "\\n")
