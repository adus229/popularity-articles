suppressPackageStartupMessages({
  library(glmnet)
  library(MASS)
  library(stargazer)
  library(broom)
  library(FactoMineR)
  library(factoextra)
  library(mgcv)
  library(randomForest)
  library(data.table)
})

if (!exists("DATA_PATH")) DATA_PATH <- "data/articles.csv"
articles_all <- read.csv(DATA_PATH)
articles_all <- articles_all[articles_all[['timedelta']] > 21, ]

articles <- articles_all[, -which(colnames(articles_all) %in% c('url','timedelta','weekday_is_sunday','is_weekend'))]

categorials_names <- c('data_channel_is_lifestyle','data_channel_is_entertainment','data_channel_is_bus','data_channel_is_socmed',
                      'data_channel_is_tech','data_channel_is_world','weekday_is_monday','weekday_is_monday','weekday_is_tuesday',
                      'weekday_is_wednesday','weekday_is_thursday','weekday_is_friday','weekday_is_saturday','weekday_is_sunday',
                      'is_weekend')

numericals_names <- c('n_tokens_title','n_tokens_content','n_unique_tokens','n_non_stop_words',
                     'n_non_stop_unique_tokens','num_hrefs','num_self_hrefs','num_imgs','num_videos','average_token_length',
                     'num_keywords','kw_min_min','kw_max_min','kw_avg_min','kw_min_max','kw_max_max','kw_avg_max','kw_min_avg',
                     'kw_max_avg','kw_avg_avg','self_reference_min_shares','self_reference_max_shares','self_reference_avg_sharess',
                     'LDA_00','LDA_01','LDA_02','LDA_03','LDA_04','global_subjectivity','global_sentiment_polarity','global_rate_positive_words',
                     'global_rate_negative_words','rate_positive_words','rate_negative_words','avg_positive_polarity','min_positive_polarity',
                     'max_positive_polarity','avg_negative_polarity','min_negative_polarity','max_negative_polarity','title_subjectivity',
                     'title_sentiment_polarity','abs_title_subjectivity','abs_title_sentiment_polarity','shares')

variances <- diag(var(articles[, numericals_names]))
too_much_variation <- names(variances[variances > 1000])
for (name in too_much_variation) {
  articles[articles[, name] > 0, name] <- log(articles[articles[, name] > 0, name])
  articles[articles[, name] < 0, name] <- -log(abs(articles[articles[, name] < 0, name]))
}

set.seed(1)
random_selection <- runif(nrow(articles))
train_set <- articles[random_selection <= 0.7, ]
test_set  <- articles[random_selection > 0.7, ]

Y_train <- train_set[, 'shares']
X_train <- train_set[, -which(colnames(train_set) == 'shares')]

get_rmse <- function(model, newdata = test_set) {
  pred <- predict(model, newdata = newdata)
  sqrt(mean((pred - newdata$shares)^2))
}

rmse <- 1:3; aic <- 1:3; nb_var_selected <- 1:3

reg.model <- lm(formula = shares ~ ., data = train_set)

reg.ols_fw <- stepAIC(lm(shares ~ 1, data = train_set), direction = 'forward', trace = FALSE, scope = formula(reg.model))
rmse[1] <- get_rmse(reg.ols_fw); nb_var_selected[1] <- length(reg.ols_fw$coefficients) - 1

reg.ols_bw <- stepAIC(reg.model, direction = 'backward', trace = FALSE)
rmse[2] <- get_rmse(reg.ols_bw); nb_var_selected[2] <- length(reg.ols_bw$coefficients) - 1

autometrics <- data.table::fread('Variables  Coefficient  Std.Error  t-value  t-prob Part.R^2
Constant 6.34564 0.1165 54.5 0.0000 0.0980
num_hrefs 0.00576486 0.0005678 10.2 0.0000 0.0038')
formule_auto <- 'shares ~' ; cols <- c(autometrics$Variables)[-1]
for (col in cols) { formule_auto <- paste0(formule_auto, '+', trimws(col)) }
reg.auto <- lm(as.formula(formule_auto), data = train_set)
rmse[3] <- get_rmse(reg.auto); nb_var_selected[3] <- length(reg.auto$coefficients) - 1
aic <- AIC(reg.ols_fw, reg.ols_bw, reg.auto)$AIC

try(dir.create('reports', recursive = TRUE), silent = TRUE)
try(stargazer(reg.ols_fw, reg.ols_bw, reg.auto,
  column.labels = c('Forward','Backward','Autometrics'),
  dep.var.labels = rep('Popularity',3),
  add.lines = list(c('AIC', round(aic,3)), c('RMSE', round(rmse,3)), c('Nb var', nb_var_selected)),
  out = 'reports/regression_lineaire_autometrics.tex'), silent = TRUE)

reg.ridge_cv <- glmnet::cv.glmnet(model.matrix(shares ~ ., data = train_set), Y_train, family = 'gaussian', nfolds = 10, alpha = 0, keep = TRUE)
pred.ridge <- predict(reg.ridge_cv, newx = model.matrix(shares ~ ., data = test_set), s = c(reg.ridge_cv$lambda.min, reg.ridge_cv$lambda.1se))
mse_ridge <- c(
  sqrt(mean((pred.ridge[,1] - test_set[, 'shares'])^2)),
  sqrt(mean((pred.ridge[,2] - test_set[, 'shares'])^2))
)
ridge_comp_results <- data.frame(lambda = c(reg.ridge_cv$lambda.min, reg.ridge_cv$lambda.1se), RMSE = mse_ridge, row.names = c('Min rule','Std rule'))
try(stargazer(ridge_comp_results, out='reports/ridge_compare_results.tex', summary=FALSE, table.placement='h', flip=TRUE), silent = TRUE)

reg.lasso_cv <- glmnet::cv.glmnet(model.matrix(shares ~ ., data = train_set), Y_train, family = 'gaussian', nfolds = 10, alpha = 1, keep = TRUE)
pred.lasso <- predict(reg.lasso_cv, newx = model.matrix(shares ~ ., data = test_set), s = c(reg.lasso_cv$lambda.min, reg.lasso_cv$lambda.1se))
mse_lasso <- c(
  sqrt(mean((pred.lasso[,1] - test_set[, 'shares'])^2)),
  sqrt(mean((pred.lasso[,2] - test_set[, 'shares'])^2))
)
lasso_comp_results <- data.frame(lambda = c(reg.lasso_cv$lambda.min, reg.lasso_cv$lambda.1se), RMSE = mse_lasso, row.names = c('Min rule','Std rule'))
try(stargazer(lasso_comp_results, out='reports/lasso_compare_results.tex', summary=FALSE, table.placement='h', flip=TRUE), silent = TRUE)

reg.lasso_final <- glmnet::glmnet(X_train, Y_train, family = 'gaussian', alpha = 1, lambda = reg.lasso_cv$lambda.min, standardize = FALSE)
lasso_results <- data.frame(Lasso = round(reg.lasso_final$beta[,1],3)); lasso_results[['var']] <- rownames(lasso_results)
lasso_results <- data.frame(lasso_results[lasso_results$Lasso != 0, ])
ols_results <- data.frame(Ols = round(reg.ols_bw$coefficients,3)); ols_results[['var']] <- rownames(ols_results)
results_ols_lasso <- merge(ols_results, lasso_results, by='var', all=TRUE)
results_ols_lasso <- rbind(results_ols_lasso, c('===========','===========','==========='), c('Number of var', nrow(ols_results)-1, nrow(lasso_results)))
try(stargazer(results_ols_lasso, out='reports/ols_lasso_results.tex', summary=FALSE, table.placement='h', rownames=FALSE), silent = TRUE)

pca <- FactoMineR::PCA(articles[, numericals_names[-which(numericals_names=='shares')]], ncp = length(numericals_names))
pca.percents <- factoextra::get_eigenvalue(pca)
try(stargazer(pca.percents, out='reports/pca_percents.tex', summary=FALSE, table.placement='h'), silent = TRUE)
pca.components <- data.frame(pca$ind$coord[,1:24])
pca.reg_data <- cbind(shares = articles[, 'shares'], pca.components)
pca.reg_full_data <- cbind(pca.reg_data, articles_all[,categorials_names])
train_set_pca <- pca.reg_full_data[random_selection <= 0.7, ]; test_set_pca <- pca.reg_full_data[random_selection > 0.7, ]
reg.pca_full_model <- lm(shares ~ ., data = train_set_pca)
formula_initial <- formula(lm(shares ~ ., data = pca.reg_data))
reg.pca <- stepAIC(lm(formula_initial, data = pca.reg_full_data), direction = 'forward', trace = FALSE, scope = formula(reg.pca_full_model))
get_rmse2 <- function(model, newdata) { pred <- predict(model, newdata = newdata); sqrt(mean((pred - newdata$shares)^2)) }
pca_mse <- get_rmse2(reg.pca, newdata = test_set_pca)
try(stargazer(reg.pca, out='reports/pca_regression.tex', table.placement='h', add.lines=list(c('RMSE', round(pca_mse,3)))), silent = TRUE)

formule <- 'shares~'
for (col in categorials_names[!categorials_names %in% c('weekday_is_sunday','is_weekend')]) { formule <- paste0(formule, '+', col) }
for (col in numericals_names[numericals_names != 'shares']) { formule <- paste0(formule, '+s(', col, ',k=6)') }
reg.gam <- mgcv::gam(as.formula(formule), data = train_set)
gam_rmse <- get_rmse(reg.gam)

reg.rf <- randomForest::randomForest(x = X_train, y = Y_train)

compare_all_final <- data.frame(
  models = c('OLS','Ridge','Lasso','PCA','GAM','Random Forest'),
  RMSE   = c(rmse[2], mse_ridge[1], mse_lasso[1], pca_mse, gam_rmse, get_rmse(reg.rf)),
  var    = c(length(reg.ols_bw$coefficients)-1, NA, NA, length(reg.pca$coefficients)-1, 56, '-')
)
try(stargazer(compare_all_final, out='reports/compare_all_final_models.tex', summary=FALSE, table.placement='h', rownames=FALSE), silent = TRUE)
print(compare_all_final)