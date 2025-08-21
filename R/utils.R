# Utility functions

#' Load CSV data.
#'
#' @param path character, path to the CSV (default "data/articles.csv").
#' @return data.frame with loaded data.
load_data <- function(path) {
  #' Load CSV data file
  return(read.csv(path))
}
#' Filter out very recent rows (<= 21 days).
#'
#' @param df data.frame with at least a "timedelta" column.
#' @param min_days integer threshold (default 21).
#' @return filtered data.frame.
filter_recent <- function(df, days=21) {
  #' Remove rows with timedelta <= days
  return(df[df$timedelta > days, ])
}

#' Drop columns considered useless for modeling.
#'
#' @param df data.frame.
#' @param cols character vector of columns to drop.
#' @return data.frame without the specified columns.
drop_columns <- function(df, cols) {
  #' Drop columns by name
  return(df[, !(colnames(df) %in% cols)])
}

#' Return the canonical categorical and numerical variable names used in the project.
#'
#' @return list(cat = <character[]>, num = <character[]>)
get_feature_names <- function() {
  #' Return lists of categorical and numerical variable names
  categorials <- c("data_channel_is_lifestyle","data_channel_is_entertainment",
                   "data_channel_is_bus","data_channel_is_socmed",
                   "data_channel_is_tech","data_channel_is_world",
                   "weekday_is_monday","weekday_is_tuesday",
                   "weekday_is_wednesday","weekday_is_thursday",
                   "weekday_is_friday","weekday_is_saturday","weekday_is_sunday",
                   "is_weekend")
  numericals <- c("n_tokens_title","n_tokens_content","n_unique_tokens",
                  "n_non_stop_words","n_non_stop_unique_tokens","num_hrefs",
                  "num_self_hrefs","num_imgs","num_videos","average_token_length",
                  "num_keywords","kw_min_min","kw_max_min","kw_avg_min","kw_min_max",
                  "kw_max_max","kw_avg_max","kw_min_avg","kw_max_avg","kw_avg_avg",
                  "self_reference_min_shares","self_reference_max_shares",
                  "self_reference_avg_sharess","LDA_00","LDA_01","LDA_02","LDA_03",
                  "LDA_04","global_subjectivity","global_sentiment_polarity",
                  "global_rate_positive_words","global_rate_negative_words",
                  "rate_positive_words","rate_negative_words",
                  "avg_positive_polarity","min_positive_polarity",
                  "max_positive_polarity","avg_negative_polarity",
                  "min_negative_polarity","max_negative_polarity",
                  "title_subjectivity","title_sentiment_polarity",
                  "abs_title_subjectivity","abs_title_sentiment_polarity","shares")
  return(list(categorials=categorials, numericals=numericals))
}


#' Log-transform high-variance numeric columns (|var| > threshold).
#'
#' @param df data.frame.
#' @param num_cols character[] numeric column names.
#' @param var_threshold numeric variance threshold (default 1000).
#' @return list(df = transformed data.frame, transformed = character[] columns transformed)
log_high_variance <- function(df, threshold=1000) {
  #' Log-transform columns with variance > threshold
  vars <- diag(var(df))
  high_var <- names(vars[vars > threshold])
  for (name in high_var) {
    df[df[, name] > 0, name] <- log(df[df[, name] > 0, name])
    df[df[, name] < 0, name] <- -log(abs(df[df[, name] < 0, name]))
  }
  return(df)
}

#' Split dataset into train / test using a random uniform mask.
#'
#' @param df data.frame.
#' @param p_train numeric in (0,1), default 0.7.
#' @param seed integer seed, default 1.
#' @return list(train = data.frame, test = data.frame)
split_train_test <- function(df, seed=1, p=0.7) {
  set.seed(seed)
  random_selection <- runif(nrow(df))
  train_idx <- which(random_selection <= p)
  test_idx  <- which(random_selection > p)
  return(list(train=df[train_idx, ],
              test=df[test_idx, ],
              train_idx=train_idx,
              test_idx=test_idx))
}


#' Compute RMSE of a model on a new dataset with 'shares' target.
#'
#' @param model any model with predict().
#' @param newdata data.frame containing 'shares'.
#' @return numeric RMSE value.
rmse_on <- function(model, newdata) {
  pred <- predict(model, newdata=newdata)
  sqrt(mean((pred - newdata$shares)^2))
}
