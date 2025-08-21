suppressPackageStartupMessages({
  library(vtable)
  library(ggplot2)
  library(ggcorrplot)
  library(gridExtra)
  library(scales)
})
if (!exists("DATA_PATH")) DATA_PATH <- "data/articles.csv"
df <- read.csv(DATA_PATH, sep = ",", header = TRUE)
df <- df[df$timedelta > 21, ]

var <- c('shares','n_tokens_title','n_tokens_content','num_hrefs','num_imgs','num_videos')
print(utils::head(df[var]))
cat('NAs in selected vars:', sum(is.na(df[var])), '\n')

try(stargazer::stargazer(df[var], omit.summary.stat = c('p25','p75'),
                         out = 'reports/summary_stats.tex'), silent = TRUE)

# Simple plots (will display if interactive)
ggplot(df, aes(x = shares)) + geom_histogram()
ggplot(df, aes(x = num_videos)) + geom_histogram()

# Correlation of integer vars
num_df <- df[, unlist(lapply(df, is.integer))]
if (ncol(num_df) > 1) {
  corr <- stats::cor(num_df)
  ggcorrplot(corr, hc.order = TRUE, type = 'lower', lab = TRUE)
}