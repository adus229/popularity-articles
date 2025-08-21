# Stargazer export utility
#' Export a stargazer comparison table (optional).
#'
#' @param models list of lm objects.
#' @param column_labels character[] labels per model.
#' @param add_lines list of vectors: each vector is c(label, val_m1, val_m2, ...).
#' @param out_path character path where to write .tex.
#' @return invisible(NULL)
export_stargazer_optional <- function(models, file, add_lines=NULL, col_labels=NULL, dep_label="Popularity") {
  if (!requireNamespace("stargazer", quietly=TRUE)) {
    warning("stargazer not installed, skipping export.")
    return(NULL)
  }
  stargazer::stargazer(models,
                       column.labels=col_labels,
                       dep.var.labels=dep_label,
                       add.lines=add_lines,
                       out=file)
}
