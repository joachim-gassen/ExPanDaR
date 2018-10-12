#' @title Prepares a by Group Violin Graph
#'
#' @description
#' Reads a data frame containing a grouping factor and a numerical variable
#' and plots a series of violin graphs by the grouping factor.
#'
#' @param df Data frame containing the grouping factor and the numerical variable
#' to be plotted
#' @param by_var a string containing the column name of the grouping factor
#' @param var a string containing the column name of the numerical variable
#' @param order_by_mean a logical value indicating whether you want your
#'   violins to be ordered by group means (defaults to FALSE)
#' @param group_on_y a logical value indicating whether you want your
#'   violins to be oriented horizontally (defaults to TRUE)
#' @param ... additional parameters that are passed to
#'   \code{\link[ggplot2]{geom_violin}}
#'
#' @return The plot as returned by \code{ggplot2}
#'
#' @examples
#' data(russell_3000)
#' df <- treat_outliers(russell_3000)
#' prepare_by_group_violin_graph(df, "sector", "nioa")
#' @export

prepare_by_group_violin_graph <- function(df, by_var, var,
                                       order_by_mean = FALSE,
                                       group_on_y = TRUE, ...) {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  df <- df[stats::complete.cases(df[, c(by_var, var)]), c(by_var, var)]
  df[, by_var] = as.factor(df[, by_var])
  if (order_by_mean) df[, by_var] <-
    stats::reorder(df[, by_var], -df[, var], mean, na.rm = TRUE)
  plot <- ggplot2::ggplot(df, ggplot2::aes_string(x = by_var, y = var, fill = by_var)) +
    ggplot2::geom_violin(...) + ggplot2::guides(fill = FALSE)
  if (group_on_y) plot + ggplot2::coord_flip() else plot
}

