#' @title Prepares a by Group Bar Graph
#'
#' @description
#' Reads a data frame containing a grouping factor and a numerical variable
#' and plots a bar graph of a given statistic of the variable
#' by the grouping factor.
#'
#' @param df Data frame containing the grouping factor and the numerical variable
#' to be plotted
#' @param by_var a string containing the column name of the grouping factor
#' @param var a string containing the column name of the numerical variable
#' @param stat_fun a function to be called on the numerical variable.
#'   Will be called with \code{na.rm = TRUE} to ignore missing values
#' @param order_by_stat a logical value indicating whether you want your bars to be ordered
#'   the value of the statistic (defaults to FALSE)
#' @param color bar color
#'
#' @return A list containing two items:
#' \describe{
#'  \item{"df"}{A data frame containing the statistics by group}
#'  \item{"plot"}{The plot as returned by \code{ggplot}}
#' }
#'
#' @examples
#' data(russell_3000)
#' graph <- prepare_by_group_bar_graph(russell_3000, "sector", "ni_sales", median)
#' graph$plot
#' @export

prepare_by_group_bar_graph <- function(df, by_var, var, stat_fun = mean,
                                       order_by_stat = FALSE, color = "red") {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  stat_var <- paste0('stat_', var)

  df <- data.frame(by_var = df[, by_var], var = df[, var])
  df %>%
    dplyr::group_by(by_var) %>%
    dplyr::summarize(stat = stat_fun(var, na.rm = TRUE)) -> df

  colnames(df) <- c(by_var, stat_var)
  df <- as.data.frame(df)
  df[, by_var] <- as.factor(df[, by_var])
  if (order_by_stat) df[, by_var] <- stats::reorder(df[, by_var], -df[, stat_var])
  else df[, by_var] <- stats::reorder(df[, by_var], nrow(df):1)

  plot <- ggplot2::ggplot(df, ggplot2::aes_string(x = by_var, y = stat_var)) +
    ggplot2::coord_flip() +
    ggplot2::geom_col(fill = color)

  list(df = df, plot = plot)
}

