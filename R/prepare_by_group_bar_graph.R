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
#' @param stat_fun a string containing a function to be called on the numerical variable.
#'   Will be called with \code{na.rm = TRUE} to ignore missing values
#' @param order_by_stat a logical value indicating whether you want your bars to be ordered
#'   the value of the statistic (defaults to FALSE)
#' @param color bar color
#'
#' @return A list containing two items:
#' \describe{
#'  \item{"df"}{A data frame containg the statisitics by group}
#'  \item{"plot"}{The plot as returned by ggplot}
#' }
#'
#' @examples
#' data(russell_3000)
#' graph <- prepare_by_group_bar_graph(russell_3000, "sector", "ni_sales", "median")
#' graph$plot
#' @export

prepare_by_group_bar_graph <- function(df, by_var, var, stat_fun = "mean",
                                       color = "red", order_by_stat = FALSE) {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  stat_var <- paste0(stat_fun, '_', var)

  df <- df[, c(by_var, var)]
  df %>%
    dplyr::group_by_(.dots = by_var) %>%
    dplyr::summarize_(.dots = setNames(paste0(stat_fun, '(', var, ', na.rm = TRUE)'), stat_var)) -> df

  df <- as.data.frame(df)
  df[, by_var] <- as.factor(df[, by_var])
  if (order_by_stat) df[, by_var] <- reorder(df[, by_var], -df[, stat_var])
  else df[, by_var] <- reorder(df[, by_var], nrow(df):1)

  plot <- ggplot2::ggplot(df, ggplot2::aes_string(x = by_var, y = paste0(stat_fun, '_', var))) +
    ggplot2::coord_flip() +
    ggplot2::geom_col(fill = color) + ggplot2::ylab(paste(stat_fun, var))

  list(df = df, plot = plot)
}

