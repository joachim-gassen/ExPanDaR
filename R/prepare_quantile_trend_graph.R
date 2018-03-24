#' @title Prepares a Quantile Trend Graph
#'
#' @description
#' Reads a data frame containing two variables (an ordered factor and a numeric variable)
#' and plots the quantiles of the numeric variables
#' by the ordered factor.
#'
#' @param df Data frame containing the ordered factor and the numerical variable
#' to be plotted
#' @param xvar a string containing the column name of the ordered factor
#' @param quantiles a numerical vector containing the quantiles that are to be plotted
#'
#' @return A list containing two items:
#' \describe{
#'  \item{"df"}{A data frame containg the plotted means and standard errors}
#'  \item{"plot}{The plot as returned by ggplot}
#' }
#'
#' @examples
#' df <- data.frame(year = floor(stats::time(datasets::EuStockMarkets)),
#'                  DAX = datasets::EuStockMarkets[,"DAX"])
#' graph <- prepare_quantile_trend_graph(df, "year", c(0.05, 0.25, 0.5, 0.75, 0.95))
#' graph$plot
#' @export

prepare_quantile_trend_graph <- function(df, xvar, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  if(!is.numeric(xvar)) xnum <- suppressWarnings(as.numeric(as.character(df[,xvar]))) else xnum <- xvar
  if (anyNA(xnum)) {
    x_is_factor <- TRUE
  } else {
    df[,xvar] <- xnum
    x_is_factor <- FALSE
  }
  q <- sapply(quantiles,
              function(x){by(df[, which(!colnames(df) %in% xvar)], df[, xvar], stats::quantile, na.rm=TRUE,x)})
  if (x_is_factor) q <- data.frame(rownames(q),q) else q <- data.frame(as.numeric(rownames(q)),q)
  colnames(q)[1] <- xvar
  colnames(q)[2:(length(quantiles) + 1)] <- sprintf("q%02d", quantiles*100)
  gg <- tidyr::gather_(data = q,
                       key_col = "quantile",
                       value_col = colnames(df)[!(xvar == colnames(df))],
                       gather_cols = colnames(q)[!(xvar == colnames(q))])
  gg$quantile <- factor(gg$quantile, levels = unique(gg$quantile))
  if (x_is_factor) {
    plot <- ggplot2::ggplot(gg, ggplot2::aes_string(x = xvar, y=gg[,3], color="quantile", group = "quantile")) +
      ggplot2::stat_summary(fun.y=sum, geom="line") + ggplot2::ylab(colnames(df[ncol(df)])) +
      ggplot2::scale_color_discrete(labels=quantiles)
  } else {
    plot <- ggplot2::ggplot(gg) + ggplot2::ylab(colnames(df[ncol(df)])) +
      ggplot2::geom_line(ggplot2::aes_string(x=xvar, y=gg[,3], color="quantile")) +
      ggplot2::scale_color_discrete(labels=quantiles)
  }
  list(df = gg, plot = plot)
}

