#' @title Prepares a Quantile Trend Graph
#'
#' @description
#' Reads a data frame containing two variables (an ordered factor and a numeric variable)
#' and plots the quantiles of the numeric variables
#' by the ordered factor.
#'
#' @param df Data frame containing the ordered factor and the numerical variable
#' to be plotted
#' @param xvar a string countaining column name of the ordered factor
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
  # ___BUG__ Not sure how this will work if ts_id factors cannot be coerced to numeric
  df[,xvar] <- as.numeric(gsub("[^0-9\\.]", "", as.character(df[,xvar])))
  q <- sapply(quantiles,
              function(x){by(df[,2], df[, xvar], stats::quantile, na.rm=TRUE,x)})
  q <- data.frame(as.numeric(rownames(q)),q)
  colnames(q)[1] <- xvar
  colnames(q)[2:(length(quantiles) + 1)] <- sprintf("q%02d", quantiles*100)
  gg <- tidyr::gather_(data = q,
                       key_col = "quantile",
                       value_col = colnames(df)[!(xvar == colnames(df))],
                       gather_cols = colnames(q)[!(xvar == colnames(q))])
  plot <- ggplot2::ggplot(gg) + ggplot2::ylab(colnames(df[ncol(df)])) +
    ggplot2::geom_line(ggplot2::aes_string(x=xvar, y=gg[,3], color="quantile"))+
    ggplot2::scale_color_discrete(labels=quantiles)
  list(df = gg, plot = plot)
}

