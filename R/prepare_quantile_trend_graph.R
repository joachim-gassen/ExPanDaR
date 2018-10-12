#' @title Prepares a Quantile Trend Graph
#'
#' @description
#' Reads a data frame
#' and plots the quantiles of the specified variable
#' by an ordered factor (normally the time-series indicator)
#'
#' @param df Data frame containing the ordered factor and the numerical variable
#' to be plotted
#' @param ts_id a string containing the column name of the ordered factor
#'   (normally the time-series indicator)
#' @param quantiles a numerical vector containing the quantiles that are to be plotted
#' @param var a string containing the column name of the variable
#'   to be plotted. Defaults to the last numerical variable of the data frame
#'   that is not \code{ts_id}.
#'
#' @return A list containing two items:
#' \describe{
#'  \item{"df"}{A data frame containing the plotted quantiles}
#'  \item{"plot"}{The plot as returned by \code{ggplot}}
#' }
#'
#' @examples
#' prepare_quantile_trend_graph(worldbank, "year", var = "SP.DYN.LE00.IN")$plot +
#'   ggplot2::ylab("Life expectancy at birth world-wide")
#' df <- data.frame(year = floor(stats::time(datasets::EuStockMarkets)),
#'                  DAX = datasets::EuStockMarkets[,"DAX"])
#' graph <- prepare_quantile_trend_graph(df, "year", c(0.05, 0.25, 0.5, 0.75, 0.95))
#' graph$plot
#' @export

prepare_quantile_trend_graph <- function(df, ts_id,
                                         quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                         var = utils::tail(colnames(df[sapply(df, is.numeric) & colnames(df) != ts_id]), n = 1)) {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  if (! ts_id %in% colnames(df)) stop("ts_id need to be in df")
  if (length(var) > 1) stop("var needs to identify a single variable")
  if (! var %in% colnames(df)) stop("var needs to be in df")

  df <- droplevels(df[stats::complete.cases(df[, c(ts_id, var)]), c(ts_id, var)])
  if(!is.numeric(ts_id)) xnum <- suppressWarnings(as.numeric(as.character(df[,ts_id]))) else xnum <- ts_id
  if (anyNA(xnum)) {
    x_is_factor <- TRUE
  } else {
    df[,ts_id] <- xnum
    x_is_factor <- FALSE
  }
  q <- sapply(quantiles,
              function(x){by(df[, which(!colnames(df) %in% ts_id)], df[, ts_id], stats::quantile, na.rm=TRUE,x)})
  if (x_is_factor) q <- data.frame(rownames(q),q) else q <- data.frame(as.numeric(rownames(q)),q)
  colnames(q)[1] <- ts_id
  colnames(q)[2:(length(quantiles) + 1)] <- sprintf("q%02d", quantiles*100)
  gg <- tidyr::gather_(data = q,
                       key_col = "quantile",
                       value_col = colnames(df)[!(ts_id == colnames(df))],
                       gather_cols = colnames(q)[!(ts_id == colnames(q))])
  gg$quantile <- factor(gg$quantile, levels = unique(gg$quantile))
  if (x_is_factor) {
    plot <- ggplot2::ggplot(gg, ggplot2::aes_string(x = ts_id, y=gg[,3], color="quantile", group = "quantile")) +
      ggplot2::stat_summary(fun.y=sum, geom="line") + ggplot2::ylab(colnames(df[ncol(df)])) +
      ggplot2::scale_color_discrete(labels=quantiles)
  } else {
    plot <- ggplot2::ggplot(gg) + ggplot2::ylab(colnames(df[ncol(df)])) +
      ggplot2::geom_line(ggplot2::aes_string(x=ts_id, y=gg[,3], color="quantile")) +
      ggplot2::scale_color_discrete(labels=quantiles)
  }
  list(df = gg, plot = plot)
}

