#' @title Prepares a Graph Displaying Missing Values in Panel Data
#'
#' @description
#' Displays a heatmap of missing value frequency across the panel
#'
#' @param df Data frame containing the data. Only variables containing
#'   numerical or logical data will be used.
#' @param ts_id A string containing the name of the variable indicating the time dimension.
#'   Needs to be coercible into an ordered factor.
#' @return A ggplot2 graph
#'
#' @details
#'   This was inspireed by a blog post of Rense Nieuenhuis. Thanks!
#'   http://www.rensenieuwenhuis.nl/r-sessions-30-visualizing-missing-values/
#'
#' @examples
#' df <- data.frame(year = as.ordered(floor(time(EuStockMarkets))), EuStockMarkets)
#' ts_id <- "year"
#' prepare_missing_values_graph(df, ts_id="year")
#' @export

prepare_missing_values_graph <- function(df, ts_id) {
  # Make devtools:check() and CRAN happy
  value <- NULL
  if(! is.data.frame(df)) stop("df needs to be a dataframe")
  if (! ts_id %in% names(df)) stop("'ts_id' needs to be present in data frame 'df'")
  df[,ts_id] <- as.ordered(df[,ts_id])
  df <- cbind(df[ts_id], df[sapply(df, is.logical) | sapply(df, is.numeric)])

  nas <- matrix(ncol=ncol(df) - 1, nrow=nlevels(df[,ts_id]))
  for (i in 2:ncol(df))
  {
    nas[,i - 1] <- tapply(df[,i], df[,ts_id], function(x) sum(is.na(x)) / length(x))
  }
  mv <- data.frame(levels(df[, ts_id]), nas)
  names(mv) <- c(ts_id, names(df)[2:ncol(df)])
  mv <- tidyr::gather_(mv, key_col = "variable", value_col = "value",
                       gather_cols = names(df)[2:ncol(df)])
  mv$variable <- factor(mv$variable, levels = names(df)[2:ncol(df)])

  ggplot2::ggplot(mv, ggplot2::aes_string("variable", ts_id)) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90),
                   axis.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 8),
                   legend.title = ggplot2::element_text(size = 8)) +
    ggplot2::labs(fill = "% missing\n") +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour="white") +
    ggplot2::scale_fill_continuous(labels=scales::percent)
}
