#' @title Prepares a Graph Displaying Missing Values in Panel Data
#'
#' @description
#' Displays a heatmap of missing value frequency across the panel
#'
#' @param df Data frame containing the data. Only variables containing
#'   numerical or logical data will be used.
#' @param period A string containing the name of the factor indicating the time dimension.
#' @return A ggplot2 graph
#'
#' @details
#'   This was inspireed by a blog post of Rense Nieuenhuis. Thanks!
#'   http://www.rensenieuwenhuis.nl/r-sessions-30-visualizing-missing-values/
#'
#' @examples
#' df <- data.frame(year = as.ordered(floor(time(EuStockMarkets))), EuStockMarkets)
#' period <- "year"
#' prepare_missing_values_graph(df, period="year")
#' @export

prepare_missing_values_graph <- function(df, period) {
  # Make devtools:check() and CRAN happy
  value <- NULL
  if(! is.data.frame(df)) stop("df needs to be a dataframe")
  if (! period %in% names(df)) stop("'period' needs to be present in data frame 'df'")
  df <- cbind(df[period], df[sapply(df, is.logical) | sapply(df, is.numeric)])

  nas <- matrix(ncol=ncol(df) - 1, nrow=nlevels(df[,period]))
  for (i in 2:ncol(df))
  {
    nas[,i - 1] <- tapply(df[,i], df[,period], function(x) sum(is.na(x)) / length(x))
  }
  mv <- data.frame(levels(df[, period]), nas)
  names(mv) <- c(period, names(df)[2:ncol(df)])
  mv <- tidyr::gather_(mv, key_col = "variable", value_col = "value",
                       gather_cols = names(df)[2:ncol(df)])
  mv$variable <- factor(mv$variable, levels = names(df)[2:ncol(df)])

  ggplot2::ggplot(mv, ggplot2::aes_string("variable", period)) +
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
