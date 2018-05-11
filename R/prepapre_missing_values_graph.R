#' @title Prepares a Graph Displaying Missing Values in Panel Data
#'
#' @description
#' Displays a heatmap of missing value frequency across the panel
#'
#' @param df Data frame containing the data.
#' @param ts_id A string containing the name of the variable indicating the time dimension.
#'   Needs to be coercible into an ordered factor.
#' @param no_factors A logical variable indicating whether you want to limit the plot to
#'   logical and numerical variables. Defaults to FALSE.
#'
#' @return A \code{ggplot2} plot.
#'
#' @details
#'   This was inspired by a
#'   \href{http://www.rensenieuwenhuis.nl/r-sessions-30-visualizing-missing-values/}{blog post of Rense Nieuwenhuis}.
#'   Thanks!
#'
#'
#' @examples
#' prepare_missing_values_graph(russell_3000, ts_id="period")
#' @export

prepare_missing_values_graph <- function(df, ts_id, no_factors = FALSE) {
  # Make devtools:check() and CRAN happy
  value <- NULL
  if(! is.data.frame(df)) stop("df needs to be a dataframe")
  if (! ts_id %in% names(df)) stop("'ts_id' needs to be present in data frame 'df'")
  if (any(is.na(df[,ts_id]))) stop("'ts_id' must not contain missing values")
  if (! is.logical(no_factors)) stop("'no_factors' needs to be a logcial scalar")

  df <- as.data.frame(df)
  df[,ts_id] <- as.ordered(df[,ts_id])

  if (no_factors)
    df <- cbind(df[ts_id], df[sapply(df, is.logical) | sapply(df, is.numeric)])
  else
    df <- cbind(df[ts_id], df[, !(ts_id == names(df))])

  nas <- matrix(ncol=ncol(df) - 1, nrow=nlevels(df[,ts_id]))
  for (i in 2:ncol(df))
  {
    nas[,i - 1] <- tapply(df[,i], df[,ts_id], function(x) sum(is.na(x)) / length(x))
  }
  mv <- data.frame(levels(df[, ts_id]), nas)
  names(mv) <- c(ts_id, names(df)[2:ncol(df)])
  if (!anyNA(suppressWarnings(as.numeric(as.character(mv[,ts_id])))))
    mv[,ts_id] <- as.numeric(as.character(mv[,ts_id]))
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
