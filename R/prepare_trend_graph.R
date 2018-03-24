#' @title Prepares a Trend Graph
#'
#' @description
#' Reads a data frame and line plots all variables (which need to be numeric)
#'  by an ordered factor.
#'
#' @param df Data frame containing the ordered factor and a set of numerical variables
#' to be plotted
#' @param xvar a string countaining the column name of the ordered factor
#'
#' @return A list containing two items:
#' \describe{
#'  \item{"df"}{A data frame containg the plotted means and standard errors}
#'  \item{"plot}{The plot as returned by ggplot}
#' }
#'
#' @examples
#' df <- data.frame(year = floor(time(EuStockMarkets)), EuStockMarkets)
#' graph <- prepare_trend_graph(df, "year")
#' graph$plot
#' @export



prepare_trend_graph <- function(df, xvar) {
  # Make devtools:check() and CRAN happy
  value <- se <- NULL
  if(!is.numeric(xvar)) xnum <- suppressWarnings(as.numeric(as.character(df[,xvar]))) else xnum <- xvar
  if (anyNA(xnum)) {
    x_is_factor <- TRUE
  } else {
    df[,xvar] <- xnum
    x_is_factor <- FALSE
  }

  gf <- tidyr::gather_(data = df,
                       key_col = "variable",
                       value_col = "value",
                       gather_cols = colnames(df)[!(xvar == colnames(df))]) %>%
    dplyr::group_by_at(dplyr::vars("variable", xvar)) %>%
    dplyr::summarise(mean = mean(value, na.rm=TRUE),
              se = stats::sd(value, na.rm=TRUE)/sqrt(length(which(!is.na(value)))))
  gf <- as.data.frame(gf)

  if (x_is_factor) {
    plot <- ggplot2::ggplot(gf, ggplot2::aes_string(x = xvar, color="variable", group = "variable")) +
      ggplot2::stat_summary(fun.y=sum, geom="line", ggplot2::aes(y = mean)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-se, ymax = mean+se), width = .1) +
      ggplot2::geom_point(ggplot2::aes(y = mean)) + ggplot2::xlab(xvar) + ggplot2::ylab("")
  } else {
    plot <- ggplot2::ggplot(gf, ggplot2::aes_string(x=xvar, colour="variable")) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-se, ymax = mean+se), width = .1) +
      ggplot2::geom_line(ggplot2::aes(y = mean)) +
      ggplot2::geom_point(ggplot2::aes(y = mean)) + ggplot2::xlab(xvar) + ggplot2::ylab("")
  }

  list(df = gf, plot = plot)
}
