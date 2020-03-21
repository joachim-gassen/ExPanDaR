#' @title Prepares a Scatter Plot
#'
#' @description
#' Reads a data frame and prepares a scatter plot.
#'
#' @param df Data frame containing the data
#' @param x a string containing the column name of the x variable
#' @param y a string containing the column name of the y variable
#' @param size a string containing the column name of the variable providing the size aesthetic
#' @param color a string containing the column name of the variable providing the color aesthetic
#'   (can be numerical or a factor)
#' @param loess a numerical scalar
#' \describe{
#'  \item{0}{No loess curve}
#'  \item{1}{loess curve with equal weights}
#'  \item{2}{loess curve with weights based on \code{size} variable}
#' }
#' @param alpha The alpha value to be used.
#'   If missing, it calculates a default based on the sample size
#'
#' @return the plot as returned by \code{ggplot}
#'
#' @examples
#' df <- data.frame(year = floor(stats::time(datasets::EuStockMarkets)),
#'                  datasets::EuStockMarkets[, c("DAX", "FTSE")])
#' prepare_scatter_plot(df, x="DAX", y="FTSE", color="year")
#' @export


prepare_scatter_plot <- function(df, x, y, color = "", size = "", loess = 0,
                                 alpha = min(1,1/((1 + (max(0,log(nrow(df)) - log(100))))))) {
  color_there <- (color != "")
  size_there <- (size != "")
  vars <- c(x, y, color, size)
  vars <- vars[which(vars != "")]
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  df <- df[stats::complete.cases(df[,vars]),vars]
  if (loess < 2) scatter <- ggplot2::ggplot(df, ggplot2::aes_string(x = x, y = y))
  else scatter <- ggplot2::ggplot(df, ggplot2::aes_string(x = x, y = y, weight=size))
  if (size_there & color_there) scatter <- scatter +
    ggplot2::geom_point(alpha = alpha, ggplot2::aes_string(size = size, color = color))
  else if (color_there) scatter <- scatter +
    ggplot2::geom_point(alpha = alpha, ggplot2::aes_string(color = color))
  else if (size_there) scatter <- scatter +
    ggplot2::geom_point(alpha = alpha, ggplot2::aes_string(size = size))
  else scatter <- scatter + ggplot2::geom_point(alpha = alpha)
  scatter <- scatter + ggplot2::ylab(y) + ggplot2::xlab(x)
  if (color_there)  scatter <- scatter +
    ggplot2::guides(color=ggplot2::guide_legend(color, override.aes = list(alpha = 1)))
  if (size_there) scatter <- scatter +
    ggplot2::guides(size=ggplot2::guide_legend(size, override.aes = list(alpha = 1)))
  if (loess > 0) scatter + ggplot2::geom_smooth()
  else scatter
}
