#' @title Prepares a By Group Trend Graph
#'
#' @description
#' Reads a data frame and line plots the selected variables (which need to be numeric)
#'  by group and an ordered factor (normally the time-series indicator).
#'
#' @param df Data frame containing the ordered factor and a set of numerical variables
#'   to be plotted.
#' @param ts_id a string containing the column name of the ordered factor
#'   (normally the time-series indicator).
#' @param group_var a variable coercible into a factor to group the data on.
#' @param var The name of the variable that you want to plot.
#' @param points Do you want points to indicate the observations? Defaults to \code{TRUE}.
#' @param error_bars Do you want error bars to be plotted? Defaults to \code{FALSE}.
#'
#' @return A list containing two items:
#' \describe{
#'  \item{"df"}{A data frame containing the plotted means and standard errors by group}
#'  \item{"plot}{The plot as returned by \code{ggplot}}
#' }
#'
#' @examples
#' df <- worldbank
#' df$gdp_capita <- worldbank$NY.GDP.PCAP.KD
#' graph <- prepare_by_group_trend_graph(df, "year", "region", "gdp_capita")
#' graph$plot
#' @export



prepare_by_group_trend_graph <- function(df, ts_id, group_var, var,
                                         points = TRUE, error_bars = FALSE) {
  # Make devtools:check() and CRAN happy
  value <- se <- NULL
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  if (! ts_id %in% colnames(df)) stop("ts_id need to be in df")
  if (! group_var %in% colnames(df)) stop("group_var need to be in df")
  if (! var %in% colnames(df)) stop("var names need to be in df")

  df <- droplevels(df[stats::complete.cases(df[, c(ts_id, group_var, var)]),
                      c(ts_id, group_var, var)])

  df[, ts_id] <- try_convert_ts_id(df[, ts_id])
  df[, group_var] <- as.factor(df[, group_var])

  gf <- df %>% dplyr::group_by(!! rlang::sym(ts_id), !! rlang::sym(group_var)) %>%
    dplyr::summarise(mean = mean(!! rlang::sym(var), na.rm=TRUE),
                     se = stats::sd(!! rlang::sym(var), na.rm=TRUE)/
                       sqrt(length(which(!is.na(!! rlang::sym(var))))))
  gf <- as.data.frame(gf)

  if (error_bars) {
    if (is.factor(df[, ts_id])) {
      plot <- ggplot2::ggplot(gf, ggplot2::aes_string(x = ts_id, color=group_var, group = group_var)) +
        ggplot2::stat_summary(fun = sum, geom = "line", ggplot2::aes(y = mean)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-se, ymax = mean+se), width = .1) +
        ggplot2::xlab(ts_id) + ggplot2::ylab(var)
    } else {
      plot <- ggplot2::ggplot(gf, ggplot2::aes_string(x=ts_id, color=group_var, group = group_var)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-se, ymax = mean+se), width = .1) +
        ggplot2::geom_line(ggplot2::aes(y = mean)) +
        ggplot2::xlab(ts_id) + ggplot2::ylab(var)
    }
    if (points) plot <- plot + ggplot2::geom_point(ggplot2::aes(y = mean))
  } else {
    plot <- ggplot2::ggplot(gf, ggplot2::aes_string(x = ts_id, y = "mean",
                                                    color=group_var, group = group_var)) +
      ggplot2::geom_line() + ggplot2::xlab(ts_id) + ggplot2::ylab(var)
    if (points) plot <- plot + ggplot2::geom_point()
  }

  list(df = gf, plot = plot)
}
