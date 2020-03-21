# Helper function that tries to convert the time series indicator to
# different data types to get nicer tick brakes if possible
# Used by all time_trend functions.

try_convert_ts_id <- function(ts_id) {
  if(!(is.numeric(ts_id) ||
       inherits(ts_id, 'Date') ||
       inherits(ts_id, 'POSIXct') ||
       inherits(ts_id, 'POSIXlt'))) {
    try_conv <- try(suppressWarnings(as.Date(as.character(ts_id))), silent = T)
    if(!inherits(try_conv, 'try-error') && !anyNA(try_conv)) {
      ts_id <- try_conv
    } else {
      try_conv <- try(suppressWarnings(as.numeric(as.character(ts_id))), silent = T)
      if(!inherits(try_conv, 'try-error') && !anyNA(try_conv)) {
        ts_id <- try_conv
      } else {
        try_conv <- try(suppressWarnings(strptime(as.character(ts_id), "%Y-%m-%d %H:%M:%S")), silent = T)
        if(!inherits(try_conv, 'try-error') && !anyNA(try_conv)) {
          ts_id <- try_conv
        }  else if (!is.factor(ts_id)) ts_id <- as.ordered(ts_id)
      }
    }
  }
  ts_id
}


#' @title Prepares a Trend Graph
#'
#' @description
#' Reads a data frame and line plots all variables (which need to be numeric)
#'  by an ordered factor (normally the time-series indicator).
#'
#' @param df Data frame containing the ordered factor and a set of numerical variables
#'   to be plotted
#' @param ts_id a string containing the column name of the ordered factor
#'   (normally the time-series indicator)
#' @param var a character vector containing the column names of the variables
#'   that should be plotted. Defaults to all numeric variables of the data frame
#'   besides the one indicated by \code{ts_id}.
#'
#' @return A list containing two items:
#' \describe{
#'  \item{"df"}{A data frame containing the plotted means and standard errors}
#'  \item{"plot}{The plot as returned by \code{ggplot}}
#' }
#'
#' @examples
#' df <- data.frame(year = floor(time(EuStockMarkets)), EuStockMarkets)
#' graph <- prepare_trend_graph(df, "year")
#' graph$plot
#' @export



prepare_trend_graph <- function(df, ts_id,
                                var = colnames(df[sapply(df, is.numeric) & colnames(df) != ts_id])) {
  # Make devtools:check() and CRAN happy
  value <- se <- NULL
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  if (! ts_id %in% colnames(df)) stop("ts_id need to be in df")
  if (any(! var %in% colnames(df))) stop("var names need to be in df")

  df <- droplevels(df[stats::complete.cases(df[, c(ts_id, var)]), c(ts_id, var)])
  df[, ts_id] <- try_convert_ts_id(df[, ts_id])

  gf <- tidyr::gather_(data = df,
                       key_col = "variable",
                       value_col = "value",
                       gather_cols = colnames(df)[!(ts_id == colnames(df))]) %>%
    dplyr::group_by_at(dplyr::vars("variable", ts_id)) %>%
    dplyr::summarise(mean = mean(value, na.rm=TRUE),
              se = stats::sd(value, na.rm=TRUE)/sqrt(length(which(!is.na(value)))))
  gf <- as.data.frame(gf)

  if (is.factor(df[, ts_id])) {
    plot <- ggplot2::ggplot(gf, ggplot2::aes_string(x = ts_id, color="variable", group = "variable")) +
      ggplot2::stat_summary(fun = sum, geom = "line", ggplot2::aes(y = mean)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-se, ymax = mean+se), width = .1) +
      ggplot2::geom_point(ggplot2::aes(y = mean)) + ggplot2::xlab(ts_id) + ggplot2::ylab("")
  } else {
    plot <- ggplot2::ggplot(gf, ggplot2::aes_string(x=ts_id, colour="variable")) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean-se, ymax = mean+se), width = .1) +
      ggplot2::geom_line(ggplot2::aes(y = mean)) +
      ggplot2::geom_point(ggplot2::aes(y = mean)) + ggplot2::xlab(ts_id) + ggplot2::ylab("")
  }

  list(df = gf, plot = plot)
}
