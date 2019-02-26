#' @title Prepares a Table Displaying Extreme Observations
#'
#' @description
#' Reads a data frame, sorts it by the given variable and displays the top and bottom n observations.
#' @param df Data frame
#' @param n The number of top/bottom observations that you want to report.
#' @param cs_id The variable(s) identifying the cross-section in the data.
#' @param ts_id The variable identifying the time-series in the data.
#' @param var Variable to display. Defaults to the last numerical variable of the data frame.
#' @param ... Additional parameters that are passed to \code{\link[knitr]{kable}}.
#' @return A list containing two items:
#' \describe{
#'  \item{"df"}{A data frame containing the top/bottom n observations}
#'  \item{"kable_ret"}{The return value provided by \code{\link[knitr]{kable}} containing the formatted table}
#' }
#'
#' @details When both \code{cs_id} and \code{ts_id} are omitted, all variables are tabulated.
#'   Otherwise, \code{var} is tabulated along with the identifiers.
#'   Infinite values in \code{var} are omitted.
#'   The default parameters for calling \code{\link[knitr]{kable}},
#'   are \code{format = "html", digits = 3, format.args = list(big.mark = ','), row.names = FALSE}.
#'
#' @examples
#' t <- prepare_ext_obs_table(russell_3000, n = 10,
#'                            cs_id = c("coid", "coname"),
#'                            ts_id = "period", var = "sales")
#' t$df
#' @export


prepare_ext_obs_table <- function(df, n = 5, cs_id = NULL, ts_id = NULL,
                                  var = utils::tail(colnames(df[sapply(df, is.numeric) &
                                                                  (! colnames(df) %in% c(cs_id, ts_id))]),
                                                    n=1), ...) {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  if (2*n > nrow(df))
    stop("'n' needs to be <= nrow(df)/2")
  if (length(var) > 1) stop("var needs to identify a single variable")
  if (!var %in% colnames(df)) stop("var need to be in df")
  if (!is.null(ts_id) && !ts_id %in% colnames(df)) stop("ts_id needs to be in df")
  if (!is.null(cs_id) && any(! cs_id %in% colnames(df))) stop("cs_id names need to be in df")

  vars <- stats::na.omit(c(cs_id, ts_id, var))
  if (length(vars) == 1) vars <- c(colnames(df[!colnames(df) %in% var]), var)
  df <- df[is.finite(df[, var]), vars]

  df <- rbind(utils::head(df[order(-df[,ncol(df)]),], n),
              utils::tail(df[order(-df[,ncol(df)]),], n))
  t <- rbind(df[1:n,], rep(NA, ncol(df)), df[(n+1):(2*n),])
  rownames(t)[n+1] <- ""

  kable_args <- list(...)
  kable_args$x = t
  if (!"format" %in% names(kable_args)) kable_args$format <- "html"
  if (!"digits" %in% names(kable_args)) kable_args$digits <- 3
  if (!"format.args" %in% names(kable_args)) kable_args$format.args <- list(big.mark = ',')
  if (!"row.names" %in% names(kable_args)) kable_args$row.names <- FALSE
  opt <- getOption("knitr.kable.NA")
  options(knitr.kable.NA = '...')
  kable_ret <- do.call(knitr::kable, kable_args)
  options(knitr.kable.NA = opt)
  list(df = df, kable_ret = kable_ret)
}
