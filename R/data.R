#' Accounting and Annual Stock Return Data for a Sample of Russell 3000 Firms (2013-2016)
#'
#' Data collected from Google Finance and Yahoo finance using the package \code{tidyquant}.
#'
#' @docType data
#'
#' @usage data(russell_3000)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets, accounting data, capital market data
#'
#' @source Has been collected using the \code{\link[tidyquant]{tq_get}} function family in Summer 2017.
#'  The data has been collected for the sole purpose to showcase the functions of the \code{ExPanDaR} package.
#'  Use in scientific studies is strongly discouraged.
#'
#' @examples
#' data(russell_3000)
#' prepare_missing_values_graph(russell_3000, period = "period")
#'
"russell_3000"
