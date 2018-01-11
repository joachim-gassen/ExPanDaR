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

#' Default Configuration to use with ExPanD and the Russell 3000 Data Set
#'
#' List to use as a \code{list_config} parameter when starting \code{ExPanD()}.
#'
#' @docType data
#'
#' @usage data(ExPanD_config_russell_3000)
#'
#' @format An object of class \code{"list"}.
#'
#' @keywords ExPanD, config
#'
#' @examples
#' data(russell_3000)
#' data(ExPanD_config_russell_3000)
#' ExPanD(russell_3000, c("coid", "coname"), "period", ExPanD_config_russell_3000)
#'
"ExPanD_config_russell_3000"
