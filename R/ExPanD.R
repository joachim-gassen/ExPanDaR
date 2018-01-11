#' @title Explore Panal Data (ExPanD)
#'
#' @description
#' A shiny based app that uses ExPanDaR functionality for interactive
#'  panel data exploration
#'
#' @param df A data that containing the panel data that you want to explore
#' @param cs_id A character vector containing the names of the variables that
#'   identify the cross-section in your data. The according variables should be factors.
#' @param ts_id A character scalar identify the name of the variables that
#'   identifies the time series in your data. The according variables needs to be anordered vector.
#' @param config_list a list containing the startupo configuraion for ExPanD to display.
#'   Take a look at \code{data(ExPanD_config_russell_3000)} to understand the format.
#' @param ... Additional parameters that are passed on to \code{\link[shiny]{runApp}}.

#' @examples
#' data(russell_3000)
#' ExPanD(russell_3000, c("coid", "coname"), "period")
#' data(ExPanD_config_russell_3000)
#' ExPanD(russell_3000, c("coid", "coname"), "period", ExPanD_config_russell_3000)
#'
#' @details
#' Use the \code{\link[Hmisc]{label}} command to add label defining the variables to your data frame if you want
#'   to have nice floatting variable descriptions in the descriptive table.


#' @export

ExPanD <- function(df, cs_id, ts_id, config_list = NULL, ...)
{
  shiny_df <<- df
  shiny_df_name <<- deparse(substitute(df))
  shiny_cs_id <<- cs_id
  shiny_ts_id <<- ts_id
  shiny_config_list <<- config_list
  shiny::runApp(appDir = system.file("application", package = "ExPanDaR"), ...)
}
