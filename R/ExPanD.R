#' @title Explore Panal Data (ExPanD)
#'
#' @description
#' A shiny based app that uses ExPanDaR functionality for interactive
#'  panel data exploration
#'
#' @param df A data that containing the panel data that you want to explore
#' @param cs_id A character vector containing the names of the variables that
#'   identify the cross-section in your data. The according variables should be factors.
#' @param ts_id A character scalar identify the name of the variable that
#'   identifies the time series in your data. The according variable needs to be an ordered vector.
#' @param config_list a list containing the startup configuraion for ExPanD to display.
#'   Take a look at \code{data(ExPanD_config_russell_3000)} for the format.
#' @param title the title to display in the shiny web app.
#' @param abstract As introductory text to display in the shiny web app. Needs to be formatted as clean HTML.
#' @param store_encrypted Do you want the user-side saved config files to be encrypted?
#'   A security measure to avoid that users can inject arbitrary code in the config list.
#' @param key_phrase The key phrase to use for encryption.
#'   Potentially a good idea to change this from the default if you want to encrypt.
#' @param debug Do you want ExPanD to echo some timing information to the console/log file?
#' @param ... Additional parameters that are passed on to \code{\link[shiny]{runApp}}.

#' @examples
#' \dontrun{
#'   data(russell_3000)
#'   ExPanD(russell_3000, c("coid", "coname"), "period")
#'   data(ExPanD_config_russell_3000)
#'   ExPanD(russell_3000, c("coid", "coname"), "period", ExPanD_config_russell_3000)
#' }
#' @details
#' Use the \code{\link[Hmisc]{label}} command to add label defining the variables to your data frame if you want
#'   to have nice floatting variable descriptions in the descriptive table.


#' @export

ExPanD <- function(df, cs_id, ts_id, config_list = NULL,
                   title = "ExPanD - Explore panel data interactively",
                   abstract = NULL,
                   store_encrypted = FALSE,
                   key_phrase = "What a wonderful key",
                   debug = FALSE, ...)
{
  shiny_df <- df
  shiny_df_name <- deparse(substitute(df))
  shiny_cs_id <- cs_id
  shiny_ts_id <- ts_id
  shiny_config_list <- config_list
  shiny_title <- title
  shiny_abstract <- abstract
  shiny_key_phrase <- key_phrase
  shiny_store_encrypted <- store_encrypted
  shiny_debug <- debug
  app_dir <- system.file("application", package = "ExPanDaR")
  save(list = ls(pattern = "shiny"), file = paste0(app_dir, "/shiny_data.Rda"))
  try(shiny::runApp(appDir = system.file("application", package = "ExPanDaR"), ...))
  unlink(paste0(app_dir, "/shiny_data.Rda"))
}
