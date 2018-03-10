#' @title Explore Panal Data (ExPanD)
#'
#' @description
#' A shiny based web app that uses ExPanDaR functionality for interactive
#'  panel data exploration
#'
#' @param df A data that or a list of dataframes containing the panel data that you want to explore
#' @param cs_id A character vector or a list of character vectors containing the names of the variables that
#'   identify the cross-section in your data. The according variables should be factors.
#' @param ts_id A character scalar or a character vector identifing the name of the variable that
#'   identifies the time series in your data. The according variable needs to be an ordered vector.
#' @param var_def if you specify a dataframe containing variable names and variable definitions,
#'   ExPanD will use these on the provided sample(s) to create the analysis sample. In that case,
#'   the user gets the opportunity to extend these variable definitions within the app.
#'   See the details section for the structure of the \code{var_def} dataframe. If NULL (default) than the sample(s)
#'   provided by df will be used as anaylsis sample(s) directly.
#' @param config_list a list containing the startup configuraion for ExPanD to display.
#'   Take a look at \code{data(ExPanD_config_russell_3000)} for the format. The easiest way to generate
#'   a config list is to customize the display within the app and then save the configuration locally.
#' @param title the title to display in the shiny web app.
#' @param abstract An introductory text to display in the shiny web app. Needs to be formatted as clean HTML.
#' @param df_name A character string or a vector of character strings
#'   characterizing the dataframe(s) provided in \code{df} (will be used in the selection menu of the app)
#' @param store_encrypted Do you want the user-side saved config files to be encrypted?
#'   A security measure to avoid that users can inject arbitrary code in the config list.
#' @param key_phrase The key phrase to use for encryption.
#'   Potentially a good idea to change this from the default if you want to encrypt.
#' @param debug Do you want ExPanD to echo some timing information to the console/log file?
#' @param ... Additional parameters that are passed on to \code{\link[shiny]{runApp}}.
#'
#' @details
#' Use the \code{\link[Hmisc]{label}} command to add label defining the variables
#'   to your data frame if you want to have nice floatting variable descriptions
#'   in the descriptive table.
#'
#' A \code{var_def} dataframe can contain the following variables
#' \describe{
#'  \item{"var_name"}{Required: The names of the variables that are to be calculated}
#'  \item{"var_def"}{Required: The code that is passed to the dataframe (grouped by cross-sectional units)
#'    in calls to \code{\link[dplyr]{mutate}} as right hand side to calculate the respective variable.
#'    Be sure to set order_by when creating lead/lag variables}
#'  \item{"type"}{Required: One of the strings "cs_id", "ts_id", "factor", "logical" or "numeric"
#'    indicating the type of the variable. Please note that at least one variable has to be assigned
#'    as a cross-sectional identifier ("cs_id") and exactly one ordered factor has to be assigned
#'    as the time-series identifier ("ts_id").}
#'  \item{"can_be_na"}{Optional: If included, then all variables with this value set to FALSE are
#'    required to be non missing in the dataset. This reduces the number of observations.
#'    If missing, it defaults to all variables other than cs_id and ts_id being TRUE}
#' }
#'
#' @examples
#' \dontrun{
#'   data(russell_3000)
#'   ExPanD(russell_3000, c("coid", "coname"), "period")
#'   data(ExPanD_config_russell_3000)
#'   ExPanD(russell_3000, c("coid", "coname"), "period", ExPanD_config_russell_3000)
#' }
#' @export

ExPanD <- function(df, cs_id, ts_id,
                   var_def = NULL, config_list = NULL,
                   title = "ExPanD - Explore panel data interactively",
                   abstract = NULL,
                   df_name = "User provided data",
                   store_encrypted = FALSE,
                   key_phrase = "What a wonderful key",
                   debug = FALSE, ...) {
  if (!is.data.frame(df) && !is.list(df)) stop("df is neither a dataframe nor a list of dataframes")
  if (!is.data.frame(df) && length(which(!sapply(df, is.data.frame))) > 0) stop("df is a list containing non-dataframe members")
  if (!is.data.frame(df) && length(df) != length(cs_id)) stop("df is a list but cs_id does not have length of list")
  if (!is.data.frame(df) && length(df) != length(ts_id)) stop("df is a list but ts_id does not have length of list")
  shiny_df <- df
  shiny_cs_id <- cs_id
  shiny_ts_id <- ts_id
  shiny_var_def <- var_def
  shiny_config_list <- config_list
  shiny_title <- title
  shiny_abstract <- abstract
  if (is.data.frame(df)) {
    shiny_df_id <- deparse(substitute(df))
  } else {
    shiny_df_id <- names(df)
    if (is.null(shiny_df_id)) shiny_df_id <- paste0("df list member ", 1:length(df))
  }
  shiny_df_name <- df_name
  shiny_key_phrase <- key_phrase
  shiny_store_encrypted <- store_encrypted
  shiny_debug <- debug
  app_dir <- system.file("application", package = "ExPanDaR")
  save(list = ls(pattern = "shiny"), file = paste0(app_dir, "/shiny_data.Rda"))
  try(shiny::runApp(appDir = system.file("application", package = "ExPanDaR"), ...))
  unlink(paste0(app_dir, "/shiny_data.Rda"))
}
