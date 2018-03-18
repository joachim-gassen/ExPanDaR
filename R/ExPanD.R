#' @title Explore Panal Data (ExPanD)
#'
#' @description A shiny based web app that uses ExPanDaR functionality for
#' interactive panel data exploration
#'
#' @param df A dataframe or a list of dataframes containing the panel data that
#'   you want to explore
#' @param cs_id A character vector or a list of character vectors containing the
#'   names of the variables that identify the cross-section in your data. The
#'   according variables should be factors.
#' @param ts_id A character scalar or a character vector identifing the name of
#'   the variable that identifies the time series in your data. The according
#'   variable needs to be an ordered vector.
#' @param df_def An optional dataframe (or a list of dataframes) containing
#'   variable names, definitions and types. If NULL (the default) ExPanD will
#'   look for labels in df to read the definitions and will determine the
#'   variable type (factor, numeric, logical) based on the class of the data.
#'   See the details section for further information.
#' @param var_def if you specify a dataframe containing variable names and
#'   variable definitions, ExPanD will use these on the provided sample(s) to
#'   create the analysis sample. In that case, the user gets the opportunity to
#'   extend these variable definitions within the app. See the details section
#'   for the structure of the \code{var_def} dataframe. If NULL (default) than
#'   the sample(s) provided by df will be used as anaylsis sample(s) directly.
#' @param config_list a list containing the startup configuraion for ExPanD to
#'   display. Take a look at \code{data(ExPanD_config_russell_3000)} for the
#'   format. The easiest way to generate a config list is to customize the
#'   display within the app and then save the configuration locally.
#' @param title the title to display in the shiny web app.
#' @param abstract An introductory text to display in the shiny web app. Needs
#'   to be formatted as clean HTML.
#' @param df_name A character string or a vector of character strings
#'   characterizing the dataframe(s) provided in \code{df} (will be used in the
#'   selection menu of the app)
#' @param long_def If you set this to TRUE (default) and are providing a
#'   \code{var_def} then ExPanD will add the definitions of the used variables
#'   of the underlying dataframe to the definitions provided for the analysis
#'   sample to make these more informative to the user.
#'   If set to FALSE only the variable definitions provided in the
#'   \code{var_def} sample will be provided to the user.
#' @param store_encrypted Do you want the user-side saved config files to be
#'   encrypted? A security measure to avoid that users can inject arbitrary code
#'   in the config list.
#' @param key_phrase The key phrase to use for encryption. Potentially a good
#'   idea to change this from the default if you want to encrypt.
#' @param debug Do you want ExPanD to echo some timing information to the
#'   console/log file?
#' @param ... Additional parameters that are passed on to
#'   \code{\link[shiny]{runApp}}.
#'
#' @details
#'
#' If you do not provide data defintions in \code{df_def}, ExPanD uses labels
#' set via \code{\link[Hmisc]{label}} in your dataframe as variable definitions.
#' Variable definitions are shown as tooltips in the descriptive table of the
#' ExPanD app.
#'
#' When you provide \code{var_def}, ExPanD starts up in the "advanced mode". The
#' advanced mode uses (a) base sample(s) (the one(s) you provide via \code{df})
#' and the variable definitions in  \code{var_def} to generate an analysis
#' sample based on the active base sample. In the advanced mode, the app user
#' can generate additional variables interactively.
#'
#' A \code{df_def} or \code{var_def} dataframe can contain the following
#' variables
#' \describe{
#' \item{"var_name"}{Required: The names of the variables
#' that are to be calculated}
#' \item{"var_def"}{Required: The code that is passed
#' to the dataframe (grouped by cross-sectional units) in calls to
#' \code{\link[dplyr]{mutate}} as right hand side to calculate the respective
#' variable. Be sure to set order_by when creating lead/lag variables}
#' \item{"type"}{Required: One of the strings "cs_id", "ts_id", "factor",
#' "logical" or "numeric" indicating the type of the variable. Please note that
#' at least one variable has to be assigned as a cross-sectional identifier
#' ("cs_id") and exactly one ordered factor has to be assigned as the
#' time-series identifier ("ts_id").}
#' \item{"can_be_na"}{Optional: If included,
#' then all variables with this value set to FALSE are required to be non
#' missing in the dataset. This reduces the number of observations. If missing,
#' it defaults to all variables other than cs_id and ts_id being TRUE} }
#'
#' @examples
#' \dontrun{
#'   data(russell_3000)
#'   ExPanD(russell_3000, c("coid", "coname"), "period")
#'   data(ExPanD_config_russell_3000)
#'   ExPanD(df = russell_3000, cs_id = c("coid", "coname"), ts_id = "period",
#'     config_list = ExPanD_config_russell_3000)
#'   exploratory_sample <- sample(nrow(russell_3000), round(0.75*nrow(russell_3000)))
#'   test_sample <- setdiff(1:nrow(russell_3000), exploratory_sample)
#'   ExPanD(df = list(russell_3000[exploratory_sample, ], russell_3000[test_sample, ]),
#'     cs_id = list(c("coid", "coname"), c("coid", "coname")), ts_id = c("period", "period"),
#'     df_name = c("Exploratory sample", "Test sample"))}
#' @export

ExPanD <- function(df, cs_id, ts_id,
                   df_def = NULL, var_def = NULL, config_list = NULL,
                   title = "ExPanD - Explore panel data interactively",
                   abstract = NULL,
                   df_name = "User provided data",
                   long_def = TRUE,
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
  shiny_df_def <- df_def
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
  shiny_long_def <- long_def
  shiny_key_phrase <- key_phrase
  shiny_store_encrypted <- store_encrypted
  shiny_debug <- debug
  app_dir <- system.file("application", package = "ExPanDaR")
  save(list = ls(pattern = "shiny"), file = paste0(app_dir, "/shiny_data.Rda"))
  try(shiny::runApp(appDir = system.file("application", package = "ExPanDaR"), ...))
  unlink(paste0(app_dir, "/shiny_data.Rda"))
}
