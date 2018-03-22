#' @title Explore Panal Data (ExPanD)
#'
#' @description A shiny based web app that uses ExPanDaR functionality for
#' interactive panel data exploration
#'
#' @param df A dataframe or a list of dataframes containing the panel data that
#'   you want to explore. If NULL, ExPanD will start up with a file upload
#'   dialog.
#' @param cs_id A character vector or a list of character vectors containing the
#'   names of the variables that identify the cross-section in your data. The
#'   according variables should be factors.
#'   Can only be NULL if df_def is provided instead.
#' @param ts_id A character scalar or a character vector identifing the name of
#'   the variable that identifies the time series in your data. The according
#'   variable needs to be coercible to an ordered vector.
#'   Can only be NULL if df_def is provided instead.
#' @param df_def An optional dataframe (or a list of dataframes) containing
#'   variable names, definitions and types. If NULL (the default) ExPanD
#'   uses \code{cs_id} and \code{ts_id} to identify the panel structure and
#'   determines the variable types (factor, numeric, logical) based on the
#'   classes of the data. See the details section for further information.
#' @param var_def If you specify here a dataframe containing variable names and
#'   variable definitions, ExPanD will use these on the provided sample(s) to
#'   create the analysis sample. In that case, the user gets the opportunity to
#'   add additional variables in the app. See the details section
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
#' @param components A logical vector indicating which reports you want
#'   ExPanD to generate. If specified, the vector does not have to be named but needs
#'   to be of correct length.
#' @param store_encrypted Do you want the user-side saved config files to be
#'   encrypted? A security measure to avoid that users can inject arbitrary code
#'   in the config list.
#' @param key_phrase The key phrase to use for encryption. Potentially a good
#'   idea to change this from the default if you want to encrypt.
#' @param debug Do you want ExPanD to echo some debug timing information to the
#'   console/log file and to store some diagnostics to the global environment?
#'   Probably not.
#' @param ... Additional parameters that are passed on to
#'   \code{\link[shiny]{runApp}}.
#'
#' @details
#'
#' If you start ExPanD without any options, it will start with an upload
#' dialog so that the the user (e.g., you) can upload a data file
#' for anaylsis. Supported formats are as provided
#' by the \code{rio} package.
#'
#' If you provide variable defintions in \code{df_def} and/or \code{var_def},
#' ExPanD displays these as tooltips in the descriptive table of the
#' ExPanD app.
#'
#' When you provide \code{var_def}, ExPanD starts up in the "advanced mode". The
#' advanced mode uses (a) base sample(s) (the one(s) you provide via \code{df})
#' and the variable definitions in \code{var_def} to generate an analysis
#' sample based on the active base sample. In the advanced mode, the app user
#' can generate additional variables from within the app.
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
#' ("cs_id") and exactly one variable which is coercible into an ordered factor
#' has to be assigned as the time-series identifier ("ts_id").}
#' \item{"can_be_na"}{Optional: If included,
#' then all variables with this value set to FALSE are required to be non
#' missing in the dataset. This reduces the number of observations. If missing,
#' it defaults to all variables other than cs_id and ts_id being TRUE} }
#'
#' @examples
#' \dontrun{
#'   ExPanD()
#'   # Use this if you want to read very large files via the file dialog
#'   options(shiny.maxRequestSize = 1024^3)
#'   ExPanD()
#'
#'   data(russell_3000)
#'   ExPanD(russell_3000, c("coid", "coname"), "period")
#'   ExPanD(russell_3000, df_def = russell_3000_data_def)
#'   ExPanD(russell_3000, df_def = russell_3000_data_def, components = c(T, F, T, F, F, F, F, F, T, T))
#'   data(ExPanD_config_russell_3000)
#'   ExPanD(df = russell_3000, df_def = russell_3000_data_def,
#'     config_list = ExPanD_config_russell_3000)
#'   exploratory_sample <- sample(nrow(russell_3000), round(0.5*nrow(russell_3000)))
#'   test_sample <- setdiff(1:nrow(russell_3000), exploratory_sample)
#'   ExPanD(df = list(russell_3000[exploratory_sample, ], russell_3000[test_sample, ]),
#'     df_def = list(russell_3000_data_def, russell_3000_data_def),
#'     df_name = c("Exploratory sample", "Test sample"))
#'   ExPanD(worldbank, df_def = worldbank_data_def, var_def = worldbank_var_def,
#'     config_list = ExPanD_config_worldbank)}
#' @export

ExPanD <- function(df = NULL, cs_id = NULL, ts_id = NULL,
                   df_def = NULL, var_def = NULL, config_list = NULL,
                   title = "ExPanD - Explore panel data interactively",
                   abstract = NULL,
                   df_name = "User provided data",
                   long_def = TRUE,
                   components = c(bar_chart = TRUE,
                                  missing_values = TRUE,
                                  descriptive_table = TRUE,
                                  histogram = TRUE,
                                  ext_obs = TRUE,
                                  trend_graph = TRUE,
                                  quantile_trend_graph = TRUE,
                                  corrplot = TRUE,
                                  scatter_plot = TRUE,
                                  regression = TRUE),
                   store_encrypted = FALSE,
                   key_phrase = "What a wonderful key",
                   debug = FALSE, ...) {
  if (!is.null(df) && !is.data.frame(df) && !is.list(df)) stop("df is neither a dataframe nor a list of dataframes")
  if (!is.null(df)) {
    if (is.null(df_def) && (is.null(cs_id) || is.null(ts_id))) stop("df is provided but not df_def and either cs_id or ts_id is NULL")
    if (!is.null(df_def) && (!is.null(cs_id) || !is.null(ts_id))) stop("provide either df_def or cs_id and ts_id but not both")
    if (!is.data.frame(df) && length(which(!sapply(df, is.data.frame))) > 0) stop("df is a list containing non-dataframe members")
    if (!is.data.frame(df) && !is.null(cs_id) && length(df) != length(cs_id)) stop("df is a list but cs_id does not have length of list")
    if (!is.data.frame(df) && !is.null(ts_id) && length(df) != length(ts_id)) stop("df is a list but ts_id does not have length of list")
    if (!is.data.frame(df) && !is.null(df_def) && length(df) != length(df_def)) stop("df is a list but ts_id does not have length of list")
  }
  if (length(components) != 10 | !is.vector(components) | !is.logical(components)) stop("Components vector is invalid")
  comp_names <- c("bar_chart", "missing_values", "descriptive_table", "histogram",
                 "ext_obs", "trend_graph", "quantile_trend_graph", "corrplot",
                 "scatter_plot", "regression")
  if (is.null(names(components))) names(components) <- comp_names
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
  shiny_components <- components
  app_dir <- system.file("application", package = "ExPanDaR")
  save(list = ls(pattern = "shiny"), file = paste0(app_dir, "/shiny_data.Rda"))
  try(shiny::runApp(appDir = system.file("application", package = "ExPanDaR"), ...))
  unlink(paste0(app_dir, "/shiny_data.Rda"))
}
