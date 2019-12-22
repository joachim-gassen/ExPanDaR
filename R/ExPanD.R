#' @title Explore Your Data (ExPanD)
#'
#' @description A shiny based web app that uses ExPanDaR functionality for
#' interactive data exploration. Designed for long-form panel data but works on
#' simple cross-sectional data as well.
#'
#' @param df A data frame or a list of data frames containing the data that
#'   you want to explore. If NULL, ExPanD will start up with a file upload
#'   dialog.
#' @param cs_id A character vector containing the names of the variables that
#'   identify the cross-section in your data. \code{df_def} overrides if
#'   provided.
#' @param ts_id A character scalar identifying the name of
#'   the variable that identifies the time series in your data. The according
#'   variable needs to be coercible to an ordered vector.
#'   If you provide a time series indicator that already is an ordered vector,
#'   ExPanD will verify that it has the same levels for each data frame
#'   and throw an error otherwise. \code{df_def} overrides if
#'   provided. If \code{cs_id} and \code{ts_id} are not provided either
#'   directly of by \code{df_def}, the data is treated as cross-sectional and
#'   only appropriate displays are included.
#' @param df_def An optional dataframe (or a list of dataframes) containing
#'   variable names, definitions and types. If NULL (the default) ExPanD
#'   uses \code{cs_id} and \code{ts_id} to identify the data structure and
#'   determines the variable types (factor, numeric, logical) based on the
#'   classes of the data. See the details section for further information.
#' @param var_def If you specify here a dataframe containing variable names and
#'   variable definitions, ExPanD will use these on the provided sample(s) to
#'   create the analysis sample. See the details section
#'   for the structure of the \code{var_def} dataframe. If NULL (default)
#'   the sample(s) provided by \code{df} will be used as analysis sample(s)
#'   directly.
#' @param config_list a list containing the startup configuration for ExPanD to
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
#' @param factor_cutoff ExPanD treats factors different from numerical variables.
#'   Factors are available for sub-sampling data and for certain plots.
#'   Each variable classified as such will be treated as a factor. In addition,
#'   ExPanD classifies all logical values and all numerical values with less or
#'   equal than \code{factor_cutoff} unique values as a factor.
#' @param components A named logical vector indicating the components that you want
#'   ExPanD to generate and their order. See the function head of \code{ExpanD}
#'   for the list of available components. By default, all components are reported.
#'   You can also exclude selected components from the standard order by setting
#'   then to \code{FALSE}. In addition, you can include an arbitrary number of
#'   \code{html_block} components. Each block will render clean HTML code as contained in
#'   the \code{html_blocks} parameter below. This allows you to customize your
#'   ExPanD report.
#' @param html_blocks A character vector containing the clean HTML code for each
#'   \code{html_block} that is included in \code{components}.
#' @param export_nb_option Do you want to give the user the option to download your
#'   data and an R notebook containing code for the analyses that \code{ExPanD} displays?
#'   Defaults to \code{FALSE}.
#' @param store_encrypted Do you want the user-side saved config files to be
#'   encrypted? A security measure to avoid that users can inject arbitrary code
#'   in the config list. Probably a good idea when you are hosting sensitive data
#'   on a publicly available server.
#' @param key_phrase The key phrase to use for encryption.
#'   Change this from the default if you want to encrypt the config files.
#' @param debug Do you want ExPanD to echo some debug timing information to the
#'   console/log file and to store some diagnostics to the global environment?
#'   Probably not.
#' @param ... Additional parameters that are passed on to
#'   \code{\link[shiny]{runApp}}.
#'
#' @details
#'
#' If you start ExPanD without any options, it will start with an upload
#' dialog so that the user (e.g., you) can upload a data file
#' for analysis. Supported formats are as provided
#' by the \code{rio} package.
#'
#' When you start ExPanD with a dataframe as the only parameter, it will assume
#' the data to be cross-sectional and will use its row names as the
#' cross-sectional identifier.
#'
#' When you have panel data in long format, set the \code{ts_id} and
#' \code{cs_id} parameters to identify the variables that determine
#' the time series and cross-sectional dimensions.
#'
#' If you provide variable definitions in \code{df_def} and/or \code{var_def},
#' ExPanD displays these as tooltips in the descriptive table of the
#' ExPanD app. In this case, you need to identify the panel dimensions in the
#' variable definitions (see below).
#'
#' When you provide more than one data frame in \code{df}, make sure that all have
#' the same variables and variable types defined. If not, ExPanD will throw
#' an error. When you provide only one \code{df_def} for multiple data frames,
#' \code{df_def} will be recycled.
#'
#' When you provide \code{var_def}, ExPanD starts up in the "advanced mode". The
#' advanced mode uses (a) base sample(s) (the one(s) you provide via \code{df})
#' and the variable definitions in \code{var_def} to generate an analysis
#' sample based on the active base sample. In the advanced mode, the app user
#' can delete variables from the analysis sample within the app.
#'
#' A \code{df_def} or \code{var_def} dataframe can contain the following
#' variables
#' \describe{
#' \item{"var_name"}{Required: The names of the variables
#' that are provided by the base sample
#' or are to be calculated for the analysis sample}
#' \item{"var_def"}{Required: For a \code{var_def} data frame,
#' the code that is passed to the data frame
#' (grouped by cross-sectional units) in calls to
#' \code{\link[dplyr]{mutate}} as right hand side
#' to calculate the respective variable.
#' For a \code{data_def} data frame, a string
#' describing the nature of the variable.}
#' \item{"type"}{Required: One of the strings "cs_id", "ts_id", "factor",
#' "logical" or "numeric", indicating the type of the variable. Please note that
#' at least one variable has to be assigned as a cross-sectional identifier
#' ("cs_id") and exactly one variable that is coercible into an ordered factor
#' has to be assigned as the time-series identifier ("ts_id").}
#' \item{"can_be_na"}{Optional: If included,
#' then all variables with this value set to FALSE are required to be non
#' missing in the data set. This reduces the number of observations. If missing,
#' it defaults to being TRUE for all variables other than cs_id and ts_id.} }
#'
#' @examples
#' \dontrun{
#'   ExPanD()
#'
#'   # Use this if you want to read very large files via the file dialog
#'   options(shiny.maxRequestSize = 1024^3)
#'   ExPanD()
#'
#'   # Explore cross-sectional data
#'   ExPanD(mtcars)
#'
#'   # Include the option to download notebook code and data
#'   ExPanD(mtcars, export_nb_option = TRUE)
#'
#'   # Use ExPanD on long-form panel data
#'   data(russell_3000)
#'   ExPanD(russell_3000, c("coid", "coname"), "period")
#'   ExPanD(russell_3000, df_def = russell_3000_data_def)
#'   ExPanD(russell_3000, df_def = russell_3000_data_def,
#'     components = c(ext_obs = T, descriptive_table = T, regression = T))
#'   ExPanD(russell_3000, df_def = russell_3000_data_def,
#'     components = c(missing_values = F, by_group_violin_graph = F))
#'   ExPanD(russell_3000, df_def = russell_3000_data_def,
#'     components = c(html_block = T, descriptive_table = T,
#'     html_block = T, regression = T),
#'     html_blocks = c(
#'     paste('<div class="col-sm-2"><h3>HTML Block 1</h3></div>',
#'     '<div class="col-sm-10">',
#'     "<p></p>This is a condensed variant of ExPanD with two additional HTML Blocks.",
#'     "</div>"),
#'     paste('<div class="col-sm-2"><h3>HTML Block 2</h3></div>',
#'     '<div class="col-sm-10">',
#'     "It contains only the descriptive table and the regression component.",
#'     "</div>")))
#'   data(ExPanD_config_russell_3000)
#'   ExPanD(df = russell_3000, df_def = russell_3000_data_def,
#'     config_list = ExPanD_config_russell_3000)
#'   exploratory_sample <- sample(nrow(russell_3000), round(0.5*nrow(russell_3000)))
#'   test_sample <- setdiff(1:nrow(russell_3000), exploratory_sample)
#'   ExPanD(df = list(russell_3000[exploratory_sample, ], russell_3000[test_sample, ]),
#'     df_def = russell_3000_data_def,
#'     df_name = c("Exploratory sample", "Test sample"))
#'   ExPanD(worldbank, df_def = worldbank_data_def, var_def = worldbank_var_def,
#'     config_list = ExPanD_config_worldbank)
#'
#'     }
#' @export

ExPanD <- function(df = NULL, cs_id = NULL, ts_id = NULL,
                   df_def = NULL, var_def = NULL, config_list = NULL,
                   title = "ExPanD - Explore your data!",
                   abstract = NULL,
                   df_name = deparse(substitute(df)),
                   long_def = TRUE,
                   factor_cutoff = 10L,
                   components = c(sample_selection = TRUE,
                                  subset_factor = TRUE,
                                  grouping = TRUE,
                                  bar_chart = TRUE,
                                  missing_values = TRUE,
                                  udvars = TRUE,
                                  descriptive_table = TRUE,
                                  histogram = TRUE,
                                  ext_obs = TRUE,
                                  by_group_bar_graph = TRUE,
                                  by_group_violin_graph = TRUE,
                                  trend_graph = TRUE,
                                  quantile_trend_graph = TRUE,
                                  corrplot = TRUE,
                                  scatter_plot = TRUE,
                                  regression = TRUE),
                   html_blocks = NULL,
                   export_nb_option = FALSE,
                   store_encrypted = FALSE,
                   key_phrase = "What a wonderful key",
                   debug = FALSE, ...) {
  orig_df_name <- deparse(substitute(df))
  shiny_df_name <- df_name
  if (!is.null(df) && !is.data.frame(df) && !is.list(df))
    stop("df is neither a dataframe nor a list of dataframes")
  if (!is.null(df) && !is.data.frame(df) &&
      length(which(!sapply(df, is.data.frame))) > 0)
    stop("df is a list containing non-dataframe members")
  if (!is.data.frame(df) && !is.null(df_def) && is.data.frame(df_def))
    df_def <- rep(list(df_def), length(df))
  if (length(factor_cutoff) != 1 && !is.integer(factor_cutoff))
    stop("factor_cutoff needs to be an integer scalar.")

  shiny_cs_data <- !is.null(df) && is.null(ts_id) &&
    is.null(cs_id) && is.null(df_def)
  if (shiny_cs_data) {
    ts_id <- "ts_id"
    cs_id <- "cs_id"
  }
  if(!is.null(df_def)) cs_id <- NULL

  if(!is.null(df)) {
    if(!is.data.frame(df)) {
      if (shiny_cs_data) {
        lapply(df, function(x) x[, "cs_id"] <- row.names(x))
        lapply(df, function(x) x[, "ts_id"] <- 1)
        if(!is.null(df_def)) {
          df_def <- lapply(
            df_def,
            function(x) rbind(
              x,
              list("cs_id", "Cross-sectional indicator", "cs_id", FALSE),
              list("ts_id", "Pseudo time series indicator", "ts_id", FALSE)
            )
          )
        }
      }
      names_df <- lapply(df, names)
      if (!is.null(df_def)) {
        for(i in 1: length(names_df)) {
          df_def[[i]][1:3] <- lapply(df_def[[i]][1:3], as.character)
          if(!identical(names_df[[i]], df_def[[i]]$var_name))
            stop ("Provided data definitions have different variable names than data frames")
        }
        ts_id <- as.character(df_def[[1]][df_def[[1]][, 3] == "ts_id", 1])
        cs_id <- as.character(df_def[[1]][df_def[[1]][, 3] == "cs_id", 1])
      }
      if (! ts_id %in% names_df[[1]]) stop ("Time series identifier not included in data frames.")
      if (! all(cs_id %in% names_df[[1]])) stop ("Not all cross sectional identifier(s) are included in data frames.")
      for (i in 2:length(names_df)) {
        if(!identical(names_df[[1]], names_df[[i]])) stop ("Provided data frames do not have identical variable names")
        if(is.ordered(df[[1]][, ts_id]) &
           !identical(levels(df[[1]][, ts_id]), levels(df[[i]][, ts_id]))) {
          stop("Provided data frames' time series identifiers have different levels")
        }
      }
    } else {
      if (shiny_cs_data) {
        df$cs_id <- row.names(df)
        df$ts_id <- 1
        if (!is.null(df_def)) {
          df_def <- rbind(
            df_def,
            list("cs_id", "Cross-sectional indicator", "cs_id", FALSE),
            list("ts_id", "Pseudo time series indicator", "ts_id", FALSE)
          )
        }
      }
      if (!is.null(df_def)) {
        df_def[1:3] <- lapply(df_def[1:3], as.character)
        if(!identical(names(df), df_def$var_name)) stop ("Provided data definitions have different variable names than data frame")
      } else {
        if (! ts_id %in% names(df)) stop ("Time series identifier not included in data frame.")
        if (! all(ts_id %in% names(df))) stop ("Cross sectional identifier(s) not all included in data frame.")
      }
    }
    tdf <- df
    tdf_def <- df_def
    if(!is.data.frame(df)) {
      tdf <- df[[1]]
      tdf_def <- df_def[[1]]
    }
    if (!is.null(tdf_def)) {
      if(length(which(tdf_def$type == "numeric")) < 2) stop ("Less than two numerical variables contained in data frame. At least two are required.")
    } else {
      sdf <- tdf[, -which(names(tdf) %in% c(cs_id, ts_id))]
      num_numeric <- length(which(sapply(sdf, is.numeric)))
      if(num_numeric < 2) stop ("Less than two numerical variables contained in data frame. At least two are required.")
    }
  }

  comp_names <- c("sample_selection", "subset_factor", "grouping",
                  "bar_chart", "missing_values", "udvars", "descriptive_table",
                  "histogram", "ext_obs", "by_group_bar_graph",
                  "by_group_violin_graph", "trend_graph",
                  "quantile_trend_graph", "corrplot", "scatter_plot",
                  "regression")

  if (!is.vector(components) | !is.logical(components)) stop("components needs to be a vector of logical values")
  # The followiing legacy code is for the old calling style in Version 0.2.0
  # using unnamed vectors
  if (is.null(names(components)) & length(components) == 12) {
    components <- c(rep(TRUE, 4), components)
    names(components) <- comp_names
  }
  if (is.null(names(components))) stop(sprintf("Component vector has missing names and is not of valid length %d", length(comp_names)))
  if (!all(names(components) %in% c(comp_names, "html_block"))) stop(paste("Component vector has invalid names. Valid names are:", paste(c(comp_names, "html_block"), collapse = ", ")))
  if (all(components == FALSE)) {
    rem_components <- comp_names[!(comp_names %in% names(components))]
    components <- rep(TRUE, length(rem_components))
    names(components) <- rem_components
  }
  if (any("html_block" %in% names(components))) {
    if (sum(names(components) == "html_block") != length(html_blocks))
      stop("Number of html_blocks texts provided does not match number of html_block tags in components")
  } else if (!is.null(html_blocks)) stop("html_blocks provided but no html_block tag found in components")

  if(shiny_cs_data) {
    components <- components[!names(components) %in%
                               c("missing_values", "trend_graph",
                                 "quantile_trend_graph")]
  }

  if (!is.logical(export_nb_option))
    stop("export_nb_option needs to be a logical value")

  if(!is.null(var_def)) var_def[1:3] <- lapply(var_def[1:3], as.character)

  shiny_df <- df
  shiny_cs_id <- cs_id
  shiny_ts_id <- ts_id
  shiny_df_def <- df_def
  shiny_var_def <- var_def
  shiny_config_list <- config_list
  shiny_title <- title
  shiny_abstract <- abstract
  if (is.data.frame(df)) {
    shiny_df_id <- orig_df_name
  } else {
    shiny_df_id <- names(df)
    if (is.null(shiny_df_id)) shiny_df_id <- paste0("df list member ", 1:length(df))
  }
  if (!is.data.frame(df) && length(df_name) == 1) shiny_df_name <- paste(df_name, 1:length(df))
  shiny_long_def <- long_def
  shiny_factor_cutoff <- factor_cutoff
  shiny_key_phrase <- key_phrase
  shiny_store_encrypted <- store_encrypted
  shiny_debug <- debug
  shiny_components <- components[components]
  shiny_html_blocks <- html_blocks
  shiny_export_nb_option <- export_nb_option

  pkg_app_dir <- system.file("application", package = "ExPanDaR")
  file.copy(pkg_app_dir, tempdir(), recursive=TRUE)
  app_dir <- paste0(tempdir(), "/application")
  save(list = ls(pattern = "shiny"), file = paste0(app_dir, "/shiny_data.Rda"))
  on.exit(unlink(app_dir, recursive = TRUE))
  try(shiny::runApp(appDir = app_dir, ...))
}
