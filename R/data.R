#' Annual Financial Accounting and Stock Return Data for a Sample of Russell 3000 Firms (2013-2016)
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
#'  Use in scientific studies is not advised without prior cleaning/checking.
#'
#' @examples
#' data(russell_3000)
#' prepare_missing_values_graph(russell_3000, period = "period")
#'
"russell_3000"

#' Default Configuration to use with ExPanD and the Russell 3000 Data Set
#'
#' List to use as a \code{list_config} parameter when starting \link{ExPanD}.
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
#' \dontrun{
#'   ExPanD(russell_3000, c("coid", "coname"), "period", ExPanD_config_russell_3000)
#' }
"ExPanD_config_russell_3000"

#' A Snapshot of Macroeconomic Data as Provided by the World Bank API (1960 - 2016)
#'
#' Data collected from the World Bank API using the package \code{wbstats}.
#'
#' @docType data
#'
#' @usage data(worldbank)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets, macroeconomic data, country level data
#'
#' @source Has been collected using the \link[wbstats]{wb} function
#'  from the World Bank API in March 2018.
#'  The code to generate this data is available in the
#'  \href{https://github.com/joachim-gassen/ExPanDaR}{github repository} of this package.
#'  Use in scientific studies is not advised without prior cleaning/checking.
#'
#' @examples
#' data(worldbank)
#' prepare_missing_values_graph(worldbank, period = "year")
#'
"worldbank"

#' Data Definitions for \code{worldbank} Data Set
#'
#' A data frame containing variable definitions for the \code{worldbank}.
#' The data definitions can be passed to \link{ExPanD} via the
#' \code{df_def} parameter.
#'
#' Data definitions are as provided by the World Bank API and
#' the code to generate them is available in the
#'  \href{https://github.com/joachim-gassen/ExPanDaR}{github repository}
#'  of this package.
#'
#' @docType data
#'
#' @usage data(worldbank_var_def)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords ExPanD, config
#'
#' @examples
#' data(worldbank)
#' data(worldbank_data_def)
#' data(worldbank_var_def)
#' data(ExPanD_config_worldbank)
#' \dontrun{
#'   ExPanD(worldbank, "country", "year", df_def = worldbank_data_def,
#'     var_def = worldbank_var_def, config_list = ExPanD_config_worldank)
#' }
"worldbank_data_def"

#' Variable Definitions to Construct Analysis Sample Based on \code{worldbank} Data Set
#'
#' A data frame containing variable definitions that can be passed to \link{ExPanD}
#' via the var_def parameter.
#'
#' @docType data
#'
#' @usage data(worldbank_var_def)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords ExPanD, config
#'
#' @examples
#' data(worldbank)
#' data(worldbank_data_def)
#' data(worldbank_var_def)
#' data(ExPanD_config_worldbank)
#' \dontrun{
#'   ExPanD(worldbank, "country", "year", df_def = worldbank_data_def,
#'     var_def = worldbank_var_def, config_list = ExPanD_config_worldank)
#' }
"worldbank_var_def"

#' Default Configuration to Use with ExPanD and the \code{worldbank} Data Set
#'
#' List to use as a \code{list_config} parameter when starting \link{ExPanD}.
#'
#' @docType data
#'
#' @usage data(ExPanD_config_worldbank)
#'
#' @format An object of class \code{"list"}.
#'
#' @keywords ExPanD, config
#'
#' @examples
#' data(worldbank)
#' data(worldbank_var_def)
#' data(ExPanD_config_worldbank)
#' \dontrun{
#'   ExPanD(worldbank, "country", "year",
#'     var_def = worldbank_var_def, config_list = ExPanD_config_worldank)
#' }
"ExPanD_config_worldbank"
