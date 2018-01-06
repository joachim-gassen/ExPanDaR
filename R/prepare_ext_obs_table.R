#' @title Prepares a Table Displaying Extreme Observations
#'
#' @description
#' Reads a data frame containing the variable of interest as the last variable,
#'   sorts it by it and displays the top and bottom n variables.
#' @param df Data frame containing at least one variable that is numeric as variable of
#'   interest. Most likely it will also contain the variables identifying an observation.
#' @param n The number of top/bottom observations that you want to report.
#' @param ... Additional parameters that are passed to \code{knitr::kable()}.
#' @return A list containing two items:
#' \itemize{
#'  \item{"df"}{A data frame containg the top/bottom n observations}
#'  \item{"kable_ret"}{The return value provided by \code{knitr::kable()} containing the formatted table}
#' }
#'
#' @details The default parameters for calling \code{knitr::kable},
#'   are \code{format = "html", digits = 3, format.args = list(big.mark = ','), row.names = FALSE}.
#'
#' @examples
#' t <- prepare_ext_obs_table(data.frame(name = rownames(mtcars), hp = mtcars$hp))
#' t$kable_ret
#' @export


prepare_ext_obs_table <- function(df, n = 5, ...) {
  if(!is.numeric(df[,ncol(df)]))
    stop("last variable of df is not numeric")
  if (2*n > nrow(df))
    stop("'n' needs to be <= nrow(df)/2")

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
