#' @title Prepares a Table of Descriptive Statistics
#'
#' @description
#' Reads a data frame and reports descriptive statistics
#' (n, mean, standard deviation, minimum, first quartile, median,
#' third quartile, maximum) for all members of the data frame that are either
#' numeric or logical.
#'
#' @param df Data frame containing at least one variable that is either numeric
#'   or logical and at least two observations.
#' @param digits Number of decimal digits that you want to be displayed for each
#'   column. If you provide NA, then the column is omitted from the output.
#' @param format character scalar that is handed over to \code{\link[knitr]{kable}}
#'   (e.g., "html" or "latex").
#' @return A list containing two items.
#' \describe{
#'  \item{"df"}{A data frame containing the descriptive table}
#'  \item{"kable_ret"}{The return value provided by \code{\link[knitr]{kable}} containing the formatted table}
#' }
#'
#'
#' @details
#' The \code{digits} parameter from \code{prepare_descriptive_table()} uses the default method of
#'   \code{\link[knitr]{kable}} to format numbers, calling \code{\link{round}}. This implies that trailing zeroes are
#'   just omitted.
#'
#' @examples
#' t <- prepare_descriptive_table(mtcars)
#' t$df
#' @export


prepare_descriptive_table <- function(df, digits = c(0, 3, 3, 3, 3, 3, 3, 3), format = "html") {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  df <- df[sapply(df, is.logical) | sapply(df, is.numeric)]
  if ((ncol(df) < 1) | (nrow(df) < 2)) stop("insuitable data frame (does it contain numerical data?)")
  if (!is.numeric(digits) | length(digits) != 8) stop("digits vector is not numeric or has wrong length (!= 8)")

  t <- cbind(apply(df,2,function(x) as.integer(sum(!is.na(x)))),
             apply(df,2,mean, na.rm=TRUE),
             apply(df,2,stats::sd, na.rm=TRUE),
             t(apply(df, 2, function(x) stats::quantile(x, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))))
  colnames(t) <- c("N", "Mean", "Std. dev.", "Min.", "25 %", "Median", "75 %", "Max.")
  t <- as.data.frame(t)
  t$N <- as.integer(t$N)
  t <- t[which(!is.na(digits))]
  digits <- digits[!is.na(digits)]
  list(df = t, kable_ret = knitr::kable(t, format, digits,
                                        format.args = list(big.mark = ","),
                                        caption = "Descriptive Statistics"))
}
