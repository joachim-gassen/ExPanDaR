treat_vector_outliers <- function(x, truncate, percentile) {
  lim <- stats::quantile(x, probs=c(percentile, 1-percentile), na.rm = TRUE)
  if (!truncate) {
    x[x < lim[1]] <- lim[1]
    x[x > lim[2]] <- lim[2]
  } else {
    x[x < lim[1]] <- NA
    x[x > lim[2]] <- NA
  }
  x
}


#' @title Treats Outliers in Numerical Data
#'
#' @description
#' Treats numerical outliers either by winsorizing or by truncating.
#'
#' @param x Data that is coercible into a numeric vector or matrix.
#'   If it is a data frame then all numerical variables
#'   of the data frame are coerced into a matrix.
#' @param percentile A numeric scalar.
#'   The percentile below which observations
#'   are considered to be outliers. Is treated symmetrical so that
#' \code{c(percentile, 1-percentile)} are used as boundaries.
#'   Defaults to 0.01 and needs to be > 0 and < 0.5.
#' @param truncate A logical scalar. If TRUE then data are truncated
#'   (i.e., set to NA if out of bounds). Defaults to FALSE.
#' @param by NULL or either a factor vector or a character string
#'   identifying a factor variable in the data frame provided by x.
#'   The factor indicated by 'by' is being used to identify groups
#'   by which the outlier treatment is applied. Defaults to NULL (no grouping).
#'   If provided, the resulting vector must not contain NAs and needs to be such so that
#'   \code{length(byvec) == nrows(as.matrix(x))}.
#' @return A numeric vector or matrix containing the outlier-treated \code{x}.
#'   if a data frame was provided in \code{x}, a data frame with its numeric variables
#'   replaced by their outlier-treated values.
#'
#' @details All members of the numerical matrix are checked for finiteness and are
#'   set to NA if they are not finite.
#'
#' @examples
#' treat_outliers(seq(1:100), 0.05)
#' treat_outliers(seq(1:100), truncate = TRUE, 0.05)
#'
#' df <- data.frame(a = seq(1:1000), b = rnorm(1000), c = sample(LETTERS[1:5], 1000, replace=TRUE))
#' winsorized_df <- treat_outliers(df)
#' summary(df)
#' summary(winsorized_df)
#'
#' winsorized_df <- treat_outliers(df, 0.05, by="c")
#' by(df, df$c, summary)
#' by(winsorized_df, df$c, summary)
#'
#' hist(treat_outliers(rnorm(1000)), breaks=100)
#' @export

treat_outliers <- function(x, percentile = 0.01, truncate = FALSE, by = NULL) {
  x_is_df <- is.data.frame(x)
  x_is_vector <- is.vector(x)
  x_is_matrix <- is.matrix(x)
  if (!x_is_df & !x_is_vector & !x_is_matrix)
    stop("'x' is of invalid type")
  if (x_is_vector) lenx <- length(x)
  else lenx <- nrow(x)

  if (!is.numeric(percentile) || (length(percentile) != 1))
    stop("bad value for 'percentile': Needs to be a numeric scalar")
  if (percentile <= 0 | percentile >= 0.5) {
    stop("bad value for 'percentile': Needs to be > 0 and < 0.5")
  }

  if (length(truncate) != 1 || !is.logical(truncate))
    stop("bad value for 'truncate': Needs to be a logical scalar")

  if (!is.null(by)) {
    if (is.character(by) & !x_is_df)
      stop("'by' is a string but no data frame provided.")
    if (is.character(by)) by <- as.vector(x[[by]])
    else by <- as.vector(by)
    if (anyNA(by))
      stop("by vector contains NA values")
    if (length(by) != lenx)
      stop("by vector number of rows differs from x")
  }

  if (x_is_df) {
    df <- x
    x <- x[sapply(x, is.numeric)]
  }
  if (x_is_matrix | x_is_vector)
    x <- as.data.frame(x)
  if (!is.numeric(as.matrix(x)))
    stop("bad value for 'x': needs to contain numeric vector or matrix")
  x <- do.call(data.frame, lapply(x, function(xv) replace(xv,
                                                          !is.finite(xv), NA)))
  if (is.null(by))
    retx <- as.data.frame(lapply(x, function(vx) treat_vector_outliers(vx,
                                                                       truncate, percentile)))
  else {
    old_order <- (1:lenx)[order(by)]
    retx <- do.call(rbind,
                    by(x, by,
                       function(mx)
                         apply(mx, 2,
                               function(vx) treat_vector_outliers(vx, truncate, percentile))))
    retx <- as.data.frame(retx[order(old_order),])
  }
  if (x_is_vector)
    return(retx[, 1])
  if (x_is_df) {
    df[colnames(retx)] <- retx
    return(df)
  }
  else return(as.matrix(retx))
}

