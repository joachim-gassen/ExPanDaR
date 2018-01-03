

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

#' @title A function to treat outliers in numerical data
#'
#' @description
#' Treats numerical outliers either by winsorizing or by truncating.
#'
#' @param x A numeric vector or matrix.
#' @param percentile A numeric scalar.
#'   The percentile below which observations
#'   are considered to be outliers. Is treated symmetrical so that
#' \code{c(percentile, 1-percentile)} are used as boundaries.
#'   Defaults to 0.01.
#' @param truncate A logical scalar. If TRUE then data are truncated
#'   (i.e., set to NA if out of bounds). Defaults to FALSE.
#' @param byvec NULL or a factor vector containing groups
#'   by which the outlier treatment is applied. Defaults to NULL.
#'   If provided, it needs to be such so that \code{length(byvec) == nrows(as.matrix(x))}.
#' @return A numeric vector or matrix containing the outlier-treated \code{x}.
#' @examples
#' treat_outliers(seq(1:100), 0.05)
#' treat_outliers(seq(1:100), truncate = TRUE, 0.05)
#' summary(treat_outliers(data.frame(a = seq(1:100), b = rnorm(100)), percentile = 0.05))
#' hist(treat_outliers(rnorm(1000)), breaks=100)
#' @export
treat_outliers <- function(x, percentile = 0.01, truncate = FALSE, byvec = NULL) {
  if(length(percentile) != 1 || percentile < 0 ||
     percentile > 0.5) {
    stop("bad value for 'percentile': Needs to be between 0 and 0.5.")
  }
  if(!is.numeric(as.matrix(x)))
    stop("bad value for 'x': needs to be coercible into a numeric vector or matrix.")
  if(length(truncate) != 1 || !is.logical(truncate)) {
    stop("bad value for 'truncate': Needs to be a logical scalar.")
  }
  if (is.null(byvec)) {
    if (is.vector(x)) treat_vector_outliers(x, truncate, percentile)
    else apply(x, 2, function(vx) treat_vector_outliers(vx, truncate, percentile))
  }
  else {
    if (length(byvec) != nrow(as.matrix(x))) stop("by vector number of rows differs from df")
    if (is.vector(x)) do.call(rbind, by(x, byvec, treat_vector_outliers(x, percentile, truncate)))
    else do.call(rbind,
            by(x, byvec, function (mx) apply(mx, 2, function(vx) treat_vector_outliers(vx, truncate, percentile))))
  }
}
