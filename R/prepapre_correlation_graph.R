cor_mat <- function(x, ...) {
  mat <- as.matrix(x)
  n <- ncol(mat)
  out_r <- out_n <- out_p <- matrix(NA, n, n)
  diag(out_r) <- 1
  diag(out_p) <- 0.00
  diag(out_n) <- colSums(!is.na(mat))

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      out_n[i, j] <- out_n[j, i] <- sum(is.finite(mat[, i]) & is.finite(mat[, j]))
      if (out_n[i, j] > 2) {
        tmp <- stats::cor.test(mat[, i], mat[, j], ...)
        out_r[i, j] <- out_r[j, i] <- tmp$estimate
        out_p[i, j] <- out_p[j, i] <- tmp$p.value
      }
    }
  }
  colnames(out_r) <- rownames(out_r) <- colnames(mat)
  colnames(out_p) <- rownames(out_p) <- colnames(mat)
  colnames(out_n) <- rownames(out_n) <- colnames(mat)
  list(r = out_r, p = out_p, n = out_n)
}


#' @title Prepares a Correlation Graph
#'
#' @description
#' Reads a data frame and presents Pearson correlations above
#' and Spearman correlations the diagonal using a fancy graph prepared
#' by the package \code{corrplot}.
#'
#' @param df Data frame containing at least two variables that are either numeric
#'   or logical and at least five observations.
#' @return The function directly renders the graph as produced by \code{corrplot}.
#' In addition, it returns a list containing three items:
#' \describe{
#'  \item{"df_corr"}{A data frame containing the correlations}
#'  \item{"df_prob"}{A data frame containing the p-values of the correlations}
#'  \item{"df_n"}{A data frame containing the number of observations used for the correlations}
#' }
#' @examples
#' prepare_correlation_graph(mtcars)
#' @export

prepare_correlation_graph <- function(df) {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  df <- df[sapply(df, is.logical) | sapply(df, is.numeric)]
  if (nrow(df) < 5 | ncol(df) < 2)
    stop("'df' needs to contain at least two variables and five observations of numerical data")

  pcorr <- cor_mat(as.matrix(df), method="pearson", na.action = "na.omit", exact = FALSE)
  scorr <- cor_mat(as.matrix(df), method="spearman", na.action = "na.omit", exact = FALSE)
  correl_r <- pcorr$r
  correl_r[lower.tri(correl_r)] <- scorr$r[lower.tri(scorr$r)]
  # Hmisc:rcorr returns r/rho values > 1 from time to time
  # (of course only by a very small margin) and this
  # causes corrplot to choke
  # Not sure wheter cor.test() does the same leaving
  # this here for the time being
  correl_r <- pmin(correl_r,1)
  correl_n <- pcorr$n
  correl_n[lower.tri(correl_n)] <- scorr$n[lower.tri(scorr$n)]
  correl_p <- pcorr$p
  correl_p[lower.tri(correl_p)] <- scorr$p[lower.tri(scorr$p)]
  corrplot::corrplot(correl_r, type = "upper", method="ellipse", tl.pos="lt", tl.col="black", bg="grey90", addgrid.col="white")
  corrplot::corrplot(correl_r, add=TRUE, type = "lower", diag=FALSE, tl.pos="n", cl.pos="n", method="ellipse", bg="grey90", addgrid.col="white")
  return(list(df_corr = correl_r, df_prob = correl_p, df_n = correl_n))
}
