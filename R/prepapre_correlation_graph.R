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

  pcorr <- Hmisc::rcorr(as.matrix(df), type="pearson")
  scorr <- Hmisc::rcorr(as.matrix(df), type="spearman")
  correl_r <- pcorr$r
  correl_r[lower.tri(correl_r)] <- scorr$r[lower.tri(scorr$r)]
  # Hmisc:rcorr returns r/rho values > 1 from time to time
  # (of course only by a very small margin) and this
  # causes corrplot to choke
  correl_r <- pmin(correl_r,1)
  correl_n <- pcorr$n
  correl_n[lower.tri(correl_n)] <- scorr$n[lower.tri(scorr$n)]
  correl_p <- pcorr$P
  correl_p[lower.tri(correl_p)] <- scorr$P[lower.tri(scorr$P)]
  corrplot::corrplot(correl_r, type = "upper", method="ellipse", tl.pos="lt", tl.col="black", bg="grey90", addgrid.col="white")
  corrplot::corrplot(correl_r, add=TRUE, type = "lower", diag=FALSE, tl.pos="n", cl.pos="n", method="ellipse", bg="grey90", addgrid.col="white")
  return(list(df_corr = correl_r, df_prob = correl_p, df_n = correl_n))
}
