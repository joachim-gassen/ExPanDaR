#' @title Prepares a Correlation Graph
#'
#' @description
#' Reads a data frame and presents Pearson correlations above the diagonal
#' and Spearman correlations using a fany graph prepared by the package \code{corrplot}.
#'
#' @param df Data frame containing at least two variables that are either numeric
#'   or logical and at least five observations.
#' @return The graph as prepared by corrplot
#'
#' @examples
#' prepare_correlation_graph(mtcars)
#' @export

prepare_correlation_graph <- function(df) {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- df[sapply(df, is.logical) | sapply(df, is.numeric)]
  if (nrow(df) < 5 | ncol(df) < 2)
    stop("'df' needs to contain at least two variables and five observations of numerical data")

  pcorr <- Hmisc::rcorr(as.matrix(df), type="pearson")
  scorr <- Hmisc::rcorr(as.matrix(df), type="spearman")
  correl_r <- pcorr$r
  correl_r[lower.tri(correl_r)] <- scorr$r[lower.tri(scorr$r)]
  correl_n <- pcorr$n
  correl_n[lower.tri(correl_n)] <- scorr$n[lower.tri(scorr$n)]
  correl_p <- pcorr$P
  correl_p[lower.tri(correl_p)] <- scorr$P[lower.tri(scorr$P)]
  corrplot::corrplot(pcorr$r, type = "upper", method="ellipse", tl.pos="lt", tl.col="black", bg="grey90", addgrid.col="white")
  corrplot::corrplot(scorr$r, add=TRUE, type = "lower", diag=FALSE, tl.pos="n", cl.pos="n", method="ellipse", bg="grey90", addgrid.col="white")
}
