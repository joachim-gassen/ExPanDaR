#' @title Prepares a Correlation Table
#'
#' @description
#' Reads a data frame and presents Pearson correlations above the diagonal
#' and Spearman correlations below.
#'
#' @param df Data frame containing at least two variables that are either numeric
#'   or logical and at least five observations.
#' @param digits The number of digits that you want to report.
#' @param bold Indicate the p-Value for for identifying significant correlations
#'   in bold brint. Defaults to 0.05. If set to NA, no bold print is being used.
#' @param format The format that you want knitr::kable to produce ("html" or "latex")
#' @return A list containing four items:
#' \itemize{
#'  \item{"df_corr"}{A data frame containg the correlations}
#'  \item{"df_prob"}{A data frame containg the p-values of the correlations}
#'  \item{"df_n"}{A data frame containg the number of observations used for the correlations}
#'  \item{"kable_ret"}{The return value provided by kable() containing the formatted table}
#' }
#'
#' @examples
#' t <- prepare_correlation_table(mtcars)
#' t$kable_ret
#' @export

prepare_correlation_table <- function(df, digits = 2, bold = 0.05, format = "html") {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- df[sapply(df, is.logical) | sapply(df, is.numeric)]
  if (nrow(df) < 5 | ncol(df) < 2)
    stop("'df' needs to contain at least two variables and five observations of numerical data")
  if (!is.numeric(digits) | length(digits) != 1 | digits <= 0 | digits >= 5)
    stop("argument 'digits' needs to be a numerical scalar with 0 < digits <= 5")
  if (!is.numeric(bold) | length(bold) != 1 | bold <= 0 | bold >= 1)
    stop("argument 'bold' needs to be a numerical scalar with 0 < bold < 1")

  pcorr <- Hmisc::rcorr(as.matrix(df), type="pearson")
  scorr <- Hmisc::rcorr(as.matrix(df), type="spearman")
  correl_r <- pcorr$r
  correl_r[lower.tri(correl_r)] <- scorr$r[lower.tri(scorr$r)]
  correl_n <- pcorr$n
  correl_n[lower.tri(correl_n)] <- scorr$n[lower.tri(scorr$n)]
  correl_p <- pcorr$P
  correl_p[lower.tri(correl_p)] <- scorr$P[lower.tri(scorr$P)]
  diag(correl_p) <- 1
  fted_correl_r <- matrix(sapply(1:ncol(correl_r)^2,
                          function(pos) kableExtra::cell_spec(sprintf(paste0("%.", digits, "f"), correl_r[pos]),
                                                              format,
                                                              bold = ifelse(correl_p[pos] < bold, TRUE, FALSE))),
                          nrow = nrow(correl_r), ncol = ncol(correl_r))
  diag(fted_correl_r) <- ""
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  colnames(fted_correl_r) <- LETTERS702[1:ncol(correl_r)]
  rownames(fted_correl_r) <- paste0(LETTERS702[1:ncol(correl_r)], ": ",rownames(correl_r))
  kr <- knitr::kable(fted_correl_r, align = rep("r", ncol(correl_r)),
                     caption = "Correlations[note]", format, escape = FALSE)
  kr <- kableExtra::add_footnote(kr, paste("This table reports Pearson correlations above and spearman correlations below the diagonal.",
                                           ifelse(max(correl_n) == min(correl_n),
                                                  sprintf("Number of observations: %d.", min(correl_n)),
                                                  sprintf("The number of observations ranges from %d to %d.", min(correl_n), max(correl_n))),
                                           sprintf("Correlations with significance levels below %.0f%% appear in bold print.", bold*100)),
                                 notation = "symbol")

  list(df_corr = as.data.frame(correl_r),
       df_prob = as.data.frame(correl_p),
       df_n = as.data.frame(correl_n),
       kable_ret = kr)
}
