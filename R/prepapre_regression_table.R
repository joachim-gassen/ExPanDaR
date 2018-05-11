estimate_model <- function(df, dl) {
  dv <- dl$dvs
  idvs <- dl$idvs
  feffects <- dl$feffects
  clusters <- dl$clusters
  fe_str <- gsub("_","",paste(feffects, collapse = ", ")) # stargaze chokes on _
  cl_str <- gsub("_","",paste(clusters, collapse = ", ")) # stargaze chokes on _
  if ((feffects[1] != "" & clusters[1] != "") & (!is.factor(df[,dv]))) {
    f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + "), " | ",
                          paste(feffects, collapse = " + "), " | 0 | ", paste(clusters, collapse = " + ")))
  } else if (!is.factor(df[,dv]) & feffects[1] != "") {
    f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + "), " | ",
                          paste(feffects, collapse = " + ")))
  } else if (!is.factor(df[,dv]) & clusters[1] != "") {
    f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + "), " | 0 | 0 | ",
                          paste(clusters, collapse = " + ")))
  } else {
    f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + ")))
    if (is.factor(df[,dv]) & nlevels(df[,dv]) > 2) stop("multinomial logit is not implemented. Sorry.")
    if (is.factor(df[,dv]) & feffects != "") stop("fixed effects logit is not implemented. Sorry.")
  }
  if (is.factor(df[,dv])) {
    type_str = "logit"
    model <- glm(f, family = "binomial", df)
  } else {
    type_str = "OLS"
    model <- lfe::felm(f, data=df, psdef=FALSE)
  }
  list(model = model, type_str = type_str, fe_str = fe_str, cl_str = cl_str)
}


#' @title Prepares a Regression Table
#'
#' @description
#' Builds a regression table based on a set of user-specified models or a single model and a partitioning variable.
#'
#' @param df Data frame containing the data to estimate the models on.
#' @param dvs A character vector containing the dependent variable(s).
#' @param idvs A character vector or a a list of character vectors containing the independent variables.
#' @param feffects A character vector or a a list of character vectors containing the fixed effects.
#' @param clusters A character vector or a a list of character vectors containing the cluster variables.
#' @param byvar A factorial variable to estimate the model on (only possible if only one model is being estimated).
#' @param format A character scalar that is passed on \code{\link[stargazer]{stargazer}} as \code{type} to determine the presentation
#'   format ("html", "text", or "latex").
#' @param drop_underscore A quick'n'dirty hack to address a bug in stargazer that triggers it to choke on underscores in variable names.
#'   If not NULL, all underscores in variable names will be replaced by the given string.
#'
#' @return A list containing two items
#' \describe{
#'  \item{"models"}{A list containing the model results and by values if appropriate}
#'  \item{"table"}{The output of \code{\link[stargazer]{stargazer}} containing the table}
#' }
#'
#' @details
#' This is a wrapper function calling the stargazer package. Depending on whether the dependent variable
#'   is numeric or a factor with two levels, the models are estimated
#'   using \code{\link[lfe]{felm}} or \code{\link[stats]{glm}} (with \code{family = binomial(link="logit")}).
#'   Fixed effects and clustered standard errors are only supported with continuous dependent variables.
#'   If run with \code{byvar}, only levels that have more observations than coefficients are estimated.
#'
#'
#' @examples
#' df <- data.frame(year = as.factor(floor(stats::time(datasets::EuStockMarkets))),
#'                  datasets::EuStockMarkets)
#' dvs = c("DAX", "SMI", "CAC", "FTSE")
#' idvs = list(c("SMI", "CAC", "FTSE"),
#'             c("DAX", "CAC", "FTSE"),
#'             c("SMI", "DAX", "FTSE"),
#'             c("SMI", "CAC", "DAX"))
#' feffects = list("year", "year", "year", "year")
#' clusters = list("year", "year", "year", "year")
#' t <- prepare_regression_table(df, dvs, idvs, feffects, clusters, format = "text")
#' t$table
#' t <- prepare_regression_table(df, "DAX", c("SMI", "CAC", "FTSE"), byvar="year", format = "text")
#' print(t$table)
#' @export

prepare_regression_table <- function(df, dvs, idvs, feffects = rep("", length(dvs)),
                                     clusters = rep("", length(dvs)), byvar = "", format = "html", drop_underscore = NULL) {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  if (byvar != "") if(!is.factor(df[,byvar])) stop("'byvar' needs to be a factor.")
  if ((length(dvs) > 1) & byvar != "") stop("you cannot subset multiple models in one table")
  if ((length(dvs) > 1) &&
      ((length(dvs) != length(idvs)) | (length(dvs) != length(feffects)) | (length(dvs) != length(clusters))))
    stop("'dvs', 'idvs', 'feffects' and 'clusters' need to be of equal lenghth.")
  if (!is.null(drop_underscore)) {
    names(df) <- gsub("_", drop_underscore, names(df))
    dvs <- gsub("_", drop_underscore, dvs)
    if (is.list(idvs))
      idvs <- lapply(idvs, function(x) gsub("_", drop_underscore, x))
    else idvs <- gsub("_", drop_underscore, idvs)
    if (is.list(feffects))
      feffects <- lapply(feffects, function(x) gsub("_", drop_underscore, x))
    else feffects <- gsub("_", drop_underscore, feffects)
    if (is.list(clusters))
      clusters <- lapply(clusters, function(x) gsub("_", drop_underscore, x))
    else clusters <- gsub("_", drop_underscore, clusters)
    byvar <- gsub("_", drop_underscore, byvar)
  }
  datalist <- list()
  if (byvar != "") {
    datalist <- list(dvs = dvs,
                          idvs = idvs,
                          feffects = feffects,
                          clusters = clusters)
    vars <- c(byvar, dvs, idvs, feffects, clusters)
    vars <- vars[which(!vars %in% "")]
    df <- df[stats::complete.cases(df[, vars]), vars]
    bylevels <- unique(as.character(df[,byvar]))[order(unique(df[,byvar]))]
    n_by_level <- unlist(lapply(bylevels, function(x) nrow(df[df[,byvar] == x,])))
    bylevels <- bylevels[n_by_level > length(idvs) + 1]
    if (length(bylevels) < 1) stop("no by levels with sufficient degrees of freedom to estimate model")
    mby <- lapply(bylevels, function(x) estimate_model(df[df[,byvar] == x,], datalist))
    models <- list()
    models[[1]] <- estimate_model(df, datalist)
    models[[1]]$byvalue <- "Full"
    for (i in 2:(length(mby) + 1)) {
      models[[i]] <- mby[[i-1]]
      models[[i]]$byvalue <- bylevels[i-1]
    }
  } else {
    if (length(dvs) > 1) {
      for (i in 1:length(dvs))
        datalist[[i]] <- list(dvs = dvs[[i]],
                              idvs = idvs[[i]],
                              feffects = feffects[[i]],
                              clusters = clusters[[i]])
      models <- lapply(datalist, function (x) estimate_model(df, x))
    } else {
      datalist <- list(dvs = dvs,
                            idvs = idvs,
                            feffects = feffects,
                            clusters = clusters)
      models <- list(estimate_model(df, datalist))
    }
  }
  fe_str <- "Fixed effects"
  cl_str <- "Std. errors clustered"
  m <- list()
  ret <- list()
  for (i in 1:length(models)) {
    if (models[[i]]$fe_str != "")  fe_str <- c(fe_str, models[[i]]$fe_str)
    else fe_str <- c(fe_str, "None")
    if (models[[i]]$cl_str != "")  cl_str <- c(cl_str, models[[i]]$cl_str)
    else cl_str <- c(cl_str, "No")
    m[[i]] <- models[[i]]$model
    ret[[i]] <- models[[i]]
  }
  if (byvar != "") {
    labels <- gsub("_", "", c("Full Sample", levels(df[,byvar])))
    labels <- gsub("&", "+", labels)
    htmlout <- utils::capture.output(stargazer::stargazer(m,
                                                   type=format,
                                                   column.labels = labels,
                                                   omit.stat = c("f", "ser"),
                                                   add.lines=list(fe_str, cl_str),
                                                   dep.var.labels=dvs))
  } else htmlout <- utils::capture.output(stargazer::stargazer(m,
                                         type=format,
                                         omit.stat = c("f", "ser"),
                                         add.lines=list(fe_str, cl_str),
                                         dep.var.labels=unlist(dvs)))
  list(models = ret, table = htmlout)
}
