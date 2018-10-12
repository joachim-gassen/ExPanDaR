escape_for_latex <- function(s) {
  if (!is.character(s)) s_out <- as.character(s)
  else s_out <- s
  s_out <- gsub("\\", "\\textbackslash", s_out, fixed = TRUE)
  s_out <- gsub("&", "\\&", s_out, fixed = TRUE)
  s_out <- gsub("%", "\\%", s_out, fixed = TRUE)
  s_out <- gsub("#", "\\#", s_out, fixed = TRUE)
  s_out <- gsub("_", "\\_", s_out, fixed = TRUE)
  s_out <- gsub("{", "\\{", s_out, fixed = TRUE)
  s_out <- gsub("}", "\\}", s_out, fixed = TRUE)
  s_out <- gsub("~", "\\textasciitilde ", s_out, fixed = TRUE)
  s_out <- gsub("^", "\\textasciicircum ", s_out, fixed = TRUE)
  return(s_out)
}

estimate_model <- function(df, dl) {
  dv <- dl$dvs
  idvs <- dl$idvs
  feffects <- dl$feffects
  clusters <- dl$clusters
  type_str <- dl$models
  fe_str <- paste(feffects, collapse = ", ")
  cl_str <- paste(clusters, collapse = ", ")
  se <- NULL
  p <- NULL
  omit_vars <- NULL
  if (type_str == "auto") {
    if(!(is.factor(df[,dv]) | is.logical(df[,dv]))) type_str <- "ols"
    else type_str <- "logit"
  }
  if (feffects[1] != "") {
    df[,feffects] <- lapply(as.data.frame(df[,feffects]), function (x) factor(x, ordered = FALSE))
  }
  if(type_str == "ols") {
    if (feffects[1] != "" & clusters[1] != "") {
      f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + "), " | ",
                                   paste(feffects, collapse = " + "), " | 0 | ", paste(clusters, collapse = " + ")))
    } else if (feffects[1] != "") {
      f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + "), " | ",
                                   paste(feffects, collapse = " + ")))
    } else if (clusters[1] != "") {
      f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + "), " | 0 | 0 | ",
                                   paste(clusters, collapse = " + ")))
    } else {
      f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + ")))
    }
  } else if (nlevels(as.factor(df[,dv])) > 2) {
    stop("multinomial logit is not implemented. Sorry.")
  } else {
    df[,dv] <- as.factor(df[,dv])
    if (feffects[1] != "") {
      f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + "), " + ", paste(feffects, collapse = " + ")))
    } else {
      f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + ")))
    }
  }
  if (type_str == "logit") {
    model <- glm(f, family = "binomial", df)
    model$pseudo_r2 <- 1 - model$deviance / model$null.deviance
    model$success <- levels(as.factor(df[, dv]))[2]
    if (clusters[1] != "") {
      vcov <- multiwayvcov::cluster.vcov(model, cluster = df[, clusters])
      test <- lmtest::coeftest(model, vcov)
      se <- test[,2]
      p <- test[,4]
    }
    if (feffects[1] != "") {
        omit_vars <- (length(idvs) + 1):length(model$coefficients)
    }
  } else {
    model <- lfe::felm(f, data=df, psdef=FALSE)
  }
  list(model = model, type_str = type_str, fe_str = fe_str, cl_str = cl_str, p = p, se = se, omit_vars = omit_vars)
}


#' @title Prepares a Regression Table
#'
#' @description
#' Builds a regression table based on a set of user-specified models or a single model and a partitioning variable.
#'
#' @param df Data frame containing the data to estimate the models on.
#' @param dvs A character vector containing the variable names for the dependent variable(s).
#' @param idvs A character vector or a a list of character vectors containing the variable names of the independent variables.
#' @param feffects A character vector or a a list of character vectors containing the variable names of the fixed effects.
#' @param clusters A character vector or a a list of character vectors containing the variable names of the cluster variables.
#' @param models A character vector indicating the model types to be estimated ('ols', 'logit', or 'auto')
#' @param byvar A factorial variable to estimate the model on (only possible if only one model is being estimated).
#' @param format A character scalar that is passed on \code{\link[stargazer]{stargazer}} as \code{type} to determine the presentation
#'   format ("html", "text", or "latex").
#'
#' @return A list containing two items
#' \describe{
#'  \item{"models"}{A list containing the model results and by values if appropriate}
#'  \item{"table"}{The output of \code{\link[stargazer]{stargazer}} containing the table}
#' }
#'
#' @details
#' This is a wrapper function calling the stargazer package. Depending on whether the dependent variable
#'   is numeric, logical or a factor with two levels, the models are estimated
#'   using \code{\link[lfe]{felm}} (for numeric dependent variables)
#'   or \code{\link[stats]{glm}} (with \code{family = binomial(link="logit")}) (for two-level factors or logical variables).
#'   You can override this behavior by specifying the model with the \code{models} parameter.
#'   Multinomial logit models are not supported.
#'   For \code{\link[stats]{glm}}, clustered standard errors are estimated using
#'   \code{\link[multiwayvcov]{cluster.vcov}}.
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
                                     clusters = rep("", length(dvs)),
                                     models = rep("auto", length(dvs)),
                                     byvar = "", format = "html") {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  if (byvar != "") if(!is.factor(df[,byvar])) stop("'byvar' needs to be a factor.")
  if ((length(dvs) > 1) & byvar != "") stop("you cannot subset multiple models in one table")
  if ((length(dvs) > 1) &&
      ((length(dvs) != length(idvs)) | (length(dvs) != length(feffects)) | (length(dvs) != length(clusters) | (length(dvs) != length(models)))))
    stop("'dvs', 'idvs', 'feffects', 'clusters' and 'models' need to be of equal lenghth.")
  datalist <- list()
  if (byvar != "") {
    datalist <- list(dvs = dvs,
                     idvs = idvs,
                     feffects = feffects,
                     clusters = clusters,
                     models = models)
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
    models[[1]]$byvalue <- "Full Sample"
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
                              clusters = clusters[[i]],
                              models = models[[i]])
      models <- lapply(datalist, function (x) estimate_model(df, x))
    } else {
      datalist <- list(dvs = dvs,
                       idvs = idvs,
                       feffects = feffects,
                       clusters = clusters,
                       models = models)
      models <- list(estimate_model(df, datalist))
    }
  }

  mt_str <- "Estimator"
  success_str <- "Probability estimated"
  fe_str <- "Fixed effects"
  cl_str <- "Std. errors clustered"
  n_str <- "Observations"
  r2_str <- "$R^{2}$"
  adjr2_str <- "Adjusted $R^{2}$"
  pr2_str <- "Pseudo $R^{2}$"

  m <- vector("list", length(models))
  mtype <- vector("list", length(models))
  ret <- vector("list", length(models))
  p <- vector("list", length(models))
  se <- vector("list", length(models))
  ols_present <- FALSE
  logit_present <- FALSE
  omit_vars <- NULL

  for (i in 1:length(models)) {
    m[[i]] <- models[[i]]$model
    if (i == 1) mtype <- models[[i]]$type_str
    else mtype <- c(mtype, models[[i]]$type_str)
    mt_str <- c(mt_str, mtype[[i]])

    if (models[[i]]$fe_str != "")  fe_str <- c(fe_str, models[[i]]$fe_str)
    else fe_str <- c(fe_str, "None")
    if (models[[i]]$cl_str != "")  cl_str <- c(cl_str, models[[i]]$cl_str)
    else cl_str <- c(cl_str, "No")

    n_str <- c(n_str, format(m[[i]]$N, big.mark = ","))

    if (models[[i]]$type_str == "ols") {
      ols_present <- TRUE
      r2_str <- c(r2_str, sprintf("%.3f", summary(m[[i]])$r.squared))
      adjr2_str <- c(adjr2_str, sprintf("%.3f", summary(m[[i]])$adj.r.squared))
      success_str <- c(success_str, "")
      pr2_str <- c(pr2_str, "")
    }

    if (models[[i]]$type_str == "logit") {
      logit_present <- TRUE
      pr2_str <- c(pr2_str, sprintf("%.3f", m[[i]]$pseudo_r2))
      success_str <- c(success_str, m[[i]]$success)
      r2_str <- c(r2_str, "")
      adjr2_str <- c(adjr2_str, "")
    }

    ret[[i]] <- models[[i]]
    if (!is.null(models[[i]]$se)) se[[i]] <- models[[i]]$se
    if (!is.null(models[[i]]$p)) p[[i]] <- models[[i]]$p
    if (byvar != "") {
      if (i == 1) labels <- models[[i]]$byvalue
      else labels <- c(labels, models[[i]]$byvalue)
    }
    if (!is.null(models[[i]]$omit_vars)) {
      if (is.null(omit_vars)) omit_vars <- models[[i]]$omit_vars
      else omit_vars <- c(omit_vars, models[[i]]$omit_vars)
    }
  }
  dvs <- escape_for_latex(dvs)
  fe_str <- escape_for_latex(fe_str)
  cl_str <- escape_for_latex(cl_str)

  # The following is not being used for the time being as I am unable to figure
  # out the logic that stargazer uses when multicolumn ist set to TRUE
  # dvs <- dvs[c(TRUE, (dvs[-length(dvs)] != dvs[-1]) | (mtype[-length(mtype)] != mtype[-1]))]

  if (ols_present & logit_present)
    add.lines <- list(mt_str, success_str, fe_str, cl_str, r2_str, adjr2_str, pr2_str)
  else if (logit_present)
    add.lines <- list(mt_str, success_str, fe_str, cl_str, pr2_str)
  else add.lines <- list(mt_str, fe_str, cl_str, n_str, r2_str, adjr2_str)

  if (byvar != "") {
    # Stargazer 5.2.2 seems to have a bug on how column.labels are treated in text/html
    # Escaping labels does not help and even sometimes introduces an error.
    labels <- gsub("&", "+", labels, fixed = TRUE)
    labels <- gsub("_", "\\_", labels, fixed = TRUE)
    labels <- gsub("$", "USD", labels, fixed = TRUE)
    htmlout <- utils::capture.output(stargazer::stargazer(m,
                                                   type=format,
                                                   column.labels = labels,
                                                   dep.var.labels = dvs,
                                                   model.names = FALSE,
                                                   omit.stat = "all",
                                                   omit = omit_vars,
                                                   se = se,
                                                   p = p,
                                                   add.lines = add.lines))
  } else htmlout <- utils::capture.output(stargazer::stargazer(m,
                                         type=format,
                                         dep.var.labels = dvs,
                                         model.names = FALSE,
                                         multicolumn = FALSE,
                                         omit.stat = "all",
                                         omit = omit_vars,
                                         se = se,
                                         p = p,
                                         add.lines = add.lines))

  list(models = ret, table = htmlout)
}
