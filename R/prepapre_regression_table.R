estimate_model <- function(df, dl) {
  dv <- dl$dvs
  idvs <- dl$idvs
  feffects <- dl$feffects
  clusters <- dl$clusters
  type_str <- dl$models
  fe_str <- paste(feffects, collapse = ", ")
  cl_str <- paste(clusters, collapse = ", ")
  if (type_str == "auto") {
    if(!(is.factor(df[,dv]) | is.logical(df[,dv]))) type_str <- "ols"
    else type_str <- "logit"
  }
  if (feffects[1] != "") {
    df[,feffects] <- lapply(
      as.data.frame(df[,feffects]), function (x) factor(x, ordered = FALSE)
    )
  }
  if (feffects[1] != "") {
    f <- stats::as.formula(
      paste(
        dv, "~", paste(idvs, collapse = " + "), " | ",
        paste(feffects, collapse = " + ")
      )
    )
  } else {
    f <- stats::as.formula(paste(dv, "~", paste(idvs, collapse = " + ")))
  }
  if (nlevels(as.factor(df[,dv])) > 2 & type_str == "logit") {
    stop("multinomial logit is not implemented. Sorry.")
  }
  if (type_str == "logit") {
    if(clusters[1] != "") model <- fixest::feglm(
      f, family = stats::binomial(link = "logit"), cluster = clusters, data = df
    )
    else model <- fixest::feglm(
      f, family = stats::binomial(link = "logit"), se = "standard", data = df
    )
    model$success <- levels(as.factor(df[, dv]))[2]
  } else {
    if(clusters[1] != "") model <- fixest::feols(f, cluster = clusters, data=df)
    else model <- fixest::feols(f, se = "standard", data=df)
  }
  list(
    model = model, type_str = type_str, fe_str = fe_str, cl_str = cl_str
  )
}


#' @title Prepares a Regression Table
#'
#' @description
#' Builds a regression table based on a set of user-specified models or a single
#' model and a partitioning variable.
#'
#' @param df Data frame containing the data to estimate the models on.
#' @param dvs A character vector containing the variable names for the dependent
#' variable(s).
#' @param idvs A character vector or a a list of character vectors containing
#' the variable names of the independent variables.
#' @param feffects A character vector or a a list of character vectors
#' containing the variable names of the fixed effects.
#' @param clusters A character vector or a a list of character vectors
#' containing the variable names of the cluster variables.
#' @param models A character vector indicating the model types to be estimated
#' ('ols', 'logit', or 'auto')
#' @param byvar A factorial variable to estimate the model on (only possible if
#' only one model is being estimated).
#' @param fmt A formatting function for numeric values in the table. Applied to
#' all coefficients, standard errors and goodness-of-fit statistics.
#' @param output A character scalar that is passed on
#' \code{\link[modelsummary]{modelsummary}}  to determine
#' the presentation format (e.g., "html", "text", or "latex"). "text" is
#' converted to "markdown". The old parameter \code{format} is now depreciated
#' and converted to \code{output} with a warning.
#' @param format Deprecated. See 'output'.
#'
#' @return A list containing two items
#' \describe{
#'  \item{"models"}{A list containing the model results and by values if
#'  appropriate}
#'  \item{"table"}{The output of \code{\link[modelsummary]{modelsummary}}
#'  containing the table}
#' }
#'
#' @details
#' This is a wrapper function calling \code{\link[modelsummary]{modelsummary}}.
#' Depending on whether the dependent variable is numeric, logical or a factor
#' with two levels, the models are estimated using \code{\link[fixest]{feols}}
#' (for numeric dependent variables) or \code{\link[fixest]{feglm}} (with
#' \code{family = binomial(link="logit")}) (for two-level factors or logical
#' variables). You can override this behavior by specifying the model with the
#' \code{models} parameter. Multinomial logit models are not supported.
#' If run with \code{byvar}, only levels that have more observations than
#' coefficients are estimated.
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

prepare_regression_table <- function(
  df, dvs, idvs, feffects = rep("", length(dvs)),
  clusters = rep("", length(dvs)), models = rep("auto", length(dvs)),
  byvar = "",
  fmt = function(x) {
    if (is.integer(x)) format(x, big.mark = ",")
    else format(round(x, 3), nsmall = 3, big.mark = ",")
  },
  output = "html", format = NULL
) {
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  if (byvar != "") if(!is.factor(df[,byvar])) stop(
    "'byvar' needs to be a factor."
  )
  if ((length(dvs) > 1) & byvar != "") stop(
    "you cannot subset multiple models in one table"
  )
  if ((length(dvs) > 1) &&
      ((length(dvs) != length(idvs)) | (length(dvs) != length(feffects)) |
       (length(dvs) != length(clusters) | (length(dvs) != length(models)))))
    stop(paste(
      "'dvs', 'idvs', 'feffects', 'clusters' and 'models'",
      "need to be of equal lenghth."
    ))
  if (!missing("format")){
    warning(paste(
      "Argument 'format' has been deprecated and replaced by 'output'.",
      "Please use this instead."
    ))
    output <- format
  }

  datalist <- list()
  if (byvar != "") {
    datalist <- list(
      dvs = dvs, idvs = idvs, feffects = feffects,
      clusters = clusters, models = models
    )
    vars <- c(byvar, dvs, idvs, feffects, clusters)
    vars <- vars[which(!vars %in% "")]
    df <- df[stats::complete.cases(df[, vars]), vars]
    bylevels <- unique(as.character(df[,byvar]))[order(unique(df[,byvar]))]
    n_by_level <- unlist(
      lapply(bylevels, function(x) nrow(df[df[,byvar] == x,]))
    )
    bylevels <- bylevels[n_by_level > length(idvs) + 1]
    if (length(bylevels) < 1) stop(
      "no by levels with sufficient degrees of freedom to estimate model"
    )
    mby <- lapply(
      bylevels, function(x) estimate_model(df[df[,byvar] == x,], datalist)
    )
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
        datalist[[i]] <- list(
          dvs = dvs[[i]], idvs = idvs[[i]], feffects = feffects[[i]],
          clusters = clusters[[i]], models = models[[i]]
        )
      models <- lapply(datalist, function (x) estimate_model(df, x))
    } else {
      datalist <- list(
        dvs = dvs, idvs = idvs, feffects = feffects,
        clusters = clusters, models = models
      )
      models <- list(estimate_model(df, datalist))
    }
  }

  gm <- list(
    list("raw" = "nobs", "clean" = "N", "fmt" = fmt),
    list("raw" = "r.squared", "clean" = "R2", "fmt" = fmt),
    list("raw" = "adj.r.squared", "clean" = "Adjusted R2", "fmt" = fmt),
    list("raw" = "pseudo.r.squared", "clean" = "Pseudo R2", "fmt" = fmt)
  )

  ar <- data.frame(
    Term = c(
      "Estimator", "Probability estimated", "Fixed effects",
      "Std. errors clustered"
    ),
    stringsAsFactors = FALSE
  )

  m <- vector("list", length(models))
  ols_present <- FALSE
  logit_present <- FALSE

  for (i in 1:length(models)) {
    m[[i]] <- models[[i]]$model

    ar[i + 1] <- ""
    ar[1, i + 1] <- models[[i]]$type_str
    ar[3, i + 1] <- ifelse(
      models[[i]]$fe_str != "", models[[i]]$fe_str, "None"
    )
    ar[4, i + 1] <- ifelse(
      models[[i]]$cl_str != "", models[[i]]$cl_str, "None"
    )

    if (models[[i]]$type_str == "ols") ols_present <- TRUE
    if (models[[i]]$type_str == "logit") {
      logit_present <- TRUE
      ar[2, i + 1] <- models[[i]]$model$success
    }

    if (byvar != "") {
      if (i == 1) labels <- models[[i]]$byvalue
      else labels <- c(labels, models[[i]]$byvalue)
    }
  }

  if (byvar != "") names(m) <- labels
  names(ar) <- c("Term", names(m))
  if (!ols_present) {
    gm <- gm[c(1,4)]
  }
  if (!logit_present) {
    gm <- gm[1:3]
    ar <- ar[c(1, 3:4),]
  }
  if (output == "text") output = "markdown"
  if (grepl("latex", output, fixed = TRUE)) {
    gm <- lapply(gm, function (x) {
      x$clean <- sub("R2", "$R^{2}$", x$clean, fixed = TRUE)
      x
    })
  }

  htmlout <- modelsummary::modelsummary(
    m, type = format, fmt = fmt, output = output,
    stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
    gof_map = gm,
    add_rows = ar
  )
  list(models = models, table = htmlout)
}
