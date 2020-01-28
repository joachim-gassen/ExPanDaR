# Utility functions of server()

check_vars <- function(cross_sectional = FALSE) {
  if (!cross_sectional)
    factor_names <- unique(c(lfactor$name, lcs_id$name, lts_id$name, llogical$name, "None"))
  else
    factor_names <- unique(c(lfactor$name, lcs_id$name, llogical$name, "None"))
  numeric_names <- c(lnumeric$name, llogical$name, "None")
  depvar_names <- unique(c(lnumeric$name, llogical$name, l2level$name))

  if (!uc$bar_chart_var1 %in% factor_names) uc$bar_chart_var1 = factor_names[1]
  if (!uc$bar_chart_var2 %in% factor_names) uc$bar_chart_var2 = "None"
  if (!uc$bgbg_var %in% numeric_names) uc$bgbg_var = numeric_names[1]
  if (!uc$bgbg_byvar %in% factor_names) uc$bgbg_byvar = factor_names[1]
  if (!uc$bgvg_var %in% numeric_names) uc$bgvg_var = numeric_names[1]
  if (!uc$bgvg_byvar %in% factor_names) uc$bgvg_byvar = factor_names[1]
  if (uc$bgbg_var == uc$bgbg_byvar) uc$bgbg_var = numeric_names[2]
  if (uc$bgvg_var == uc$bgvg_byvar) uc$bgvg_var = numeric_names[2]
  if (!uc$hist_var %in% numeric_names) uc$hist_var = numeric_names[1]
  if (!uc$ext_obs_var %in% numeric_names) uc$ext_obs_var = numeric_names[1]
  if (!uc$trend_graph_var1 %in% numeric_names) uc$trend_graph_var1 = numeric_names[1]
  if (!uc$trend_graph_var2 %in% numeric_names) uc$trend_graph_var2 = "None"
  if (!uc$trend_graph_var3 %in% numeric_names) uc$trend_graph_var3 = "None"
  if (!uc$quantile_trend_graph_var %in% numeric_names) uc$quantile_trend_graph_var = numeric_names[1]
  if (!uc$scatter_x %in% numeric_names) uc$scatter_x = numeric_names[1]
  if (!uc$scatter_y %in% numeric_names) uc$scatter_y = numeric_names[2]
  if (!uc$scatter_size %in% numeric_names) uc$scatter_size = "None"
  if (!uc$scatter_color %in% union(factor_names, numeric_names)) uc$scatter_color = "None"
  if (!uc$reg_y %in% depvar_names) uc$reg_y = numeric_names[1]
  uc$reg_x <- intersect(uc$reg_x, numeric_names)
  if (length(uc$reg_x) == 0) uc$reg_x = numeric_names[2]

  if (!uc$reg_fe1 %in% factor_names) uc$reg_fe1 = "None"
  if (!uc$reg_fe2 %in% factor_names) uc$reg_fe2 = "None"
  if (!uc$reg_by %in% factor_names) uc$reg_by = "None"
  if (!uc$model %in% c("ols", "logit")) uc$model = "ols"
  if (uc$cluster == 2 & uc$reg_fe1 == "None") uc$cluster = 1
  if (uc$cluster == 3 & uc$reg_fe2 == "None") uc$cluster = 1
  if (uc$cluster == 4 & uc$reg_fe1 != "None" & uc$reg_fe2 == "None") uc$cluster = 2
  if (uc$cluster == 4 & uc$reg_fe1 == "None" & uc$reg_fe2 != "None") uc$cluster = 3
  if (uc$cluster == 4 & uc$reg_fe1 == "None" & uc$reg_fe2 == "None") uc$cluster = 1
}

save_udv <- function(udv_name, udv_def, udv_vector) {
  udv <- cbind(udv_name, udv_def)
  if (is.numeric(udv_vector)) type <- "numeric"
  else if (is.logical(udv_vector)) type <- "logical"
  else type <- "factor"
  can_be_na <- TRUE
  new_def <- data.frame(var_name = udv_name, var_def = udv_def, type, can_be_na, stringsAsFactors = FALSE)
  if (shiny_long_def && server_side_data && any(base_variable$var_def != "")) {
    tokens <- getParseData(parse(text = udv_def, keep.source = TRUE))
    vars <- tokens$text[tokens$token == "SYMBOL"]
    if (length(vars) > 1) udv_defs <- c(udv_def, rep("", length(vars) - 1)) else udv_defs <- udv_def
    new_def <- paste(udv_defs, paste0(vars, ": ",
                                      bs_definition$var_def[match(vars, bs_definition$var_name)]),
                     collapse = "\n", sep = "\n")
    new_def <- data.frame(var_name = udv_name, var_def = new_def, type, can_be_na, stringsAsFactors = FALSE)
  }
  if (!is.null(uc$udvars)) {
    uc$udvars <<- rbind(uc$udvars, udv)
    udv_sample <<- cbind(udv_sample, udv_vector)
    udv_definition <<- rbind(udv_definition, new_def)
  }
  else {
    udv_sample <<- data.frame(udv_vector)
    uc$udvars <<- udv
    udv_definition <<- new_def
  }
  colnames(udv_sample)[ncol(udv_sample)] <<- udv_name
  dummy <- 0
}

mleadlag <- function(x, n, ts_id) {
  pos <- match(as.numeric(ts_id) + n, as.numeric(ts_id))
  x[pos]
}

lead <- function(x, n = 1L) {
  df <- cbind(create_base_sample()[,c(as.character(bs_definition[bs_definition$type == "cs_id" |
                                                                   bs_definition$type == "ts_id", "var_name"]))], x)
  colnames(df)[ncol(df)] <- "xval"
  colnames(df)[colnames(df) == as.character(bs_definition[bs_definition$type == "ts_id", "var_name"])] <- "ts_id"

  df %>% dplyr::group_by_at(vars(one_of(as.character(bs_definition[bs_definition$type == "cs_id", "var_name"])))) %>%
    dplyr::mutate(y = mleadlag(xval, n, ts_id)) %>%
    dplyr::ungroup() %>% dplyr::pull(y)
}

lag <- function(x, n = 1L) {
  df <- cbind(create_base_sample()[,c(as.character(bs_definition[bs_definition$type == "cs_id" |
                                                                   bs_definition$type == "ts_id", "var_name"]))], x)
  colnames(df)[ncol(df)] <- "xval"
  colnames(df)[colnames(df) == as.character(bs_definition[bs_definition$type == "ts_id", "var_name"])] <- "ts_id"

  df %>% dplyr::group_by_at(vars(dplyr::one_of(as.character(bs_definition[bs_definition$type == "cs_id", "var_name"])))) %>%
    dplyr::mutate(y = mleadlag(xval, -n, ts_id)) %>%
    dplyr::ungroup() %>% dplyr::pull(y)
}

test_udv_definition <- function(udv_definition) {
  # Prepare a sandbox environment that should be user code-safe
  myenv = new.env(parent=emptyenv())
  # Define names of R functions which are allowed for calculation
  allowedFunctions = c("(", "==", "&", "|", "+", "-", "*", "/", "<", ">", "!", "is.na", "^","exp", "log", "lag", "lead")
  # Assign the functions to the evaluation environment
  for(name in allowedFunctions){
    assign(name,match.fun(name), envir=myenv)
  }
  # Plus the variables contained in the analysis sample
  df <- create_ca_sample()
  for(name in names(df)){
    if (is.factor(df[,name])) assign(name, as.character(df[,name]), envir=myenv)
    else assign(name, df[,name], envir=myenv)
  }

  # Plus additional variables from base data frame if !simple_call_mode
  if (!simple_call_mode) {
    bs <- create_base_sample()
    new_names_bs <- names(bs)[which(!(names(bs) %in% names(df)))]
    for(name in new_names_bs){
      if (is.factor(bs[,name])) assign(name, as.character(bs[,name]), envir=myenv)
      else assign(name, bs[,name], envir=myenv)
    }
  }

  new_var <- try(eval(parse(text=udv_definition), envir=myenv), silent=TRUE)
  if (length(new_var) == length(df[,1])) return (new_var) else return (NULL)
}

create_udv_sample <- function() {
  if (DEBUG) tictoc::tic("create_udv_sample")
  udv_definition <<- NULL
  udv_sample <<- NULL
  apply(isolate(uc$udvars), 1, function(x) {
    new_var <- test_udv_definition(x[2])
    if (!is.null(new_var)) {
      if (!is.null(udv_sample)) udv_sample <<- cbind(udv_sample, new_var)
      else udv_sample <<- data.frame(new_var)
      colnames(udv_sample)[ncol(udv_sample)] <<- x[1]
      if (is.numeric(udv_sample[,ncol(udv_sample)])) type <- "numeric"
      else if (is.logical(udv_sample[,ncol(udv_sample)])) type <- "logical"
      else type <- "factor"
      new_def <- cbind(x[1], x[2], type, 1)
      if (shiny_long_def && server_side_data && any(base_variable$var_def != "")) {
        tokens <- getParseData(parse(text = x[2], keep.source = TRUE))
        vars <- tokens$text[tokens$token == "SYMBOL"]
        if (length(vars) > 1) var_defs <- c(x[2], rep("", length(vars) - 1)) else var_defs <- x[2]
        var_def <- paste(var_defs, paste0(vars, ": ",
                                          bs_definition$var_def[match(vars, bs_definition$var_name)]),
                         collapse = "\n", sep = "\n")
        new_def <- cbind(x[1], var_def, type, 1)
      }
      if (is.null(udv_definition)) udv_definition <<- data.frame(new_def, stringsAsFactors = FALSE)
      else udv_definition <<- rbind(udv_definition, new_def)
    }
  })
  if (!is.null(udv_definition)) colnames(udv_definition) <<- c("var_name", "var_def", "type", "can_be_na")
  if (DEBUG) message(do.call(tictoc::toc.outmsg, tictoc::toc(quiet = TRUE)))
}

get_suitable_vars <- function(t, s, v) {
  if(t == "factor") {
    return(
      which(
        (v$type == "factor" |
           sapply(s, function (x) length(unique(na.omit(x))) <= factor_cutoff)) &
          sapply(s, function (x) length(unique(na.omit(x))) > 1)
      )
    )
  } else if (t == "2level") {
    return(which(sapply(s, function (x) length(unique(na.omit(x))) == 2)))
  } else return(which(v$type == t))
}

create_var_categories <- function(s, v) {
  for (type in c("cs_id", "ts_id", "numeric", "logical", "factor", "2level")) {
    cand <- get_suitable_vars(type, s, v)
    assign(paste0("l", type), data.frame(
      col = cand,
      name = v$var_name[cand],
      stringsAsFactors = FALSE
    ), envir = parent.env(parent.env(environment())))
  }
}

parse_config <- function(l) {
  if (!is.null(l)) {
    for (name in c("sample", names(default_config))) {
      if (name %in% names(l)) uc[[name]] <<- l[[name]]
      else uc[[name]] <<- default_config[[name]]
    }
    if (length(isolate(uc$udvars)) != 0) create_udv_sample()
    uc$config_parsed <<- TRUE
  }
}

reset_config <- function() {
  for (name in names(default_config)) {
    uc[[name]] <<- NULL
  }
  uc$sample <<- NULL
  uc$config_parsed <<- NULL
}
