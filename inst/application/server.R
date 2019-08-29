library(ExPanDaR)
library(openssl)
library(dplyr)

options(shiny.maxRequestSize = 1024^3)

load("shiny_data.Rda")
factor_cutoff <- shiny_factor_cutoff

key <- openssl::sha256(charToRaw(shiny_key_phrase))

DEBUG <- shiny_debug
if (DEBUG) sample_count <<- 0

bs_definition <- NULL
cas_definition <- NULL
correl_n <- NULL
correl_p <- NULL
correl_r <- NULL
sample_definition <- NULL
udv_definition <- NULL
udv_sample <- NULL

server_side_data <- !is.null(shiny_df)
simple_call_mode <- server_side_data & is.null(shiny_var_def)

default_config <- list(
  subset_factor = "Full Sample",
  subset_value = "All",
  group_factor = "None",
  balanced_panel = FALSE,
  outlier_treatment = "1",
  outlier_factor = "None",
  udvars = NULL,
  delvars = NULL,
  bar_chart_var1 = "None",
  bar_chart_var2 = "None",
  bar_chart_group_by = "All",
  bar_chart_relative = FALSE,
  missing_values_group_by = "All",
  desc_group_by = "All",
  bgbg_var = "None",
  bgbg_byvar = "None",
  bgbg_stat = "mean",
  bgbg_sort_by_stat = FALSE,
  bgbg_group_by = "All",
  bgvg_var = "None",
  bgvg_byvar = "None",
  bgvg_sort_by_stat = FALSE,
  bgvg_group_by = "All",
  hist_var = "None",
  hist_group_by = "All",
  hist_nr_of_breaks = 20,
  ext_obs_var = "None",
  ext_obs_group_by = "All",
  ext_obs_period_by = "All",
  trend_graph_var1 = "None",
  trend_graph_var2 = "None",
  trend_graph_var3 = "None",
  trend_graph_group_by = "All",
  quantile_trend_graph_var = "None",
  quantile_trend_graph_quantiles = c("0.05", "0.25", "0.50", "0.75", "0.95"),
  quantile_trend_graph_group_by = "All",
  corrplot_group_by = "All",
  scatter_x = "None",
  scatter_y = "None",
  scatter_size = "None",
  scatter_color = "None",
  scatter_group_by = "All",
  scatter_loess = FALSE,
  scatter_sample = TRUE,
  reg_y = "None",
  reg_x = "None",
  reg_fe1 = "None",
  reg_fe2 = "None",
  reg_by = "None",
  cluster = 1,
  model = "ols"
)

quote_escape <- function(string) {
  t <- gsub("\"", "&#34;", string)
  t <- gsub("\'", "&#39;", t)
  t <- gsub("\n", "&#10;", t)
  t
}

select_factor <- function(df, max_cases = factor_cutoff) {
  df <- as.data.frame(df)
  no_cases <- sapply(df, function(x) length(unique(x)))
  if (length(df[no_cases <= max_cases]) > 0)
    return (names(df[no_cases <= max_cases])[1])
  else return(names(df[no_cases == min(no_cases)])[1])
}


load_sample <- function(df, id, description) {
  if (DEBUG) message("loading user data")
  ds <- data.frame(ds_id = id,
                   ds_description = description,
                   stringsAsFactors = FALSE)
  s <- data.frame(ds_id = id, df)
  v <- data.frame(
    ds_id = id,
    var_name = names(s)[2:length(s)],
    var_def = "",
    stringsAsFactors = FALSE
  )
  v$type <- "factor"
  v$type[which(sapply(df, is.logical))] <- "logical"
  v$type[which(sapply(df, is.numeric))] <- "numeric"
  return(list(ds, s, v))
}


check_ids <- function(s, cs_id, ts_id) {
  keys <- c("ds_id", cs_id, ts_id)
  return(!anyDuplicated(s[,keys]))
}


add_ids <- function(v, ds_id, cs_id, ts_id) {
  v$type[which(v$ds_id == ds_id & v$var_name %in% cs_id)] <- "cs_id"
  v$type[which(v$ds_id == ds_id & v$var_name == ts_id)] <- "ts_id"
  return(v)
}


create_config <- function(s, v, ds_id) {
  c <- list(
    sample = ds_id,
    subset_factor = "Full Sample",
    subset_value = "All",
    group_factor = "None",
    balanced_panel = FALSE,
    outlier_treatment = "1",
    outlier_factor = "None",
    udvars = NULL,
    delvars = NULL,
    bar_chart_var1 = v$var_name[v$ds_id == ds_id & v$type == "ts_id"],
    bar_chart_var2 = select_factor(s[s$ds_id == ds_id, v$var_name[v$ds_id == ds_id & v$type != "cs_id" & v$type != "ts_id"], drop = FALSE]),
    bar_chart_group_by = "All",
    bar_chart_relative = FALSE,
    missing_values_group_by = "All",
    desc_group_by = "All",
    bgbg_var = v$var_name[v$ds_id == ds_id & v$type == "numeric"][1],
    bgbg_byvar = select_factor(s[s$ds_id == ds_id, v$var_name[v$ds_id == ds_id & v$type != "cs_id" & v$type != "ts_id"], drop = FALSE]),
    bgbg_stat = "mean",
    bgbg_sort_by_stat = TRUE,
    bgbg_group_by = "All",
    bgvg_var = v$var_name[v$ds_id == ds_id & v$type == "numeric"][1],
    bgvg_byvar = select_factor(s[s$ds_id == ds_id, v$var_name[v$ds_id == ds_id & v$type != "cs_id" & v$type != "ts_id"], drop = FALSE]),
    bgvg_sort_by_stat = TRUE,
    bgvg_group_by = "All",
    hist_var = v$var_name[v$ds_id == ds_id & v$type == "numeric"][1],
    hist_group_by = "All",
    hist_nr_of_breaks = 20,
    ext_obs_var = v$var_name[v$ds_id == ds_id & v$type == "numeric"][1],
    ext_obs_group_by = "All",
    ext_obs_period_by = "All",
    trend_graph_var1 = v$var_name[v$ds_id == ds_id & v$type == "numeric"][1],
    trend_graph_var2 = "None",
    trend_graph_var3 = "None",
    trend_graph_group_by = "All",
    quantile_trend_graph_var = v$var_name[v$ds_id == ds_id & v$type == "numeric"][1],
    quantile_trend_graph_quantiles = c("0.05", "0.25", "0.50", "0.75", "0.95"),
    quantile_trend_graph_group_by = "All",
    corrplot_group_by = "All",
    scatter_x = v$var_name[v$ds_id == ds_id & v$type == "numeric"][1],
    scatter_y = v$var_name[v$ds_id == ds_id & v$type == "numeric"][2],
    scatter_size = v$var_name[v$ds_id == ds_id & v$type == "numeric"][3],
    scatter_color = select_factor(s[s$ds_id == ds_id, v$var_name[v$ds_id == ds_id & (v$type != "cs_id" | v$type != "ts_id")], drop = FALSE]),
    scatter_group_by = "All",
    scatter_loess = TRUE,
    scatter_sample = TRUE,
    reg_y = v$var_name[v$ds_id == ds_id & v$type == "numeric"][2],
    reg_x = v$var_name[v$ds_id == ds_id & v$type == "numeric"][1],
    reg_fe1 = "None",
    reg_fe2 = "None",
    reg_by = "None",
    cluster = 1,
    model = "ols"
  )
  return(c)
}


# Define the server for the Shiny app
function(input, output, session) {
  uc <- reactiveValues()
  app_config <- NULL
  data_source <- NULL
  ca_sample <- NULL
  ca_variable <- NULL
  base_data <- NULL
  base_variable <- NULL

  check_whether_data_is_valid <- function(v) {
    if (length(which(v$type == "numeric")) < 2) {
      if (DEBUG) warning("Less than two numerical variables in data")
      session$sendCustomMessage(type = 'testmessage',
                                message = paste0('Your data contains less than two numerical variables. At least two are required.'))
      return(FALSE)
    }
    return(TRUE)
  }


  check_vars <- function() {
    factor_names <- unique(c(lfactor$name, lcs_id$name, lts_id$name, llogical$name, "None"))
    numeric_names <- c(lnumeric$name, llogical$name, "None")
    depvar_names <- unique(c(lnumeric$name, llogical$name, l2level$name))
    if (!uc$bar_chart_var1 %in% factor_names) uc$bar_chart_var1 = factor_names[1]
    if (!uc$bar_chart_var2 %in% factor_names) uc$bar_chart_var2 = "None"
    if (!uc$bgbg_var %in% numeric_names) uc$bgbg_var = numeric_names[1]
    if (!uc$bgbg_byvar %in% factor_names) uc$bgbg_byvar = factor_names[1]
    if (!uc$bgvg_var %in% numeric_names) uc$bgvg_var = numeric_names[1]
    if (!uc$bgvg_byvar %in% factor_names) uc$bgvg_byvar = factor_names[1]
    if (!uc$hist_var %in% numeric_names) uc$hist_var = numeric_names[1]
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


  if (server_side_data) {
    if (is.data.frame(shiny_df)) {
      shiny_df <- list(shiny_df)
      if (!is.null(shiny_df_def)) shiny_df_def <- list(shiny_df_def)
    }
    for (i in 1:length(shiny_df)) {
      ret <- load_sample(shiny_df[[i]], shiny_df_id[i], shiny_df_name[i])

      if (!is.null(shiny_df_def[[i]])) {
        ret[[3]] <- data.frame(ds_id = shiny_df_id[i],
                               shiny_df_def[[i]],
                               stringsAsFactors = FALSE)
      }
      if (i == 1) {
        ds <- ret[[1]]
        s <- ret[[2]]
        v <- ret[[3]]
      } else {
        ds <- rbind(ds, ret[[1]])
        s <- rbind(s, ret[[2]])
        v <- rbind(v, ret[[3]])
      }
      if (is.null(shiny_df_def)) {
        for (i in 1:length(shiny_df)) {
          v <- add_ids(v, shiny_df_id[i], shiny_cs_id, shiny_ts_id)
        }
      }
    }
    if(!"can_be_na" %in% names(v)) v$can_be_na <- ifelse(v$type == "cs_id" | v$type == "ts_id", FALSE, TRUE)

    data_source <- ds
    cs_id <- unique(v$var_name[v$type == "cs_id"])
    ts_id <- unique(v$var_name[v$type == "ts_id"])

    if (simple_call_mode) {
      ca_sample <- s
      ca_variable <- v
    } else {
      order_cols <- c("ds_id", cs_id, ts_id)
      base_data <- s %>% arrange_(.dots = order_cols)
      base_variable <- v

      code <- paste0("base_data %>% group_by(ds_id, ",
                     paste(cs_id, collapse=", "),
                     ") %>%")

      vars_to_assign <- shiny_var_def[shiny_var_def$var_name != shiny_var_def$var_def,]
      if (length(vars_to_assign$var_name) > 0) {
        assignments <- paste0(vars_to_assign$var_name, " = ", vars_to_assign$var_def, ",")
        assignments[length(assignments)] <- substr(assignments[length(assignments)], 1, nchar(assignments[length(assignments)])-1)
        code <- c(code, "mutate(", assignments, ") %>%")
      }

      code <- c(code, paste0("select(ds_id, ", paste(shiny_var_def$var_name, collapse = ", "), ") -> samples"))
      eval(parse(text = code))

      keys <- c("ds_id", cs_id, ts_id)
      if (anyDuplicated(samples[,keys])) stop("prepare_samples: Found duplicate data in panel sample. Fix that")

      ca_sample <- as.data.frame(samples)
      ca_variable <- data.frame(ds_id = rep(data_source$ds_id, each = nrow(shiny_var_def)),
                                shiny_var_def)
      if (!"can_be_na" %in% names(ca_variable)) ca_variable$can_be_na <-
        ifelse(ca_variable$type == "cs_id" | ca_variable$type == "ts_id", FALSE, TRUE)
      if (shiny_long_def && any(base_variable$var_def != "")) {
        for (i in 1:nrow(ca_variable)) {
          tokens <- utils::getParseData(parse(text = ca_variable$var_def[i], keep.source = TRUE))
          vars <- tokens$text[tokens$token == "SYMBOL"]
          if (length(vars) > 1) var_defs <- c(ca_variable$var_def[i], rep("", length(vars) - 1)) else var_defs <- ca_variable$var_def[i]
          ca_variable$var_def[i] <- paste(var_defs,
                                          paste0(vars, ": ",
                                                 base_variable$var_def[match(vars, base_variable$var_name)]),
                                          collapse = "\n", sep = "\n")
        }
      }
    }
    base_config <- create_config(ca_sample, ca_variable, ca_variable$ds_id[1])
    if (!is.null(shiny_config_list)) {
      for (name in names(base_config)) {
        if (name %in% names(shiny_config_list)) base_config[[name]] <- shiny_config_list[[name]]
      }
    }
    app_config <- base_config
  }

  create_base_sample <- reactive({
    req(uc$subset_factor)
    bsd <- data.frame(base_variable,
                      can_be_na = TRUE)
    bs <- base_data[base_data$ds_id == uc$sample, as.character(bsd$var_name)]

    all_na_vars <- sapply(bs, function (x) all(is.na(x)))
    bs_definition <<- bsd[!all_na_vars,]
    return(bs[,as.character(bsd$var_name)])
  })

  create_ca_sample <- reactive({
    req(uc$subset_factor)
    cas_definition <<- ca_variable[ca_variable$ds_id == uc$sample, -1]
    smp <- ca_sample[ca_sample$ds_id == uc$sample, as.character(cas_definition$var_name)]
    smp[, cas_definition$var_name[cas_definition$type == "ts_id"]] <-
      as.ordered(smp[, cas_definition$var_name[cas_definition$type == "ts_id"]])
    return(smp)
  })

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

  parse_config <- function(l) {
    if (!is.null(l)) {
      for (name in c("sample", names(default_config))) {
        if (name %in% names(l)) uc[[name]] <<- l[[name]]
        else uc[[name]] <<- default_config[[name]]
      }
      if (length(isolate(uc$udvars)) != 0) create_udv_sample()
    }
  }

  observe(parse_config(app_config))

  get_suitable_vars <- function(t, s, v) {
    if(t == "factor") {
      return(which(v$type == "factor" | sapply(s, function (x) length(unique(na.omit(x))) <= factor_cutoff)))
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

  create_analysis_sample <- reactive({
    req(uc$subset_factor)
    if (DEBUG) sample_count <<- sample_count + 1
    if (DEBUG) tictoc::tic("create_analysis_sample")

    # Create core analyis and user defined samples and merge them

    smp <- create_ca_sample()
    sample_definition <<- cas_definition
    if (length(uc$udvars) != 0) {
      smp <- cbind(smp, udv_sample)
      sample_definition <<- rbind(sample_definition, udv_definition)
    }

    # Drop variables that user selected to delete
    sample_definition <<- sample_definition[! sample_definition$var_name %in% uc$delvars,]
    smp <- smp[, as.character(sample_definition$var_name)]

    # Drop numeric variables that are all NA
    smp[,sample_definition$var_name[sample_definition$type == "numeric"]] <-
      lapply(smp[,sample_definition$var_name[sample_definition$type == "numeric"]], function(x) ifelse(is.finite(x), x, NA))
    all_na_vars <- sapply(smp, function (x) all(is.na(x)))
    sample_definition <<- sample_definition[!all_na_vars,]
    smp <- smp[,as.character(sample_definition$var_name)]

    # Drop observations that are NA in variables that are not allowed to
    smp <- smp[complete.cases(smp[,as.character(sample_definition$var_name[which(sample_definition$can_be_na == FALSE)])]),]

    # Subset if requested by user
    if ((isolate(uc$subset_factor) != "Full Sample") & (uc$subset_value != "All"))
      smp <- smp[which(smp[,isolate(uc$subset_factor)] == uc$subset_value),]

    # Balance sample if requested by user
    if (uc$balanced_panel) {
      smp <- dplyr::group_by_at(smp, dplyr::vars(dplyr::one_of(sample_definition$var_name[sample_definition$type  == "cs_id"]))) %>%
        dplyr::mutate(nobs = n())
      max_nobs <- length(levels(as.data.frame(smp[, sample_definition$var_name[sample_definition$type  == "ts_id"]])[,1]))
      bal_smp <- as.data.frame(dplyr::select(dplyr::filter(smp, nobs == max_nobs), -nobs))
      if (nrow(bal_smp) > 0) smp <- as.data.frame(bal_smp)
      else {
        uc$balanced_panel <<- FALSE
        session$sendCustomMessage(type = 'testmessage',
                                  message = paste("Balancing panel yields empty sample. Deselecting option."))

      }
    }

    # Outlier treatment as requested by user
    nums <- sample_definition$var_name[sample_definition$type == "numeric"]
    if (uc$outlier_factor == "None") group <- NULL
    else group = smp[,uc$outlier_factor]

    if (uc$outlier_treatment == 2) smp[,nums] <- treat_outliers(smp[,nums], 0.01, FALSE, group)
    if (uc$outlier_treatment == 3) smp[,nums] <- treat_outliers(smp[,nums], 0.05, FALSE, group)
    if (uc$outlier_treatment == 4) smp[,nums] <- treat_outliers(smp[,nums], 0.01, TRUE, group)
    if (uc$outlier_treatment == 5) smp[,nums] <- treat_outliers(smp[,nums], 0.05, TRUE, group)

    # Verify that new sample does not violate any variable assignments in app
    smp <- droplevels(smp)
    create_var_categories(smp, sample_definition)
    isolate(check_vars())

    if (DEBUG) current_as <<- smp
    if (DEBUG) current_sd <<- sample_definition

    return(smp)
  })

  observeEvent(input$udv_submit, {
    varname <- make.names(input$udv_name)
    if (!varname %in% c(sample_definition$var_name, "None")) {
      udv_vector <- test_udv_definition(input$udv_definition)
      if (!is.null(udv_vector)) {
        save_udv(varname, input$udv_definition, udv_vector)
        session$sendCustomMessage(type = 'testmessage',
                                  message = paste("Variable", varname,
                                                  "successfully created."))
        updateTextInput(session, "udv_name", value = "")
        updateTextInput(session, "udv_definition", value = "")
      } else session$sendCustomMessage(type = 'testmessage',
                                       message = paste0('Definition "', input$udv_definition,
                                                        '" is invalid! Sorry.'))
    } else session$sendCustomMessage(type = 'testmessage',
                                     message = paste("Variable name", varname,
                                                     "is already taken or invalid! Sorry."))
  })

  observeEvent(input$delete_vars, {
    if (!is.null(input$descriptive_table_analysis_rows_selected)) {
      var_names = c(lnumeric$name, llogical$name)
      dvar_names <- var_names[input$descriptive_table_analysis_rows_selected]
      var_names <- var_names[!var_names %in% dvar_names]
      if (length(var_names) > 1) {
        if (length(uc$udvars) != 0) {
          to_delete  <- intersect(dvar_names, uc$udvars[,1])
          if (length(to_delete) > 0) {
            uc$udvars <- uc$udvars[!(uc$udvars[,1] %in% to_delete), , drop = FALSE]
            dvar_names <- dvar_names[!dvar_names %in% to_delete]
            udv_sample <<- udv_sample[,!(names(udv_sample) %in% to_delete), drop = FALSE]
            udv_definition <<- udv_definition[!(udv_definition$var_name %in% to_delete), , drop = FALSE]
          }
        }
        if (length(dvar_names) != 0) {
          if (is.null(uc$delvars)) uc$delvars <- dvar_names
          else uc$delvars <- c(uc$delvars, dvar_names)
        }
      } else session$sendCustomMessage(type = 'testmessage',
                                       message = paste("At least two variables are required! Sorry."))
    }
  })

  observeEvent(input$infile, {
    input_file <- input$infile
    if (is.null(input_file)) return(NULL)

    input_file_format <- tools::file_ext(input_file$name)
    shiny_df <- try(rio::import(file = input_file$datapath,
                                format = input_file_format))
    if (class(shiny_df) == "try-error") {
      if (input_file_format == "dta") {
        warning("rio::import failed. Trying with encoding = 'latin1'")
        shiny_df <- try(haven::read_dta(file = input_file$datapath,
                                        encoding = 'latin1'))
        if (class(shiny_df) == "try-error") {
          warning("This also did not work out. Informing user.")
          session$sendCustomMessage(type = 'testmessage',
                                    message = sprintf("Unable to parse STATA file %s. Consider trying a different format.", input_file$name))
          return(NULL)
        }
      } else {
        warning("Unknown parsing problem. Informing user.")
        session$sendCustomMessage(type = 'testmessage',
                                  message = sprintf("Unable to parse file %s. Consider trying a different format.", input_file$name))
        return(NULL)
      }
    }
    if (!is.data.frame(shiny_df) || nrow(shiny_df) < 1) {
      warning("rio::import generated object that contains no data frame with observations. Informing user.")
      session$sendCustomMessage(type = 'testmessage',
                                message = sprintf("File %s does not contain data frame.", input_file$name))
      return(NULL)
    }

    shiny_df_id <- input_file$name

    ret <- load_sample(shiny_df, shiny_df_id, "User uploaded data")

    data_source <<- ret[[1]]
    ca_sample <<- ret[[2]]
    ca_variable <<- ret[[3]]

    uc$sample <<- ca_sample$ds_id[1]
    uc$subset_factor <<- NULL
  })

  observeEvent({c(input$ts_id, input$cs_id)}, {
    req(input$cs_id, input$ts_id)
    if (check_ids(ca_sample, input$cs_id, input$ts_id)) {
      if (length(ca_variable$var_name[ca_variable$type == "cs_id"]) > 0) {
        df <- ca_sample[,which(ca_variable$type %in% c("cs_id", "ts_id")) + 1]
        for (i in 1:ncol(df)) {
          if(is.numeric(df[,i])) ca_variable$type[ca_variable$var_name == colnames(df[i])] <<- "numeric"
          else if(is.logical(df[,i])) ca_variable$type[ca_variable$var_name == colnames(df[i])] <<- "logical"
          else ca_variable$type[ca_variable$var_name == colnames(df[i])] <<- "factor"
        }
      }

      cs_id <- input$cs_id
      ts_id <- input$ts_id
      ca_variable <<- add_ids(ca_variable, ca_variable$ds_id[1], cs_id, ts_id)
      if (check_whether_data_is_valid(ca_variable)) {
        order_cols <- c("ds_id", cs_id, ts_id)
        ca_sample <<- as.data.frame(ca_sample %>% arrange_(.dots = order_cols))
        base_data <<- ca_sample
        base_variable <<- ca_variable
        ca_variable$var_def <<- ca_variable$var_name
        ca_variable$can_be_na <<-
          ifelse(ca_variable$type == "cs_id" | ca_variable$type == "ts_id", FALSE, TRUE)

        app_config <<- create_config(ca_sample, ca_variable, ca_variable$ds_id[1])
        # force invalidation... let's see whether this is sufficient. Looks like it.
        temp <<- uc$sample
        uc$sample <<- NULL
        uc$sample <<- temp
        parse_config(app_config)
      } else {
        uc$sample <<- NULL
        ca_sample <<- NULL
        ca_variable <<- NULL
        data_source <<- NULL
      }
    } else session$sendCustomMessage(type = 'testmessage',
                                     message = paste("The variables you selected yield duplicate observations. Choose different variables and/or check your sample."))
  })

  observeEvent(input$restore_analysis_sample, {
    uc$udvars <<- NULL
    uc$delvars <<- NULL
  })

  observeEvent(input$sample, {
    req(uc$subset_factor)
    if (req(input$sample) != uc$sample) {
      uc$sample <<- input$sample
      uc$subset_value <<- "All"
      uc$desc_group_by <<- "All"
      uc$bgbg_group_by <<- "All"
      uc$bgvg_group_by <<- "All"
      uc$hist_group_by <<- "All"
      uc$ext_obs_group_by <<- "All"
      uc$trend_graph_group_by <<- "All"
      uc$quantile_trend_graph_group_by <<- "All"
      uc$corrplot_group_by <<- "All"
      uc$scatter_group_by <<- "All"
      if (length(uc$udvars) > 0) create_udv_sample()
    }
    df <- create_analysis_sample()
    if (length(c(lnumeric$name, llogical$name)) < 2) {
      session$sendCustomMessage(type = 'testmessage',
                                message = paste("At least two variables are required! Resetting sample. Sorry."))
      uc$udvars <<- NULL
      uc$delvars <<- NULL
    }
  }, priority = 1)

  observeEvent(input$subset_factor, {
    if (req(input$subset_factor) != uc$subset_factor) {
      uc$subset_factor <<- req(input$subset_factor)
      uc$subset_value <<- "All"
    }
  })

  observeEvent(input$group_factor, {
    if (req(input$group_factor) != uc$group_factor) {
      uc$group_factor <<- input$group_factor
      uc$desc_group_by <<- "All"
      uc$bgbg_group_by <<- "All"
      uc$bgvg_group_by <<- "All"
      uc$hist_group_by <<- "All"
      uc$ext_obs_group_by <<- "All"
      uc$trend_graph_group_by <<- "All"
      uc$quantile_trend_graph_group_by <<- "All"
      uc$corrplot_group_by <<- "All"
      uc$scatter_group_by <<- "All"
    }
  })

  observe({uc$subset_value <<- req(input$subset_value)})
  observe({uc$outlier_treatment <<- req(input$outlier_treatment)})
  observe({uc$outlier_factor <<- req(input$outlier_factor)})
  observe({if (is.logical(input$balanced_panel)) uc$balanced_panel <<- input$balanced_panel})
  observe({uc$bar_chart_var1 <<- req(input$bar_chart_var1)})
  observe({uc$bar_chart_var2 <<- req(input$bar_chart_var2)})
  observe({uc$bar_chart_group_by <<- req(input$bar_chart_group_by)})
  observe({uc$missing_values_group_by <<- req(input$missing_values_group_by)})
  observe({if (is.logical(input$bar_chart_relative)) uc$bar_chart_relative <<- input$bar_chart_relative})
  observe({uc$desc_group_by <<- req(input$desc_group_by)})
  observe({uc$bgbg_var <<- req(input$bgbg_var)})
  observe({uc$bgbg_byvar <<- req(input$bgbg_byvar)})
  observe({uc$bgbg_stat <<- req(input$bgbg_stat)})
  observe({if (is.logical(input$bgbg_sort_by_stat)) uc$bgbg_sort_by_stat <<- input$bgbg_sort_by_stat})
  observe({uc$bgbg_group_by <<- req(input$bgbg_group_by)})
  observe({uc$bgvg_var <<- req(input$bgvg_var)})
  observe({uc$bgvg_byvar <<- req(input$bgvg_byvar)})
  observe({if (is.logical(input$bgvg_sort_by_stat)) uc$bgvg_sort_by_stat <<- input$bgvg_sort_by_stat})
  observe({uc$bgvg_group_by <<- req(input$bgvg_group_by)})
  observe({uc$hist_var <<- req(input$hist_var)})
  observe({uc$hist_group_by <<- req(input$hist_group_by)})
  observe({uc$hist_nr_of_breaks <<- req(input$hist_nr_of_breaks)})
  observe({uc$ext_obs_var <<- req(input$ext_obs_var)})
  observe({uc$ext_obs_group_by <<- req(input$ext_obs_group_by)})
  observe({uc$ext_obs_period_by <<- req(input$ext_obs_period_by)})
  observe({uc$trend_graph_var1 <<- req(input$trend_graph_var1)})
  observe({uc$trend_graph_var2 <<- req(input$trend_graph_var2)})
  observe({uc$trend_graph_var3 <<- req(input$trend_graph_var3)})
  observe({uc$trend_graph_group_by <<- req(input$trend_graph_group_by)})
  observe({uc$quantile_trend_graph_var <<- req(input$quantile_trend_graph_var)})
  observe({uc$quantile_trend_graph_quantiles <<- req(input$quantile_trend_graph_quantiles)})
  observe({uc$quantile_trend_graph_group_by <<- req(input$quantile_trend_graph_group_by)})
  observe({uc$corrplot_group_by <<- req(input$corrplot_group_by)})
  observe({uc$scatter_x <<- req(input$scatter_x)})
  observe({uc$scatter_y <<- req(input$scatter_y)})
  observe({uc$scatter_size <<- req(input$scatter_size)})
  observe({uc$scatter_color <<- req(input$scatter_color)})
  observe({uc$scatter_group_by <<- req(input$scatter_group_by)})
  observe({if (is.logical(input$scatter_loess)) uc$scatter_loess <<- input$scatter_loess})
  observe({if (is.logical(input$scatter_sample)) uc$scatter_sample <<- input$scatter_sample})
  observe({uc$reg_y <<- req(input$reg_y)})
  observe({uc$reg_x <<- req(input$reg_x)})
  observe({uc$reg_fe1 <<-req(input$reg_fe1)})
  observe({uc$reg_fe2 <<- req(input$reg_fe2)})
  observe({uc$reg_by <<- req(input$reg_by)})
  observe({uc$cluster <<- req(input$cluster)})
  observe({uc$model <<- req(input$model)})

  for (i in 1:30) output[[paste0("ui_separator", i)]] <- renderUI({
    req(uc$subset_factor)
    hr()
  })

  output$ui_sample <- renderUI({
    if (!server_side_data) {
      fileInput('infile', label = NULL)
    } else {
      req(uc$sample)
      choices <- setNames(as.list(data_source$ds_id), data_source$ds_description)
      tagList(selectInput("sample", "Sample Selection",
                          choices,
                          selected = uc$sample),
              helpText("Select the sample that you want to study.")
      )
    }
  })

  output$ui_select_ids <- renderUI({
    req(uc$sample)
    tagList(selectInput("cs_id", "Select cross sectional identifier(s)",
                        ca_variable$var_name, multiple = TRUE,
                        selected = {if(exists("ca_variable")) ca_variable$var_name[ca_variable$type == "cs_id"] else NULL}),
            helpText("Select the variable that identifies a cross-sectional unit. Selecting multiple values is possible."),
            selectInput("ts_id", "Select time series identifier",
                        c("", ca_variable$var_name),
                          selected = {if(exists("ca_variable")) ca_variable$var_name[ca_variable$type == "ts_id"] else NULL}),
            helpText("Select the variable that identifies a the time series. Variable needs to be coercible into an ordered factor."))
  })

  output$ui_subset_factor <- renderUI({
    req (uc$subset_factor)
    df <- create_analysis_sample()
    tagList(selectInput("subset_factor", label = "Subset factor",
                        c("Full Sample",  unique(c(lfactor$name, llogical$name))),
                          selected = isolate(uc$subset_factor)),
            helpText("Indicate whether you want to study the full sample or subset to a specific factor."))

  })

  output$ui_subset_value <- renderUI({
    req (uc$subset_factor)
    df <- create_analysis_sample()
    if (uc$subset_factor != "Full Sample") {
      tagList(selectInput("subset_value", label = "Select subsample",
                          c("All",  sort(levels(as.factor(df[,isolate(uc$subset_factor)])))),
                          selected = isolate(uc$subset_value)),
              helpText("Identify the group that you want to limit your sample to."))
    }
  })

  output$ui_group_factor <- renderUI({
    req(uc$group_factor)
    df <- create_analysis_sample()
    tagList(selectInput("group_factor", label = "Group factor",
                        c("None",  unique(c(lcs_id$name, lts_id$name, lfactor$name, llogical$name))),
                        selected = isolate(uc$group_factor)),
            helpText("Select a factor for subsetting specific analyses to."))

  })

  output$ui_outlier_treatment <- renderUI({
    req(uc$subset_factor)
    df <- create_analysis_sample()
    tagList(radioButtons("outlier_treatment", "Outlier treatment",
                         choices = list("No treatment" = 1, "Winsorization 1%/99%" = 2, "Winsorization 5%/95%" = 3,
                                        "Truncation 1%/99%" = 4, "Truncation 5%/95%" = 5),
                         selected = uc$outlier_treatment),
            selectInput("outlier_factor", label = "By group factor",
                        c("None",  unique(c(lcs_id$name, lts_id$name, lfactor$name, llogical$name))),
                        selected = isolate(uc$outlier_factor)),
            helpText("Indicate whether you want no outlier treatment",
                     "or whether you want outliers to be winsorized",
                     "to the given percentile or truncated if they exceed the given percentile.",
                     "Give a by group if you want outlier treatment to be done independently by group."))
  })

  output$ui_balanced_panel <- renderUI({
    req(uc$subset_factor)
    # Here I removed the isolate wrapper a while ago but now I do not longer understand why. Need to check this.
    tagList(checkboxInput("balanced_panel", "Balanced panel", value = uc$balanced_panel),
            helpText("Check if you want only observations included that have data for all periods."))
  })

  output$ui_bar_chart <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(h3("Bar Chart"),
                   selectInput("bar_chart_var1", label = "Select factor to display",
                               unique(c(lts_id$name, lfactor$name, llogical$name)),
                               selected = isolate(uc$bar_chart_var1)),
                   selectInput("bar_chart_var2", label = "Select additional factor to display",
                               unique(c("None", lts_id$name, lfactor$name, llogical$name)),
                               selected = isolate(uc$bar_chart_var2)),
                   checkboxInput("bar_chart_relative", "Relative display", value = uc$bar_chart_relative),
                   helpText("Check if you want to see the additional factor relative to the first factor."))
    if (uc$group_factor != "None")
      mytags <- append(mytags, list(hr(),
                                    selectInput("bar_chart_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$bar_chart_group_by))))
    tagList(mytags)
  })

  output$ui_missing_values <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(h3("Missing Values"),
                   helpText("This graphs shows the ratio of missing values for all variable years."))
    if (uc$group_factor != "None")
      mytags <- append(mytags, list(hr(),
                                    selectInput("missing_values_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$missing_values_group_by))))
    tagList(mytags)
  })

  output$ui_udv_name <- renderUI({
    req(uc$subset_factor)
    tagList(textInput('udv_name', "Enter name for your additional variable",""),
            helpText("If you want to create additional variables for the analysis,",
                     "provide a name (must not be taken) and a definition here.",
                     "A definition can consist of the base set variables,",
                     "parentheses and the operators",
                     "'+', '-', '*', '/', '==', '&', '|', '<', '>', '!', 'is.na()', '^', 'exp()', 'log()', 'lead()' and 'lag()'."))
  })

  output$ui_udv_def <- renderUI({
    req(uc$subset_factor)
    tagList(textInput('udv_definition', "Enter definition for your additional variable",""),
            actionButton("udv_submit","Submit"))
  })

  output$ui_descriptive_table_left <- renderUI({
    df <- create_analysis_sample()
    if (simple_call_mode)
      mytags <- list(h3("Descriptive Statistics"),
                   helpText("Hover over variable names with mouse to see variable definitions."))
    else
      mytags <- list(h3("Descriptive Statistics"),
                     helpText("Hover over variable names with mouse to see variable definitions."),
                     helpText("Select Tab to choose the analysis set of variables",
                              "or the base set of variables (to define new variables)."),
                     hr())
    if (uc$group_factor != "None")
      mytags <- append(mytags, list(selectInput("desc_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$desc_group_by)), hr()))
    if (!simple_call_mode)
      mytags <- append(mytags, list(helpText("Click here to delete selected variables from the analysis sample."),
                                    actionButton("delete_vars", "Delete Variables"),
                                    hr(),
                                    helpText("Click here to delete all user defined variables",
                                             "and to restore the original variable set of the analysis sample."),
                                    actionButton("restore_analysis_sample", "Restore Sample")))
    tagList(mytags)
  })

  output$ui_descriptive_table_right <- renderUI({
    req(uc$subset_factor)
    if (!simple_call_mode) {
      tabsetPanel(type = "tabs",
                  tabPanel("Analysis Set", DT::dataTableOutput("descriptive_table_analysis")),
                  tabPanel("Base Set", DT::dataTableOutput("descriptive_table_base")))
    } else {
      DT::dataTableOutput("descriptive_table_analysis")
    }
  })

  output$ui_histogram <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(h3("Histogram"),
                   selectInput("hist_var", label = "Select variable to display",
                               c(lnumeric$name, llogical$name),
                               selected = isolate(uc$hist_var)),
                   hr())
    if (uc$group_factor != "None")
      mytags <- append(mytags, list(selectInput("hist_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$hist_group_by)), hr()))
    mytags <- append(mytags, list(sliderInput("hist_nr_of_breaks", label = "Suggested number of cells", 5, 250,
                                              isolate(uc$hist_nr_of_breaks), step=5, ticks = FALSE)))
    tagList(mytags)
  })

  output$ui_ext_obs <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(h3("Extreme Observations"),
                   selectInput("ext_obs_var", label = "Select variable to sort data by",
                               c(lnumeric$name, llogical$name),
                               selected = isolate(uc$ext_obs_var)))
    if (uc$group_factor != "None")
      mytags <- append(mytags, list(selectInput("ext_obs_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$ext_obs_group_by))))
    mytags <- append(mytags, list(selectInput("ext_obs_period_by", label = "Select period to subset to",
                                              c("All", levels(df[,lts_id$col])),
                                              selected = isolate(uc$ext_obs_period_by))))
    tagList(mytags)
  })

  output$ui_by_group_bar_graph <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(h3("By Group Bar Chart"),
                   selectInput("bgbg_var", label = "Select variable to display",
                               c(lnumeric$name, llogical$name),
                               selected = isolate(uc$bgbg_var)),
                   selectInput("bgbg_byvar", label = "Select variable to group by",
                               unique(c(lts_id$name, lfactor$name)),
                               selected = isolate(uc$bgbg_byvar)),
                   selectInput("bgbg_stat", label = "Select statistic to display",
                               c("Mean" = "mean",
                                 "Median" = "median",
                                 "Standard deviation" = "sd",
                                 "Minimum" = "min",
                                 "25 %" = "q25",
                                 "75 %" = "q75",
                                 "Maximum" = "max"),
                               selected = isolate(uc$bgbg_stat)),
                   hr(),
                   checkboxInput("bgbg_sort_by_stat", "Sort by statistic", value = uc$bgbg_sort_by_stat))
    if (uc$group_factor != "None")
      mytags <- append(mytags, list(selectInput("bgbg_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$bgbg_group_by)), hr()))
    tagList(mytags)
  })

  output$ui_by_group_violin_graph <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(h3("By Group Violin Chart"),
                   selectInput("bgvg_var", label = "Select variable to display",
                               c(lnumeric$name, llogical$name),
                               selected = isolate(uc$bgvg_var)),
                   selectInput("bgvg_byvar", label = "Select variable to group by",
                               unique(c(lts_id$name, lfactor$name)),
                               selected = isolate(uc$bgvg_byvar)),
                   checkboxInput("bgvg_sort_by_stat", "Sort by group means", value = uc$bgvg_sort_by_stat),
                   helpText("(Note: Consider treating your outliers if this graph looks odd)"),
                   hr())
    if (uc$group_factor != "None")
      mytags <- append(mytags, list(selectInput("bgvg_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$bgvg_group_by)), hr()))
    tagList(mytags)
  })

  output$ui_trend_graph <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(
      h3("Time Trend Graph"),
      selectInput("trend_graph_var1", label = "Select first variable to display",
                  c(lnumeric$name, llogical$name),
                  selected = isolate(uc$trend_graph_var1)),
      selectInput("trend_graph_var2", label = "Select second variable to display",
                  c("None", c(lnumeric$name, llogical$name)),
                  selected = isolate(uc$trend_graph_var2)),
      selectInput("trend_graph_var3", label = "Select third variable to display",
                  c("None", c(lnumeric$name, llogical$name)),
                  selected = isolate(uc$trend_graph_var3)))
    if (uc$group_factor != "None")
      mytags <- append(mytags, list(hr(),
                                    selectInput("trend_graph_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$trend_graph_group_by))))
    tagList(mytags)
  })

  output$ui_quantile_trend_graph <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(
      h3("Quantile Time Trend Graph"),
      selectInput("quantile_trend_graph_var", label = "Select variable to display",
                  c(lnumeric$name, llogical$name),
                  selected = isolate(uc$quantile_trend_graph_var)),
      hr(),
      checkboxGroupInput("quantile_trend_graph_quantiles", "Quantiles to show:",
                         c("Min" = "0",
                           "1 %" = "0.01",
                           "5 %" = "0.05",
                           "10 %" = "0.10",
                           "25 %" = "0.25",
                           "50 %" = "0.50",
                           "75 %" = "0.75",
                           "90 %" = "0.90",
                           "95 %" = "0.95",
                           "99 %" = "0.99",
                           "Max" = "1"),
                         selected=isolate(uc$quantile_trend_graph_quantiles)))

    if (uc$group_factor != "None")
      mytags <- append(mytags, list(hr(),
                                    selectInput("quantile_trend_graph_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$quantile_trend_graph_group_by))))
    tagList(mytags)
  })

  output$ui_corrplot <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(h3("Correlation Plot"),
                   helpText("This plot visualizes sample correlations (Pearson above, Spearman below diagonal).",
                            "Reports correlations for all continuous variables. Hover over ellipse to get rho, P-Value and n."))
    if (uc$group_factor != "None")
      mytags <- append(mytags, list(hr(),
                                    selectInput("corrplot_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$corrplot_group_by))))
    tagList(mytags)
  })

  output$ui_scatter_plot <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(
      h3("Scatter Plot"),
      selectInput("scatter_x", label = "Select the x variable to display",
                  c(lnumeric$name, l2level$name),
                  selected = isolate(uc$scatter_x)),
      selectInput("scatter_y", label = "Select the y variable to display",
                  c(lnumeric$name, l2level$name),
                  selected = isolate(uc$scatter_y)),
      selectInput("scatter_size", label = "Select the variable to be reflected by dot size",
                  c("None", lnumeric$name, l2level$name),
                  selected = isolate(uc$scatter_size)),
      selectInput("scatter_color", label = "Select the variable to be reflected by color",
                  c("None", names(df)),
                  selected = isolate(uc$scatter_color)),
      checkboxInput("scatter_sample",
                    label = "Sample 1,000 observations to display if number of observations is higher",
                    value = isolate(uc$scatter_sample)),
      checkboxInput("scatter_loess",
                    label="Display Loess smoother",
                    value = isolate(uc$scatter_loess)),
      helpText("(Note: This might take a while to compute for large samples)"))
    if (uc$group_factor != "None")
      mytags <- append(mytags, list(hr(),
                                    selectInput("scatter_group_by", label = "Select group to subset to",
                                                c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                                selected = isolate(uc$scatter_group_by))))
    tagList(mytags)
  })

  output$ui_regression <- renderUI({
    df <- create_analysis_sample()
    tagList(
      h3("Regression Analysis"),
      selectInput("reg_y", label = "Select the dependent variable",
                  unique(c(lnumeric$name, l2level$name)),
                  selected = isolate(uc$reg_y)),
      selectInput("reg_x", label = "Select independent variable(s)",
                  c(lnumeric$name, l2level$name), multiple=TRUE,
                  selected = isolate(uc$reg_x)),
      hr(),
      selectInput("reg_fe1", label = "Select a categorial variable as the first fixed effect",
                  unique(c("None", lcs_id$name, lts_id$name, lfactor$name)),
                  selected = isolate(uc$reg_fe1)),
      selectInput("reg_fe2", label = "Select a categorial variable as the second fixed effect",
                  unique(c("None", lcs_id$name, lts_id$name, lfactor$name)),
                  selected = isolate(uc$reg_fe2)),
      hr(),
      selectInput("reg_by", label = "Select a categorial variable to subset the estimated models on",
                  unique(c("None", lts_id$name, lfactor$name)),
                  selected = isolate(uc$reg_by))
    )
  })


  output$ui_model <- renderUI({
    req (uc$reg_y)
    model <- 0
    if (isolate(uc$reg_y) %in% c(lnumeric$name, llogical$name))  model <- 1
    if (isolate(uc$reg_y) %in% l2level$name)  model <- model + 2
    if (model == 1) uc$model = "ols"
    if (model == 2) uc$model = "logit"
    if (model == 3) {
      list(radioButtons("model", "Estimator to use",
                        choices = list("Standard OLS" = "ols",
                                       "Logit regression" = "logit"),
                        selected = isolate(uc$model)),
           helpText("Indicate which estimator you want to use",
                    "(only OLS and binary response logit are implemented)"))
    }
  })


  output$ui_clustering <- renderUI({
    req (uc$reg_fe1, uc$reg_fe2)
    cluster <- "1"
    if (isolate(uc$reg_fe1) != "None")  cluster <- "2"
    if (isolate(uc$reg_fe2) != "None")  cluster <- "3"
    if ((isolate(uc$reg_fe1) != "None") & (isolate(uc$reg_fe2) != "None")) cluster <- "4"
    uc$cluster <- min(uc$cluster, cluster)
    list(switch(cluster,
           "1"= radioButtons("cluster", "Calculation of Standard Errors",
                             choices = list("Standard" = 1), selected = isolate(uc$cluster)),
           "2"= radioButtons("cluster", "Calculation of Standard Errors",
                             choices = list("Standard" = 1, "Clustered for the first fixed effect" = 2), selected = isolate(uc$cluster)),
           "3"= radioButtons("cluster", "Calculation of Standard Errors",
                             choices = list("Standard" = 1, "Clustered for the second fixed effect" = 3), selected = isolate(uc$cluster)),
           "4"= radioButtons("cluster", "Calculation of Standard Errors",
                             choices = list("Standard" = 1, "Clustered for the first fixed effect" = 2,
                                            "Clustered for the second fixed effect" = 3,
                                            "Two-way clustering" = 4), selected = isolate(uc$cluster))),
    helpText("Indicate how you want your standard errors to be estimated"))
  })

  output$bar_chart <- renderPlot({
    req(uc$bar_chart_var1, uc$bar_chart_var2, uc$bar_chart_group_by)
    df <- create_analysis_sample()
    df[, uc$bar_chart_var1] <- as.factor(df[, uc$bar_chart_var1])
    if (uc$bar_chart_var2 != "None")
      df[, uc$bar_chart_var2] <- as.factor(df[, uc$bar_chart_var2])
    if (uc$bar_chart_group_by != "All")
      df <- df[df[, uc$group_factor] == uc$bar_chart_group_by,]
    if (!anyNA(suppressWarnings(as.numeric(as.character(df[, uc$bar_chart_var1])))))
      numeric_bar_chart_var1 <- as.numeric(as.character(df[, uc$bar_chart_var1]))
    if (uc$bar_chart_var2 != "None" & (!uc$bar_chart_relative))
      p <- ggplot2::ggplot(df, ggplot2::aes(df[,uc$bar_chart_var1])) +
      ggplot2::geom_bar(ggplot2::aes(fill=df[,uc$bar_chart_var2]), position = "stack") +
      ggplot2::labs(x = uc$bar_chart_var1, fill = uc$bar_chart_var2)
    else if (uc$bar_chart_var2 != "None")
      p <- ggplot2::ggplot(df, ggplot2::aes(df[,uc$bar_chart_var1])) +
      ggplot2::geom_bar(ggplot2::aes(fill=df[,uc$bar_chart_var2]), position = "fill") +
      ggplot2::labs(x = uc$bar_chart_var1, fill = uc$bar_chart_var2, y = "Percent") +
      ggplot2::scale_y_continuous(labels = scales::percent_format())
    else p <- ggplot2::ggplot(df, ggplot2::aes(df[,uc$bar_chart_var1])) +
      ggplot2::geom_bar() + ggplot2::labs(x = uc$bar_chart_var1)
    if (length(levels(df[,uc$bar_chart_var1])) > 10 &&
        !anyNA(suppressWarnings(as.numeric(as.character(df[, uc$bar_chart_var1])))))
      p <- p + ggplot2::scale_x_discrete(breaks = pretty(as.numeric(as.character(df[, uc$bar_chart_var1])), n = 10))
    p
  })

  output$missing_values <- renderPlot({
    req(uc$missing_values_group_by)
    df <- create_analysis_sample()
    if (uc$missing_values_group_by != "All")
      df <- df[df[, uc$group_factor] == uc$missing_values_group_by,]
    prepare_missing_values_graph(df, lts_id$name)
  })

  output$descriptive_table_analysis <- DT::renderDataTable(
    server=FALSE,
    expr = {
      df <- create_analysis_sample()
      rowtips = as.character(sample_definition[c(lnumeric$col, llogical$col), "var_def"])
      DT::datatable({
          if (uc$desc_group_by == "All") prepare_descriptive_table(df[,c(lnumeric$col, llogical$col)])$df
          else prepare_descriptive_table(df[df[, uc$group_factor] == uc$desc_group_by, c(lnumeric$col, llogical$col)])$df
        },
        colnames=c('Variable'=1), selection=list(mode = 'multiple', target = 'row'),
        options= list(dom='t', paging=FALSE, ordering=FALSE),
        callback = DT::JS("var tips =[", paste("'", quote_escape(rowtips), "'", collapse=", ", sep=""), "],
                      firstColumn = $('#descriptive_table_analysis tr td:first-child');
                      for (var i = 0; i < tips.length; i++) {
                      $(firstColumn[i]).attr('title', jQuery('<div />').html(tips[i]).text());
                      }")
      ) %>%
        DT::formatCurrency(1, currency ="", interval=3, mark=',', digits=0) %>%
        DT::formatCurrency('Mean', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('Std. dev.', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('Min.', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('25 %', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('Median', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('75 %', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('Max.', currency = "", interval=3, mark=',', digits=3)
    }
  )

  output$descriptive_table_base <- DT::renderDataTable(
    server=FALSE,
    expr = {
      df <- create_base_sample()
      rowtips = as.character(bs_definition[bs_definition$type == "numeric" |
                                             bs_definition$type == "logical", "var_def"])
      DT::datatable({
        if (uc$desc_group_by == "All") prepare_descriptive_table(df[,c(as.character(bs_definition[bs_definition$type == "numeric" |
                                                                                       bs_definition$type == "logical", "var_name"]))])$df
        else prepare_descriptive_table(df[df[, uc$group_factor] == uc$desc_group_by, c(as.character(bs_definition[bs_definition$type == "numeric" |
                                                                                                                    bs_definition$type == "logical", "var_name"]))])$df
      },
      colnames=c('Variable'=1), selection=list(mode = 'none'),
      options= list(dom='t', paging=FALSE, ordering=FALSE),
      callback = DT::JS("var tips =[", paste("'", quote_escape(rowtips), "'", collapse=", ", sep=""), "],
                      firstColumn = $('#descriptive_table_base tr td:first-child');
                      for (var i = 0; i < tips.length; i++) {
                      $(firstColumn[i]).attr('title', jQuery('<div />').html(tips[i]).text());
                      }")
       )%>%
        DT::formatCurrency(1, currency ="", interval=3, mark=',', digits=0) %>%
        DT::formatCurrency('Mean', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('Std. dev.', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('Min.', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('25 %', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('Median', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('75 %', currency = "", interval=3, mark=',', digits=3) %>%
        DT::formatCurrency('Max.', currency = "", interval=3, mark=',', digits=3)
    }
  )

  output$histogram <- renderPlot({
    req(uc$hist_group_by, uc$hist_var, uc$hist_nr_of_breaks)
    df <- create_analysis_sample()
    if (uc$hist_group_by == "All") hist(as.numeric(df[,uc$hist_var]), main="", xlab = uc$hist_var, col="red", right = FALSE, breaks=uc$hist_nr_of_breaks)
    else hist(as.numeric(df[df[, uc$group_factor] == uc$hist_group_by, uc$hist_var]), main="", xlab = uc$hist_var, col="red", right=FALSE, breaks=uc$hist_nr_of_breaks)
  })

  output$ext_obs <- renderPrint({
      df <- create_analysis_sample()
      vars <- c(lcs_id$name, lts_id$name, uc$ext_obs_var)
      if (uc$group_factor != "None") vars <- c(vars, uc$group_factor)
      df <- df[, vars]
      if (req(uc$ext_obs_period_by) != "All")
        df <- df[df[, lts_id$name] == uc$ext_obs_period_by,]
      if (req(uc$ext_obs_group_by) != "All")
        df <- df[df[, uc$group_factor] == uc$ext_obs_group_by, vars]
      df <- droplevels(df[complete.cases(df),])
      if (nrow(df) <= 10) cat("Not enough data to generate table")
      else {
        tab <- prepare_ext_obs_table(df, var = uc$ext_obs_var)
        cat(tab$kable_ret %>%
              kableExtra::kable_styling())
      }
    }
  )

  output$by_group_bar_graph.ui <- renderUI({
    req(uc$bgbg_group_by, uc$bgbg_var, uc$bgbg_byvar)
    df <- create_analysis_sample()
    if (uc$bgbg_group_by == "All")
      bins <- length(unique(df[is.finite(df[,uc$bgbg_var]), uc$bgbg_byvar]))
    else
      bins <- length(unique(df[df[, uc$group_factor] == uc$bgbg_group_by & is.finite(df[,uc$bgbg_var]), uc$bgbg_byvar]))
    isolate(plotOutput("by_group_bar_graph",
                       height=max(400, 15 * bins)))
  })

  output$by_group_bar_graph <- renderPlot({
    req(uc$bgbg_var, uc$bgbg_byvar)
    q25 <- function(x, na.rm) {quantile(x, 0.25, na.rm)}
    q75 <- function(x, na.rm) {quantile(x, 0.75, na.rm)}

    df <- create_analysis_sample()
    if (uc$bgbg_group_by == "All")
      prepare_by_group_bar_graph(df[, c(uc$bgbg_byvar, uc$bgbg_var)],
                                 uc$bgbg_byvar, uc$bgbg_var, get(uc$bgbg_stat), uc$bgbg_sort_by_stat)$plot +
      ggplot2::ylab(paste(uc$bgbg_stat, uc$bgbg_var))
    else
      prepare_by_group_bar_graph(df[df[, uc$group_factor] == uc$bgbg_group_by, c(uc$bgbg_byvar, uc$bgbg_var)],
                                 uc$bgbg_byvar, uc$bgbg_var, get(uc$bgbg_stat), uc$bgbg_sort_by_stat)$plot +
      ggplot2::ylab(paste(uc$bgbg_stat, uc$bgbg_var))
  })

  output$by_group_violin_graph.ui <- renderUI({
    req(uc$bgvg_group_by, uc$bgvg_var, uc$bgvg_byvar)
    df <- create_analysis_sample()
    if (uc$bgvg_group_by == "All")
      bins <- length(unique(df[is.finite(df[,uc$bgvg_var]), uc$bgvg_byvar]))
    else
      bins <- length(unique(df[df[, uc$group_factor] == uc$bgvg_group_by & is.finite(df[,uc$bgvg_var]), uc$bgvg_byvar]))
    isolate(plotOutput("by_group_violin_graph",
                       height=max(400, 30 * bins)))
  })

  output$by_group_violin_graph <- renderPlot({
    req(uc$bgvg_var, uc$bgvg_byvar)
    df <- create_analysis_sample()
    if (uc$bgvg_group_by == "All")
      prepare_by_group_violin_graph(df[, c(uc$bgvg_byvar, uc$bgvg_var)],
                                    uc$bgvg_byvar, uc$bgvg_var, uc$bgvg_sort_by_stat)
    else
      prepare_by_group_violin_graph(df[df[, uc$group_factor] == uc$bgvg_group_by, c(uc$bgvg_byvar, uc$bgvg_var)],
                                    uc$bgvg_byvar, uc$bgvg_var, uc$bgvg_sort_by_stat)
  })

  output$trend_graph <- renderPlot({
    req(uc$trend_graph_var1,uc$trend_graph_var2,uc$trend_graph_var3)
    df <- create_analysis_sample()
    if (uc$trend_graph_group_by == "All")
      varlist <- c(lts_id$name, uc$trend_graph_var1, uc$trend_graph_var2, uc$trend_graph_var3)
    else
      varlist <- c(lts_id$name, uc$group_factor, uc$trend_graph_var1, uc$trend_graph_var2, uc$trend_graph_var3)
    varlist <- varlist[! varlist %in% "None"]
    df <- df[,varlist]
    if (uc$trend_graph_group_by == "All") prepare_trend_graph(df, lts_id$name)
    else prepare_trend_graph(df[df[, uc$group_factor] == uc$trend_graph_group_by,-which(names(df) %in% uc$group_factor)], lts_id$name)
  })

  output$quantile_trend_graph <- renderPlot({
    req(uc$quantile_trend_graph_var)
    df <- create_analysis_sample()
    quantiles <- as.numeric(uc$quantile_trend_graph_quantiles)
    if (uc$quantile_trend_graph_group_by == "All")
      prepare_quantile_trend_graph(df[, c(lts_id$name, uc$quantile_trend_graph_var)], lts_id$name, quantiles)
    else
      prepare_quantile_trend_graph(df[df[, uc$group_factor] == uc$quantile_trend_graph_group_by,
                                      c(lts_id$name, uc$quantile_trend_graph_var)], lts_id$name, quantiles)
  })

  output$corrplot.ui <- renderUI({
    req(uc$subset_factor)
    isolate(plotOutput("corrplot", hover = hoverOpts("corrplot_hover", delay = 100, delayType = "debounce"),
               height = max(min(length(c(lnumeric$col, llogical$col))*50,1000), 300),
               width = max(min(length(c(lnumeric$col, llogical$col))*50,1000), 300)))
  })

  output$corrplot <- renderPlot({
    if (DEBUG) tictoc::tic("rendering corrplot")
    df <- create_analysis_sample()
    if (uc$corrplot_group_by != "All") df <- df[df[, uc$group_factor] == uc$corrplot_group_by,]
    vars <- c(lnumeric$col, llogical$col)
    vars <- vars[vars %in% which(!sapply(df, function(x) all(is.na(x))))]
    vars <- vars[vars %in% which(!sapply(df, function(x) (all(duplicated(x)[-1]))))]
    ret <- prepare_correlation_graph(df[, vars])
    correl_r <<- ret$df_corr
    correl_p <<- ret$df_prob
    correl_n <<- ret$df_n
    if (DEBUG) message(do.call(tictoc::toc.outmsg, tictoc::toc(quiet = TRUE)))
  })

  output$corrplot_hover_info <- renderUI({
    req(input$corrplot_hover)
    hover <- input$corrplot_hover
    if (!is.null(hover)) {
      nvars <- nrow(correl_r)
      r <- rep(1:nvars,nvars)
      c <- rep(1:nvars, each=nvars)
      df <- data.frame(r,c)
      point <- nearPoints(df, hover, threshold = 10, maxpoints = 1, xvar = "c", yvar="r", addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

      # calculate distance from left and bottom side of the picture in pixels
      left_px <- (hover$range$left + left_pct * (hover$range$right - hover$range$left)) /
        hover$img_css_ratio$x
      top_px <- (hover$range$top + top_pct * (hover$range$bottom - hover$range$top)) /
        hover$img_css_ratio$y

      # create style property for tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")

      # actual tooltip created as wellPanel
      wellPanel(
        style = style, class = "well-sm",
        htmltools::HTML(paste0(formatC(correl_r[nvars - point$r + 1, point$c], format ="f", digits=3), " (",
                    formatC(correl_p[nvars - point$r + 1, point$c], format ="f", digits=3), ")<br> n: ",
                    formatC(correl_n[nvars - point$r + 1, point$c], format ="f", digits=0, big.mark=",")))
      )
    }
  })

  output$scatter_plot <- renderPlot({
    req(uc$scatter_x,uc$scatter_y,uc$scatter_size,uc$scatter_color)
    if (DEBUG) tictoc::tic("rendering scatter_plot")
    scatter_df <- create_analysis_sample()
    if (uc$scatter_group_by != "All")
      scatter_df <- scatter_df[scatter_df[, uc$group_factor] == uc$scatter_group_by,]
    varlist <- c(lcs_id$name, lts_id$name,
                 uc$scatter_x, uc$scatter_y, uc$scatter_color, uc$scatter_size)
    scatter_df <- scatter_df[,varlist[!grepl("None", varlist)]]
    scatter_df <- scatter_df[complete.cases(scatter_df),]
    if (uc$scatter_color %in% lfactor$name) scatter_df[,uc$scatter_color] <- as.factor(scatter_df[,uc$scatter_color])
    if (uc$scatter_sample & (nrow(scatter_df) > 1000)) {
      set.seed(42)
      scatter_df <- dplyr::sample_n(scatter_df, 1000)
    }
    scatter_color <- ifelse(uc$scatter_color == "None", "", uc$scatter_color)
    scatter_size <- ifelse(uc$scatter_size == "None", "", uc$scatter_size)
    scatter_loess <- ifelse(uc$scatter_loess, 1, 0)
    plot <- prepare_scatter_plot(scatter_df, uc$scatter_x, uc$scatter_y,
                                 scatter_color, scatter_size, scatter_loess)
    if (DEBUG) message(do.call(tictoc::toc.outmsg, tictoc::toc(quiet = TRUE)))
    plot
  })

  output$hover_info <- renderUI({
    req(input$plot_hover,uc$scatter_x,uc$scatter_y,uc$scatter_size,uc$scatter_color)
    scatter_df <- create_analysis_sample()
    if (uc$scatter_group_by != "All")
      scatter_df <- scatter_df[scatter_df[, uc$group_factor] == uc$scatter_group_by,]
    varlist <- c(lcs_id$name, lts_id$name,
                 uc$scatter_x, uc$scatter_y, uc$scatter_size, uc$scatter_color)
    scatter_df <- scatter_df[,varlist[!grepl("None", varlist)]]
    hover <- input$plot_hover
    if (!is.null(hover)) {
      hover$mapping$x <- uc$scatter_x
      hover$mapping$y <- uc$scatter_y
    }
    point <- nearPoints(scatter_df, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- (hover$range$left + left_pct * (hover$range$right - hover$range$left)) /
      hover$img_css_ratio$x
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top) /
      hover$img_css_ratio$y

    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # Get rid of the factors in the data frame by converting all columns to character
    point[] <- lapply(point, as.character)

    # actual tooltip created as wellPanel
    wellPanel(
      style = style, class = "well-sm",
      htmltools::HTML(paste0(point[,lcs_id$name], collaspe=" "),
                  "<br>",
                  point[,lts_id$name])
    )
  })

  output$regression <- renderPrint({
    if (DEBUG) tictoc::tic("estimating regressions")
    req(uc$reg_y, uc$reg_x, uc$reg_fe1, uc$reg_fe2, uc$model, uc$reg_by)
    varlist <- c(uc$reg_y, uc$reg_x, uc$reg_fe1, uc$reg_fe2, uc$reg_by)
    varlist <- varlist[!varlist %in% "None"]
    df <- create_analysis_sample()[,varlist]
    if (uc$reg_by != "None") df[, uc$reg_by] <- as.factor(df[, uc$reg_by])
    if (!uc$reg_y %in% c(lnumeric$name, llogical$name))
      df[, uc$reg_y] <- as.factor(df[, uc$reg_y])
    df <- droplevels(df[complete.cases(df),])

    feffect <- ""
    if (uc$reg_fe1 != "None") {
      feffect <- uc$reg_fe1
      if (uc$reg_fe2 != "None") feffect <- c(feffect, uc$reg_fe2)
    }
    else if (uc$reg_fe2 != "None") feffect <- uc$reg_fe2
    cluster <- ""
    if (uc$cluster == 2) cluster <- uc$reg_fe1
    if (uc$cluster == 3) cluster <- uc$reg_fe2
    if (uc$cluster == 4) cluster <- c(uc$reg_fe1, uc$reg_fe2)
    cluster <- cluster[!cluster %in% "None"]
    reg_by <- ifelse(uc$reg_by != "None", uc$reg_by, "")
    t <- prepare_regression_table(df = df, dvs = uc$reg_y, idvs = uc$reg_x,
                                  feffects = feffect, clusters = cluster,
                                  models = uc$model, byvar = reg_by)
    if (DEBUG) message(do.call(tictoc::toc.outmsg, tictoc::toc(quiet = TRUE)))
    htmltools::HTML(t$table)
  })

  observe({
    in_file <- req(input$upload)
    if (!is.null(in_file)) {
      tryCatch(
        {
          if (shiny_store_encrypted) {
            encrypted <- readRDS(in_file$datapath)
            decrypted <- openssl::aes_cbc_decrypt(encrypted, key)
            config_list <- unserialize(decrypted)
          } else config_list <- readRDS(in_file$datapath)
          if (!is.list(config_list)) stop()
          isolate(parse_config(config_list))
        }, error = function(cond) {
          session$sendCustomMessage(type = 'testmessage', message = "Unable to parse file")
        })
    }
  })

  output$download <- downloadHandler(
    filename ="ExPanD.RDS",
    content = function(file) {
      if (shiny_store_encrypted) {
        raw <- serialize(reactiveValuesToList(uc), NULL)
        encrypted <- openssl::aes_cbc_encrypt(raw, key, iv = NULL)
        saveRDS(encrypted, file)
      } else saveRDS(reactiveValuesToList(uc), file)
    }
  )

  create_nb_code_for_component <- function(comp) {
    df <- create_analysis_sample()

    if (comp == "setup") return({
      nb_code <- c(
        "### Setup", " ",
        "```{r setup}", " ",
        "suppressWarnings(suppressMessages({",
          "  library(knitr)",
          "  library(kableExtra)",
          "  library(htmltools)",
          "  library(tidyverse)",
          "  library(scales)",
          "  library(ExPanDaR)",
        "}))",
        " ", "```", " ", " "
      )
    })


    if (comp == "create_sample") return({
      nb_code <- c(
        "### Create Sample", " ",
        "This step reads the raw data provided by `ExPanD()` and generates the sample for the analysis.",
        " ",
        "```{r create_sample}", " ",
        "create_sample <- function(df, df_def) {",
        "  # Set infinite numerical variables to NA",
        "  df[, df_def$var_name[df_def$type == \"numeric\"]] <-",
        "    lapply(df[, df_def$var_name[df_def$type == \"numeric\"]],",
        "      function(x) ifelse(is.finite(x), x, NA))",
        " ",
        "  # Delete numerical variables that only contain NAs",
        "  all_na_vars <- sapply(df, function (x) all(is.na(x)))",
        "  df_def <- df_def[!all_na_vars,]",
        "  df <- df[, df_def$var_name]",
        " ",
        "  # Drop observations that are NA in variables that are not allowed to",
        "  df <- df[complete.cases(df[, df_def$var_name[which(df_def$can_be_na == FALSE)]]), ]"
      )

      if ((uc$subset_factor != "Full Sample") & (uc$subset_value != "All"))
        nb_code <- c(nb_code,
                     " ",
                     "  # Subset the analysis as requested in ExPanD()",
                     sprintf('  df <- df[df$%s == "%s", ]', uc$subset_factor, uc$subset_value))

      if (uc$balanced_panel)
        nb_code <- c(nb_code,
                     " ",
                     "  # Balance sample as requested in ExPanD()",
                     '  df <- group_by_at(df, vars(one_of(df_def$var_name[df_def$type  == "cs_id"]))) %>%',
                     '    mutate(nobs = n())',
                     '  max_nobs <- length(levels(as.data.frame(df[, df_def$var_name[df_def$type  == "ts_id"]])[,1]))',
                     '  bal_df <- as.data.frame(select(filter(df, nobs == max_nobs), -nobs))',
                     '  df <- as.data.frame(bal_df)')

      if (uc$outlier_treatment > 1) {
        if (uc$outlier_factor == "None")
          group <- "NULL"
        else
          group = sprintf('"%s"', df[,uc$outlier_factor])

        nb_code <- c(nb_code,
                     " ",
                     "  # Outlier treatment as requested in ExPanD()",
                     '  nums <- df_def$var_name[df_def$type == "numeric"]')
      }

      if (uc$outlier_treatment == 2)
        nb_code <- c(nb_code, sprintf('  df[, nums] <- treat_outliers(df[, nums], 0.01, FALSE, %s)', group))
      if (uc$outlier_treatment == 3)
        nb_code <- c(nb_code, sprintf('  df[, nums] <- treat_outliers(df[, nums], 0.05, FALSE, %s)', group))
      if (uc$outlier_treatment == 4)
        nb_code <- c(nb_code, sprintf('  df[, nums] <- treat_outliers(df[, nums], 0.01, TRUE, %s)', group))
      if (uc$outlier_treatment == 5)
        nb_code <- c(nb_code, sprintf('  df[, nums] <- treat_outliers(df[, nums], 0.05, TRUE, %s)', group))

      nb_code <- c(nb_code,
                   " ",
                   "  df <- droplevels(df)",
                   "  return(list(df = df, df_def = df_def))",
                   "}",
                   " ",
                   "load(\"expand_nb_data.Rdata\")",
                   " ",
                   "smp_list <- create_sample(nb_df, nb_df_def)",
                   "smp <- smp_list$df",
                   "smp_def <- smp_list$df_def",
                   " ", "```", " ", " ")

      nb_code
    })


    if (comp == "bar_chart") return({
      nb_code <- c(
        "### Bar Chart", " ",
        "```{r bar_chart}", " ",
        "df <- smp"
      )
      if (uc$bar_chart_group_by != "All")
        nb_code <- c(nb_code,
                     sprintf('df <- df[df$%s == "%s", ]',
                             uc$group_factor,
                             uc$bar_chart_group_by))
      nb_code <- c(nb_code,
                   sprintf('df$%s <- as.factor(df$%s)', uc$bar_chart_var1, uc$bar_chart_var1))

      if (uc$bar_chart_var2 != "None")
        nb_code <- c(nb_code,
                     sprintf('df$%s <- as.factor(df$%s)', uc$bar_chart_var2, uc$bar_chart_var2))

      if (uc$bar_chart_var2 != "None" & (!uc$bar_chart_relative)) {
        nb_code <- c(nb_code,
                     sprintf('p <- ggplot(df, aes(x = %s)) +', uc$bar_chart_var1),
                     sprintf('  geom_bar(aes(fill= %s), position = "stack") +', uc$bar_chart_var2),
                     sprintf('  labs(x = "%s", fill = "%s")', uc$bar_chart_var1, uc$bar_chart_var2))
      } else if (uc$bar_chart_var2 != "None") {
        nb_code <- c(nb_code,
                     sprintf('p <- ggplot(df, aes(x = %s)) +', uc$bar_chart_var1),
                     sprintf('  geom_bar(aes(fill = %s), position = "fill") +', uc$bar_chart_var2),
                     sprintf('  labs(x = "%s", fill = "%s", y = "Percent") +', uc$bar_chart_var1, uc$bar_chart_var2),
                     'scale_y_continuous(labels = percent_format())')
      } else {
        nb_code <- c(nb_code,
                     sprintf('p <- ggplot(df, aes(x = %s)) +', uc$bar_chart_var1),
                     sprintf('geom_bar() + labs(x = "%s")', uc$bar_chart_var1))
      }

      if (length(levels(df[,uc$bar_chart_var1])) > 10 &&
          !anyNA(suppressWarnings(as.numeric(as.character(df[, uc$bar_chart_var1])))))
        nb_code <- c(nb_code,
                     sprintf('p <- p + scale_x_discrete(breaks = pretty(as.numeric(as.character(df$%s)), n = 10))', uc$bar_chart_var1))

      nb_code <- c(nb_code, "p", " ", "```", " ", " ")

      nb_code
    })


    if(comp == "missing_values") return({
      nb_code <- c(
        "### Missing Values", " ",
        "```{r missing_values}", " ",
        "df <- smp"
      )
      if (uc$missing_values_group_by != "All")
        nb_code <- c(nb_code,
                     sprintf('df <- df[df$%s == "%s",]',
                             uc$group_factor,
                             uc$missing_values_group_by))
      nb_code <- c(nb_code,
                   sprintf('prepare_missing_values_graph(df, "%s")', lts_id$name),
                   " ", "```", " ", " ")

      nb_code
    })


    if(comp == "descriptive_table") return({
      nb_code <- c(
        "### Descriptive Statistics", " ",
        "```{r descriptive_statistics}", " ",
        "df <- smp",
        "t <- prepare_descriptive_table(smp)",
        "t$kable_ret  %>%",
        '  kable_styling("condensed", full_width = F, position = "center")',
        " ", "```", " ", " ")

      nb_code
    })


    if(comp == "histogram") return({
      nb_code <- c(
        "### Histogram", " ",
        "```{r histogram}", " "
      )

      if (uc$hist_group_by == "All")
        nb_code <- c(nb_code,
                     sprintf('var <- as.numeric(smp$%s)', uc$hist_var))
      else
        nb_code <- c(nb_code,
                     sprintf('var <- as.numeric(smp$%s[smp$%s == "%s"])',
                             uc$hist_var, uc$group_factor, uc$hist_group_by))

      nb_code <- c(nb_code,
                   sprintf('hist(var, main="", xlab = "%s", col="red", right = FALSE, breaks= %d)',
                           uc$hist_var, uc$hist_nr_of_breaks),
                   " ", "```", " ", " ")

      nb_code
    })


    if(comp == "ext_obs") return({
      nb_code <- c(
        "### Extreme Observations", " ",
        "```{r extreme_obs}", " ",
        "df <- smp")

      if (uc$group_factor != "None")
        nb_code <- c(nb_code,
                     sprintf('vars <- c("%s", "%s", "%s", "%s")',
                             lcs_id$name, lts_id$name, uc$ext_obs_var, uc$group_factor))
      else nb_code <- c(nb_code,
                        sprintf('vars <- c("%s", "%s", "%s")',
                                lcs_id$name, lts_id$name, uc$ext_obs_var))

      if (uc$ext_obs_period_by != "All")
        nb_code <- c(nb_code,
                     sprintf('df <- df[df$%s == "%s", ]',
                             lts_id$name, uc$ext_obs_period_by))
      if (uc$ext_obs_group_by != "All")
        nb_code <- c(nb_code,
                     sprintf('df <- df[df$%s == "%s", ]',
                             uc$group_factor, uc$ext_obs_group_by))

      nb_code <- c(nb_code,
        "df <- df[, vars]",
        "df <- droplevels(df[complete.cases(df), ])",
        "if (nrow(df) <= 10) {",
        '  cat("Not enough data to generate table")',
        "} else {",
        sprintf('  tab <- prepare_ext_obs_table(df, var = "%s")', uc$ext_obs_var),
        "  tab$kable_ret %>%",
        "    kable_styling()",
        "}",
        " ", "```", " ", " ")

      nb_code
    })


    if (uc$bgbg_group_by == "All") {
      prepare_by_group_bar_graph(df[, c(uc$bgbg_byvar, uc$bgbg_var)],
                                 uc$bgbg_byvar, uc$bgbg_var, get(uc$bgbg_stat), uc$bgbg_sort_by_stat)$plot +
        ylab(paste(uc$bgbg_stat, uc$bgbg_var))
    } else {
      prepare_by_group_bar_graph(df[df[, uc$group_factor] == uc$bgbg_group_by, c(uc$bgbg_byvar, uc$bgbg_var)],
                                 uc$bgbg_byvar, uc$bgbg_var, get(uc$bgbg_stat), uc$bgbg_sort_by_stat)$plot +
        ylab(paste(uc$bgbg_stat, uc$bgbg_var))
    }


    if(comp == "by_group_bar_graph") return({
      nb_code <- c(
        "### By Group Bar Graph", " ",
        "```{r by_group_bar_graph}", " ",
        "df <- smp", "")

      if (uc$bgbg_stat == "q25")
        nb_code <- c(nb_code,
                     'q25 <- function(x, na.rm) {quantile(x, 0.25, na.rm)}')
      if (uc$bgbg_stat == "q75")
        nb_code <- c(nb_code,
                     'q75 <- function(x, na.rm) {quantile(x, 0.75, na.rm)}')

      if (uc$bgbg_group_by != "All") {
        nb_code <- c(nb_code,
                     sprintf('df <- df[df$%s == "%s", ]',
                             uc$group_factor, uc$bgbg_group_by))
      }

      nb_code <- c(nb_code,
                   sprintf('prepare_by_group_bar_graph(df, "%s", "%s", %s, %s)$plot +',
                           uc$bgbg_byvar, uc$bgbg_var, uc$bgbg_stat, uc$bgbg_sort_by_stat),
                   sprintf('  ylab("%s %s")', uc$bgbg_stat, uc$bgbg_var),
                   " ", "```", " ", " ")

      nb_code
    })


    if(comp == "by_group_violin_graph") return({
      nb_code <- c(
        "### By Group Violin Graph", " ",
        "```{r by_group_violin_graph}", " ",
        "df <- smp", "")

      if (uc$bgvg_group_by != "All") {
        nb_code <- c(nb_code,
                     sprintf('df <- df[df$%s == "%s", ]',
                             uc$group_factor, uc$bgvg_group_by))
      }

      nb_code <- c(nb_code,
                   sprintf('prepare_by_group_violin_graph(df, "%s", "%s", %s)',
                           uc$bgvg_byvar, uc$bgvg_var, uc$bgvg_sort_by_stat),
                   " ", "```", " ", " ")

      nb_code
    })


    if(comp == "trend_graph") return({
      nb_code <- c(
        "### Trend Graph", " ",
        "```{r trend_graph}", " ")

      vars <- c(lts_id$name, uc$trend_graph_var1, uc$trend_graph_var2, uc$trend_graph_var3)
      vars <- vars[which(!vars %in% "None")]

      if (uc$trend_graph_group_by == "All") {
        nb_code <- c(nb_code, sprintf('df <- smp[, c("%s")]', paste(vars, collapse = '", "')), "")
      } else {
        nb_code <- c(nb_code,
                     sprintf('df <- df[df$%s == "%s", %s]',
                             uc$group_factor, uc$trend_graph_group_by, vars))
      }

      nb_code <- c(nb_code,
                   sprintf('prepare_trend_graph(df, "%s")$plot', lts_id$name),
                   " ", "```", " ", " ")

      nb_code
    })


    if(comp == "quantile_trend_graph") return({
      nb_code <- c(
        "### Quantile Trend Graph", " ",
        "```{r quantile_trend_graph}", " ")

      quantiles <- as.numeric(uc$quantile_trend_graph_quantiles)

      if (uc$quantile_trend_graph_group_by != "All") {
        nb_code <- c(nb_code,
                     sprintf('df <- df[df$%s == "%s", ]',
                             uc$group_factor, uc$quantile_trend_graph_group_by))
      }

      nb_code <- c(nb_code,
                   sprintf('prepare_quantile_trend_graph(df, "%s", c(%s), "%s")$plot',
                           lts_id$name, paste(quantiles, collapse = ", "), uc$quantile_trend_graph_var),
                   " ", "```", " ", " ")

      nb_code
    })


    if(comp == "corrplot") return({
      nb_code <- c(
        "### Correlation Graph", " ",
        "```{r corrplot}", " ",
        "df <- smp", "")

      if (uc$corrplot_group_by != "All") {
        nb_code <- c(nb_code,
                     sprintf('df <- df[df$%s == "%s", ]',
                             uc$group_factor, uc$corrplot_group_by))
      }

      vars <- which(sapply(df, function(x) all(is.numeric(x) | is.logical(x))))
      vars <- vars[vars %in% which(!sapply(df, function(x) all(is.na(x))))]
      vars <- vars[vars %in% which(!sapply(df, function(x) (all(duplicated(x)[-1]))))]

      nb_code <- c(nb_code,
                   sprintf('ret <- prepare_correlation_graph(df[, c(%s)])', paste(vars, collapse = ", ")),
                   " ", "```", " ", " ")

      nb_code
    })


    if(comp == "scatter_plot") return({
      nb_code <- c(
        "### Scatter Plot", " ",
        "```{r scatter_plot}", " ",
        "df <- smp", "")

      if (uc$scatter_group_by != "All") {
        nb_code <- c(nb_code,
                     sprintf('df <- df[df$%s == "%s", ]',
                             uc$group_factor, uc$scatter_group_by))
      }

      vars <- c(lcs_id$name, lts_id$name,
                uc$scatter_x, uc$scatter_y, uc$scatter_color, uc$scatter_size)
      vars <- vars[which(!vars %in% "None")]

      nb_code <- c(nb_code,
                   sprintf('df <- df[, c("%s")]', paste(vars, collapse = '", "')),
                   "df <- df[complete.cases(df), ]")

      scatter_df <- df[, vars]
      scatter_df <- scatter_df[complete.cases(scatter_df), ]

      if (uc$scatter_color %in% lfactor$name)
        nb_code <- c(nb_code,
                     sprintf('df$%s <- as.factor(df$%s)', uc$scatter_color, uc$scatter_color))
      if (uc$scatter_sample & (nrow(scatter_df) > 1000)) {
        nb_code <- c(nb_code,
                     "set.seed(42)",
                     "df <- sample_n(df, 1000)")
      }

      parm_str <- sprintf('"%s", "%s"', uc$scatter_x, uc$scatter_y)
      if (uc$scatter_color != "None") parm_str <- paste0(parm_str, sprintf(', color = "%s"', uc$scatter_color))
      if (uc$scatter_size != "None") parm_str <- paste0(parm_str, sprintf(', size = "%s"', uc$scatter_size))
      if (uc$scatter_loess) parm_str <- paste0(parm_str, ", loess = 1")

      nb_code <- c(nb_code,
                   sprintf('prepare_scatter_plot(df, %s)', parm_str),
                   " ", "```", " ", " ")

      nb_code
    })


    if(comp == "regression") return({
      nb_code <- c(
        "### Regresssion Table", " ",
        "```{r regression}", " ",
        "df <- smp", "")

      vars <- c(uc$reg_y, uc$reg_x, uc$reg_fe1, uc$reg_fe2, uc$reg_by)
      vars <- vars[!vars %in% "None"]

      nb_code <- c(nb_code,
                   sprintf('df <- df[, c("%s")]', paste(vars, collapse = '", "')),
                   "df <- df[complete.cases(df), ]")


      if (uc$reg_by != "None")
        nb_code <- c(nb_code,
                     sprintf('df$%s <- as.factor(df$%s)', uc$reg_by, uc$reg_by))
      if (!uc$reg_y %in% c(lnumeric$name, llogical$name))
        nb_code <- c(nb_code,
                     sprintf('df$%s <- as.factor(df$%s)', uc$reg_y, uc$reg_y))

      nb_code <- c(nb_code, "df <- droplevels(df)")


      feffect <- ""
      if (uc$reg_fe1 != "None") {
        feffect <- uc$reg_fe1
        if (uc$reg_fe2 != "None") feffect <- c(feffect, uc$reg_fe2)
      } else if (uc$reg_fe2 != "None") feffect <- uc$reg_fe2
      cluster <- ""
      if (uc$cluster == 2) cluster <- uc$reg_fe1
      if (uc$cluster == 3) cluster <- uc$reg_fe2
      if (uc$cluster == 4) cluster <- c(uc$reg_fe1, uc$reg_fe2)
      cluster <- cluster[!cluster %in% "None"]
      reg_by <- ifelse(uc$reg_by != "None", uc$reg_by, "")


      parm_str <- sprintf('df, dvs = "%s", idvs = c("%s")',  uc$reg_y, paste(uc$reg_x, collapse = '", "'))
      if (feffect != "") parm_str <- paste0(parm_str,
                                            sprintf(', feffects = c("%s")', paste(feffect, collapse = '", "')))
      if (cluster != "") parm_str <- paste0(parm_str,
                                            sprintf(', clusters = c("%s")', paste(cluster, collapse = '", "')))

      if (reg_by != "") parm_str <- paste0(parm_str,
                                            sprintf(', byvar = "%s"', uc$reg_by))

      parm_str <- paste0(parm_str, sprintf(', models = "%s"', uc$model))

      nb_code <- c(nb_code,
                   sprintf('t <- prepare_regression_table(%s)', parm_str),
                   "HTML(t$table)",
                   " ", "```", " ", " ")

      nb_code
    })


    if(comp == "end_note") return({
      nb_code <- c(
        "### Note", " ",
        "This Notebook has been automatically generated using the [ExPanDaR](https://joachim-gassen.github.io/ExPanDaR) package.", " "
        )
    })

    return("")
  }


  output$nb_download <- downloadHandler(
    filename = "ExPanD_nb.zip",
    content = function(file) {
      nb <- c(
        "---",
        sprintf("title: %s", shiny_title),
        "output: html_notebook",
        "---", " ", " "
      )

      if (!is.null(shiny_abstract)) {
        nb <- c(nb,
          "### Abstract",
          shiny_abstract, " ", " "
        )
      }

      nb_blocks <- c("setup", "create_sample",
                     names(shiny_components)[!names(shiny_components) %in%
                                               c("sample_selection", "subset_factor",
                                                 "grouping", "udvars")],
                     "end_note")
      pos_html_blocks <- 0

      for (blk in nb_blocks) {
        if (blk == "html_block") {
          pos_html_blocks <- pos_html_blocks + 1
          nb <- c(nb, shiny_html_blocks[pos_html_blocks], rep(" ", 2))
        } else {
          comp_code <- create_nb_code_for_component(blk)
          if(length(comp_code) <= 1) {
            stop(sprintf('Encountered unknown notebook block: "%s"', blk))
          } else nb <- c(nb, comp_code)
        }
      }

      write(nb, file = "ExPanD_nb_code.Rmd", sep = "\n")

      nb_df <- create_ca_sample()
      nb_df_def <- cas_definition
      if (length(uc$udvars) != 0) {
        nb_df <- cbind(nb_df, udv_sample)
        nb_df_def <- rbind(nb_df_def, udv_definition)
      }
      nb_df_def <- nb_df_def[!nb_df_def$var_name %in% uc$delvars,]
      nb_df <- nb_df[, as.character(nb_df_def$var_name)]

      save(nb_df, nb_df_def, file = "ExPanD_nb_data.Rdata")
      zip(file, files = c("ExPanD_nb_code.Rmd", "ExPanD_nb_data.Rdata"))
    }
  )
}
