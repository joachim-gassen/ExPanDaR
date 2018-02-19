library(ExPanDaR)

DEBUG <- TRUE
store_encrypted <- FALSE
store_key <- PKI::PKI.digest(charToRaw("What a wonderful key"), "SHA256")

if (DEBUG) sample_count <- 0

`%>%` <- dplyr::`%>%`

quote_escape <- function(string) {
  t <- gsub("\"", "&#34;", string)
  t <- gsub("\'", "&#39;", t)
  t
}

load("shiny_data.Rda")

if (exists("shiny_df") && is.data.frame(shiny_df))
  simple_call_mode <- TRUE else simple_call_mode <- FALSE

if (simple_call_mode) {
  data_source <- data.frame(ds_id = shiny_df_name,
                            ds_description = "User defined data set",
                            stringsAsFactors = FALSE)
  ca_sample <- data.frame(ds_id = shiny_df_name, shiny_df)
  ca_variable <- data.frame(
    var_name = names(shiny_df),
    var_def = Hmisc::label(shiny_df),
    stringsAsFactors = FALSE
  )

  ca_variable$type <- NA
  ca_variable$type[which(sapply(shiny_df, is.factor))] <- "factor"
  ca_variable$type[which(sapply(shiny_df, is.logical))] <- "logical"
  ca_variable$type[which(sapply(shiny_df, is.numeric))] <- "numeric"
  ca_variable$type[which(ca_variable$var_name %in% shiny_cs_id)] <- "cs_id"
  ca_variable$type[which(ca_variable$var_name == shiny_ts_id)] <- "ts_id"
  ca_variable$can_be_na <-
    ifelse(ca_variable$type == "cs_id" | ca_variable$type == "ts_id", 0, 1)

  if (!is.null(shiny_config_list)) app_config <- shiny_config_list
  else app_config <- list(
    sample = ca_sample$ds_id[1],
    subset_factor = "Full Sample",
    subset_value = "All",
    group_factor = "None",
    balanced_panel = FALSE,
    outlier_treatment = "1",
    outlier_factor = "None",
    udvars = NULL,
    delvars = NULL,
    bar_chart_var1 = shiny_ts_id,
    bar_chart_var2 = ca_variable$var_name[ca_variable$type == "factor"][1],
    bar_chart_group_by = "All",
    bar_chart_relative = FALSE,
    desc_group_by = "All",
    hist_var = ca_variable$var_name[ca_variable$type == "factor"][1],
    hist_group_by = "All",
    hist_nr_of_breaks = 20,
    ext_obs_var = ca_variable$var_name[ca_variable$type == "numeric"][1],
    ext_obs_group_by = "All",
    ext_obs_period_by = "All",
    trend_graph_var1 = ca_variable$var_name[ca_variable$type == "numeric"][1],
    trend_graph_var2 = "None",
    trend_graph_var3 = "None",
    trend_graph_group_by = "All",
    quantile_trend_graph_var = ca_variable$var_name[ca_variable$type == "numeric"][1],
    quantile_trend_graph_quantiles = c("0.05", "0.25", "0.50", "0.75", "0.95"),
    quantile_trend_graph_group_by = "All",
    corrplot_group_by = "All",
    scatter_x = ca_variable$var_name[ca_variable$type == "numeric"][1],
    scatter_y = ca_variable$var_name[ca_variable$type == "numeric"][2],
    scatter_size = ca_variable$var_name[ca_variable$type == "numeric"][3],
    scatter_color = ca_variable$var_name[ca_variable$type == "factor"][1],
    scatter_group_by = "All",
    scatter_loess = TRUE,
    scatter_sample = TRUE,
    reg_y = ca_variable$var_name[ca_variable$type == "numeric"][2],
    reg_x = ca_variable$var_name[ca_variable$type == "numeric"][1],
    reg_fe1 = "None",
    reg_fe2 = "None",
    reg_by = "None",
    cluster = 1
  )
}

if (!simple_call_mode) {
  stop("Sorry: Complex call modes not implemented yet.")

  # Not run: Initialize global data
  data_source <- readRDS("app_data/data_source.RDS")
  data_item <- readRDS("app_data/data_item.RDS")
  ca_variable <- readRDS("app_data/ca_variable.RDS")
  ca_sample <- readRDS("app_data/ca_sample.RDS")
  base_data <- readRDS("app_data/base_data.RDS")
  app_config <- readRDS("app_data/config.RDS")
  if (DEBUG) sample_count <<- 0
}

default_config <- list(
  sample = ca_sample$ds_id[1],
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
  desc_group_by = "All",
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
  cluster = 1
)


# Define the server for the Shiny app
function(input, output, session) {
  uc <- reactiveValues()

  check_vars <- function() {
    factor_names <- c(lfactor$name, lcs_id$name, lts_id$name, llogical$name, "None")
    numeric_names <- c(lnumeric$name, llogical$name, "None")
    if (!uc$bar_chart_var1 %in% factor_names) uc$bar_chart_var1 = factor_names[1]
    if (!uc$bar_chart_var2 %in% factor_names) uc$bar_chart_var2 = "None"
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
    if (!uc$reg_y %in% numeric_names) uc$reg_y = numeric_names[1]
    uc$reg_x <- intersect(uc$reg_x, numeric_names)
    if (length(uc$reg_x) == 0) uc$reg_x = numeric_names[2]

    if (!uc$reg_fe1 %in% factor_names) uc$reg_fe1 = "None"
    if (!uc$reg_fe2 %in% factor_names) uc$reg_fe2 = "None"
    if (!uc$reg_by %in% factor_names) uc$reg_by = "None"
    if (uc$cluster == 2 & uc$reg_fe1 == "None") uc$cluster = 1
    if (uc$cluster == 3 & uc$reg_fe2 == "None") uc$cluster = 1
    if (uc$cluster == 4 & uc$reg_fe1 != "None" & uc$reg_fe2 == "None") uc$cluster = 2
    if (uc$cluster == 4 & uc$reg_fe1 == "None" & uc$reg_fe2 != "None") uc$cluster = 3
    if (uc$cluster == 4 & uc$reg_fe1 == "None" & uc$reg_fe2 == "None") uc$cluster = 1
  }

  create_base_sample <- reactive({
    bsd <- data.frame(var_name = data_item$data_item,
                      var_def = data_item$di_description,
                      type = data_item$type,
                      can_be_na = 1)
    bs <- base_data[base_data$ds_id == uc$sample, as.character(bsd$var_name)]

    all_na_vars <- sapply(bs, function (x) all(is.na(x)))
    bs_definition <<- bsd[!all_na_vars,]
    return(bs[,as.character(bsd$var_name)])
  })

  create_ca_sample <- reactive({
    cas_definition <<- ca_variable
    if("label" %in% names(cas_definition)) {
      cas_definition$var_def <<- cas_definition$label
    }
    return(ca_sample[ca_sample$ds_id == uc$sample, as.character(cas_definition$var_name)])
  })

  save_udv <- function(udv_name, udv_def, udv_vector) {
    udv <- cbind(udv_name, udv_def)
    if (is.numeric(udv_vector)) type <- "numeric"
    else if (is.logical(udv_vector)) type <- "logical"
    else type <- "factor"
    can_be_na <- 1
    new_def <- data.frame(var_name = udv_name, var_def = udv_def, type, can_be_na, stringsAsFactors = FALSE)
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

  lead <- function(x, n = 1L) {
    df <- cbind(create_base_sample()[,c(as.character(bs_definition[bs_definition$type == "cs_id" |
                                                                     bs_definition$type == "ts_id", "var_name"]))], x)
    colnames(df)[ncol(df)] <- "xval"
    df %>% dplyr::group_by_at(vars(one_of(as.character(bs_definition[bs_definition$type == "cs_id", "var_name"])))) %>%
      dplyr::mutate(y = dplyr::lead(xval, n)) %>%
      dplyr::ungroup() %>% dplyr::pull(y)
  }

  lag <- function(x, n = 1L) {
    df <- cbind(create_base_sample()[,c(as.character(bs_definition[bs_definition$type == "cs_id" |
                                                                     bs_definition$type == "ts_id", "var_name"]))], x)
    colnames(df)[ncol(df)] <- "xval"
    df %>% dplyr::group_by_at(vars(dplyr::one_of(as.character(bs_definition[bs_definition$type == "cs_id", "var_name"])))) %>%
      dplyr::mutate(y = dplyr::lag(xval, n)) %>%
      dplyr::ungroup() %>% dplyr::pull(y)
  }

  test_udv_definition <- function(udv_definition) {
    df <- create_base_sample()
    # Prepare a sandbox environment that should be user code-safe
    myenv = new.env(parent=emptyenv())
    # Define names of R functions which are allowed for calculation
    allowedFunctions = c("(", "==", "&", "|", "+", "-", "*", "/", "<", ">", "^","exp", "log", "lag", "lead")
    # Assign the functions to the evaluation environment
    for(name in allowedFunctions){
      assign(name,match.fun(name), envir=myenv)
    }
    # And our data frame
    for(name in names(df)){
      if (is.factor(df[,name])) assign(name, as.character(df[,name]), envir=myenv)
      else assign(name, df[,name], envir=myenv)
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
        if (is.null(udv_definition)) udv_definition <<- data.frame(new_def, stringsAsFactors = FALSE)
        else udv_definition <<- rbind(udv_definition, new_def)
      }
    })
    if (!is.null(udv_definition)) colnames(udv_definition) <<- c("var_name", "var_def", "type", "can_be_na")
    if (DEBUG) message(do.call(tictoc::toc.outmsg, tictoc::toc(quiet = TRUE)))
  }

  parse_config <- function(l) {
    config <- default_config
    for (name in names(config)) {
      uc[[name]] <<- l[[name]]
    }
    if (length(isolate(uc$udvars)) != 0) create_udv_sample()
  }

  parse_config(app_config)

  create_var_categories <- function(sd) {
    for (type in c("cs_id", "ts_id", "numeric", "logical", "factor")) {
      assign(paste0("l", type), data.frame(
        col = which(sd$type == type),
        name = sd$var_name[sd$type == type],
        stringsAsFactors = FALSE
      ), envir = globalenv())
    }
  }

  create_analysis_sample <- reactive({
    # showReactLog(time = TRUE)
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
    smp <- smp[complete.cases(smp[,as.character(sample_definition$var_name[which(sample_definition$can_be_na == 0)])]),]

    # Subset if requested by user
    if ((isolate(uc$subset_factor) != "Full Sample") & (uc$subset_value != "All"))
      smp <- smp[which(smp[,isolate(uc$subset_factor)] == uc$subset_value),]

    # Balance sample if requested by user
    if (uc$balanced_panel) {
      smp <- dplyr::group_by_at(smp, dplyr::vars(dplyr::one_of(sample_definition$var_name[sample_definition$type  == "cs_id"]))) %>%
        dplyr::mutate(nobs = n())
      max_nobs <- max(smp$nobs)
      smp <- as.data.frame(dplyr::select(dplyr::filter(smp, nobs == max_nobs), -nobs))
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
    create_var_categories(sample_definition)
    isolate(check_vars())

    if (DEBUG) message(do.call(tictoc::toc.outmsg, tictoc::toc(quiet = TRUE)))
    return(droplevels(smp))
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

  observeEvent(input$restore_analysis_sample, {
    uc$udvars <<- NULL
    uc$delvars <<- NULL
  })

  observeEvent(input$sample, {
    if (req(input$sample) != uc$sample) {
      uc$sample <<- input$sample
      uc$subset_value <<- "All"
      uc$desc_group_by <<- "All"
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
  observe({if (is.logical(input$bar_chart_relative)) uc$bar_chart_relative <<- input$bar_chart_relative})
  observe({uc$desc_group_by <<- req(input$desc_group_by)})
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

  output$ui_sample <- renderUI({
    req (uc$sample)
    choices <- setNames(as.list(data_source$ds_id), data_source$ds_description)
    tagList(selectInput("sample", "Sample Selection",
                 choices,
                 selected = uc$sample),
    helpText("Select the sample that you want to study.")
    )
  })

  output$ui_subset_factor <- renderUI({
    req (uc$subset_factor)
    df <- create_analysis_sample()
    tagList(selectInput("subset_factor", label = "Subset factor",
                        c("Full Sample",  c(lfactor$name, llogical$name)),
                          selected = isolate(uc$subset_factor)),
            helpText("Indicatate whether you want to study the full sample or subset to a specific factor."))

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
                        c("None",  c(lcs_id$name, lts_id$name, lfactor$name, llogical$name)),
                        selected = isolate(uc$group_factor)),
            helpText("Select a factor for subsetting specific analyses to."))

  })

  output$ui_outlier_treatment <- renderUI({
    tagList(radioButtons("outlier_treatment", "Outlier treatment",
                         choices = list("No treatment" = 1, "Winsorization 1%/99%" = 2, "Winsorization 5%/95%" = 3,
                                        "Truncation 1%/99%" = 4, "Truncation 5%/95%" = 5),
                         selected = uc$outlier_treatment),
            selectInput("outlier_factor", label = "By group factor",
                        c("None",  c(lcs_id$name, lts_id$name, lfactor$name, llogical$name)),
                        selected = isolate(uc$outlier_factor)),
            helpText("Indicatate whether you want no outlier treatment",
                     "or whether you want outliers to be winsorized",
                     "to the given percentile or truncated if they exceed the given percentile.",
                     "Give a by group if you want outlier treatment to be done independently by group."))
  })

  output$ui_balanced_panel <- renderUI({
    # Here I removed the isolate wrapper a while ago but now I do not longer understand why. Need to check this.
    tagList(checkboxInput("balanced_panel", "Balanced panel", value = uc$balanced_panel),
            helpText("Check if you want only observations included that have data for all periods."))
  })

  output$ui_bar_chart <- renderUI({
    df <- create_analysis_sample()
    mytags <- list(h3("Bar Chart"),
                   selectInput("bar_chart_var1", label = "Select factor to display",
                               c(lts_id$name, lfactor$name, llogical$name),
                               selected = isolate(uc$bar_chart_var1)),
                   selectInput("bar_chart_var2", label = "Select additional factor to display",
                               c("None", lts_id$name, lfactor$name, llogical$name),
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

  output$ui_descriptive_table <- renderUI({
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
                  c(lnumeric$name, llogical$name),
                  selected = isolate(uc$scatter_x)),
      selectInput("scatter_y", label = "Select the y variable to display",
                  c(lnumeric$name, llogical$name),
                  selected = isolate(uc$scatter_y)),
      selectInput("scatter_size", label = "Select the variable to be reflected by dot size",
                  c("None", lnumeric$name, llogical$name),
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
                  c(lnumeric$name, llogical$name),
                  selected = isolate(uc$reg_y)),
      selectInput("reg_x", label = "Select independent variable(s)",
                  c(lnumeric$name, llogical$name), multiple=TRUE,
                  selected = isolate(uc$reg_x)),
      hr(),
      selectInput("reg_fe1", label = "Select a categorial variable as the first fixed effect",
                  c("None", lcs_id$name, lts_id$name, lfactor$name),
                  selected = isolate(uc$reg_fe1)),
      selectInput("reg_fe2", label = "Select a categorial variable as the second fixed effect",
                  c("None", lcs_id$name, lts_id$name, lfactor$name),
                  selected = isolate(uc$reg_fe2)),
      hr(),
      selectInput("reg_by", label = "Select a categorial variable to subset the estimated models on",
                  c("None", lts_id$name, lfactor$name),
                  selected = isolate(uc$reg_by))
    )
  })


  output$ui_clustering <- renderUI({
    req (uc$reg_fe1, uc$reg_fe2)
    cluster <- "1"
    if (isolate(uc$reg_fe1) != "None")  cluster <- "2"
    if (isolate(uc$reg_fe2) != "None")  cluster <- "3"
    if ((isolate(uc$reg_fe1) != "None") & (isolate(uc$reg_fe2) != "None")) cluster <- "4"
    uc$cluster <- min(uc$cluster, cluster)
    switch(cluster,
           "1"= radioButtons("cluster", "Calculation of Standard Errors",
                             choices = list("Standard OLS" = 1), selected = isolate(uc$cluster)),
           "2"= radioButtons("cluster", "Calculation of Standard Errors",
                             choices = list("Standard OLS" = 1, "Clustering for the first fixed effect" = 2), selected = isolate(uc$cluster)),
           "3"= radioButtons("cluster", "Calculation of Standard Errors",
                             choices = list("Standard OLS" = 1, "Clustering for the second fixed effect" = 3), selected = isolate(uc$cluster)),
           "4"= radioButtons("cluster", "Calculation of Standard Errors",
                             choices = list("Standard OLS" = 1, "Clustering for the first fixed effect" = 2,
                                            "Clustering for the second fixed effect" = 3,
                                            "Two-way clustering" = 4), selected = isolate(uc$cluster)))
  })

  output$bar_chart <- renderPlot({
    req(uc$bar_chart_var1, uc$bar_chart_var2, uc$bar_chart_group_by)
    df <- create_analysis_sample()
    if (uc$bar_chart_group_by != "All")
      df <- df[df[, uc$group_factor] == uc$bar_chart_group_by,]
    if (uc$bar_chart_var2 != "None" & (!uc$bar_chart_relative))
      ggplot2::ggplot(df, ggplot2::aes(df[,uc$bar_chart_var1])) +
      ggplot2::geom_bar(ggplot2::aes(fill=df[,uc$bar_chart_var2]), position = "stack") +
      ggplot2::labs(x = uc$bar_chart_var1, fill = uc$bar_chart_var2)
    else if (uc$bar_chart_var2 != "None")
      ggplot2::ggplot(df, ggplot2::aes(df[,uc$bar_chart_var1])) +
      ggplot2::geom_bar(ggplot2::aes(fill=df[,uc$bar_chart_var2]), position = "fill") +
      ggplot2::labs(x = uc$bar_chart_var1, fill = uc$bar_chart_var2, y = "Percent") +
      ggplot2::scale_y_continuous(labels = scales::percent_format())
    else ggplot2::ggplot(df, ggplot2::aes(df[,uc$bar_chart_var1])) +
      ggplot2::geom_bar() + ggplot2::labs(x = uc$bar_chart_var1)
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
                      $(firstColumn[i]).attr('title', tips[i]);
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
                      $(firstColumn[i]).attr('title', tips[i]);
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
      df <- df[, vars]
      if (req(uc$ext_obs_period_by) != "All")
        df <- df[df[, lts_id$name] == uc$ext_obs_period_by,]
      if (req(uc$ext_obs_group_by) != "All")
        df <- df[df[, uc$group_factor] == uc$ext_obs_group_by, vars]
      df <- droplevels(df[complete.cases(df),])
      prepare_ext_obs_table(df)$kable_ret %>%
        kableExtra::kable_styling()
    }
  )

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
    isolate(plotOutput("corrplot", hover = hoverOpts("corrplot_hover", delay = 100, delayType = "debounce"),
               height=max(min(length(c(lnumeric$col, llogical$col))*50,1000),300),
               width=max(min(length(c(lnumeric$col, llogical$col))*50,1000),300)))
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
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

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
    if (uc$scatter_sample & (nrow(scatter_df) > 1000)) scatter_df <- dplyr::sample_n(scatter_df, 1000)
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
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style, class = "well-sm",
      htmltools::HTML(paste0(unlist(point[,lcs_id$name]), collaspe=" "),
                  "<br>",
                  as.character(point[,lts_id$name]))
    )
  })

  output$regression <- renderPrint({
    if (DEBUG) tictoc::tic("estimating regressions")
    req(uc$reg_y, uc$reg_x, uc$reg_fe1, uc$reg_fe2, uc$reg_by)
    varlist <- c(uc$reg_y, uc$reg_x, uc$reg_fe1, uc$reg_fe2, uc$reg_by)
    varlist <- varlist[!varlist %in% "None"]
    df <- create_analysis_sample()[,varlist]
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
    t <- prepare_regression_table(df, uc$reg_y, uc$reg_x, feffect, cluster, reg_by)
    if (DEBUG) message(do.call(tictoc::toc.outmsg, tictoc::toc(quiet = TRUE)))
    htmltools::HTML(t$table)
  })

  observe({
    in_file <- req(input$upload)
    if (!is.null(in_file)) {
      key <- PKI.digest(charToRaw("What a wonderful key"), "SHA256")
      tryCatch(
        {
          if (store_encrypted) {
            encrypted <- readRDS(in_file$datapath)
            decrypted <- PKI.decrypt(encrypted, key, "aes256")
            config_list <- unserialize(decrypted)
          } else config_list <- readRDS(in_file$datapath)
          isolate(parse_config(config_list))
        }, error = function(cond) {
          session$sendCustomMessage(type = 'testmessage', message = paste("Unable to read", in_file$datapath))
        })
    }
  })

  output$download <- downloadHandler(
    filename = function() { "ExPanD.RDS" },
    content = function(file) {
      if (store_encrypted) {
        raw <- serialize(reactiveValuesToList(uc), NULL)
        encrypted <- PKI.encrypt(raw, key, "aes256")
        saveRDS(encrypted, file)
      } else saveRDS(reactiveValuesToList(uc), file)
    }
  )
}
