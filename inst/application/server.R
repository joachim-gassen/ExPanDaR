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

if (!server_side_data) shiny_abstract <- paste(
  "Welcome to ExPanD! To start exploring your data, please upload a data file",
  "containing at least two numerical variables. For panel data, the data needs",
  "to be in long format and without duplicate observations.",
  "Currently supported formats are Excel, CSV, RData, RDS, STATA and SAS."
)

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

check_whether_data_is_valid <- function(v) {
  if (length(which(v$type == "numeric")) < 2) {
    if (DEBUG) warning("Less than two numerical variables in data")
    session$sendCustomMessage(type = 'testmessage',
                              message = paste0('Your data contains less than two numerical variables. At least two are required.'))
    return(FALSE)
  }
  return(TRUE)
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
  cross_sec_data <- reactiveVal(shiny_cs_data)

  source("server_components_ui.R", local = TRUE)
  source("server_components_displays.R", local = TRUE)
  source("server_create_notebook.R", local = TRUE)
  source("server_dynamic_ui.R", local = TRUE)
  source("server_utility_functions.R", local = TRUE)

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


  # --- Reactive Functions -----------------------------------------------------

  components <- reactive({
    if (server_side_data) shiny_components
    else {
      if (cross_sec_data())
        shiny_components[!names(shiny_components) %in%
                           c("missing_values", "trend_graph",
                             "quantile_trend_graph")]
      else {
        if(!is.null(ca_variable)) shiny_components
        else c(sample_selection = TRUE)
      }
    }
  })

  create_base_sample <- reactive({
    req(isolate(uc$subset_factor))
    bsd <- data.frame(base_variable,
                      can_be_na = TRUE)
    bs <- base_data[base_data$ds_id == uc$sample, as.character(bsd$var_name)]

    all_na_vars <- sapply(bs, function (x) all(is.na(x)))
    bs_definition <<- bsd[!all_na_vars,]
    return(bs[,as.character(bsd$var_name)])
  })

  create_ca_sample <- reactive({
    req(isolate(uc$subset_factor))
    cas_definition <<- ca_variable[ca_variable$ds_id == uc$sample, -1]
    smp <- ca_sample[ca_sample$ds_id == uc$sample, as.character(cas_definition$var_name)]
    smp[, cas_definition$var_name[cas_definition$type == "ts_id"]] <-
      as.ordered(smp[, cas_definition$var_name[cas_definition$type == "ts_id"]])
    return(smp)
  })

  create_analysis_sample <- reactive({
    req(isolate(uc$subset_factor))
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
    isolate(check_vars(cross_sec_data()))

    if (DEBUG) current_as <<- smp
    if (DEBUG) current_sd <<- sample_definition

    return(smp)
  })


  # --- Observer Fuctions ------------------------------------------------------

  observe(parse_config(app_config))

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
    if (any(c("cs_id", "ts_id") %in% names(shiny_df))) {
      warning("'cs_id' or _ts_id' prensent in names(df). Informing user.")
      session$sendCustomMessage(
        type = 'testmessage',
        message = sprintf(
          paste("File %s data frame contains internally used variable names",
                "'cs_id' or 'ts_id'. Please rename variable names prior to use."),
          input_file$name)
      )
      return(NULL)
    }

    shiny_df$cs_id <- row.names(shiny_df)
    shiny_df$ts_id <- 1

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
      if (cs_id == "cs_id" && ts_id == "ts_id") {
        cross_sec_data(TRUE)
      }

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
      if (uc$subset_value != "All") uc$subset_value <<- "All"
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
}
