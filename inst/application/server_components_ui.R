# This file contains the user interface code for all components.

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
  vars <- upload_variable$var_name
  names(vars) <- upload_variable$var_name
  vars <- vars[!vars %in% c("cs_id", "ts_id")]
  tagList(
    selectInput(
      "cs_id", "Select cross sectional identifier(s)",
      c("Row names" = "cs_id", vars), multiple = TRUE
    ),
    helpText(
      "Select the variable(s) that (together) identif(y/ies) a",
      "cross-sectional unit. Selecting multiple variables is possible.",
      "You can use the row names of the data frame",
      "to identify cross-sectional data."
    ),
    selectInput(
      "ts_id", "Select time series identifier",
      c("", "None" = "ts_id", vars)
    ),
    helpText(
      "Select the variable that identifies a the time series.",
      "It needs to be coercible into an ordered factor.",
      "If you select 'None', then the data is treated as cross-sectional.")
  )
})

output$ui_subset_factor <- renderUI({
  req(uc$config_parsed)
  df <- create_analysis_sample()
  if (isolate(uc$subset_factor) != "Full Sample")
    avail_choices <- c(
      "Full Sample",
      sort(unique(c(lfactor$name, llogical$name, isolate(uc$subset_factor))))
    )
  else avail_choices <- c(
    "Full Sample",
    sort(unique(c(lfactor$name, llogical$name)))
  )

  tagList(selectInput("subset_factor", label = "Subset factor",
                      avail_choices,
                      selected = isolate(uc$subset_factor)),
          helpText("Indicate whether you want to study the full sample or subset to a specific factor."))
})

output$ui_subset_value <- renderUI({
  req(uc$subset_factor)
  df <- create_analysis_sample()
  if (uc$subset_factor != "Full Sample") {
    tagList(selectInput("subset_value", label = "Select subsample",
                        c("All",  sort(levels(as.factor(df[,isolate(uc$subset_factor)])))),
                        selected = isolate(uc$subset_value)),
            helpText("Identify the group that you want to limit your sample to."))
  }
})

output$ui_group_factor <- renderUI({
  req(uc$config_parsed)
  df <- create_analysis_sample()
  if (cross_sec_data()) avail_choices <- c(
    "None",  sort(unique(c(lfactor$name, llogical$name)))
  )
  else avail_choices <- c(
    "None",  sort(unique(c(lcs_id$name, lts_id$name, lfactor$name, llogical$name)))
  )
  tagList(selectInput("group_factor", label = "Group factor",
                      avail_choices,
                      selected = isolate(uc$group_factor)),
          helpText("Select a factor for subsetting specific analyses to."))

})

output$ui_outlier_treatment <- renderUI({
  req(uc$config_parsed)
  df <- create_analysis_sample()
  if (cross_sec_data()) avail_choices <- c(
    "None",  sort(unique(c(lfactor$name, llogical$name)))
  )
  else avail_choices <- c(
    "None",  sort(unique(c(lcs_id$name, lts_id$name, lfactor$name, llogical$name)))
  )
  tagList(radioButtons("outlier_treatment", "Outlier treatment",
                       choices = list("No treatment" = 1, "Winsorization 1%/99%" = 2, "Winsorization 5%/95%" = 3,
                                      "Truncation 1%/99%" = 4, "Truncation 5%/95%" = 5),
                       selected = uc$outlier_treatment),
          selectInput("outlier_factor", label = "By group factor",
                      avail_choices,
                      selected = isolate(uc$outlier_factor)),
          helpText("Indicate whether you want no outlier treatment",
                   "or whether you want outliers to be winsorized",
                   "to the given percentile or truncated if they exceed the given percentile.",
                   "Give a by group if you want outlier treatment to be done independently by group."))
})

output$ui_balanced_panel <- renderUI({
  req(uc$config_parsed)
  if (!cross_sec_data()) {
    # Here I removed the isolate wrapper a while ago but now I do not longer understand why. Need to check this.
    tagList(checkboxInput("balanced_panel", "Balanced panel", value = uc$balanced_panel),
            helpText("Check if you want only observations included that have data for all periods."))
  }
})

output$ui_bar_chart <- renderUI({
  req(uc$config_parsed)
  df <- create_analysis_sample()
  if (cross_sec_data()) suitable_vars <- unique(c(lfactor$name, llogical$name))
  else suitable_vars <- unique(c(lts_id$name, lfactor$name, llogical$name))
  mytags <- list(h3("Bar Chart"),
                 selectInput("bar_chart_var1", label = "Select factor to display",
                             suitable_vars,
                             selected = isolate(uc$bar_chart_var1)),
                 selectInput("bar_chart_var2", label = "Select additional factor to display",
                             c("None", suitable_vars),
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
  req(uc$config_parsed)
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
  req(uc$config_parsed)
  tagList(textInput('udv_name', "Enter name for your additional variable",""),
          helpText("If you want to create additional variables for the analysis,",
                   "provide a name (must not be taken) and a definition here.",
                   "A definition can consist of the base set variables,",
                   "parentheses and the operators",
                   "'+', '-', '*', '/', '==', '&', '|', '<', '>', '!', 'is.na()', '^', 'exp()', 'log()', 'lead()' and 'lag()'."))
})

output$ui_udv_def <- renderUI({
  req(uc$config_parsed)
  tagList(textInput('udv_definition', "Enter definition for your additional variable",""),
          actionButton("udv_submit","Submit"))
})

output$ui_descriptive_table_left <- renderUI({
  req(uc$config_parsed)
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
  req(uc$config_parsed)
  if (!simple_call_mode) {
    tabsetPanel(type = "tabs",
                tabPanel("Analysis Set", DT::dataTableOutput("descriptive_table_analysis")),
                tabPanel("Base Set", DT::dataTableOutput("descriptive_table_base")))
  } else {
    DT::dataTableOutput("descriptive_table_analysis")
  }
})

output$ui_histogram <- renderUI({
  req(uc$config_parsed)
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
  req(uc$config_parsed)
  df <- create_analysis_sample()
  mytags <- list(h3("Extreme Observations"),
                 selectInput("ext_obs_var", label = "Select variable to sort data by",
                             c(lnumeric$name, llogical$name),
                             selected = isolate(uc$ext_obs_var)))
  if (uc$group_factor != "None")
    mytags <- append(mytags, list(selectInput("ext_obs_group_by", label = "Select group to subset to",
                                              c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                              selected = isolate(uc$ext_obs_group_by))))
  if (!cross_sec_data()) mytags <- append(
    mytags, list(selectInput("ext_obs_period_by", label = "Select period to subset to",
                             c("All", levels(df[,lts_id$col])),
                             selected = isolate(uc$ext_obs_period_by)))
  )
  tagList(mytags)
})

output$ui_by_group_bar_graph <- renderUI({
  req(uc$config_parsed)
  df <- create_analysis_sample()
  if (cross_sec_data()) suitable_vars <- unique(c(lfactor$name, llogical$name))
  else suitable_vars <- unique(c(lts_id$name, lfactor$name, llogical$name))
  mytags <- list(h3("By Group Bar Chart"),
                 selectInput("bgbg_var", label = "Select variable to display",
                             c(lnumeric$name, llogical$name),
                             selected = isolate(uc$bgbg_var)),
                 selectInput("bgbg_byvar", label = "Select variable to group by",
                             suitable_vars,
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
  req(uc$config_parsed)
  df <- create_analysis_sample()
  if (cross_sec_data()) suitable_vars <- unique(c(lfactor$name, llogical$name))
  else suitable_vars <- unique(c(lts_id$name, lfactor$name, llogical$name))
  mytags <- list(h3("By Group Violin Chart"),
                 selectInput("bgvg_var", label = "Select variable to display",
                             c(lnumeric$name, llogical$name),
                             selected = isolate(uc$bgvg_var)),
                 selectInput("bgvg_byvar", label = "Select variable to group by",
                             suitable_vars,
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
  req(uc$config_parsed)
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
  req(uc$config_parsed)
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

output$ui_by_group_trend_graph <- renderUI({
  req(uc$config_parsed)
  df <- create_analysis_sample()
  suitable_vars <- unique(c(lfactor$name, llogical$name))
  suitable_vars <- suitable_vars[suitable_vars != lts_id$name]
  mytags <- list(
    h3("By Group Time Trend Graph"),
    selectInput("bgtg_var", label = "Select variable to display",
                c(lnumeric$name, llogical$name),
                selected = isolate(uc$bgtg_var)),
    selectInput("bgtg_byvar", label = "Select variable to group by",
                suitable_vars,
                selected = isolate(uc$bgtg_byvar))
  )
  if (uc$group_factor != "None")
    mytags <- append(mytags, list(hr(),
                                  selectInput("bgtg_group_by", label = "Select group to subset to",
                                              c("All", sort(levels(as.factor(df[,uc$group_factor])))),
                                              selected = isolate(uc$bgtg_group_by))))
  tagList(mytags)
})

output$ui_corrplot <- renderUI({
  req(uc$config_parsed)
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
  req(uc$config_parsed)
  df <- create_analysis_sample()
  if (cross_sec_data())
    suitable_vars <- names(df)[which(!names(df) %in%
                                       unique(c(lts_id$name, lcs_id$name)))]
  else suitable_vars <- names(df)[which(!names(df) %in% lcs_id$name)]
  mytags <- list(
    h3("Scatter Plot"),
    selectInput("scatter_x", label = "Select the x variable to display",
                unique(c(lnumeric$name, l2level$name)),
                selected = isolate(uc$scatter_x)),
    selectInput("scatter_y", label = "Select the y variable to display",
                unique(c(lnumeric$name, l2level$name)),
                selected = isolate(uc$scatter_y)),
    selectInput("scatter_size", label = "Select the variable to be reflected by dot size",
                unique(c("None", lnumeric$name, l2level$name)),
                selected = isolate(uc$scatter_size)),
    selectInput("scatter_color", label = "Select the variable to be reflected by color",
                unique(c("None", suitable_vars)),
                selected = isolate(uc$scatter_color)),
    checkboxInput("scatter_sample",
                  label = "Sample 1,000 observations to display if number of observations is higher",
                  value = isolate(uc$scatter_sample)),
    checkboxInput("scatter_loess",
                  label="Display smoother",
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
  req(uc$config_parsed)
  df <- create_analysis_sample()
  if (cross_sec_data()) suitable_vars <- unique(c("None", lfactor$name))
  else suitable_vars <- unique(c("None", lcs_id$name, lts_id$name, lfactor$name))
  tagList(
    h3("Regression Analysis"),
    selectInput("reg_y", label = "Select the dependent variable",
                unique(c(lnumeric$name, l2level$name)),
                selected = isolate(uc$reg_y)),
    selectInput("reg_x", label = "Select independent variable(s)",
                unique(c(lnumeric$name, l2level$name)), multiple=TRUE,
                selected = isolate(uc$reg_x)),
    hr(),
    selectInput("reg_fe1", label = "Select a categorial variable as the first fixed effect",
                suitable_vars,
                selected = isolate(uc$reg_fe1)),
    selectInput("reg_fe2", label = "Select a categorial variable as the second fixed effect",
                suitable_vars,
                selected = isolate(uc$reg_fe2)),
    hr(),
    selectInput("reg_by", label = "Select a categorial variable to subset the estimated models on",
                suitable_vars[which(!suitable_vars %in% lcs_id$name)],
                selected = isolate(uc$reg_by))
  )
})


output$ui_model <- renderUI({
  req(uc$config_parsed)
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
