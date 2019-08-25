library(shinycssloaders)

load("shiny_data.Rda")

server_side_data <- !is.null(shiny_df)
simple_call_mode <- server_side_data & is.null(shiny_var_def)

if (!server_side_data) shiny_abstract <- paste("Welcome to ExPanD! To start exploring panel data, please upload a panel data file.",
                                               "The data needs to be in long format with at least two numerical variables and without duplicate observations.",
                                               "Currently supported formats are Excel, CSV, RData, RDS, STATA and SAS.")


expand_header <- list(
  # this provides css information for the regression table to assure proper spacing
  tags$head(
    tags$style(HTML(
      "
      #regression table > thead > tr > th,
      #regression table > tbody > tr > th,
      #regression table > tfoot > tr > th,
      #regression table > thead > tr > td,
      #regression table > tbody > tr > td,
      #regression table > tfoot > tr > td {
      padding:0px 5px;
      }")),
    tags$script(src = "message-handler.js")
  ),

  titlePanel(shiny_title),
  if(!is.null(shiny_abstract)) {
    fluidRow(
      column (12,
              HTML(shiny_abstract),
              p(),
              hr()
      )
    )
  }
)

udv_row <- function() {
  list(fluidRow(column(6, uiOutput("ui_udv_name")),
                column(6, uiOutput("ui_udv_def"))),
       uiOutput("ui_separator15"))
}

ll <- length(shiny_components)
if (simple_call_mode) expand_components <- vector("list", ll) else expand_components <- vector("list", ll + 1)
lpos <- 1
html_block_pos <- 1
for (i in 1:ll) {
  if (names(shiny_components[i]) == "udvars") {
    expand_components[[lpos]] <- list(fluidRow(
      column(6, uiOutput("ui_udv_name")),
      column(6, uiOutput("ui_udv_def"))),
      uiOutput("ui_separator15"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "html_block") {
    expand_components[[lpos]] <- list(fluidRow(
      HTML(shiny_html_blocks[html_block_pos])
    ),
    uiOutput(paste0("ui_separator", 18 + html_block_pos)))

    html_block_pos <- html_block_pos + 1
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "sample_selection") {
    if(!server_side_data) {
      expand_components[[lpos]] <- list(fluidRow(
        column (4, uiOutput("ui_sample")),
        column (4, uiOutput("ui_select_ids")),
        column (4, uiOutput("ui_balanced_panel"))),
        uiOutput("ui_separator1"))
    } else {
      expand_components[[lpos]] <- list(fluidRow(
        column (6, uiOutput("ui_sample")),
        column (6, uiOutput("ui_balanced_panel"))),
        uiOutput("ui_separator1"))
    }
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "subset_factor") {
    expand_components[[lpos]] <- list(fluidRow(
      column (6, uiOutput("ui_subset_factor")),
      column (6, uiOutput("ui_subset_value"))
    ),
    uiOutput("ui_separator2"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "grouping") {
    expand_components[[lpos]] <- list(fluidRow(
      column (6, uiOutput("ui_group_factor")),
      column (6, uiOutput("ui_outlier_treatment"))
    ),
    uiOutput("ui_separator17"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "bar_chart") {
    expand_components[[lpos]] <- list(fluidRow(
      column (2,
              uiOutput("ui_bar_chart")
      ),
      column (10, withSpinner(plotOutput("bar_chart")))
    ),

    uiOutput("ui_separator3"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "missing_values") {
    expand_components[[lpos]] <- list(fluidRow(
      column (2,
              uiOutput("ui_missing_values")
      ),
      column (10, withSpinner(plotOutput("missing_values")))
    ),

    uiOutput("ui_separator4"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "descriptive_table") {
    expand_components[[lpos]] <- list(fluidRow(
      column(2, uiOutput("ui_descriptive_table_left")),
      column(10, align="center", uiOutput("ui_descriptive_table_right"))
    ),

    uiOutput("ui_separator5"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "histogram") {
    expand_components[[lpos]] <- list(fluidRow(
      column(2, uiOutput("ui_histogram")),
      column(10, withSpinner(plotOutput("histogram")))
    ),

    uiOutput("ui_separator6"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "ext_obs") {
    expand_components[[lpos]] <- list(fluidRow(
      column(2, uiOutput("ui_ext_obs")),
      column(10, align="center", tableOutput("ext_obs"))
    ),

    uiOutput("ui_separator7"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "by_group_bar_graph") {
    expand_components[[lpos]] <- list(fluidRow(
      column(2, uiOutput("ui_by_group_bar_graph")),
      column(10,
             div(
               style = "position:relative",
               uiOutput("by_group_bar_graph.ui", height="100%"))
      )
    ),

    uiOutput("ui_separator8"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "by_group_violin_graph") {
    expand_components[[lpos]] <- list(fluidRow(
      column(2, uiOutput("ui_by_group_violin_graph")),
      column(10,
             div(
               style = "position:relative",
               uiOutput("by_group_violin_graph.ui", height="100%"))
      )
    ),

    uiOutput("ui_separator9"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "trend_graph") {
    expand_components[[lpos]] <- list(fluidRow(
      column(2, uiOutput("ui_trend_graph")),
      column(10, withSpinner(plotOutput("trend_graph")))
    ),

    uiOutput("ui_separator10"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "quantile_trend_graph") {
    expand_components[[lpos]] <- list(fluidRow(
      column(2, uiOutput("ui_quantile_trend_graph")),
      column(10, withSpinner(plotOutput("quantile_trend_graph", height="600px")))
    ),

    uiOutput("ui_separator11"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "corrplot") {
    expand_components[[lpos]] <- list(fluidRow(
      column(2,uiOutput("ui_corrplot")),
      column(10,
             div(
               style = "position:relative",
               uiOutput("corrplot.ui", height="100%"),
               uiOutput("corrplot_hover_info")
             ))
    ),

    uiOutput("ui_separator12"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "scatter_plot") {
    expand_components[[lpos]] <- list(fluidRow(
      column(2, uiOutput("ui_scatter_plot")),
      column(10,
             div(
               style = "position:relative",
               withSpinner(plotOutput("scatter_plot",
                                      hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                                      height="600px")),
               uiOutput("hover_info")
             ))
    ),

    uiOutput("ui_separator13"))
    lpos <- lpos + 1
  }

  if(names(shiny_components[i]) == "regression") {
    expand_components[[lpos]] <- list(fluidRow(
      column(2,
             uiOutput("ui_regression"),
             uiOutput("ui_separator14"),
             uiOutput("ui_clustering"),
             uiOutput("ui_separator18"),
             uiOutput("ui_model")),
      column(10, align="center", htmlOutput("regression"))
    ),
    uiOutput("ui_separator16"))
    lpos <- lpos + 1
  }
}

expand_footer <- list(
  fluidRow(
    column(6, align="center",
           downloadButton('download', 'Save Settings'),
           helpText("Click here to save ExPanD settings to your local environment.")
           ),
    column(6, align="center",
          fileInput('upload', ''),
          helpText("Select RDS file to load locally stored settings.")
          )
  ),

  hr(),

  fluidRow(
    column(12, align="center",
           HTML("ExPanD based on <a href=https://joachim-gassen.github.io/ExPanDaR>ExPanDaR</a>, Joachim Gassen, Humboldt-Universität zu Berlin, 2019<p>")
    )
  )
)

if(shiny_export_nb_option) {
  expand_footer <- c(
    expand_footer[1:2],
    list(
      fluidRow(
        column(12, align="center",
               downloadButton('nb_download', 'Export Data and Notebook Code'),
               helpText("Click here to export a zip file containing data and R notebook code to continue the analysis in R")
        )
      ),
      hr()
    ),
    expand_footer[3]
  )
}

do.call(fluidPage, c(expand_header,
                     expand_components,
                     expand_footer))

