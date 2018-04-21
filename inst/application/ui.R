library(shinycssloaders)

load("shiny_data.Rda")

server_side_data <- !is.null(shiny_df)
simple_call_mode <- server_side_data & is.null(shiny_var_def)

if (!server_side_data) shiny_abstract <- "Welcome to ExPanD! To start exploring panel data, please upload a panel data file. Currently supported formats are Excel, CSV, RData, RDS, STATA and SAS."
fluidPage(
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
      }"))),
  titlePanel(shiny_title),
  if(!is.null(shiny_abstract)) {
    fluidRow(
      column (12,
              HTML(shiny_abstract),
              p(),
              hr()
      )
    )},

  fluidRow(
    column (12,
            p("Based on the ExPanD app of the",
              HTML("<a href=https://joachim-gassen.github.io/ExPanDaR>ExPanDaR R package</a>"),
              "developed by Joachim Gassen, Humboldt-Universität zu Berlin,",
              HTML("<a href=\"mailto:gassen@wiwi.hu-berlin.de\">gassen@wiwi.hu-berlin.de</a>.")),
            singleton(
              tags$head(tags$script(src = "message-handler.js"))
            ),
            hr()
    )
  ),

  if(!server_side_data) {
    fluidRow(
      column (4, uiOutput("ui_sample")),
      column (4, uiOutput("ui_select_ids")),
      column (4, uiOutput("ui_balanced_panel"))
    ) } else fluidRow(
    column (6, uiOutput("ui_sample")),
    column (6, uiOutput("ui_balanced_panel"))
  ),

  hr(),

  fluidRow(
    column (6, uiOutput("ui_subset_factor")),
    column (6, uiOutput("ui_subset_value"))
  ),

  hr(),

  fluidRow(
    column (6, uiOutput("ui_group_factor")),
    column (6, uiOutput("ui_outlier_treatment"))
  ),

  hr(),
  if(shiny_components["bar_chart"]) {
    list(fluidRow(
      column (2,
              uiOutput("ui_bar_chart")
      ),
      column (10, withSpinner(plotOutput("bar_chart")))
    ),

    hr())
  },

  if(shiny_components["missing_values"]) {
    list(fluidRow(
      column (2,
              uiOutput("ui_missing_values")
      ),
      column (10, withSpinner(plotOutput("missing_values")))
    ),

    hr())
  },

  if(!simple_call_mode) {
  fluidRow(
    column (6,textInput('udv_name', "Enter name for your additional variable",""),
            helpText("If you want to create additional variables for the analysis,",
                     "provide a name (must not be taken) and a definition here.",
                     "A definition can consist of the base set variables,",
                     "parentheses and the operators",
                     "'+', '-', '*', '/', '==', '&', '|', '<', '>', '^', 'exp()', 'log()', 'lead()' and 'lag()'.")),
    column (6,textInput('udv_definition', "Enter definition for your additional variable",""),
            actionButton("udv_submit","Submit"))
  )},

  if(!simple_call_mode) hr(),

  if(shiny_components["descriptive_table"]) {
    list(fluidRow(
      column(2,
             uiOutput("ui_descriptive_table"),
             hr()),
      if (!simple_call_mode) {
        column(10, align="center", tabsetPanel(type = "tabs",
                                               tabPanel("Analysis Set", DT::dataTableOutput("descriptive_table_analysis")),
                                               tabPanel("Base Set", DT::dataTableOutput("descriptive_table_base"))))
      } else {
        column(10, align="center", DT::dataTableOutput("descriptive_table_analysis"))
      }
    ),

    hr())
  },

  if(shiny_components["by_group_bar_graph"]) {
    list(fluidRow(
      column(2, uiOutput("ui_by_group_bar_graph")),
      column(10,
             div(
               style = "position:relative",
               uiOutput("by_group_bar_graph.ui", height="100%"))
             )
    ),

    hr())
  },

  if(shiny_components["histogram"]) {
    list(fluidRow(
      column(2, uiOutput("ui_histogram")),
      column(10, withSpinner(plotOutput("histogram")))
    ),

    hr())
  },

  if(shiny_components["ext_obs"]) {
    list(fluidRow(
      column(2, uiOutput("ui_ext_obs")),
      column(10, align="center", tableOutput("ext_obs"))
    ),

    hr())
  },

  if(shiny_components["trend_graph"]) {
    list(fluidRow(
      column(2, uiOutput("ui_trend_graph")),
      column(10, withSpinner(plotOutput("trend_graph")))
    ),

    hr())
  },

  if(shiny_components["quantile_trend_graph"]) {
    list(fluidRow(
      column(2, uiOutput("ui_quantile_trend_graph")),
      column(10, withSpinner(plotOutput("quantile_trend_graph", height="600px")))
    ),

    hr())
  },

  if(shiny_components["corrplot"]) {
    list(fluidRow(
      column(2,uiOutput("ui_corrplot")),
      column(10,
             div(
               style = "position:relative",
               uiOutput("corrplot.ui", height="100%"),
               uiOutput("corrplot_hover_info")
             ))
    ),

    hr())
  },

  if(shiny_components["scatter_plot"]) {
    list(fluidRow(
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

    hr())
  },

  if(shiny_components["regression"]) {
    list(fluidRow(
      column(2,
             uiOutput("ui_regression"),
             hr(),
             uiOutput("ui_clustering"),
             helpText("Indicatate how you want your standard errors to be estimated")),
      column(10, align="center", htmlOutput("regression"))
    ),

    hr())
  },

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
           HTML("ExPanD based on <a href=https://joachim-gassen.github.io/ExPanDaR>ExPanDaR</a>, Joachim Gassen, Humboldt-Universität zu Berlin, 2018<p>")
           )
    )
)
