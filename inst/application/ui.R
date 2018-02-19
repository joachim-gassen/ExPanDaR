load("shiny_data.Rda")

if (exists("shiny_df") && is.data.frame(shiny_df))
  simple_call_mode <- TRUE else simple_call_mode <- FALSE

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
  titlePanel("ExPanD - Explore panel data interactively"),

  fluidRow(
    column (12,
            p("This is a web app to explore panel data.",
              "Developed by Martin Bierey and Joachim Gassen, Humboldt-Universität zu Berlin,",
              HTML("<a href=\"mailto:gassen@wiwi.hu-berlin.de\">gassen@wiwi.hu-berlin.de</a>.")),
            hr()
    )
  ),

  fluidRow(
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

  fluidRow(
    column (2,
            uiOutput("ui_bar_chart")
    ),
    column (10, plotOutput("bar_chart"))
  ),

  hr(),
  if(!simple_call_mode) {
  fluidRow(
    column (6,textInput('udv_name', "Enter name for your additional variable",""),
            helpText("If you want to create additional variables for the analysis,",
                     "provide a name (must not be taken) and a definition here.",
                     "A definition can consist of the base set variables,",
                     "parentheses and the operators",
                     "'+', '-', '*', '/', '==', '&', '|', '<', '>', '^', 'exp()', 'log()', 'lead()' and 'lag()'.")),
    column (6,textInput('udv_definition', "Enter definition for your additional variable",""),
            actionButton("udv_submit","Submit")),
    singleton(
      tags$head(tags$script(src = "message-handler.js"))
    )
  )},

  if(!simple_call_mode) hr(),

  fluidRow(
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

  hr(),

  fluidRow(
    column(2, uiOutput("ui_histogram")),
    column(10, plotOutput("histogram"))
  ),

  hr(),

  fluidRow(
    column(2, uiOutput("ui_ext_obs")),
    column(10, align="center", tableOutput("ext_obs"))
  ),

  hr(),

  fluidRow(
    column(2, uiOutput("ui_trend_graph")),
    column(10, plotOutput("trend_graph"))
  ),

  hr(),

  fluidRow(
    column(2, uiOutput("ui_quantile_trend_graph")),
    column(10, plotOutput("quantile_trend_graph", height="600px"))
  ),

  hr(),

  fluidRow(
    column(2,uiOutput("ui_corrplot")),
    column(10,
           div(
             style = "position:relative",
             uiOutput("corrplot.ui", height="100%"),
             uiOutput("corrplot_hover_info")
           ))
  ),

  hr(),

  fluidRow(
    column(2, uiOutput("ui_scatter_plot")),
    column(10,
           div(
             style = "position:relative",
             plotOutput("scatter_plot",
                        hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                        height="600px"),
             uiOutput("hover_info")
           ))
  ),

  hr(),

  fluidRow(
    column(2,
           uiOutput("ui_regression"),
           hr(),
           uiOutput("ui_clustering"),
           helpText("Indicatate how you want your standard errors to be estimated")),
    column(10, align="center", htmlOutput("regression"))
  ),

  hr(),

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
           helpText("Martin Bierey and Joachim Gassen, Humboldt-Universität zu Berlin, January 2018")
           )
    )
)
