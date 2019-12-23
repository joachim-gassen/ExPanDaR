# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de, see LICENSE for license
#
# This file contains some simple use cases for the ExPanDaR package.
# It is not a part of the package itself.
# ------------------------------------------------------------------------------

# Start this with a virgin R session

library(ExPanDaR)
ExPanD()

# --- Use ExPanD with cross-sectional data -------------------------------------

ExPanD(mtcars, export_nb_option = TRUE)


# --- Use ExPanD on a condensed Worldbank data set -----------------------------

library(ExPanDaR)
library(tidyverse)

assign_vars <- function(var_name, definition) {
  assignments <- paste0(var_name, " = ", definition, ",")
  assignments[length(assignments)] <- substr(assignments[length(assignments)], 1,
                                             nchar(assignments[length(assignments)])-1)
  return(assignments)
}

calc_variables <- function(df, var_name, definition, type, can_be_na) {
  cs_id <- definition[type == "cs_id"]
  ts_id <- definition[type == "ts_id"]

  code <- c("df %>% arrange(",
            paste(c(cs_id, ts_id), collapse=", "),
            ") %>%")

  vars_to_assign <- which(var_name %in% cs_id)
  code <- c(code, "mutate(",
            assign_vars(var_name[vars_to_assign], definition[vars_to_assign]),
            ") %>% ")

  code <- c(code,"group_by(",
            paste(cs_id, collapse=", "),
            ") %>%")

  vars_to_assign <- which(!var_name %in% cs_id)
  code <- c(code, "transmute(",
            assign_vars(var_name[vars_to_assign], definition[vars_to_assign]),
            ") %>%")
  code <- c(code, "drop_na(",
            paste(var_name[can_be_na != 1], collapse = ","),
            ") -> ret ")

  eval(parse(text = code))
  return(as.data.frame(ret))
}

wb_var_def <- worldbank_var_def %>%
  slice(c(1:4,8,16:23))

wb_var_def <- wb_var_def[c(1:5, 13, 6:12),]

wb_var_def$can_be_na[wb_var_def$var_name == "lifeexpectancy"] <- 0

wb <- calc_variables(worldbank,
                     wb_var_def$var_name,
                     wb_var_def$var_def,
                     wb_var_def$type,
                     wb_var_def$can_be_na)

# write_csv(wb, "wb_condensed.csv")

ExPanD(wb, cs_id = "country", ts_id ="year", export_nb_option = TRUE)

# A niced ExPanD version with variable definitions and
# a short info text to put online.

wb_data_def <- wb_var_def %>%
  left_join(worldbank_data_def, by = c("var_def" = "var_name")) %>%
  select(-var_def) %>%
  rename(var_def = var_def.y,
         type = type.x) %>%
  select(var_name, var_def, type, can_be_na)

# write_csv(wb_data_def, "wb_data_def.csv")

title <- "Explore the Preston Curve with ExPanDaR"
abstract <- paste(
  "The data for this sample has been collected using the",
  "<a href=https://data.worldbank.org>World Bank API</a>.",
  "See this <a href=https://joachim-gassen.github.io/2018/12/interactive-panel-eda-with-3-lines-of-code>",
  "blog post</a> for further information."
)

ExPanD(wb, df_def = wb_data_def,
       title = title, abstract = abstract,
       export_nb_option = TRUE)


# --- Customize ExPanD to explore EPA fuel economy data-------------------------

# See https://joachim-gassen.github.io/2019/04/customize-your-interactive-eda-explore-the-fuel-economy-of-the-u.s.-car-market/
# for more info

# The following two chuncks borrow
# from the raw data code of the
# fueleconomy package by Hadley Wickham,
# See: https://github.com/hadley/fueleconomy

library(tidyverse)
library(ExPanDaR)

if(!file.exists("vehicles.csv")) {
  tmp <- tempfile(fileext = ".zip")
  download.file("http://www.fueleconomy.gov/feg/epadata/vehicles.csv.zip",
                tmp, quiet = TRUE)
  unzip(tmp, exdir = ".")
}

raw <- read.csv("vehicles.csv", stringsAsFactors = FALSE)
countries <- read.csv("https://joachim-gassen.github.io/data/countries.csv",
                      stringsAsFactors = FALSE)

vehicles <- raw %>%
  mutate(car = paste(make, model, trany),
         mpg_hwy = ifelse(highway08U > 0, highway08U, highway08),
         mpg_city = ifelse(city08U > 0, city08U, city08)) %>%
  left_join(countries) %>%
  select(car, make, country, trans = trany,
         year,
         class = VClass, drive = drive, fuel = fuelType,
         cyl = cylinders, displ = displ,
         mpg_hwy, mpg_city) %>%
  filter(drive != "",
         year > 1985,
         year < 2020) %>%
  mutate(fuel = case_when(
    fuel == "CNG" ~ "gas",
    fuel == "Gasoline or natural gas" ~ "hybrid_gas",
    fuel == "Gasoline or propane" ~ "hybrid_gas",
    fuel == "Premium and Electricity" ~ "hybrid_electro",
    fuel == "Premium Gas or Electricity" ~ "hybrid_electro",
    fuel == "Premium Gas and Electricity" ~ "hybrid_electro",
    fuel == "Regular Gas or Electricity" ~ "hybrid_electro",
    fuel == "Electricity" ~ "electro",
    fuel == "Diesel" ~ "diesel",
    TRUE ~ "gasoline"
  ),
  class = case_when(
    grepl("Midsize", class) ~ "Normal, mid-size",
    grepl("Compact", class) ~ "Normal, compact",
    grepl("Small Station Wagons", class) ~ "Normal, compact",
    grepl("Large Cars", class) ~ "Normal, large",
    grepl("Minicompact", class) ~ "Normal, sub-compact",
    grepl("Subcompact", class) ~ "Normal, sub-compact",
    grepl("Two Seaters", class) ~ "Two Seaters",
    grepl("Pickup Trucks", class) ~ "Pickups",
    grepl("Sport Utility Vehicle", class) ~ "SUVs",
    grepl("Special Purpose Vehicle", class) ~ "SUVs",
    grepl("Minivan", class) ~ "(Mini)vans",
    grepl("Vans", class) ~ "(Mini)vans"
  ),
  drive = case_when(
    grepl("4-Wheel", drive) ~ "4-Wheel Drive",
    grepl("4-Wheel", drive) ~ "4-Wheel Drive",
    grepl("All-Wheel", drive) ~ "4-Wheel Drive",
    grepl("Front-Wheel", drive) ~ "Front-Wheel Drive",
    grepl("Rear-Wheel", drive) ~ "Rear-Wheel Drive"
  ),
  trans = case_when(
    grepl("Automatic", trans) ~ "Automatic",
    grepl("Manual", trans) ~ "Manual"
  )) %>%
  na.omit()

df_def <- data.frame(
  var_name = names(vehicles),
  var_def = c("Make, model and transition type indentifying a unique car in the data",
              "Make of car",
              "Country where car producing firm is loacted",
              "Transition type (automatic or manual)",
              "Year of data",
              "Classification type of car (simplified from orginal data)",
              "Drive type of car (Front Wheel, Rear Wheel or 4 Wheel)",
              "Fuel type (simplified from orginal data)",
              "Number of engine cylinders",
              "Engine displacement in liters",
              "Highway miles per gallon (MPG). For electric and CNG vehicles this number is MPGe (gasoline equivalent miles per gallon).",
              "City miles per gallon (MPG). For electric and CNG vehicles this number is MPGe (gasoline equivalent miles per gallon)."),
  type = c("cs_id", rep("factor", 3), "ts_id", rep("factor", 3), rep("numeric", 4))
)

html_blocks <- c(
  paste("<div class='col-sm-12'>",
        "By default, this display uses all data from car makes with more",
        "than 100 cars in the 'fueleconomy.gov' database.",
        "Above, you can limit the analysis to cars from a certain make,",
        "class, country, fuel type or other factor present in the data.",
        "</div>"),
  paste("<div class='col-sm-12'>",
        "In the display above, remove the check mark to see the absolute",
        "number of cars included in the data each year.",
        "Also, change the additional factor to see how the distribution",
        "of cars across countries, transition types, etc. changes over time",
        "</div>"),
  paste("<div class='col-sm-12'>",
        "In the two tables above, you can assess the distributions of the",
        "four numerical variables of the data set. Which car has the",
        "largest engine of all times?",
        "</div>"),
  paste("<div class='col-sm-12'>",
        "Explore the numerical variables across factors. You will see,",
        "not surprisingly, that fuel economy varies by car class.",
        "Does it also vary by drive type?",
        "</div>"),
  paste("<div class='col-sm-12'>",
        "The above two panels contain good news. Fuel economy has",
        "increased over the last ten years. See for yourself:",
        "Has the size of engines changed as well?",
        "</div>"),
  paste("<div class='col-sm-12'>",
        "The scatter plot documents a clear link between engine size",
        "and fuel economy in term of miles per gallon.",
        "Below, you can start testing for associations.",
        "</div>"),
  paste("<div class='col-sm-12'>",
        "Probably, you will want to test for some associations that",
        "require you to construct new variables. No problem. Just enter the",
        "variable definitions above. Some ideas on what to do:",
        "<ul><li>Define country dummies (e.g., country == 'US') to see",
        "whether cars from certain countries are less fuel efficient than others.</li>",
        "<li>Define a dummy for 4-Wheel drive cars to assess the penalty",
        "of 4-Wheel drives on fuel economy.</li>",
        "<li>If you are from a metric country, maybe your are mildly annoyed",
        "by the uncommon way to assess fuel economy via miles per gallon.",
        "Fix this by defining a liter by 100 km measure",
        "(hint: 'l100km_hwy := 235.215/mpg_hwy').</li></ul>",
        "</div>"),
  paste("<div class='col-sm-12'>",
        "Above, you can play around with certain regression parameters.",
        "See how robust coefficients are across car classes by estimating",
        "the models by car class ('subset' option).",
        "Try a by year regression to assess the development of fuel economy",
        "over time. <br> <br>",
        "If you like your analysis, you can download a zipfile containing",
        "the data and an R notebook reporting the analysis. Alternatively,",
        "you can store the ExPanD configuration and reload it at a later",
        "stage.",
        "</div>")
)

cl <- list(
  ext_obs_period_by = "2019",
  bgbg_var = "mpg_hwy",
  bgvg_var = "mpg_hwy",
  scatter_loess = FALSE,
  delvars = NULL,
  scatter_size = "cyl",
  bar_chart_relative = TRUE,
  reg_x = c("cyl", "displ", "trans"),
  scatter_x = "displ",
  reg_y = "mpg_hwy",
  scatter_y = "mpg_hwy",
  bgvg_byvar = "class",
  quantile_trend_graph_var = "mpg_hwy",
  bgbg_byvar = "country",
  scatter_color = "country", bar_chart_var2 = "class",
  ext_obs_var = "mpg_hwy",
  trend_graph_var1 = "mpg_hwy",
  trend_graph_var2 = "mpg_city",
  sample = "vehicles"
)

abstract <- paste(
  "This interactive display features the",
  "<a href=https://www.fueleconomy.gov/>",
  "fuel economy data provided by the U.S. Environmental Protection Agency.</a>",
  "It allows you to explore the fuel economy of cars in the U.S. market",
  "across time and other dimensions.",
  "<br>&nbsp;<br>",
  "It is based on the 'ExPanD' display provided by the",
  "<a href=https://joachim-gassen.github.io/ExPanDaR>'ExPanDaR' package</a>.",
  "Click <a href=https://jgassen.shinyapps.io/expand>here</a> to explore your",
  "own data with 'ExPanD'.",
  "<br>&nbsp;<br>",
  "Otherwise: Scroll down and start exploring!"
)

ExPanD(vehicles, df_def = df_def, config_list = cl,
       title = "Explore the Fuel Economy of Cars in the U.S. Market",
       abstract = abstract,
       components = c(subset_factor = TRUE,
                      html_block = TRUE,
                      bar_chart = TRUE,
                      html_block = TRUE,
                      descriptive_table = TRUE,
                      ext_obs = TRUE,
                      html_block = TRUE,
                      by_group_bar_graph = TRUE,
                      by_group_violin_graph = TRUE,
                      html_block = TRUE,
                      trend_graph = TRUE,
                      quantile_trend_graph = TRUE,
                      html_block = TRUE,
                      scatter_plot = TRUE,
                      html_block = TRUE,
                      udvars = TRUE,
                      html_block = TRUE,
                      regression = TRUE,
                      html_block = TRUE),
       html_blocks = html_blocks,
       export_nb_option = TRUE
)


# --- Use ExPanD to explore IMDB data ------------------------------------------

library(tidyverse)

name_basics <- read_tsv("https://datasets.imdbws.com/name.basics.tsv.gz",
                        na = "\\N", quote = '')
title_basics <- read_tsv("https://datasets.imdbws.com/title.basics.tsv.gz",
                         na = "\\N", quote = '')
title_ratings <- read_tsv("https://datasets.imdbws.com/title.ratings.tsv.gz",
                          na = "\\N", quote = '')
title_akas <- read_tsv("https://datasets.imdbws.com/title.akas.tsv.gz",
                       na = "\\N", quote = '')
title_crew <- read_tsv("https://datasets.imdbws.com/title.crew.tsv.gz",
                       na = "\\N", quote = '')
title_episode <- read_tsv("https://datasets.imdbws.com/title.episode.tsv.gz",
                          na = "\\N", quote = '')
title_principals <- read_tsv("https://datasets.imdbws.com/title.principals.tsv.gz",
                             na = "\\N", quote = '')


name_basics %>%
  filter(str_detect(primaryProfession, "actor|actress"))  %>%
  select(nconst, primaryName, birthYear)  -> actors

name_basics %>%
  filter(str_detect(primaryProfession, "director"))  %>%
  select(nconst, primaryName, birthYear)  -> directors

lead_actor <- title_principals %>%
  filter(str_detect(category, "actor|actress")) %>%
  select(tconst, ordering, nconst, category) %>%
  group_by(tconst) %>%
  filter(ordering == min(ordering)) %>%
  mutate(lead_actor_gender = ifelse(category == "actor", "male", "female")) %>%
  left_join(name_basics) %>%
  rename(lead_actor_name = primaryName,
         lead_actor_yob =  birthYear,
         lead_actor_yod = deathYear) %>%
  select(tconst, lead_actor_name, lead_actor_gender,
         lead_actor_yob, lead_actor_yod)

director <- title_principals %>%
  filter(str_detect(category, "director")) %>%
  select(tconst, ordering, nconst, category) %>%
  group_by(tconst) %>%
  filter(ordering == min(ordering)) %>%
  left_join(name_basics) %>%
  rename(director_name = primaryName,
         director_yob =  birthYear,
         director_yod = deathYear) %>%
  select(tconst, director_name, director_yob, director_yod)

imdb <- title_ratings %>%
  left_join(title_basics) %>%
  left_join(lead_actor) %>%
  left_join(director) %>%
  filter(titleType == "movie" | titleType == "tvSeries",
         numVotes >= 10000,
         isAdult == 0) %>%
  mutate(year = as.ordered(startYear),
         lead_actor_age = ifelse(startYear - lead_actor_yob > 0,
                                 startYear - lead_actor_yob, NA),
         director_age = ifelse(startYear - director_yob > 0,
                               startYear - director_yob, NA),
         genre = str_split(genres, ',', simplify = TRUE)[,1],
         type = ifelse(titleType == "movie", "Movie", "TV Series"),
         ts_id = 1) %>%
  rename(avg_rating = averageRating,
         num_votes = numVotes,
         length_minutes = runtimeMinutes,
         title = primaryTitle) %>%
  select(ts_id, tconst, year, type, title, genre,
         num_votes, avg_rating, length_minutes,
         director_name, director_age,
         lead_actor_name, lead_actor_age, lead_actor_gender)


ExPanD(imdb, ts_id = "ts_id", cs_id = c("tconst", "title"),
       components = c(trend_graph = FALSE, quantile_trend_graph = FALSE),
       export_nb_option = TRUE)

# ------------------------------------------------------------------------------
