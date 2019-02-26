# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de, see LICENSE for license
#
# This file contains some simple use cases for the ExPanDaR package.
# It is not a part of the package itself.
# ------------------------------------------------------------------------------

# Start this with a virgin R session

rm (list=ls())
library(ExPanDaR)

# --- Use ExPanD with cross-sectional data -------------------------------------


df <- mtcars
df$cs_id <- row.names(df)
df$ts_id <-1
ExPanD(df, cs_id ="cs_id", ts_id = "ts_id",
       components = c(trend_graph = FALSE, quantile_trend_graph = FALSE))


# --- Use the new html_block feature to customize ExPanD reports ---------------

html_blocks <- c(
  paste('<div class="col-sm-2">&nbsp;</div>',
        '<div class="col-sm-10">',
        "<p>&nbsp;</p>",
        "Below you see the <a href=https://en.wikipedia.org/wiki/Preston_curve>",
        "Preston Curve</a>, based on data provided by the",
        "<a href=https://worldbank.org>World Bank</a>.",
        "It was first documented in the 70ies and indicates a robust",
        "association of life expectancy with country-level",
        "income per capita.",
        "<p>&nbsp;</p>",
        "This interactive display allows you to assess the robustness",
        "of this association.",
        "</div>"),
  paste('<div class="col-sm-2"><h3>Transform independent variable</h3></div>',
        '<div class="col-sm-10">',
        "<p>&nbsp;</p>The Preston Curve is far from",
        "linear. Maybe you can come up with a transformation",
        "of <code>gdp_capita</code> that makes the association",
        "a little bit more well behaved?",
        "Use the dialog below to define new variables and",
        "assess their association with <code>lifexpectancy</code>",
        "in the scatter plot above.",
        "</div>"),
  paste('<div class="col-sm-2"><h3>Assess Robustness</h3></div>',
        '<div class="col-sm-10">',
        "You see that the linear regression coefficient",
        "for <code>gdp_capita</code> for <cod>lifeexpectancy</cod>",
        "is signficant at conventional levels. Does this also",
        "hold when you use your tranformed independent variable?",
        "What about additional controls?",
        "Do you see what happens to the number of observations",
        "as you include controls?",
        "What happens when you include fixed effects?",
        "Are your findings stable across regions or income groups?",
        "Have fun exploring!",
        "</div>")
)

my_wb_config <- ExPanD_config_worldbank
my_wb_config$reg_x <- c("gdp_capita")
my_wb_config$reg_fe1 <- "None"
my_wb_config$reg_fe2 <- "None"
my_wb_config$cluster <- "None"

ExPanD(worldbank, df_def = worldbank_data_def, var_def = worldbank_var_def,
       config_list = my_wb_config,
       title = "Explore the Preston Curve",
       components = c(html_block = TRUE, scatter_plot = TRUE,
                     html_block = TRUE, udvars = TRUE,
                     html_block = TRUE, regression = TRUE),
       html_blocks = html_blocks)


my_r3_config <- ExPanD_config_russell_3000
my_r3_config$group_factor <- "sector"

ExPanD(df = russell_3000, df_def = russell_3000_data_def,
       config_list = my_r3_config,
       abstract = "The data for this sample has been collected from Google Finance and Yahoo Finance.",
       components = c(html_block = TRUE,
                      grouping = TRUE,
                      bar_chart = TRUE,
                      descriptive_table = TRUE,
                      histogram = TRUE,
                      ext_obs = TRUE,
                      by_group_bar_graph = TRUE,
                      by_group_violin_graph = TRUE,
                      corrplot = TRUE,
                      scatter_plot = TRUE,
                      regression = TRUE),
       html_blocks = c(paste('<div class="col-sm-12">',
                             "This short panel presents you with some",
                             "financial accounting and stock return data",
                             "of U.S. firms. When you scroll down,",
                             "you will see a regression assessing the association",
                             "of cash flows and accruals with concurrent",
                             "stock returns. Feel free to experiment with",
                             "the various exploration tools. Enjoy!",
                             '</div>')))



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

ExPanD(wb, cs_id = "country", ts_id ="year")

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
  "See this <a href=https://joachim-gassen.github.io/2018/12/interactive-panel-eda-with-3-lines-of-code/>",
  "blog post</a> for further information."
)

ExPanD(wb, df_def = wb_data_def,
       title = title, abstract = abstract)


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
       components = c(trend_graph = FALSE, quantile_trend_graph = FALSE))

# ------------------------------------------------------------------------------
