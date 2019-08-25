# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de, see LICENSE for license
#
# This file contains some simple use cases for the ExPanDaR package.
# It is not a part of the package itself.
# ------------------------------------------------------------------------------

# Start this with a virgin R session

library(tidyverse)
library(ExPanDaR)

# --- Use ExPanD with cross-sectional data -------------------------------------

df <- mtcars
df$cs_id <- row.names(df)
df$ts_id <-1
ExPanD(df, cs_id ="cs_id", ts_id = "ts_id",
       components = c(trend_graph = FALSE, quantile_trend_graph = FALSE))


# --- A somewhat more elaborate example with the same data ---------------------

df_var <- tibble(
  var_name = c("make", "origin", "pseudo_ts", names(mtcars)),
  var_def = c("Make of car",
              "Origin of manufacturer",
              "Pseudo time series indicator",
              "Miles/(US) gallon",
              "Number of cylinders",
              "Displacement (cu.in.)",
              "Gross horsepower",
              "Rear axle ratio",
              "Weight (1000 lbs)",
              "1/4 mile time",
              "Engine (0 = V-shaped, 1 = straight)",
              "Transmission (0 = automatic, 1 = manual)",
              "Number of forward gears",
              "Number of carburetors"),
  type = c("cs_id", "factor", "ts_id",
           rep("numeric", 7), rep("factor", 2), rep("numeric", 2))
)

df <- mtcars %>%
  add_column(make = rownames(mtcars),
             origin = c(rep("Japan", 3), rep("US", 4),
                        rep("Europe", 7), rep("US", 3), "Europe",
                        rep("Japan", 3), rep("US", 4),
                        rep("Europe", 3), "US", rep("Europe",3)),
             pseudo_ts = 1, .before = 1)

ExPanD(df, df_def = df_var,
       components = c(descriptive_table = TRUE,
                      udvars = TRUE,
                      ext_obs = TRUE,
                      histogram = TRUE,
                      by_group_violin_graph = TRUE,
                      scatter_plot = TRUE,
                      regression = TRUE))

# l100km = 235.215/mpg

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
