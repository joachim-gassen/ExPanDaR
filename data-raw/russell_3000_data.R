# (C) Joachim Gassen 2018, gassen@wiwi.hu-berlin.de, see LICENSE file for details
#
# This code uses the tidyquant package to generate
# the russell_3000 dataset by pulling financial statement data
# from Google Finance and daily stock returns data from Yahoo finance.
#
# The Google Finance API is currently defunct and the
# RUSSEL 3000 index membership list provided by
# tq_index() is incomplete so that the
# code is provided here for verification purposes only.
#
# In other words: You cannot replicate the worldscope_3000
# dataset by running this code.
#
# Obviously this code assumes no copyright on the obtained data.
# Copyright remains with the original data producers.

# clear workspace

rm(list=ls())

library(tidyquant)
library(data.table)
library(dplyr)
library(lubridate)

get_exchange_data <- function(refresh = FALSE) {
  if (refresh) {
    ticker_nasdaq <- cbind("NASDAQ", tq_exchange("NASDAQ"))
    names(ticker_nasdaq)[1] <- "exchange"
    ticker_nyse <- cbind("NYSE", tq_exchange("NYSE"))
    names(ticker_nyse)[1] <- "exchange"
    ticker_amex <- cbind("AMEX", tq_exchange("AMEX"))
    names(ticker_amex)[1] <- "exchange"
    ed <- unique(rbind(ticker_nyse, ticker_amex, ticker_nasdaq))
    saveRDS(ed, paste0("data-raw/goyahoo_data/exchange_data_", substr(now(),1,10), ".RDS"))
  } else ed <- readRDS("data-raw/goyahoo_data/exchange_data_2017-04-04.RDS")
  ed
}

pull_data_from_google_finance <- function(ticker, refresh = FALSE) {
  if (refresh) {
    # The following takes a while ( ~ 6h) and generates a lot of errors because of incorrect symbols/missing data
    gf_list <<- lapply(ticker$symbol, function(symbol) {
      cat(paste0("Trying symbol \"", symbol, "\" ...\n"))
      l <- try(tq_get(symbol, get = "financials"))
      Sys.sleep(pmax(rnorm(1,2,0.4),0)) # not sure about google's lockout rules but this seems to be on the safe side
      return(l)
    })
    saveRDS(gf_list,paste0("data-raw/goyahoo_data/google_financials_list_", substr(now(),1,10), ".RDS"))
  } else gf_list <<- readRDS("data-raw/goyahoo_data/google_financials_list_2017-04-04.RDS")
}

convert_gf_list_to_dsets <- function() {
  for (symbol in 1:nrow(ticker)) {
    if (length(gf_list[[symbol]]) == 3) {
      for (fs in 1:length(gf_list[[symbol]]$type)) {
        for (annualpos in 1:max(gf_list[[symbol]]$annual[[fs]]$group)) {
          r <- as.data.frame(gf_list[[symbol]]$annual[[fs]])
          r <- cbind(rep(ticker[symbol,]$symbol, nrow(r[r$group == annualpos,])),
                     r[r$group == annualpos,c("date", "value")])
          names(r) <- c("symbol","date", paste0(gf_list[[symbol]]$type[fs], "_ANN_", annualpos))
          if (fs == 1 & annualpos == 1) dfa <- r
          else dfa <- merge(dfa, r, by=c("symbol","date"))
        }
        for (quartalpos in 1:max(gf_list[[symbol]]$quarter[[fs]]$group)) {
          r <- as.data.frame(gf_list[[symbol]]$quarter[[fs]])
          r <- cbind(rep(ticker[symbol,]$symbol, nrow(r[r$group == quartalpos,])),
                     r[r$group == quartalpos,c("date", "value")])
          names(r) <- c("symbol","date", paste0(gf_list[[symbol]]$type[fs], "_QTR_", quartalpos))
          if (fs == 1 & quartalpos == 1) dfq <- r
          else dfq <- merge(dfq, r, by=c("symbol","date"))
        }
      }
      if (symbol == 1) gf_annual <- dfa else gf_annual <- rbind(gf_annual, dfa)
      if (symbol == 1) gf_quarterly <- dfq else gf_quarterly <- rbind(gf_quarterly, dfq)
    }
  }
  gf_annual <- unique(gf_annual)
  gf_quarterly <- unique(gf_quarterly)
  saveRDS(gf_annual,"goyahoo_data/gf_annual_data.RDS")
  saveRDS(gf_quarterly,"goyahoo_data/gf_quarterly_data.RDS")
}

extract_gf_variable_definitions <- function() {
  annual_vars <- cbind(c(rep("BS",42),rep("CF",19),rep("IS",49)), unique(rbind(gf_list[[1]]$annual[[1]][,(c("group","category"))],
                                                                               gf_list[[1]]$annual[[2]][,(c("group","category"))],
                                                                               gf_list[[1]]$annual[[3]][,(c("group","category"))])))
  gf_annual_vars <- as.data.frame(cbind(paste0(annual_vars[,1], "_ANN_", annual_vars[,2]), annual_vars[,3]))
  names(gf_annual_vars) <- c("var_name", "category")

  quarterly_vars <- cbind(c(rep("BS",42),rep("CF",19),rep("IS",49)), unique(rbind(gf_list[[1]]$quarter[[1]][,(c("group","category"))],
                                                                                  gf_list[[1]]$quarter[[2]][,(c("group","category"))],
                                                                                  gf_list[[1]]$quarter[[3]][,(c("group","category"))])))
  gf_quarterly_vars <- as.data.frame(cbind(paste0(quarterly_vars[,1], "_QTR_", quarterly_vars[,2]), quarterly_vars[,3]))
  names(gf_quarterly_vars) <- c("var_name", "category")
  saveRDS(gf_annual_vars,"data-raw/goyahoo_data/gf_annual_var_defs.RDS")
  saveRDS(gf_quarterly_vars,"data-raw/goyahoo_data/gf_quarterly_var_defs.RDS")
}

pull_stock_data_from_yahoo_finance <- function(ticker, from, to, refresh = FALSE) {
  if (refresh) {
    ys_list <<- lapply(ticker$symbol, function(symbol) {
      cat(paste0("Trying symbol \"", symbol, "\" ...\n"))
      l <- try(tq_get(symbol, get = "stock.prices", from = from, to = to))
      return(l)
    })
    saveRDS(ys_list,paste0("data-raw/goyahoo_data/yahoo_stock_list_", substr(now(),1,10), ".RDS"))
  } else ys_list <<- readRDS("data-raw/goyahoo_data/yahoo_stock_list_2017-04-05.RDS")
}

convert_ys_list_to_dsets <- function() {
  keep_list_item = rep(TRUE, nrow(ticker))
  for (sym in 1:nrow(ticker)) {
    if (length(ys_list[[sym]]) != 7) keep_list_item[[sym]] == FALSE
  }
  ys_daily <- do.call(rbind, cbind(rep(ticker[keep_list_item, "symbol"], length(ys_list[keep_list_item]$date)), ys_list[keep_list_item]))
  ys_daily <- as.data.table(ys_daily)
  ys_daily <- ys_daily[unique(ys_daily$symbol,ys_daily$date),]
  saveRDS(as.data.frame(ys_daily),"data-raw/goyahoo_data/ys_daily_data.RDS")
}

get_google_finance_data <- function(ticker, refresh = FALSE) {
  if (refresh == TRUE) {
    pull_data_from_google_finance(ticker, refresh)
    convert_gf_list_to_dsets()
    extract_gf_variable_definitions()
  } else {
   gf_annual <<- readRDS("data-raw/goyahoo_data/gf_annual_data.RDS")
   gf_annual_vars <<- readRDS("data-raw/goyahoo_data/gf_annual_var_defs.RDS")
  }
}

get_yahoo_finance_data <- function(ticker, refresh = FALSE) {
  if (refresh == TRUE) {
    pull_stock_data_from_yahoo_finance(ticker, from="2009-12-31", to="2018-03-31", refresh)
    convert_ys_list_to_dsets()
  } else {
    ys_daily <<- readRDS("data-raw/goyahoo_data/ys_daily_data.RDS")
  }
}

get_index_data <- function(index = "RUSSELL3000", refresh = FALSE) {
  if (refresh) tq_index(index)
  else readRDS("data-raw/goyahoo_data/index_data.RDS")
}

hslag <- function(x, ts_id) {
  pos <- match(as.numeric(ts_id) - 1, as.numeric(ts_id))
  x[pos]
}

ticker <- get_exchange_data()
get_google_finance_data(ticker = ticker)
get_yahoo_finance_data(ticker = ticker)


ys_daily %>%
  mutate(year = year(date),
         month = month(date),
         mday = mday(date)) %>%
  group_by(symbol, year, month) %>%
  mutate(max_mday = max(mday)) %>%
  filter(mday == max_mday) %>%
  select (-mday, -max_mday) -> ymdata

temp <- left_join(gf_annual, ticker)
temp <- temp[(temp$date >= as.Date("2013-01-01")) & (temp$date <= as.Date("2016-12-31")),]
temp$industry <- as.factor(temp$industry)
temp$sector <- as.factor(temp$sector)
temp$fyr <- as.factor(paste0("FY",year(temp$date)))
temp$year <- year(temp$date)
temp$month <- month(temp$date)

goyahoo_data <- left_join(temp, ymdata, by=c("symbol", "year", "month")) %>%
  rename(fye = date.x, date_prc = date.y)

r3_holdings <- get_index_data()

r3_holdings[1] %>%
  inner_join(goyahoo_data, by="symbol")  %>%
  distinct(symbol, fyr, .keep_all = TRUE) -> r3
r3 <- r3[which(r3$IS_ANN_1 > 0),]
r3$fyr <- factor(r3$fyr, ordered = TRUE)

r3 %>%
  arrange(symbol, fyr, market.cap) %>%
  distinct(company, fyr, BS_ANN_17, IS_ANN_1, .keep_all = TRUE) -> r3

r3$symbol <- as.factor(r3$symbol)

# Fix some naming bugs in underlying data (Source: Yahoo Finance Webpage)
r3$company <- as.character(r3$company)
r3$company[r3$symbol == "EPC"] <- "Edgewell Personal Care Company"
r3$company[r3$symbol == "GCI"] <- "Gannett Co., Inc."
r3$company[r3$symbol == "MSG"] <- "The Madison Square Garden Company"

var_def <- read.csv("data-raw/russell_3000_var_def.csv", stringsAsFactors = FALSE)
vars_to_assign <- var_def[var_def$var_name != var_def$goyahoo_def,]
assignments <- paste0(vars_to_assign$var_name, " = ", vars_to_assign$goyahoo_def, ",")
assignments[length(assignments)] <- substr(assignments[length(assignments)], 1, nchar(assignments[length(assignments)])-1)

code <- "r3 %>% group_by(symbol) %>%"
code <- c(code, "mutate(", assignments, ") %>%")
code <- c(code, "ungroup() %>%")
code <- c(code, paste0("select(", paste(var_def$var_name, collapse = ", "), ") -> russell_3000"))
eval(parse(text = code))

russell_3000 <- as.data.frame(russell_3000)
save(russell_3000, file = "data/russell_3000.RData")

russell_3000_data_def = data.frame(var_name = var_def$var_name,
                                   var_def = var_def$var_def,
                                   type = c("cs_id", "ts_id", "cs_id", "factor", "factor",
                                            rep("numeric", 19)),
                                   stringsAsFactors = FALSE)
save(russell_3000_data_def, file = "data/russell_3000_data_def.RData")

