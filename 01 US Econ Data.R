# 01 US

library(tidyverse)
library(zoo)
library(readxl)
library(httr)
library(lubridate) 
library(here)
#library(plyr)
library(dplyr)
library(fredr)
# library(devtools)
# install_github("mikeasilva/blsAPI")
# library(blsAPI)
# library(jsonlite)
fredr_set_key("d8f33d9c089a79dfad8d60323b2d41e1")

folder <- here::here("data/")


######################### Time to import and edit all data ~ 6-9sec
a <- Sys.time()

# FRED Economic Data
# Trade Weighted U.S. Dollar Index: Broad, Goods and Services
# DTWEXBGS
# https://fred.stlouisfed.org/series/DTWEXBGS
TW_USD <- fredr("DTWEXBGS") %>%
  select(c(date, value)) %>%
  `colnames<-`(c("Date", "Index"))

# Effective Federal Funds Rate
FedFundsRate <- fredr("FEDFUNDS") %>%
  select(c(date, value)) %>%
  mutate(id = "Eff_Rate") %>%
  rbind(
    fredr("DFEDTARU") %>%
      select(c(date, value)) %>%
      mutate(id = "Target_Max")
  ) %>%
  rbind(
    fredr("DFEDTARL") %>%
      select(c(date, value)) %>%
      mutate(id = "Target_Min")
  ) %>%
  pivot_wider(id_cols =date, names_from = id) %>%
  mutate(Target_Max = na.locf(Target_Max, na.rm=FALSE)
           , Target_Min= na.locf(Target_Min, na.rm=FALSE)) %>%
  na.omit()


# https://fred.stlouisfed.org/series/T5YIFR
FiveY_Fwd_Inflation_Exp <- fredr("T5YIFR") %>%
  select(c(date, value)) %>%
  `colnames<-`(c("Date", "Index"))

# S&P500
SPX <- fredr("SP500") %>%
  select(c(date, value)) 

#dow
DJI <- fredr("DJIA") %>%
  select(c(date, value)) 

#nasdaq
NDX <- fredr("NASDAQ100") %>%
  select(c(date, value)) 

#vix
VIX <- fredr("VIXCLS") %>%
  select(c(date, value)) 

# U.S. Bureau of Labor Statistics, Historical CPI-U, CPIAUCSL
Hist_CPIU <- fredr("CPIAUCSL") %>%
  select(c(date, value)) 


# US Treasury sys
# https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yieldYear&year=2018
#Daily Treasury Yield Curve Rates
tmp_hist <- read_csv("https://home.treasury.gov/system/files/276/yield-curve-rates-1990-2021.csv") %>%
  `colnames<-`(c("Date", "1m", "2m", "3m", "4m", "6m", "1y", "2y", "3y", "5y", "7y", "10y", "20y", "30y")) %>%
  select(-c(5)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))


tmp_2023 <- read_csv("https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/2023/all?type=daily_treasury_yield_curve&field_tdr_date_value=2023&page&_format=csv") %>%
  `colnames<-`(c("Date", "1m", "2m", "3m", "4m", "6m", "1y", "2y", "3y", "5y", "7y", "10y", "20y", "30y")) %>%
  select(-c(5)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

tmp_2022 <- read_csv("https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/2022/all?type=daily_treasury_yield_curve&field_tdr_date_value=2022&page&_format=csv") %>%
  `colnames<-`(c("Date", "1m", "2m", "3m", "4m", "6m", "1y", "2y", "3y", "5y", "7y", "10y", "20y", "30y")) %>%
  select(-c(5)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

Yield_Curve_US <- tmp_hist %>%
  rbind(tmp_2022) %>%
  rbind(tmp_2023) %>%
  arrange(Date) %>%
  na.omit()


##output df's as Rda files
save(TW_USD,file=here::here("data-edited","Trade Weighted U.S. Dollar Index.Rda"))
save(FedFundsRate,file=here::here("data-edited","Effective Federal Funds Rate.Rda"))
save(Yield_Curve_US,file=here::here("data-edited","Daily Treasury Yield Curve Rates.Rda"))
save(SPX,file=here::here("data-edited","SPX.Rda"))
save(DJI,file=here::here("data-edited","DJI.Rda"))
save(NDX,file=here::here("data-edited","NDX.Rda"))
save(VIX,file=here::here("data-edited","VIX.Rda"))
save(FiveY_Fwd_Inflation_Exp,file=here::here("data-edited","U.S. 5-Year, 5-Year Forward Inflation Expectation Rate.Rda"))
save(Hist_CPIU,file=here::here("data-edited","U.S. Bureau of Labor Statistics, Historical CPI-U.Rda"))


b <- Sys.time()
b-a




