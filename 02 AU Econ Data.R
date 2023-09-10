# 02 Aus

library(tidyverse)
library(zoo)
library(readxl)
library(httr)
library(lubridate) 
library(rlist)
#library(plyr)
library(dplyr)
library(stringr)
library(here)


folder <- here::here("data/")


######################### Time to import and edit all data ~ 6-9sec

#NOte: Thomson Reuters Data AND Daily Treasury Yield Curve Rates have to be updated manually at this stage :(((((

a <- Sys.time()


url1 <- "https://www.rba.gov.au/statistics/tables/csv/f11.1-data.csv"
con <- url(url1, "rb")
AUD_Exch <- read_csv(con) # read in with specified column names
names <- AUD_Exch[2,] # Pull out col names
names <- names[ , colSums(is.na(names)) == 0] # Remove cols with only NA vals 
num <- ncol(names)+1 # remaining #  of cols
names <- vapply(str_split(names, " "), `[`, 1, FUN.VALUE=character(1)) # extract first str for names
names[names == "Australian" ] <- "AUD_Trade_Weighted_Index" #change name for specific variable
AUD_Exch <- filter(AUD_Exch, rowSums(is.na(AUD_Exch)) != ncol(AUD_Exch)) #remove NA only rows
AUD_Exch <- AUD_Exch[-c(1:10),] #remove header junk
colnames(AUD_Exch) <- names # add names to df
AUD_Exch <- AUD_Exch[,-c(num:ncol(AUD_Exch))] # remove blank cols





### RBA
### AGB Mid Rates
### F16 table
############################################################################################ MATURITY DATE (METHOSD 1)

##### DEPRECATED DUE TO CHANGES IN DATA PUBLISHED BY THE RBA
# TB Yields
load(here::here("data-edited","TB Yields 1992-Current (Indicative Mid Rates of Australian Government Securities – F16).Rda"))
# TIB Yields
load(here::here("data-edited","TIB Yields 1992-Current (Indicative Mid Rates of Australian Government Securities – F16).Rda"))

# save(TB_Yields,file="TB_Yields_1992_Current.Rda")
# 
# ############ TIB Table
# TIB_Yields <- rbind.fill(ACGB_Yields_TIB, ACGB_Yields_TIB1)
# TIB_Yields <- rbind.fill(TIB_Yields, ACGB_Yields_TIB2)
# TIB_Yields <- rbind.fill(TIB_Yields, ACGB_Yields_TIB3)
# save(TIB_Yields,file="TIB_Yields_1992_Current.Rda")





# ######################## 1992 to 2008
# url1 <- "https://www.rba.gov.au/statistics/tables/xls-hist/f16hist.xls"
# location <- paste(folder, "AGB Mid Rates pre08.xls", sep = "")
# download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
# ACGB_Yields <- read_excel(here::here("data", "AGB Mid Rates pre08.xls"))
# ACGB_Yields <- ACGB_Yields[, colSums(is.na(ACGB_Yields)) != nrow(ACGB_Yields)]
# 
# id <- ACGB_Yields[3,] # Pull out col names
# id[1] <- "Date"
# id <- gsub("TI", "TIB", id)
# 
# maturity <- ACGB_Yields[5,] # Pull out col names
# temp <- maturity[[1,1]]
# maturity <- maturity[-c(1)] #remove header junk
# maturity <- as.character(as.Date(as.numeric(maturity[1,]), origin = "1899-12-30"))
# maturity <- list.append(temp, maturity)
# 
# ACGB_Yields <- ACGB_Yields[-c(1:10),] #remove header junk
# colnames(ACGB_Yields) <- id
# ACGB_Yields$Date <- as.Date(as.numeric(ACGB_Yields$Date), origin = "1899-12-30")
# 
# ACGB_Yields_TB <- cbind(select(ACGB_Yields,starts_with("D")),
#                          select(ACGB_Yields,starts_with("TB")))
# 
# ACGB_Yields_TIB <- cbind(select(ACGB_Yields,starts_with("D")),
#                           select(ACGB_Yields,starts_with("TI")))
# 
# 
# 
# 
# ################## 2009 to 2013
# url1 <- "https://www.rba.gov.au/statistics/tables/xls-hist/f16hist-2009-2018.xls"
# location <- paste(folder, "AGB Mid Rates 08-18.xls", sep = "")
# download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
# ACGB_Yields1 <- read_excel(here::here("data", "AGB Mid Rates 08-18.xls"), 1)
# ACGB_Yields1 <- ACGB_Yields1[, colSums(is.na(ACGB_Yields1)) != nrow(ACGB_Yields1)]
# 
# id1 <- ACGB_Yields1[3,] # Pull out col names
# id1[1] <- "Date"
# id1 <- gsub("TI", "TIB", id1)
# 
# # maturity <- ACGB_Yields1[5,] # Pull out col names
# # temp <- maturity[[1,1]]
# # maturity <- maturity[-c(1)] #remove header junk
# # maturity <- as.character(as.Date(as.numeric(maturity[1,]), origin = "1899-12-30"))
# # maturity <- list.append(temp, maturity)
# 
# ACGB_Yields1 <- ACGB_Yields1[-c(1:10),] #remove header junk
# colnames(ACGB_Yields1) <- id1
# ACGB_Yields1$Date <- as.Date(as.numeric(ACGB_Yields1$Date), origin = "1899-12-30")
# 
# ACGB_Yields_TB1 <- cbind(select(ACGB_Yields1,starts_with("D")),
#                          select(ACGB_Yields1,starts_with("TB")))
# 
# ACGB_Yields_TIB1 <- cbind(select(ACGB_Yields1,starts_with("D")),
#                          select(ACGB_Yields1,starts_with("TI")))
# 
# #colnames(ACGB_Yields1) <- maturity
# 
# 
# 
# 
# ################## 2013 to 2018
# ACGB_Yields2 <- read_excel(here::here("data", "AGB Mid Rates 08-18.xls"), 2)
# ACGB_Yields2 <- ACGB_Yields2[, colSums(is.na(ACGB_Yields2)) != nrow(ACGB_Yields2)]
# 
# type <- ACGB_Yields2[1:2,]
# type[type == "Treasury  Bonds"] <- "TB"
# type[type == "Treasury Indexed Bonds"] <- "TIB"
# 
# type1 <- data.frame(stringr::str_extract(type[2,], "\\d{3}"))
# type2 <- data.frame(t(type[1,]))
# id2 <- str_c(type2[,1], type1[,1])
# id2[1] <- "Date"
# 
# # maturity <- ACGB_Yields2[5,] # Pull out col names
# # temp <- maturity[[1,1]]
# # maturity <- maturity[-c(1)] #remove header junk
# # maturity <- as.character(as.Date(as.numeric(maturity[1,]), origin = "1899-12-30"))
# # maturity <- list.append(temp, maturity)
# 
# ACGB_Yields2 <- ACGB_Yields2[-c(1:10),] #remove header junk
# colnames(ACGB_Yields2) <- id2
# ACGB_Yields2$Date <- as.Date(as.numeric(ACGB_Yields2$Date), origin = "1899-12-30")
# 
# ACGB_Yields_TB2 <- cbind(select(ACGB_Yields2,starts_with("D")),
#                          select(ACGB_Yields2,starts_with("TB")))
# 
# ACGB_Yields_TIB2 <- cbind(select(ACGB_Yields2,starts_with("D")),
#                          select(ACGB_Yields2,starts_with("TIB")))
# 
# #colnames(ACGB_Yields1) <- maturity
# 
# 
# ############################# 2019 - current
# url1 <- "https://www.rba.gov.au/statistics/tables/xls/f16.xls?v=2021-05-12-12-18-04"
# location <- paste(folder, "AGB Mid Rates post18.xls", sep = "")
# download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
# ACGB_Yields3 <- read_excel(here::here("data", "AGB Mid Rates post18.xls"))
# 
# type <- ACGB_Yields3[1:2,]
# type[type == "Treasury  Bonds"] <- "TB"
# type[type == "Treasury Indexed Bonds"] <- "TIB"
# 
# type1 <- data.frame(stringr::str_extract(type[2,], "\\d{3}"))
# type2 <- data.frame(t(type[1,]))
# id3 <- str_c(type2[,1], type1[,1])
# id3[is.na(id3)] <- "Date"
# 
# # maturity <- gsub("^.*\n", "", ACGB_Yields3[2,])
# # temp <- "Date"
# # maturity <- maturity[-c(1)] #remove header junk
# # maturity <- as.character(as.Date(maturity, "%d-%b-%Y"))
# # maturity <- list.append(temp, maturity)
# 
# ACGB_Yields3 <- ACGB_Yields3[-c(1:10),] #remove header junk
# colnames(ACGB_Yields3) <- id3
# ACGB_Yields3$Date <- as.Date(as.numeric(ACGB_Yields3$Date), origin = "1899-12-30")
# 
# ACGB_Yields_TB3 <- cbind(select(ACGB_Yields3,starts_with("D")),
#                          select(ACGB_Yields3,starts_with("TB")))
# 
# ACGB_Yields_TIB3 <- cbind(select(ACGB_Yields3,starts_with("D")),
#                           select(ACGB_Yields3,starts_with("TI")))
# 
# 
# ############ TB Table
# 
# 
# TB_Yields <- rbind.fill(ACGB_Yields_TB, ACGB_Yields_TB1)
# TB_Yields <- rbind.fill(TB_Yields, ACGB_Yields_TB2)
# TB_Yields <- rbind.fill(TB_Yields, ACGB_Yields_TB3)
# save(TB_Yields,file="TB_Yields_1992_Current.Rda")
# 
# ############ TIB Table
# TIB_Yields <- rbind.fill(ACGB_Yields_TIB, ACGB_Yields_TIB1)
# TIB_Yields <- rbind.fill(TIB_Yields, ACGB_Yields_TIB2)
# TIB_Yields <- rbind.fill(TIB_Yields, ACGB_Yields_TIB3)
# save(TIB_Yields,file="TIB_Yields_1992_Current.Rda")






######## NEED TO PULL DATA FROM from F2 tables on RBA website
url1 <- "https://www.rba.gov.au/statistics/tables/xls-hist/f02dhist.xls"
location <- paste(folder, "AGB Capital Mkt Yields 95to13.xls", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
AGB_BM_Yields <- read_excel(here::here("data", "AGB Capital Mkt Yields 95to13.xls"), skip=10)
AGB_BM_Yields <- AGB_BM_Yields[-c(7:9)]
AGB_BM_Yields <- AGB_BM_Yields[-c(4640:4669),]
colnames(AGB_BM_Yields) <- c("Date", "2y", "3y", "5y", "10y", "Indexed")


url1 <- "https://www.rba.gov.au/statistics/tables/xls/f02d.xls?v=2021-05-13-14-51-28"
location <- paste(folder, "AGB Capital Mkt Yields post13.xls", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
AGB_BM_Yields1 <- read_excel(here::here("data", "AGB Capital Mkt Yields post13.xls"), skip=10)
AGB_BM_Yields1 <- AGB_BM_Yields1[-c(7:9)]
colnames(AGB_BM_Yields1) <- c("Date", "2y", "3y", "5y", "10y", "Indexed")

AGB_BM_Yields <- rbind(AGB_BM_Yields, AGB_BM_Yields1)






######## ABS CPI Data
# Percentage Change from Corresponding Quarter of Previous Year ;  All groups CPI ;  Australia ;
url1 <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/mar-2021/640101.xls"
location <- paste(folder, "ABS CPI.xls", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
ABS_CPI <- read_excel(here::here("data", "ABS CPI.xls"),2, skip = 13)
ABS_CPI <- ABS_CPI[c(1,19)]
colnames(ABS_CPI) <- c("Date", "A2325847F")


##### Cash rate target
#####################
url1 <- "https://rba.gov.au/statistics/tables/xls/a02hist.xls"
location <- paste(folder, "RBA_Cash_Rate.xls", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
cash_rate <- read_excel(here::here("data", "RBA_Cash_Rate.xls"), skip = 13)
cash_rate <- cash_rate[c(1,3)]
colnames(cash_rate) <- c("Date", "Rate")




#############  Inflation expectations
###### RBA Statitical Tables -- Inflation Expectations – G3
url1 <- "https://rba.gov.au/statistics/tables/xls/g03hist.xls"
location <- paste(folder, "RBA_Infltn_Exp.xls", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
infl_exp <- read_excel(here::here("data", "RBA_Infltn_Exp.xls"), skip = 10)
infl_exp <- infl_exp[-c(4:5)]
colnames(infl_exp) <- c("Date", "1yC", "3mnthBus", "1yMktEcon", "2yMktEcon", "10yB/E")
infl_exp$Date <- as.Date(infl_exp$Date)




##output df's as Rda files
save(AUD_Exch,file=here::here("data-edited","Exchange Rates – Daily - 2018 to Current – F11.1.Rda"))
#save(TB_Yields,file=here::here("data-edited","TB Yields 1992-Current (Indicative Mid Rates of Australian Government Securities – F16).Rda"))
#save(TIB_Yields,file=here::here("data-edited","TIB Yields 1992-Current (Indicative Mid Rates of Australian Government Securities – F16).Rda"))
save(AGB_BM_Yields,file=here::here("data-edited","AGB Yields 1995-Current (Capital Market Yields – Government Bonds – F2).Rda"))
save(ABS_CPI,file=here::here("data-edited","ABS CPI Data (Seasonally Adjusted - A2325847F).Rda"))
save(cash_rate,file=here::here("data-edited","RBA Cash rate target - A02.Rda"))
save(infl_exp,file=here::here("data-edited","RBA Inflation Expectations – G3.Rda"))




b <- Sys.time()
b-a




