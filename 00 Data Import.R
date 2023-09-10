# 00 Data Import

library(tidyverse)
library(zoo)
library(lubridate)
library(dplyr)
library(here)
library(readr)

######################### Time to import all data ~ 11sec

a <- Sys.time()

folder <- here::here("data/")


#### Holdings of Foreign Long-term Securities by U.S. Residents
#TABLE 1F:	Foreign Long-Term Securities Held by U.S. Residents
F_LTS_USRes <- read_csv("https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/slt1f_globl.csv"
                 , col_names = c("Country", "Code","Month", "Total_LTS", "Bonds","Stocks")
                 , skip = 13)
first <- which(is.na(F_LTS_USRes[1]), arr.ind=TRUE)[1]
if(is.na(first) == F){
  last <- nrow(F_LTS_USRes)
  F_LTS_USRes <- F_LTS_USRes[-c(first:last),]
  }
F_LTS_USRes[F_LTS_USRes=="n.a."] <- NA
F_LTS_USRes$Month <- as.Date(as.yearmon(F_LTS_USRes$Month))
F_LTS_USRes$Bonds <- as.numeric(gsub(",", "", F_LTS_USRes$Bonds))/1000
F_LTS_USRes$Stocks <- as.numeric(gsub(",", "", F_LTS_USRes$Stocks))/1000
F_LTS_USRes$Total_LTS <- F_LTS_USRes$Total_LTS/1000

save(F_LTS_USRes,file=here("data-edited","Holdings of Foreign Long-term Securities by U.S. Residents.Rda"))




#### Holdings of U.S. Long-term Securities by Foreign Residents
# TABLE 1D:  U.S. Long-Term Securities Held by Foreign Residents
tmp <- read_csv("https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/slt1d_globl.csv"
                        , col_names = c("Country", "Code","Month", "Total_LTS", "USTs"
                                          , "Agency_Bonds", "Corp_Bonds", "Stocks")
                        , skip = 14)
first <- which(is.na(tmp[1]), arr.ind=TRUE)[1]
if(is.na(first) == F){
  last <- nrow(tmp)
  tmp <- tmp[-c(first:last),]
}
tmp[tmp=="n.a."] <- NA
US_LTS_FRes <- tmp %>%
  mutate(Month = as.Date(as.yearmon(Month))
         , Total_LTS  = as.numeric(Total_LTS)
         , USTs = as.numeric(gsub(",", "", USTs))
         , Agency_Bonds = as.numeric(gsub(",", "", Agency_Bonds))
         , Corp_Bonds = as.numeric(gsub(",", "", Corp_Bonds))
         , Stocks = as.numeric(gsub(",", "", Stocks))
  )
#Divide vals by 1000
US_LTS_FRes[4:8] <- US_LTS_FRes[4:8]/1000

save(US_LTS_FRes,file=here("data-edited","Holdings of U.S. Long-term Securities by Foreign Residents.Rda"))




#### A.K.A. Monthly Holdings of Short-term Debt
#### Short-term U.S. securities held by foreigners
# Total Liabilities to Foreigners by Type and Country

setClass("num.with.commas")
setAs("character", "num.with.commas", 
      function(from) as.numeric(gsub(",", "", from) ) )


tmp <- read_csv("https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/lb_globl.csv"
                , skip = 19
                , col_names = c("Country", "Code","Month"
                                # TOTAL LIABILITIES
                                , "Total_Liab", "USD_Liab", "Bank_Liab", "Custody_Liab"
                                # Liabilities payable in dollars
                                #Non-negotiable Deposits
                                , "NND_Foreign_Institutions", "NND_Foreign_Other"
                                # Negotiable CDs and ST securities
                                ,"NCDST_Foreign_Institutions", "NCDST_Foreign_Other", "NCDST_USTs", "NCDST_ST_Other", "NCDST_CD"
                                # Selected other liabilities
                                , "Other_Liab_Foreign_Institutions", "Other_Liab_Foreign_Other"
                                #TOTAL LIABILITIES payable in foreign currencies
                                , "Total_Liab_FCCY")
                )
first <- which(is.na(tmp[1]), arr.ind=TRUE)[1]
if(is.na(first) == F){
  last <- nrow(tmp)
  tmp <- tmp[-c(first:last),]
}
tmp[tmp=="n.a."] <- NA

Total_Liab_Type_Cty <- cbind(tmp[c(1:3)]
        , sapply(tmp[c(4)], readr::parse_number)
        , tmp[c(5)]
        , sapply(tmp[c(6:17)], readr::parse_number)
  ) %>%
  mutate(Month = as.Date(as.yearmon(Month))) 
  
save(Total_Liab_Type_Cty,file=here("data-edited","Short-term U.S. securities held by foreigners.Rda"))








#### Gross Foreign Purchases and Sales of Long-term Securities by Country
# U.S. Transactions with Foreigners in Long-term Domestic and Foreign Securities, by Type and Country
tmp <- read_csv("https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/s1_globl.csv"
                                , skip = 19
                                , col_names = c("Country", "Code","Month"
                                                # Gross purchases by foreigners from U.S. residents
                                                # Domestic securities
                                                , "GPFRDS_USTs_BankBonds", "GPFRDS_GovCorp_Agency", "GPFRDS_USCorpBonds", "GPFRDS_USCorpStocks"
                                                # Foreign securities
                                                , "GPFR_FS_Bonds", "GPFR_FS_Stocks"
                                                # Gross sales by foreigners to U.S. residents
                                                # Domestic securities
                                                , "GSFRDS_USTs_BankBonds", "GSFRDS_GovCorp_Agency", "GSFRDS_USCorpBonds", "GSFRDS_USCorpStocks"
                                                # Foreign securities
                                                , "GSFR_FS_Bonds", "GSFR_FS_Stocks"))

first <- which(is.na(tmp[1]), arr.ind=TRUE)[1]
if(is.na(first) == F){
  last <- nrow(tmp)
  tmp <- tmp[-c(first:last),]
}
tmp[tmp=="n.a."] <- NA

US_Trs_LTs_Type_Cty <- tmp %>%
  mutate(Month = as.Date(as.yearmon(Month))) 

n <- ncol(US_Trs_LTs_Type_Cty)
US_Trs_LTs_Type_Cty[4:n] <- US_Trs_LTs_Type_Cty[4:n]/1000

save(US_Trs_LTs_Type_Cty,file=here("data-edited","Gross Foreign Purchases and Sales of Long-term Securities by Country.Rda"))




       
#### Major Foreign Holders of U.S. Treasuries                          
# TABLE 3D:  U.S. Treasury Securities Held by	Foreign Residents
tmp <- read_csv("https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/slt3d_globl.csv"
                                , skip = 14
                                , col_names = c("Country", "Code","Month"
                                                , "Total_USTs", "LT_USTs", "ST_USTs"))
first <- which(is.na(tmp[1]), arr.ind=TRUE)[1]
if(is.na(first) == F){
  last <- nrow(tmp)
  tmp <- tmp[-c(first:last),]
}
tmp[tmp=="n.a."] <- NA

UST_FRes <- tmp %>%
  mutate(Month = as.Date(as.yearmon(Month))
         , Total_USTs = as.numeric(gsub(",", "", Total_USTs))
         , ST_USTs = as.numeric(gsub(",", "", ST_USTs))
  ) 

save(UST_FRes,file=here("data-edited","Major Foreign Holders of U.S. Treasuries.Rda"))



#### Securities (A): U.S. Transactions with Foreign-Residents in Long-Term Securities
#### Parent Page: https://home.treasury.gov/data/treasury-international-capital-tic-system-home-page/tic-forms-instructions/securities-a-us-transactions-with-foreign-residents-in-long-term-securities

  ############## USELESS DATA -- NOT A TIME SERIES
  # A.  Net Foreign Purchases, by country
    # 1. Net purchases of U.S. long-term securities:
        # a. By month. Also in csv format.
            # https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/snetus.csv
        # b. By quarter. Also in csv format. [This file was revised on 9-29-2020; corrected calculation errors.]
            # https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/snetusq.csv
    # 2. Net purchases of Foreign long-term securities:
        # a. By month. Also in csv format.
            # https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/snetfor.csv
        # b. By quarter. Also in csv format.
            # https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/snetforq.csv


############# SPECIAL SERIES FOR U.S. SECURITIES
  # E. Net Foreign Purchases of U.S. long-term securities by major foreign sector:
      # 1. U.S. Treasury Bonds and Notes: 
            # https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/tressect.txt
      # 2. U.S. Gov't corp & Federally-sponsored Agency Bonds: 
            # https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/agnsect.txt
      # 3. U.S. Corporate & Other Bonds: 
            # https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/corpsect.txt
      # 4. U.S. Stocks: 
            # https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/stksect.txt


  # F. Other Acquisitions of U.S. Long-term Securities:
      # 1. Estimates by the Federal Reserve Bank of New York of unrecorded principal repayments to foreigners on domestic corporate and agency asset-backed securities (ABS). 
      #     (The methodology was last revised on 11-16-2012, when use of the SLT data began.)
            # Parent Page: https://home.treasury.gov/data/treasury-international-capital-tic-system-home-page/help-files/estimates-of-principal-repayments-for-asset-backed-securities-abs-including-mortgage-backed-securities-mbs
            # Data: https://ticdata.treasury.gov/Publish/absdata.txt
      # 2. Estimated foreign portfolio acquisitions of U.S. stocks through stock swaps.
            # Data: https://ticdata.treasury.gov/Publish/swapdata.txt (SAME LINK AS BELOW)
    
  
  # G. Net Purchases of Foreign Long-term Securities, by type of foreign security.
      # Data: https://ticdata.treasury.gov/Publish/netfsec.txt


  # H. Estimated U.S. portfolio acquisitions of foreign stocks through stock swaps.
      # Parent page: https://home.treasury.gov/adjustments-for-stock-swaps
      # Data: https://ticdata.treasury.gov/Publish/swapdata.txt
      # Methodology: https://home.treasury.gov/adjustments-for-stock-swaps




# TIC Press Releases -- by topic
# Parent page: https://home.treasury.gov/data/treasury-international-capital-tic-system-home-page/tic-press-releases-by-topic#1
    # Monthly Releases and Archives of Treasury International Capital (TIC) Data {Across-U.S. Border Financial Flows}
        # Download: Previous releases: ----- historical data (most recent) (CSV): a column of historical data for each row in the data table in the press release.
        # File Name: npr_history
        # Data: https://treasury.gov/resource-center/data-chart-center/tic/Documents/npr_history.csv






########################### TO DOWNLOAD 
#########   E. 1-4
  # (IN MILLIONS OF DOLLARS)
  # (NEGATIVE FIGURES INDICATE NET SALES BY FOREIGNERS TO U.S. RESIDENTS OR A NET OUTFLOW OF CAPITAL FROM THE UNITED STATES)

# 1. U.S. Treasury Bonds and Notes: 
url1 <- "https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/tressect.txt" #data url
location <- paste(folder, "E1 Net_FR_Purchases_USTs.txt", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
Net_FR_Purchases_USTs <- read_fwf(here::here("data", "E1 Net_FR_Purchases_USTs.txt")
                                  , skip=18
                                  , fwf_positions(c(5,22,37,50,69), c(12,30,44,58,NA)))
colnames(Net_FR_Purchases_USTs) <- c("Month", "Net_FR_Purchases", "FR_Official_Purchases", "FR_Other_Purchases", "INTL_Org_Purchases")
Net_FR_Purchases_USTs$Month <- as.Date(as.yearmon(Net_FR_Purchases_USTs$Month))



# 2. U.S. Gov't corp & Federally-sponsored Agency Bonds: 
url1 <- "https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/agnsect.txt" #data url
location <- paste(folder, "E2 Net_FR_Purchases_US_Agency_Bonds.txt", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
Net_FR_Purchases_US_Agency_Bonds <- read_fwf(here::here("data", "E2 Net_FR_Purchases_US_Agency_Bonds.txt")
                                  , skip=18
                                  , fwf_positions(c(5,22,37,50,69), c(12,30,44,58,NA)))
colnames(Net_FR_Purchases_US_Agency_Bonds) <- c("Month", "Net_FR_Purchases", "FR_Official_Purchases", "FR_Other_Purchases", "INTL_Org_Purchases")
Net_FR_Purchases_US_Agency_Bonds$Month <- as.Date(as.yearmon(Net_FR_Purchases_US_Agency_Bonds$Month))


# 3. U.S. Corporate & Other Bonds: 
url1 <- "https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/corpsect.txt" #data url
location <- paste(folder, "E3 Net_FR_Purchases_US_Corp_Bonds.txt", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
Net_FR_Purchases_US_Corp_Bonds <- read_fwf(here::here("data", "E3 Net_FR_Purchases_US_Corp_Bonds.txt")
                                             , skip=18
                                             , fwf_positions(c(5,22,37,50,69), c(12,30,44,58,NA)))
colnames(Net_FR_Purchases_US_Corp_Bonds) <- c("Month", "Net_FR_Purchases", "FR_Official_Purchases", "FR_Other_Purchases", "INTL_Org_Purchases")
Net_FR_Purchases_US_Corp_Bonds$Month <- as.Date(as.yearmon(Net_FR_Purchases_US_Corp_Bonds$Month))


# 4. U.S. Stocks: 
url1 <- "https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/stksect.txt" #data url
location <- paste(folder, "E4 Net_FR_Purchases_US_Stocks.txt", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
Net_FR_Purchases_US_Stocks <- read_fwf(here::here("data", "E4 Net_FR_Purchases_US_Stocks.txt")
                                           , skip=18
                                           , fwf_positions(c(5,22,37,50,69), c(12,30,44,58,NA)))
colnames(Net_FR_Purchases_US_Stocks) <- c("Month", "Net_FR_Purchases", "FR_Official_Purchases", "FR_Other_Purchases", "INTL_Org_Purchases")
Net_FR_Purchases_US_Stocks$Month <- as.Date(as.yearmon(Net_FR_Purchases_US_Stocks$Month))


#### Divide all by 1000
Net_FR_Purchases_USTs[2:5] <- Net_FR_Purchases_USTs[2:5]/1000
Net_FR_Purchases_US_Corp_Bonds[2:5] <- Net_FR_Purchases_US_Corp_Bonds[2:5]/1000
Net_FR_Purchases_US_Agency_Bonds[2:5] <- Net_FR_Purchases_US_Agency_Bonds[2:5]/1000
Net_FR_Purchases_US_Stocks[2:5] <- Net_FR_Purchases_US_Stocks[2:5]/1000


#Save rda files
save(Net_FR_Purchases_USTs,file=here("data-edited","Net Foreign purchases of U.S. Treasury Bonds and Notes.Rda"))
save(Net_FR_Purchases_US_Agency_Bonds,file=here("data-edited","Net Foreign purchases of U.S. Gov't corp & Federally-sponsored Agency Bonds.Rda"))
save(Net_FR_Purchases_US_Corp_Bonds,file=here("data-edited","Net Foreign purchases of U.S. Corporate & Other Bonds.Rda"))
save(Net_FR_Purchases_US_Stocks,file=here("data-edited","Net Foreign purchases of U.S. Stocks.Rda"))







## csv data not downloading 

##########   H. U.S.  acquisitions of foreign stocks through swaps.
# ($ Millions)
url1 <- "https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/swapdata.csv" #data url
location <- paste(folder, "H Stock_Purchases_w_Swaps.csv", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
Stock_Purchases_w_Swaps <- read_csv(here::here("data", "H Stock_Purchases_w_Swaps.csv")
                                       , skip=14) %>%
  `colnames<-`(c("Month", "US_Purchases_FR_Stocks", "FR_Purchases_US_Stocks")) %>%
  filter(rowSums(is.na(.)) != ncol(.)) %>%
  mutate(Month = as.Date(as.yearmon(Month,"%Y-%b")))

save(Stock_Purchases_w_Swaps,file=here("data-edited","U.S.  acquisitions of foreign stocks through swaps.Rda"))


##########   (TIC) Data {Across-U.S. Border Financial Flows}
#   npr_history
url1 <- "https://www.treasury.gov/resource-center/data-chart-center/tic/Documents/npr_history.csv" #data url
location <- paste(folder, "TIC Data.csv", sep = "")
download.file(url1, destfile = location, method = "auto",quiet =TRUE,mode="wb")
TIC_Data_XBorder_Portfolio_Flows <- read_csv(here::here("data", "TIC Data.csv")
                                    , skip=13) %>%
  `colnames<-`(c("Date"
                 ,"Gross FR Purchases of US LT Securities"
                 ,"Gross FR Sales of US LT Securities"
                 , "Domestic LT Purchases net"
                 , "Private net"
                 , "Private USTs net" 
                 , "Private Gov Agency net"
                 , "Private Corp Bonds net"
                 , "Private Stocks net"
                 , "Official net"
                 , "Official USTs net" 
                 , "Official Gov Agency net"
                 , "Official Corp Bonds net"
                 , "Official Stocks net"
                 , "Gross Foreign Purchases of FR LT Securities from US"
                 , "Gross Foreign Sales of FR LT Securities to US"
                 , "FR LT Securities Purchases net"
                 , "FR Bonds Purchases net"
                 , "FR Stocks Purchases net"
                 , "Net LT Securities transactions"
                 , "Other purchases of LT Securities"
                 , "Net FR purchases of LT Securities"
                 , "Increase in FR holding of USD ST US Securities"
                 , "US TBills"
                 , "x.Private net"
                 , "x.Official net"
                 , "Other instruments and liabilities"
                 , "y.Private net"
                 , "y.Official net"
                 , "Change in Banks Net USD liabilities"
                 , "Net monthly TIC flows"
                 , "z.Private net"
                 , "z.Official net"))  %>%
  mutate(Date = as.Date(as.yearmon(Date,"%Y-%b")))
  
n <- ncol(TIC_Data_XBorder_Portfolio_Flows)
TIC_Data_XBorder_Portfolio_Flows[2:n] <- TIC_Data_XBorder_Portfolio_Flows[2:n]/1000

save(TIC_Data_XBorder_Portfolio_Flows,file=here("data-edited","TIC Data (Across-U.S. Border Financial Flows).Rda"))



b <- Sys.time()

b-a


