# 01 US Summary

library(tidyverse)
library(zoo)
library(reshape2)


#### US Summary tab fully replicated:
  # 1. US_Res_Holding_of_FR_Sec (Foreign securities held by U.S. residents - outflows)
  # 2. US_Sec_Held_by_FR (U.S. securities held by foreign residents)


a <- Sys.time()


################################################################################ Foreign securities held by U.S. residents - outflows

##########################   GROSS Purchases and Sales
# Create join_1 data frame
join_1 <- TIC_Data_XBorder_Portfolio_Flows[1] 
colnames(join_1) <- c("Month")
join_1$Total_Buy <- TIC_Data_XBorder_Portfolio_Flows$`Gross Foreign Sales of FR LT Securities to US`
join_1$Total_Sell <- TIC_Data_XBorder_Portfolio_Flows$`Gross Foreign Purchases of FR LT Securities from US`


# Create join_2 data frame
temp <- US_Trs_LTs_Type_Cty %>%
  filter(Code == "99996") 

join_2 <- temp[3]
join_2$Bond_Buy <- temp$GSFR_FS_Bonds
join_2$Bond_Sell <- temp$GPFR_FS_Bonds
join_2$Equity_Buy <- temp$GSFR_FS_Stocks
join_2$Equity_Sell <- temp$GPFR_FS_Stocks

# Merge join_1 and join_2
joiny <- merge(join_1, join_2, by = "Month") %>%
  arrange(desc(as.Date(Month)))



# Create Net_US_Purchases data frame
# == US_Summary -> Foreign securities held by U.S. residents - outflows -> Flows data
Net_US_Purchases <- TIC_Data_XBorder_Portfolio_Flows[1]
colnames(Net_US_Purchases) <-  c("Month")
Net_US_Purchases$Total_Flows <- TIC_Data_XBorder_Portfolio_Flows$`FR LT Securities Purchases net` 
Net_US_Purchases$Bond_Flows <- TIC_Data_XBorder_Portfolio_Flows$`FR Bonds Purchases net`
Net_US_Purchases$Equity_Flows <- TIC_Data_XBorder_Portfolio_Flows$`FR Stocks Purchases net`


# Filter F_LTS_USRes data
US_Res_Holding_of_FR_Sec <- F_LTS_USRes %>%
  filter(Code == "99996") %>%
  inner_join(Net_US_Purchases, by = "Month") %>% # Merge Net_US_Purchases with US_Res_Holding_of_FR_Sec and reorder\
  arrange(desc(as.Date(Month))) %>%
  mutate(
    Bond_Proportion = Bonds / Total_LTS # Calculate Bond_Proportion and Equity_Proportion
    , Equity_Proportion = Stocks / Total_LTS
    , TotalFlow_12m_sum = zoo::rollsum(Total_Flows, 12, na.pad = TRUE, align = "left")
    , TotalFlow_3m_ma = zoo::rollmean(Total_Flows, 3, na.pad = TRUE, align = "left")
    , BondFlow_12m_sum = zoo::rollsum(Bond_Flows, 12, na.pad = TRUE, align = "left")
    , BondFlow_3m_ma = zoo::rollmean(Bond_Flows, 3, na.pad = TRUE, align = "left")
    , EquityFlow_12m_sum = zoo::rollsum(Equity_Flows, 12, na.pad = TRUE, align = "left")
    , EquityFlow_3m_ma = zoo::rollmean(Equity_Flows, 3, na.pad = TRUE, align = "left")
  ) %>%
  inner_join(joiny, by = "Month") %>% # Merge joiny (gross purchases/sales) with US_Res_Holding_of_FR_Sec
  arrange(desc(as.Date(Month)))

save(US_Res_Holding_of_FR_Sec,file=here("data-edited","Foreign securities held by U.S. residents - outflows.Rda"))



############## Summary Statistics:
US_Res_Holding_of_FR_Sec_Summ <- melt(US_Res_Holding_of_FR_Sec[c(1:24),c(7:9,18:23)]) %>%
  group_by(variable) %>%
  summarise(mean= mean(value), sd= sd(value), max = max(value),min = min(value)) %>%
  merge(melt(US_Res_Holding_of_FR_Sec[c(1),c(7:9,18:23)])) %>%
  mutate(zscore = (value - mean)/sd)

save(US_Res_Holding_of_FR_Sec_Summ,file=here("data-edited", "Foreign securities held by U.S. residents - outflows (SUMMARY).Rda"))






################################################################################   U.S. securities held by foreign residents


##########################   GROSS Purchases and Sales

# Create join_1 data frame
join_1 <- TIC_Data_XBorder_Portfolio_Flows[1] 
colnames(join_1) <- c("Month")
join_1$Total_Buy <- TIC_Data_XBorder_Portfolio_Flows$`Gross FR Purchases of US LT Securities`
join_1$Total_Sell <- TIC_Data_XBorder_Portfolio_Flows$`Gross FR Sales of US LT Securities`

temp <- US_Trs_LTs_Type_Cty %>%
  filter(Code == "99996") 

# Create join_2 data frame
join_2 <- temp[3]
join_2$Bond_Buy <- temp$GPFRDS_USTs_BankBonds + temp$GPFRDS_GovCorp_Agency + temp$GPFRDS_USCorpBonds
join_2$Bond_Sell <- temp$GSFRDS_USTs_BankBonds + temp$GSFRDS_GovCorp_Agency + temp$GSFRDS_USCorpBonds
join_2$Equity_Buy <- temp$GPFRDS_USCorpStocks
join_2$Equity_Sell <- temp$GSFRDS_USCorpStocks


# Merge join_1 and join_2
joiny <- merge(join_1, join_2, by = "Month") %>%
  arrange(desc(as.Date(Month)))


################################### Net Foreign Purchases


# Create Net foreign Purchases data frame
Net_FR_Purchases <- Net_FR_Purchases_USTs[1]
Net_FR_Purchases$Bond_Flows <- Net_FR_Purchases_USTs$Net_FR_Purchases + Net_FR_Purchases_US_Agency_Bonds$Net_FR_Purchases + Net_FR_Purchases_US_Corp_Bonds$Net_FR_Purchases
Net_FR_Purchases$Equity_Flows <- Net_FR_Purchases_US_Stocks$Net_FR_Purchases
Net_FR_Purchases$Total_Flows <- Net_FR_Purchases$Bond_Flows + Net_FR_Purchases$Equity_Flows
Net_FR_Purchases <- Net_FR_Purchases[c(1,4,2,3)]


US_Sec_Held_by_FR <- US_LTS_FRes %>%
  filter(Code == "99996") %>%
  mutate(Bonds = USTs + Agency_Bonds + Corp_Bonds) %>%
  select(-c(5:7)) %>%
  select(c(1:4, 6, 5)) %>%
  merge(Net_FR_Purchases, by = "Month") %>%
  mutate(Bond_Proportion = Bonds / Total_LTS
    , Equity_Proportion = Stocks / Total_LTS
    , TotalFlow_12m_sum = zoo::rollsum(Total_Flows, 12, na.pad = TRUE, align = "left")
    , TotalFlow_3m_ma = zoo::rollmean(Total_Flows, 3, na.pad = TRUE, align = "left")
    , BondFlow_12m_sum = zoo::rollsum(Bond_Flows, 12, na.pad = TRUE, align = "left")
    , BondFlow_3m_ma = zoo::rollmean(Bond_Flows, 3, na.pad = TRUE, align = "left")
    , EquityFlow_12m_sum = zoo::rollsum(Equity_Flows, 12, na.pad = TRUE, align = "left")
    , EquityFlow_3m_ma = zoo::rollmean(Equity_Flows, 3, na.pad = TRUE, align = "left")
  ) %>%
  merge(joiny, by = "Month") %>%
  arrange(desc(as.Date(Month)))

save(US_Sec_Held_by_FR,file=here("data-edited","U.S. securities held by foreign residents.Rda"))



############## Summary Statistics:
US_Sec_Held_by_FR_Summ <- melt(US_Sec_Held_by_FR[c(1:24),c(7:9,18:23)]) %>%
  group_by(variable) %>%
  summarise(mean= mean(value), sd= sd(value), max = max(value),min = min(value)) %>%
  merge(melt(US_Sec_Held_by_FR[c(1),c(7:9,18:23)])) %>%
  mutate(zscore = (value - mean)/sd)

save(US_Sec_Held_by_FR_Summ,file=here("data-edited","U.S. securities held by foreign residents (SUMMARY).Rda"))






b <- Sys.time()

b-a

