# 20 au summ

library(tidyverse)
library(zoo)


# U.S. Securities held by Australian Residents
# Filter the data
Au_Sum <- US_LTS_FRes %>%
  filter(Code == "60089") %>%
  
  # Calculate US_Bonds
  mutate(US_Bonds = USTs + Corp_Bonds + Agency_Bonds) %>%
  
  # Select specific columns
  select(1:4, 9, 8) %>%
  
  # Calculate flows and proportions
  mutate(
    Total_Flows = Total_LTS - lead(Total_LTS),
    Bond_Flows = US_Bonds - lead(US_Bonds),
    Equity_Flows = Stocks - lead(Stocks),
    Bond_Proportion = US_Bonds / Total_LTS,
    Equity_Proportion = Stocks / Total_LTS
  ) %>%
  
  # Calculate rolling sums and means
  mutate(
    TotalFlow_12m_sum = zoo::rollsum(Total_Flows, 12, na.pad = TRUE, align = "left"),
    TotalFlow_3m_ma = zoo::rollmean(Total_Flows, 3, na.pad = TRUE, align = "left"),
    BondFlow_12m_sum = zoo::rollsum(Bond_Flows, 12, na.pad = TRUE, align = "left"),
    BondFlow_3m_ma = zoo::rollmean(Bond_Flows, 3, na.pad = TRUE, align = "left"),
    EquityFlow_12m_sum = zoo::rollsum(Equity_Flows, 12, na.pad = TRUE, align = "left"),
    EquityFlow_3m_ma = zoo::rollmean(Equity_Flows, 3, na.pad = TRUE, align = "left")
  )




