library(gsheet)
library(tidyverse)
library(lubridate)
library(scales)

print("Tinker called")

dat <<- gsheet2tbl('docs.google.com/spreadsheets/d/12p_yT8VF34wybsTxsta85TjpbmhA9tSwXKgYJSaVdpk/edit?usp=sharing')
names(dat)

print("Data read")

dat2 <<- dat %>%
  select(DATE:`TOTAL PAYMENT`) %>%
  mutate(date = dmy(DATE),
         bal = parse_number(`ACTUAL BALANCE`),
         int = parse_number(INTEREST),
         off = parse_number(`OFFSET SAVING`),
         tot = parse_number(`TOTAL PAYMENT`),
         dateNum = as.numeric(date),
         dateNum = dateNum - ((2012-1970)*365)
         ) %>%
  filter(!is.na(date), !is.na(bal)) %>%
  #group_by(date) %>%
  arrange(date) %>%
  mutate(drop = lag(bal) - bal) %>%
  ungroup() %>%
  select(date:drop)

print("Data jigged")
  
