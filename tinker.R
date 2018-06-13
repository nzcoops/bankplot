install.packages("tidyverse")
install.packages("gsheet")
library(googlesheets)

library(gsheet)
library(tidyverse)
library(lubridate)
library(scales)

dat <- gsheet2tbl('docs.google.com/spreadsheets/d/12p_yT8VF34wybsTxsta85TjpbmhA9tSwXKgYJSaVdpk/edit?usp=sharing')
names(dat)

dat2 <- dat %>%
  select(DATE:`TOTAL PAYMENT`) %>%
  mutate(date = dmy(DATE),
         bal = parse_number(`ACTUAL BALANCE`),
         int = parse_number(INTEREST),
         off = parse_number(`OFFSET SAVING`)
         ) %>%
  filter(!is.na(date), !is.na(bal)) %>%
  #group_by(date) %>%
  arrange(date) %>%
  mutate(drop = lag(bal) - bal) %>%
  ungroup() %>%
  select(date:drop)
  

ggplot(dat2, aes(x=date, y = bal)) +
  #geom_line() +
  expand_limits(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                y = c(0,300000)) +
  geom_smooth(method = "lm", fullrange=T) +
  #geom_point() + 
  scale_y_continuous(labels = dollar_format())
  
?xlim
