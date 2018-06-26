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
         off = parse_number(`OFFSET SAVING`),
         dateNum = as.numeric(date),
         dateNum = dateNum - ((2012-1970)*365)
         ) %>%
  filter(!is.na(date), !is.na(bal)) %>%
  #group_by(date) %>%
  arrange(date) %>%
  mutate(drop = lag(bal) - bal) %>%
  ungroup() %>%
  select(date:drop)
  

new.df <- data.frame(date = dat2$date[nrow(dat2)] + months(1:300))
new.df <- new.df %>% 
  mutate(dateNum = as.numeric(date),
         dateNum = dateNum - ((2012-1970)*365)
  )
  
mod1 <- lm(bal ~ dateNum, data = dat2)
new.df$predBal <- predict(mod1, new.df)

mod1 <- lm(bal ~ dateNum + I(dateNum^2) + I(dateNum^3), data = dat2)
new.df$predBal <- predict(mod1, new.df)




ggplot(dat2, aes(x=date, y = bal)) +
  #geom_line() +
  expand_limits(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                y = c(0,300000)) +
  geom_smooth(method = "lm", fullrange=T) +
  #geom_point() + 
  scale_y_continuous(labels = dollar_format())
  
?xlim
