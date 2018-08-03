library(gsheet)
library(tidyverse)
library(lubridate)
library(scales)
library(DT)
library(formattable)
library(gganimate)

source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R")

mortgage(250000, 3.75, 30)

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
  

orig_prin <- dat2$bal[1]
mortgage(orig_prin, 3.75, 30)
# What to present
# Initial loan
currency(dat2$bal[1])
# Current balance
currency(dat2$bal[nrow(dat2)])
# Total interest to pay
currency(sum(aDFmonth$Monthly_Interest))
# Principal paid to date
currency(sum(dat2$drop, na.rm = T))
# Amount of time since loan start
round(interval(dat2$date[1], today()) / years(1),2)
# Amount of time to go
30 - round(interval(dat2$date[1], today()) / years(1),2)
# Interest paid to date
currency(sum(dat2$int))
# Remaining interest to be paid
currency(sum(aDFmonth$Monthly_Interest) - sum(dat2$int))


# Plots - to date
# Principal remaining over time
ggplot(dat2, aes(x=date, y = bal)) +
  geom_line() +
  coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                y = c(0, max(dat2$bal) * 1.2)) +
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")

# Interest paid over time
ggplot(dat2, aes(x=date, y = int)) +
  geom_line() +
  coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                y = c(0, max(dat2$int) * 1.2)) +
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")

# Total monthly payment overtime

ggplot(dat2, aes(x=date, y = tot)) +
  geom_line() +
  coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                  y = c(0, max(dat2$tot) * 1.2)) +
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")

# Offset contribution overtime

ggplot(dat2, aes(x=date, y = off)) +
  geom_line() +
  coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                  y = c(0, max(dat2$off) * 1.2)) +
  scale_y_continuous(labels = dollar_format()) +
  labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")


# Forecasts - to date an into the future
# Plot forward from current trajectory?

ggplot(dat2, aes(x=date, y = bal)) +
  geom_line() +
  coord_cartesian(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(20)),
                  y = c(0, max(dat2$bal) * 1.2)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3) , fullrange=T) +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_date(expand=c(0,0), limits = c(dat2$date[1], dat2$date[nrow(dat2)] + years(20))) +
  labs(x = "Calendar Year", y = "Balance of Prinipal ($)", title = "Some title")


  
summary(lm(bal ~ dateNum + I(dateNum^2) + I(dateNum^3), data = dat2))
summary(lm(bal ~ dateNum + I(dateNum^2), data = dat2))


new.df <- data.frame(date = dat2$date[nrow(dat2)] + months(1:360))
new.df <- data.frame(date = dat2$date[1] + months(1:360))
new.df <- new.df %>% 
  mutate(dateNum = as.numeric(date),
         dateNum = dateNum - ((2012-1970)*365)
  )

mod1 <- lm(bal ~ dateNum, data = dat2)
new.df$predBal <- predict(mod1, new.df)

mod1 <- lm(bal ~ dateNum + I(dateNum^2) + I(dateNum^3), data = dat2)
new.df$predBal <- predict(mod1, new.df)
h(new.df)
ta(new.df)

mortgage(orig_prin, 4.5, 30)
temp <- cbind(new.df, aDFmonth, actual = c(dat2$bal, rep(0, 360-length(dat2$bal))))

temp %>%
  select(date, predBal, Amortization, actual) %>%
  h(100)


temp$date[which(temp$predBal < 0)[1]]
round(interval(dat2$date[1], temp$date[which(temp$predBal < 0)[1]]) / years(1),2)

temp %>%
  select(date, predBal, Amortization, actual) %>%
  mutate(difference = Amortization - predBal)

# Plot forecast from ammort table?
# Extra repayments (find function - build?)


# Balance in 1 year, 3 year, 5 year










