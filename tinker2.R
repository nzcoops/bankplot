
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
  geom_line() +
  expand_limits(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                y = c(0,300000)) +
  scale_y_continuous(labels = dollar_format())


ggplot(dat2, aes(x=date, y = int)) +
  geom_line() +
  expand_limits(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                y = c(0,1400)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) , fullrange=T) +
  scale_y_continuous(labels = dollar_format())

ggplot(dat2, aes(x=date, y = off)) +
  geom_line() +
  expand_limits(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                y = c(0,200)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) , fullrange=T) +
  scale_y_continuous(labels = dollar_format())

ggplot(dat2, aes(x=date, y = tot)) +
  geom_line() +
  expand_limits(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                y = c(0,1600)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) , fullrange=T) +
  scale_y_continuous(labels = dollar_format())






ggplot(dat2, aes(x=date, y = tot)) +
  #geom_line() +
  expand_limits(x = c(dat2$date[1], dat2$date[nrow(dat2)] + years(3)),
                y = c(0,300000)) +
  geom_smooth(method = "lm", fullrange=T) +
  #geom_point() + 
  scale_y_continuous(labels = dollar_format())

?xlim

