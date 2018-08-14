theme_set(theme_minimal(base_size = 18))

dat <<-
  gsheet2tbl(
    'docs.google.com/spreadsheets/d/12p_yT8VF34wybsTxsta85TjpbmhA9tSwXKgYJSaVdpk/edit?usp=sharing'
  )
names(dat)

source(
  "https://raw.githubusercontent.com/nzcoops/r-code/master/modified_mortgage_calculator.R"
)

dat2 <<- dat %>%
  select(DATE:`TOTAL PAYMENT`) %>%
  mutate(
    date = dmy(DATE),
    bal = parse_number(`ACTUAL BALANCE`),
    add = parse_number(`ADDITIONAL PAYMENT`),
    int = parse_number(INTEREST),
    off = parse_number(`OFFSET SAVING`),
    tot = parse_number(`TOTAL PAYMENT`),
    dateNum = as.numeric(date),
    dateNum = dateNum - ((2012 - 1970) * 365),
    add = ifelse(is.na(add), 0, add)
    #add = case_when(add == NA_real_ ~ 0,
    #                 TRUE ~ as.numeric(add))
  ) %>%
  filter(!is.na(date),!is.na(bal)) %>%
  #group_by(date) %>%
  arrange(date) %>%
  mutate(drop = lag(bal) - bal) %>%
  ungroup() %>%
  select(date:drop)

mine <- mortgage(dat2$bal[1], 5, 30)
