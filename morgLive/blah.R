install.packages("installr")
library(installr)
install.Rtools(choose_version = FALSE, check = TRUE, use_GUI = TRUE,
               page_with_download_url = "http://cran.r-project.org/bin/windows/Rtools/, keep_install_file=TRUE")
install.packages("devtools")
library(devtools)
devtools::install_github('rstudio/shinyapps')


library(rsconnect)
setAccountInfo(name='nzcoops', token='542462AB387BFC305D112CA6D8377E97', secret='ykRLOTpAPOUnBfu8g0rtkHE66jNg5QrqTF')
