install.packages('lubridate')
library(dplyr)
library(lubridate)
info <- read.csv(url("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-06/Postwork/match.data.csv"), header = TRUE, sep = ',')

info2 <- data.frame(info)

info2 <- info2 %>% 
  mutate(sumagoles = select(., 3,5) %>% rowSums())
info2$date <- as.Date(info2$date , format = "%Y-%m-%d")
head(info2)
str(info2)
