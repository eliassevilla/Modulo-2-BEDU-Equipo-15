#install.packages('lubridate')
library(dplyr)
library(lubridate)
info <- read.csv(url("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-06/Postwork/match.data.csv"), header = TRUE, sep = ',')

info2 <- data.frame(info)

info2 <- info2 %>% 
  mutate(sumagoles = select(., 3,5) %>% rowSums())
info2$date <- as.Date(info2$date , format = "%Y-%m-%d")
head(info2)
str(info2)

info3 <- info2 %>% group_by(month=floor_date(date, "month")) %>%
  summarize(
    goalspermonth=sum(sumagoles),
    games = n(),
    averagegoals = sum(sumagoles)/n()
  )

info2
str(info3)
head(info3)

goles.ts <- ts(info3[, 4], start = c(2010, 8), frequency = 12)
print(goles.ts)

plot(goles.ts,
     main = "Promedio de goles por partido", 
     xlab = "Tiempo",
     sub = "Agosto de 2010 - Diciembre de 2018")
