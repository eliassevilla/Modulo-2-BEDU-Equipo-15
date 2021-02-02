install.packages("tidyvers")

library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyverse)

data1718 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")
data1819 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
data1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

lista <- list(data1718, data1819, data1920)
lista <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

lista[[1]] <- mutate(lista[[1]], Date = as.Date(Date, "%d/%m/%y"))
lista[[2]] <- mutate(lista[[2]], Date = as.Date(Date, "%d/%m/%Y"))
lista[[3]] <- mutate(lista[[3]], Date = as.Date(Date, "%d/%m/%Y"))

data <- do.call(rbind, lista)

x <- data$FTHG
y <- data$FTAG

tablex <- prop.table(table(x))
tabley <- prop.table(table(y))
tablexy <- prop.table(table(x,y))

cocientes <- sweep(tablexy, MARGIN = 1, 1/tablex, `*`)
cocientes <- sweep(cocientes, MARGIN = 2, 1/tabley, `*`)

newx <- sample(x, replace = TRUE)
bootstrapx <- prop.table(table(newx))

newy <- sample(y, replace = TRUE)
bootstrapy <- prop.table(table(newy))

bootstrapxy <- prop.table(table(newx, newy))

bootstrap.cocientes <- sweep(bootstrapxy, MARGIN = 2, 1/bootstrapy, `*`)
bootstrap.cocientes <- sweep(bootstrap.cocientes, MARGIN = 1, 1/bootstrapx, `*`)
bootstrap.cocientes

