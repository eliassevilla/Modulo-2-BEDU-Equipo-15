# Ahora investigarás la dependencia o independencia del número de goles anotados por el equipo de casa y el número de goles 
# anotados por el equipo visitante mediante un procedimiento denominado bootstrap, revisa bibliografía en internet para que 
# tengas nociones de este desarrollo.

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

# 1. Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles (x=0,1,... ,8), y el equipo 
# visitante anote Y=y goles (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al dividir estas probabilidades 
# conjuntas por el producto de las probabilidades marginales correspondientes.

cocientes <- sweep(tablexy, MARGIN = 1, 1/tablex, `*`)
cocientes <- sweep(cocientes, MARGIN = 2, 1/tabley, `*`)
cocientes

# 2. Mediante un procedimiento de boostrap, obtén más cocientes similares a los obtenidos en la tabla del punto anterior. 
# Esto para tener una idea de las distribuciones de la cual vienen los cocientes en la tabla anterior. Menciona en cuáles casos 
# le parece razonable suponer que los cocientes de la tabla en el punto 1, son iguales a 1 (en tal caso tendríamos independencia 
# de las variables aleatorias X y Y).

# Repetir n veces (1000 veces)

newx <- sample(x, replace = TRUE)
bootstrapx <- prop.table(table(newx))

newy <- sample(y, replace = TRUE)
bootstrapy <- prop.table(table(newy))

bootstrapxy <- prop.table(table(newx, newy))

bootstrap.cocientes <- sweep(bootstrapxy, MARGIN = 2, 1/bootstrapy, `*`)
bootstrap.cocientes <- sweep(bootstrap.cocientes, MARGIN = 1, 1/bootstrapx, `*`)
bootstrap.cocientes
  
