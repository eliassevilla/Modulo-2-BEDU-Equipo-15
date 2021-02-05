# Ahora graficaremos probabilidades (estimadas) marginales y conjuntas para el número de goles que anotan 
# en un partido el equipo de casa o el equipo visitante.

# 1. Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas 
# para estimar las siguientes probabilidades:

library(dplyr)
library(reshape2)
library(ggplot2)

data1718 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")
data1819 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
data1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

View(data1718)
View(data1819)
View(data1920)

lista <- list(data1718, data1819, data1920)
lista <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

head(lista[[1]]); head(lista[[2]]); head(lista[[3]]);

lista[[1]] <- mutate(lista[[1]], Date = as.Date(Date, "%d/%m/%y"))
lista[[2]] <- mutate(lista[[2]], Date = as.Date(Date, "%d/%m/%Y"))
lista[[3]] <- mutate(lista[[3]], Date = as.Date(Date, "%d/%m/%Y"))

head(lista[[1]]); head(lista[[2]]); head(lista[[3]]);

data <- do.call(rbind, lista)

head(data)
View(data)

x <- data$FTHG
y <- data$FTAG

# - La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)

(tablex <- table(x))
(tablex <- prop.table(tablex))
str(tablex)

tablex <- as.data.frame(tablex)
str(tablex)

# - La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)

(tabley <- table(y))
(tabley <- prop.table(tabley))
str(tabley)

tabley <- as.data.frame(tabley)
str(tabley)

# - La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega 
#   como visitante anote y goles (x=0,1,2,, y=0,1,2,)

(tablexy <- table(x,y))
(tablexy <- prop.table(tablexy))
str(tablexy)

tablexy <- melt(tablexy)
str(tablexy)

# 2. Realiza lo siguiente:

# - Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo de casa

plotx <- ggplot(tablex, aes(x = x, y = Freq)) + 
  geom_bar (stat="identity", fill = 'blue') +
  ggtitle('Equipo de casa')

plotx

# - Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo visitante

ploty <- ggplot(tabley, aes(x = y, y = Freq)) + 
  geom_bar (stat="identity", fill = 'purple') +
  ggtitle('Equipo de visita')

ploty

# - Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el equipo de casa y el equipo 
#   visitante en un partido.

tablexy %>% ggplot(aes(x, y)) + 
  geom_tile(aes(fill = value)) + 
  ggtitle('Probabilidades Conjuntas') +
  scale_fill_gradient(low = 'blue', high = 'red') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))


