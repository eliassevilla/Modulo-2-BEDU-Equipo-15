
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

(tablex <- table(x))
(tablex <- prop.table(tablex))
str(tablex)

tablex <- as.data.frame(tablex)
str(tablex)

plotx <- ggplot(tablex, aes(x = x, y = Freq)) + 
  geom_bar (stat="identity", fill = 'blue') +
  ggtitle('Equipo de casa')

plotx



(tabley <- table(y))
(tabley <- prop.table(tabley))
str(tabley)

tabley <- as.data.frame(tabley)
str(tabley)

ploty <- ggplot(tabley, aes(x = y, y = Freq)) + 
  geom_bar (stat="identity", fill = 'purple') +
  ggtitle('Equipo de visita')

ploty



(tablexy <- table(x,y))
(tablexy <- prop.table(tablexy))
str(tablexy)

tablexy <- melt(tablexy)
str(tablexy)

tablexy %>% ggplot(aes(x, y)) + 
  geom_tile(aes(fill = value)) + 
  ggtitle('Probabilidades Conjuntas') +
  scale_fill_gradient(low = 'blue', high = 'red') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))







