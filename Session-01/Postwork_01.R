
soccer.data <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
dim(soccer.data)
class(soccer.data)
tail(soccer.data); str(soccer.data)


(x <- soccer.data$FTHG)
(y <- soccer.data$FTAG)

?table

(tablex <- table(x))
(prop.table(tablex))

(tabley <- table(y))
(prop.table(tabley))

(tablexy <- table(x,y))
(prop.table(tablexy))


