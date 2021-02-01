install.packages('mongolite')
library(mongolite)
# 01
m <- mongo(
  collection = "match",
  url = "mongodb+srv://toni:PASSWORD@cluster0.cahdx.mongodb.net/admin",
  db = "match_games"
)

data = read.csv('data.csv', header = TRUE)
data$Date <- as.Date(data$Date , format = "%Y-%m-%d")
head(data)
str(data)

m$drop()
m$insert(data)

# 02
m$count()

# 03
m$find('{ "Date": "2015-12-20" }')
# esta fecha no tiene ningun resultado, asi que se hace la actividad con la fecha
query <- '
{ 
   "Date": "2017-12-09",
   "$or":
       [
         { "HomeTeam": "Real Madrid" }
       ] 
 }'

fields <- '{ "FTHG": true, "_id": false }'
RMGoals <- m$find(query = query, fields = fields)
RMGoals

#04
m$disconnect()
