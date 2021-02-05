
# A partir del conjunto de datos de soccer de la liga española de las temporadas 2017/2018, 2018/2019 
# y 2019/2020, crea el data frame SmallData, que contenga las columnas date, home.team, home.score, 
# away.team y away.score; esto lo puede hacer con ayuda de la función select del paquete dplyr. 
# Luego establece un directorio de trabajo y con ayuda de la función write.csv guarda el data frame 
# como un archivo csv con nombre soccer.csv. Puedes colocar como argumento row.names = FALSE en write.csv.

library(dplyr)

LaLiga1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
LaLiga1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
LaLiga1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"

download.file(LaLiga1920,"LaLiga1920.csv",mode = "wb")
download.file(LaLiga1819,"LaLiga1819.csv",mode = "wb")
download.file(LaLiga1718,"LaLiga1718.csv",mode = "wb")

lista <- lapply(list.files(), read.csv)
lista <- lapply(lista, select, Date, HomeTeam, FTHG, AwayTeam ,FTAG)
data <- do.call(rbind, lista)
data <- mutate(data, Date = as.Date(Date, '%d/%m/%y'))
data <- rename(data, date = Date, home.team = HomeTeam, 
               home.score = FTHG, away.team = AwayTeam, 
               away.score = FTAG)
write.csv(data, file = 'soccer.csv', row.names = FALSE)

# Con la función create.fbRanks.dataframes del paquete fbRanks importe el archivo soccer.csv a R y 
# al mismo tiempo asignelo a una variable llamada listasoccer. Se creará una lista con los elementos 
# scores y teams que son data frames listos para la función rank.teams. Asigna estos data frames 
# a variables llamadas anotaciones y equipos.

install.packages("fbRanks")
library(fbRanks)
?fbRanks

listasoccer <- create.fbRanks.dataframes(scores.file = 'soccer.csv', date.format = '%Y-%m-%d')
marcadores <- listasoccer$scores
equipos <- listasoccer$teams

head(marcadores)

# Con ayuda de la función unique crea un vector de fechas (fecha) que no se repitan y que 
# correspondan a las fechas en las que se jugaron partidos. Crea una variable llamada n que contenga 
# el número de fechas diferentes. Posteriormente, con la función rank.teams y usando como argumentos 
# los data frames anotaciones y equipos, crea un ranking de equipos usando únicamente datos desde la 
# fecha inicial y hasta la penúltima fecha en la que se jugaron partidos, estas fechas las deberá 
# especificar en max.date y min.date. Guarda los resultados con el nombre ranking.

fecha <- unique(marcadores$date)
fecha <- sort(fecha)
n <- length(fecha)

ranking <- rank.teams(marcadores, teams = equipos, 
                      max.date = fecha[n-1], 
                      min.date = fecha[1])

# Finalmente estima las probabilidades de los eventos, el equipo de casa gana, el equipo visitante 
# gana o el resultado es un empate para los partidos que se jugaron en la última fecha del vector 
# de fechas fecha. Esto lo puedes hacer con ayuda de la función predict y usando como argumentos 
# ranking y fecha[n] que deberá especificar en date.

predict(ranking, date = fecha[n])







