
# 1. Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R, 
# los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php

LaLiga1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"
LaLiga1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
LaLiga1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"

download.file(LaLiga1920,"LaLiga1920.csv",mode = "wb")
download.file(LaLiga1819,"LaLiga1819.csv",mode = "wb")
download.file(LaLiga1718,"LaLiga1718.csv",mode = "wb")

lista <- lapply(list.files(), read.csv)

# 2. Obten una mejor idea de las características de los data frames al usar las funciones: str, head, View y summary
str(lista)
head(lista)
View(lista)
summary(lista)

# 3. Con la función select del paquete dplyr selecciona únicamente las columnas 
# Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data frames. (Hint: también puedes usar lapply).

lista <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
str(lista)

# 4. Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo.
# Con ayuda de la función rbind forma un único data frame que contenga las seis columnas mencionadas en el punto 3.

lista <- lapply(lista, mutate, Date = as.Date(Date, format = "%d/%m/%Y"))
str(lista)

Goles1720 <- do.call(rbind, lista)
head(Goles1720); tail(Goles1720)
