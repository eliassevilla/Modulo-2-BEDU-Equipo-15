
# 1. Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R, 
# los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php

soccer.data <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
dim(soccer.data)
class(soccer.data)
tail(soccer.data); str(soccer.data)

# 2. Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados 
# por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)

(x <- soccer.data$FTHG)
(y <- soccer.data$FTAG)

# 3. Consulta cómo funciona la función table en R al ejecutar en la consola ?table

?table

# 4. Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
# - La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)

(tablex <- table(x))
(prop.table(tablex))

# - La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)

(tabley <- table(y))
(prop.table(tabley))

# - La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como 
#   visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

(tablexy <- table(x,y))
(prop.table(tablexy))


