
########################################################################################################
#####                                   Instalar librerÃ­as                                          ####
########################################################################################################


install.packages("lubridate")
install.packages("dplyr")
#install.packages("pool")
install.packages("ggplot2")
install.packages("forecast")


########################################################################################################
#####                                   Utilizar librerÃ­as                                          ####
########################################################################################################


library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)


########################################################################################################
#####                                 Limpiar el CVS princial                                       ####
########################################################################################################

# Se lee el archivo base para la limpieza de datos. Se encontró que era necesario leerlo con un encoding UTF-8-DOM
data <- read.csv("https://raw.githubusercontent.com/eliassevilla/Modulo-2-BEDU-Equipo-15/main/Project/Indicadores.csv", fileEncoding="UTF-8-BOM")

str(data)                                                                                    ###################
data <- select(data, General:Energeticos)                                                    ## Hacer una vez ##
data <- mutate(data, Periodo= seq(ymd("1970-1-1"), ymd("2020-12-1"), by = "months"))         ###################
write.csv(data, "Indice-nacional-de-precios-al-consumidor.csv", row.names = FALSE)


########################################################################################################
#####                            Construir el data frame principal                                  ####
########################################################################################################

# El archivo obtenido de la limpieza de datos será utilizado para el desarrollo de la práctica
data <- read.csv("Indice-nacional-de-precios-al-consumidor.csv", fileEncoding="UTF-8-BOM")

# Para cada uno de los valores se utiliza la función mutate para convertir el valor leído del archivo a un 
# valor numérido o de fecha respectivamente
data <- mutate(data, Periodo = as.Date(Periodo, "%Y-%m-%d"))
data <- mutate(data, General = as.numeric(as.character(General)))
data <- mutate(data, Subyacente = as.numeric(as.character(Subyacente)))
data <- mutate(data, Mercancias = as.numeric(as.character(Mercancias)))
data <- mutate(data, Servicios = as.numeric(as.character(Servicios)))
data <- mutate(data, Nosubyacente = as.numeric(as.character(Nosubyacente)))
data <- mutate(data, Agropecuarios = as.numeric(as.character(Agropecuarios)))
data <- mutate(data, Energeticos = as.numeric(as.character(Energeticos)))

# Se visualiza el dataframe data para poder observar su estructruca y como esta conformado
str(data)
head(data)
tail(data)


########################################################################################################
#####                            Construir data frame de 2005 a 2020                                ####
########################################################################################################

# Debido a que este archivo tiene datos históricos desde 1970 y nuestro caso de estudio se centra en el 
# comportamiento actual de la inflación en México, se decidió analizar desde el año 2005 hasta el actual
indices.2005 <-  data %>% filter(Periodo >= ymd("2005-1-1")) 

# Nuevamente se visualiza la estructura del dataframe obtenido para un mejor conocimiento del mismo
str(indices.2005)
head(indices.2005)
tail(indices.2005)


########################################################################################################
#####                      Informacion general relevante de 2005 a 2020                             ####
########################################################################################################

# En este apartado buscaremos información relevante que podemos obtener del dataframe. Buscaremos 
# en que periodo se encontró la mayor y la menor inflación registrada dentro de los años 2005 y 2020

indices.2005.maxgeneral <- which.max(indices.2005$General)
paste("En la fecha", indices.2005$Periodo[indices.2005.maxgeneral]," se presento la mayor inflaciÃ³n registrada de 2005 a 2020 con un" , indices.2005$General[indices.2005.maxgeneral], "%")

indices.2005.mingeneral <- which.min(indices.2005$General)
paste("En la fecha", indices.2005$Periodo[indices.2005.mingeneral]," se presento la menor inflaciÃ³n registrada de 2005 a 2020 con un" , indices.2005$General[indices.2005.mingeneral], "%")


########################################################################################################
#####                    Construir data frame por cada uno de los elementos                         ####
########################################################################################################

# En esta sección costruimos una lista general que contiene toda la información obtenida para que pudiera 
# ser graficada. Primero descompusimos cada tipo de inflación en una lista independiente. Posteriormente 
# utilizamos la función do.call para juntar todas estas listas en uan sola

indices.2005.general <- data.frame(x = indices.2005$Periodo, y = indices.2005$General, group = rep("General", nrow(indices.2005)))
indices.2005.subyacente <- data.frame(x = indices.2005$Periodo, y = indices.2005$Subyacente, group = rep("Subyacente", nrow(indices.2005)))
indices.2005.mercancias <- data.frame(x = indices.2005$Periodo, y = indices.2005$Mercancias, group = rep("Mercancias", nrow(indices.2005)))
indices.2005.servicios <- data.frame(x = indices.2005$Periodo, y = indices.2005$Servicios, group = rep("Servicios", nrow(indices.2005)))
indices.2005.nosubyacente <- data.frame(x = indices.2005$Periodo, y = indices.2005$Nosubyacente, group = rep("Nosubyacente", nrow(indices.2005)))
indices.2005.agropecuarios <- data.frame(x = indices.2005$Periodo, y = indices.2005$Agropecuarios, group = rep("Agropecuarios", nrow(indices.2005)))
indices.2005.energeticos <- data.frame(x = indices.2005$Periodo, y = indices.2005$Energeticos, group = rep("Energeticos", nrow(indices.2005)))

indices.2005.lista <- list(indices.2005.general, 
                           indices.2005.subyacente,
                           indices.2005.mercancias,
                           indices.2005.servicios,
                           indices.2005.nosubyacente,
                           indices.2005.agropecuarios,
                           indices.2005.energeticos)

indices.2005.groupdata <- do.call(rbind,indices.2005.lista)
indices.2005.groupdata


########################################################################################################
#####                         Informacion relevante de 2005 a 2020                                  ####
########################################################################################################

# Con ayuda de la lista general buscaremos cual fué la mayor y la menor inflación registrada entre todos 
# los tipos de inflación, buscaremos este valor de inflación, a que tipo corresponde y el periodo en el que
# se registró

indices.2005.maxgroup <- which.max(indices.2005.groupdata$y)
paste("En la fecha", indices.2005.groupdata$x[indices.2005.maxgroup], "la categorÃ­a de", 
      indices.2005.groupdata$group[indices.2005.maxgroup],
      "se presento la mayor inflaciÃ³n registrada de 2005 a 2020 con el" , indices.2005.groupdata$y[indices.2005.maxgroup], "%")

indices.2005.mingroup <- which.min(indices.2005.groupdata$y)
paste("En la fecha", indices.2005.groupdata$x[indices.2005.mingroup], "la categorÃ­a de", 
      indices.2005.groupdata$group[indices.2005.mingroup],
      "se presento la menor inflaciÃ³n registrada de 2005 a 2020 con el" , indices.2005.groupdata$y[indices.2005.mingroup], "%")


########################################################################################################
#####                            Graficar la informacion de indices                                 ####
########################################################################################################

# Para poder observar y analizar el comportamiento de la inflación, graficaremos los datos obtenidos, en 
# diferentes gráficas. En la primera solo se mostrará como se comportó la inflación general

indices.2005.general.plot <- ggplot(indices.2005.general, aes(x, y, col = group)) + 
                             geom_line()  + ggtitle("InflaciÃ³n en MÃ©xico 2005 - 2020") + 
                             xlab("Periodo") + ylab("InflaciÃ³n ( % )") +
                             labs(col = "Tipo de inflaciÃ³n")

indices.2005.general.plot

# En la siguiente gráfica se podrá apreciar como se comporta la inflación general con la inflación subyacente
# y la inflación no subyacente. Se puede apreciar en la gráfica que la inflación subyacente se comporta muy
# similar a la general. La no subyacente llega a ser mucho más volátil

indices.2005.subgeneral.plot <- ggplot( rbind(indices.2005.general,
                                       indices.2005.subyacente,
                                       indices.2005.nosubyacente),
                                aes(x, y, col = group)) + geom_line() + 
                                ggtitle("InflaciÃ³n en MÃ©xico 2005 - 2020") + 
                                xlab("Periodo") + ylab("InflaciÃ³n ( % )") +
                                labs(col = "Tipo de inflaciÃ³n")

indices.2005.subgeneral.plot

# En la siguiente gráfica vamos a comparar los diferentes componentes de la inflación subyacente con la inflación
# general 

indices.2005.total.plot <- ggplot(rbind(indices.2005.general,
                                  indices.2005.mercancias,
                                  indices.2005.servicios),
                           aes(x, y, col = group)) + geom_line() +
                           ggtitle("InflaciÃ³n en MÃ©xico 2005 - 2020") + 
                           xlab("Periodo") + ylab("InflaciÃ³n ( % )") +
                           labs(col = "Tipo de inflaciÃ³n")

indices.2005.total.plot

# En la siguiente gráfica vamos a comparar los diferentes componentes de la inflación no subyacente con la inflación
# general

indices.2005.total.plot <- ggplot(rbind(indices.2005.general,
                                        indices.2005.agropecuarios,
                                        indices.2005.energeticos),
                           aes(x, y, col = group)) + geom_line() +
                           ggtitle("InflaciÃ³n en MÃ©xico 2005 - 2020") + 
                           xlab("Periodo") + ylab("InflaciÃ³n ( % )") +
                           labs(col = "Tipo de inflaciÃ³n")

indices.2005.total.plot

# Finalmente vamos a observar como se ven todos los componenetes en una sola gráfica.

indices.2005.total.plot <- ggplot(rbind(indices.2005.general,
                                        indices.2005.subyacente,
                                        indices.2005.mercancias,
                                        indices.2005.servicios,
                                        indices.2005.nosubyacente,
                                        indices.2005.agropecuarios,
                                        indices.2005.energeticos),
                           aes(x, y, col = group)) + geom_line() +
                           ggtitle("InflaciÃ³n en MÃ©xico 2005 - 2020") + 
                           xlab("Periodo") + ylab("InflaciÃ³n ( % )") +
                           labs(col = "Tipo de inflaciÃ³n")

indices.2005.total.plot


########################################################################################################
#####                                Boxplot de los indices                                         ####
########################################################################################################

# Para tener un panorama más amplio del comportamiento de los diferentes tipos de inflaciones graficaremos 
# la información en boxplots.

ggplot(indices.2005.groupdata, aes(x = group, y = y, fill = group)) + geom_boxplot() +
  ggtitle("Boxplots de tipo de inflaciones") +
  xlab("CategorÃ­as") +
  ylab("InflaciÃ³n") +
  labs(fill = "Tipo de inflaciÃ³n")

# Podemos observar que los datos del índice no subyacente estan más dispersos entre si. El índice de
# agropecuarios y energeticos tienen outliners debido a su amplia distribución. Los indices subyacentes 
# tienen sus datos más agrupados y se comportan similar a la inflación general.

########################################################################################################
#####                                      Regresion Lineal                                         ####
########################################################################################################

# Llevamos a cabo el ajuste de un modelo, Y = beta0 + beta1*Subyacente + beta2*Nosubyacente + e para observar
# si los indices subyacentes y no subyacentes influyen en la inflación general. Al analizar la estructura del 
# modelo obtenido obtuvimos p values muy cercano a 0 por lo que descartamos la hipótesis nula H0: beta1 = beta2 = 0
# por lo que el indice subyacente y no subyacente influyen en la inflación general. Se podría pensar que la inflación
# no subyacente no influye por ser muy volátil, pero se observa que si influye.

modelo.general <- lm(indices.2005$General ~ indices.2005$Subyacente + indices.2005$Nosubyacente)
summary(modelo.general)

# Llevamos a cabo el ajuste de un modelo, Y = beta0 + beta1*Mercancias + beta2*Servicios + beta3*Agropecuarios + 
# beta4*Energéticos + e para observar si estos componenetes influyen en la inflación general. Al analizar la estructura 
# del modelo obtenido obtuvimos p values muy cercano a 0 por lo que descartamos la hipótesis nula
# H0: beta1 = beta2 = beta3 = beta4 por lo que estos componentes influyen en la inflación general.

modelo.componentes <- lm(indices.2005$General ~ indices.2005$Mercancias + indices.2005$Servicios + indices.2005$Agropecuarios + indices.2005$Energeticos)
summary(modelo.componentes)


########################################################################################################
#####                                      Serie de tiempo                                          ####
########################################################################################################

# Pasamos el dataframe a una serie de tiempo para hacer un análisis de tiempo

indices.2005.general.ts <- ts(indices.2005$General, start = 2005, freq = 12)
(indices.2005.general.ts)

plot(indices.2005.general.ts, 
     main = "InflaciÃ³n en MÃ©xico", 
     xlab = "Tiempo",
     ylab = "InflaciÃ³n",
     sub = "Enero de 2005 - Diciembre de 2020")


########################################################################################################
#####                                      Modelo Aditivo                                           ####
########################################################################################################

# Descomponemos la serie de tiempo con el modelo aditivo para poder obtener sus componentes, la tendencia
# la estacional y el ruido. De la misma manera graficamos estas componentes para observarlas
indices.2005.general.ts.aditivo <- decompose(indices.2005.general.ts)

plot(indices.2005.general.ts.aditivo, xlab = "Tiempo", 
     sub = "DescomposiciÃ³n de los datos de inflacion")

Tendencia.aditivo <- indices.2005.general.ts.aditivo$trend
Estacionalidad.aditivo <- indices.2005.general.ts.aditivo$seasonal
Aleatorio.aditivo <- indices.2005.general.ts.aditivo$random

# Observamos como se suman los componentes aditivos para formar el indice de la inflación general
ts.plot(cbind(Tendencia.aditivo, Tendencia.aditivo + Estacionalidad.aditivo), 
        xlab = "Tiempo", main = "InflaciÃ³n en MÃ©xico (modelo aditivo)", 
        ylab = "InflaciÃ³n", lty = 1:2)


########################################################################################################
#####                                    Modelo Multiplicativo                                      ####
########################################################################################################


# Descomponemos la serie de tiempo con el modelo multiplicativo para poder obtener sus componentes, la tendencia
# la estacional y el ruido. De la misma manera graficamos estas componentes para observarlas
indices.2005.general.ts.multiplicativo <- decompose(indices.2005.general.ts, type = "mult")

plot(indices.2005.general.ts.multiplicativo, xlab = "Tiempo", 
     sub = "DescomposiciÃ³n de los datos de inflaciÃ³n")

Trend.multiplicativo <- indices.2005.general.ts.multiplicativo$trend
Seasonal.multiplicativo <- indices.2005.general.ts.multiplicativo$seasonal
Random.multiplicativo <- indices.2005.general.ts.multiplicativo$random

# Observamos como se suman los componentes multiplicativos para formar el indice de la inflación general
ts.plot(cbind(Trend.multiplicativo, Trend.multiplicativo*Seasonal.multiplicativo), xlab = "Tiempo", main = "InflaciÃ³n en MÃ©xico (modelo multiplicativo)", 
        ylab = "InflaciÃ³n", lty = 1:2)


########################################################################################################
#####                                         PredicciÃ³n                                            ####
########################################################################################################

# Utilizamos un modelo autoarima para hacer una proyeccion de los valores
modelo <- auto.arima(indices.2005.general.ts)
summary(modelo)

pronostico <- forecast(modelo,12,level=95)

# Obtenemos el pronostico obtenido con el modelo autoarima
pronostico 

plot(pronostico,
     main = 'PronÃ³stico con auto.arima',
     xlab ='AÃ±os',
     ylab = '%')


