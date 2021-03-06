
########################################################################################################
#####                                   Instalar librerías                                          ####
########################################################################################################


install.packages("lubridate")
install.packages("dplyr")
#install.packages("pool")
install.packages("ggplot2")
install.packages("forecast")


########################################################################################################
#####                                   Utilizar librerías                                          ####
########################################################################################################


library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)


########################################################################################################
#####                                 Limpiar el CVS princial                                       ####
########################################################################################################

# Se lee el archivo base para la limpieza de datos. Se encontr� que era necesario leerlo con un encoding UTF-8-DOM
data <- read.csv("https://raw.githubusercontent.com/eliassevilla/Modulo-2-BEDU-Equipo-15/main/Project/Indicadores.csv", fileEncoding="UTF-8-BOM")

str(data)                                                                                    ###################
data <- select(data, General:Energeticos)                                                    ## Hacer una vez ##
data <- mutate(data, Periodo= seq(ymd("1970-1-1"), ymd("2020-12-1"), by = "months"))         ###################
write.csv(data, "Indice-nacional-de-precios-al-consumidor.csv", row.names = FALSE)


########################################################################################################
#####                            Construir el data frame principal                                  ####
########################################################################################################

# El archivo obtenido de la limpieza de datos ser� utilizado para el desarrollo de la pr�ctica
data <- read.csv("Indice-nacional-de-precios-al-consumidor.csv", fileEncoding="UTF-8-BOM")

# Para cada uno de los valores se utiliza la funci�n mutate para convertir el valor le�do del archivo a un 
# valor num�rido o de fecha respectivamente
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

# Debido a que este archivo tiene datos hist�ricos desde 1970 y nuestro caso de estudio se centra en el 
# comportamiento actual de la inflaci�n en M�xico, se decidi� analizar desde el a�o 2005 hasta el actual
indices.2005 <-  data %>% filter(Periodo >= ymd("2005-1-1")) 

# Nuevamente se visualiza la estructura del dataframe obtenido para un mejor conocimiento del mismo
str(indices.2005)
head(indices.2005)
tail(indices.2005)


########################################################################################################
#####                      Informacion general relevante de 2005 a 2020                             ####
########################################################################################################

# En este apartado buscaremos informaci�n relevante que podemos obtener del dataframe. Buscaremos 
# en que periodo se encontr� la mayor y la menor inflaci�n registrada dentro de los a�os 2005 y 2020

indices.2005.maxgeneral <- which.max(indices.2005$General)
paste("En la fecha", indices.2005$Periodo[indices.2005.maxgeneral]," se presento la mayor inflación registrada de 2005 a 2020 con un" , indices.2005$General[indices.2005.maxgeneral], "%")

indices.2005.mingeneral <- which.min(indices.2005$General)
paste("En la fecha", indices.2005$Periodo[indices.2005.mingeneral]," se presento la menor inflación registrada de 2005 a 2020 con un" , indices.2005$General[indices.2005.mingeneral], "%")


########################################################################################################
#####                    Construir data frame por cada uno de los elementos                         ####
########################################################################################################

# En esta secci�n costruimos una lista general que contiene toda la informaci�n obtenida para que pudiera 
# ser graficada. Primero descompusimos cada tipo de inflaci�n en una lista independiente. Posteriormente 
# utilizamos la funci�n do.call para juntar todas estas listas en uan sola

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

# Con ayuda de la lista general buscaremos cual fu� la mayor y la menor inflaci�n registrada entre todos 
# los tipos de inflaci�n, buscaremos este valor de inflaci�n, a que tipo corresponde y el periodo en el que
# se registr�

indices.2005.maxgroup <- which.max(indices.2005.groupdata$y)
paste("En la fecha", indices.2005.groupdata$x[indices.2005.maxgroup], "la categoría de", 
      indices.2005.groupdata$group[indices.2005.maxgroup],
      "se presento la mayor inflación registrada de 2005 a 2020 con el" , indices.2005.groupdata$y[indices.2005.maxgroup], "%")

indices.2005.mingroup <- which.min(indices.2005.groupdata$y)
paste("En la fecha", indices.2005.groupdata$x[indices.2005.mingroup], "la categoría de", 
      indices.2005.groupdata$group[indices.2005.mingroup],
      "se presento la menor inflación registrada de 2005 a 2020 con el" , indices.2005.groupdata$y[indices.2005.mingroup], "%")


########################################################################################################
#####                            Graficar la informacion de indices                                 ####
########################################################################################################

# Para poder observar y analizar el comportamiento de la inflaci�n, graficaremos los datos obtenidos, en 
# diferentes gr�ficas. En la primera solo se mostrar� como se comport� la inflaci�n general

indices.2005.general.plot <- ggplot(indices.2005.general, aes(x, y, col = group)) + 
                             geom_line()  + ggtitle("Inflación en México 2005 - 2020") + 
                             xlab("Periodo") + ylab("Inflación ( % )") +
                             labs(col = "Tipo de inflación")

indices.2005.general.plot

# En la siguiente gr�fica se podr� apreciar como se comporta la inflaci�n general con la inflaci�n subyacente
# y la inflaci�n no subyacente. Se puede apreciar en la gr�fica que la inflaci�n subyacente se comporta muy
# similar a la general. La no subyacente llega a ser mucho m�s vol�til

indices.2005.subgeneral.plot <- ggplot( rbind(indices.2005.general,
                                       indices.2005.subyacente,
                                       indices.2005.nosubyacente),
                                aes(x, y, col = group)) + geom_line() + 
                                ggtitle("Inflación en México 2005 - 2020") + 
                                xlab("Periodo") + ylab("Inflación ( % )") +
                                labs(col = "Tipo de inflación")

indices.2005.subgeneral.plot

# En la siguiente gr�fica vamos a comparar los diferentes componentes de la inflaci�n subyacente con la inflaci�n
# general 

indices.2005.total.plot <- ggplot(rbind(indices.2005.general,
                                  indices.2005.mercancias,
                                  indices.2005.servicios),
                           aes(x, y, col = group)) + geom_line() +
                           ggtitle("Inflación en México 2005 - 2020") + 
                           xlab("Periodo") + ylab("Inflación ( % )") +
                           labs(col = "Tipo de inflación")

indices.2005.total.plot

# En la siguiente gr�fica vamos a comparar los diferentes componentes de la inflaci�n no subyacente con la inflaci�n
# general

indices.2005.total.plot <- ggplot(rbind(indices.2005.general,
                                        indices.2005.agropecuarios,
                                        indices.2005.energeticos),
                           aes(x, y, col = group)) + geom_line() +
                           ggtitle("Inflación en México 2005 - 2020") + 
                           xlab("Periodo") + ylab("Inflación ( % )") +
                           labs(col = "Tipo de inflación")

indices.2005.total.plot

# Finalmente vamos a observar como se ven todos los componenetes en una sola gr�fica.

indices.2005.total.plot <- ggplot(rbind(indices.2005.general,
                                        indices.2005.subyacente,
                                        indices.2005.mercancias,
                                        indices.2005.servicios,
                                        indices.2005.nosubyacente,
                                        indices.2005.agropecuarios,
                                        indices.2005.energeticos),
                           aes(x, y, col = group)) + geom_line() +
                           ggtitle("Inflación en México 2005 - 2020") + 
                           xlab("Periodo") + ylab("Inflación ( % )") +
                           labs(col = "Tipo de inflación")

indices.2005.total.plot


########################################################################################################
#####                                Boxplot de los indices                                         ####
########################################################################################################

# Para tener un panorama m�s amplio del comportamiento de los diferentes tipos de inflaciones graficaremos 
# la informaci�n en boxplots.

ggplot(indices.2005.groupdata, aes(x = group, y = y, fill = group)) + geom_boxplot() +
  ggtitle("Boxplots de tipo de inflaciones") +
  xlab("Categorías") +
  ylab("Inflación") +
  labs(fill = "Tipo de inflación")

# Podemos observar que los datos del �ndice no subyacente estan m�s dispersos entre si. El �ndice de
# agropecuarios y energeticos tienen outliners debido a su amplia distribuci�n. Los indices subyacentes 
# tienen sus datos m�s agrupados y se comportan similar a la inflaci�n general.

########################################################################################################
#####                                      Regresion Lineal                                         ####
########################################################################################################

# Llevamos a cabo el ajuste de un modelo, Y = beta0 + beta1*Subyacente + beta2*Nosubyacente + e para observar
# si los indices subyacentes y no subyacentes influyen en la inflaci�n general. Al analizar la estructura del 
# modelo obtenido obtuvimos p values muy cercano a 0 por lo que descartamos la hip�tesis nula H0: beta1 = beta2 = 0
# por lo que el indice subyacente y no subyacente influyen en la inflaci�n general. Se podr�a pensar que la inflaci�n
# no subyacente no influye por ser muy vol�til, pero se observa que si influye.

modelo.general <- lm(indices.2005$General ~ indices.2005$Subyacente + indices.2005$Nosubyacente)
summary(modelo.general)

# Llevamos a cabo el ajuste de un modelo, Y = beta0 + beta1*Mercancias + beta2*Servicios + beta3*Agropecuarios + 
# beta4*Energ�ticos + e para observar si estos componenetes influyen en la inflaci�n general. Al analizar la estructura 
# del modelo obtenido obtuvimos p values muy cercano a 0 por lo que descartamos la hip�tesis nula
# H0: beta1 = beta2 = beta3 = beta4 por lo que estos componentes influyen en la inflaci�n general.

modelo.componentes <- lm(indices.2005$General ~ indices.2005$Mercancias + indices.2005$Servicios + indices.2005$Agropecuarios + indices.2005$Energeticos)
summary(modelo.componentes)


########################################################################################################
#####                                      Serie de tiempo                                          ####
########################################################################################################

# Pasamos el dataframe a una serie de tiempo para hacer un an�lisis de tiempo

indices.2005.general.ts <- ts(indices.2005$General, start = 2005, freq = 12)
(indices.2005.general.ts)

plot(indices.2005.general.ts, 
     main = "Inflación en México", 
     xlab = "Tiempo",
     ylab = "Inflación",
     sub = "Enero de 2005 - Diciembre de 2020")


########################################################################################################
#####                                      Modelo Aditivo                                           ####
########################################################################################################

# Descomponemos la serie de tiempo con el modelo aditivo para poder obtener sus componentes, la tendencia
# la estacional y el ruido. De la misma manera graficamos estas componentes para observarlas
indices.2005.general.ts.aditivo <- decompose(indices.2005.general.ts)

plot(indices.2005.general.ts.aditivo, xlab = "Tiempo", 
     sub = "Descomposición de los datos de inflacion")

Tendencia.aditivo <- indices.2005.general.ts.aditivo$trend
Estacionalidad.aditivo <- indices.2005.general.ts.aditivo$seasonal
Aleatorio.aditivo <- indices.2005.general.ts.aditivo$random

# Observamos como se suman los componentes aditivos para formar el indice de la inflaci�n general
ts.plot(cbind(Tendencia.aditivo, Tendencia.aditivo + Estacionalidad.aditivo), 
        xlab = "Tiempo", main = "Inflación en México (modelo aditivo)", 
        ylab = "Inflación", lty = 1:2)


########################################################################################################
#####                                    Modelo Multiplicativo                                      ####
########################################################################################################


# Descomponemos la serie de tiempo con el modelo multiplicativo para poder obtener sus componentes, la tendencia
# la estacional y el ruido. De la misma manera graficamos estas componentes para observarlas
indices.2005.general.ts.multiplicativo <- decompose(indices.2005.general.ts, type = "mult")

plot(indices.2005.general.ts.multiplicativo, xlab = "Tiempo", 
     sub = "Descomposición de los datos de inflación")

Trend.multiplicativo <- indices.2005.general.ts.multiplicativo$trend
Seasonal.multiplicativo <- indices.2005.general.ts.multiplicativo$seasonal
Random.multiplicativo <- indices.2005.general.ts.multiplicativo$random

# Observamos como se suman los componentes multiplicativos para formar el indice de la inflaci�n general
ts.plot(cbind(Trend.multiplicativo, Trend.multiplicativo*Seasonal.multiplicativo), xlab = "Tiempo", main = "Inflación en México (modelo multiplicativo)", 
        ylab = "Inflación", lty = 1:2)


########################################################################################################
#####                                         Predicción                                            ####
########################################################################################################

# Utilizamos un modelo autoarima para hacer una proyeccion de los valores
modelo <- auto.arima(indices.2005.general.ts)
summary(modelo)

pronostico <- forecast(modelo,12,level=95)

# Obtenemos el pronostico obtenido con el modelo autoarima
pronostico 

plot(pronostico,
     main = 'Pronóstico con auto.arima',
     xlab ='Años',
     ylab = '%')


