
########################################################################################################
#####                                   Instalar librerías                                          ####
########################################################################################################


install.packages("lubridate")
install.packages("dplyr")
install.packages("pool")
install.packages("ggplot2")


########################################################################################################
#####                                   Utilizar librerías                                          ####
########################################################################################################


library(dplyr)
library(ggplot2)


########################################################################################################
#####                                 Limpiar el CVS princial                                       ####
########################################################################################################


data <- read.csv("indicadores.csv", fileEncoding="UTF-8-BOM")
str(data)                                                                                    ###################
data <- select(data, General:Energeticos)                                                    ## Hacer una vez ##
data <- mutate(data, Periodo= seq(ymd("1970-1-1"), ymd("2020-12-1"), by = "months"))         ###################
write.csv(data, "Indice-nacional-de-precios-al-consumidor.csv", row.names = FALSE)


########################################################################################################
#####                            Construir el data frame principal                                  ####
########################################################################################################


data <- read.csv("Indice-nacional-de-precios-al-consumidor.csv", fileEncoding="UTF-8-BOM")

data <- mutate(data, Periodo = as.Date(Periodo, "%Y-%m-%d"))
data <- mutate(data, General = as.numeric(as.character(General)))
data <- mutate(data, Subyacente = as.numeric(as.character(Subyacente)))
data <- mutate(data, Mercancias = as.numeric(as.character(Mercancias)))
data <- mutate(data, Servicios = as.numeric(as.character(Servicios)))
data <- mutate(data, Nosubyacente = as.numeric(as.character(Nosubyacente)))
data <- mutate(data, Agropecuarios = as.numeric(as.character(Agropecuarios)))
data <- mutate(data, Energeticos = as.numeric(as.character(Energeticos)))

str(data)
head(data)
tail(data)


########################################################################################################
#####                            Construir data frame de 2005 a 2020                                ####
########################################################################################################


indices.2005 <-  data %>% filter(Periodo >= ymd("2005-1-1")) 

str(indices.2005)
head(indices.2005)
tail(indices.2005)


########################################################################################################
#####                      Informacion general relevante de 2005 a 2020                             ####
########################################################################################################


indices.2005.maxgeneral <- which.max(indices.2005$General)
paste("En la fecha", indices.2005$Periodo[indices.2005.maxgeneral]," se presento la mayor inflación registrada de 2005 a 2020 con un" , indices.2005$General[indices.2005.maxgeneral], "%")

indices.2005.mingeneral <- which.min(indices.2005$General)
paste("En la fecha", indices.2005$Periodo[indices.2005.mingeneral]," se presento la menor inflación registrada de 2005 a 2020 con un" , indices.2005$General[indices.2005.mingeneral], "%")


########################################################################################################
#####                    Construir data frame por cada uno de los elementos                         ####
########################################################################################################


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


indices.2005.general.plot <- ggplot(indices.2005.general, aes(x, y, col = group)) + geom_line()
indices.2005.general.plot

indices.2005.subgeneral.plot <- ggplot( rbind(indices.2005.general,
                                       indices.2005.subyacente,
                                       indices.2005.nosubyacente),
                                 aes(x, y, col = group)) + geom_line()
indices.2005.subgeneral.plot


ts.plot(cbind(Trend, Trend*Seasonal), , main = "Datos de Producción de Electricidad", 
        ylab = "Producción de electricidad", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")


indices.2005.total.plot <- ggplot(rbind(indices.2005.general,
                                  indices.2005.mercancias,
                                  indices.2005.servicios),
                            aes(x, y, col = group)) + geom_line()

indices.2005.total.plot


indices.2005.total.plot <- ggplot(rbind(indices.2005.general,
                                        indices.2005.agropecuarios,
                                        indices.2005.energeticos),
                                  aes(x, y, col = group)) + geom_line()

indices.2005.total.plot


########################################################################################################
#####                                Boxplot de los indices                                         ####
########################################################################################################


ggplot(indices.2005.groupdata, aes(x = group, y = y, fill = group)) + geom_boxplot() +
  ggtitle("Boxplots de tipo de inflaciones") +
  xlab("Categorías") +
  ylab("Inflación")

########################################################################################################
#####                                      Regresion Lineal                                         ####
########################################################################################################

modelo.general <- lm(indices.2005$General ~ indices.2005$Subyacente + indices.2005$Nosubyacente)
summary(modelo.general)

modelo.componentes <- lm(indices.2005$General ~ indices.2005$Mercancias + indices.2005$Servicios + indices.2005$Agropecuarios + indices.2005$Energeticos)
summary(modelo.componentes)


########################################################################################################
#####                                      Serie de tiempo                                          ####
########################################################################################################


indices.2005.general.ts <- ts(indices.2005$General, start = 2005, freq = 12)

plot(indices.2005.general.ts, 
     main = "Inflación en México", 
     xlab = "Tiempo",
     ylab = "Inflación",
     sub = "Enero de 2005 - Diciembre de 2020")


########################################################################################################
#####                                      Modelo Aditivo                                           ####
########################################################################################################


indices.2005.general.ts.aditivo <- decompose(indices.2005.general.ts)

plot(indices.2005.general.ts.aditivo, xlab = "Tiempo", 
     sub = "Descomposición de los datos de inflacion")

Tendencia <- indices.2005.general.ts.aditivo$trend
Estacionalidad <- indices.2005.general.ts.aditivo$seasonal
Aleatorio <- indices.2005.general.ts.aditivo$random

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Inflación en México", 
        ylab = "Inflación", lty = 1:2)


########################################################################################################
#####                                    Modelo Multiplicativo                                      ####
########################################################################################################


indices.2005.general.ts.multiplicativo <- decompose(indices.2005.general.ts, type = "mult")

plot(indices.2005.general.ts.multiplicativo, xlab = "Tiempo", 
     sub = "Descomposición de los datos de inflación")

Trend <- indices.2005.general.ts.multiplicativo$trend
Seasonal <- indices.2005.general.ts.multiplicativo$seasonal
Random <- indices.2005.general.ts.multiplicativo$random

ts.plot(cbind(Trend, Trend*Seasonal), xlab = "Tiempo", main = "Inflación en México", 
        ylab = "Inflación", lty = 1:2)







