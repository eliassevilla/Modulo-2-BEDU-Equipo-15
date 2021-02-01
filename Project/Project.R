
########################################################################################################
#####                                 Instalar y cargar librerias                                   ####
########################################################################################################


install.packages("lubridate")

library(dbplyr)
library(pool)
library(lubridate)


########################################################################################################
#####                                 Limpiar el cvs princial                                       ####
########################################################################################################


data <- read.csv("indicadores.csv", fileEncoding="UTF-8-BOM")
str(data)
data <- select(data, General:Energeticos) 
data <- mutate(data, Periodo= seq(ymd("1970-1-1"), ymd("2020-12-1"), by = "months"))
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
#####                    Construir data frame por cada uno de los elmentos                          ####
########################################################################################################


indices.2005.general <- data.frame(x = indices.2005$Periodo, y = indices.2005$General, group = rep("General", nrow(indices.2005)))
indices.2005.subyacente <- data.frame(x = indices.2005$Periodo, y = indices.2005$Subyacente, group = rep("Subyacente", nrow(indices.2005)))
indices.2005.mercancias <- data.frame(x = indices.2005$Periodo, y = indices.2005$Mercancias, group = rep("Mercancias", nrow(indices.2005)))
indices.2005.servicios <- data.frame(x = indices.2005$Periodo, y = indices.2005$Servicios, group = rep("Servicios", nrow(indices.2005)))
indices.2005.nosubyacente <- data.frame(x = indices.2005$Periodo, y = indices.2005$Nosubyacente, group = rep("Nosubyacente", nrow(indices.2005)))
indices.2005.agropecuarios <- data.frame(x = indices.2005$Periodo, y = indices.2005$Agropecuarios, group = rep("Agropecuarios", nrow(indices.2005)))
indices.2005.energeticos <- data.frame(x = indices.2005$Periodo, y = indices.2005$Energeticos, group = rep("Energeticos", nrow(indices.2005)))


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


indices.2005.total.plot <- ggplot(rbind(indices.2005.general,
                                  indices.2005.subyacente,
                                  indices.2005.mercancias,
                                  indices.2005.servicios,
                                  indices.2005.nosubyacente,
                                  indices.2005.agropecuarios,
                                  indices.2005.energeticos),
                            aes(x, y, col = group)) + geom_line()

indices.2005.total.plot


########################################################################################################
#####                            Graficar la informacion de indices                                 ####
########################################################################################################

modelo.general <- lm(indices.2005$General ~ indices.2005$Subyacente + indices.2005$Nosubyacente)
summary(modelo.general)

