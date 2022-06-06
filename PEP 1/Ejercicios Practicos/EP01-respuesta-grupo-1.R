#EP01 IME
#Esteban Arenas
#Nicolas Torreblanca
#Rodrigo Escobar

#Preguntas
#¿Qué variables se han cargado?
#   Se han cargado variables como Region y fechas en formato dd-mm-aaaa

#¿Qué tipo tiene cada una de estas variables?
#   La variable Region corresponde a una variable categórica nominales
#   Las variables de fecha son variables categóricas ordinales

#¿Qué escala parecen tener estas variables?
#   La variable Region tiene escala nominal
#   Las variables de fechas tienen escala ordinal

#Paquete
library(dplyr)

#---- Pregunta 1 ----
#Se importan los valores de un archivo separado por punto y comas
casos <- read.csv2("EP01 Datos Covid.csv", stringsAsFactors = FALSE)

#Se seleccionan las columnas que pertenecena a la region del Maule
casos_maule <- casos %>% filter(Region == "Maule")

#Se seleccionan las columnas entre el 1 de Octubre de 2020 y el 31 de Mayo de 2021
casos_maule_intervalo <- casos_maule %>% select(X01.10.2020:X31.05.2021)

#Mostrar dia con el mayor numero de casos en la Region del Maule entre 01-oct-2020 y el 31-may-2021
maxContagios <- which.max(casos_maule_intervalo)
cat("El dia con el mayor numero de casos en la Region del Maule entre 01-oct-2020 y el 31-may-2021 es: ", colnames(casos_maule_intervalo)[maxContagios])

#---- Pregunta 2 ----
#Se suman los datos de cada mes dentro del periodo seleccionado y la Region del Maule
octubre <- rowSums(casos_maule_intervalo %>% select(X01.10.2020:X31.10.2020))
noviembre <- rowSums(casos_maule_intervalo %>% select(X01.11.2020:X30.11.2020))
diciembre <- rowSums(casos_maule_intervalo %>% select(X01.12.2020:X31.12.2020))
enero <- rowSums(casos_maule_intervalo %>% select(X01.01.2021:X31.01.2021))
febrero <- rowSums(casos_maule_intervalo %>% select(X01.02.2021:X28.02.2021))
marzo <- rowSums(casos_maule_intervalo %>% select(X01.03.2021:X31.03.2021))
abril <- rowSums(casos_maule_intervalo %>% select(X01.04.2021:X30.04.2021))
mayo <- rowSums(casos_maule_intervalo %>% select(X01.05.2021:X31.05.2021))

#Se muestra por pantalla el total de contagios por cada mes del periodo
cat("Contagios Octubre: ",octubre)
cat("Contagios Noviembre: ",noviembre)
cat("Contagios Diciembre: ",diciembre)
cat("Contagios Enero: ",enero)
cat("Contagios Febrero: ",febrero)
cat("Contagios Marzo: ",marzo)
cat("Contagios Abril: ",abril)
cat("Contagios Mayo: ",mayo)
