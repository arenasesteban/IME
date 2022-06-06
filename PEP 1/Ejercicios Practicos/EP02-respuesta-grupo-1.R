#EP02 IME
#Esteban Arenas
#Nicolas Torreblanca
#Rodrigo Escobar

#Librerias
library (ggpubr)
library(dplyr)

#Lectura de archivo e importacion de datos
datos <- read.csv2("EP02 Datos Casen 2017.csv")

#Se agrupan los valores por provincia y se cuentan los datos que tiene cada uno
agrupado_provincia <- group_by(datos, provincia) %>% summarise(count = n())

#Se realiza un grafico de torta para visualizar y comparar la cantidad de datos que tiene cada provincia
grafico <- ggpie (agrupado_provincia ,
            x = "count",
            label = "provincia",
            fill = c("red ", " yellow ", " green ", "blue", "orange", "pink"),
            title = "Cantidad de gente encuestada por provincia",
            lab.pos = "out")

#Se muestra por pantalla el grafico
print(grafico)

#Pregunta Grupo 1
#¿Se encuestó más o menos la misma cantidad de gente en cada provincia de la RM?
#Se elige la varianza como medida estadística para analizar que tan parecidos son los datos importados, 
#dado que esta medida permite verificar que tan separados estan los datos entre sí.

#A partir del grafico de Torta que se muestra en pantalla, se puede apreciar que existe una diferencia notable
#entre la cantidad de personas encuestadas en la provincia de Santiago en comparación al resto de provincias,
#por lo que se puede afirmar que no se encuestó cantidades similares de personas por provincia.

#Calculo de Varianza
varianza <- agrupado_provincia %>% summarise(Varianza = var(count))
print(varianza)

#Como se puede observar el valor obtenido al calcular la varianza muestral es muy alto, 
#lo que nos indica y confirma que los datos graficados estan muy dispersos.