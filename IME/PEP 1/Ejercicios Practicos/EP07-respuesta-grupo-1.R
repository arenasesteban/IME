#EP07 IME
#Esteban Arenas
#Nicolas Torreblanca
#Rodrigo Escobar

library(dplyr)
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

#---- Pregunta 1 ----
# Una popular discoteca quiere analizar el interés que generan 
# los concursos de baile entre sus visitantes más asiduos. 
# Para ello ha invitado a sus mejores clientes, 8 hombres y 10 mujeres,
# a un evento privado con la posibilidad de participar en un concurso.
# 8 mujeres y 1 hombre decidieron participar. 
# ¿Influye el género en la participación en concursos de baile?

#Definición de Hipótesis
#H0: Las variables son independientes
#HA: Las variables estÃ¡n relacionadas

#Construir la tabla de contingencia
Mujeres <- c(8,2)
Hombres <- c(1,7)

tabla <- as.table(rbind(Mujeres,Hombres))

dimnames (tabla)<-list(sexo = c("Mujeres" , "Hombres"),
                       especialidad = c("Pariticipa","No Participa"))
print(tabla)

# Se aplica la prueba exacta de Fisher.
alfa <- 0.05
prueba_fisher <- fisher.test(tabla, 1-alfa)
print(prueba_fisher)

#---- Respuesta 1 ----
# Dado que al realizar la prueba de Fisher se obtuvo un valor p de 
# 0.01522 menor que el alfa de 0.05, esto quiere decir que se 
# rechaza la hipótesis nula en favor de la hipótesis alternativa, esto
# quiere decir que las variables están relacionadas, en este caso, 
# los géneros influyen en la participación del concurso.


#---- Pregunta 2 ----
# En un estudio acerca del efecto de la deficiencia de vitamina B1 
# durante la infancia temprana Katz, Haltus & Friedmann (2022)
# (Journal of Neurolinguistics, 62(5), 101042), se ha concluido 
# que tal carencia se traduce en severos problemas de lenguaje. 
# Un nuevo estudio busca verificar este hallazgo, para lo cual 
# ha evaluado eldesarrollo del lenguaje a los 5 años de vida de 
# 35 parejas de gemelos idénticos donde, por razones médicas, 
# bebés son alimentados con diferentes fórmulas (una carente de 
# vitamina B1 y la otra, no). Los datos registrados muestran que: 
# * En 10 parejas de gemelos, ninguno presenta trastornos del 
# lenguaje. 
# * En 5 parejas de gemelos, ambos presentan trastornos del lenguaje.
# * En 9 parejas de gemelos, solo aquel que fue alimentado con la
# fórmula que contiene vitamina B1 desarrolla trastornos del lenguaje.
# * En las 11 parejas de gemelos restantes, solo el gemelo con 
# carencia de vitamina B1 presenta trastornos del lenguaje.
# ¿Soportan los nuevos datos la conclusión del estudio original?

#Definición de Hipótesis
#H0: La deficiencia de B1 produce trastornos del lenguaje
#HA: La deficiencia de B1 no produce trastornos del lenguaje



#Construir la tabla de contingencia
Gemelo_B1 <- c(10,9)
Gemelo_Sin_B1 <- c(11,5)

tabla_2 <- as.table(rbind(Gemelo_B1,Gemelo_Sin_B1))

dimnames (tabla_2)<-list(gemelo_Sin_B1 = c("Sin Trastorno" , "Con Trastorno"),
                       gemelo_B1 = c("Sin Trastorno","Con Trastorno"))
print(tabla_2)


# Se aplica la prueba de McNemar
prueba_mcnemar <- mcnemar.test(tabla_2)
print(prueba_mcnemar)

#---- Respuesta 2 ----
# En base a lo resultados de la prueba de McNemar en donde se obtuvo
# un valor p de 0.8231 el cual es mayor al alfa de 0.05, se puede
# conluir que no hay evidencia suficiente para rechazar la hipótesis
# nula, por lo tanto se rechaza la hipótesis alternativa, es decir, 
# que la deficiencia de la vitamina B1 tiene gran incidencia en el
# desarrollo de trastornos del lenguaje en niños.

#---- Pregunta 3 ----
# En su afán de comprender mejor a sus eternos enemigos,  
# los vampiros, Van Helsing ha decidido estudiar si vampiresas y 
# vampiros tienen preferencias similares en cuanto al tipo 
# sanguíneo de sus víctimas. 
# ¿Qué puede concluir a partir las preferencias alimentarias 
# de estos seres de acuerdo a sus registros?

#Definición de Hipótesis
#H0: Las variables género de vampiro y tipo de sangre son independientes.
#H1: Las variables género de vampiro y tipo de sangre están relacionadas
 
#Construir la tabla
Vampiro <- c(15, 12, 8, 6)
Vampiresa <- c(9, 14, 5, 7)

tabla_3 <- as.table(rbind(Vampiro,Vampiresa))

dimnames (tabla_3)<-list(genero = c("Vampiro" , "Vampiresa"),
                         tipo_sangre = c("A","B", "AB","O"))
print(tabla_3)

# Se hace prueba de independencia
prueba_independencia <- chisq.test(tabla_3)
cat("\nLa prueba internamente calcula los valores esperados :\n")
esperados <- round(prueba_independencia [["expected"]], 3)
print(esperados)
cat("\nResultado de la prueba :\n")
print(prueba_independencia) 

#---- Respuesta 3 ----
# En base a los resultados de la prueba de independencia, en la cual
# se obtuvo un valor p de 0.5804 se puede decir que se falla en rechazar
# la hipótesis nula, por lo tanto, se rechaza la hipótesis alternativa,
# la que proponía una relación entre el género del vampiro y gusto en 
# el tipo de sangre de sus víctimas. Por ende, se puede afirmar con 95%
# de confianza que las los gustos de sangre de los vampiros son
# independientes de su género.


#---- Pregunta 4 ----
# La Facultad de Ingeniería desea saber si existe diferencia 
# significativa en el desempeño de los estudiantes en asignaturas 
# críticas de primer semestre. Para ello, le ha entregado un 
# archivo de datos que, para 3 asignaturas, indica si una muestra 
# de 50 estudiantes aprobados o reprobados. ¿Qué puede concluir la 
# Facultad? Indicación: obtenga la muestra a partir del archivo
# EP07 Datos.csv, usando la semilla 102. Considere un nivel de 
# significación alfa = 0,05.

#Definición de Hipótesis
#H0 : La proporción de aprobados es la misma para todos los grupos
#H1 : La proporción de aprobados es diferente para al menos un grupo

# Se extraen los datos del archivo csv
datos <- read.csv2("EP07 Datos.csv")

# Se define una semilla 102
set.seed(102)

# Se define un nivel de significación de 0.05
alfa = 0.05

# Se obtiene una muestra de 50 estudiantes
muestra_50 <- sample(datos[["Id"]], 50, replace = FALSE, prob = NULL)
print(muestra_50)

# Se seleccionan los datos para trabajar con ellos con mayor facilidad
calculo_datos <- t(datos %>% select("Calculo"))
algebra_datos <- t(datos %>% select("Algebra"))
fisica_datos <- t(datos %>% select("Fisica"))

# Se guardan los valores de la muestra en vectores para cada asignatura
i = 1
calculo <- c()
algebra <- c()
fisica <- c()

while(i <= 50){
  pos <- muestra_50[i]
  calculo[i] <- calculo_datos[pos]
  algebra[i] <- algebra_datos[pos]
  fisica[i] <- fisica_datos[pos]
  i=i+1
}

# Se guardan todos los valores de la muestra en un dataframe
new_datos <- data.frame(muestra_50 , calculo , algebra , fisica)

# Se transforma la matriz a formato largo
new_datos_largos <- new_datos %>% pivot_longer(c("calculo", "algebra","fisica"),
                                 names_to = "Asignatura",
                                 values_to = "Estado")

# Se renombra la columna para mejor comprensión
names(new_datos_largos)[names(new_datos_largos) == "muestra_50"] <- "Id"

new_datos_largos[["Id"]] <- factor(new_datos_largos [["Id"]])
new_datos_largos[["Asignatura"]] <- factor(new_datos_largos[["Asignatura"]])

# Se realiza la prueba Q de Cochran.
prueba_cochran <- cochran.qtest(Estado ~ Asignatura | Id ,
                        data = new_datos_largos , alpha = alfa)
print(prueba_cochran)

#---- Respuesta 4 ----
# Dado que al realizar la prueba Q de Cochran se obtuvo
# valor p de 0.01083, esto quiere que se rechaza la hipótesis nula en favor 
# a la hipótesis alternativa, esto quiere decir que se concluye 
# con 95% de confianza que al menos una de las asignaturas los 
#estudiantes tienen desempeño diferente a las demás.


  
  
  
  
  

