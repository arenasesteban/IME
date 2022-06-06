#EP04 IME
#Esteban Arenas
#Nicolas Torreblanca
#Rodrigo Escobar

# Importar paquetes.
library(dplyr)
library(ggpubr)

# Lectura de archivos
datos <- read.csv2("EP04 datos.csv")

# ---- Pregunta 1 ----

# El Comité Olímpico cree que el mejor tiempo medio de los atletas negros 
# después de ingresar al programa de entrenamiento es superior a 9,52 segundos. 
# ¿Soportan los datos esta afirmación?

#Hipótesis Nula: El mejor tiempo medio de los atletas negros después de
#ingresar al programa de entrenamiento es igual a 9.52 segundos.
# u = 9.52

#Hipótesis Alternativa: El mejor tiempo medio de los atletas negros después de
#ingresar al programa de entrenamiento es mayor a 9.52 segundos.
# u > 9.52

#Primero se tiene que hacer una seleccion de los datos que se van a trabajar

# Se seleccionan los tiempos de los atletas negros
atletlas_n <- datos %>% filter(Raza == "Negra")
tiempos_posterior <- atletlas_n[["Posterior"]]

# Establecer los datos conocidos. 
n <- length(tiempos_posterior)
grados_libertad <- n - 1
valor_nulo_1 <- 9.52

# Luego de eso, se verifica si la muestra se asimila a una distribucion normal

grafico_1 <- ggqqplot(data = data.frame(tiempos_posterior),
                    x = "tiempos_posterior",
                    color = "steelblue",
                    xlab = "Teorico",
                    ylab = "Muestra",
                    title = "Gráfico Q-Q Muestra Atletas vs Distr. Normal")
print(grafico_1)

#Como se puede ver en el gráfico, se puede ver que muchos puntos entran dentro
#de la zona azul, por lo que se puede decir que corresponde a una distribución normal

#Confirmando lo anterior, se puede aplicar la prueba de T de Student para una muestra


# Se fija un nivel de significación
alfa <- 0.025

# Calcular el estadístico de prueba 
cat("\tPrueba t para una muestra\n\n")
media_1 <- mean(tiempos_posterior)
cat("Media =", media_1 , "M$\n")
desv_est_1 <- sd(tiempos_posterior)
error_1 <- desv_est_1 / sqrt(n)
t_1 <- (media_1 - valor_nulo_1) / error_1
cat("t =", t_1, "\n")

# Calcular el valor p.
p_1 <- pt(t_1, df = grados_libertad , lower.tail = TRUE)
cat("p =", p_1, "\n")

# Construir el intervalo de confianza
t_critico_1 <- qt(alfa , df = grados_libertad , lower.tail = FALSE)
superior_1 <- media_1 + t_critico_1 * error_1
cat("Intervalo de confianza = (-Inf , ", superior_1 , "]\n", sep = "")

# Se aplica la prueba t de Student
prueba_t_1 <- t.test(tiempos_posterior,
                     alternative = "greater",
                     mu = valor_nulo_1,
                     conf.level = 1 - alfa)
#Se muestra por pantalla los resultados de la prueba
print(prueba_t_1)

# ---- Respuesta 1 ----
# La prueba realizada dio como resultado un p-valor menor al valor
# de significancia por lo que la hipótesis nula queda rechaza al mismo tiempo
# que la hipótesis alternativa es aceptada.
# Por otra parte, dado que la media de los tiempos de los atletas obtenida 
# es de 9.784283 segundos, la afirmación del Comité Olímpico se puede decir
# que es correcta porque este valor es superior a 9.52 segundos.


# ---- Pregunta 2 ----
# Sugieren los datos que la mejor marca de los atletas blancos 
# se reduce en 5,04 segundos tras el entrenamiento?

#Hipótesis Nula: Las medias de las diferencias en las marcas de los
#atletas blancos es igual a 5.04 segundos
#udiff = 5.04

#Hipótesis Alternativa: Las medias de las diferencias en la marcas
#de los atletas blancos no se reduce en 5.04 segundos
#udiff =/= 5.04

#Se seleccionan los tiempos de los atletas blancos
atletlas_b <- datos %>% filter(Raza == "Blanca")
tiempos_posterior_b <- atletlas_b[["Posterior"]]
tiempos_previo_b <- atletlas_b[["Previo"]]

diferencia <- tiempos_previo_b - tiempos_posterior_b

# Verificar si la distribución se acerca a la normal.
normalidad <- shapiro.test(diferencia)
print(normalidad)

#Como los valores de p son altos, se acepta la distribución como similar a una normal.

#Esto permite trabajar con la prueba de T de student para muestras pareadas

valor_nulo_2 <- 5.04

#Fijar un nivel de significación. (se usa el mismo de antes 0.025)

#Se aplica la prueba t de Student
prueba_t_2 <- t.test(x = tiempos_previo_b,
                       y = tiempos_posterior_b,
                       paired = TRUE,
                       alternative = "two.sided",
                       mu = valor_nulo_2,
                       conf.level = 1 - alfa)
#Se muestran los resultados de la prueba
print(prueba_t_2)


#---- Respuesta 2 ----
# Tomando como base los datos obtenidos al aplicar la prueba t de Student 
# se puede observar que el valor de p es mucho mejnor que el valor de 
# significancia que se planteó, por lo tanto, se rechaza la hipótesis nula
# en favor de la hipótesis alternativa. Por otra parte se obtiene que la
# diferencia de medias corresponde a 2.721894 el cual es un valor menor al 5.04
# que se menciona en la pregunta. Es por esto que la afirmación se
# rechaza ya que las marcas de los atletas blancos no se 
# redujeron en 5.04 segundos tras el entrenamiento.


#---- Pregunta 3 ----
# ¿Es posible afirmar que, en promedio, los atletas blancos superan 
# a los orientales por menos de 1,16 segundos antes del entrenamiento?

#Hipótesis Nula:Los atletas blancos tienen en promedio un rendimiento
#similar a los atletas orientales antes del entrenamiento
# Ua = Ub

#Hipótesis Alternativa: Los atletas blancos tienen un rendimiento mejor
#que los atletas orientales.
# Ua > Ub

#Para determinar la similitud entre las 2 medias, se utilizara un valor nulo
#este Valor Nulo es de 1.16 como indica el enunciado

valor_nulo_3 <- 1.16

#Se seleccionan los atletas orientales y sus tiempos previos
atletlas_o <- datos %>% filter(Raza == "Oriental")
tiempos_previo_o <- atletlas_o[["Previo"]]

# Se verifica si las muestras se distribuyen de manera cercana 
# a la normal.
normalidad_b <- shapiro.test(tiempos_previo_b)
print(normalidad_b)
normalidad_o <- shapiro.test(tiempos_previo_o)
print(normalidad_o)

#Como los p tienen un valor alto, se aceptan las muestras como similares a
#una distribución estándar.

#El nivel de significación sigue siendo el mismo que antes (0.025)

#Se aplica la prueba t para dos muestras independientes
prueba_t_3 <- t.test(x = tiempos_previo_b,
                     y = tiempos_previo_o,
                     paired = FALSE,
                     alternative = "greater",
                     mu = 1.16,
                     conf.level = 1 - alfa)
print(prueba_t_3)

#Calcular la diferencia entre las medias
media_b <- mean(tiempos_previo_b)
media_o <- mean(tiempos_previo_o)
diferencia_bo <- media_b - media_o

cat("Diferencia de las medias =", abs(diferencia_bo) , "[segundos]\n")


#---- Respuesta 3 ----
# Dado que el valor t obtenido por la prueba t está fuera del intervalo del
# intervalo de confianza la hipótesis nula queda rechazada por lo que la
# hipótesis alternativa sería aceptada. Por otro lado, se obtuvo que la
# diferencia de las medias toma el valor de 3.911156 dado que este valor
# es mayor que los 1.16 segundos la afirmación es correcta.

