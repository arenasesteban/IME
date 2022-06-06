# EP11 IME
# Nicolas Torreblanca
# Esteban Arenas
# Rodrigo Escobar

# ---- Librerias ----
library(dplyr)
library(ggpubr)
library(simpleboot)
library(boot)

# ---- Definición de Funciones ----
# Función para obtener una permutación.
# Argumentos:
# - i: iterador (para llamadas posteriores).
# - muestra_1, muestra_2: muestras.
# Valor:
# - lista con las muestras resultantes tras la permutación.
obtiene_permutacion <- function(i, muestra_1, muestra_2) {
  n_1 <- length(muestra_1)
  combinada <- c(muestra_1, muestra_2)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace = FALSE)
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1, nueva_2))
}

# Función para calcular la diferencia de un estadístico de interés entre las
# dos muestras.
# Argumentos:
# - muestras: lista con las muestras.
# - FUN: nombre de la función que calcula el estadístico de interés.
# Valor:
# - diferencia de un estadístico para dos muestras.
calcular_diferencia <- function(muestras, FUN) {
  muestra_1 <- muestras[[1]]
  muestra_2 <- muestras[[2]]
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# Función para calcular el valor p.
# Argumentos:
# - distribucion: distribución nula del estadístico de interés.
# - valor_observado: valor del estadístico de interés para las muestras
#   originales.
# - repeticiones: cantidad de permutaciones a realizar.
# - alternative: tipo de hipótesis alternativa. "two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales.
# Valor:
# - el valorp calculado.
calcular_valor_p <- function(distribucion, valor_observado,
                             repeticiones, alternative) {
  if(alternative == "two.sided") {
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if(alternative == "greater") {
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else {
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  return(valor_p)
}

# Función para graficar una distribución.
# Argumentos:
# - distribucion: distribución nula del estadístico de interés.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot.
graficar_distribucion <- function(distribucion, ...) {
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadístico de interés",
                            ylab = "Frecuencia", bins = 30, ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  # Crear una única figura con todos los gráficos de dispersión.
  figura  <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

# Función para hacer la prueba de permutaciones.
# Argumentos:
# - muestra_1, muestra_2: vectores numéricos con las muestras a comparar.
# - repeticiones: cantidad de permutaciones a realizar.
# - FUN: función del estadístico E para el que se calcula la diferencia.
# - alternative: tipo de hipótesis alternativa. "two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales.
# - plot: si es TRUE, construye el gráfico de la distribución generada.
# - ...: otros argumentos a ser entregados a graficar_distribucion.
contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, ...) {
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(list(muestra_1, muestra_2), FUN)
  cat("Valor observado:", observado, "\n")
  
  n_1 <- length(muestra_1)
  
  # Generar permutaciones.
  permutaciones <- lapply(1:repeticiones, obtiene_permutacion, muestra_1,
                          muestra_2)
  
  # Generar la distribución.
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  # Graficar la distribución.
  if(plot) {
    graficar_distribucion(distribucion, ...)
  }
  
  # Calcular el valor p.
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              alternative)
  
  cat("Valor p:", valor_p, "\n\n")
}

# ---- Pregunta 1
# Propongan una pregunta de investigación original, que 
# involucre la comparación de las medias de dos grupos 
# independientes (más abajo se dan unos ejemplos).
# Fijando una semilla propia, seleccionen una muestra aleatoria 
# de hogares (250 < n < 500) y respondan la pregunta propuesta 
# utilizando una simulación Monte Carlo.

# ---- Enunciado Creado 1 ----
# Se dice que el promedio de ingresos per cápita de las mujeres de 
# la Región Metropolitana es igual al de las mujeres de 
# la Región del Biobío

# ---- Definición de Hipótesis ----
# Hipótesis Nula: En promedio, los ingresos per cápita de las mujeres 
# de la Región Metropolitana de Santiago y de las mujeres de la 
# Región del Biobío son iguales.
# uM - uB = 0

# Hipótesis Alternativa: En promedio, los ingresos per cápita de las mujeres 
# de la Región Metropolitana de Santiago y de las mujeres de la 
# Región del Biobío son diferentes.
# uM - uB != 0

# Se define un nivel de significación de 0.05
# alfa <- 0.05

# Lectura de datos
datos <- read.csv2("EP11 Datos.csv")

# Se define una semilla
set.seed(666)

# Se define el tamaño de la muestra
tamanoMuestra1 <- 300

# Se obtiene una muestra de los datos
muestra <- datos[sample(nrow(datos), tamanoMuestra1),]

# Se calcula el ingreso per capita de cada vivienda y se guardan 
# los resultados en una nueva variable ingresoPerCapita
muestra <- muestra %>% mutate(ytotcorh, numper, 
                          ingresoPerCapita = ytotcorh/numper)

# Se obtienen los datos de mujeres de la Región del Biobío
biobio <- muestra %>% filter(region == "Región del Biobío", 
                             sexo == "Mujer")

# Se obtienen los datos de mujeres de la Región Metropolitana de Santiago
metropolitana <- muestra %>% filter(region == "Región Metropolitana de Santiago", 
                                    sexo == "Mujer")

# Se seleccionan solo las columnas de id e ingresoPerCapita de cada tabla
mujerBiobioIngreso <- biobio %>% select(id.vivienda, ingresoPerCapita)
mujerMetropolitanaIngreso <- metropolitana %>% select(id.vivienda, ingresoPerCapita)

# Se pasan los datos de las columnas a vectores para 
# poder operar sobre ellos
vecBiobio <- mujerBiobioIngreso$ingresoPerCapita
vecMetropolitana <- mujerMetropolitanaIngreso$ingresoPerCapita

# Se establece la cantidad de repeticiones.
R = 5999

# Se realiza la prueba de permutaciones para la media.
contrastar_hipotesis_permutaciones(vecBiobio, vecMetropolitana, repeticiones = R, FUN = mean,
                                   alternative = "two.sided", plot = TRUE,
                                   color = "blue", fill = "blue")

# ---- Respuesta 1 ----
# Luego de realizar la prueba de permutaciones para comparar una variable 
# continua en dos muestras independientes se obtuvo un valor p de 0.03416667 
# el cual corresponde a un valor menor que el nivel de significación definido
# de 0.05, es por esto que se logra rechazar la hipótesis nula y se acepta 
# la hipótesis alternativa, esto quiere decir que, en promedio, los ingresos per cápita de las mujeres 
# de la Región Metropolitana de Santiago y de las mujeres de la 
# Región del Biobío son diferentes.


# ---- Pregunta 2 ----
# Propongan una pregunta de investigación original, que 
# involucre la comparación de las medias de más de dos grupos 
# independientes (más abajo se dan unos ejemplos). Fijando una 
# semilla distinta a la anterior, seleccionen una muestra 
# aleatoria de hogares (400 < n < 600) y respondan la pregunta
# propuesta utilizando bootstrapping. Solo por ejercicio académico,
# aplique un análisis post-hoc con bootstrapping aunque este no 
# sea necesario.

# ---- Enunciado Creado 2 ----
# Se dice que el promedio de ingresos per cápita entre las regiones de Atacama, Tarapacá
# y Antofagasta son iguales.

# ---- Definición de Hipótesis ----
# Hipótesis Nula: En promedio, el ingreso per cápita en la Región de Atacama, 
# de Tarapacá y de Antofagasta son iguales. Por lo que las diferencias de
# las medias son igual a 0.

# Hipótesis Alternativa: En promedio, el ingreso per cápita en la Región de Atacama, 
# de Tarapacá y de Antofagasta en al menos una de ellas es distinto. Por lo 
# que las diferencias de las medias son distintas a 0.

# Se define una semilla para este caso
set.seed(11)

# Se define un tamaño de muestra para este caso
tamanoMuestra2 <- 520

# Se toma una muestra de los datos originales
muestra2 <- datos[sample(nrow(datos), tamanoMuestra2),]

# Se calcula el ingreso per capita de cada vivienda y se guardan 
# los resultados en una nueva variable ingresoPerCapita
muestra2 <- muestra2 %>% mutate(ytotcorh, numper, 
                              ingresoPerCapita = ytotcorh/numper)

# Se obtienen los datos por región
atacama <- muestra %>% filter(region == "Región de Atacama")
tarapaca <- muestra %>% filter(region == "Región de Tarapacá")
antofagasta <- muestra %>% filter(region == "Región de Antofagasta")

# Se pasan los datos de las columnas a vectores para 
# poder operar sobre ellos
vecAtacama <- atacama$ingresoPerCapita
vecTarapaca <- tarapaca$ingresoPerCapita
vecAntofagasta <- antofagasta$ingresoPerCapita

# Se obtiene la cantidad de datos de cada muestra
n_atacama <- length(vecAtacama)
n_tarapaca <- length(vecTarapaca)
n_antofagasta <- length(vecAntofagasta)

# Comprobar normalidad de las muestras.
print(shapiro.test(vecAtacama))
print(shapiro.test(vecTarapaca))
print(shapiro.test(vecAntofagasta))

# Se definen 3 combinaciones posibles de Bootstrapping de 2 muestras independientes
# [1] Atacama - Tarapacá
# [2] Atacama - Antofagasta
# [3] Antofagasta - Tarapacá

# Se genera un dataframe por combinación
# [1]
region1 <- c(rep("Región de Atacama", n_atacama), rep("Región de Tarapacá", n_tarapaca))
ingreso1 <- c(vecAtacama, vecTarapaca)
datos1 <- data.frame(region1, ingreso1)
# [2]
region2 <- c(rep("Región de Atacama", n_atacama), rep("Región de Antofagasta", n_antofagasta))
ingreso2 <- c(vecAtacama, vecAntofagasta)
datos2 <- data.frame(region2, ingreso2)
# [3]
region3 <- c(rep("Región de Antofagasta", n_antofagasta), rep("Región de Tarapacá", n_tarapaca))
ingreso3 <- c(vecAntofagasta, vecTarapaca)
datos3 <- data.frame(region3, ingreso3)

# Se calculan las diferencias observada entre las medias muestrales
# para las 3 combinaciones
media_Atacama <- mean(vecAtacama)
media_Tarapaca <- mean(vecTarapaca)
media_Antofagasta <- mean(vecAntofagasta)

diferencia_1 <- media_Atacama - media_Tarapaca
diferencia_2 <- media_Atacama - media_Antofagasta
diferencia_3 <- media_Antofagasta - media_Tarapaca

# Se establece el nivel de significación.
# alfa <- 0.05

# Se define un valor nulo
valor_nulo <- 0

# Se crea la distribución bootstrap para las 3 combinaciones
B <- 9999

distribucion_bootstrap1 <- two.boot(vecAtacama, vecTarapaca, 
                                    FUN = mean, R = B)
distribucion_bootstrap2 <- two.boot(vecAtacama, vecAntofagasta, 
                                    FUN = mean, R = B)
distribucion_bootstrap3 <- two.boot(vecAntofagasta, vecTarapaca, 
                                    FUN = mean, R = B)

desplazamiento1 <- mean(distribucion_bootstrap1[["t"]]) - valor_nulo
distribucion_nula1 <- distribucion_bootstrap1[["t"]] - desplazamiento1

desplazamiento2 <- mean(distribucion_bootstrap2[["t"]]) - valor_nulo
distribucion_nula2 <- distribucion_bootstrap2[["t"]] - desplazamiento2

desplazamiento3 <- mean(distribucion_bootstrap3[["t"]]) - valor_nulo
distribucion_nula3 <- distribucion_bootstrap3[["t"]] - desplazamiento3

# Se determina el valor p para cada combinación
p1 <- (sum(abs(distribucion_nula1) > abs(diferencia_1)) + 1) / (B + 1)
cat("Valor p1:", p1)

p2 <- (sum(abs(distribucion_nula2) > abs(diferencia_2)) + 1) / (B + 1)
cat("Valor p2:", p2)

p3 <- (sum(abs(distribucion_nula3) > abs(diferencia_3)) + 1) / (B + 1)
cat("Valor p3:", p3)

# ---- Análisis Post Hoc ----
#{Aqui iría el Analisis Post Hoc}

# ---- Respuesta 2 ----
# Luego de realizar el proceso de comparación mediante bootstraping
# para cada combinación de muestras con un valor nulo de 0,
# se obtuvieron los siguientes valores p, para el caso [1] se 
# obtuvo un valor p de 0.903, en tanto al caso [2] se obtuvo
# un valor p de 0.1394 y finalmente para el caso [3] se obtuvo
# un p de 0.1783, dado que los tres valores p son mayores que el
# nivel de significación de 0.05, no se logra rechazar la hipótesis
# nula, por lo que los datos no evidencian una diferencia entre
# ellos, es por esto se debería proceder a realizar pruebas post hoc.
# La prueba post hoc a utilizar debería ser para muestras independientes.

