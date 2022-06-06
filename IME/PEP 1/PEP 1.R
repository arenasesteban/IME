# Cargar librerias
library(dplyr)
library(pwr)
library(Hmisc)

# Cargar datos
datos <- read.csv2("PEP 1 Datos.csv", stringsAsFactors = TRUE)

# Obtener muestra
t_muestra <- 300
set.seed(263)
muestra <- sample_n(datos, t_muestra)

# ---- PREGUNTA 1 ----
# Formulacion de hiposis
# H0: No hay diferencia entre la proporcion de ninos explotados en las plantas
# etiquetadoras de Salsacia y Conservia
# p_conservia - p_salsacia = 0
# HA: Hay diferencia entre la proporcion de ninos explotados en las plantas
# etiquetadoras de Salsacia y Conservia
# p_conservia - p_salsacia != 0

# Las observaciones de la muestra son independientes entre si y fueron escogidas
# de manera aleatoria

# Crear tabla de contigencia para la variable "Pais"
contingencia <- table(muestra[["Pais"]])
cat("Tabla de contingencia:")
print(contingencia)
cat("\n")

# Guardar totales en variables
n_conservia <- contingencia[1]
n_salsacia <- contingencia[2]

# Calcular cantidad de exitos
muestra_ninos = muestra %>% count(Pais, Edad < 18)
exitos_conservia = muestra_ninos[2, 3]
exitos_salsacia = muestra_ninos[4, 3]

# Fijar valores conocidos
n <- c(n_conservia, n_salsacia)
exitos <- c(exitos_conservia, exitos_salsacia)
alfa <- 0.05

# Prueba de Wilson en R
prueba <- prop.test(exitos, n = n, alternative = "two.sided",
                    conf.level = 1 - alfa)

print(prueba)

# ---- RESPUESTA PREGUNTA 1 ----
# Con un p valor de 0.005447 se falla en rechazar la hipotesis nula en favor 
# de la hipotesis alternativa, por lo que se puede concluir que hay una
# diferencia entre la proporcion de los ninos explotados en las plantas 
# etiquetadoras de Salsacia y Conservia

# ---- PREGUNTA 2 ----
# Guardar probabilidades
p1 <- prueba[[4]][1]
p2 <- prueba[[4]][2]

# Calcular tamano del efecto
t_efecto <- ES.h(p1, p2)

# Calcular poder estadistico
poder <- pwr.2p2n.test(t_efecto, n_conservia, n_salsacia, alternative = "two.sided")

print(poder)

# ---- RESPUESTA PREGUNTA 2 ----
# El poder estadistico que se obtuvo a partir de la prueba de una diferencia
# de proporciones es de 0.8161139

# ---- PREGUNTA 3 ----
# Calcular fraccion
fraccion <- n_conservia / (n_conservia + n_salsacia)

# Fijar valores conocidos 
poder_p3 <- 0.75
alfa_p3 <- 0.05

# Calcular tamano muestra
resultado <- bsamsize(p1, p2, fraction = fraccion, alfa_p3, poder_p3)

print(resultado)