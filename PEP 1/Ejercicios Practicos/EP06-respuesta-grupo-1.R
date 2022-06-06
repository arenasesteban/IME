#EP06 IME
#Esteban Arenas
#Nicolas Torreblanca 

#Paquetes
library(pwr)
library(Hmisc)

# Se entablan los valores con los cuales se ve a trabajar
Mujeres <- c(54,71,35,30,45,44,56,21,17)

Hombres <- c(52,66,41,42,65,62,88,40,35)

tabla <- as.table(rbind(Mujeres,Hombres))

dimnames (tabla)<-list(sexo = c("Mujeres" , "Hombres"),
                       especialidad = c("Pediatría","Obstetricia","Dermatología ","Psiquiatría",
                                        "Medicina Interna","Oncología","Neurología","Anestesiología","Radiología"))
print(tabla)

# ---- Pregunta 1 ----

# Estudios previos habían determinado que la 
# proporción de autoras en la especialidad de 
# dermatología era de 61%. 
# ¿Respaldan estos datos tal estimación?

#H0: La proporción de autoras en la especialidad 
# dermatología es de 61%
# p = 0.61

#HA: La proporción de autoras en la especialidad 
# dermatología es menor a 61%
# p < 0.61

# Dado que la fuente de los datos es un articulo
# científico se asume que es confiable y que los 
# datos entregados son independientes y aleatorios.

# Fijar valores conocidos

n_derma <- Mujeres[3]+Hombres[3]

mujeres_derma <- Mujeres[3]

p_exito <- mujeres_derma/n_derma

alfa <- 0.05
valor_nulo <- 0.61

#Construccion del intervalo de confianza
error_estandar <- sqrt(p_exito*(1-p_exito)/n_derma)
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
sup <- p_exito + Z_critico * error_estandar
inf <- p_exito - Z_critico * error_estandar
print(inf)
print(sup)

#Prueba de Hipótesis
error_est_hip <- sqrt(( valor_nulo * (1 - valor_nulo)) / n_derma)
Z <- (p_exito - valor_nulo) / error_est_hip
p <- pnorm(Z, lower.tail = FALSE)
cat("Hipótesis alternativa unilateral\n")
cat("Z =", Z, "\n")
cat("p =", p)

# ---- Respuesta 1 ----

# Dado que se obtuvo un valor p de 0.996 el cual es mayor al alfa
# de 0.05 la evidencia no es suficiente para rechazar la 
# hipótesis nula por lo que se concluye con 95% de confianza que 
# no es cierto que la proporción de autoras en la especialidad 
# dermatología es menor a 61%



# ---- Pregunta 2 ----

# Según estos datos, ¿es igual la proporción
# de autoras en las áreas de oncología y medicina interna?

#H0: No hay diferencia entre la proporcion de autoras en las
#áreas de oncología y medicina interna
# p1 - p2 = 0

#HA: Hay diferencia entre la proporcion de autoras en las
#áreas de oncología y medicina interna
# p1 - p2 != 0

#Fijar valores conocidos
n_onco <- Mujeres[6]+Hombres[6]
n_medi <- Mujeres[5]+Hombres[5]

#Exitos 
exitos_onco <- Mujeres[6]
exitos_medi <- Mujeres[5]

# Probabilidades de exito
p_exitos_onco <- exitos_onco/n_onco
p_exitos_medi <- exitos_medi/n_medi

valor_nulo_2 <- 0

# Estimar diferencia
diferencia <- p_exitos_onco - p_exitos_medi

# Construcción de intervalo de confianza
error_onco <- (p_exitos_onco * (1 - p_exitos_onco)) / n_onco
error_medi <- (p_exitos_medi * (1 - p_exitos_medi)) / n_medi

error_est_2 <- sqrt(error_onco + error_medi)

z_critico_2 <- qnorm(alfa / 2, lower.tail = FALSE)

inf_2 <- diferencia - z_critico_2 * error_est_2
sup_2 <- diferencia + z_critico_2 * error_est_2


#Prueba de Hipótesis
p_agrupada <- (exitos_medi+exitos_onco)/(n_medi+n_onco)

error_onco_agrup <- (p_agrupada * (1-p_agrupada)) / n_onco
error_medi_agrup <- (p_agrupada * (1-p_agrupada)) / n_medi

error_est_hip_2 <- sqrt(error_onco_agrup + error_medi_agrup)


Z_2 <- (diferencia - valor_nulo_2) / error_est_hip_2

p_2 <- 2 * pnorm(Z_2, lower.tail = FALSE)
  
cat("Hipótesis alternativa bilateral\n")
cat("Z =", Z_2, "\n")
cat("p =", p_2)

# ---- Respuesta 2 ----
# Dado que se obtuvo un valor p de 0.929 el cual es mayor al
# valor de alfa de 0.05 por lo que se falla en rechazar la hipótesis
# nula, por lo que podemos concluir con 95% de confianza que 
# no hay diferencia entre la proporción de autoras en las
# áreas de oncología y medicina interna.


# ---- Pregunta 3 ---- 

# Suponiendo que la diferencia en la proporción 
# de autoras en la especialidad de obstetricia y 
# la de neurología es de 0,19. ¿A cuántos autores 
# deberíamos monitorear para obtener un intervalo de 
# confianza del 97,5% y poder estadístico de 75%, 
# si se intenta mantener aproximadamente la misma 
# proporción de gente estudiada en cada caso?

#Fijar valores conocidos
n_obs <- Mujeres[2]+Hombres[2]
n_neuro <- Mujeres[7]+Hombres[7]

#Exitos 
exitos_obs <- Mujeres[2]
exitos_neuro <- Mujeres[7]

#Probabilidades de exito
p_exitos_obs <- exitos_obs/n_obs
p_exitos_neuro <- exitos_neuro/n_neuro

fraccion <- n_obs/(n_obs + n_neuro)

# Con la prueba de Wilson, Se calcula el tamaño de las muestras 
# con intervalo de confianza del 97,5% y poder estadístico de 75%
resultado <- bsamsize(p_exitos_obs, p_exitos_neuro, 
                      fraction = fraccion, alpha = 0.025,
                      power = 0.75)
print(resultado)

# ---- Respuesta 3 ----
# A partir de los resultados obtenidos mediante
# la prueba de Wilson podemos concluir que con un intervalo de
# 97.5% y poder estadístico de 75% debemos monitorear 245 autores
# obstetricia y 257 de neurología para mantener la misma
# proporción de gente estudiada.














