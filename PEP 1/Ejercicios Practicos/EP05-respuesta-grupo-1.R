# EP05 IME
# Esteban Arenas
# Nicolas Torreblanca
# Rodrigo Escobar

# Importar paquetes.
library(dplyr)
library(ggpubr)
library(ggplot2)
library(pwr)

# Se sabe que una máquina que envasa detergentes industriales 
# llena bidones con un volumen de producto que sigue una distribución 
# normal con desviación estándar de 1 litro. 
# Usando una muestra aleatoria de 100 botellas, el ingeniero a cargo 
# de la planta requiere determinar si la máquina está llenando los
# bidones con una media de 10 litros

#---------- Preguntas ----------

#---- Pregunta 1 ----
# Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra 
# presente una media menor a 9,7 litros o mayor a 10,3 litros, 
# ¿cuál es la probabilidad de que cometa un error de tipo I?

# H0: u = 10
# HA: u < 10.3 and u > 9.7

# Primero se fijan los valores conocidos dados por el enunciado

sigma <- 1
alfa <- 0.05
n <- 100
media_nula <- 10

# Luego realizamos el gráfico de la distribución en la que
# calcularemos la zona crítica, cabe destacar que se usarán los valores dados.

x <- seq(6, 14, 0.01)
y <- dnorm(x, mean = media_nula , sd = sigma)
g1 <- ggplot(data = data.frame(x, y), aes(x))

g1 <- g1 + stat_function( fun = dnorm ,
                        args = list(mean = media_nula , sd = sigma),
                        colour = "red", size = 1)
g1 <- g1 + ylab("")
g1 <- g1 + scale_y_continuous(breaks = NULL)
g1 <- g1 + scale_x_continuous(name = "Distribución de Medias [L]", 
                               breaks = seq(6, 14, 1))
g1 <- g1 + theme_pubr()


# Luego de esto, fijamos los valores de la zona crítica
# Calculando la zona crítica utilizando los valores otorgados por el enunciado

q_critico_inferior <- 9.7
q_critico_superior <- 10.3

# Una vez fijadas las zonas críticas
# Se pueden utilizar para colorear la distribución
# Permite visualizar gráficamente cual es el espacio que ocupa en la distribución

df_1 <- data.frame(x=x,y=y)

g1 <- g1 + geom_area(data=subset(df_1 ,x < q_critico_inferior),aes(y = y),
                     colour = "red", fill = "red", alpha = 0.5)

g1 <- g1 + geom_area(data=subset(df_1 ,x > q_critico_superior) ,aes(y = y),
                   colour = "red", fill = "red", alpha = 0.5)

# Se muestra el gráfico, con las zonas rellenadas en pantalla
print(g1)


# Cálculo del área coloreada
# Para realizar el cálculo del area coloreada, obtenemos la probabilidad de que
# la muestra pueda caer en cada intervalo

auc_derecha <- 1 - pnorm(10.3, mean = media_nula, sd = 1, lower.tail = TRUE)
auc_total <- auc_derecha * 2
#print(auc_total)

#---- Respuesta 1 ----
# En este ejercicio se solicita obtener la probabilidad de que se cometa un error
# De tipo I, se desea obtener la probabilidad de que una media sea menor a
# 9.7 y mayor a 10.3. Para obtener esa probabilidad se debe fijar
# una zona crítica en una distribución, y luego calcular la probabilidad
# de que dada una media caiga dentro de aquella zona crítica.

# Luego de realizar los cálculos se obtiene como resultado
# una probabilidad de 0.7641772 de cometer un error de tipo I.


#---- Pregunta 2 ----
# Si el verdadero volumen medio de los bidones fuera de 10,2 litros,
# ¿cuál sería la probabilidad de que el ingeniero, que obviamente 
# no conoce este dato, cometa un error de tipo II?

# HO: u = 10.2
# HA: u =/= 10.2

# Se define el d de Cohen
d <- (10.2 - media_nula)/sigma

# Se procede a calcular el poder estadístico con los datos definidos anteriormente
poder_1 <- power.t.test(n = n,
                      delta = d,
                      sd = sigma,
                      sig.level = alfa,
                      power = NULL,
                      type = 'one.sample',
                      alternative = 'two.sided')

# Se muestran los resultado de la prueba de poder
print(poder_1)

# Se extrae solo el resultado de poder
valor_poder <- poder_1[["power"]]

# Se calcula la probabilidad de cometer un error tipo II
beta <- 1 - valor_poder
print(beta)

#---- Respuesta 2 ----
# Se sabe que el poder estadístico de una prueba de hipótesis se define
# como la probabilidad de rechazar correctamente la hipótesis nula
# cuando esta es falsa, también sabiendo que el poder es equivalente
# a 1 - b, se puede llegar a la conclusión de que b corresponde a 
# la probabilidad de no rechazar la hipótesis en favor de la hipótesis
# alternativa cuando esta es verdadadera, es decir, cometer un error
# de tipo II.

# Luego de calcular el valor b que representa la probabilidad de
# cometer el error de tipo II con 1 - poder, se obtuvo que la
# probabilidad es de 0.4917758


#---- Pregunta 3 ----
# Como no se conoce el verdadero volumen medio, genere un gráfico 
# del poder estadístico con las condiciones anteriores, pero suponiendo
# que el verdadero volumen medio podría variar de 9,5 a 10,5 litros.

# Se grafica la distribución
x <- seq(6, 14, 0.01)
y <- dnorm(x, mean = media_nula , sd = sigma)
df = data.frame(x=x,y=y)
g2 <- ggplot(data = data.frame(x, y), aes(x))

g2 <- g2 + stat_function(
  fun = dnorm ,
  args = list(mean = media_nula , sd = sigma),
  colour = "red", size = 1)
g2 <- g2 + ylab("")
g2 <- g2 + scale_y_continuous(breaks = NULL)
g2 <- g2 + scale_x_continuous(name = "Distribución de Medias [L]",
                            breaks = seq(6, 14, 1))

g2 <- g2 + theme_pubr()

# Se colorean las regiones rojas
media_nula <- 0
Z_critico <- qnorm(alfa/2, mean = media_nula , sd = sigma , lower.tail = FALSE)
q_critico_inferior <- 9.5
q_critico_superior <- 10.5

g2 <- g2 + geom_area(data = subset(df , x < q_critico_inferior), aes(y = y),
                   colour = "red", fill = "red", alpha = 0.5)
g2 <- g2 + geom_area(data = subset(df , x > q_critico_superior), aes(y = y),
                   colour = "red", fill = "red", alpha = 0.5)

# Se grafica la segunda distribución en base a q_critico_inferior
g2 <- g2 + stat_function(
  fun = dnorm ,
  args = list(mean = q_critico_inferior , sd = sigma),
  colour = "blue", size = 1)

#  Se colorean las regiones azules
x1 <- seq(6, 14, 0.01)
y1 <- dnorm(x, mean = q_critico_inferior , sd = sigma)
df1 = data.frame(x=x1,y=y1)
g2 <- g2 + geom_area(data = subset(df1,
                                 x < q_critico_inferior),
                   aes(x = x, y = y),
                   colour = "blue",
                   fill = "blue",
                   alpha = 0.5)

g2 <- g2 + geom_area(data = subset(df1, x > q_critico_superior),
                   aes(x = x, y = y), colour = "blue",
                   fill = "blue",
                   alpha = 0.5)
print(g2)


#---- Pregunta 4 ----
# Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían
# revisarse para conseguir un poder estadístico de 0,75 y un nivel de
# significación de 0,05?

# En esta pregunta se busca cuál debe ser el tamaño de una muestra para 
# que el poder estadístico sea de 0,75

# Se calcula el delta mediante d de cohen
delta_4 = (10.2 - 10) / 1

# Se fija el nivel de significación y poder con los valores dados
poder_4 = 0.75
nivel_significacion_4 = 0.05

# Se deja n como NULL para que la función power lo calcule
resultado_4 <- power.t.test(n = NULL,
                          delta = delta_4,
                          sig.level = nivel_significacion_4,
                          power = poder_4,
                          type = "one.sample" ,
                          alternative = "two.sided" )
# Se muestra por pantalla la cantidad de observaciones resultante 
print (resultado_4)
#Se extrae el valor n 
n_4 <- ceiling(resultado_4[["n"]])
#Se muestra por pantalla el valor extraído
cat ( " Revisiones necesarias = " , n_4 , "\n" )

#---- Respuesta 4 ----
# En base a los cálculos realizados se puede observar 
# que se necesitan 176 revisiones.


#---- Pregunta 5 ----
# ¿Y si el ingeniero fuese muy exigente y quisiera reducir la 
# probabilidad de cometer un error de tipo I a un 1% solamente?

# Se mantienen las mismas variables solo que el nivel de significación
# Disminuye de 0.05 a 0.01
poder_5 = poder_4
delta_5 = delta_4
nivel_significacion_5 = 0.01

# Se calcula la cantidad de observaciones se que se necesitarían
# si se quiere cometer un error de tipo I a un 1%
resultado_5 <- power.t.test(n = NULL,
                            delta = delta_5,
                            sig.level = nivel_significacion_5,
                            power = poder_5,
                            type = "one.sample" ,
                            alternative = "two.sided" )
# Se muestra por pantalla los resultados de la función power
print(resultado_5)

# Se extrae el valor de la cantidad de observaciones
n_5 <- ceiling(resultado_5[["n"]])
# Se muestra por pantalla la cantidad de observaciones
cat ( " Revisiones necesarias = " , n_5 , " \n " )

#---- Respuesta 5 ----
# Se solicita encontrar el N para el cual se tiene un nivel de 
# significación de 0.01, para esto se utiliza la función power.t.test()
# dejando todos los valores con un valor definido exceptuando n, el
# cual es el valor que se busca calcular
# Luego de realizar los cálculos, se puede concluir 
# que se necesitan almenos 268 revisiones.