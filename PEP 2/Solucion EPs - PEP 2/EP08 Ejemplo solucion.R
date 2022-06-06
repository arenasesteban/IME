# Fijar carpeta de trabajo.

# Cargar paquetes
library(tidyverse)
library(ggpubr)
library(ez)

# Cargar datos
datos <- read.csv2("EP08 Datos.csv", stringsAsFactors = TRUE)

################################################################################
# En este momento, los investigadores buscan determinar si existen diferencias
# en el tiempo que tardan los usuarios en formular una consulta para un problema
# fácil en las áreas de f�sica, qu�mica y biolog�a.
################################################################################

# Puesto que en este caso se requiere inferir acerca de las medias de más de dos
# muestras independientes, se requiere usar un procedimiento ANOVA con las
# siguientes hipótesis:
# H0: El tiempo requerido para formular la consulta asociada a un problema de
#     información de baja dificultad es el mismo para las áreas de f�ica,
#     qu�mica y biolog�a.
# HA: El tiempo requerido para formular la consulta asociada a un problema de
#     información de baja dificultad es diferente para al menos una de las áreas
#     (f�sica, qu�mica y biolog�a).

# Seleccionar datos de interés
datos <- datos %>% filter(dificultad == "Baja")
datos <- datos %>% filter(area == "F�sica" | area == "Qu�mica" | area == "Biolog�a")
datos <- droplevels(datos)
datos[["id"]] <- factor(datos[["id"]])
datos <- datos %>% select(id, area, tiempo)

# Verificación de condiciones
# Puesto que la variable dependiente corresponde al tiempo, éste se mide en una
# escala continua de intervalos iguales.

# El enunciado indica que las observaciones son independientes entre sí, pues
# provienen de individuos diferentes que fueron asignados a cada grupo de manera
# aleatoria.

# Comprobción de normalidad
g <- ggqqplot(datos,
              x = "tiempo",
              y = "area",
              color = "area")

g <- g + facet_wrap(~ area)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# El gráfico generado muestra que la distribución de los datos de cada una de
# las muestras puede considerarse cercana a la normal pues, si bien no forman
# una recta, todos se encuentran dentro de la región aceptable del gráfico Q-Q.

# En cuanto a la condición de homocedasticidad, se posterga su discusión hasta
# ver el resultado de la prueba de Levene efectuada por ezAnova.

# Procedimiento ANOVA
# Puesto que no tenemos hasta ahora motivos que indiquen que los datos podrían
# estar al límite de cumplir las condiciones, consideramos un nivel de
# significación de 0,05.
alfa <- 0.05

cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
omnibus <- ezANOVA(
  data = datos,
  dv = tiempo,
  between = area,
  wid = id,
  return_aov = TRUE)

print(omnibus)
print(summary(omnibus[["aov"]]))

# La prueba de homocedasticidad de Levene entrega un valor p de 0,3090, mayor
# que el nivel de significación. As�, se falla al rechazar la hipótesis nula de
# esta prueba, por lo que tenemos 95% de confianza en que se cumple la condición
# de homocedasticidad.

# El procedimiento ANOVA, por otra parte, entrega un valor p de 0,01 (redondeado
# al segundo decimal), menor que el nivel de significación considerado.
# En consecuencia, rechazamos la hipótesis nula en favor de la hipótesis
# alternativa. As�, la conclusión del procedimiento ómnibus, con una confianza
# de 95%, es que, para al menos una de las tres áreas de f�sica, qu�mica y
# biolog�a, los usuarios requieren de una cantidad de tiempo diferente para
# formular las consultas asociadas a un problema de información de dificultad baja.

# Puesto que el procedimiento ómnibus encuentra diferencias estad�sticamente
# significativas, es necesario realizar un procedimiento post-hoc. Puesto que no
# requerimos hacer contrastes adicionales, usaremos la prueba HSD de Tukey, más
# poderosa que los factores de corrección de Bonferroni y Holm.
post.hoc <- TukeyHSD(omnibus[["aov"]], which = "area", ordered = TRUE,
                     conf.level = 1 - alfa)

print(post.hoc)

# El resultado del procedimiento arroja valores p inferiores al nivel de
# significación para los pares biolog�a-qu�mica y biolog�a-f�sica. En
# consecuencia, podemos concluir con 95% de confianza que los usuarios requieren
# de una cantidad de tiempo diferente para formular la consulta asociada a un
# problema de información de baja dificultad en el área de biolog�a.

# Graficar el tamaño del efecto
efecto <- ezPlot(data = datos, dv = tiempo, wid = id, between = area,
                 y_lab = " Tiempo requerido para formular consulta [s]",
                 x = area)

print(efecto)

# El gráfico del tamaño del efecto nos permite comprobar que los usuarios que
# resuelven un problema de información sencillo en el área de biología requieren
# alrededor de dos segundos más para formular la consulta que sus pares en las
# áreas de física y química.
