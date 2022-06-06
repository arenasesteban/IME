# EP13 IME
# Nicolás Torreblanca
# Esteban Arenas
# Rodrigo Escobar

# ---- Librerias ----
library(dplyr)
library(ggpubr)
library(car)

# ---- Pregunta 1 ----
# Se pide construir un modelo de regresión lineal múltiple para predecir la
# variable Peso, de acuerdo con las siguientes instrucciones: 

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro 
# dígitos del RUN (sin considerar el dígito verificador) del integrante 
# de menor edad del equipo.

# Se define la semilla del RUN de Esteban Arenas
set.seed(9921)

# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un número 
# par) o 50 hombres (si la semilla es impar).

# Se cargan los datos del archivo
datos <- read.csv2("EP13 Datos.csv")

# Dado que el seed es impar se obtiene una muestra de 50 hombres

# Se seleccionan solo los hombres de los datos
datosHombres <- datos %>% filter(Gender == 1)

# Se define el tamaño de la muestra
tamanoMuestra <- 50

# Se obtiene una muestra de los datos de hombres
muestraHombres <- datosHombres[sample(nrow(datosHombres), tamanoMuestra),]

# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras.

# Se obtiene los nombres de las columnas del dataframe
nombres <- colnames(muestraHombres)

# Se obtienen las 8 variables de forma aleatoria
muestraNombres <- nombres[sample(8)]

# Se genera una muestra de 5 variables de forma aleatoria para el RLM
muestraNombres <- muestraNombres[sample(5)]

# Se seleccionan las columnas con las variables con las que se va a trabajar
muestraHombres <- muestraHombres %>% select(all_of(muestraNombres),Chest.Girth,Height)

# 4. Seleccionar, de las otras variables, una que el equipo considere que
# podría ser útil para predecir la variable Peso, justificando bien esta 
# selección.

# Se selecciona el grosor del pecho para predecir la variable Peso
variableSeleccionada <- nombres[11] #Chest.Girth

# 5. Usando el entorno R, construir un modelo de regresión lineal simple 
# con el predictor seleccionado en el paso anterior.

# Se ajustar el modelo con Chest.Girth como predictor para la variable Height
modeloSimple <- lm(Height ~ Chest.Girth, data = muestraHombres)

# Se muestran los valores obtenidos en el modelo
print(summary(modeloSimple))

# Se grafica el modelo
plotModeloSimple <- ggscatter(muestraHombres, x = "Chest.Girth", y = "Height", color = "blue", fill = "blue",
               xlab = "Grosor [cm]", ylab = "Peso [kg]")

plotModeloSimple <- plotModeloSimple + geom_smooth(method = lm, se = FALSE, colour = "red")
print(plotModeloSimple)

# Se crean gráficos para evaluar el modelo.
#plot(modeloSimple)

# Se seleccionan solo las columnas de interés
muestraHombresSelect <- muestraHombres %>% select(Chest.Girth, Height)

# Se usar el modelo para predecir el peso  y ver los 
# residuos resultantes.
predicciones <- predict(modeloSimple, muestraHombresSelect)
residuos <- muestraHombresSelect$Height - predicciones
muestraHombresSelect <- data.frame(muestraHombresSelect, residuos)

# Se grafican los residuos
plotResiduoSimple <- ggscatter( muestraHombresSelect, x = "Chest.Girth", y = "residuos", color = "blue",
                fill = "blue", xlab = "Grosor [cm]", ylab = "Residuo")

plotResiduoSimple <- plotResiduoSimple + geom_hline(yintercept = 0, colour = "red")
print(plotResiduoSimple)

# 6. Usando herramientas para la exploración de modelos del entorno R,
# buscar entre dos y cinco predictores de entre las variables seleccionadas 
# al azar en el punto 3, para agregar al modelo de regresión lineal 
# simple obtenido en el paso 5.

# Se ajusta el modelo completo.
completo <- lm(Height ~ ., data = muestraHombres)
cat("=== Modelo completo ===\n")
# print(summary(completo))

# Se genera una copia del modelo simple para usar la exploración de datos
# con Selección hacia adelante
copiaModeloSimple <- modeloSimple

# Se ajusta el modelo con selección hacia adelante.
adelante <- step(copiaModeloSimple, scope = list(upper = completo), direction = "forward",
                 trace = 0)
# Se muestran los valores obtenidos en el RLS y en el RLM
cat("=== Modelo Simple ===\n")
print(summary(modeloSimple))
cat("AIC ModeloSimple =", AIC(modeloSimple), "\n\n")
cat("=== Modelo con selección hacia adelante ===\n")
print(summary(adelante))
cat("AIC Adelante =", AIC(adelante), "\n\n")

# 7. Evaluar los modelos y "arreglarlos" en caso de que tengan algún 
# problema con las condiciones que deben cumplir.

# Verificación de las condiciones

# Se comprueba que las variables predictoras son dicotómicas o 
# numéricas a nivel de intervalo y que ninguna de ellas corresponde 
# a una constante.

# Se puede decir que las observaciones son independientes entre si al tratarse de distintos apartados de la anatomía
# humana que no parecen estar siguiendo algún un criterio de selección(más allá de los valores entregados por cada
# variable en los datos de entrada). Del mismo modo, podemos comprobar que la variable dependiente es numérica a nivel
# de intervalo sin restricciones.

# Se verifica la independencia de los Residuos
durbinWatson <- durbinWatsonTest(adelante)

# Dado que al aplicar la prueba de Durbin-Watson se obtuvo un valor p
# de 0.072 el cual es mayor que el nivel de significación de 0.05 se puede
# concluir que los residuos son independientes.

# Se verifica la distribución normal de los residuos
shapiroWilk <- shapiro.test(adelante$residuals)

# Dado que al aplicar la prueba de Shapiro-Wilk se obtuvo un valor p
# de 0.7475 el cual es mayor que el nivel de significación de 0.05 se puede
# concluir que el supuesto de distribución de los residuos se asemeja a la normal

# Se verifica la Homocedasticidad de los residuos
breusch <- ncvTest(adelante)

# Dado que al aplicar la prueba de Breusch-Pagan-Godfrey se obtuvo un valor p
# de 0.62087 el cual es mayor que el nivel de significación de 0.05 se puede
# concluir que el supuesto de distribución de homocedasticidad se cumple.

# Se verifica la Multicolinealidad
vifTest <- vif(adelante)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs:\n")
print(vifTest)
cat("- Tolerancias:\n")
print(1 / vifTest)
cat("- VIF medio:", mean(vifTest), "\n")
    
# Al aplicar la funcion vif se obtuvo que los valores vif de cada variable
# son menores a 10, por lo que no son preocupantes, en tanto a las tolerancias,
# se puede ver que son mayores a 0.4 por lo que los valores deberían ser revisados,
# finalmente, el vif medio de 1.430924 es mayor a 1, por lo que se puede
# concluir que podría haber sesgo en el modelo.

# 8. Evaluar el poder predictivo del modelo en datos no utilizados 
# para construirlo (o utilizando validación cruzada).




















