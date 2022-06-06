# ----+ Construir una matriz de datos +----

# Crear un vector de strings y guardarlo en la variable nombre
nombre <- c("Alan",
            "Zacarias",
            "Elsa")

# Crear un vector de fechas y guardarlo en la varible fecha_nombre

fecha_nacimiento <- as.Date(c("2008-10-25", "2006-10-4", "2008-3-27"))

#Crear tres vectores de reales entre 1.0 y 7.0 y guardarlos en prueba_i
# respectivamente
prueba_1 <- c(5.5 , 3.4 , 4.5)
prueba_2 <- c(3.2 , 4.7 , 4.1)
prueba_3 <- c(4.8 , 4.3 , 5.1)

# Construir un data frame a partir de vectores anteriores y guardarlo en
# la variable dataframe
dataframe <- data.frame(nombre,
                        fecha_nacimiento,
                        prueba_1,
                        prueba_2,
                        prueba_3,
                        stringsAsFactors = FALSE)
print(dataframe)

# ----+ Modificacion de una matriz de datos +----

# Eliminar del data frame la columna fecha_nacimiento
dataframe$fecha_nacimiento <- NULL

# Agregar al dataframe la columna edad
dataframe$edad <- c(23, 25, 23)

# Crear una nueva observacion
nueva <- data.frame(nombre = "Elba",
                    prueba_1 = 6.4,
                    prueba_2 = 2.3,
                    prueba_3 = 4.6,
                    edad = 24)

# Agregar la nueva observacion al data frame
dataframe <- rbind(dataframe, nueva)

# Eliminar las primera 3 observaciones al data frame
dataframe <- data.frame[-c(1:3),]

# ----+ Modificacion de una matriz de datos  con paquete dplyr+----
library(dplyr)

# filter() selecciona instancias (filas) de acuerdo a su valor
# arrange() modifica el orden de las filas
# select() permite seleccionar varibales por sus nombre, a las ve que
# las redondea
# mutate() permite agregar nuevas variables que s eobtienen como funciones
# de otras

datos <- iris

# Seleccionar observaciones correspondientes a la especie versicolor
versicolor <- datos %>% filter(Species == "versicolor")

# Seleccionar observaciones de la especie versicolor cuyos sepalos tengan
# una longitud igual o superior a 6
largas <- datos %>% filter(Species == "versicolor" & Sepal.Length >= 6)

# Seleccionar la especie y varibales relativas a los petalos
petalos <- datos %>% select(Species, starts_with("Petal"))

# Seleccionar varibales de anco y la especie
anchos <- datos %>% select(ends_with("Width"), Species)

# Ordenar el conjunto de datos de p é talos en forma descendente segun la razon
# de los petalos .
petalos <- petalos %>% arrange(desc(Petal.Length))

# Ordenar el conjunto de datos de petalos en forma ascendente segun el largo de
petalos <- petalos %>% arrange(Petal.Length)
