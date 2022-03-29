# Librerias


library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

# Datos

DATA_ELECTORAL_4 <- read_excel("Clase5/DATA_ELECTORAL_4.xlsx")

write.csv(fill_1_edit_centroids, 'fill_1_edit_centroids.csv')

fill_1_edit_centroids <- read_csv("fill_1_edit_centroids.csv")

#### 

DATA_ELECTORAL_4$COMUNA_2020 = tolower(DATA_ELECTORAL_4$COMUNA_2020) # convertir a minusculas

fill_1_edit_centroids$nombre_comuna = tolower(fill_1_edit_centroids$nombre_comuna) # convertir a minusculas

DATA_ELECTORAL_4$COMUNA_2020 = chartr("áéíóúñü", "aeiounu", DATA_ELECTORAL_4$COMUNA_2020)

# function  - necesitamos esta funcion despues de remover los tildes. 
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

fill_1_edit_centroids = fill_1_edit_centroids %>% mutate(nombre_comuna_nuevo = firstup(nombre_comuna))

DATA_ELECTORAL_4 = DATA_ELECTORAL_4 %>% mutate(nombre_comuna_nuevo = firstup(COMUNA_2020))

########

# Hacer un join para unir fill_1_edit_centroids y DATA_ELECTORAL_4. 

# filtrar la base de datos para solo incluir las comunas de (STG, Provi, Las Condes, Recoleta)

# Graficar los datos con PARTICIPACIÓN_2017 como el fill. 

# Elegir 3 otras variables numericas que quieres mostrar con el fill

# Transformar la base de datos a un formato long con una fila para cada 4 de las variables que quieres mostrar con el fill. 

# Graficar las 4 variables con facet_wrap. 







