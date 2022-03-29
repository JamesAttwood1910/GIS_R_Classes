## Clases R ##

# Clase 1 #

install.packages('dplyr')

library(dplyr) # peprocesamiento de datos
library(hablar) # conversion entre chr, numerico. 
library(ggplot2) # graficos
library(hms) # hora
library(lubridate) # hora
library(leaflet) # mapa
library(readr) # importar csv

# Primeras 6 observaciones #

bike_data <- read_csv("bike_data.csv")

bike_data %>% head()

# Verla como formato tibble #

bike_data<- bike_data %>% as_tibble()

bike_data

## Aplicar filtros ##

## Observaciones posteriores a 2016 ##

attach(bike_data)

data_mayor16<- bike_data %>% filter(year >= 2016)

## 4244 observaciones son posteriores a 2016

## Observaciones posteriores a 2015 y posteriores a noviembre 

data_posteriores20216_post_noviembre<- bike_data %>% filter(year >= 2016 & month >= 11)

data_posteriores20216_post_noviembre

## Observaciones de subscribe tipo X o Y ##

## ? No pude usar el | para dos variables de caract?res, por ejemplo "peras o manzanas"
 
## Salida del error ##

data_y_o<- bike_data %>% filter(subscriber_type == "Local365" | "24-Hour Kiosk (Austin B-cycle)")

bike_data %>% filter(subscriber_type == "Local365" | "24-Hour Kiosk (Austin B-cycle)")

bike_data %>% filter(subscriber_type %in% c("Local365", "24-Hour Kiosk (Austin B-cycle)"))

bike_data %>% filter(subscriber_type == "Local365" | subscriber_type == "24-Hour Kiosk (Austin B-cycle)")

# wide long 

# como hacer un trasformacion wide a long. 




Error: Problem with `filter()` input `..1`.
i Input `..1` is `subscriber_type == "Local365" | "24-Hour Kiosk (Austin B-cycle)"`.
x solo son posibles operaciones para variables de tipo num?rico, compleja o l?gico
Run `rlang::last_error()` to see where the error occurred.


data_y_o<- bike_data %>% filter(subscriber_type == "Local365")


# leaflet 

# primer mapa 
leaflet() %>%
  addTiles()

?addTiles
# https://rstudio.github.io/leaflet/ 

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-56.19, lat=-34.902, popup="Uruguay") %>% 
  addMarkers(lng = -70, lat = -35, label = 'Chile')

## Ejercicio con Bike Data ##

# Policia data. 

# Cargo Bike_data

Casos<- bike_data

Casos

# Ver el n?mero de filas y columnas #

Casos %>% nrow() # filas

Casos %>% length() # columnas

## Trabajar con un subconjunto de datos ##

Casos_2000 = Casos[1:2000,]


## Mapear datos ##

names(Casos)

library(rgdal)
library(dplyr)

install.packages("Rcpp")

library(leaflet)

 ## Salida_1 ##

leaflet(data = Casos_2000) %>%  
          addTiles() %>%  
          addMarkers(lng = ~longitude, lat = ~latitude)

# label

attach(Casos_2000)

leaflet(data= Casos_2000) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, label = ~ bikeid)

# Popup (no los muestra)

attach(Casos_2000)

leaflet(data= Casos_2000) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, popup = ~as.character(bikeid))


?addMarkers
leaflet(data= Casos_2000) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, label = ~bikeid)
             


  
  