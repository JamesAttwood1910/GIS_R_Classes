# Codigo para crear fill_1_edit_centroides

library(chilemapas)
library(dplyr)
library(ggplot2)
library(readxl)
library(sf)


poblacion = chilemapas::censo_2017_comunas

comunas = chilemapas::mapa_comunas

# Filtrar para solo incluir primera region. 

region = '13'

region = comunas %>% filter(codigo_region == region)

# Sumar la poblacion en cada comuna

fill_1_edit = poblacion %>% group_by(codigo_comuna) %>% summarise(pob_sum = sum(poblacion)) %>% 
  inner_join(region) %>% mutate(pob_sum_mil = pob_sum/1000)

# Numeros para cada comuna 

fill_1_edit$numeros = rep(1:52)

# centroides para cda comuna 

fill_1_edit_centroids = cbind(fill_1_edit, st_coordinates(st_centroid(fill_1_edit$geometry))) 

# Nombres de cada comuna

nombres = chilemapas::codigos_territoriales %>% select(codigo_comuna, nombre_comuna)

fill_1_edit_centroids = fill_1_edit_centroids %>% inner_join(nombres)

