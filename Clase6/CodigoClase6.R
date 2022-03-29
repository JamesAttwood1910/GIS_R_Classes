# Clase 5

library(chilemapas)
library(dplyr)
library(ggplot2)
library(readxl)

# Cargar bases de datos

DATA_ELECTORAL_4 <- read_excel("Clase5/DATA_ELECTORAL_4.xlsx")

# Usar chile mapas para crear base de datos para poblacion en cada comuna, y geo coordiantos de cada comuna

poblacion = chilemapas::censo_2017_comunas

comunas = chilemapas::mapa_comunas

# Filtrar para solo incluir primera region. 

region = '13'

region = comunas %>% filter(codigo_region == region)

ggplot() +
  geom_sf(data = region$geometry) +
  labs( x = "Longitude", y = "Latitude")


# Agregar un fill? 

fill_1 = poblacion %>% group_by(codigo_comuna) %>% summarise(pob_sum = sum(poblacion)) %>% 
  inner_join(region) 

ggplot() +
  geom_sf(data = fill_1$geometry, aes(fill = fill_1$pob_sum)) +
  labs( x = "Longitude", y = "Latitude")

options(scipen=999)

# Agregar comas a la leyenda? 

ggplot() +
  geom_sf(data = fill_1$geometry, aes(fill = fill_1$pob_sum)) +
  labs( x = "Longitude", y = "Latitude") + scale_fill_gradient(label = scales::comma)

# Cambiar la escala a poblacion en mil?

fill_1_edit = poblacion %>% group_by(codigo_comuna) %>% summarise(pob_sum = sum(poblacion)) %>% 
  inner_join(region) %>% mutate(pob_sum_mil = pob_sum/1000)

ggplot() +
  geom_sf(data = fill_1_edit$geometry, aes(fill = fill_1_edit$pob_sum_mil)) +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "Poblacion (Mil)") 

# los nombres?

fill_1_edit$numeros = rep(1:52)

fill_1_edit %>% View()

fill_1_edit_centroids = cbind(fill_1_edit, st_coordinates(st_centroid(fill_1_edit$geometry))) 

ggplot() +
  geom_sf(data = fill_1_edit_centroids$geometry, aes(fill = fill_1_edit_centroids$pob_sum_mil)) +
  geom_text(data = fill_1_edit_centroids, aes(X, Y, label = fill_1_edit_centroids$numeros), size = 2, color = "white") +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "Poblacion (Mil)") 

# nombres

nombres = chilemapas::codigos_territoriales %>% select(codigo_comuna, nombre_comuna)

fill_1_edit_centroids = fill_1_edit_centroids %>% inner_join(nombres)

fill_1_edit_centroids %>% View()


Santiago_cerrillos = fill_1_edit_centroids %>% filter(nombre_comuna %in% c('Santiago', 'Cerrillos'))

ggplot() +
  geom_sf(data = Santiago_cerrillos$geometry, aes(fill = Santiago_cerrillos$pob_sum_mil)) +
  geom_text(data = Santiago_cerrillos, aes(X, Y, label = Santiago_cerrillos$nombre_comuna), size = 2, color = "white") +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "Poblacion (Mil)") 


### Usar DATA_ELECTORAL_4 para fill mapa con nueva informacion

fill_1_edit_centroids %>% as_tibble()

DATA_ELECTORAL_4 %>% as_tibble()

# tenemos que usar Nombre de comuna  para hacer el join. 
# dos problemas - mayúsculos, tildes

DATA_ELECTORAL_4$COMUNA_2020 = tolower(DATA_ELECTORAL_4$COMUNA_2020) # convertir a minusculas

fill_1_edit_centroids$nombre_comuna = tolower(fill_1_edit_centroids$nombre_comuna) # convertir a minusculas


fill_1_edit_centroids %>% filter(nombre_comuna == 'nunoa')

DATA_ELECTORAL_4 %>% filter(COMUNA_2020 == 'ñuñoa')


DATA_ELECTORAL_4$COMUNA_2020 = chartr("áéíóúñü", "aeiounu", DATA_ELECTORAL_4$COMUNA_2020)

fill_1_edit_centroids$nombre_comuna = chartr('aeiounu','áéíóúñü', fill_1_edit_centroids$nombre_comuna)

DATA_ELECTORAL_4

DATA_ELECTORAL_4 %>% filter(COMUNA_2020 == 'nunoa')

# function  - necesitamos esta funcion despues de remover los tildes. 
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

fill_1_edit_centroids = fill_1_edit_centroids %>% mutate(nombre_comuna_nuevo = firstup(nombre_comuna))

DATA_ELECTORAL_4 = DATA_ELECTORAL_4 %>% mutate(nombre_comuna_nuevo = firstup(COMUNA_2020))

fill_1_edit_centroids %>% filter(nombre_comuna_nuevo == 'Nunoa') %>% select(nombre_comuna_nuevo)

DATA_ELECTORAL_4 %>% filter(nombre_comuna_nuevo == 'Nunoa') %>% select(nombre_comuna_nuevo)



# Elegir columnas en DATA_ELECTORAL_4 de interes

test2 = DATA_ELECTORAL_4 %>% select(nombre_comuna_nuevo, `% Lavín`, `% Sichel`, `% Boric`)

graph = fill_1_edit_centroids %>% inner_join(test2) # hacer el join

graph %>% View()

fill_1_edit_centroids %>% View()

ggplot() +
  geom_sf(data = graph$geometry, aes(fill = graph$`% Sichel`)) +
  geom_text(data = graph, aes(X, Y, label = graph$numeros), size = 2, color = "white") +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "Lavin % del voto") 





## Wide to long para usar facet wrap

# gather(data_wide, nueva_variable_categorica, nueva_variable_numerica, variables_que_se_usan)

library(tidyr)


graph_long = gather(graph, candidato, porcentaje_del_voto, `% Lavín`:`% Boric`, factor_key = TRUE)

graph_long %>% View()

ggplot() +
  geom_sf(data = graph_long$geometry, aes(fill = graph_long$porcentaje_del_voto)) +
  facet_grid(cols = vars(graph_long$candidato)) +
  geom_text(data = graph_long, aes(X, Y, label = graph_long$nombre_comuna), size = 3, color = "black") +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "% del voto") 


# numero etiquetas

graph_long$numeros = rep(1:7, times=3)

ggplot() +
  geom_sf(data = graph_long$geometry, aes(fill = graph_long$porcentaje_del_voto)) +
  facet_grid(cols = vars(graph_long$candidato)) +
  geom_text(data = graph_long, aes(X, Y, label = graph_long$numeros), size = 2, color = "black") +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "% del voto") 






