# Clase 5

library(chilemapas)
library(dplyr)
library(ggplot2)

# Cargar bases de datos

DATA_ELECTORAL_4 <- read_excel("Clase5/DATA_ELECTORAL_4.xlsx")

# Usar chile mapas para crear base de datos para poblacion en cada comuna, y geo coordiantos de cada comuna

poblacion = chilemapas::censo_2017_comunas

comunas = chilemapas::mapa_comunas

# Filtrar para solo incluir primera region. 

region_1 = comunas %>% filter(codigo_region == '01')

ggplot() +
  geom_sf(data = region_1$geometry) +
  labs( x = "Longitude", y = "Latitude")


# Agregar un fill? 

fill_1 = poblacion %>% group_by(codigo_comuna) %>% summarise(pob_sum = sum(poblacion)) %>% 
  inner_join(region_1) 

ggplot() +
  geom_sf(data = fill_1$geometry, aes(fill = fill_1$pob_sum)) +
  labs( x = "Longitude", y = "Latitude")

# Agregar comas a la leyenda? 

ggplot() +
  geom_sf(data = fill_1$geometry, aes(fill = fill_1$pob_sum)) +
  labs( x = "Longitude", y = "Latitude") + scale_fill_gradient(label = scales::comma)

# Cambiar la escala a poblacion en mil?

fill_1_edit = poblacion %>% group_by(codigo_comuna) %>% summarise(pob_sum = sum(poblacion)) %>% 
  inner_join(region_1) %>% mutate(pob_sum_mil = pob_sum/1000)

ggplot() +
  geom_sf(data = fill_1_edit$geometry, aes(fill = fill_1_edit$pob_sum_mil)) +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "Poblacion (Mil)") 

# los nombres? 

nombres = chilemapas::codigos_territoriales %>% select(codigo_comuna, nombre_comuna)

fill_1_edit = fill_1_edit %>% inner_join(nombres)

fill_1_edit %>% View()

fill_1_edit_centroids = cbind(fill_1_edit, st_coordinates(st_centroid(fill_1_edit$geometry)))

fill_1_edit_centroids %>% View()

ggplot() +
  geom_sf(data = fill_1_edit$geometry, aes(fill = fill_1_edit$pob_sum_mil)) +
  geom_text(data = fill_1_edit_centroids, aes(X, Y, label = fill_1_edit_centroids$nombre_comuna), size = 3, color = "white") +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "Poblacion (Mil)") 


### Usar DATA_ELECTORAL_4 para fill mapa con nueva informacion

fill_1_edit_centroids %>% as_tibble()

DATA_ELECTORAL_4 %>% as_tibble()

# tenemos que usar Nombre de comuna  para hacer el join. 
# dos problemas - mayúsculos, tildes

DATA_ELECTORAL_4$COMUNA_2020 = tolower(DATA_ELECTORAL_4$COMUNA_2020) # convertir a minusculas

fill_1_edit_centroids$nombre_comuna = tolower(fill_1_edit_centroids$nombre_comuna) # convertir a minusculas

# Tildes

DATA_ELECTORAL_4$COMUNA_2020_sin_tildes = chartr("áéíóúñü", "aeiounu", DATA_ELECTORAL_4$COMUNA_2020)

DATA_ELECTORAL_4

# Elegir columnas en DATA_ELECTORAL_4 de interes

test2 = DATA_ELECTORAL_4 %>% select(COMUNA_2020_sin_tildes, `% Lavín`, `% Sichel`, `% Boric`)

test2 = test2 %>% rename(nombre_comuna = COMUNA_2020_sin_tildes) # mismo nombre que en fill_1_edit_centroids

graph = fill_1_edit_centroids %>% inner_join(test2) # hacer el join

graph

ggplot() +
  geom_sf(data = graph$geometry, aes(fill = graph$`% Lavín`)) +
  geom_text(data = graph, aes(X, Y, label = graph$nombre_comuna), size = 3, color = "black") +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "Lavin % del voto") 


## Wide to long para usar facet wrap

# gather(data_wide, nueva_variable_categorica, nueva_variable_numerica, variables_que_se_usan)

graph_long = gather(graph, candidato, porcentaje_del_voto, `% Lavín`:`% Boric`, factor_key = TRUE)

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
  geom_text(data = graph_long, aes(X, Y, label = graph_long$numeros), size = 3, color = "black") +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "% del voto") 





