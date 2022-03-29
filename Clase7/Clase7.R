# Codigo para crear fill_1_edit_centroides

library(chilemapas)
library(dplyr)
library(ggplot2)
library(readxl)
library(sf)

chilemapas::generar_regiones() %>% View()

regiones = chilemapas::generar_regiones()

ggplot() +
  geom_sf(data = regiones$geometry)

DATA_ELECTORAL_4 %>% View()



# `VOTOS TOTAL (2021)` # Sumar

# `% Boric` # Promedio 

# Elegir COMUNA_2020 y dos variables numericas

ELectoral = DATA_ELECTORAL_4 %>% select(COMUNA_2020, `Región nombre`, ID_Región, `% Boric`, `VOTOS TOTAL (2021)`)

# Crear base de datos de comunas
comunas = chilemapas::mapa_comunas

#convertir a tibble

comunas = comunas %>% as_tibble()
comunas = comunas %>% select(-geometry)
regiones = regiones %>% as_tibble()

# unir comunas y regiones
geo = comunas %>% left_join(regiones, by = 'codigo_region')


#crear base de datos de nombres de comuna y region
nombres = chilemapas::codigos_territoriales %>% select(codigo_comuna, nombre_comuna, codigo_region, nombre_region)

# unir geo con nombres
geo = geo %>% left_join(nombres)

ELectoral


# hacer conversion de mayoscula y tildes. 

ELectoral$COMUNA_2020 = tolower(ELectoral$COMUNA_2020)

ELectoral$COMUNA_2020 = chartr("áéíóúñü", "aeiounu", ELectoral$COMUNA_2020)

ELectoral$nombre_comuna_nuevo = tools::toTitleCase(ELectoral$COMUNA_2020)

geo$nombre_comuna = tolower(geo$nombre_comuna) # convertir a minusculas

geo$nombre_comuna_nuevo = tools::toTitleCase(geo$nombre_comuna)

geo = geo %>% select(-c(`Región nombre`, 'ID_Región'))

# Unir geo con datos electorales
geo = geo %>% left_join(ELectoral) %>% select(-c(codigo_provincia, nombre_comuna, COMUNA_2020))

# elegir distintas regiones
regiones = regiones %>% as_tibble()
nombres_regiones = nombres %>% select(codigo_region, nombre_region) %>% distinct()

# agregar nombre de las regiones
regiones = regiones %>% inner_join(nombres_regiones)

# agregar los votos segun region, y unir con regiones
geo_regiones = aggregate(`VOTOS TOTAL (2021)`~nombre_region, FUN = "sum", data = geo) %>% left_join(regiones)


# graficar. 
ggplot() +
  geom_sf(data = geo_regiones$geometry, aes(fill = geo_regiones$`VOTOS TOTAL (2021)`))+
  facet_wrap(vars(geo_regiones$nombre_region)) +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "votos") + xlim(80,60)

# Crear 3 graficos seperados
unique(geo_regiones$nombre_region)

geo_1 = c("Arica y Parinacota", "Tarapaca", "Antofagasta", "Atacama" ,
          "Coquimbo")

geo_2 = c("Valparaiso", "Metropolitana de Santiago", "Libertador General Bernardo OHiggins",
          "Maule", "Biobio", "Nuble")

geo_3 = c("La Araucania", "Los Rios", "Los Lagos", "Aysen del General Carlos Ibanez del Campo",
          "Magallanes y de la Antartica Chilena" )


geo_regiones_1 = geo_regiones %>% filter(nombre_region %in% geo_1)

geo_regiones_1 = cbind(geo_regiones_1, st_coordinates(st_centroid(geo_regiones_1$geometry))) 


geo_regiones_2 = geo_regiones %>% filter(nombre_region %in% geo_2)

geo_regiones_2 = cbind(geo_regiones_2, st_coordinates(st_centroid(geo_regiones_2$geometry))) 


geo_regiones_3 = geo_regiones %>% filter(nombre_region %in% geo_3)

geo_regiones_3 = cbind(geo_regiones_3, st_coordinates(st_centroid(geo_regiones_3$geometry))) 


plot1 = ggplot() +
  geom_sf(data = geo_regiones_1$geometry, aes(fill = geo_regiones_1$`VOTOS TOTAL (2021)`)) + 
  geom_label(data = geo_regiones_1, aes(x = X, y = Y, label = codigo_region), fill = "black", color = 'white', nudge_x = 3) +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "Votos") + 
  scale_fill_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 70))

plot2 = ggplot() +
  geom_sf(data = geo_regiones_2$geometry, aes(fill = geo_regiones_2$`VOTOS TOTAL (2021)`))+
  geom_text(aes(X = 71.34, y = 35, label = ""))
  labs( x = "Longitude", y = "Latitude") + labs(fill = "votos") + xlim(74,69) + ylim(39,32)


plot3 = ggplot() +
  geom_sf(data = geo_regiones_3$geometry, aes(fill = geo_regiones_3$`VOTOS TOTAL (2021)`))+
  geom_label(data = geo_regiones_3, aes(x = X, y = Y, label = codigo_region), fill = "black", color = 'white', nudge_x = 3) +
  labs( x = "Longitude", y = "Latitude") + labs(fill = "Votos") + 
  scale_fill_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 70))


library(gridExtra)
grid.arrange(plot1, plot2, plot3, ncol=3,
             top = "Title")

