poblacion = chilemapas::censo_2017_comunas

comunas = chilemapas::mapa_comunas

RM = comunas %>% filter(codigo_provincia == '131')

ggplot() +
  geom_sf(data = RM$geometry) +
  labs( x = "Longitude", y = "Latitude")

poblacion %>% group_by(codigo_comuna) %>% summarise(pob_sum = sum(poblacion)) %>% 
  inner_join(region) 

fill_2 = poblacion %>% inner_join(RM) %>% group_by(codigo_comuna) %>% summarise(pob_sum = sum(poblacion)) %>%
  inner_join(RM) 

ggplot() +
  geom_sf(data = fill_2$geometry, aes(fill = fill_2$pob_sum)) +
  labs( x = "Longitude", y = "Latitude")


