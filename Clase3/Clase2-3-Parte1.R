library(dplyr) # peprocesamiento de datos
library(hablar) # conversion entre chr, numerico. 
library(ggplot2) # graficos
library(hms) # hora
library(lubridate) # hora
library(leaflet) # mapa
library(readr) # importar csv
library(ggmap)


############################


# AUSTIN BIKE DATA - COMO PUEDES JUNTAR LA BASE DE Datos?? 

austin_bikeshare_stations <- read_csv("Clase2/austin_bikeshare_stations.csv")

austin_bikeshare_trips <- read_csv("Clase2/austin_bikeshare_trips.csv")

trips = austin_bikeshare_trips %>% select(trip_id, bikeid, duration_minutes, start_time, 
                                  start_station_id, start_station_name, end_station_id, end_station_name)


trips[1:1000, c(1,3,5,6:8)]
trips = trips[1:1000,]

names(austin_bikeshare_stations)

names(austin_bikeshare_trips)

##### Date time

trips # que tipo de variable es start time? 

# ya es un date time asi que podemos usar lubridate altiro para sacar mes, hora, dia etc.

trips %>% select(start_time) #yyyy-mm-dd hh:mm:ss

# sacamos la hora? 

lubridate


trips %>% mutate(hora = lubridate::hour(trips$start_time)) %>% select(start_time,hora)

trips = trips %>% mutate(hora = lubridate::hour(trips$start_time))

#  second(), minute(), hour(), day(), yday(), mday(), wday(), week(), month(), year()

# crea una nueva variable para dia del mes.

day_mes = trips %>% mutate(dia = lubridate::mday(trips$start_time)) %>% select(start_time,dia)


# crea una variable nueva para mes 

# crea una variable nueva para si es un fin de semana o no? 

trips %>% mutate(dia_semana = lubridate::wday(trips$start_time)) %>% select(start_time,dia_semana) %>%
  mutate(fin_de = if_else(dia_semana %in% c(1:5), 0, 1))

# como podemos guardar esta informacion?

trips = trips %>% mutate(dia_semana = lubridate::wday(trips$start_time)) %>%
  mutate(fin_de = if_else(dia_semana %in% c(1:5), 0, 1))




# que pasa si no es datetime?? 

trips_100 = trips[1:100,]%>% hablar::convert(chr(start_time))

trips_100 %>% mutate(dia = lubridate::mday(trips_100$start_time)) %>% select(start_time,dia)



lubridate::ymd_hms(trips_100$start_time) %>% lubridate::wday()# podemos convertir a datetime 

# lubridate::mdy_hm()



# Tenemos la base de datos con informacion sobre los viajes en bici. 

# Ahora veamos la informacion sobre las estaciones. 

austin_bikeshare_stations

trips

names(trips)
names(austin_bikeshare_stations)
# Ambas tienen informacion sobre station_id y nombre? 

# Podemos usar un join? Que tipo de join?

stations_start = austin_bikeshare_stations
stations_end = austin_bikeshare_stations

trips %>% left_join(stations_start)

# Rename

stations_start = stations_start %>% rename(start_station_id = station_id, 
                          start_station_name = name,
                          start_station_latitude = latitude,
                          start_station_longitude = longitude) %>% select(-c(status, location))

trips = trips %>% left_join(stations_start)

# Como puedes hacer lo mismo pero con los end? 

names(trips)
names(stations_end)


stations_end = stations_end %>% rename(end_station_id = station_id, 
                                           end_station_name = name,
                                       end_station_latitude = latitude,
                                       end_station_longitude = longitude) %>% select(-c(status, location))

trips = trips %>% left_join(stations_end)

trips %>% View()



###############
# Mapa 


library(ggmap)

bc_bbox <- make_bbox(lat = start_station_latitude, lon = start_station_longitude, data = trips, f = 0.1)

?make_bbox


bc_big <- get_map(location = bc_bbox)

?get_map
ggmap(bc_big) + 
  geom_point(data = trips, mapping = aes(x = start_station_longitude, y = start_station_latitude), color = 'red', size = 4)


trips_count = trips %>% group_by(start_station_name) %>% summarise(number = n()) %>% left_join(trips)

ggmap(bc_big) + 
  geom_point(data = trips_count, mapping = aes(x = start_station_longitude, y = start_station_latitude, size = number), color = 'red')

# fin de semana vs dia laboral

test = trips %>% mutate(day = lubridate::wday(start_time)) %>% mutate(fin_de_semana = case_when(day %in% c(6,7) ~ 'Fin De Semana',
                                              TRUE ~ 'Dia Laboral')) %>% 
  group_by(start_station_name, fin_de_semana) %>% summarise(number = n()) %>% arrange(start_station_name) %>% 
  left_join(stations_start %>% select(start_station_name, start_station_latitude, start_station_longitude))


ggmap(bc_big) + geom_point(data = test, mapping = aes(x = start_station_longitude, y = start_station_latitude, size = number, color = fin_de_semana))

# se ve bien? 

ggmap(bc_big) + geom_point(data = test, mapping = aes(x = start_station_longitude, y = start_station_latitude, size = number)) +
  geom_text(data = test %>% filter(start_station_name %in% c('Red River & 8th Street', 'San Jacinto & 8th Street')), mapping = aes(x = start_station_longitude, y = start_station_latitude, label = start_station_name), size = 3) +
  facet_wrap(vars(fin_de_semana)) + ggtitle("Viajes en bici - fin de semana vs 
dia laboral") + xlab("Longitud") + ylab("Latitude") 

?geom_text
test$start_station_name


etiquetas <- c(
  `Dia Laboral` = "Work day",
  `Fin De Semana` = "Weekend"
)

ggmap(bc_big) + geom_point(data = test, mapping = aes(x = start_station_longitude, y = start_station_latitude, size = number), color = "#232f3f", alpha =0.6) +
  facet_wrap(vars(fin_de_semana), labeller = as_labeller(etiquetas)) + ggtitle("Viajes en bici - fin de semana vs 
dia laboral") + xlab("Longitud") + ylab("Latitude") + labs(size = "Numero de viajes") +
  theme_light()

theme

ggmap(bc_big) + geom_point(data = test %>% filter(number > 15), mapping = aes(x = start_station_longitude, y = start_station_latitude, size = number), color = "#232f3f", alpha =0.6) +
  facet_wrap(vars(fin_de_semana), labeller = as_labeller(etiquetas)) + ggtitle("Viajes en bici - fin de semana vs 
dia laboral") + xlab("Longitud") + ylab("Latitude") + labs(size = "Numero de viajes") +
  theme_light()

# https://imagecolorpicker.com/ 


















