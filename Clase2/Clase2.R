library(dplyr) # peprocesamiento de datos
library(hablar) # conversion entre chr, numerico. 
library(ggplot2) # graficos
library(hms) # hora
library(lubridate) # hora
library(leaflet) # mapa
library(readr) # importar csv

# clase 2 


# data frame 1

df1 = data.frame(CustomerId = c(1:6), Product = c("Oven","Television","Mobile","WashingMachine","Lightings","Ipad"))

df1 = df1 %>% as_tibble()

# data frame 2

df2 = data.frame(CustomerId = c(2, 4, 6, 7, 8), State = c("California","Newyork","Santiago","Texas","Indiana")) 

df2 = df2 %>% as_tibble()

#### inner_join function 

inner_join(df1, df2)

inner_join(df1, df2, by = 'CustomerId')

# Full join 

df1 %>% full_join(df2,by="CustomerId")

full_join(df1, df2)

# Left join

df1 %>% left_join(df2)





# otra solcuion MERGE

#### Inner using merge function

merge(x=df1,y=df2,by="CustomerId")

#### Full join using merge function

merge(x=df1,y=df2,by="CustomerId", all = TRUE)

#### left join con merge

merge(x=df1,y=df2,by="CustomerId", all.x =TRUE)

merge(x=df1,y=df2,by="CustomerId", all.y =TRUE)


# Ejemplo

df_primary <- tribble(
  ~ID, ~y,
  "A", 5,
  "B", 5,
  "C", 8,
  "D", 0,
  "F", 9)
df_secondary <- tribble(
  ~ID, ~z,
  "A", 30,
  "B", 21,
  "C", 22,
  "D", 25,
  "E", 29)

?tribble

# Que pasara con: 

df_primary %>% full_join(df_secondary)

df_primary %>% left_join(df_secondary, by = 'ID')

merge(df_primary, df_secondary, by = 'ID')

merge(df_primary, df_secondary, by = 'ID', all.x = TRUE)


# Ejemplo 2 - RENAME!!!

df_3 <- tribble(
  ~ID_, ~y, ~x,
  "A", 5, 10,
  "B", 5, 3,
  "C", 8, 7,
  "D", 0, 4,
  "F", 9, 12)

df_4 <- tribble(
  ~id, ~z, ~o,
  "A", 30, 45,
  "B", 21,40,
  "C", 22,30,
  "D", 25,34,
  "E", 29, 12)

# inner df_3 - df_4 

# left df3 - df4

# Tarea - usar dataframe y tribble para crear unas bases de datos tuyos, y intenta
# usar unos joins. 


############################


# AUSTIN BIKE DATA - COMO PUEDES JUNTAR LA BASE DE Datos?? 

austin_bikeshare_stations <- read_csv("Clase2/austin_bikeshare_stations.csv")

austin_bikeshare_trips <- read_csv("Clase2/austin_bikeshare_trips.csv")

trips = austin_bikeshare_trips %>% select(trip_id, bikeid, duration_minutes, start_time, 
                                  start_station_id, start_station_name, end_station_id, end_station_name)

trips = trips[1:1000,]

##### Date time

trips # que tipo de variable es start time? 

# ya es un date time asi que podemos usar lubridate altiro para sacar mes, hora, dia etc.

trips %>% select(start_time) #yyyy-mm-dd hh:mm:ss

# sacamos la hora? 

trips %>% mutate(hora = lubridate::hour(trips$start_time)) %>% select(start_time,hora)

trips = trips %>% mutate(hora = lubridate::hour(trips$start_time))

#  second(), minute(), hour(), day(), yday(), mday(), wday(), week(), month(), year()

# crea una nueva variable para dia del mes. 

# crea una variable nueva para mes 

# crea una variable nueva para si es un fin de semana o no? 

trips %>% mutate(dia_semana = lubridate::wday(trips$start_time)) %>% select(start_time,dia_semana) %>%
  mutate(fin_de = if_else(dia_semana %in% c(1:5), 0, 1))




# que pasa si no es datetime?? 

trips_100 = trips[1:100,]%>% hablar::convert(chr(start_time))

lubridate::ymd_hms(trips_100$start_time) # podemos convertir a datetime 

# lubridate::mdy_hm()







# Tenemos la base de datos con informacion sobre los viajes en bici. 

# Ahora veamos la informacion sobre las estaciones. 

austin_bikeshare_stations

trips

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

stations_end = stations_end %>% rename(end_station_id = station_id, 
                                           end_station_name = name,
                                       end_station_latitude = latitude,
                                       end_station_longitude = longitude) %>% select(-c(status, location))

trips = trips %>% left_join(stations_end)




###############
# Mapa 

register_google(key = "[]")

bc_bbox <- make_bbox(lat = start_station_latitude, lon = start_station_longitude, data = trips)

bc_big <- get_map(location = bc_bbox, source = "google", maptype = "terrain")

ggmap(bc_big) + 
  geom_point(data = trips, mapping = aes(x = end_station_longitude, y = end_station_latitude), color = 'red')

trips_count = trips %>% group_by(end_station_name) %>% summarise(number = n()) %>% left_join(trips)

ggmap(bc_big) + 
  geom_point(data = trips_count, mapping = aes(x = end_station_longitude, y = end_station_latitude, size = number), color = 'red')

# fin de semana vs dia laboral

test = trips %>% mutate(day = lubridate::wday(start_time)) %>% mutate(fin_de_semana = case_when(day %in% c(6,7) ~ 'Fin De Semana',
                                              TRUE ~ 'Dia Laboral')) %>% 
  group_by(end_station_name, fin_de_semana) %>% summarise(number = n()) %>% arrange(end_station_name) %>% 
  left_join(stations_end %>% select(end_station_name, end_station_latitude, end_station_longitude))


ggmap(bc_big) + geom_point(data = test, mapping = aes(x = end_station_longitude, y = end_station_latitude, size = number, color = fin_de_semana))

# se ve bien? 

ggmap(bc_big) + geom_point(data = test, mapping = aes(x = end_station_longitude, y = end_station_latitude, size = number)) +
  facet_wrap(vars(fin_de_semana)) + ggtitle("Viajes en bici - fin de semana vs 
dia laboral") + xlab("Longitud") + ylab("Latitude") 



etiquetas <- c(
  `Dia Laboral` = "Work day",
  `Fin De Semana` = "Weekend"
)

ggmap(bc_big) + geom_point(data = test, mapping = aes(x = end_station_longitude, y = end_station_latitude, size = number), color = "#232f3f", alpha =0.6) +
  facet_wrap(vars(fin_de_semana), labeller = as_labeller(etiquetas)) + ggtitle("Viajes en bici - fin de semana vs 
dia laboral") + xlab("Longitud") + ylab("Latitude") + labs(size = "Numero de viajes") +
  theme_light()

ggmap(bc_big) + geom_point(data = test %>% filter(number > 15), mapping = aes(x = end_station_longitude, y = end_station_latitude, size = number), color = "#232f3f", alpha =0.6) +
  facet_wrap(vars(fin_de_semana), labeller = as_labeller(etiquetas)) + ggtitle("Viajes en bici - fin de semana vs 
dia laboral") + xlab("Longitud") + ylab("Latitude") + labs(size = "Numero de viajes") +
  theme_light()



















