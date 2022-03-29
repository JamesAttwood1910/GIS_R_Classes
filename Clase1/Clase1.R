# Clase 1

install.packages('dplyr')

library(dplyr) # peprocesamiento de datos
library(hablar) # conversion entre chr, numerico. 
library(ggplot2) # graficos
library(hms) # hora
library(lubridate) # hora
library(leaflet) # mapa
library(readr) # importar csv

# usar files pestanna para abrir tipo de fichero extranjo. 

data()
data('iris')
?dplyr()

# DPLYR primeras funciones: 

data("iris")

iris %>% head()

iris = iris %>% as_tibble()

# Cuantas observaciones tienen un Sepal.Length mayor que 5? 

iris_mayor5 = iris %>% filter(Sepal.Length > 5)

# & 
# |
# Cuantos observaciones tienen un Sepal.lenght mayor que 4.6 y un Petal.Lenght menor que 1.5?

iriscominado = iris %>% filter(Sepal.Length > 4.6 & Petal.Length < 1.5)

# Cuantos observaciones tienen un Sepal.lenght mayor que 4.6 o un Petal.Lenght menor que 1.5?

iristres = iris %>% filter(Sepal.Length > 4.6 | Petal.Length < 1.5)

# Cuantos Sotosa hay?

iris_sotosa = iris %>% filter(Species == 'setosa')

iris %>% group_by(Species) %>% count()

# Cuantos Setosa tienen un Petal lenght mayor que 1.5? 

iris %>% group_by(Species) %>% filter(Petal.Length >1.5)

iris %>% filter(Species == 'setosa' & Petal.Length > 1.5)


# Tarea 1 

# Proponte unas preguntas sobre las columnas en esta base de datos. 

data('diamonds') 

########################----------------------------

# leaflet 

# primer mapa 
leaflet() %>%
  addTiles()

?addTiles
# https://rstudio.github.io/leaflet/ 

# agregar puntos

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-56.19, lat=-34.902, popup="Uruguay") %>% 
  addMarkers(lng = -70, lat = -35, label = 'Chile')


leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-0.1276474, lat=51.5073219)

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-56.19, lat=-34.902, popup="Uruguay") %>% 
  addMarkers(lng = -70, lat = -35, label = 'Chile') %>% setView(lng=-70, lat=-35, zoom = 10)


# Policia data. 

SLC_Police_Cases_2016 <- read_csv("SLC_Police_Cases_2016.csv")

cases = SLC_Police_Cases_2016

cases 

cases %>% nrow() # filas

cases %>% length() # columnas

cases_100 = cases[1:500,]

# mapear datos

leaflet(data= cases_100) %>%
  addTiles() %>%
  addMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords)

# tenemos los puntos, que mas informacion queremos agregar? 

cases_100 %>% head()# veamos las columnas

cases_100 %>% View() # view puede ser util para ver toda la base de datos.

# Agregamos informacion sobre tipo de delito. 

# label
leaflet(data= cases_100) %>%
  addTiles() %>%
  addMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, label = ~`day of week`)

# popup
leaflet(data= cases_100) %>%
  addTiles() %>%
  addMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = ~description)

# agregar mas informacion a la etiqueta? 

# dia de la semana? 

# Que podria ser un problema con dia de la semana (ya que ya tomamos las primeras 500 filas?)

cases_100 %>% View()

cases_100 %>% count(`day of week`)

# Hora? 

cases_100$new_date = lubridate::dmy_hms(cases_100$occurred)

lubridate::hour(cases_100$new_date)

cases_100$time = as_hms(cases_100$new_date)

cases_100$hour = lubridate::hour(cases_100$time)

leaflet(data= cases_100) %>%
  addTiles() %>%
  addMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = 
               paste(cases_100$description, cases_100$hour))

leaflet(data= cases_100) %>%
  addTiles() %>%
  addMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = 
               paste(sep = '<br/>', "<strong>Delito:</strong> ", cases_100$description,
                     "<strong>Hora:</strong> ", cases_100$hour))

# cambiar a palabra y menosculos.

# te doy 3 minutos para intentar encontrar una solucion

# https://es.stackoverflow.com/

# https://stackoverflow.com/ 

cases_100 = cases_100 %>% mutate(descriptionlower = tolower(description))

# datos %>% mutate(dia3 = dia1 + dia2)
?tolittlecase()

leaflet(data= cases_100) %>%
  addTiles() %>%
  addMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = 
               paste(sep = '<br/>', "<strong>Delito:</strong> ", cases_100$descriptionlower,
                     "<strong>Hora:</strong> ", cases_100$hour))

# function 
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

cases_100 = cases_100 %>% mutate(descriptionlower = firstup(descriptionlower)) 

leaflet(data= cases_100) %>%
  addTiles() %>%
  addMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = 
               paste(sep = '<br/>', "<strong>Delito:</strong> ", cases_100$descriptionlower,
                     "<strong>Hora:</strong> ", cases_100$hour))


#tenemos los puntos con la hora y tipo de delito, podemos acercarnos, y alejarnos.... agregar un filtro? 

cases_100 %>% head()

# hacer el mismo mapa pero solo Stolen Vehicle. 

# hacer el mismo mapa pero Stolen vehicle despues de 5 pm . 

# guardar el grafico en una variable, guardar base de datos o poner en data. 

# solo delitos que occurrio a las 2pm. 

dos_pm = cases_100 %>% filter(hour == '2')

leaflet(data= cases_100  %>% filter(hour == '2')) %>%
  addTiles() %>%
  addMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = 
               paste(sep = '<br/>', "<strong>Delito:</strong> ", cases_100$descriptionlower,
                     "<strong>Hora:</strong> ", cases_100$hour))

mapa
# Preguntas hasta aqui??

############# ---------------------------------------

# podemos agregar color?? para mostrar que informacion? tipo de delito? hora?

cases_100 %>% count(descriptionlower) %>% arrange(-n) # 19 tipos de delito. 

# crear nueva variable con 4 tipos y un otro. 

cases_100 = cases_100 %>% 
  mutate(Type = case_when(descriptionlower == "Assault" ~ "Assault",
                          descriptionlower == "Drugs"  ~ "Drugs",
                          descriptionlower == "Public order" ~ "Public order",
                          descriptionlower == "Stolen vehicle" ~ "Stolen vehicle",
                          TRUE ~ 'Other')) 

cases_100 %>% count(Type) %>% arrange(-n)

# Create a palette that maps factor levels to colors
pal <- colorFactor(palette = c("red", "black", "green", "blue", 'pink'), 
                    domain = c('Assault', 'Drugs', 'Other', 'Public order', 'Stolen vehicle') )

# domain va en orden alfabetico. 

leaflet(data= cases_100) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = 
                     paste(sep = '<br/>', "<strong>Delito:</strong> ", cases_100$Type,
                           "<strong>Hora:</strong> ", cases_100$hour),
                   radius = ~ifelse(Type == "Drugs", 15, 5),
                   color = ~pal(Type), fillOpacity = 0.2, stroke = TRUE)


# se ve linde cierto? Con los circulos grandes y otros mas chicos. 
# cual es el problema con este mapa?
# color y tamano estan mostrando la misma informacion. 


leaflet(data= cases_100) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = 
                     paste(sep = '<br/>', "<strong>Delito:</strong> ", cases_100$Type,
                           "<strong>Hora:</strong> ", cases_100$hour),
                   radius = ~ifelse(hour >= 19 & hour <= 23, 15, 5),
                   color = ~pal(Type), fillOpacity = 0.7, stroke = TRUE)

# ahora podemos ver tipo de delito y si hay una diferencia en ubicacion segun la hora. 

# que patrones puedes ver? 

# la calle al nor oeste que es solo en la manana 
# calles principales se identifican

leaflet(data= cases_100 %>% filter(Type == 'Other')) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = 
                     paste(sep = '<br/>', "<strong>Delito:</strong> ", cases_100$Type,
                           "<strong>Hora:</strong> ", cases_100$hour),
                   radius = ~ifelse(hour >= 19 & hour <= 23, 15, 5),
                   color = ~pal(Type), fillOpacity = 0.7, stroke = TRUE)

no_other = cases_100 %>% filter(!Type == 'Other')

leaflet(data= no_other) %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = 
                     paste(sep = '<br/>', "<strong>Delito:</strong> ", no_other$Type,
                           "<strong>Hora:</strong> ", no_other$hour),
                   radius = ~ifelse(hour >= 19 & hour <= 23, 15, 5),
                   color = ~pal(Type), fillOpacity = 0.7, stroke = TRUE)




leaflet(data= cases_100) %>%
  addTiles() %>%
  addMarkers(lng = ~x_gps_coords, lat = ~y_gps_coords, popup = 
               paste(sep = '<br/>', "<strong>Delito:</strong> ", cases_100$Type,
                     "<strong>Hora:</strong> ", cases_100$hour), 
             clusterOptions = markerClusterOptions())


# Hacer algo similar con taxi datos. 
# Agregar puntos (X,Y)
# Agregar informacion en unas etiquetas
# Cluster + Dplyr filter. 
# Como cambian los clusters segun dia de la semana? hora? 
# Filtrar base de datos por fin de semana. Poner colo segun sabado o domingo. Tamano por precio. 
# https://data.mendeley.com/datasets/tm47d7z5xf/2
# https://data.cityofchicago.org/browse?limitTo=datasets
# averiguar como poner una leyenda. 











