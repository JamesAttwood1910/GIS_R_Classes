olddata_wide <- read.table(header=TRUE, text='
 subject sex control cond1 cond2
       1   M     7.9  12.3  10.7
       2   F     6.3  10.6  11.1
       3   F     9.5  13.1  13.8
       4   M    11.5  13.4  12.9
')

olddata_wide

# gather(data_wide, nueva_variable_categorica, nueva_variable_numerica, variables_que_se_usan)

# Todas las otras variables, sus filas se repiteran. 

data_long <- gather(olddata_wide, condition, measurement, control:cond2)






olddata_long <- read.table(header=TRUE, text='
 subject sex condition measurement
       1   M   control         7.9
       1   M     cond1        12.3
       1   M     cond2        10.7
       2   F   control         6.3
       2   F     cond1        10.6
       2   F     cond2        11.1
       3   F   control         9.5
       3   F     cond1        13.1
       3   F     cond2        13.8
       4   M   control        11.5
       4   M     cond1        13.4
       4   M     cond2        12.9
')

olddata_long

# spread(base_datos_long, nombre_columna_con_nombres, nombre_columna_con_valores)

data_wide <- spread(olddata_long, condition, measurement)


# iris

data('iris')

iris 

gather(iris, Medidad, measurement, Sepal.Length:Petal.Width) %>% View()
ggplot() + 
  geom_histogram(aes(x = measurement)) + 
  facet_grid(vars(Medidad))

gather(iris, Medidad, measurement, Sepal.Length:Petal.Width) %>% ggplot() + 
  geom_histogram(aes(x = measurement)) + 
  facet_wrap(vars(Species, Medidad))

# diamonds

# gather(data_wide, nueva_variable_categorica, nueva_variable_numerica, variables_que_se_usan)


diamonds = diamonds %>% select(cut, price, depth, x)

gather(diamonds, tipo, valores, price:x) 

gather(diamonds, tipo, valor, c(price, depth, x)) %>% ggplot() + geom_histogram(aes(x = valor)) + 
  facet_wrap(vars(tipo))




df["Group_B"][df["Group_B"] == 11] <- 77


fill_1_edit_centroids['nombre_comuna_nuevo'][fill_1_edit_centroids['nombre_comuna_nuevo'] == 'Estacion central'] = 'Estacion Central'
7']




