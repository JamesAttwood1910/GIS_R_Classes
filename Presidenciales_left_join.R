library(dplyr)
primera = Servel_20211121_PRESIDENCIALES_CHILE
segunda = Servel_20211121_PRESIDENCIALES_SEGUNDA_VUELTA_NAC_V2


primera %>% View()

# circele_id
# mesa_descuadrada_preliminar
# mesa_descuadrada_provisorio

primera_ = primera %>% select(circele_id, mesa_id, candidato, votos_preliminar_string) %>% 
  rename(votos_preliminar_string_primera = votos_preliminar_string)

segunda_ = segunda %>% select(circele_id, mesa_id, candidato, votos_preliminar_string)%>% 
  rename(votos_preliminar_string_segunda = votos_preliminar_string)

primera_ %>% left_join(segunda_) %>% View()

