# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Piloto de Hipótesis
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##
## Dependencies:      World Justice Project
##
## Creation date:     Jun 4th, 2024
##
## This version:      Jun 5th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


library(stargazer)

# Sociodemograficos

Main_database %>% 
  mutate(
    Educacion_inferior = case_when(
      Educacion_superior == 1 ~ 0,
      Educacion_superior == 0 ~ 1,
      TRUE ~ NA_real_
    )) %>%
  select(Sexo, Educacion_inferior, Color_piel_oscuro, LGBTQ, Etnia, 
         vulnerabilidad_economica, Edad, discapacidad) %>%
  stargazer(as.data.frame(.),type = "text")

base %>% 
  mutate(
    Educacion_inferior = case_when(
      Educacion_superior == 1 ~ 0,
      Educacion_superior == 0 ~ 1,
      TRUE ~ NA_real_
    )) %>%
  select(Sexo, Educacion_inferior, Color_piel_oscuro, LGBTQ, Etnia, 
         vulnerabilidad_economica, Edad, discapacidad) %>%
  stargazer(as.data.frame(.),type = "text")



#Estado


  table(Main_database$Estado_arresto)

  table(base$Estado_arresto)
  
  
#Centro oenitenciario
  
  table(Main_database$NOM_ENT)
  
  table(base$NOM_ENT)
  
  