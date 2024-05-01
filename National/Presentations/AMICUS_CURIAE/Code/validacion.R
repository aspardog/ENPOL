## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Validation
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres
##
## Dependencies:      World Justice Project
##
## Creation date:     Marzo 11th, 2024
##
## This version:      Marzo 11th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Chart 1                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PP <- Main_database_2008 %>%
  select(P5_9) %>%
  mutate( 
    PP = 
      case_when(
        P5_9 == 1 ~ "Prisión preventiva",
        P5_9 == 2 ~ "En libertad",
        T ~ NA_character_),
    counter = 1
  ) %>%
  group_by(PP) %>%
  summarise(
    prision_preventiva = sum(counter, na.rm = T)
  ) %>%
  drop_na() %>%
  mutate(
    prision_preventiva = prision_preventiva/sum(prision_preventiva, na.rm = T),
    prision_preventiva = paste0(round(prision_preventiva*100,0), "%")
    )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Chart 2                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PP_tipo <- Main_database_2008 %>%
  select(tipo_prision_preventiva) %>%
  mutate(
    counter = 1
  ) %>%
  group_by(tipo_prision_preventiva) %>%
  summarise(
    value = sum(counter, na.rm = T)
  ) %>%
  drop_na() %>%
  mutate(
    value = value/sum(value)*100,
    value = paste0(round(value,0), "%")
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Chart 3                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PP_tiempo <- Main_database_2008 %>%
  mutate(
    PP_tiempo =
    case_when(
      P5_10 < 7 ~ "Hasta 2 años",
      P5_10 > 6 ~ "Más de 2 años",
      P5_10 == 8 ~ NA_character_,
      P5_10 == 9 ~ NA_character_
    ),
    counter = 1 
  ) %>%
  group_by(PP_tiempo) %>%
  summarise(
    value = sum(counter , na.rm = T)
  ) %>%
  drop_na() %>%
  mutate(
    value = value/sum(value)*100,
    value = paste0(round(value, 0), "%")
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Chart 4                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PP_tiempo_tipo <-  Main_database_2008 %>%
  mutate(
    PP_tiempo =
      case_when(
        P5_10 < 7 ~ "Hasta 2 años",
        P5_10 > 6 ~ "Más de 2 años",
        P5_10 == 8 ~ NA_character_,
        P5_10 == 9 ~ NA_character_
      ),
    counter = 1 
  )

PP_tipo_tiempo <- PP_tiempo_tipo %>%
  filter(tipo_prision_preventiva == "Prisión Preventiva Justificada") %>%
  group_by(PP_tiempo)  %>%
  summarise(
    value = sum(counter , na.rm = T)
  ) %>%
  drop_na() %>%
  mutate(
    value = value/sum(value)*100,
    value = paste0(round(value, 0), "%"),
    tipo_prision_preventiva = "Prisión Preventiva Justificada"
  ) %>%
  rbind(
    PP_tiempo_tipo %>%
      filter(tipo_prision_preventiva == "Prisión Preventiva Oficiosa") %>%
      group_by(PP_tiempo)  %>%
      summarise(
        value = sum(counter , na.rm = T)
      ) %>%
      drop_na() %>%
      mutate(
        value = value/sum(value)*100,
        value = paste0(round(value, 0), "%"),
        tipo_prision_preventiva = "Prisión Preventiva Oficiosa"
      )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Chart 5                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PP_defensa_juez <- Main_database_2008 %>%
  select(defensa_juez = P5_1, defensa_mp = P4_1_05) %>%
  mutate(
    defensa_juez =
      case_when(
        defensa_juez == 1 ~ "Abogado juez",
        defensa_juez == 2 ~ "NO abogado juez"
      ),
    defensa_mp = 
      case_when(
        defensa_mp == 1 ~ "Abogado MP",
        defensa_mp == 2 ~ "NO abogado MP"
      ),
    counter = 1
  ) %>%
  group_by(defensa_juez, defensa_mp) %>%
  summarise(
    value = sum(counter , na.rm = T)
  ) %>%
  drop_na() %>%
  mutate(
    value = value/sum(value)*100,
    value = paste0(round(value, 0), "%")
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Chart 6                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PP_defensa_juez <- Main_database_2008 %>%
  mutate(
    procedimiento_abreviado =
      case_when(
        P5_6 == 1 ~ "Juicio",
        P5_6 == 2 ~ "Procedimiento abreviado"
      ),
    counter = 1
  ) %>%
  group_by(procedimiento_abreviado) %>%
  summarise(
    value = sum(counter , na.rm = T)
  ) %>%
  drop_na() %>%
  mutate(
    value = value/sum(value)*100,
    value = paste0(round(value, 0), "%")
  )

