## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Percepcion proceso justo
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Abril 24, 2024
##
## This version:      Abril 24, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  B-1.1.1 Cumplimiento del derecho a contar con información sobre la detención y la acusación durante 
##        el proceso penal para las personas privadas de la libertad en México, 2015 a 2021             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data2plot <-  Main_database %>% 
              filter( Anio_arresto >= 2015, 
                      NSJP = 1) %>%
              mutate(
                guardar_silencio_detencion = 
                  case_when(
                    P3_14_5 == 1 ~ 1,
                    P3_14_5 == 0 ~ 0
                  ),
                guardar_silencio_mp = 
                  case_when(
                    P4_1_04 == 1 ~ 1,
                    P4_1_04 == 2 ~ 0
                  ),
                guardar_silencio_juez = 
                  case_when(
                    P5_2_4 == 1 ~ 1,
                    P5_2_4 == 2 ~ 0
                  )
              )



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  B-1.1.2. Claridad de los actores durante las audiencias             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  B-1.1.3. Cumplimiento de la obligación de informar sobre el derecho a guardar silencio y a no 
##     autoincriminarse en el tiempo para las personas privadas de la libertad en México, 2015 a 2021  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




