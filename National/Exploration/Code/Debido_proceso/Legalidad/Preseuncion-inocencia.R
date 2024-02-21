## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration data & hipothesys
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres 
##
## Dependencies:      World Justice Project
##
## Creation date:     February 05th, 2024
##
## This version:      February 05th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Pre-settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/Settings.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(prision_preventiva = case_when(
    PPO == 1 ~ "ppo",
    P5_9 == 1 & PPO == 0 ~ "pp",
    T ~ NA_character_))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Set functions                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


exp_data <- function(data, group_var, seccion, nombre) {
  # Create the new data frame
  new_data <- data %>%
    group_by(prision_preventiva, {{ group_var }}) %>%
    summarise(frecuencia = n()) %>%
    ungroup() 
  # Save the data frame to an Excel file
  write.xlsx(as.data.frame(new_data), 
             file      = file.path(paste0(path2SP,
                                          "/National/Exploration/Input/Debido_proceso/legalidad", "/exp_", seccion,".xlsx"),
                                   fsep = "/"),  
             sheetName = paste0(nombre),
             append    = T,
             row.names = F)
}

exp_data_wide <- function(data, group_var, seccion, nombre) {
  # Create the new data frame
  new_data <- data %>%
    group_by(Anio_arresto, {{ group_var }}) %>%
    summarise(frecuencia = n()) %>%
    ungroup() %>% 
    pivot_wider(
      names_from = Anio_arresto,
      values_from = frecuencia
    )
  # Save the data frame to an Excel file
  write.xlsx(as.data.frame(new_data), 
             file      = file.path(paste0(path2SP,
                                          "/National/Exploration/Input/Debido_proceso/legalidad", "/exp_", seccion,".xlsx"),
                                   fsep = "/"),  
             sheetName = paste0(nombre),
             append    = T,
             row.names = F)
}

exp_data_mean <- function(data, target_col, group_col,group_col_1 = NA,seccion, nombre) {
  
  # Compute the mean by the category of the grouping column
  mean_data <- data %>%
    group_by({{ group_col }}, {{ group_col_1 }}) %>%
    summarise(mean_value = mean(as.numeric({{ target_col }}), na.rm = TRUE))
  
  # Save the data frame to an Excel file
  write.xlsx(as.data.frame(mean_data), 
             file      = file.path(paste0(path2SP,
                                          "/National/Exploration/Input/Debido_proceso/legalidad", "/exp_", seccion,".xlsx"),
                                   fsep = "/"),  
             sheetName = paste0(nombre),
             append    = T,
             row.names = F)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Presunción de inocencia                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# 4.1 Defensa adecuada ---------------------------------------------------------------


presuncion <- c("P5_20_1", 
             "P5_20_2",
             "P5_20_3")

Main_database_2008 <- clean_columns.fn(Main_database_2008, presuncion) %>% 
  rename(vidrio  =  P5_20_1,
         esposado = P5_20_2,
         uniforme = P5_20_3) %>% 
  mutate(considera_culpable_aj = case_when(as.numeric(P5_25) == 1 ~ 1,
                                           as.numeric(P5_25) == 2 ~ 0,
                                           as.numeric(P5_25) == 3 ~ 0,
                                           T ~ NA_real_))

## Características de preseunción de inocencia

## PP - vidiro
exp_data(
  data = Main_database_2008,
  group_var = vidrio,
  seccion = "presuncion_inocencia",
  nombre = "vidrio_PP"
)


## PP - esposado
exp_data(
  data = Main_database_2008,
  group_var = esposado,
  seccion = "presuncion_inocencia",
  nombre = "esposado_PP"
)

## PP - uniforme
exp_data(
  data = Main_database_2008,
  group_var = uniforme,
  seccion = "presuncion_inocencia",
  nombre = "uniformeo_PP"
)

## PP - el juez lo consideró culpable antes del juicio
exp_data(
  data = Main_database_2008,
  group_var = considera_culpable_aj,
  seccion = "presuncion_inocencia",
  nombre = "considera_culpable_aj_PP"
)

## el juez lo consideró culpable antes del juicio - maduración del sistema
exp_data_wide(
  data = Main_database_2008,
  group_var = considera_culpable_aj,
  seccion = "presuncion_inocencia",
  nombre = "considera_culpable_aj_años"
)

## el juez lo consideró culpable antes del juicio - maduración del sistema
exp_data_wide(
  data = Main_database_2008,
  group_var = vidrio,
  seccion = "presuncion_inocencia",
  nombre = "vidrio_años"
)

## el juez lo consideró culpable antes del juicio - maduración del sistema
exp_data_wide(
  data = Main_database_2008,
  group_var = uniforme,
  seccion = "presuncion_inocencia",
  nombre = "uniforme_años"
)

## el juez lo consideró culpable antes del juicio - maduración del sistema
exp_data_wide(
  data = Main_database_2008,
  group_var = esposado,
  seccion = "presuncion_inocencia",
  nombre = "esposado_años"
)