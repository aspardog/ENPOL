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
         NSJP == 1)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Set functions                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


exp_data <- function(data, group_var, seccion, nombre) {
  # Create the new data frame
  new_data <- data %>%
    group_by(sentencia_años, {{ group_var }}) %>%
    summarise(frecuencia = n()) %>%
    ungroup() %>%
    pivot_wider(
      names_from = sentencia_años,
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
## 1.Defensa adecuada                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# 1.1 Defensa adecuada ---------------------------------------------------------------

## Exploración: El tener o no defensa tiene efecto en el número de años de sentencia

defensa <- c("P4_1_05", 
             "P5_1",
             "P5_2_5")

Main_database_2008 <- clean_columns.fn(Main_database_2008, defensa) %>% 
  rename(defensa_mp  =  P4_1_05,
         defensa_antes_juez = P5_1,
         defensa_juez = P5_2_5,
        sentencia_años = P5_4_A) %>% 
  mutate(defensa_momento = case_when(defensa_mp == 1 & defensa_antes_juez == 1 ~ "MP-JUEZ",
                                     defensa_mp == 0 & defensa_antes_juez == 0 ~ "NINGUNO",
                                     defensa_mp == 0 & defensa_antes_juez == 1 ~ "JUEZ",
                                     defensa_mp == 1 & defensa_antes_juez == 0 ~ "MP",
                                     T~ NA_character_),
         procedimiento_abreviado = case_when(P5_6 == 1 ~ 0,
                                             P5_6 == 2 ~ 1,
                                             T ~ NA_real_))


## Años sentencia

## Defesna MP
exp_data(
  data = Main_database_2008,
  group_var = defensa_mp,
  seccion = "defensa_adecuada",
  nombre = "defensa_mp_años_sentencia"
)

# Defensa antes de ver al juez
exp_data(
  data = Main_database_2008,
  group_var = defensa_antes_juez,
  seccion = "defensa_adecuada",
  nombre = "defensa_antes_juez_años_sentencia"
)


# Defensa con el juez
exp_data(
  data = Main_database_2008,
  group_var = defensa_juez,
  seccion = "defensa_adecuada",
  nombre = "defensa_juez_años_sentencia"
)

## Media de sentencia por condición de defensa

# Defensa en MP
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_mp,
  seccion = "defensa_adecuada",
  nombre = "defensa_mp_sentencia_media"
)

# Defensa antes del juez
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_antes_juez,
  seccion = "defensa_adecuada",
  nombre = "defensa_antes_juez_sentencia_media"
)

# Defensa en juez
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_juez,
  seccion = "defensa_adecuada",
  nombre = "defensa_juez_sentencia_media"
)

## Defensa media por condición de defensa y delito único

# Defensa juez
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_juez,
  group_col_1 = Delito_unico_categ,
  seccion = "defensa_adecuada",
  nombre = "defensa_juez_sentencia_media_delito"
)

# Defensa MP
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_mp,
  group_col_1 = Delito_unico_categ,
  seccion = "defensa_adecuada",
  nombre = "defensa_mp_sentencia_media_delito"
)

# Defensa antes del juez
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_antes_juez,
  group_col_1 = Delito_unico_categ,
  seccion = "defensa_adecuada",
  nombre = "defensa_a_juez_sent_media_delito"
)

## Defensa media por condición de defensa y estado

# Defensa juez
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_juez,
  group_col_1 = Estado_arresto,
  seccion = "defensa_adecuada",
  nombre = "defensa_juez_sentencia_media_estado"
)

# Defensa MP
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_mp,
  group_col_1 = Estado_arresto,
  seccion = "defensa_adecuada",
  nombre = "defensa_mp_sentencia_media_estado"
)

# Defensa antes del juez
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_antes_juez,
  group_col_1 = Estado_arresto,
  seccion = "defensa_adecuada",
  nombre = "defensa_a_juez_sent_media_estado"
)


## Defensa media por condición de defensa y defensor público o privado

# Defensa juez
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_juez,
  group_col_1 = abogado_publico,
  seccion = "defensa_adecuada",
  nombre = "defensa_juez_sentencia_media_delito"
)

# Defensa MP
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_mp,
  group_col_1 = Delito_unico_categ,
  seccion = "defensa_adecuada",
  nombre = "defensa_mp_sentencia_media_delito"
)

# Defensa antes del juez
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_antes_juez,
  group_col_1 = Delito_unico_categ,
  seccion = "defensa_adecuada",
  nombre = "defensa_a_juez_sent_media_delito"
)



# Defensa privada o pública -----------------------------------------------

# Defensa antes del juez
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_momento,
  group_col_1 = abogado_publico,
  seccion = "defensa_adecuada",
  nombre = "defensa_momento_publico"
)


# Defensa antes del juez
exp_data_mean(
  data = Main_database_2008,
  target_col = sentencia_años,
  group_col = defensa_momento,
  group_col_1 = procedimiento_abreviado,
  seccion = "defensa_adecuada",
  nombre = "defensa_momento_proabreviado"
)


