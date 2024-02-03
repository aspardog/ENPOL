## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL Hypothesis
##
## Script:            Hypothesis generation
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvarez         (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 30th, 2024
##
## This version:      January 31th, 2024
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
source("Code/settings.R")

Codebook.df <- read_excel("Input/Codebook.xlsx")
load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

data2table <- normalizacion()

vars2estimate <- Codebook.df %>%
  filter(Subindicador %in% "Derecho a una defensa oportuna") %>%
  pull(Variable)

subindicators <- data2table %>%
  select(all_of(paste0(vars2estimate, "_norm"))) %>%
  rowwise() %>%
  mutate(
    sub_indicador = mean(
      c_across(everything()), 
      na.rm = TRUE)
  ) %>%
  ungroup()



# Inicializar una lista para almacenar los resultados de cada subindicador
all_scores_list <- list()

# Iterar a través de cada subindicador
for (subindicator in unique_subindicators) {
  
  # Seleccionar las variables asociadas al subindicador actual
  vars2estimate <- Codebook.df %>%
    filter(Subindicador %in% subindicator) %>%
    pull(Variable)
  
  # Calcular puntajes para el subindicador actual
  subindicator_scores <- data2table %>%
    select(all_of(paste0(vars2estimate, "_norm"))) %>%
    rowwise() %>%
    mutate(
      sub_indicador = mean(
        c_across(everything()), 
        na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(sub_indicador)
  
  # Obtener el nombre del indicador asociado a este subindicador
  indicator_name <- Codebook.df %>%
    filter(Subindicador %in% subindicator) %>%
    pull(Subindicador) %>%
    unique() 
  
  # Renombrar la columna subindicator
  subindicator_scores <- subindicator_scores %>%
    rename(!!indicator_name := sub_indicador)
  
  # Almacenar los resultados en la lista
  all_scores_list[[indicator_name]] <- subindicator_scores
}

# Combinar los resultados de cada subindicador en un marco de datos
all_scores <- bind_cols(all_scores_list) %>%
  mutate(
    across(
      everything(),
      ~if_else(.x %in% NaN, NA_real_, .x)
    )
  ) %>%
  rowwise() %>%
  mutate(
    `Defensa Adecuada` = mean(
      c_across(everything()),
               na.rm = T)
    ) %>%
  ungroup()

scores <- all_scores %>%
  summarise(
    across(
      everything(),
      mean, na.rm = T
    )
  )

# Añadir las columnas al marco de datos principal
base_principal.df <- bind_cols(data2table, all_scores)
