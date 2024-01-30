## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Descriptivos 
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    D. Cristina Álvarez Venzor     (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 12th, 2023
##
## This version:      January 12th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Load Settings                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("National/Data_cleaning/Code/settings.R")


load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Set function                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

calcular_porcentaje_y_graficar <- function(data, columnas) {
  porcentajes <- map_dbl(data[columnas], ~mean(.x, na.rm = TRUE) * 100)
  
  # Crear la tabla de porcentajes
  tabla_porcentajes <- tibble(Columna = columnas, Porcentaje = porcentajes) %>%
    arrange(desc(Porcentaje))
  
  # Guardar la tabla de porcentajes en un archivo CSV
  #write.csv(tabla_porcentajes, "porcentajes.csv", row.names = FALSE)
  
  # Devolver la tabla de porcentajes
  return(tabla_porcentajes)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Directory desccriptives                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## a) Debido Proceso ----------------------------------------------------------


### a.1) Legalidad ---------------------------------------------------------------

#Legalidad questions
capacidad_legal <- c("P3_14_5", "P4_1_03", "P4_1_04", "P4_1_13", "P4_1_14", "P4_1_15", "P4_1_16", "P4_6_2",
                     "P4_6A_2", "P5_17_1", "P5_17_2", "P5_17_3", "P5_17_4", "P5_20_4")

Main_database <- Main_database %>% mutate(P4_1_03 = case_when(P4_1_03 == "1" ~ 1,
                                                              P4_1_03 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_1_04 = case_when(P4_1_04 == "1" ~ 1,
                                                              P4_1_04 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_1_13 = case_when(P4_1_13 == "1" ~ 1,
                                                              P4_1_13 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_1_14 = case_when(P4_1_14 == "1" ~ 1,
                                                              P4_1_14 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_1_15 = case_when(P4_1_15 == "1" ~ 1,
                                                              P4_1_15 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_1_16 = case_when(P4_1_16 == "1" ~ 1,
                                                              P4_1_16 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_6_2 = case_when(P4_6_2 == "1" ~ 1,
                                                             P4_6_2 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_6A_2 = case_when(P4_6A_2 == "1" ~ 1,
                                                              P4_6A_2 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P5_17_1 = case_when(P5_17_1 == "1" ~ 1,
                                                              P5_17_1 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P5_17_2 = case_when(P5_17_2 == "1" ~ 1,
                                                              P5_17_2 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P5_17_3 = case_when(P5_17_3 == "1" ~ 1,
                                                              P5_17_3 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P5_17_4 = case_when(P5_17_4 == "1" ~ 1,
                                                              P5_17_4 == "2" ~ 0,
                                                             T ~ NA_real_))
Main_database <- Main_database %>% mutate(P5_20_4 = case_when(P5_20_4 == "1" ~ 1,
                                                              P5_20_4 == "2" ~ 0,
                                                              T ~ NA_real_))


resultados <- calcular_porcentaje_y_graficar(Main_database, capacidad_legal)

labels <- read_xlsx((paste0(path2DB,"National/Exploration/Input/col_names_label.xlsx")))

resultados <- merge(resultados, labels, by.x = "Columna", by.y = "col_name")

# Graficar barras horizontales con ggplot
ggplot(resultados, aes(x = Porcentaje, y = fct_reorder(Lab, Porcentaje))) +
  geom_col(fill = "skyblue") +
  labs(title = "Legalidad",
       x = "Porcentaje",
       y = "") +
  theme_minimal()

### a.2) No discrimnación ---------------------------------------------------------------

#Legalidad questions
no_discriminacion <- c("P3_14_5",
                       "P4_3A_2",
                       "P4_3A_7",
                       "P4_6_1",
                       "P4_6_4",
                       "P4_3A_9",
                       "P4_3A_6",
                       "P4_3A_8")

Main_database <- Main_database %>% mutate(P3_14_5 = case_when(P3_14_5 == "1" ~ 1,
                                                              P3_14_5 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_3A_2 = case_when(P4_3A_2 == "1" ~ 1,
                                                              P4_3A_2 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_3A_7 = case_when(P4_3A_7 == "1" ~ 1,
                                                              P4_3A_7 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_6_1 = case_when(P4_6_1 == "1" ~ 1,
                                                             P4_6_1 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_6_4 = case_when(P4_6_4 == "1" ~ 1,
                                                             P4_6_4 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_3A_9 = case_when(P4_3A_9 == "1" ~ 1,
                                                              P4_3A_9 == "2" ~ 0,
                                                              T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_3A_6 = case_when(P4_3A_6 == "1" ~ 1,
                                                              P4_3A_6 == "2" ~ 0,
                                                             T ~ NA_real_))
Main_database <- Main_database %>% mutate(P4_3A_8 = case_when(P4_3A_8 == "1" ~ 1,
                                                              P4_3A_8 == "2" ~ 0,
                                                              T ~ NA_real_))


resultados <- calcular_porcentaje_y_graficar(Main_database, no_discriminacion)

labels <- read_xlsx((paste0(path2DB,"National/Exploration/Input/col_names_label.xlsx")))

resultados <- merge(resultados, labels, by.x = "Columna", by.y = "col_name", all.x = TRUE)

# Graficar barras horizontales con ggplot
ggplot(resultados, aes(x = Porcentaje, y = fct_reorder(Lab, Porcentaje))) +
  geom_col(fill = "skyblue") +
  labs(title = "Derecho a la No discrimanción",
       x = "Porcentaje",
       y = "") +
  theme_minimal()
