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
## 4. Corrupción                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# 4.1 Percepción de corrupción  ---------------------------------------------------------------


corrupcion <- c("P5_20_1", 
                "P5_20_2",
                "P5_20_3")

Main_database_2008 <- clean_columns.fn(Main_database_2008, presuncion) %>% 
  rename(vidrio  =  P5_20_1,
         esposado = P5_20_2,
         uniforme = P5_20_3) %>% 