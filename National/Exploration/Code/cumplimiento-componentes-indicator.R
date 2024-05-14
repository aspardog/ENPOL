
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL Hypothesis
##
## Script:            Punchline exploration
##
## Author(s):         Cristina Álvarez Venzor    (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     May 13, 2024
##
## This version:      May 13, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/Settings.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, 
         sentenciado == 1)


# ¿Qué componente se cumplió más en el índice? ----------------------------

# ¿Qué tipo de pruebas ha presentado la parte acusadora contra usted? - procesada

indicator_components <- c("PJ_1",
                       "PJ_2",
                       "PJ_3",
                       "PJ_4",
                       "PJ_5",
                       "PJ_6",
                       "PJ_7",
                       "UAA_1",
                       "UAA_2",
                       "UAA_3",
                       "UAA_4",
                       "GDH_1",
                       "GDH_2")

labels <- c("PJ_1 El defensor te explicó cómo sería tu proceso",
            "PJ_2 El defensor te explicó los hechos por los cuales te acusa",
            "PJ_3 No incriminación por presión ante el MP",
            "PJ_4 Defensa oportuna ante el MP",
            "PJ_5 Juez No consideraba culpable antes del juicio",
            "PJ_6 Tiempo de traslado ante autoridad competente",
            "PJ_7 Juez presente en las audiencias",
            "UAA_1 Personas detenidas con uso proporcional de la autoridad",
            "UAA_2 No Corrupción ante Policía",
            "UAA_3 No Corrupción ante el MP",
            "UAA_4 No Corrupción ante el Juez",
            "GDH_1 No Tortura (Policía & MP)",
            "GDH_2 Detenciones regulares")

Main_database_2008 <- clean_columns.fn(Main_database_2008, indicator_components)

data2plot <- set_data.fn(Main_database_2008, indicator_components, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = rep("#E2E2F7", 13),
                              title = "Cumpliento por componente")
barChart
