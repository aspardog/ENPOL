## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration descriptives
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
## 0.  Pre-settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/Settings.R")

load(paste0(path2SP,"/National/Presentations/INL_MARZO/Input/Indicators_database.RData"))

# filter database ---------------------------------------------------------

Indicators_database <- Indicators_database %>% 
  mutate(eliminar = case_when( na_UAA >= 2 |
         na_GDH >= 2 |
         na_PJ >= 3 ~ 1,
         T~ 0),
         indicator_GDH = round(indicator_GDH, 1),
         indicator_UAA = round(indicator_UAA, 1),
         indicator_PJ = round(indicator_PJ, 1),
         indicator_general = round(indicator_general, 1)) %>% 
  filter(eliminar != 1)

# 1. Histograma  ------------------------------------

HistGraph <- HistGraph.fn("indicator_general", .1 , "Indicador General")
HistGraph

HistGraph <- HistGraph.fn("indicator_GDH", .1 , "Indicador Garantía de Derechos Humanos")
HistGraph


HistGraph <- HistGraph.fn("indicator_UAA", .1 , "Indicador Uso Arbitrario de la Fuerza")
HistGraph

HistGraph <- HistGraph.fn("indicator_PJ", .1 , "Indicador Uso Proceso Justo")
HistGraph



# Barras ------------------------------------------------------------------


