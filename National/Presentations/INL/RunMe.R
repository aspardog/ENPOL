## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL Presentation
##
## Script:            Presentation preliminary resultas
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 22th, 2024
##
## This version:      January 22th, 2024
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
source("Code/event_study.R")
source("Code/dataViz.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

colors4plot <- c("dependent_cmpl" = "#003B88", 
                 "dependent_var" = "#fa4d57")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Uso Excesivo de la fuerza                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Figura 1.1

data2plot <- lineChartData.fn(dependent_var = "proporcionalidad_uso_fuerza")

lineChart <- lineChartViz(data = data2plot);lineChart

ggsave(plot   = lineChart,
       file   = paste0("Visualizations/Figure1/Figure1_1.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figrua 1.2

data2plot <- logit_dataBase.fn(dependent_var = "proporcionalidad_uso_fuerza")

logitPlot <- logit_demo_panel(mainData = data2plot)

ggsave(plot   = logitPlot,
       file   = paste0("Visualizations/Figure1/Figure1_4.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figrua 1.3

data2plot <- groupBarData.fn(group_var = Corporacion_grupos, prop_var = proporcionalidad_uso_fuerza )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Integridad Personal                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Figura 2.1

data2plot <- lineChartData.fn(dependent_var = "tortura_generalizada")

lineChart <- lineChartViz(data = data2plot)

ggsave(plot   = lineChart,
       file   = paste0("Visualizations/Figure2/Figure2_1.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


## Figura 2.2



## Figura 2.3



## Figrua 2.4



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Libertad                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Figura 3.1

data2plot <- lineChartData.fn(dependent_var = "det_ninguna")

lineChart <- lineChartViz(data = data2plot)

ggsave(plot   = lineChart,
       file   = paste0("Visualizations/Figure3/Figure3_1.svg"), 
       width  = 190, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figura 3.2

data2plot <- logit_dataBase.fn(dependent_var = "det_ninguna")

logitPlot <- logit_demo_panel(mainData = data2plot)

ggsave(plot   = logitPlot,
       file   = paste0("Visualizations/Figure3/Figure3_2.svg"), 
       width  = 175, 
       height = 65,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figura 3.3

data2plot <- simpleBarData.fn(group_var = Primer_lugar_traslado)

## Figrua 3.4

data2plot <- simpleBarData.fn(group_var = Tiempo_traslado)
