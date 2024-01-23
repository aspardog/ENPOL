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
colors4bars <- c("sí" = "#003B88", 
                 "no" = "#fa4d57")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Uso Excesivo de la fuerza                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Figura 1.1

data2plot <- lineChartData.fn(dependent_var = "proporcionalidad_uso_fuerza")

lineChart <- lineChartViz(data = data2plot)

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
       file   = paste0("Visualizations/Figure1/Figure1_2_2015.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figrua 1.3

data2plot <- groupBarData.fn(group_var = Corporacion_grupos, prop_var = proporcionalidad_uso_fuerza) 

categories <- c("Operativo Conjunto","Ejército o Marina","Policía Federal", "Policía Federal Ministerial","Policía Estatal",
                "Policía Municipal","Guardia Nacional")

barsPlot <- barsChart.fn(data.df = data2plot, 
                         labels_var = "labels", 
                         value2plot = "value2plot", 
                         grouping_var = "group_var", 
                         categories_grouping_var = categories, 
                         label_figures = "figure", 
                         order = T, order_value = "legend_order", 
                         nbars = 8, 
                         colors4plot = colors4bars)

ggsave(plot   = barsPlot,
       file   = paste0("Visualizations/Figure1/Figure1_3.svg"), 
       width  = 175, 
       height = 125,
       units  = "mm",
       dpi    = 72,
       device = "svg")
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

data2plot <- logit_dataBase.fn(dependent_var = "tortura_generalizada")

logitPlot <- logit_demo_panel(mainData = data2plot)

ggsave(plot   = logitPlot,
       file   = paste0("Visualizations/Figure2/Figure2_2_2015.svg"), 
       width  = 175, 
       height = 65,
       units  = "mm",
       dpi    = 72,
       device = "svg")


## Figura 2.3

data2plot <- simpleBarData.fn(group_var = tortura_lugar)

barChart <- BarSimpleChartVizVertical(fill_colors = c("Ministerio Público" = "#003B88",
                                              "Traslado" = "#99b0cf"))

ggsave(plot   = barChart,
       file   = paste0("Visualizations/Figure2/Figure2_3.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figrua 2.4

data2plot <- simpleBarData.fn(group_var = tortura_tipo)
data2plot <- data2plot %>%
  mutate(order_var = rank(-value2plot, ties.method = "min"))

barChart <- BarSimpleChartViz(fill_colors = c("Física" = "#003B88", 
                                              "Psicológica" = "#99b0cf"))

ggsave(plot   = barChart,
       file   = paste0("Visualizations/Figure2/Figure2_4.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

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
       height = 75,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figura 3.2

data2plot <- logit_dataBase.fn(dependent_var = "det_ninguna")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)

ggsave(plot   = logitPlot,
       file   = paste0("Visualizations/Figure3/Figure3_2_2015.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figura 3.3

data2plot <- simpleBarData.fn(group_var = Primer_lugar_traslado)
data2plot <- data2plot %>%
  mutate(order_var = rank(-value2plot, ties.method = "min"))

barChart <- BarSimpleChartViz(fill_colors = c("Agencia del Ministerio Público" = "#003B88",
                                              "Juez de lo penal" = "#99b0cf",
                                              "Instalación de la policía" = "#003B88",
                                              "Centro de arraigo" = "#99b0cf",
                                              "Centro penitenciario" = "#99b0cf",
                                              "Oficina del gobierno" = "#99b0cf",
                                              "Casa particular" = "#99b0cf",
                                              "Establecimiento comercial" = "#99b0cf",
                                              "Vehículo" = "#99b0cf",
                                              "Terreno baldío" = "#99b0cf",
                                              "Zona militar" = "#99b0cf",
                                              "Centro de detención para migrantes" = "#99b0cf",
                                              "Hospital" = "#99b0cf",                                         
                                              "Otra" = "#99b0cf"))

ggsave(plot   = barChart,
       file   = paste0("Visualizations/Figure3/Figure3_3.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figrua 3.4

data2plot <- simpleBarData.fn(group_var = Tiempo_traslado)
data2plot <- data2plot %>%
  mutate(order_var = case_when(
    category == "Hasta 30 minutos"                    ~ 1,
    category == "Más de 30 minutos hasta 1 hora"      ~ 2,
    category == "Más de 1 hora hasta 2 horas"         ~ 3,
    category == "Más de 2 horas hasta 4 horas"        ~ 4,
    category == "Más de 4 horas hasta 6 horas"        ~ 5,
    category == "Más de 6 horas hasta 24 horas"       ~ 6,
    category == "Más de 24 horas hasta 48 horas"      ~ 7,
    category == "Más de 48 horas hasta 72 horas"      ~ 8,
    category == "Más de 72 horas"                    ~ 8,
    TRUE                                               ~ NA_integer_
  ))

barChart <- BarSimpleChartViz(fill_colors = c("Hasta 30 minutos" = "#003B88",
                                              "Más de 30 minutos hasta 1 hora" = "#99b0cf",
                                              "Más de 1 hora hasta 2 horas" = "#99b0cf",
                                              "Más de 2 horas hasta 4 horas" = "#99b0cf",
                                              "Más de 4 horas hasta 6 horas" = "#99b0cf",
                                              "Más de 6 horas hasta 24 horas" = "#003B88",
                                              "Más de 24 horas hasta 48 horas" = "#99b0cf",
                                              "Más de 48 horas hasta 72 horas" = "#99b0cf",
                                              "Más de 72 horas" = "#99b0cf"))

ggsave(plot   = barChart,
       file   = paste0("Visualizations/Figure3/Figure3_4.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")
