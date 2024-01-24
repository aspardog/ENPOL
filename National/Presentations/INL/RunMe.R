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

colors4bars <- c("sí" = "#003B88", 
                 "no" = "#fa4d57")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Uso Excesivo de la fuerza                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Figura 1.1
colors4plot <- c("dependent_var" = "#003B88", 
                 "dependent_cmpl" = "#fa4d57")

data2plot <- lineChartData.fn(dependent_var = "proporcionalidad_uso_fuerza")

lineChart <- lineChartViz(data = data2plot)

ggsave(plot   = lineChart,
       file   = paste0("Visualizations/Figure1/Figure1_1.svg"), 
       width  = 190, 
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
                "Policía Municipal")

barsPlot <- barsChart.fn(data.df = data2plot, 
                         labels_var = "labels", 
                         value2plot = "value2plot", 
                         grouping_var = "group_var", 
                         categories_grouping_var = categories, 
                         label_figures = "figure", 
                         order = T, order_value = "legend_order", 
                         nbars = 7, 
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

colors4plot <- c("dependent_cmpl" = "#003B88", 
                 "dependent_var" = "#fa4d57")

data2plot <- lineChartData.fn(dependent_var = "tortura_generalizada")

lineChart <- lineChartViz(data = data2plot)

ggsave(plot   = lineChart,
       file   = paste0("Visualizations/Figure2/Figure2_1.svg"), 
       width  = 190, 
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
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


## Figura 2.3

data2plot <- simpleBarData.fn(group_var = tortura_lugar)

barChart <- BarSimpleChartVizVertical(fill_colors = c("Ministerio Público" = "#003B88",
                                              "Traslado" = "#E2E2F7"))

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
                                              "Psicológica" = "#E2E2F7"))

ggsave(plot   = barChart,
       file   = paste0("Visualizations/Figure2/Figure2_4.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figura 2.4 - Final

data2plot <- groupBarData.fn(group_var = tortura_tra, prop_var = RND_3 ) 
data2plot <- data2plot %>% mutate(group_var = case_when(
  group_var == "sí"                    ~ "Después del RND",
  group_var == "no"      ~ "Antes del RND",
  T ~ NA))
data2plot <- data2plot %>% mutate(legend_order = case_when(
  legend_order == 1                    ~ 3,
  legend_order == 2      ~ 2,
  legend_order == 3      ~ 1,
  T ~ NA))
colors4bars <- c("Después del RND" = "#003B88", 
                 "Antes del RND" = "#fa4d57")

categories <- c("Física", "Psicológica", "Ambas")

barsPlot <- barsChart.fn(data.df = data2plot, 
                         labels_var = "labels", 
                         value2plot = "value2plot", 
                         grouping_var = "group_var", 
                         categories_grouping_var = categories, 
                         label_figures = "figure", 
                         order = F, order_value = "legend_order", 
                         nbars = 3, 
                         colors4plot = colors4bars)

ggsave(plot   = barsPlot,
       file   = paste0("Visualizations/Figure2/Figure2_4.svg"), 
       width  = 175, 
       height = 125,
       units  = "mm",
       dpi    = 72,
       device = "svg")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Libertad                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Figura 3.1

colors4plot <- c("dependent_cmpl" = "#003B88", 
                 "dependent_var" = "#fa4d57")

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

## Figrua 3.3

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
                                              "Más de 30 minutos hasta 1 hora" = "#E2E2F7",
                                              "Más de 1 hora hasta 2 horas" = "#E2E2F7",
                                              "Más de 2 horas hasta 4 horas" = "#E2E2F7",
                                              "Más de 4 horas hasta 6 horas" = "#E2E2F7",
                                              "Más de 6 horas hasta 24 horas" = "#003B88",
                                              "Más de 24 horas hasta 48 horas" = "#E2E2F7",
                                              "Más de 48 horas hasta 72 horas" = "#E2E2F7",
                                              "Más de 72 horas" = "#E2E2F7"))

ggsave(plot   = barChart,
       file   = paste0("Visualizations/Figure3/Figure3_3.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figura 3.4


data2table <- pivot_vars.fn(data = Main_database, variables = "Tiempo_traslado")

data2plot1 <- lineChartData.fn(data = data2table, dependent_var = "Tiempo_traslado_Hasta 30 minutos") %>%
  filter(category %in% c("dependent_var"))

data2plot2 <- lineChartData.fn(data = data2table, dependent_var = "Tiempo_traslado_Más de 6 horas hasta 24 horas") %>%
  filter(category %in% c("dependent_var")) %>%
  mutate(
    category = 
      case_when(
        category == "dependent_var" ~ "dependent_cmpl"
        )
  )

data2plot <- bind_rows(data2plot1, data2plot2)

colors4plot <- c("dependent_var" = "#003B88", 
                 "dependent_cmpl" = "#fa4d57")

lineChart <- lineChartViz(data = data2plot)

ggsave(plot   = lineChart,
       file   = paste0("Visualizations/Figure3/Figure3_4.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figura 3.5

data2plot <- simpleBarData.fn(group_var = Primer_lugar_traslado)
data2plot <- data2plot %>%
  mutate(order_var = rank(-value2plot, ties.method = "min"))

barChart <- BarSimpleChartViz(fill_colors = c("Agencia del Ministerio Público" = "#003B88",
                                              "Juez de lo penal" = "#E2E2F7",
                                              "Instalación de la policía" = "#003B88",
                                              "Centro de arraigo" = "#E2E2F7",
                                              "Centro penitenciario" = "#E2E2F7",
                                              "Oficina del gobierno" = "#E2E2F7",
                                              "Casa particular" = "#E2E2F7",
                                              "Establecimiento comercial" = "#E2E2F7",
                                              "Vehículo" = "#E2E2F7",
                                              "Terreno baldío" = "#E2E2F7",
                                              "Zona militar" = "#E2E2F7",
                                              "Centro de detención para migrantes" = "#E2E2F7",
                                              "Hospital" = "#E2E2F7",                                         
                                              "Otra" = "#E2E2F7"))

ggsave(plot   = barChart,
       file   = paste0("Visualizations/Figure3/Figure3_5.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## Figura 3.5

data2table <- pivot_vars.fn(data = Main_database, variables = "Primer_lugar_traslado")

data2plot1 <- lineChartData.fn(data = data2table, dependent_var = "Primer_lugar_traslado_Agencia del Ministerio Público") %>%
  filter(category %in% c("dependent_var"))

data2plot2 <- lineChartData.fn(data = data2table, dependent_var = "Primer_lugar_traslado_Instalación de la policía") %>%
  filter(category %in% c("dependent_var")) %>%
  mutate(
    category = 
      case_when(
        category == "dependent_var" ~ "dependent_cmpl"
      )
  )

data2plot <- bind_rows(data2plot1, data2plot2)

colors4plot <- c("dependent_var" = "#003B88", 
                 "dependent_cmpl" = "#a90099")
#"#a90099"
lineChart <- lineChartViz(data = data2plot)

ggsave(plot   = lineChart,
       file   = paste0("Visualizations/Figure3/Figure3_5.svg"), 
       width  = 190, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")