# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Settings Presentations
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     March 12th, 2024
##
## This version:      March 12th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Audiencias

audiencias <- c("P5_19_3",
                "P5_26",
                "P5_20_1",
                "P5_20_2",
                "P5_20_3",
                "P5_26A",
                "P5_26B",
                "P3_14_5",
                "P4_1_04",
                "P4_3A_2",
                "P5_2_4",
                "P3_14_4",
                "P4_1_03",
                "P4_6_2",
                "P5_22_01",
                "P5_22_02",
                "P5_20_4",
                "P5_17_2",
                "P5_17_3",
                "P5_17_1",
                "P5_17_4",
                "P4_6_4"
                )
all_vars <-   c("P5_2",
                "P5_19_3",
                "P5_16_5",
                "P5_14",
                "P5_16_2",
                "P5_18_5",
                "P5_26",
                "P5_20_1",
                "P5_20_2",
                "P5_20_3",
                "P5_25_1",
                "P5_26A",
                "P5_26B",
                "P3_14_5",
                "P4_1_04",
                "P4_3A_2",
                "P4_7_4_5",
                "P5_2_4",
                "P3_14_4",
                "P4_1_03",
                "P4_6_2",
                "P5_2_1",
                "P5_22_01",
                "P5_22_02",
                "P5_20_4",
                "P5_17_2",
                "P5_17_3",
                "P5_17_1",
                "P5_17_4",
                "P5_10_5",
                "P5_10_6",
                "P5_6_1",
                "P5_6_2",
                "P4_6_4",
                "indicator_general"
                )

labels_vars <- c("Audiencia - Tuvo contacto con el juez",
                 "Audiencia - Hubo registro de video",
                 "Audiencia - Hubo acceso al público",
                 "Audiencia - El juez de control fue distinto al que sentenció",
                 "Audiencia - El juez estuvo presente en las audiencias",
                 "Audiencia - El juez llevaba el control de las audiencias",
                 "Audiencia - La persona juzgada se sintió escuchada",
                 "Audiencia - La persona estaba tras un vidrio",
                 "Audiencia - La persona estaba esposada",
                 "Audiencia - La persona traía uniforme penitenciario",
                 "Audiencia - La persona consideró que el juez lo consideró culpable antes de oir las pruebas",
                 "Audiencia - Proceso justo",
                 "Audiencia - Sentencia justa",
                 "Derechos - No autoincriminarse - detención",
                 "Derechos - No autoincriminarse - MP",
                 "Derechos - No autoincriminarse - Declaracion",
                 "Derechos - Autoincriminación - Amenazas o agresión fisica",
                 "Derechos - No autoincriminarse - Audiencia inicial",
                 "Derechos - Explicación detención - Detención",
                 "Derechos - Explicación detención - Motivo acusación",
                 "Derechos - Explicación detención - Lectura declaración",
                 "Derechos - Explicación detención - Audiencia inicial",
                 "Derechos - Explicación detención - Abogado defensor explicó los hechos de acusación",
                 "Derechos - Explicación detención - Abogado defensor le explicó como seria su proceso",
                 "Derechos - Explicación detención - Podía escuchar",
                 "Derechos - Explicación detención - Entendió al juez",
                 "Derechos - Explicación detención - Entendió al MP",
                 "Derechos - Explicación detención - Entendió al abogado defensor",
                 "Derechos - Explicación detención - Entendió al abogado víctima",
                 "Derechos - Tiempo sentencia - seis meses",
                 "Derechos - Tiempo sentencia - uno a dos años",
                 "Sentencia - Juicio",
                 "Sentencia - Procedimiento abreviado",
                 "Culpabilidad: Culpable ante el MP",
                 "Indicador general"
)

# Time analysis

all_vars_series.df <- time_analysis.fn(variables2summarise = all_vars,
                                         labels_vars = labels_vars, 
                                         groups = F)

# Loop over each label and create corresponding plot
for (i in seq_along(all_vars_series.df)) {
  colors4plot <- c("black")
  
  lineChart <- lineChartViz(data = all_vars_series.df[[i]],
                            value2plot = "value2plot",
                            period = "period", 
                            order_value = NULL, 
                            category = "var_name", 
                            labels = "labels", 
                            event = F) +
    labs(title = labels_vars[[i]])
  
  # Save the plot
  ggsave(lineChart, 
         filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", labels_vars[[i]], "_series.svg"),
         width  = 200, 
         height = 150,
         units  = "mm",
         dpi    = 72,
         device = "svg")
}

### Presuncion

presuncion <- all_vars_series.df[["P5_25_1"]] %>%
  mutate(
    value2plot = 100 - value2plot,
    labels = if_else(period %in% c("2012", "2014", "2016", "2018", "2020"),
                     paste0(round(value2plot, 0), "%"), NA_character_),
    label_var = "Audiencia - La persona consideró que el juez NO lo consideró culpable antes de oir las pruebas"
  ) %>%
  rbind(y = all_vars_series.df[["P5_14"]])

lineChart <- lineChartViz(data.df = presuncion,
                          value2plot = "value2plot",
                          period = "period", 
                          order_value = NULL, 
                          category = "var_name", 
                          labels = "labels", 
                          event = F) 

# Save the plot
ggsave(lineChart, 
       filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", "presuncion_control", "_series_group.svg"),
       width  = 200, 
       height = 150,
       units  = "mm",
       dpi    = 72,
       device = "svg")

### Proceso Justo 
proceso_justo <- all_vars_series.df[["P5_26A"]] %>%
  rbind(y = all_vars_series.df[["P5_6_2"]])

lineChart <- lineChartViz(data.df = proceso_justo,
                          value2plot = "value2plot",
                          period = "period", 
                          order_value = NULL, 
                          category = "var_name", 
                          labels = "labels", 
                          event = F) 
# Save the plot
ggsave(lineChart, 
       filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", "proceso_justo_procedimiento_abreviado", "_series_group.svg"),
       width  = 200, 
       height = 150,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Culpabilidad

culpabilidad <- all_vars_series.df[["P4_6_4"]] %>%
  rbind(y = all_vars_series.df[["P5_26A"]])
colors4plot <- c("#a90099",
                 "#1a2689")
lineChart <- lineChartViz(data.df = culpabilidad,
                          value2plot = "value2plot",
                          period = "period", 
                          order_value = NULL, 
                          category = "var_name", 
                          labels = "labels", 
                          event = F) 

# Save the plot
ggsave(lineChart, 
       filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", "proceso_justo_culpabilidad", "_series_group.svg"),
       width  = 200, 
       height = 150,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Indicator 
proceso_justo_indicador <- all_vars_series.df[["P5_26A"]] %>%
  rbind(y = all_vars_series.df[["indicator_general"]])
colors4plot <- c("#a90099", "#1a2689")
lineChart <- lineChartViz(data.df = proceso_justo_indicador,
                          value2plot = "value2plot",
                          period = "period", 
                          order_value = NULL, 
                          category = "var_name", 
                          labels = "labels", 
                          event = F) 
# Save the plot
ggsave(lineChart, 
       filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", "proceso_justo_indicador", "_series_group.svg"),
       width  = 200, 
       height = 150,
       units  = "mm",
       dpi    = 72,
       device = "svg")
# Series by group

variables2summarise <- all_vars

all_vars_group_series.df <- time_analysis.fn(data.df = Main_database_2008, 
                                               variables2summarise = all_vars, 
                                               labels_vars = labels_vars, 
                                               groups = T)

# Loop over each label and create corresponding plot
for (i in seq_along(all_vars_group_series.df)) {
  colors4plot <- c("#1a2689",
                   "#a90099")
  
  lineChart <- lineChartViz(data.df = all_vars_group_series.df[[i]],
                            value2plot = "value2plot",
                            period = "period", 
                            order_value = NULL, 
                            category = "category", 
                            labels = "labels", 
                            event = F) +
    labs(title = labels_vars[[i]])
  
  # Save the plot
  ggsave(lineChart, 
         filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", labels_vars[[i]], "_series_group.svg"),
         width  = 200, 
         height = 150,
         units  = "mm",
         dpi    = 72,
         device = "svg")
}

#####


