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
                "P5_26B")
all_vars <-   c("P5_2_1",
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
                "P5_26B")

labels_vars <- c("Tuvo contacto con el juez",
                 "Hubo registro de video",
                 "Hubo acceso al público",
                 "El juez de control fue distinto al que sentenció",
                 "El juez estuvo presente en las audiencias",
                 "El juez llevaba el control de las audiencias",
                 "La persona juzgada se sintió escuchada",
                 "La persona estaba tras un vidrio",
                 "La persona estaba esposada",
                 "La persona traía uniforme penitenciario",
                 "La persona consideró que el juez lo consideró culpable antes de oir las pruebas",
                 "Proceso justo",
                 "Sentencia justa"
)

# Event Study

all_vars.df <- event_study.fn(variables2summarise = all_vars, 
                                labels_vars = labels_vars)

# Loop over each label and create corresponding plot
for (i in seq_along(all_vars.df)) {
  colors4plot <- c("black")
  
  lineChart <- lineChartViz(data.df = all_vars.df[[i]], 
                            period = "period", 
                            value2plot = "value2plot", 
                            order_value = "order_value",
                            category = "var_name",
                            labels = "labels",
                            event = T,
                            color4plot = colors4plot) +
    labs(title = labels_vars[[i]])
  
  # Save the plot
  ggsave(lineChart, 
         filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", labels_vars[[i]], ".svg"),
         width  = 200, 
         height = 150,
         units  = "mm",
         dpi    = 72,
         device = "svg")
}

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

# Event study by groups

variables2summarise <- all_vars

all_vars_group.df <- event_study.fn(data.df = Main_database_2008, 
                                      variables2summarise = all_vars, 
                                      labels_vars = labels_vars, 
                                      groups = T)

# Loop over each label and create corresponding plot
for (i in seq_along(all_vars_group.df)) {
  colors4plot <- c("#1a2689",
                   "#a90099")
  
  lineChart <- lineChartViz(data.df = all_vars_group.df[[i]],
                            value2plot = "value2plot",
                            period = "period", 
                            order_value = "order_value", 
                            category = "category", 
                            labels = "labels", 
                            event = T) +
    labs(title = labels_vars[[i]])
  
  # Save the plot
  ggsave(lineChart, 
         filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", labels_vars[[i]], "_group.svg"),
         width  = 200, 
         height = 150,
         units  = "mm",
         dpi    = 72,
         device = "svg")
}

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
