## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Indicator descriptives
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvarez         (mtorres@worldjusticeproject.org)
##                    Marcelo Torres           (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 30th, 2023
##
## This version:      January 30th, 2023
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
source("Almanac_outline/Settings.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Información / capacidad legal                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1.1. Derecho a conocer sus derechos

## 1.2. Derecho a tener y comprender la información sobre su proceso

## 1.3. Derecho a los ajustes razonables a partir de necesidades específicas

derecho_ajustes_razonables <- c("P4_1_13", "P4_1_14", "P4_1_15", "P4_1_16")

result <- clean_columns.fn(Main_database, derecho_ajustes_razonables)

labels <- c("Al momento de llegar a la Agencia del M.P., 
            ¿la autoridad contactó al consulado de su país?",
            "Al momento de llegar a la Agencia del M.P., 
            ¿necesitaba un traductor por no hablar español?",
            "Al momento de llegar a la Agencia del M.P., 
            ¿tuvo el apoyo de un traductor?",
            "Al momento de llegar a la Agencia del M.P., 
            ¿tuvo el apoyo de alguna persona en la Agencia del M.P. por no saber leer o escribir?") 

data2plot <- set_data.fn(result, derecho_ajustes_razonables, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              order_var = order_var,
                              fill_colors = c("#E2E2F7", "#E2E2F7", "#003B88"),
                              title = "Derecho a los ajustes razonables a partir de necesidades específicas")
barChart

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Información / capacidad legal                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 2.1. Derecho a guardar silencio y no declarar
 
derecho_guardar_silencio <- c("P3_14_5", "P4_3A_2", "P5_2_4")

result <- clean_columns.fn(Main_database, derecho_guardar_silencio)

labels <- c("Al momento de su detención, ¿el policía o autoridad le informó sobre sus derechos a 
            guardar silencio y a no declarar sin la presencia de su abogado?",
            "En ese interrogatorio, ¿le explicaron que podía guardar silencio y no responder?",
            "En su primer encuentro con el juez, ¿le informó sobre su derecho a guardar silencio 
            y a no declarar sin la presencia de su abogado?") 
 
data2plot <- set_data.fn(result, derecho_guardar_silencio, labels)
 
barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              order_var = order_var,
                              fill_colors = c("#E2E2F7", "#E2E2F7", "#003B88"),
                              title = "Derecho a guardar silencio y no declarar")
barChart

## 2.2.	Derecho a la autonomía / Prohibición a la coacción

derecho_autonomia <- c("P4_3A_6", "P4_3A_7", "P4_3A_8", "P4_6_1")
 
result <- clean_columns.fn(Main_database, "P4_7", 4)
 
result_1 <- clean_columns.fn(Main_database, "P4_7", 8)
 
result$P4_7_8 <- result_1$P4_7
 
labels <- c("En ese interrogatorio, ¿fue engañado(a) para inculpar a alguien más?", 
            "En ese interrogatorio, ¿usted fue golpeado(a) o maltratado(a) para 
            echarse la culpa o aceptar hechos falsos?",
            "En ese interrogatorio, ¿usted fue golpeado(a) o maltratado(a)
            para inculpar a alguien más?",
            "Al momento de rendir o firmar su declaración ante el Ministerio 
            Público… ¿los policías o autoridades lo presionaron a dar otra versión de los hechos?",
            "Podría decirme, ¿cuál fue la principal razón por la que se declaró culpable?... 
            Porque me presionaron o amenazaron para hacerlo", 
            "Podría decirme, ¿cuál fue la principal razón por la que se declaró culpable?...
            Porque me agredieron físicamente") 
 
derecho_autonomia <- c("P4_3A_6", "P4_3A_7", "P4_3A_8", "P4_6_1", "P4_7", "P4_7_8")

data2plot <- set_data.fn(result, derecho_autonomia, labels)
 
barChart <- BarSimpleChartViz(data = data2plot,
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              order_var = order_var,
                              fill_colors = c("#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7", "#003B88","#E2E2F7"),
                              title = "Derecho a la autonomía / Prohibición a la coacción")
barChart

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Defensa adecuada                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 3.1. 1.	Derecho a una defensa oportuna

> derecho_defensa_oportuna <- c("P4_1_05",
                                "P4_1_06", 
                                "P4_3A_1",
                                "P4_6_3", 
                                "P4_14_1", 
                                "P5_1", 
                                "P5_2_5" )

result <- clean_columns.fn(Main_database, derecho_defensa_oportuna)
 
labels <- c("Al momento de llegar a la Agencia del Ministerio Público,
            ¿tuvo la asesoría de un abogado?",
            "Al momento de llegar a la Agencia del Ministerio Público,
            ¿habló a solas con su abogado?",
            "En ese interrogatorio, ¿estuvo presente su abogado?",
            "Al momento de rendir o firmar su declaración ante el Ministerio Público… 
            ¿estuvo presente su abogado?",
            "Al momento de ser presentado(a), … ¿estaba presente su abogado defensor?",
            "Antes de llegar con un juez por primera vez,
            ¿tuvo la asesoría de un abogado defensor?",
            "En su primer encuentro con el juez
            … ¿estuvo presente su abogado defensor?") 
 
data2plot <- set_data.fn(result, derecho_defensa_oportuna, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              order_var = order_var,
                              fill_colors = c("#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7","#E2E2F7", "#003B88"),
                              title = "Derecho a una defensa oportuna")

barChart