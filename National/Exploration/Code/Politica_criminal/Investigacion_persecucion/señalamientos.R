# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration descriptives
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvarez         (calvarez@worldjuticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 10th, 2024
##
## This version:      June 10th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:     Señalamientos                                                                                       ----
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


# 4.10 alguien que haya dicho que lo(la) vio cometer el delito ----------------------------------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) 

señalo <- c("P4_10")

labels <- c("hubo alguien que haya dicho que lo vio cometer el delito") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, señalo) 


data2plot <- set_data.fn(Main_database_2008, señalo, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = rep("#20204a", 15),
                              title = "4.10")
barChart



# 4.11 La(s) persona(s) que lo identificó(aron) o señaló(aron) ----------------------------------


Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)


señalo <- c("P4_11_01", 
                    "P4_11_02",
                    "P4_11_03",
                    "P4_11_04",
                    "P4_11_05", 
                    "P4_11_06")

labels <- c("era un conocido",
            "era un desconocido",
            "era una persona detenida o presa",
            "era la víctima del delito",
            "era un policía o alguna otra autoridad",
            "Otro") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, señalo) 


data2plot <- set_data.fn(Main_database_2008, señalo, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = rep("#E2E2F7", 15),
                              title = "4.11 La persona que lo señaló...")
barChart



# 4.12  -------------------------------------------------------------------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(P4_12 = case_when( P4_12 == "1" ~ "Usted fue mostrado(a) solo(a)", 
                            P4_12 == "2" ~ "Usted fue mostrado(a) con otras personas",
                            P4_12 == "3" ~ "Usted fue mostrado(a) con otras personas, pero lo muestran de uno en uno",
                            P4_12 == "4" ~ "Usted fue mostrado(a) con otras personas, a través de fotografías o videograbaciones",
                            P4_12 == "5" ~ "Otro",
                            T ~ NA_character_))

# 4.13  -------------------------------------------------------------------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(P4_12 = case_when( P4_12 == "1" ~ "Usted fue mostrado(a) solo(a)", 
                            P4_12 == "2" ~ "Usted fue mostrado(a) con otras personas",
                            P4_12 == "3" ~ "Usted fue mostrado(a) con otras personas, pero lo muestran de uno en uno",
                            P4_12 == "4" ~ "Usted fue mostrado(a) con otras personas, a través de fotografías o videograbaciones",
                            P4_12 == "5" ~ "Otro",
                            T ~ NA_character_))


# 4.14 Al momento de ser presentado(a) por la autoridad ante la(s) víctima(s) o testigo(s) para que lo(a) identificaran ----------------------------------


Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)


presentado <- c("P4_14_1", 
            "P4_14_2",
            "P4_14_3",
            "P4_14_4",
            "P4_14_5", 
            "P4_14_6",
            "P4_14_7", 
            "P4_14_8",
            "P4_14_9")

labels <- c("estaba presente su abogado defensor",
            "estaba presente la autoridad que lo detuvo",
            "*estaba usted detrás de un espejo donde no podía ver al testigo",
            "*había otras personas sospechosas del mismo delito en esa fila",
            "*había agentes de policía o personal del MP o fiscalía en esa fila",
            "había otros detenidos en esa fila",
            "alguien registró por escrito como se realizó la identificación",
            "alguien videograbó cómo se realizó la identificación",
            "usted resultó identificado(a) o señalado(a) como el(la) responsable por el testigo") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, presentado) 


data2plot <- set_data.fn(Main_database_2008, presentado, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = rep("#b1a6ff", 15),
                              title = "4.14 Al momento de ser presentado ante los testigos...")
barChart