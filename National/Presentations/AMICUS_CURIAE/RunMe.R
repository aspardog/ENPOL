## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            SOCIO-DEMOGRAPHICS DESCRIPTIVES
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

path2local <- "~/Library/CloudStorage/OneDrive-WorldJusticeProject/LOCAL/ENPOL/National/Presentations/AMICUS_CURIAE"


load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, 
         procesado == 0)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. GRÁFICOS                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Proceso no en libertad (sentenciadas y procesados)---------------------------------------------------

Main_database_2008 <-  Main_database_2008 %>% 
  mutate(proceso_privado_libertad_todos = case_when( tipo_prision_preventiva == "Prisión Preventiva Oficiosa" ~ "Proceso en prisión preventiva", 
                                                     tipo_prision_preventiva == "Prisión Preventiva Justificada" ~ "Proceso en prisión preventiva", 
                                                     tipo_prision_preventiva == "Proceso en libertad" ~ "Proceso en libertad",
                                                     T ~ NA_character_))


data2plot <- count_frequency.fn(Main_database_2008$proceso_privado_libertad_todos)

data2plot <- data2plot %>% mutate(order_var = case_when(Value =="Proceso en prisión preventiva" ~ 1, 
                                                        Value =="Proceso en libertad" ~ 2,
                                                        T ~ NA_real_))


barChart <- BarSimpleChartViz(fill_colors = c("#9c94ff", "#12006b")) + expand_limits(y = c(0, 110))
barChart

ggsave(plot = barChart, 
       filename = paste0(path2local,"/Visualizations/figure1.svg"),
       width = 189.7883,
       height = 90,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# PP vs. PPO vs. Libertad sentenciadas y procesadas --------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$tipo_prision_preventiva)


barChart <- BarSimpleChartViz(fill_colors = c("#12006b", "#4e43dd", "#9c94ff")) + expand_limits(y = c(0, 110))
barChart

ggsave(plot = barChart, 
       filename = paste0(path2local,"/Visualizations/figure2.svg"),
       width = 189.7883,
       height = 90,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Tiempo en prisión preventiva --------------------------------------------

#procesados meses en prisión preventiva
Main_database_2008 <- Main_database_2008 %>% 
  mutate(P5_34_A = replace(P5_34_A, P5_34_A %in% c( "98", "99"), NA),
         P5_34_M = replace(P5_34_M, P5_34_M %in% c( "98", "99"), NA),
         P5_34_A = replace(P5_34_A, P5_34_A %in% c( "96"), 0),
         P5_34_M = replace(P5_34_M, P5_34_M %in% c( "96"), 0),
         P5_10 = replace(P5_10, P5_10 %in% c("8", "9"), NA)) %>% 
  mutate(procesados_meses_pp = ((as.numeric(P5_34_A)*12) + as.numeric(P5_34_M))) %>% 
  mutate(mas2anios_prisionpreventiva = case_when(as.numeric(P5_10) == 7 ~ 1,
                                                 procesados_meses_pp  > 24   ~ 1,
                                                 as.numeric(P5_10) == 1 | as.numeric(P5_10) == 2 | 
                                                   as.numeric(P5_10) == 3 | as.numeric(P5_10) == 4 |
                                                   as.numeric(P5_10) == 5 |as.numeric(P5_10) == 6 ~ 0,
                                                 procesados_meses_pp  <= 24 ~ 0,
                                                 T ~ NA_real_)) 




Main_database_2008 <- Main_database_2008 %>% 
  mutate(mas2anios_prisionpreventiva = case_when(mas2anios_prisionpreventiva == 1 ~ "Más de 2 años", 
                                                 mas2anios_prisionpreventiva == 0 ~ "Menos, o hasta, 2 años",
                                                 T ~ NA_character_))

data2plot <- count_frequency.fn(Main_database_2008$mas2anios_prisionpreventiva)

barChart <- BarSimpleChartViz(fill_colors = c("#9c94ff","#9c94ff")) + expand_limits(y = c(0, 110))
barChart

ggsave(plot = barChart, 
       filename = paste0(path2local,"/Visualizations/figure3.svg"),
       width = 189.7883,
       height = 90,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Tiempo en prisión por tipo de pp ----------------------------------------


colors4plot <- c("Más de 2 años" = "#2e2e95", 
                 "Menos, o hasta, 2 años" = "#b1a6ff")

plot <- BarCategoricalBarsVer.fn(column1 = tipo_prision_preventiva, column2 = mas2anios_prisionpreventiva )
plot

ggsave(plot = plot, 
       filename = paste0(path2local,"/Visualizations/figure4.svg"),
       width = 189.7883,
       height = 90,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Tipo de prueba  por tipo de juicio----------------------------------------------------------

Main_database_2008 <- Main_database_2008 %>% 
  mutate(juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                      P5_6 == "2" ~ "Procedimiento abreviado o juicio sumario", 
                                      T ~ NA_character_))%>% 
  filter(tipo_prision_preventiva == "Prisión Preventiva Oficiosa")

data2plot <- count_frequency.fn(Main_database_2008$juicio_abreviado) 


barChart <- BarSimpleChartViz(fill_colors = c("#9c94ff","#9c94ff")) + expand_limits(y = c(0, 90))
barChart

ggsave(plot = barChart, 
       filename = paste0(path2local,"/Visualizations/figure6.svg"),
       width = 189.7883,
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Defensa ----------------------------------------------------------

## Exploración: El tener o no defensa tiene efecto en el número de años de sentencia

defensa <- c("P4_1_05", 
             "P5_1",
             "P5_2_5")

Main_database_2008 <- clean_columns.fn(Main_database_2008, defensa) %>% 
  rename(defensa_mp  =  P4_1_05,
         defensa_antes_juez = P5_1,
         defensa_juez = P5_2_5,
         sentencia_años = P5_4_A) %>% 
  mutate(defensa_momento = case_when(defensa_mp == 1 & defensa_antes_juez == 1 ~ "Defensa en Ministerio Público y con Juez",
                                     defensa_mp == 0 & defensa_antes_juez == 0 ~ "Sin defensa en Ministerio Público ni con Juez",
                                     defensa_mp == 0 & defensa_antes_juez == 1 ~ "Defensa sólo con Juez",
                                     defensa_mp == 1 & defensa_antes_juez == 0 ~ "Defensa sólo en Ministerio Público",
                                     T~ NA_character_)) %>% 
  filter(tipo_prision_preventiva == "Prisión Preventiva Oficiosa")


data2plot <- count_frequency.fn(Main_database_2008$defensa_momento) 

data2plot <-  data2plot %>% mutate(order_var = case_when(Value == "Defensa en Ministerio Público y con Juez" ~ 3,
                                                         Value == "Defensa sólo con Juez" ~ 1,
                                                         Value == "Defensa sólo en Ministerio Público" ~ 2,
                                                         Value == "Sin defensa en Ministerio Público ni con Juez" ~ 4,
                                                         T ~ NA_real_ ))


barChart <- BarSimpleChartViz(fill_colors = c("#9c94ff","#9c94ff", "#9c94ff","#9c94ff")) + expand_limits(y = c(0, 80))
barChart

ggsave(plot = barChart, 
       filename = paste0(path2local,"/Visualizations/figure5.svg"),
       width = 189.7883,
       height = 95,
       units  = "mm",
       dpi    = 72,
       device = "svg")

