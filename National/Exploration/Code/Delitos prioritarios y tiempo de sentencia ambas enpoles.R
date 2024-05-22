## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration data & hipothesys
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres            (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     February 05th, 2024
##
## This version:      March 19th, 2024
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

source("National/Data_cleaning/Code/settings.R")
library(ggrepel)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Functions                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1. line chart



timeline <- function(data, group_var) {
  # Create the new data frame
  data2plot <- data %>%
    group_by(Anio_arresto) %>%
    summarise(value2plot = mean({{ group_var }}, na.rm = T)) %>%
    mutate(labels = if_else(
      Anio_arresto %in% c("2010", "2012", "2014", "2016", "2018", "2020"),
      paste0(round(value2plot,2)), NA_character_),
      group_var = "National",
      colors = "#003B88"
    )
  
  
  colors4plot <- c("#003B88")
  
  # Creating ggplot
  
  plt <- ggplot(data2plot, 
                aes(x     = Anio_arresto,
                    y     = value2plot,
                    label = labels,
                    group = group_var,
                    color = group_var)) +
    geom_point(size = 2,
               show.legend = F) +
    geom_line(size  = 1,
              show.legend = F) +
    geom_text_repel(family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0,0),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) %>%
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid")
          
    )
  return(plt)
}


## 6. Timeline var continua



timeline_cont <- function(data, group_var) {
  # Create the new data frame
  data2plot <- data %>%
    group_by(Anio_arresto) %>%
    summarise(value2plot = mean({{ group_var }}, na.rm = T)) %>%
    mutate(labels = if_else(
      Anio_arresto %in% c("2008", "2010", "2012", "2014", "2016", "2018", "2020"),
      paste0(round(value2plot,0)), NA_character_),
      group_var = "National",
      colors = "#003B88"
    )
  
  
  colors4plot <- c("#003B88")
  
  # Creating ggplot
  
  plt <- ggplot(data2plot, 
                aes(x     = Anio_arresto,
                    y     = value2plot,
                    label = labels,
                    group = group_var,
                    color = group_var)) +
    geom_point(size = 2,
               show.legend = F) +
    geom_line(size  = 1,
              show.legend = F) +
    geom_text_repel(family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0,0),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) %>%
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid")
          
    )
  return(plt)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Prepare database                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Base_1 <- Main_database %>% 
  filter(muestra_mixta == 1, NSJP == 1) %>%
  mutate(PP = case_when(sentenciado == 1 & proceso_en_libertad == 1 ~ 0,
                        sentenciado == 1 & proceso_en_libertad == 0 ~ 1,
                        sentenciado == 0 ~ 1,
                        T ~  NA_real_),
         P5_4_A = as.numeric(P5_4_A),
         sentencia_severa_0_5 = case_when(P5_4_A >= 0 & P5_4_A < 5 ~ 1,
                                          P5_4_A >= 5 ~ 0,
                                          T ~ NA_real_),
         sentencia_severa_5_10 = case_when(P5_4_A >= 5 & P5_4_A < 10 ~ 1,
                                           P5_4_A >= 10 ~ 0,
                                           P5_4_A < 5 ~ 0,
                                           T ~ NA_real_),
         sentencia_severa_10_30 = case_when(P5_4_A >= 10 & P5_4_A < 30 ~ 1,
                                            P5_4_A >= 30 ~ 0,
                                            P5_4_A < 10 ~ 0,
                                            T ~ NA_real_),
         sentencia_severa_30 = case_when(P5_4_A >= 30 ~ 1,
                                         P5_4_A < 30 ~ 0,
                                         T ~ NA_real_),
         proc_abreviado = case_when(P5_6 == "1" ~ 0,
                                    P5_6 == "2" ~ 1,
                                    T ~ NA_real_),
         tiempo_sentencia = case_when(P5_4_A == 0 ~ NA_real_,
                                      P5_4_A >= 97 ~ NA_real_,
                                      T ~ P5_4_A)) %>%
  arrange(years_since_NSJP) %>%
  group_by(Estado_arresto) %>%
  mutate(max_time_implementation = max(years_since_NSJP, na.rm = T),
         grupo_implementacion = 
           if_else(
             max_time_implementation > 6 & max_time_implementation < 9, "Implementación tardía",
             if_else(
               max_time_implementation > 9 & max_time_implementation < 11, "Implementación media",
               if_else(
                 max_time_implementation > 11, "Implementación temprana", NA_character_
               )
             ))) %>%
    mutate( Calderon = case_when(Anio_arresto == 2006 & Mes_arresto > 11 ~ 1,
                                Anio_arresto > 2006 & Anio_arresto < 2012 ~ 1,
                                Anio_arresto == 2012 & Mes_arresto < 12 ~ 1,
                                T ~ 0),
           Peña = case_when(Anio_arresto == 2012 & Mes_arresto > 11 ~ 1,
                            Anio_arresto > 2012 & Anio_arresto < 2018 ~ 1,
                            Anio_arresto == 2018 & Mes_arresto < 12 ~ 1,
                          T ~ 0),
         AMLO = case_when(Anio_arresto == 2018 & Mes_arresto > 11 ~ 1,
                          Anio_arresto > 2018 ~ 1,
                          T ~ 0),
    Delito_prioritario = case_when(Calderon == 1 & Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_08 == 1|P5_31_08 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_hh == 1|P5_31_hh == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_18 == 1|P5_31_18 == 1) ~ 1,
                                        Calderon == 1 & Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_hh == 1|P5_31_hh == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
                                        Peña == 1 & Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ 1,
                                        AMLO == 1 & Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
                                        AMLO == 1 & Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
                                        AMLO == 1 & Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
                                        AMLO == 1 & Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
                                        AMLO == 1 & Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ 1,
                                        AMLO == 1 & Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
                                        T ~ 0),
    Delito_prioritario_Cald = case_when(Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_08 == 1|P5_31_08 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_hh == 1|P5_31_hh == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_18 == 1|P5_31_18 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
                                   T ~ 0),
    Delito_prioritario_Peña = case_when( Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_hh == 1|P5_31_hh == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ 1,
                                   T ~ 0),
    Delito_prioritario_AMLO = case_when( Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ 1,
                                    Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
                                   T ~ 0)) %>%
  mutate(Delito_prioritario_ENVIPE = case_when(Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
                                             Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
                                             Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ 1,
                                             Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ 1,
                                             Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ 1,
                                             Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ 1,
                                             T ~ 0),
         Delito_unico_ungrouped_categ = case_when(Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ "Robo de casa habitación",
                                                  Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ "Robo de vehículo",
                                                  Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ "Robo a negocio",
                                                  Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ "Robo en transporte público",
                                                  Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ "Robo a transeunte en vía pública",
                                                  Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ "Robo de autopartes",
                                                  Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ "Robo en forma distinta a las anteriores",
                                                  Delito_unico == 1 & (P5_11_08 == 1|P5_31_08 == 1) ~ "Posesión ilegal de drogas",
                                                  Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ "Comercio ilegal de drogas",
                                                  Delito_unico == 1 & (P5_11_10 == 1|P5_31_10 == 1) ~ "Lesiones",
                                                  Delito_unico == 1 & (P5_11_11 == 1|P5_31_11 == 1) ~ "Homicidio culposo",
                                                  Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ "Homicidio doloso",
                                                  Delito_unico == 1 & (P5_11_hh == 1|P5_31_hh == 1) ~ "Homicidio (2016)",
                                                  Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ "Portación ilegal de armas",
                                                  Delito_unico == 1 & (P5_11_14 == 1|P5_31_14 == 1) ~ "Incumplimiento de obligaciones de asistencia familiar",
                                                  Delito_unico == 1 & (P5_11_15 == 1|P5_31_15 == 1) ~ "Violencia familiar",
                                                  Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ "Daño a la propiedad",
                                                  Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ "Secuestro o secuestro expres",
                                                  Delito_unico == 1 & (P5_11_18 == 1|P5_31_18 == 1) ~ "Violación sexual",
                                                  Delito_unico == 1 & (P5_11_19 == 1|P5_31_19 == 1) ~ "Fraude",
                                                  Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ "Delincuencia organizada",
                                                  Delito_unico == 1 & (P5_11_21 == 1|P5_31_21 == 1) ~ "Otros delitos sexuales",
                                                  Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ "Exotorsión",
                                                  Delito_unico == 1 & (P5_11_23 == 1|P5_31_23 == 1) ~ "Privación de la libertad",
                                                  Delito_unico == 1 & (P5_11_24 == 1|P5_31_24 == 1) ~ "Abuso de confianza",
                                                  Delito_unico == 1 & (P5_11_25 == 1|P5_31_25 == 1) ~ "Amenazas",
                                                  Delito_unico == 1 & (P5_11_26 == 1|P5_31_26 == 1) ~ "Otro",
                                                  T ~ NA_character_))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Tiempo sentencia                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Cambios en la severidad de las sentencias (Línea de tiempo)


lineChart <- timeline_cont(Base_1,tiempo_sentencia)

ggsave(plot   = lineChart,
       file   = paste0("National/Exploration/Output/Tiempo_sentencia_ambas_ENPOLes.svg"), 
       width  = 190, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")




Base_homicidio <- Base_1 %>% filter(Delito_unico_categ == "homicidio(2016)" | Delito_unico_categ == "hom_dol"| Delito_unico_categ == "hom_cul" )


lineChart <- timeline_cont(Base_homicidio,tiempo_sentencia)

ggsave(plot   = lineChart,
       file   = paste0("National/Exploration/Output/Tiempo_sentencia_ambas_ENPOLes_2.svg"), 
       width  = 190, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Delitos prioritarios                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Base_1$Delito_unico_ungrouped_categ

data2plot <- Base_1 %>%
  filter(!is.na(Delito_unico_ungrouped_categ)) %>%
  group_by(Delito_unico_ungrouped_categ) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "Arrestos",
         Delito = Delito_unico_ungrouped_categ,
         Delito = str_wrap(Delito, width = 30)) %>%
  select(Delito,value2plot,labels,group_var) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito))


colors4plot <- c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7","#E2E2F7")


plt <- ggplot(data2plot, 
              aes(x     = Delito,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = Delito)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot +.9 ),
            color    = "#4a4a49") +
  labs(y = "% of respondents") +
  scale_y_discrete() +
  scale_x_discrete( ) +
  expand_limits(y = c(0, 25))+
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10))+
  coord_flip()


ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Delitos_prioritarios_ambas_ENPOLes_1.svg"), 
       width  = 100, 
       height = 225,
       units  = "mm",
       dpi    = 72,
       device = "svg")



## 2 Delitos prioritarios 2018-2021


lineChart <- timeline(Base_1,Delito_prioritario) + geom_vline(xintercept = "2012") + geom_vline(xintercept = "2018")

ggsave(plot   = lineChart,
       file   = paste0("National/Exploration/Output/Delitos_prioritarios_ambas_ENPOLes_2.svg"), 
       width  = 190, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")





##3.⁠Delitos ENVIPE en ENPOL, 2018-2021

Base_2 <- Base_1 %>% filter(sentenciado==1)

data2plot <- Base_2  %>%
  filter(Delito_unico_ungrouped_categ == "Robo de vehículo" |
           Delito_unico_ungrouped_categ == "Robo de autopartes" |
           Delito_unico_ungrouped_categ == "Daño a la propiedad" |
           Delito_unico_ungrouped_categ == "Robo de casa habitación" |
           Delito_unico_ungrouped_categ == "Robo a transeunte en vía pública" |
           Delito_unico_ungrouped_categ == "Robo en transporte público" |
           Delito_unico_ungrouped_categ == "Robo en forma distinta a las anteriores" |
           Delito_unico_ungrouped_categ == "Fraude" |
           Delito_unico_ungrouped_categ == "Extorsión" |
           Delito_unico_ungrouped_categ == "Amenazas" |
           Delito_unico_ungrouped_categ == "Lesiones" |
           Delito_unico_ungrouped_categ == "Secuestro o secuestro expres" |
           Delito_unico_ungrouped_categ == "Otros delitos sexualesl" |
           Delito_unico_ungrouped_categ == "Violación sexual"
  ) %>%
  group_by(Anio_arresto,Delito_prioritario_ENVIPE) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0), "%"),
         group_var =  "Delito_prioritario_ENVIPE",
  ) %>%
  select(Anio_arresto,Delito_prioritario_ENVIPE,value2plot,labels,group_var) %>%
  filter(Delito_prioritario_ENVIPE == 1)


colors4plot <- c("#003B88")

plt <- ggplot(data2plot, 
              aes(x     = Anio_arresto,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = group_var)) +
  geom_point(size = 2,
             show.legend = F) +
  geom_line(size  = 1,
            show.legend = F) +
  geom_text_repel(
    size        = 3.514598,
    show.legend = F,
    
    # Additional options from ggrepel package:
    min.segment.length = 1000,
    seed               = 42,
    box.padding        = 0.5,
    direction          = "y",
    force              = 5,
    force_pull         = 1) +
  scale_y_continuous(limits = c(0, 105),
                     expand = c(0,0),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%")) %>%
  scale_color_manual(values = colors4plot) +
  WJP_theme() +
  expand_limits(y = c(0, 100))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"))



ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Delitos_prioritarios_ambas_ENPOLes_3.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


