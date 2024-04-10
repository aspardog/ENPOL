## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Milestone 2
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres            (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     April 4th, 2024
##
## This version:      April 8th, 2024
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


load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 



timeline <- function(data, group_var) {
  # Create the new data frame
  data2plot <- data %>%
    group_by(Anio_arresto) %>%
    summarise(value2plot = 100 * mean({{ group_var }}, na.rm = T)) %>%
    mutate(labels = if_else(
      Anio_arresto %in% c("2011", "2013", "2015", "2017", "2019", "2021"),
      paste0(round(value2plot,0), "%"), NA_character_),
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
## 1.  Prepare prioritary crimes variables                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Main_database1 <- Main_database %>%
  filter(Anio_arresto >= 2011, Delito_unico == 1) %>%
  mutate(tiempo_sentencia = case_when(as.numeric(P5_4_A) == 0 ~ NA_real_,
                                      as.numeric(P5_4_A) >= 97 ~ NA_real_,
                                      T ~ as.numeric(P5_4_A)),
         Calderon = case_when(Anio_arresto == 2006 & Mes_arresto > 11 ~ 1,
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
         # alt1 considera delitos de PPO
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
                                   T ~ 0),
    Delito_prioritario_ENVIPE = case_when(Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ 1,
                                          Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ 1,
                                          Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ 1,
                                          T ~ 0)
  
) 

Main_database2 <- Main_database %>%
  filter(Anio_sentencia >= 2011, Delito_unico == 1) %>%
  mutate(tiempo_sentencia = case_when(as.numeric(P5_4_A) == 0 ~ NA_real_,
                                      as.numeric(P5_4_A) >= 97 ~ NA_real_,
                                      T ~ as.numeric(P5_4_A)),
         Calderon = case_when(Anio_arresto == 2006 & Mes_arresto > 11 ~ 1,
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
         # alt1 considera delitos de PPO
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
                                              T ~ 0),
         Delito_prioritario_ENVIPE = case_when(Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ 1,
                                               Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ 1,
                                               Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ 1,
                                               T ~ 0)
         
  ) 



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Get data from ENVIPE                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ENVIPE <- read_dta(paste0(path2SP,"/National/Exploration/Input/Politica_criminal/ocurrencias.dta")) %>%
  pivot_longer(cols = del_1:del_15, names_to = "Delito_envipe",values_to = "Ocurrencias") %>%
  group_by(anio, Delito_envipe) %>%
  summarise(Ocurrencias = sum(Ocurrencias, na.rm = T)) %>% 
  arrange(anio, -Ocurrencias) %>%
  group_by(anio) %>%
  slice(1:3)

table(ENVIPE$anio,ENVIPE$Delito_envipe)

#anio   del_2 del_3 del_4 del_5 del_9
#2014     1     1     0     0     1
#2015     1     1     0     0     1
#2016     1     1     0     0     1
#2017     1     1     0     0     1
#2018     1     1     0     1     0
#2019     1     1     0     1     0
#2020     1     1     0     1     0
#2021     1     0     1     1     0
#2022     1     1     0     0     1
#2023     1     1     0     0     1

#Delitos prioritarios ENPOL robo de autopartes, daño a la propiedad, extorsión (a veces robo a transeúnte)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Create analysis                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 ⁠Estrategias en las detenciones
# Proporción de detenciones x DelitosPrioritarios_Gob Vs No prioritarios, 2011-2021

plt <- timeline(Main_database1,Delito_prioritario_Cald) +
  geom_vline(xintercept = "2012") +
  geom_vline(xintercept = "2018")


ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure1_1_1.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

plt <- timeline(Main_database1,Delito_prioritario_Peña) +
  geom_vline(xintercept = "2012") +
  geom_vline(xintercept = "2018")


ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure1_1_2.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

plt <- timeline(Main_database1,Delito_prioritario_AMLO) +
  geom_vline(xintercept = "2012") +
  geom_vline(xintercept = "2018")


ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure1_1_3.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Correlación con tiempo de sentencia:

cor(Main_database1$Delito_prioritario,Main_database1$tiempo_sentencia, use="pairwise")

#b. Proporción de detenciones x DelitosPrioritarios_ENVIPE Vs No prioritarios, 2011-2021


plt <- timeline(Main_database1,Delito_prioritario_ENVIPE) +
  geom_vline(xintercept = "2012") +
  geom_vline(xintercept = "2018")


ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure1_2_1.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# NOT IN USE

## 2.⁠ ⁠Promedio de Controles aplicados en la Detención para DelitosPrioritarios_Gob:
## aquí la idea es cruzar el tipo de corporación x el tipo de control aplicado. 
## (Vero avanzó este análisis pero con los delitos en general, en lugar de sobre un tipo de delito, 
## acá se ve: UsoFuerza_20231130.docx

##data2plot <- Main_database %>%
##  filter(Delito_prioritario_alt1 == 1) %>%
##  group_by(Corporacion_grupos) %>%
##  summarise(controles_cooperativos = 100 * mean(controles_cooperativos, na.rm = T),
##            sometimiento = 100 * mean(sometimiento, na.rm = T),
##            control_contacto = 100 * mean(control_contacto, na.rm = T),
##            tacticas_defensivas = 100 * mean(tacticas_defensivas, na.rm = T),
##            fuerza_letal = 100 * mean(fuerza_letal, na.rm = T)) %>%
##  pivot_longer(cols = controles_cooperativos:fuerza_letal, names_to = "tipo_control", values_to = "value2plot") %>%
##  mutate(labels = paste0(round(value2plot,0), "%"),
##    group_var = tipo_control) %>%
##  filter(Corporacion_grupos != "NS/NR")
##  colors4plot <- c("controles_cooperativos" = "#1a2580", 
##                   "sometimiento" = "#a90099",
##                   "control_contacto" = "#ef6b4b",
##                   "tacticas_defensivas" = "#ef9b4b",
##                   "fuerza_letal" = "#ef0b4b")
##
##  plt <- ggplot(data2plot, 
##                aes(x     = Corporacion_grupos,
##                    y     = value2plot,
##                    label = labels,
##                    group = tipo_control,
##                    color = group_var)) +
##    geom_point(size = 2,
##               show.legend = F) +
##    geom_text_repel(size        = 3.514598,
##                    show.legend = F,
##                    
##                    # Additional options from ggrepel package:
##                    min.segment.length = 1000,
##                    seed               = 42,
##                    box.padding        = 0.5,
##                    direction          = "y",
##                    force              = 5,
##                    force_pull         = 1) +
##    scale_y_continuous(limits = c(0, 105),
##                       expand = c(0,0),
##                       breaks = seq(0,100,20),
##                       labels = paste0(seq(0,100,20), "%")) %>%
##    scale_color_manual(values = colors4plot) +
##    WJP_theme() +
##    theme(panel.grid.major.x = element_blank(),
##          panel.grid.major.y = element_line(colour = "#d1cfd1"),
##          axis.title.x       = element_blank(),
##          axis.title.y       = element_blank(),
##          axis.line.x        = element_line(color    = "#d1cfd1"),
##          axis.ticks.x       = element_line(color    = "#d1cfd1",
##                                            linetype = "solid")
##    ) +
##    coord_flip()
##
##  ggsave(plot   = plt,
##        file   = paste0("National/Exploration/Output/Milestone2/Figure2_1.svg"), 
##        width  = 175, 
##        height = 85,
##        units  = "mm",
##        dpi    = 72,
##        device = "svg")
  


##3.⁠ ⁠Estrategias en las sanciones
#a. Número absoluto  detenciones y sentencias x secuestro, 2011-2021; 

data2plot_1 <- Main_database1 %>%
  filter(Delito_unico_9_secuestro == 1) %>%
  group_by(Anio_arresto) %>%
  summarise(value2plot = n()) %>%
  mutate(labels = if_else(
    Anio_arresto %in% c("2011", "2013", "2015", "2017", "2019", "2021"),
    paste0(value2plot), NA_character_),
    group_var = "Arrestos",
    Anio = Anio_arresto
  ) %>%
  select(Anio,value2plot,labels,group_var)

data2plot_2 <- Main_database2 %>%
  filter(Anio_sentencia >= 2011 & !is.na(Anio_sentencia)) %>%
  filter(Delito_unico_9_secuestro == 1) %>%
  group_by(Anio_sentencia) %>%
  summarise(value2plot = n()) %>%
  mutate(labels = if_else(
    Anio_sentencia %in% c("2011", "2013", "2015", "2017", "2019", "2021"),
    paste0(value2plot), NA_character_),
    group_var = "Sentencias",
    Anio = Anio_sentencia
  ) %>%
  select(Anio,value2plot,labels,group_var)

data2plot <- rbind(data2plot_1,data2plot_2)


colors4plot <- c("#003B88", "#ef0b4b")

# Creating ggplot

plt <- ggplot(data2plot, 
              aes(x     = Anio,
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
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid")) +
  geom_vline(xintercept = "2012") +
  geom_vline(xintercept = "2018")


ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure2_1.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


#b. Número absoluto  detenciones y sentencias x x hom dol, 2011-2021; 


data2plot_1 <- Main_database1 %>%
  filter(Delito_unico_6_hom_dol == 1) %>%
  group_by(Anio_arresto) %>%
  summarise(value2plot = n()) %>%
  mutate(labels = if_else(
    Anio_arresto %in% c("2011", "2013", "2015", "2017", "2019", "2021"),
    paste0(value2plot), NA_character_),
    group_var = "Arrestos",
    Anio = Anio_arresto
  ) %>%
  select(Anio,value2plot,labels,group_var)

data2plot_2 <- Main_database2 %>%
  filter(Anio_sentencia >= 2011 & !is.na(Anio_sentencia)) %>%
  filter(Delito_unico_6_hom_dol == 1) %>%
  group_by(Anio_sentencia) %>%
  summarise(value2plot = n()) %>%
  mutate(labels = if_else(
    Anio_sentencia %in% c("2011", "2013", "2015", "2017", "2019", "2021"),
    paste0(value2plot), NA_character_),
    group_var = "Sentencias",
    Anio = Anio_sentencia
  ) %>%
  select(Anio,value2plot,labels,group_var)

data2plot <- rbind(data2plot_1,data2plot_2)


colors4plot <- c("#003B88", "#ef0b4b")

# Creating ggplot

plt <- ggplot(data2plot, 
              aes(x     = Anio,
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
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid")) +
  geom_vline(xintercept = "2012") +
  geom_vline(xintercept = "2018")


ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure2_2.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")
