# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Oaxaca from Enpol
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##
## Dependencies:      World Justice Project
##
## Creation date:     Jun 14th, 2024
##
## This version:      Jun 17th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Settings                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes and other general routines are
# loaded from the following script:
source("National/Data_cleaning/Code/settings.R")
library(readxl)
library(ggrepel)
library(sandwich)

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>%
  mutate(post_reforma_PPO = case_when(fecha_arresto>fecha_PPO_2 ~ 1,
                                      fecha_arresto<=fecha_PPO_2 ~ 0,
                                      T ~ NA_real_),
         suspension_condicional = case_when(P5_11_03 == "1" | P5_31_03 == "1" ~ 1,
                                            P5_11_11 == "1" | P5_31_11 == "1" ~ 1,
                                            P5_11_13 == "1" | P5_31_13 == "1" ~ 1,
                                            P5_11_14 == "1" | P5_31_14 == "1" ~ 1,
                                            P5_11_15 == "1" | P5_31_15 == "1" ~ 1,
                                            P5_11_19 == "1" | P5_31_19 == "1" ~ 1,
                                            P5_11_21 == "1" | P5_31_21 == "1" ~ 1,
                                            P5_11_22 == "1" | P5_31_22 == "1" ~ 1,
                                            P5_11_23 == "1" | P5_31_23 == "1" ~ 1,
                                            P5_11_24 == "1" | P5_31_24 == "1" ~ 1,
                                            P5_11_25 == "1" | P5_31_25 == "1" ~ 1,
                                            T ~ 0),
         suspension_condicional_char = case_when(P5_11_03 == "1" | P5_31_03 == "1" ~ "Delito sujeto a suspensión condicional",
                                            P5_11_11 == "1" | P5_31_11 == "1" ~ "Delito sujeto a suspensión condicional",
                                            P5_11_13 == "1" | P5_31_13 == "1" ~ "Delito sujeto a suspensión condicional",
                                            P5_11_14 == "1" | P5_31_14 == "1" ~ "Delito sujeto a suspensión condicional",
                                            P5_11_15 == "1" | P5_31_15 == "1" ~ "Delito sujeto a suspensión condicional",
                                            P5_11_19 == "1" | P5_31_19 == "1" ~ "Delito sujeto a suspensión condicional",
                                            P5_11_21 == "1" | P5_31_21 == "1" ~ "Delito sujeto a suspensión condicional",
                                            P5_11_22 == "1" | P5_31_22 == "1" ~ "Delito sujeto a suspensión condicional",
                                            P5_11_23 == "1" | P5_31_23 == "1" ~ "Delito sujeto a suspensión condicional",
                                            P5_11_24 == "1" | P5_31_24 == "1" ~ "Delito sujeto a suspensión condicional",
                                            P5_11_25 == "1" | P5_31_25 == "1" ~ "Delito sujeto a suspensión condicional",
                                            T ~ "Delito no sujeto a suspensión condicional"),
         pr_prev_o = case_when(tipo_prision_preventiva == "Prisión Preventiva Oficiosa" ~ 1,
                               tipo_prision_preventiva == "Prisión Preventiva Justificada" ~ 0,
                               tipo_prision_preventiva == "Proceso en libertad" ~ 0,
                               T ~ NA_real_),
         pr_prev_j = case_when(tipo_prision_preventiva == "Prisión Preventiva Oficiosa" ~ 0,
                               tipo_prision_preventiva == "Prisión Preventiva Justificada" ~ 1,
                               tipo_prision_preventiva == "Proceso en libertad" ~ 0,
                               T ~ NA_real_),
         pr_prev_l = case_when(tipo_prision_preventiva == "Prisión Preventiva Oficiosa" ~ 0,
                               tipo_prision_preventiva == "Prisión Preventiva Justificada" ~ 0,
                               tipo_prision_preventiva == "Proceso en libertad" ~ 1,
                               T ~ NA_real_),
         prueba_confesion = case_when(P5_15_01 == 1 ~ 1,
                                      as.numeric(P5_35_01) == 1 ~ 1,
                                      P5_15_01 == 0 ~ 0,
                                      as.numeric(P5_35_01) == 2 ~ 0,
                                      T ~ NA_real_),
         prueba_declaraciones = case_when(P5_15_02 == 1  | P5_15_03 == 1 |
                                            P5_15_04 == 1| P5_15_05 == 1 | 
                                            P5_15_06 == 1~ 1,
                                          as.numeric(P5_35_02) == 1 | as.numeric(P5_35_03) == 1 |
                                            as.numeric(P5_35_04) == 1 |as.numeric(P5_35_05) == 1 |
                                            as.numeric(P5_35_06) == 1 ~ 1,
                                          P5_15_02 == 0  | P5_15_03 == 0 |
                                            P5_15_04 == 0| P5_15_05 == 0 | 
                                            P5_15_06 == 0~ 0,
                                          as.numeric(P5_35_02) == 2 | as.numeric(P5_35_03) == 2 |
                                            as.numeric(P5_35_04) == 2 |as.numeric(P5_35_05) == 2 |
                                            as.numeric(P5_35_06) == 2 ~ 0,
                                          as.numeric(P5_35_02) == 3 | as.numeric(P5_35_03) == 3 |
                                            as.numeric(P5_35_04) == 3 |as.numeric(P5_35_05) == 3 |
                                            as.numeric(P5_35_06) == 3 ~ 0,
                                          T ~ NA_real_),
         prueba_fisicas = case_when(P5_15_07 == 1  | P5_15_08 == 1 |
                                      P5_15_09 == 1| P5_15_10 == 1 | 
                                      P5_15_11 == 1~ 1,
                                    as.numeric(P5_35_07) == 1 | as.numeric(P5_35_08) == 1 |
                                      as.numeric(P5_35_09) == 1 |as.numeric(P5_35_10) == 1 |
                                      as.numeric(P5_35_11) == 1 ~ 1,
                                    P5_15_07 == 0  | P5_15_08 == 0 |
                                      P5_15_09 == 0| P5_15_10 == 0 | 
                                      P5_15_11 == 0~ 0,
                                    as.numeric(P5_35_07) == 2 | as.numeric(P5_35_08) == 2 |
                                      as.numeric(P5_35_09) == 2 |as.numeric(P5_35_10) == 2 |
                                      as.numeric(P5_35_11) == 2 ~ 0,
                                    as.numeric(P5_35_09) == 3  ~ 0,
                                    T ~ NA_real_),
         tipo_prueba = case_when(prueba_confesion == 1 ~ "Confesión", 
                                 prueba_declaraciones == 1 ~ "Declaraciones",
                                 prueba_fisicas == 1 ~ "Física",
                                 prueba_confesion == 0 & prueba_declaraciones == 0 & prueba_fisicas == 0 ~ "Ninguna",
                                 T ~ NA_character_),
         tipo_detencion = case_when(flagrancia == 1 ~ "Flagrancia",
                                    inspeccion == 1 ~ "Inspección",
                                    orden_det == 1 ~ "Orden de detención",
                                    det_ninguna == 1 ~ "Detención Irregular")
         )


#### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Functions                                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


BarCategoricalBarsNames.fn <- function(data = Main_database_2008,
                                       column1,
                                       column2) {
  # Data manipulation
  data2plot <- data %>%
    select({{column1}}, {{column2}}) %>%
    drop_na() %>%
    group_by({{column1}}, {{column2}}) %>%
    summarise(Frequency = n(), .groups = 'drop') %>%
    group_by({{column1}}) %>%
    mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
    ungroup() %>%
    mutate(figure = paste0(round(Percentage, 0), "%"),
           labels = str_wrap(paste({{column1}}, {{column2}}, sep=" - "), width = 20),
           Value = str_wrap({{column1}}, width = 20)) %>%
    filter(complete.cases(.)) %>%
    arrange({{column1}}, desc(Percentage))
  
  
  # Plot
  plot <- ggplot(data2plot,
                 aes(
                   x     = Value, 
                   y     = Percentage,
                   fill  = {{column2}},
                   label =  paste0(figure, ", ", "N =", Frequency, ", ",{{column2}}),
                 )) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9, position = "dodge") +
    geom_text(aes(y = 50), 
              position = position_dodge(width = 0.9),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold", 
              size = 3.514598) +
    scale_fill_manual(values = colors4plot) +
    scale_y_continuous(limits = c(0, 105),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%"),
                       position = "right") +
    coord_flip() +
    theme(
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      panel.grid.major   = element_line(size = 0.25,
                                        colour = "#5e5c5a",
                                        linetype = "dashed"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#D0D1D3"),       
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank())
  
  return(list(plot = plot, data = data2plot))
}



#


#### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Plots.                                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# 3.1.6

data2plot <- Main_database_2008 %>%
  mutate(Comp_interrogatorio_1 = case_when(P4_3A_1 == "1" ~ 1,
                                           P4_3A_1 == "2" ~ 0,
                                           T ~ NA_real_),
         Comp_interrogatorio_2 = case_when(P4_3A_2 == "1" ~ 1,
                                           P4_3A_2 == "2" ~ 0,
                                           T ~ NA_real_),
         Comp_interrogatorio_3_4 = case_when(P4_3A_3 == "1" ~ 1,
                                             P4_3A_4 == "1" ~ 1,
                                             P4_3A_3 == "2" & P4_3A_4 == "2" ~ 0,
                                             T ~ NA_real_),
         Comp_interrogatorio_5_6 = case_when(P4_3A_5 == "1" ~ 1,
                                             P4_3A_6 == "1" ~ 1,
                                             P4_3A_5 == "2" & P4_3A_6 == "2" ~ 0,
                                             T ~ NA_real_),
         Comp_interrogatorio_7_8 = case_when(P4_3A_7 == 1 ~ 1,
                                             P4_3A_8 == 1 ~ 1,
                                             P4_3A_7 == 0 & P4_3A_8 == 0 ~ 0,
                                             T ~ NA_real_),
         Comp_interrogatorio_9 = case_when(P4_3A_9 == 1 ~ 1,
                                           P4_3A_9 == 0 ~ 0,
                                           T ~ NA_real_),
         total = 1,) %>%
  group_by(total) %>%
  summarise(Comp_interrogatorio_1 = 100 * mean(Comp_interrogatorio_1, na.rm = T),
            Comp_interrogatorio_2 = 100 * mean(Comp_interrogatorio_2, na.rm = T),
            Comp_interrogatorio_3_4 = 100 * mean(Comp_interrogatorio_3_4, na.rm = T),
            Comp_interrogatorio_5_6 = 100 * mean(Comp_interrogatorio_5_6, na.rm = T),
            Comp_interrogatorio_7_8 = 100 * mean(Comp_interrogatorio_7_8, na.rm = T),
            Comp_interrogatorio_9 = 100 * mean(Comp_interrogatorio_9, na.rm = T)) %>%
  select(-total) %>%
  pivot_longer(cols = everything(), values_to = "value2plot", names_to = "Comportamiento") %>%
  mutate(labels = paste0(round(value2plot), "%"))

colors4plot <- c("Comp_interrogatorio_1" = "#20201a", 
                 "Comp_interrogatorio_2" = "#20205a", 
                 "Comp_interrogatorio_3_4" = "#2a2a94", 
                 "Comp_interrogatorio_5_6" = "#4e43dd", 
                 "Comp_interrogatorio_7_8" = "#756ef9", 
                 "Comp_interrogatorio_9" = "#9c94ff")


  
  
  # Plot
  plot <- ggplot(data2plot,
                 aes(
                   x     = Comportamiento, 
                   y     = value2plot,
                   fill  = Comportamiento,
                   label =  labels,
                 )) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9, position = "dodge") +
    geom_text(aes(y = 50), 
              position = position_dodge(width = 0.9),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold", 
              size = 3.514598) +
    scale_fill_manual(values = colors4plot) +
    scale_y_continuous(limits = c(0, 105),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%"),
                       position = "right") +
    coord_flip() +
    theme(
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      panel.grid.major   = element_line(size = 0.25,
                                        colour = "#5e5c5a",
                                        linetype = "dashed"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#D0D1D3"),       
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank())
  


ggsave(plot = plot, 
       filename = paste0(path2DB,
                         "/National/Exploration",
                         "/Output/Politica_criminal/Interrogatorios_prision_preventiva/3_1_6.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")          
             
# 3.1.16a

data2plot <- Main_database_2008 %>%
  filter(P4_3A_3 == "1") %>%
  mutate(Comp_interrogatorio_1 = case_when(P4_3A_1 == "1" ~ 1,
                                           P4_3A_1 == "2" ~ 0,
                                           T ~ NA_real_),
         Comp_interrogatorio_2 = case_when(P4_3A_2 == "1" ~ 1,
                                           P4_3A_2 == "2" ~ 0,
                                           T ~ NA_real_),
         Comp_interrogatorio_4 = case_when(P4_3A_4 == "1" ~ 1,
                                             P4_3A_4 == "2" ~ 0,
                                             T ~ NA_real_),
         Comp_interrogatorio_5_6 = case_when(P4_3A_5 == "1" ~ 1,
                                             P4_3A_6 == "1" ~ 1,
                                             P4_3A_5 == "2" & P4_3A_6 == "2" ~ 0,
                                             T ~ NA_real_),
         Comp_interrogatorio_7_8 = case_when(P4_3A_7 == 1 ~ 1,
                                             P4_3A_8 == 1 ~ 1,
                                             P4_3A_7 == 0 & P4_3A_8 == 0 ~ 0,
                                             T ~ NA_real_),
         Comp_interrogatorio_9 = case_when(P4_3A_9 == 1 ~ 1,
                                           P4_3A_9 == 0 ~ 0,
                                           T ~ NA_real_),
         total = 1,) %>%
  group_by(total) %>%
  summarise(Comp_interrogatorio_1 = 100 * mean(Comp_interrogatorio_1, na.rm = T),
            Comp_interrogatorio_2 = 100 * mean(Comp_interrogatorio_2, na.rm = T),
            Comp_interrogatorio_4 = 100 * mean(Comp_interrogatorio_4, na.rm = T),
            Comp_interrogatorio_5_6 = 100 * mean(Comp_interrogatorio_5_6, na.rm = T),
            Comp_interrogatorio_7_8 = 100 * mean(Comp_interrogatorio_7_8, na.rm = T),
            Comp_interrogatorio_9 = 100 * mean(Comp_interrogatorio_9, na.rm = T)) %>%
  select(-total) %>%
  pivot_longer(cols = everything(), values_to = "value2plot", names_to = "Comportamiento") %>%
  mutate(labels = paste0(round(value2plot), "%"))

colors4plot <- c("Comp_interrogatorio_1" = "#20201a", 
                 "Comp_interrogatorio_2" = "#20205a", 
                 "Comp_interrogatorio_4" = "#2a2a94", 
                 "Comp_interrogatorio_5_6" = "#4e43dd", 
                 "Comp_interrogatorio_7_8" = "#756ef9", 
                 "Comp_interrogatorio_9" = "#9c94ff")


# Plot
plot <- ggplot(data2plot,
               aes(
                 x     = Comportamiento, 
                 y     = value2plot,
                 fill  = Comportamiento,
                 label =  labels,
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge") +
  geom_text(aes(y = 50), 
            position = position_dodge(width = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598) +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%"),
                     position = "right") +
  coord_flip() +
  theme(
    panel.background   = element_blank(),
    plot.background    = element_blank(),
    panel.grid.major   = element_line(size = 0.25,
                                      colour = "#5e5c5a",
                                      linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3"),       
    axis.title.y       = element_blank(),
    axis.title.x       = element_blank())



ggsave(plot = plot, 
       filename = paste0(path2DB,
                         "/National/Exploration",
                         "/Output/Politica_criminal/Interrogatorios_prision_preventiva/3_1_6a.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg") 


# 3.1.6b

# 3.1.7

colors4plot <- c("Física" = "#20204a",
                 "Psicológica" = "#2a2a94",
                 "Ambas" = "#4e43dd")

plot <- BarCategoricalBarsNames.fn(column1 = Corporacion_grupos, column2 = tortura_tra )



ggsave(plot = plot$plot, 
       filename = paste0(path2DB,
                         "/National/Exploration",
                         "/Output/Politica_criminal/Interrogatorios_prision_preventiva/3_1_7.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg") 



# 3.1.8

colors4plot <- c("Ministerio Público" = "#20204a",
                 "Traslado" = "#2a2a94",
                 "Traslado y Ministerio Público" = "#4e43dd",
                 "Ninguno" = "#9c94ff")

plot <- BarCategoricalBarsNames.fn(column1 = tipo_detencion, column2 = tortura_lugar )




ggsave(plot = plot$plot, 
       filename = paste0(path2DB,
                         "/National/Exploration",
                         "/Output/Politica_criminal/Interrogatorios_prision_preventiva/3_1_8.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg") 



# 3.2.1

data2plot_1 <- Main_database_2008 %>%
  filter(Anio_arresto>=2016, !is.na(tipo_prision_preventiva), post_reforma_PPO == 0) %>%
  group_by(Anio_arresto) %>%
  summarise(count= n()) %>%
  ungroup()

data2plot_2 <- Main_database_2008 %>%
  filter(Anio_arresto>=2016, !is.na(tipo_prision_preventiva), post_reforma_PPO == 0) %>%
  group_by(Anio_arresto, tipo_prision_preventiva, post_reforma_PPO ) %>%
  summarise(count2 =n()) %>%
  left_join(data2plot_1, by="Anio_arresto") %>%
  mutate(value2plot = 100 * count2 / count,
         label = paste0(format(round(value2plot, 0),
                                ),
                           "%"))

data2plot_3 <- Main_database_2008 %>%
  filter(Anio_arresto>=2016, !is.na(tipo_prision_preventiva), post_reforma_PPO == 1) %>%
  group_by(Anio_arresto) %>%
  summarise(count= n()) %>%
  ungroup()

data2plot_4 <- Main_database_2008 %>%
  filter(Anio_arresto>=2016, !is.na(tipo_prision_preventiva), post_reforma_PPO == 1) %>%
  group_by(Anio_arresto, tipo_prision_preventiva, post_reforma_PPO) %>%
  summarise(count2 =n()) %>%
  left_join(data2plot_3, by="Anio_arresto") %>%
  mutate(value2plot = 100 * count2 / count,
         label = paste0(format(round(value2plot, 0),
         ),
         "%"))

data2plot <- bind_rows(data2plot_2,data2plot_4)

colors4plot <- c("#20204a", "#2a2a94", "#4e43dd")


plot <- ggplot(data2plot,
               aes(
                 x     = Anio_arresto, 
                 y     = value2plot,
                 group = tipo_prision_preventiva,
                 color  = tipo_prision_preventiva,
                 label = label),
               ) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = "2019", linetype = "dashed", color = "black") +
  geom_text(aes(y = 50), 
            position = position_dodge(width = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%"),
                     position = "right") +
  theme(
    panel.background   = element_blank(),
    plot.background    = element_blank(),
    panel.grid.major   = element_line(size = 0.25,
                                      colour = "#5e5c5a",
                                      linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3"),       
    axis.title.y       = element_blank(),
    axis.title.x       = element_blank())



ggsave(plot = plot, 
       filename = paste0(path2DB,
                         "/National/Exploration",
                         "/Output/Politica_criminal/Interrogatorios_prision_preventiva/3_2_1.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg") 



# 3.2.2

colors4plot <- c("Proceso en libertad" = "#20204a",
                 "Prisión Preventiva Oficiosa" = "#2a2a94",
                 "Prisión Preventiva Justificada" = "#4e43dd")

plot <- BarCategoricalBarsNames.fn(column1 = suspension_condicional_char, column2 = tipo_prision_preventiva )



ggsave(plot = plot$plot, 
       filename = paste0(path2DB,
                         "/National/Exploration",
                         "/Output/Politica_criminal/Interrogatorios_prision_preventiva/3_2_2.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg") 



# 3.2.4


colors4plot <- c("Confesión" = "#20204a",
                 "Declaraciones" = "#2a2a94",
                 "Física" = "#4e43dd",
                 "Ninguna" = "#9c94ff")

plot <- BarCategoricalBarsNames.fn(column1 = tipo_prision_preventiva, column2 = tipo_prueba )



ggsave(plot = plot$plot, 
       filename = paste0(path2DB,
                         "/National/Exploration",
                         "/Output/Politica_criminal/Interrogatorios_prision_preventiva/3_2_4.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg") 

