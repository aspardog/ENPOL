## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploracion 2.2 nueva
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres            (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     July 29 23th, 2024
##
## This version:      July 29th, 2024
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
## 2.2.1 Tipo de acciones realizadas por las personas al momento de su detención                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

acciones_detencion.fn <- function(
    
  datas.df = master_data.df
  
) {
  
  df <- data.df %>%
    filter(Anio_arresto >= 2008,
           NSJP == 1) %>% 
    mutate(Acc_detencion_1 = case_when(P3_15_1 == 1 ~ 1,
                                             P3_15_1 == 0 ~ 0,
                                             T ~ NA_real_),
           Acc_detencion_2 = case_when(P3_15_2 == 1 ~ 1,
                                       P3_15_2 == 0 ~ 0,
                                             T ~ NA_real_),
           Acc_detencion_3 = case_when(P3_15_3 == 1 ~ 1,
                                       P3_15_3 == 0 ~ 0,
                                               T ~ NA_real_),
           Acc_detencion_4 = case_when(P3_15_4 == 1 ~ 1,
                                       P3_15_4 == 0 ~ 0,
                                         T ~ NA_real_),
           Acc_detencion_5 = case_when(P3_15_5 == 1 ~ 1,
                                       P3_15_5 == 0 ~ 0,
                                               T ~ NA_real_),
           Acc_detencion_6 = case_when(P3_15_6 == 1 ~ 1,
                                       P3_15_6 == 0 ~ 0,
                                         T ~ NA_real_),
           Acc_detencion_7 = case_when(P3_15_7 == 1 ~ 1,
                                       P3_15_7 == 0 ~ 0,
                                               T ~ NA_real_),
           Acc_detencion_8 = case_when(P3_15_8 == 1 ~ 1,
                                       P3_15_8 == 0 ~ 0,
                                         T ~ NA_real_),
           Acc_detencion_9 = case_when(P3_15_9 == 1 ~ 1,
                                       P3_15_9 == 0 ~ 0,
                                             T ~ NA_real_)) %>% 
    select(starts_with("Acc_"))
  
  
  data2plot <- df %>%
    pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
    drop_na() %>% 
    group_by(Column) %>% 
    summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100)) %>% 
    rename(values = Column, 
           value2plot = Percentage) %>% 
    mutate(
      labels = case_when(values == "Acc_detencion_1"     ~ "Obedeció las órdenes del policía o autoridad que lo(a)detuvo", 
                          values == "Acc_detencion_2"     ~ "Portaba alguna arma punzo cortante (navajas, cuchillos, machetes, etc.)",
                         values == "Acc_detencion_3"   ~ "Portaba alguna arma de fuego (incluya réplicas)",
                         values == "Acc_detencion_4"   ~ "Amenazó a alguien con el arma (cualquier persona o autoridad)",
                         values == "Acc_detencion_5"   ~ "Disparó el arma de fuego (sólo personas que llevaban arma de fuego)", 
                         values == "Acc_detencion_6"   ~ "Manipuló algún objeto para usarlo como arma", 
                         values == "Acc_detencion_7"   ~ "Trató de sobornar a la autoridad para evitar su detención", 
                         values == "Acc_detencion_8"   ~ "Trató de defenderse físicamente", 
                         values == "Acc_detencion_9"   ~ "Trató de escapar para que no lo(a) detuvieran"),
      figure = paste0(round(value2plot, 0), "%"),
      labels = str_wrap(labels, width = 20),
      order_var = rank(value2plot))
  
  
  
  colors4plot <- rep("#2a2a94", 9)
  
  
  plt <- ggplot(data2plot, 
                aes(x     = reorder(labels, order_var),
                    y     = value2plot,
                    label = figure,
                    color = labels)) +
    geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
             show.legend = F, width = 0.9) +
    scale_fill_manual(values = colors4plot) +
    geom_text(aes(y    = value2plot + 10 ),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%"),
                       position = "right") +
    scale_x_discrete( ) +
    WJP_theme() +
    theme(legend.position="none",
          panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                            size = 0.5),
          panel.grid.major.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text = element_text(family = "Lato Bold"),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(family = "Lato Medium",
                                   size = 3.514598*.pt,
                                   color = "Black", hjust = 0),
          legend.title = element_blank())+
    coord_flip(); plt
  
  ggsave(plot   = plt,
         file   = paste0(path2SP,"/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Entrevistas/Figure2_2_nueva.svg"), 
         width  = 94.89, 
         height = 170,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
}

