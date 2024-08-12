## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Uso excesivo de la autoridad
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Junio 16, 2024
##
## This version:      Junio 16, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. delitos sentenciados en ENPOL, 2018-2021                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

delitos_ENPOL.fn <- function(
  
  data.df = master_data.df  
  
  ){ 
  Main_database1 <- data.df  %>%
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
  
  data2plot <- Main_database1 %>%
    filter(Anio_arresto >= 2018, !is.na(Delito_unico_ungrouped_categ), sentenciado == 1) %>%
    group_by(Delito_unico_ungrouped_categ) %>%
    summarise(n = n()) %>%
    mutate(value2plot =  100 * n / sum(n),
           labels = paste0(round(value2plot,0),"%"),
           group_var = "Arrestos",
           Delito = Delito_unico_ungrouped_categ,
           Delito = str_wrap(Delito, width = 30)) %>%
    select(Delito,value2plot,labels,group_var, n) %>%
    arrange(value2plot) %>%
    mutate(Delito = factor(Delito, levels = Delito))
  
  colors4plot <- rep(mainColor, 26)
  
  
  plt <- ggplot(data2plot, 
                aes(x     = Delito,
                    y     = value2plot,
                    label = labels,
                    group = Delito,
                    color = Delito,
                    fill  = Delito)) +  # Añadido fill para asegurar coincidencia
    geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
    scale_fill_manual(values = colors4plot) +
    geom_text(aes(y    = value2plot + 2 ),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    scale_y_continuous(limits = c(0, 20),
                       breaks = seq(0, 100, 10),
                       labels = paste0(seq(0, 100, 10), "%"),
                       position = "right") +
    scale_x_discrete() +
    expand_limits(y = c(0, 25)) +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_blank(),
          axis.title.x       = element_blank(),
          axis.text.y=element_text(family   = "Lato Full",
                                   face     = "bold",
                                   size     = 3.514598*.pt,
                                   color    = "#524F4C",
                                   margin   = margin(0, 10, 0, 0),
                                   hjust = 0)) +
    coord_flip(); plt
  

  
  ggsave(plot   = plt,
         file   = paste0(
           path2SP,
           "/National/Visualization",
           "/Output/Politica criminal/",
           savePath,"/Delitos victimas",
           "/Figure1_1.svg"),
         width  = 180, 
         height = 180,
         units  = "mm",
         dpi    = 72,
         device = "svg") 
  
  
  return(data2plot)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Distribución de delitos registrados en ENVIPE, 2018-2021                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

delitos_ENVIPE.fn <- function( ){
  
  ENVIPE <- read_dta(paste0(path2SP,"/National/Exploration/Input/Politica_criminal/ocurrencias.dta")) %>%
  pivot_longer(cols = del_1:del_15, names_to = "Delito_envipe",values_to = "Ocurrencias")  %>%
  group_by(anio, Delito_envipe) %>%
  summarise(Ocurrencias = sum(Ocurrencias, na.rm = T)) %>%
  mutate(Delito_envipe = case_when(Delito_envipe == "del_1" ~ "Robo de vehículo", 
                                   Delito_envipe == "del_2" ~ "Robo de autopartes", 
                                   Delito_envipe == "del_3" ~ "Pinta de barda u otro\n tipo de vandalismo", 
                                   Delito_envipe == "del_4" ~ "Entraron sin permiso y robó\n o intentó robar algo", 
                                   Delito_envipe == "del_5" ~ "Robo en la calle o en el\n transporte público", 
                                   Delito_envipe == "del_6" ~ "Otro tipo de robo", 
                                   Delito_envipe == "del_7" ~ "Fraude bancario", 
                                   Delito_envipe == "del_8" ~ "Fraude al consumidor", 
                                   Delito_envipe == "del_9" ~ "Extorsión", 
                                   Delito_envipe == "del_10" ~ "Amenazas", 
                                   Delito_envipe == "del_11" ~ "Lesiones", 
                                   Delito_envipe == "del_12" ~ "Secuestro", 
                                   Delito_envipe == "del_13" ~ "Otros delitos sexuales", 
                                   Delito_envipe == "del_14" ~ "Violación sexual", 
                                   Delito_envipe == "del_15" ~ "Otro"))


  data2plot <- ENVIPE %>%
    filter(anio >= 2018) %>%
    group_by(Delito_envipe) %>%
    summarise(n = sum(Ocurrencias)) %>%
    mutate(value2plot =  100 * n / sum(n),
           labels = paste0(round(value2plot,0),"%"),
           group_var = "ENVIPE",
           Delito = Delito_envipe,
           Delito = str_wrap(Delito, width = 30)) %>%
    select(Delito,value2plot,labels,group_var, n) %>%
    arrange(value2plot) %>%
    mutate(Delito = factor(Delito, levels = Delito))
  
  colors4plot <- rep("#a90099", 15)
  
  plt <- ggplot(data2plot, 
                aes(x     = Delito,
                    y     = value2plot,
                    label = labels,
                    group = Delito,
                    color = Delito,
                    fill  = Delito)) +  # Añadido fill para asegurar coincidencia
    geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
    scale_fill_manual(values = colors4plot) +
    geom_text(aes(y    = value2plot + 2 ),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    scale_y_continuous(limits = c(0, 20),
                       breaks = seq(0, 100, 10),
                       labels = paste0(seq(0, 100, 10), "%"),
                       position = "right") +
    scale_x_discrete() +
    expand_limits(y = c(0, 25)) +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_blank(),
          axis.title.x       = element_blank(),
          axis.text.y=element_text(family   = "Lato Full",
                                   face     = "bold",
                                   size     = 3.514598*.pt,
                                   color    = "#524F4C",
                                   margin   = margin(0, 10, 0, 0),
                                   hjust = 0)) +
    coord_flip(); plt
  

  
  
  ggsave(plot   = plt,
         file   = paste0(
           path2SP,
           "/National/Visualization",
           "/Output/Politica criminal/",
           savePath,"/Delitos victimas",
           "/Figure1_2.svg"), 
         width  = 180, 
         height = 225,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
  
}


# ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ##
# ## 3. Proporción de sentencias de los delitos más prevalentes según ENVIPE, 2018-2021                                                            ----
# ##
# ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# 
# # requiere de Main_database1
# 
# 
# 
# Main_database2 <- Main_database %>%
#   filter(Anio_arresto >= 2018, 
#          Delito_unico == 1, 
#          sentenciado == 1) %>%
#   mutate(Delito_prioritario_ENVIPE = case_when(Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ "Robo en transporte público",
#                                                Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ "Robo a transeúnte en vía pública",
#                                                Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ "Robo de autopartes",
#                                                Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ "Comercio ilegal de drogas",
#                                                Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ "Daño a la propiedad",
#                                                Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ "Extorsión",
#                                                T ~ NA_character_))
# 
# data2plot <- Main_database2 %>%
#   filter(!is.na(Delito_prioritario_ENVIPE)) %>%
#   group_by(Delito_prioritario_ENVIPE) %>%
#   summarise(n = n()) %>%
#   mutate(value2plot =  100 * n / sum(n),
#          labels = paste0(round(value2plot,0),"%"),
#          group_var = "Arrestos",
#          Delito = Delito_prioritario_ENVIPE,
#          Delito = str_wrap(Delito, width = 30)) %>%
#   select(Delito,value2plot,labels,group_var) %>%
#   arrange(value2plot) %>%
#   mutate(Delito = factor(Delito, levels = Delito))
# 
# colors4plot <- rep(mainColor, 6)
# 
# 
# plt <- ggplot(data2plot, 
#               aes(x     = Delito,
#                   y     = value2plot,
#                   label = labels,
#                   group = group_var,
#                   color = Delito)) +
#   geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
#            show.legend = F, width = 0.9) +
#   scale_fill_manual(values = colors4plot) +
#   geom_text(aes(y    = value2plot +.9 ),
#             color    = "#4a4a49",
#             family   = "Lato Full",
#             fontface = "bold") +
#   labs(y = "% of respondents") +
#   scale_y_continuous(limits = c(0, 60),
#                      breaks = seq(0,100,20),
#                      labels = paste0(seq(0,100,20), "%"),
#                      position = "right") +
#   scale_x_discrete( ) +
#   expand_limits(y = c(0, 50))+
#   WJP_theme() +
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.major.x = element_line(color = "#D0D1D3"),
#         axis.title.y       = element_blank(),
#         axis.title.x       = element_blank(),
#         axis.text.y        = element_text(hjust = 1, size = 10))+
#   coord_flip()
# 
# plt
# 
# ggsave(plot   = plt,
#        file   = paste0(path2SP,"National/Report/prueba/Capitulo 2/charts_and_images/delitos_relevantes_victimas/Figure1_3.svg"), 
#        width  = 189.7883, 
#        height = 85,
#        units  = "mm",
#        dpi    = 72,
#        device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1 Compración Prioridad ENPOL vs. ENVIPE                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
#  #llevar a settings
# library(rdbnomics)
# library(ggbump)
# library(MetBrewer)
# library(scales)
# 
# Main_database1 <- data.df  %>%
#   mutate(Delito_prioritario_ENVIPE = case_when(Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
#                                                Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
#                                                Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ 1,
#                                                Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ 1,
#                                                Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ 1,
#                                                Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ 1,
#                                                T ~ 0),
#          Delito_unico_ungrouped_categ = case_when(Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ "Robo de casa habitación",
#                                                   Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ "Robo de vehículo",
#                                                   Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ "Robo a negocio",
#                                                   Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ "Robo en transporte público",
#                                                   Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ "Robo a transeunte en vía pública",
#                                                   Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ "Robo de autopartes",
#                                                   Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ "Robo en forma distinta a las anteriores",
#                                                   Delito_unico == 1 & (P5_11_08 == 1|P5_31_08 == 1) ~ "Posesión ilegal de drogas",
#                                                   Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ "Comercio ilegal de drogas",
#                                                   Delito_unico == 1 & (P5_11_10 == 1|P5_31_10 == 1) ~ "Lesiones",
#                                                   Delito_unico == 1 & (P5_11_11 == 1|P5_31_11 == 1) ~ "Homicidio culposo",
#                                                   Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ "Homicidio doloso",
#                                                   Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ "Portación ilegal de armas",
#                                                   Delito_unico == 1 & (P5_11_14 == 1|P5_31_14 == 1) ~ "Incumplimiento de obligaciones de asistencia familiar",
#                                                   Delito_unico == 1 & (P5_11_15 == 1|P5_31_15 == 1) ~ "Violencia familiar",
#                                                   Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ "Daño a la propiedad",
#                                                   Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ "Secuestro o secuestro expres",
#                                                   Delito_unico == 1 & (P5_11_18 == 1|P5_31_18 == 1) ~ "Violación sexual",
#                                                   Delito_unico == 1 & (P5_11_19 == 1|P5_31_19 == 1) ~ "Fraude",
#                                                   Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ "Delincuencia organizada",
#                                                   Delito_unico == 1 & (P5_11_21 == 1|P5_31_21 == 1) ~ "Otros delitos sexuales",
#                                                   Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ "Extorsión",
#                                                   Delito_unico == 1 & (P5_11_23 == 1|P5_31_23 == 1) ~ "Privación de la libertad",
#                                                   Delito_unico == 1 & (P5_11_24 == 1|P5_31_24 == 1) ~ "Abuso de confianza",
#                                                   Delito_unico == 1 & (P5_11_25 == 1|P5_31_25 == 1) ~ "Amenazas",
#                                                   Delito_unico == 1 & (P5_11_26 == 1|P5_31_26 == 1) ~ "Otro",
#                                                   T ~ NA_character_)) 
# 
# data2plot_ENPOL <- Main_database1 %>%
#   filter(Anio_arresto >= 2018, !is.na(Delito_unico_ungrouped_categ), sentenciado == 1) %>%
#   group_by(Delito_unico_ungrouped_categ) %>%
#   summarise(n = n()) %>%
#   mutate(value2plot =  100 * n / sum(n),
#          labels = paste0(round(value2plot,0),"%"),
#          group_var = "Arrestos",
#          Delito = Delito_unico_ungrouped_categ,
#          # Delito = str_wrap(Delito, width = 30)
#   ) %>%
#   select(Delito,value2plot,labels,group_var, n) %>%
#   arrange(value2plot) %>%
#   mutate(Delito = factor(Delito, levels = Delito),
#          group_var = "ENPOL")
# 
# 
# ENVIPE <- read_dta(paste0(path2SP,"/National/Exploration/Input/Politica_criminal/ocurrencias.dta")) %>%
#   pivot_longer(cols = del_1:del_15, names_to = "Delito_envipe",values_to = "Ocurrencias")  %>%
#   group_by(anio, Delito_envipe) %>%
#   summarise(Ocurrencias = sum(Ocurrencias, na.rm = T)) %>%
#   mutate(Delito_envipe = case_when(Delito_envipe == "del_1" ~ "Robo de vehículo", 
#                                    Delito_envipe == "del_2" ~ "Robo de autopartes", 
#                                    Delito_envipe == "del_3" ~ "Pinta de barda u otro\n tipo de vandalismo", 
#                                    Delito_envipe == "del_4" ~ "Entraron sin permiso y robó\n o intentó robar algo", 
#                                    Delito_envipe == "del_5" ~ "Robo en la calle o en el\n transporte público", 
#                                    Delito_envipe == "del_6" ~ "Otro tipo de robo", 
#                                    Delito_envipe == "del_7" ~ "Fraude bancario", 
#                                    Delito_envipe == "del_8" ~ "Fraude al consumidor", 
#                                    Delito_envipe == "del_9" ~ "Extorsión", 
#                                    Delito_envipe == "del_10" ~ "Amenazas", 
#                                    Delito_envipe == "del_11" ~ "Lesiones", 
#                                    Delito_envipe == "del_12" ~ "Secuestro", 
#                                    Delito_envipe == "del_13" ~ "Otros delitos sexuales", 
#                                    Delito_envipe == "del_14" ~ "Violación sexual ", 
#                                    Delito_envipe == "del_15" ~ "Otro"))
# 
# 
# data2plot_ENVIPE <- ENVIPE %>%
#   filter(anio >= 2018) %>%
#   group_by(Delito_envipe) %>%
#   summarise(n = sum(Ocurrencias)) %>%
#   mutate(value2plot =  100 * n / sum(n),
#          labels = paste0(round(value2plot,0),"%"),
#          group_var = "ENVIPE",
#          Delito = Delito_envipe,
#          Delito = str_wrap(Delito, width = 30)) %>%
#   select(Delito,value2plot,labels,group_var, n) %>%
#   arrange(value2plot) %>%
#   mutate(Delito = factor(Delito, levels = Delito))
# 
# 
# data2plot <- bind_rows(data2plot_ENVIPE, data2plot_ENPOL)
# 
# data2plot <- data2plot %>% 
#   mutate(category = case_when(Delito == "Entraron sin permiso y robó o intentó a robar algo" ~  "Robo a casa habitación",
#                               Delito == "Robo a casa habitación"                             ~  "Robo a casa habitación",
#                               Delito == "Pinta de barda u otro tipo de\nvandalismo" ~  "Daño a la propiedad",
#                               Delito == "Daño a la propiedad"                      ~  "Daño a la propiedad",
#                               Delito == "Robo en la calle o en el\ntransporte público" ~  "Robo en la calle o en el transporte público",
#                               Delito == "Robo a transeunte en vía pública"            ~  "Robo en la calle o en el transporte público",
#                               Delito == "Robo en transporte público"                  ~  "Robo en la calle o en el transporte público",
#                               Delito == "Extorsión"                                   ~  "Extorsión",
#                               Delito == "Fraude bancario"                            ~  "Fraude",
#                               Delito == "Fraude al consumidor"                      ~  "Fraude",
#                               Delito == "Fraude"                                    ~  "Fraude",
#                               Delito == "Otro tipo de robo"                            ~  "Otro tipo de robo",
#                               Delito == "Robo en forma distinta a las anteriores"      ~  "Otro tipo de robo",
#                               Delito == "Robo de vehículo"                             ~  "Robo de vehículo",
#                               Delito == "Robo de autopartes"                             ~  "Robo de vehículo",
#                               Delito == "Lesiones"                                     ~  "Lesiones",
#                               Delito == "Violación sexual"                             ~  "Violación sexual",
#                               Delito == "Secuestro"                                    ~  "Secuestro",
#                               Delito == "Secuestro o secuestro expres"                 ~  "Secuestro",
#                               Delito == "Privación de la libertad"                 ~  "Secuestro",
#                               Delito == "Otro"                                         ~  "Otro",
#                               Delito == "Amenazas"                                    ~  "Amenazas")) %>% 
#   rename(figure = labels,
#          labels = group_var) %>% 
#   drop_na() %>% 
#   select(category, labels, value2plot) %>% 
#   group_by(category, labels) %>% 
#   summarise(value2plot = sum(value2plot)) %>%
#   mutate(figure = paste0(round(value2plot,0),"%")) 
# 
# data2plot <- data2plot %>% 
#   mutate(labels = case_when(labels == "ENPOL" ~ 2019,
#                             labels == "ENVIPE" ~ 2020),
#          value2plot = case_when(category == "Violación sexual" & labels == 2020 ~ 0.43, 
#                                 category == "Fraude" & labels == 2020 ~ 11.5, 
#                                 T ~ value2plot))
# 
# countryPalette <- c("#2a2a94", "#a90099", "#3273ff", "#fa4d57", "#9d61f2", "#43a9a7", "#efa700", "#2c6d4f","#37C5CB", "#e2a4ff" )
# selected <-  c("Robo de autopartes" ,"Daño a la propiedad", "Robo en la calle o en el transporte público" ,"Extorsión", "Fraude",
#                "Robo de vehículo", "Lesiones", "Violación sexual", "Secuestro", "Amenazas", "Otro tipo de robo" ) 
# 
# plot <- data2plot %>% 
#   ggplot(aes(x = labels, y = value2plot, group = category)) +
#   geom_bump(linewidth = 0.6, color = "gray90", smooth = 6) +
#   geom_bump(aes(color = category), linewidth = 0.9, smooth = 6,
#             data = ~. |> filter(category %in% selected)) +
#   geom_point(color = "white", size = 4) +
#   geom_point(color = "gray90", size = 2) +
#   geom_point(aes(color = category), size = 2, 
#              data = ~. |> filter(category %in% selected)) +
#   geom_text(aes(label = category, y = value2plot - .1 ), x = 2020.05, hjust = 0,
#             color = "gray50", family = "Lato Full", size = 3,
#             data = data2plot %>% ungroup() %>% slice_max(order_by = labels, by = category) %>%
#               filter(!category %in% selected)) +
#   geom_text(aes(label = category, y = value2plot + .2  ), x = 2020.05, hjust = 0,
#             color = "black", family = "Lato Full", size = 3,
#             data = data2plot %>% ungroup() %>% slice_max(order_by = labels, by = category) %>%
#               filter(category %in% selected)) +
#   scale_color_manual(values = countryPalette) +
#   scale_x_continuous(limits = c(2018.9, 2020.5), expand = c(0.01,0),
#                      breaks = c(2019, 2020), labels = c("ENPOL", "ENVIPE")) +
#   scale_y_continuous(breaks = seq(from = 22, to = -1, by = -2), expand = c(0.02,0),
#                      labels = number_format(suffix = "%")) +
#   # theme_minimal(base_family = "Lato Full", base_size = 12) +
#   WJP_theme() +
#   theme(legend.position = "none",
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line(colour = "#d1cfd1"),
#         axis.title.x       = element_blank(),
#         axis.title.y       = element_blank(),
#         axis.line.x        = element_line(color    = "#d1cfd1"),
#         axis.ticks.x       = element_line(color    = "#d1cfd1",
#                                           linetype = "solid"),
#         axis.text.x       = element_text(color = "black")); plot
# 
# 
# ggsave(plot   = plot,
#        file   = paste0(
#          path2SP,
#          "/National/Visualization",
#          "/Output/Politica criminal/",
#          savePath,"/Delitos victimas",
#          "/Figure1_1_nueva.svg"), 
#        width  = 189.7883, 
#        height = 100,
#        units  = "mm",
#        dpi    = 72,
#        device = "svg")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.2 Compración Prioridad ENPOL vs. ENVIPE                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Main_database1 <- data.df  %>%
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
                                                  Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ "Portación ilegal de armas",
                                                  Delito_unico == 1 & (P5_11_14 == 1|P5_31_14 == 1) ~ "Incumplimiento de obligaciones de asistencia familiar",
                                                  Delito_unico == 1 & (P5_11_15 == 1|P5_31_15 == 1) ~ "Violencia familiar",
                                                  Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ "Daño a la propiedad",
                                                  Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ "Secuestro o secuestro expres",
                                                  Delito_unico == 1 & (P5_11_18 == 1|P5_31_18 == 1) ~ "Violación sexual",
                                                  Delito_unico == 1 & (P5_11_19 == 1|P5_31_19 == 1) ~ "Fraude",
                                                  Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ "Delincuencia organizada",
                                                  Delito_unico == 1 & (P5_11_21 == 1|P5_31_21 == 1) ~ "Otros delitos sexuales",
                                                  Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ "Extorsión",
                                                  Delito_unico == 1 & (P5_11_23 == 1|P5_31_23 == 1) ~ "Privación de la libertad",
                                                  Delito_unico == 1 & (P5_11_24 == 1|P5_31_24 == 1) ~ "Abuso de confianza",
                                                  Delito_unico == 1 & (P5_11_25 == 1|P5_31_25 == 1) ~ "Amenazas",
                                                  Delito_unico == 1 & (P5_11_26 == 1|P5_31_26 == 1) ~ "Otro",
                                                  T ~ NA_character_)) 

data2plot_ENPOL <- Main_database1 %>%
  filter(Anio_arresto >= 2018, !is.na(Delito_unico_ungrouped_categ), sentenciado == 1) %>%
  group_by(Delito_unico_ungrouped_categ) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "Arrestos",
         Delito = Delito_unico_ungrouped_categ,
         # Delito = str_wrap(Delito, width = 30)
  ) %>%
  select(Delito,value2plot,labels,group_var, n) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito),
         group_var = "ENPOL")


ENVIPE <- read_dta(paste0(path2SP,"/National/Exploration/Input/Politica_criminal/ocurrencias.dta")) %>%
  pivot_longer(cols = del_1:del_15, names_to = "Delito_envipe",values_to = "Ocurrencias")  %>%
  group_by(anio, Delito_envipe) %>%
  summarise(Ocurrencias = sum(Ocurrencias, na.rm = T)) %>%
  mutate(Delito_envipe = case_when(Delito_envipe == "del_1" ~ "Robo de vehículo", 
                                   Delito_envipe == "del_2" ~ "Robo de autopartes", 
                                   Delito_envipe == "del_3" ~ "Pinta de barda u otro\n tipo de vandalismo", 
                                   Delito_envipe == "del_4" ~ "Entraron sin permiso y robó\n o intentó robar algo", 
                                   Delito_envipe == "del_5" ~ "Robo en la calle o en el\n transporte público", 
                                   Delito_envipe == "del_6" ~ "Otro tipo de robo", 
                                   Delito_envipe == "del_7" ~ "Fraude bancario", 
                                   Delito_envipe == "del_8" ~ "Fraude al consumidor", 
                                   Delito_envipe == "del_9" ~ "Extorsión", 
                                   Delito_envipe == "del_10" ~ "Amenazas", 
                                   Delito_envipe == "del_11" ~ "Lesiones", 
                                   Delito_envipe == "del_12" ~ "Secuestro", 
                                   Delito_envipe == "del_13" ~ "Otros delitos sexuales", 
                                   Delito_envipe == "del_14" ~ "Violación sexual ", 
                                   Delito_envipe == "del_15" ~ "Otro"))


data2plot_ENVIPE <- ENVIPE %>%
  filter(anio >= 2018) %>%
  group_by(Delito_envipe) %>%
  summarise(n = sum(Ocurrencias)) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "ENVIPE",
         Delito = Delito_envipe,
         Delito = str_wrap(Delito, width = 30)) %>%
  select(Delito,value2plot,labels,group_var, n) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito))


data2plot <- bind_rows(data2plot_ENVIPE, data2plot_ENPOL)

data2plot <- data2plot %>% 
  mutate(category = case_when(Delito == "Entraron sin permiso y robó o\nintentó robar algo" ~  "Robo a casa habitación",
                              Delito == "Robo de casa habitación"                             ~  "Robo a casa habitación",
                              Delito == "Pinta de barda u otro tipo de\nvandalismo" ~  "Daño a la propiedad",
                              Delito == "Daño a la propiedad"                      ~  "Daño a la propiedad",
                              Delito == "Robo en la calle o en el\ntransporte público" ~  "Robo en la calle o en el transporte público",
                              Delito == "Robo a transeunte en vía pública"            ~  "Robo en la calle o en el transporte público",
                              Delito == "Robo en transporte público"                  ~  "Robo en la calle o en el transporte público",
                              Delito == "Extorsión"                                   ~  "Extorsión",
                              Delito == "Fraude bancario"                            ~  "Fraude",
                              Delito == "Fraude al consumidor"                      ~  "Fraude",
                              Delito == "Fraude"                                    ~  "Fraude",
                              Delito == "Otro tipo de robo"                            ~  "Otro tipo de robo",
                              Delito == "Robo en forma distinta a las anteriores"      ~  "Otro tipo de robo",
                              Delito == "Robo de vehículo"                             ~  "Robo de vehículo",
                              Delito == "Robo de autopartes"                             ~  "Robo de vehículo",
                              Delito == "Lesiones"                                     ~  "Lesiones",
                              Delito == "Violación sexual"                             ~  "Violación sexual",
                              Delito == "Otros delitos sexuales"                       ~  "Otros delitos sexuales",
                              Delito == "Secuestro"                                    ~  "Secuestro",
                              Delito == "Secuestro o secuestro expres"                 ~  "Secuestro",
                              Delito == "Privación de la libertad"                 ~  "Secuestro",
                              Delito == "Otro"                                         ~  "Otro",
                              Delito == "Amenazas"                                     ~  "Amenazas",
                              Delito == "Amenazas"                                     ~  "Amenazas",
                              T ~ Delito
                              )) %>% 
  rename(figure = labels,
         labels = group_var) %>% 
  drop_na() %>% 
  select(category, labels, value2plot) %>% 
  group_by(category, labels) %>% 
  summarise(value2plot = sum(value2plot)) %>%
  mutate(figure = paste0(round(value2plot,0),"%"))  %>% 
  mutate(order_value = case_when(
    category == "Robo de vehículo"                             ~  0,
    category == "Robo de autopartes"                          ~  1,
    category == "Daño a la propiedad"                         ~  2,
    category == "Robo en la calle o en el transporte público" ~  3,
    category == "Extorsión"                                   ~  4,
    category == "Robo a casa habitación"                       ~  5,
    category == "Fraude"                                       ~  6,
    category == "Amenazas"                                    ~  7,
    category == "Otro tipo de robo"                            ~  8,
    category == "Otros delitos sexuales"                       ~  9,
    category == "Lesiones"                                     ~  10,
    category == "Violación sexual"                              ~  11,
    category == "Otro"                                         ~  12,
    category == "Secuestro"                                    ~  13,
    category == "Robo a negocio"                               ~  14,
    category == "Homicidio doloso"                             ~  15,
    category == "Violación sexual"                             ~  16,
    category == "Posesión ilegal de drogas"                    ~  17,
    category == "Portación ilegal de armas"                    ~  18,
    category == "Posesión ilegal de drogas"                    ~  19,
    category == "Comercio ilegal de drogas"                    ~  20,
    category == "Homicidio culposo"                            ~  21,
    category == "Violencia familiar"                           ~  22,
    category == "Delincuencia organizada"                      ~  23,
    category == "Incumplimiento de obligaciones de asistencia familiar" ~  24,
    category == "Abuso de confianza"                           ~  25),
    category = str_wrap(category, width = 30))

colors4plot <- c("#2a2a9A", "#a90099")

names(colors4plot) <- c("ENPOL",
                        "ENVIPE")

plot <- ggplot(data2plot,
               aes(
                 x     = reorder(category, -order_value), 
                 y     = value2plot,
                 fill  = labels,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 1), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  coord_flip()+
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0,20,10),
                     labels = paste0(seq(0,20,10), "%"),
                     position = "right") +
  geom_vline(xintercept = 10.5, color = "#5e5c5a", linetype = "dashed")+
  theme(
    panel.background   = element_blank(),
    plot.background    = element_blank(),
    panel.grid.major   = element_line(size     = 0.25,
                                      colour   = "#5e5c5a",
                                      linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3"),       
    axis.title.y       = element_blank(),
    axis.title.x       = element_blank(),
    axis.text.y=element_text(family   = "Lato Full",
                             face     = "bold",
                             size     = 3.514598*.pt,
                             color    = "#524F4C",
                             margin   = margin(0, 10, 0, 0),
                             hjust = 0));plot



ggsave(plot   = plot,
       file   = paste0(
         path2SP,
         "/National/Visualization",
         "/Output/Politica criminal/",
         savePath,"/Delitos victimas",
         "/Figure1_2_nueva.svg"), 
       width  = 189.7883, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Proporción de sentencias de los delitos más prevalentes según ENVIPE, 2018-2021                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

prevalentes_ENVIPE.fn <- function(
  
  data.df = master_data.df  
  
  ){

      Main_database1 <- data.df  %>%
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
      
      
      data2plot <- Main_database1 %>%
        filter(Anio_arresto >= 2018) %>%
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
        select(Anio_arresto,Delito_prioritario_ENVIPE,value2plot,labels,group_var, n) %>%
        filter(Delito_prioritario_ENVIPE == 1)
      
      
      
      # Creating ggplot
      
      colors4plot <- c("#2a2a9A")
      
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
                                                linetype = "solid")); plt
      
      ggsave(plot   = plt,
             file   = paste0(
               path2SP,
               "/National/Visualization",
               "/Output/Politica criminal/",
               savePath,"/Delitos victimas",
               "/Figure1_3.svg"), 
             width  = 189.7883, 
             height = 85,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      return(data2plot)

}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Homicidios ENPOl SESNSP                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

homicidios_ENPOL_ENVIPE.fn <- function(
    
  ){

      snsp <- read.csv(paste0(path2SP,"/National/Exploration/Input/IDEFC_NM_abr24.csv"),check.names = F)
      
      inegi <- read_xlsx(paste0(path2SP,"/National/Exploration/Input/INEGI_homicidios.xlsx"))
      
      
      snsp_2<- snsp %>%
        mutate(Agosto = case_when(Anio==2021 ~ 0,     T ~ Agosto),
               Septiembre = case_when(Anio==2021 ~ 0, T ~ Septiembre),
               Octubre = case_when(Anio==2021 ~ 0,    T ~ Octubre),
               Noviembre = case_when(Anio==2021 ~ 0,  T ~ Noviembre),
               Diciembre = case_when(Anio==2021 ~ 0,  T ~ Diciembre)) %>%
        filter(Anio>=2018 & Anio<= 2021) %>%
        mutate(Incidencia = Enero+Febrero+Marzo+Abril+Mayo+Junio+Julio+Agosto+Septiembre+Octubre+Noviembre+Diciembre) %>%
        mutate(Delito=case_when(`Subtipo de delito`== "Homicidio doloso" ~ "Homicidio doloso, SESNSP",
                                T ~ "Otro")) %>% 
        group_by(Delito,Anio) %>%
        summarize(Incidencia = sum(Incidencia)) %>%
        mutate(Anio=as.character(Anio),
               value2plot =  Incidencia,
               labels = paste0(round(value2plot,0)),
               group_var =  "Homicidios_SNSP") %>%
        filter(Delito!="Otro")
      
      
      enpol_3 <- Main_database %>% 
        filter(Anio_arresto>=2018 & Anio_arresto<= 2021 )  %>%
        select(Anio_arresto,Delito_gr_1_robos,Delito_gr_2_drogas,Delito_gr_3_del_org,Delito_gr_4_lesiones,Delito_gr_5_hom_cul,
               Delito_gr_6_hom_dol,Delito_gr_7_armas,Delito_gr_8_viol_fam,Delito_gr_9_secuestro,Delito_gr_10_sexuales,
               Delito_gr_11_extorsion,Delito_gr_12_fraude,Delito_gr_13_amenazas,Delito_gr_14_otro,Delito_gr_15_ns_nr,FAC_PER)  %>% 
        pivot_longer(cols = c(Delito_gr_1_robos,Delito_gr_2_drogas,Delito_gr_3_del_org,Delito_gr_4_lesiones,Delito_gr_5_hom_cul,
                              Delito_gr_6_hom_dol,Delito_gr_7_armas,Delito_gr_8_viol_fam,Delito_gr_9_secuestro,Delito_gr_10_sexuales,
                              Delito_gr_11_extorsion,Delito_gr_12_fraude,Delito_gr_13_amenazas,Delito_gr_14_otro,Delito_gr_15_ns_nr), 
                     names_to = "Tipo_de_delito", values_to = "Detenidos") %>%
        mutate(Detenidos = Detenidos*as.numeric(FAC_PER),
               Delito = case_when(Tipo_de_delito== "Delito_gr_6_hom_dol" ~ "Homicidio doloso, ENPOL",
                                  T ~ "Otro")) %>%
        group_by(Anio_arresto,Delito) %>%
        summarize(Detenidos = sum(Detenidos)) %>%
        mutate(value2plot = Detenidos ,
               labels = paste0(round(value2plot,0)),
               group_var =  "Detenidos_ENPOL") %>%
        filter(Delito!="Otro") %>%
        rename(Anio = Anio_arresto)
      
      
      data2plot <- bind_rows(snsp_2,enpol_3) %>%
        ungroup() %>%
        select(Anio, value2plot, labels, group_var) %>%
        pivot_wider(names_from = "group_var" , id_cols = "Anio", values_from = c("value2plot","labels")) %>% 
        mutate(value2plot_Brecha = value2plot_Homicidios_SNSP - value2plot_Detenidos_ENPOL,
               labels_Brecha = paste0(round(value2plot_Brecha,0)))  %>%
        pivot_longer(cols = c("value2plot_Homicidios_SNSP", "value2plot_Detenidos_ENPOL", "labels_Homicidios_SNSP", "labels_Detenidos_ENPOL", "labels_Brecha"), 
                     names_to = c(".value", "group_var"), names_sep = "_") %>%
        mutate(value2plot_Brecha= case_when(group_var=="Brecha" ~ value2plot_Brecha,
                                            T~ NA)) %>% 
        filter(group_var != "Brecha")
      
      data2plot1 <- data2plot %>%
        filter(group_var == "Detenidos")
      
      data2plot2 <- data2plot %>%
        filter(group_var == "Homicidios")

      colors4plot <- twoColors
      
      plt <- ggplot(data2plot, 
                    aes(x     = Anio,
                        y     = value2plot,
                        label = labels,
                        group = group_var,
                        color = group_var)) +
        geom_point(data = data2plot1, 
                   aes(x     = Anio,
                       y     = value2plot,
                       label = labels,
                       group = group_var,
                       color = group_var),
                   size = 2,
                   show.legend = F) +
        geom_line(data = data2plot1, 
                  aes(x     = Anio,
                      y     = value2plot,
                      label = labels,
                      group = group_var,
                      color = group_var),
                  size  = 1,
                  show.legend = F) +
        geom_col(data = data2plot2, 
                    aes(x     = Anio,
                        y     = value2plot,
                        label = labels,
                        group = group_var,
                        fill = group_var,
                        color = group_var),
                 show.legend = F) +
        geom_text_repel(
          size        = 3.514598,
          show.legend = F,
          min.segment.length = 1000,
          seed               = 50,
          box.padding        = 0.9,
          point.padding      = 0.5,
          direction          = "y",
          force              = 9,
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
              axis.line.x        = element_line(color = "#d1cfd1"),
              axis.ticks.x       = element_line(color = "#d1cfd1", linetype = "solid"),
              axis.text.y        = element_blank(),  
              axis.ticks.y       = element_blank(),  
              axis.line.y        = element_blank())
      plt
      
      
      ggsave(plot   = plt,
             file   = paste0(
               path2SP,
               "/National/Visualization",
               "/Output/Politica criminal/",
               savePath,"/Delitos victimas",
               "/Figure1_4.svg"), 
             width  = 189.7883, 
             height = 85,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      return(data2plot)

}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. Robo de vehuculos ENPOl SESNSP                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

robo_vehiculos_ENPOL_ENVIPE.fn <- function(
    
){
  
  snsp <- read.csv(paste0(path2SP,"/National/Exploration/Input/IDEFC_NM_abr24.csv"),check.names = F)
  
  inegi <- read_xlsx(paste0(path2SP,"/National/Exploration/Input/INEGI_homicidios.xlsx"))
  
  
  snsp_2<- snsp %>%
    mutate(Agosto = case_when(Anio==2021 ~ 0,     T ~ Agosto),
           Septiembre = case_when(Anio==2021 ~ 0, T ~ Septiembre),
           Octubre = case_when(Anio==2021 ~ 0,    T ~ Octubre),
           Noviembre = case_when(Anio==2021 ~ 0,  T ~ Noviembre),
           Diciembre = case_when(Anio==2021 ~ 0,  T ~ Diciembre)) %>%
    filter(Anio>=2018 & Anio<= 2021) %>%
    mutate(Incidencia = Enero+Febrero+Marzo+Abril+Mayo+Junio+Julio+Agosto+Septiembre+Octubre+Noviembre+Diciembre) %>%
    mutate(Delito=case_when(`Subtipo de delito`== "Robo de veh\xedculo automotor" ~ "Robo de vehiculo, SESNSP",
                            T ~ "Otro")) %>% 
    group_by(Delito,Anio) %>%
    summarize(Incidencia = sum(Incidencia)) %>%
    mutate(Anio=as.character(Anio),
           value2plot =  Incidencia,
           labels = paste0(round(value2plot,0)),
           group_var =  "Robo_de_vehiculo_SESNSP") %>%
    filter(Delito!="Otro")
  
  
  enpol_3 <- Main_database %>% 
    filter(Anio_arresto>=2018 & Anio_arresto<= 2021 )  %>%
    select(Anio_arresto, Del_Robo_vehiculo, Del_Robo_casa_hab, Del_Robo_negocio, Del_Robo_transporte_pub, Del_Robo_transeunte,
           Del_Robo_autopartes, Del_Robo_otros, Del_Posesion_drogas, Del_Comercio_drogas, Del_Lesiones, Del_Hom_culposo,
           Del_Hom_doloso, Del_Portacion_armas, Del_Incum_asis_fam, Del_Violencia_fam, Del_Danio_prop, Del_Secuestro,
           Del_Violacion_sexual, Del_Fraude, Del_Delincuencia_org, Del_Otros_sexuales, Del_Extorsion, Del_Privacion_de_libertad,
           Del_Abuso_de_conf, Del_Amenazas, Del_Otros, Del_No_sabe, Del_No_responde,FAC_PER)  %>% 
    mutate(across(c( Del_Robo_vehiculo, Del_Robo_casa_hab, Del_Robo_negocio, Del_Robo_transporte_pub, Del_Robo_transeunte,
                     Del_Robo_autopartes, Del_Robo_otros, Del_Posesion_drogas, Del_Comercio_drogas, Del_Lesiones, Del_Hom_culposo,
                     Del_Hom_doloso, Del_Portacion_armas, Del_Incum_asis_fam, Del_Violencia_fam, Del_Danio_prop, Del_Secuestro,
                     Del_Violacion_sexual, Del_Fraude, Del_Delincuencia_org, Del_Otros_sexuales, Del_Extorsion, Del_Privacion_de_libertad,
                     Del_Abuso_de_conf, Del_Amenazas, Del_Otros, Del_No_sabe, Del_No_responde), ~as.numeric(.))) %>%
    pivot_longer(cols = c( Del_Robo_vehiculo, Del_Robo_casa_hab, Del_Robo_negocio, Del_Robo_transporte_pub, Del_Robo_transeunte,
                           Del_Robo_autopartes, Del_Robo_otros, Del_Posesion_drogas, Del_Comercio_drogas, Del_Lesiones, Del_Hom_culposo,
                           Del_Hom_doloso, Del_Portacion_armas, Del_Incum_asis_fam, Del_Violencia_fam, Del_Danio_prop, Del_Secuestro,
                           Del_Violacion_sexual, Del_Fraude, Del_Delincuencia_org, Del_Otros_sexuales, Del_Extorsion, Del_Privacion_de_libertad,
                           Del_Abuso_de_conf, Del_Amenazas, Del_Otros, Del_No_sabe, Del_No_responde), 
                 names_to = "Tipo_de_delito", values_to = "Detenidos") %>%
    mutate(Detenidos = Detenidos*as.numeric(FAC_PER),
           Delito = case_when(Tipo_de_delito == "Del_Robo_vehiculo" ~ "Robo de vehiculo, ENPOL",
                              T ~ "Otro")) %>%
    group_by(Anio_arresto,Delito) %>%
    summarize(Detenidos = sum(Detenidos, na.rm = T)) %>%
    mutate(value2plot = Detenidos ,
           labels = paste0(round(value2plot,0)),
           group_var =  "Detenidos_ENPOL") %>%
    filter(Delito!="Otro") %>%
    rename(Anio = Anio_arresto)
  
  
  data2plot <- bind_rows(snsp_2,enpol_3) %>%
    ungroup() %>%
    select(Anio, value2plot, labels, group_var) %>%
    pivot_wider(names_from = "group_var" , id_cols = "Anio", values_from = c("value2plot","labels"))%>% 
    mutate(value2plot_Brecha = value2plot_Robo_de_vehiculo_SESNSP - value2plot_Detenidos_ENPOL,
           labels_Brecha = paste0(round(value2plot_Brecha,0))) %>%
    pivot_longer(cols = c("value2plot_Robo_de_vehiculo_SESNSP", "value2plot_Detenidos_ENPOL", "labels_Robo_de_vehiculo_SESNSP", "labels_Detenidos_ENPOL", "labels_Brecha"), 
                 names_to = c(".value", "group_var"), names_sep = "_") %>%
    mutate(value2plot_Brecha = case_when(group_var=="Brecha" ~ value2plot_Brecha,
                                        T~ NA)) %>% 
    filter(group_var != "Brecha")
  
  data2plot1 <- data2plot %>%
    filter(group_var == "Detenidos")
  
  data2plot2 <- data2plot %>%
    filter(group_var == "Robo")
  
  colors4plot <- twoColors
  
  plt <- ggplot(data2plot, 
                aes(x     = Anio,
                    y     = value2plot,
                    label = labels,
                    group = group_var,
                    color = group_var)) +
    geom_point(data = data2plot1, 
               aes(x     = Anio,
                   y     = value2plot,
                   label = labels,
                   group = group_var,
                   color = group_var),
               size = 2,
               show.legend = F) +
    geom_line(data = data2plot1, 
              aes(x     = Anio,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = group_var),
              size  = 1,
              show.legend = F) +
    geom_col(data = data2plot2, 
             aes(x     = Anio,
                 y     = value2plot,
                 label = labels,
                 group = group_var,
                 fill = group_var,
                 color = group_var),
             show.legend = F) +
    geom_text_repel(
      size        = 3.514598,
      show.legend = F,
      min.segment.length = 1000,
      seed               = 50,
      box.padding        = 0.9,
      point.padding      = 0.5,
      direction          = "y",
      force              = 9,
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
          axis.line.x        = element_line(color = "#d1cfd1"),
          axis.ticks.x       = element_line(color = "#d1cfd1", linetype = "solid"),
          axis.text.y        = element_blank(),  
          axis.ticks.y       = element_blank(),  
          axis.line.y        = element_blank())
  plt
  
  
  ggsave(plot   = plt,
         file   = paste0(
           path2SP,
           "/National/Visualization",
           "/Output/Politica criminal/",
           savePath,"/Delitos victimas",
           "/Figure1_4.svg"), 
         width  = 189.7883, 
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}