
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
## Creation date:     Mayo 20th, 2024
##
## This version:      Mayo  20th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:     Detonadores de la investigación: Detención                                                                                            ----
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



# Tipos de detenciones y cambios en el tiempo -----------------------------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2011,
         NSJP == 1) %>% 
  mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                    orden_det   == 1 ~ "Orden de detención",
                                    inspeccion  == 1 ~ "Inspeccion",
                                    det_ninguna == 1 ~ "Irregulares",
                                    T ~ NA_character_))

data2table <- data_subset.df %>%
  select(Anio_arresto, tipo_detencion) %>% 
  group_by(Anio_arresto, tipo_detencion) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Anio_arresto) %>% 
  mutate(values = Anio_arresto,
         value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = if_else(
           Anio_arresto %in% c("2011", "2013", "2015", "2017", "2019", "2021"),
           paste0(round(value2plot, 0), "%"), NA_character_),
         group_var = tipo_detencion)
  
  

colors4plot <- c("#20204a" ,"#b1a6ff", "#ff003d","#2e2e95" )

# Creating ggplot
plt <- ggplot(data2table, 
              aes(x     = Anio_arresto,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = group_var)) +
  geom_point(size = 2,
             show.legend = F
             ) +
  geom_line(size  = 1,
            show.legend = F
            ) +
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
                     labels = paste0(seq(0,100,20), "%"))+ #%>%
  scale_color_manual(values = colors4plot) +
  WJP_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"),
        legend.position      = "bottom"
  )


plt




# Tipo de detención por estado --------------------------------------------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                    orden_det   == 1 ~ "Orden de detención",
                                    inspeccion  == 1 ~ "Inspeccion",
                                    det_ninguna == 1 ~ "Irregulares",
                                    T ~ NA_character_))


data2plot <- data_subset.df %>%
  select(Estado_arresto, tipo_detencion) %>% 
  group_by(Estado_arresto, tipo_detencion) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Estado_arresto) %>% 
  rename( values = Estado_arresto ) %>%
  arrange(desc(values)) %>%
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(values, width = 20),
         tipo_detencion = case_when(tipo_detencion == "Flagrancia" ~ "D)Flagrancia", 
                                    tipo_detencion == "Inspeccion" ~ "A)Inspeccion",
                                    tipo_detencion == "Irregulares" ~ "C)Irregulares",
                                    tipo_detencion == "Orden de detención" ~ "B)Orden de detención",
                                    T ~ NA_character_)) %>%
  ungroup() %>% 
  group_by(tipo_detencion) %>%
  mutate(order = row_number()) 



plot <- ggplot(data2plot,
               aes(
                 x     = reorder(values, order), 
                 y     = value2plot,
                 fill  = tipo_detencion,
                 label = paste0(figure)
               )) +
  geom_bar(stat = "identity", width = 0.9, position = "stack")+
  geom_text(aes(y    = value2plot), 
            position = "stack",
            hjust = 1.5, 
            vjust = 0.5,
            color    = "white",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  scale_fill_manual(values =  c("#b1a6ff", "#2e2e95","#ff003d", "#20204a")) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "left") +
  coord_flip()+
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
   legend.position      = "bottom",
   legend.title = element_blank())

plot



# 1.3.4.	Tipos de detención por tiempo del proceso, solo para los de juicio --------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1,
         P5_6 == 1) %>% 
  mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                    orden_det   == 1 ~ "Orden de detención",
                                    inspeccion  == 1 ~ "Inspeccion",
                                    det_ninguna == 1 ~ "Irregulares",
                                    T ~ NA_character_),
         tiempo_dictar_sentencia = case_when(P5_10 == 1 ~ "Hasta un mes",
                                             P5_10 == 2 ~ "Hasta un mes",
                                             P5_10 == 3 ~ "Hasta un mes",
                                             P5_10 == 4 ~ "Más de un mes hasta seis meses",
                                             P5_10 == 5 ~ "Más de seis meses hasta un año",
                                             P5_10 == 6 ~ "Más de un año hasta dos años",
                                             P5_10 == 7 ~ "Más de dos años",
                                             T ~ NA_character_))

data2plot <- data_subset.df %>%
  select(tipo_detencion, tiempo_dictar_sentencia) %>% 
  group_by(tipo_detencion, tiempo_dictar_sentencia) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(tipo_detencion) %>% 
  rename( values = tipo_detencion ) %>%
  arrange(desc(values)) %>%
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(values, width = 20),
         order  = case_when(tiempo_dictar_sentencia == "Hasta un mes" ~ 1,
                            tiempo_dictar_sentencia == "Más de un mes hasta seis meses" ~ 2,
                            tiempo_dictar_sentencia == "Más de seis meses hasta un año" ~ 3,
                            tiempo_dictar_sentencia == "Más de un año hasta dos años" ~ 4,
                            tiempo_dictar_sentencia == "Más de dos años" ~ 5,
                            T ~ NA_real_))


plot <- ggplot(data2plot,
               aes(
                 x     = values, 
                 y     = value2plot,
                 fill  = reorder(tiempo_dictar_sentencia, order),
                 label = paste0(figure)
               )) +
  geom_bar(stat = "identity", width = 0.9, position = "stack")+
  geom_text(aes(y    = value2plot), 
            position = "stack",
            # hjust = 0.5, 
             vjust = 1.25,
            color    = "white",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  scale_fill_manual(values =  countryPalette) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "left") +
  # coord_flip()+
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
    legend.position      = "right",
    legend.title = element_blank())

plot
# 1.3.4.	Tipos de detención por tiempo del proceso, solo para los de procedimiento abreviado --------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) ,
         P5_6 == 2) %>% 
  mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                    orden_det   == 1 ~ "Orden de detención",
                                    inspeccion  == 1 ~ "Inspeccion",
                                    det_ninguna == 1 ~ "Irregulares",
                                    T ~ NA_character_),
         tiempo_dictar_sentencia = case_when(P5_10 == 1 ~ "Hasta un mes",
                                             P5_10 == 2 ~ "Hasta un mes",
                                             P5_10 == 3 ~ "Hasta un mes",
                                             P5_10 == 4 ~ "Más de un mes hasta seis meses",
                                             P5_10 == 5 ~ "Más de seis meses hasta un año",
                                             P5_10 == 6 ~ "Más de un año hasta dos años",
                                             P5_10 == 7 ~ "Más de dos años",
                                             T ~ NA_character_))

data2plot <- data_subset.df %>%
  select(tipo_detencion, tiempo_dictar_sentencia) %>% 
  group_by(tipo_detencion, tiempo_dictar_sentencia) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(tipo_detencion) %>% 
  rename( values = tipo_detencion ) %>%
  arrange(desc(values)) %>%
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(values, width = 20),
         order  = case_when(tiempo_dictar_sentencia == "Hasta un mes" ~ 1,
                            tiempo_dictar_sentencia == "Más de un mes hasta seis meses" ~ 2,
                            tiempo_dictar_sentencia == "Más de seis meses hasta un año" ~ 3,
                            tiempo_dictar_sentencia == "Más de un año hasta dos años" ~ 4,
                            tiempo_dictar_sentencia == "Más de dos años" ~ 5,
                            T ~ NA_real_))


plot <- ggplot(data2plot,
               aes(
                 x     = values, 
                 y     = value2plot,
                 fill  = reorder(tiempo_dictar_sentencia, order),
                 label = paste0(figure)
               )) +
  geom_bar(stat = "identity", width = 0.9, position = "stack")+
  geom_text(aes(y    = value2plot), 
            position = "stack",
            # hjust = 0.5, 
            vjust = 1.25,
            color    = "white",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  scale_fill_manual(values =  countryPalette) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "left") +
  # coord_flip()+
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
    legend.position      = "right",
    legend.title = element_blank())

plot


