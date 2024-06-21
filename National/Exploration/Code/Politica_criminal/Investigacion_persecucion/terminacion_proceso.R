## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Política Criminal parte 3
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres            (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     April 23th, 2024
##
## This version:      April 23th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline: Señalamientos                                                                                   ----
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


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.2.5.	Tipo de conclusión del proceso en el tiempo (ENPOL 2016-2021?)                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2016,
         NSJP == 1) %>% 
  mutate(juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                      P5_6 == "2" ~ "Procedimiento abreviado o juicio sumario", 
                                      T ~ NA_character_))


data2table <- Main_database_2008 %>%
  select(Anio_arresto, juicio_abreviado) %>% 
  group_by(Anio_arresto, juicio_abreviado) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Anio_arresto) %>% 
  mutate(values = Anio_arresto,
         value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = if_else(
           Anio_arresto %in% c("2011", "2013", "2015", "2017", "2019", "2021"),
           paste0(round(value2plot, 0), "%"), NA_character_),
         group_var = juicio_abreviado)



colors4plot <- c("#20204a" ,"#b1a6ff")

# Creating ggplot
plt <- ggplot(data2table, 
              aes(x     = Anio_arresto,
                  y     = value2plot,
                  label = figure,
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
  

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.2.6.	Procedimiento abreviado () y la presión para aceptar la responsabilidad (5.6, 5.7 y 5.8 – 
## hacer el análisis de toda la cadena hasta ver por quiénes se sintieron presionados para aceptar procedimiento abreviado)                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, ) %>% 
  mutate(juicio_abreviado_presión = case_when(P5_6 == "1" ~ "Juicio", 
                                              P5_6 == "2" & P5_7 == "1" ~ "Procedimiento abreviado por presión", 
                                              P5_6 == "2" & P5_7 == "2" ~ "Procedimiento abreviado sin presión", 
                                      T ~ NA_character_))


# Supongamos que Main_database_2008 ya está cargado en tu entorno

data2plot <- Main_database_2008 %>% 
  select(juicio_abreviado_presión) %>% 
  group_by(juicio_abreviado_presión) %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  rename(values = juicio_abreviado_presión) %>% 
  mutate(
    value2plot = Frequency / sum(Frequency) * 100,
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(values, width = 20),
    ymax = cumsum(value2plot),
    ymin = c(0, head(cumsum(value2plot), -1))
  )

plot <- data2plot %>% 
  ggplot(aes(
    ymax = ymax, 
    ymin = ymin, 
    xmax = 4, 
    xmin = 3, 
    fill = values)) +
  geom_rect() +
  coord_polar(theta = "y") + 
  xlim(c(2, 4)) +
  geom_text_repel(
    aes(
      y = (ymin + ymax) / 2, 
      label = figure
    ), 
    x = 3.5,
    color = "white",
    family = "Lato Full",
    fontface = "bold", 
    size = 4.514598
  ) +
  scale_fill_manual(values = c("#cfb3ff", "#fa4d57", "#43a9a7")) +
  theme_void() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_line(size = 0.25, colour = "#5e5c5a", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3"),       
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

plot



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.2.6.	Procedimiento abreviado () y la presión para aceptar la responsabilidad (5.6, 5.7 y 5.8 – 
## hacer el análisis de toda la cadena hasta ver por quiénes se sintieron presionados para aceptar procedimiento abreviado)                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)  %>% 
  select(starts_with("P5_8"), -P5_8_8, -P5_8_9,abogado_publico) %>% 
  mutate(across(everything(), as.numeric),
         P5_8_1_publico = case_when(P5_8_1 == 1 & abogado_publico == 1  ~ 1,
                                    P5_8_1 == 1 & abogado_publico == 0 |  P5_8_1 == 0 ~ 0, T ~NA_real_),
         P5_8_1_privado = case_when(P5_8_1 == 1 & abogado_publico == 0  ~ 1,
                                    P5_8_1 == 1 & abogado_publico == 1 |  P5_8_1 == 0  ~ 0, T ~NA_real_)) %>% 
  select( -abogado_publico )


data2plot <- Main_database_2008 %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100)) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "P5_8_1"             ~ "Su abogado defensor", 
                       values == "P5_8_1_publico"     ~ "Su abogado defensor público", 
                       values == "P5_8_1_privado"     ~ "Su abogado defensor privado", 
                       values == "P5_8_2"     ~ "El Ministerio Público o Fiscal",
                       values == "P5_8_3"     ~ "El juez",
                       values == "P5_8_4"     ~ "Otro"),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 20),
    order_var = rank(value2plot))

colors4plot <- rep("#fa4d57", 6)


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = figure,
                  color = Delito)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 5 ),
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
  coord_flip()

plt


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.2.7. Duración del proceso por tipo de conclusión (contrastar PA y JO, con p.5.10)                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(juicio_abreviado = case_when(P5_6  == 1 ~ "Juicio",
                                      P5_6   == 2 ~ "Procedimeinto abreviado",
                                    T ~ NA_character_),
         tiempo_dictar_sentencia = case_when(P5_10 == 1 ~ "Más de un mes hasta seis meses",
                                             P5_10 == 2 ~ "Más de un mes hasta seis meses",
                                             P5_10 == 3 ~ "Más de un mes hasta seis meses",
                                             P5_10 == 4 ~ "Más de un mes hasta seis meses",
                                             P5_10 == 5 ~ "Más de seis meses hasta un año",
                                             P5_10 == 6 ~ "Más de un año hasta dos años",
                                             P5_10 == 7 ~ "Más de dos años",
                                             T ~ NA_character_))

data2plot <- data_subset.df %>%
  select(juicio_abreviado, tiempo_dictar_sentencia) %>% 
  group_by(juicio_abreviado, tiempo_dictar_sentencia) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(juicio_abreviado) %>% 
  rename( values = juicio_abreviado ) %>%
  arrange(desc(values)) %>%
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(values, width = 20),
         order  = case_when(
                            tiempo_dictar_sentencia == "Más de un mes hasta seis meses" ~ 1,
                            tiempo_dictar_sentencia == "Más de seis meses hasta un año" ~ 2,
                            tiempo_dictar_sentencia == "Más de un año hasta dos años" ~ 3,
                            tiempo_dictar_sentencia == "Más de dos años" ~ 4,
                            T ~ NA_real_))


plot <- ggplot(data2plot,
               aes(
                 x     = values, 
                 y     = value2plot,
                 fill  = reorder(tiempo_dictar_sentencia, -order),
                 label = paste0(figure)
               )) +
  geom_bar(stat = "identity", width = 0.9, position = "stack") +
  geom_text(aes(y    = value2plot -.1,
                label = paste0(figure)), 
            position = position_stack(vjust = 0.5),
            color    = "white",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  scale_fill_manual(values =  countryPalette) +
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
    legend.position      = "right",
    legend.title = element_blank())

plot
