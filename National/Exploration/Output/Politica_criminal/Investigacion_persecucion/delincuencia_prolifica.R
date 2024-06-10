## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration data & hypothesis
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres 
##
## Dependencies:      World Justice Project
##
## Creation date:     June 05th, 2024
##
## This version:      June 05th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline: Delincuencia profilifica y emergente                                                                                              ----
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



# : Proporción de Reincidentes vs No reincidentes -------------------------------------------------------------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(reincidencia = case_when(P9_1 == "1" ~ "reincidentes",
                                  P9_1 == "2" ~ "No reincidentes",
                                  T ~ NA_character_))

data2plot <- Main_database_2008 %>% 
            select(reincidencia) %>% 
            group_by(reincidencia) %>% 
            summarise(Frequency = n(), .groups = 'drop') %>% 
            drop_na() %>% 
            rename(values = reincidencia) %>% 
            mutate(
              value2plot = Frequency / sum(Frequency) * 100,
              figure = paste0(round(value2plot, 0), "%"),
              labels = str_wrap(values, width = 20),
              ymin = c(0, head(value2plot, -1)))
            
  
plot <- data2plot %>% 
        ggplot(aes(
          ymax=value2plot, 
          ymin=ymin, 
          xmax=4, 
          xmin=3, 
          fill=values)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  geom_text( x= 3.5,
            aes(y    = value2plot-4 , 
                label = figure), 
            # position = "stack",
            color    = "white",
            family   = "Lato Full",
            fontface = "bold", 
            size = 4.514598) +
  scale_fill_manual(values =  c("#43a9a7","#9c94ff"))+
  theme_void() +
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
    legend.title = element_blank()); plot

# : Proporción de Reincidentes vs No reincidentes -------------------------------------------------------------




# Análisis: Proporción de personas que tienen más de un proceso  --------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(multi_proceso = case_when(as.numeric(P5_28) >= 2 &  as.numeric(P5_28) <= 90 ~ "Más de un proceso abierto",
                                   as.numeric(P5_28) <= 1 ~ "Sólo un proceso abierto",
                                   T ~ NA_character_))

data2plot <- Main_database_2008 %>% 
  select(multi_proceso) %>% 
  group_by(multi_proceso) %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  rename(values = multi_proceso) %>% 
  mutate(
    value2plot = Frequency / sum(Frequency) * 100,
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(values, width = 20),
    ymin = c(0, head(value2plot, -1)))


plot <- data2plot %>% 
  ggplot(aes(
    ymax=value2plot, 
    ymin=ymin, 
    xmax=4, 
    xmin=3, 
    fill=values)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  geom_text( x= 3.5,
             aes(y    = value2plot-4 , 
                 label = figure), 
             # position = "stack",
             color    = "white",
             family   = "Lato Full",
             fontface = "bold", 
             size = 4.514598) +
  scale_fill_manual(values =  c("#a90099","#cfb3ff"))+
  theme_void() +
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
    legend.title = element_blank()); plot



# Proporción de personas que fueron sentenciadas por más de un delito --------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(Delito_unico = case_when( Delito_unico == 1 ~ "Sólo un delito",
                                   Delito_unico == 0 ~ "Más de un delito",
                                   T ~ NA_character_))

data2plot <- Main_database_2008 %>% 
  select(Delito_unico) %>% 
  group_by(Delito_unico) %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  rename(values = Delito_unico) %>% 
  mutate(
    value2plot = Frequency / sum(Frequency) * 100,
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(values, width = 20),
    ymin = c(0, head(value2plot, -1)))


plot <- data2plot %>% 
  ggplot(aes(
    ymax=value2plot, 
    ymin=ymin, 
    xmax=4, 
    xmin=3, 
    fill=values)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  geom_text( x= 3.5,
             aes(y    = value2plot-4 , 
                 label = figure), 
             # position = "stack",
             color    = "white",
             family   = "Lato Full",
             fontface = "bold", 
             size = 4.514598) +
  scale_fill_manual(values =  c("#2a2a9A","#3273ff"))+
  theme_void() +
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
    legend.title = element_blank()); plot

# -	De las personas que tienen más de un proceso, cuántas personas cometieron un delito antes  --------

subset.df <- Main_database %>% 
  mutate(multi_proceso = case_when(as.numeric(P5_28) >= 2 & 
                                   as.numeric(P5_28) <= 90 ~ "Más de un proceso abierto",
                                   as.numeric(P5_28) <= 1  ~ "Sólo un proceso abierto",
                                   T ~ NA_character_)) %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, 
         multi_proceso == "Más de un proceso abierto") %>% 
  mutate(delito_antes = case_when( P5_29_1 == "1" ~ "Antes",
                                   P5_29_1 == "0" ~ "No antes",
                                   T ~ NA_character_))

data2table <- subset.df %>% 
  select(delito_antes) %>% 
  group_by(delito_antes) %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  rename(values = delito_antes) %>% 
  mutate(
    value2plot = Frequency / sum(Frequency) * 100,
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(values, width = 20),
    ymin = c(0, head(value2plot, -1)))


plot <- data2table %>% 
  ggplot(aes(
    ymax=value2plot, 
    ymin=ymin, 
    xmax=4, 
    xmin=3, 
    fill=values)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  geom_text( x= 3.5,
             aes(y    = value2plot-4 , 
                 label = figure), 
             # position = "stack",
             color    = "white",
             family   = "Lato Full",
             fontface = "bold", 
             size = 4.514598) +
  scale_fill_manual(values =  c("#fa4d57", "#a90099"))+
  theme_void() +
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
    legend.title = element_blank());plot

