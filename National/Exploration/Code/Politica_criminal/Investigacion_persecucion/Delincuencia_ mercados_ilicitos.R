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
## Outline:     Delincuencia y mercados ilícitos                                                                                        ----
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


# Delitos vinculados a mercados ilícitos ----------------------------------

data_subset.df <- Main_database %>% 
                  filter(Anio_arresto >= 2008,
                  NSJP == 1) %>% 
                  mutate(mercado_ilicito = case_when(Robo_autopartes == "1" ~ "Delito vinculado a mercado ilícito",
                                                     Robo_vehiculo   == "1" ~ "Delito vinculado a mercado ilícito",
                                                     Comercio_drogas == "1" ~ "Delito vinculado a mercado ilícito",
                                                     Del_No_responde == "1" ~ "No vinculado",
                                                     Del_No_responde == "1" ~ "No vinculado", 
                                                     T ~ "No vinculado"))

data2plot <- data_subset.df %>% 
  select(mercado_ilicito) %>% 
  group_by(mercado_ilicito) %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  rename(values = mercado_ilicito) %>% 
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
  scale_fill_manual(values =  c("#fa4d57","#9c94ff"))+
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



# Mercado ilícito por terminación del proceso -----------------------------

data_subset.df <- Main_database %>% 
  mutate(mercado_ilicito = case_when(Robo_autopartes == "1" ~ "Delito vinculado a mercado ilícito",
                                     Robo_vehiculo   == "1" ~ "Delito vinculado a mercado ilícito",
                                     Comercio_drogas == "1" ~ "Delito vinculado a mercado ilícito",
                                     Del_No_responde == "1" ~ "No vinculado",
                                     Del_No_responde == "1" ~ "No vinculado", 
                                                          T ~ "No vinculado"),
         juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                      P5_6 == "2" ~ "Procedimiento abreviado o juicio sumario", 
                                      T ~ NA_character_)) %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)



data2plot <- data_subset.df %>%
  select(mercado_ilicito, juicio_abreviado) %>% 
  group_by(mercado_ilicito, juicio_abreviado) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(mercado_ilicito) %>% 
  mutate(values = mercado_ilicito,
         value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(mercado_ilicito, width = 20))


colors4plot <- c("Juicio" = "#fa4d57",
                 "Procedimiento abreviado o juicio sumario" = "#3273ff")


plot <- ggplot(data2plot,
               aes(
                 x     = values, 
                 y     = value2plot,
                 fill  = juicio_abreviado,
                 label = paste0(figure,",",  "\n","N =" ,Frequency)
               )) +
  geom_bar(stat = "identity", width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "left") +
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
