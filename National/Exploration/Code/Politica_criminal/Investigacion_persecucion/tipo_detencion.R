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
## Creation date:     February 05th, 2024
##
## This version:      February 05th, 2024
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

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 


# Tipo de detención todos -------------------------------------------------------------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                    orden_det   == 1 ~ "Orden de detención",
                                    inspeccion  == 1 ~ "Inspeccion",
                                    det_ninguna == 1 ~ "Irregulares",
                                    T ~ NA_character_))




data2plot <- Main_database_2008 %>%
  select(tipo_detencion) %>% 
  group_by(tipo_detencion) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  mutate(values = tipo_detencion,
         value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(tipo_detencion, width = 20),
         order_var = -rank(values))



plot <- ggplot(data2plot,
               aes(
                 x     = reorder(values, order_var), 
                 y     = value2plot,
                 fill  = values,
                 label = paste0(figure)
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 5), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values =  c("#20204a", "#2e2e95","#756ef9", "#b1a6ff")) +
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
    axis.title.x       = element_blank())

plot


ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Exploration",
                         "/Input/figure0.png"),
       width = 250,
       height = 200,
       units  = "mm",
       dpi    = 72,
       device = "png")




# Tipo de detención delitos interesante (homicidio doloso, seceustro, drogas) -------------------------------------------------------------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, 
         Delito_unico_categ == "hom_dol" |
           Delito_unico_categ == "secuestro" |
           Delito_unico_categ == "drogas" ) %>% 
  mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                    orden_det   == 1 ~ "Orden de detención",
                                    inspeccion  == 1 ~ "Inspeccion",
                                    det_ninguna == 1 ~ "Irregulares",
                                    T ~ NA_character_))




data2plot <- Main_database_2008 %>%
  select(Delito_unico_categ, tipo_detencion) %>% 
  group_by(Delito_unico_categ, tipo_detencion) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Delito_unico_categ) %>% 
  mutate(values = Delito_unico_categ,
         value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(Delito_unico_categ, width = 20),
         order_var = case_when(labels == "drogas" ~ 1, 
                               labels == "hom_dol" ~ 3,
                               labels == "secuestro" ~ 2,
                               T ~ NA_real_),
         values = case_when(labels == "drogas" ~ "Posesión o Comercio\n de Drogas", 
                            labels == "hom_dol" ~ "Homicido Doloso",
                            labels == "secuestro" ~ "Secuestro",
                            T ~ NA_character_))



plot <- ggplot(data2plot,
               aes(
                 x     = reorder(values, order_var), 
                 y     = value2plot,
                 fill  = tipo_detencion,
                 label = paste0(figure)
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 5), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values =  c("#20204a", "#2e2e95","#756ef9", "#b1a6ff")) +
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
    axis.title.x       = element_blank())

plot


ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Exploration",
                         "/Input/figure1.png"),
       width = 200,
       height = 250,
       units  = "mm",
       dpi    = 72,
       device = "png")

