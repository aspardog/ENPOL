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

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)

# Estudio a profundidad FGR -----------------------------------------------



# Tipos de detención por Policía Federal Ministerial ----------------------


Main_database_2008_FGR <- Main_database_2008 %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, 
         Corporacion_grupos == "Policía Federal Ministerial") %>% 
  mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "flagrancia",
                                    orden_det   == 1 ~ "orden de detención",
                                    inspeccion  == 1 ~ "inspeccion",
                                    det_ninguna == 1 ~ "ninguna de las enteriores",
                                    T ~ NA_character_))



data2plot <- Main_database_2008_FGR %>%
  select(tipo_detencion) %>% 
  group_by(tipo_detencion) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  mutate(values = tipo_detencion,
         value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(tipo_detencion, width = 20),
         order_var = case_when(figure == "37%" ~ 4, 
                               figure == "35%" ~ 3,
                               figure == "17%" ~ 2,
                               figure == "11%" ~ 1,
                               T ~ NA_real_))


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, order_var),
                  y     = value2plot,
                  label = paste0(figure, "\n", "N =", Frequency),
                  fill  = values)) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9)+
  scale_fill_manual(values = rep("#3273ff", 4)) +
  geom_text(aes(y    = value2plot + 5),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents",
       title = "Tipo de detenciones Policía Federal Ministerial") +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%"),
                     position = "left") +
  scale_x_discrete(limits = rev) +
  # coord_flip() +
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10),
        plot.title = element_text(face = "bold", size = 12))



# Tipos de detención por Policía Federal Ministeriala nivel entidad ----------------------

Main_database_2008_FGR <- Main_database_2008 %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, 
         Corporacion_grupos == "Policía Federal Ministerial") %>% 
  mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "flagrancia",
                                    orden_det   == 1 ~ "orden de detención",
                                    inspeccion  == 1 ~ "inspección",
                                    det_ninguna == 1 ~ "ninguna de las enteriores",
                                    T ~ NA_character_))

# Use group_by to group data by two columns
data2plot <- Main_database_2008_FGR %>%
  select(Estado_arresto, tipo_detencion) %>% 
  group_by(Estado_arresto, tipo_detencion) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Estado_arresto) %>%  # Group again by the first column to calculate percentages within each group of column1
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
  ungroup() %>%  # Remove grouping
  mutate(figure = paste0(round(Percentage, 0), "%"),
         labels = str_wrap(paste(Estado_arresto, tipo_detencion, sep=" - "), width = 20),
         Estado_arresto = str_wrap(Estado_arresto, width = 20)) %>% # Create labels combining both columns
  filter(complete.cases(.)) %>% 
  select(Estado_arresto, tipo_detencion, figure, Frequency) %>% 
  pivot_wider(
    names_from = tipo_detencion,
    values_from = c(figure, Frequency),
    names_sep = "_"
  )


write.xlsx(data2plot, paste0(path2SP,"/National/Exploration/Output/Politica_criminal/FGR_tipodetencion_estado.xlsx") )



colors4plot <- c("ninguna de las enteriores" = "#003B88", 
                 "orden de detención"        = "#fa4d57",
                 "flagrancia"                = "#12006b", 
                 "inspección"                = "#3273ff")

plot <- ggplot(data2plot,
               aes(
                 x     = Estado_arresto, 
                 y     = Percentage,
                 fill  = tipo_detencion,
                 label = paste0(figure, ", N =", Frequency)
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = Percentage + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  # geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "right") +
  coord_flip() +
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
