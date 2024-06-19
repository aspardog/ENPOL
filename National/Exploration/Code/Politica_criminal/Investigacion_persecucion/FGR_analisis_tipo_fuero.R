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

#Tipos de detención de la Policía ministerial federal en el tiempo ------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1, 
         Corporacion_grupos == "Policía Federal Ministerial") %>% 
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
         labels = figure,
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


# 3.	Frecuencias de delitos federales sentenciados  ¿De qué delito --------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1,
         fuero == "Sólo federal",
         sentenciado == 1) %>% 
  mutate(Delito_unico_ungrouped_categ = case_when(Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ "Robo de casa habitación",
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

data2plot <- Main_database_2008 %>%
  filter(!is.na(Delito_unico_ungrouped_categ)) %>%
  group_by(Delito_unico_ungrouped_categ) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "Arrestos",
         Delito = Delito_unico_ungrouped_categ,
         Delito = str_wrap(Delito, width = 30)) %>%
  select(Delito,value2plot,labels,group_var) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito)) %>% 
  filter(value2plot >= 1)

colors4plot <- rep("#E2E2F7", 9)


plt <- ggplot(data2plot, 
              aes(x     = Delito,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = Delito)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot +.9 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  #xlab("Porcentaje de criterios cumplidos")+
  scale_y_discrete() +
  scale_x_discrete( ) +
  expand_limits(y = c(0, 30))+
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10))+
  coord_flip()

plt


# Tipos de detención por Policía Federal Ministerial y GN/Policía federal ----------------------


Main_database_2008_FGR <- Main_database_2008 %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, 
         Corporacion_grupos == "Policía Federal Ministerial" | 
         Corporacion_grupos == "Guardia Nacional" |
         Corporacion_grupos == "Policía Federal" ) %>% 
  mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "flagrancia",
                                    orden_det   == 1 ~ "orden de detención",
                                    inspeccion  == 1 ~ "inspeccion",
                                    det_ninguna == 1 ~ "ninguna de las enteriores",
                                    T ~ NA_character_),
         Corporacion_grupos = case_when(Corporacion_grupos == "Policía Federal Ministerial" ~ "Policía Federal Ministerial",
                                      Corporacion_grupos == "Guardia Nacional" ~ "GN/Polcía Federal",
                                      Corporacion_grupos == "Policía Federal" ~ "GN/Polcía Federal",
                                    T ~ NA_character_))



data2plot <- Main_database_2008_FGR %>%
  select(tipo_detencion, Corporacion_grupos) %>% 
  group_by(tipo_detencion, Corporacion_grupos) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Corporacion_grupos) %>% 
  mutate(values = tipo_detencion,
         value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(tipo_detencion, width = 20))


colors4plot <- c("GN/Polcía Federal" = "#a90099",
                 "Policía Federal Ministerial" = "#2a2a94")


plot <- ggplot(data2plot,
               aes(
                 x     = values, 
                 y     = value2plot,
                 fill  = Corporacion_grupos,
                 label = paste0(figure,",",  "\n","N =" ,Frequency)
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
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
    axis.title.x       = element_blank())

plot


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



# Tipos de conclusión del proceso por delitos federales -------------------


Main_database_2008_federales <- Main_database_2008 %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, 
         fuero == "Sólo federal" ) %>% 
  mutate(juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                      P5_6 == "2" ~ "Procedimiento abreviado o juicio sumario", 
                                               T ~ NA_character_))



data2plot <- Main_database_2008_federales %>%
  select(juicio_abreviado) %>% 
  group_by(juicio_abreviado) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%")) %>% 
  rename(values = juicio_abreviado)


colors4plot 

plot <- ggplot(data2plot,
               aes(
                 x     = values, 
                 y     = value2plot,
                 fill  = values,
                 label = paste0(figure, ", N =", Frequency)
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = rep("#3273ff",2)) +
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

# Tipos de conclusión del proceso por delitos ambos fueros comparación -------------------


Main_database_2008_federales <- Main_database_2008 %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                      P5_6 == "2" ~ "Procedimiento abreviado o juicio sumario", 
                                      T ~ NA_character_))



data2plot <- Main_database_2008_federales %>%
  select(juicio_abreviado) %>% 
  group_by(juicio_abreviado) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%")) %>% 
  rename(values = juicio_abreviado)


colors4plot 

plot <- ggplot(data2plot,
               aes(
                 x     = values, 
                 y     = value2plot,
                 fill  = values,
                 label = paste0(figure, ", N =", Frequency)
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = rep("#2a2a9A",2)) +
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

# Tipos de conclusión del proceso por delitos locales -------------------


Main_database_2008_federales <- Main_database_2008 %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, 
         fuero == "Sólo común") %>% 
  mutate(juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                      P5_6 == "2" ~ "Procedimiento abreviado o juicio sumario", 
                                      T ~ NA_character_))



data2plot <- Main_database_2008_federales %>%
  select(juicio_abreviado) %>% 
  group_by(juicio_abreviado) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%")) %>% 
  rename(values = juicio_abreviado)



plot <- ggplot(data2plot,
               aes(
                 x     = values, 
                 y     = value2plot,
                 fill  = values,
                 label = paste0(figure, ", N =", Frequency)
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = rep("#20204a",2)) +
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


# Tipos de conclusión del proceso por delitos ambos fueros comparación (barras juntas) -------------------


Main_database_2008 <- Main_database_2008 %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1,
         fuero != "Algunos delitos de fuero común y algunos de fuero federal") %>% 
  mutate(juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                      P5_6 == "2" ~ "Procedimiento abreviado o juicio sumario", 
                                      T ~ NA_character_))



data2plot <- Main_database_2008 %>%
  select(juicio_abreviado, fuero) %>% 
  group_by(juicio_abreviado, fuero) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(juicio_abreviado) %>% 
  mutate(values = juicio_abreviado,
         value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(juicio_abreviado, width = 20))


colors4plot <- c("Sólo común" = "#fa4d57",
                 "Sólo federal" = "#3273ff")


plot <- ggplot(data2plot,
               aes(
                 x     = values, 
                 y     = value2plot,
                 fill  = fuero,
                 label = paste0(figure,",",  "\n","N =" ,Frequency)
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
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
    axis.title.x       = element_blank())

plot



# Logit de probabilidad de conclusión por procedimiento abreviado  --------
# Loading plotting functions from GitHub
source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/loading.R")
loadVIZ(set = "ENPOL")

logit_dataBase.fn <- function(data = Main_database_2008_federales,
                              selectables = c("Sexo", 
                                              "Educacion_superior", 
                                              "Color_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30", 
                                              "vulnerabilidad_economica",
                                              "discapacidad"),
                              dependent_var
) {
  
  master_data.df <- data %>%
    filter(Anio_arresto >= 2008) %>%
    filter(NSJP == 1) %>%
    filter(Delito_unico == 1) %>%
    mutate(
      Educacion_superior = 
        case_when(
          Educacion_superior == 1 ~ "Cuenta con título de educación universitaria",
          Educacion_superior == 0 ~ "No cuenta con título de educación universitario",
          T ~ NA_character_
        ),
      Color_piel_claro       =
        case_when(
          Color_piel_claro      == 1 ~ "Color de piel claro",
          Color_piel_claro      == 0 ~ "Color de piel oscuro",
          T ~ NA_character_
        ),
      LGBTQ                 = 
        case_when(
          LGBTQ                 == 1 ~ "Pertenece a la comunidad LGBTQ",
          LGBTQ                 == 0 ~ "No pertenece a la comunidad LGBTQ",
          T ~ NA_character_
        ),
      Etnia                 =
        case_when(
          Etnia                 == 1 ~ "Afromexicano o indígena",
          Etnia                 == 0 ~ "No se identifica con ninguna etnia",
          T ~ NA_character_
        ),
      Edad_menor30          =
        case_when(
          Edad_menor30          == 1 ~ "Menor a 30 años",
          Edad_menor30          == 0 ~ "Mayor o igual a 30 años",
          T ~ NA_character_
        ),
      vulnerabilidad_economica  =
        case_when(
          vulnerabilidad_economica == 1 ~ "Vulnerable economicamente",
          vulnerabilidad_economica == 0 ~ "No vulnerable economicamente",
          T ~ NA_character_
        ),
      discapacidad      =
        case_when(
          discapacidad == 1 ~ "Reporta algún tipo de discapacidad",
          discapacidad == 0 ~ "No presenta discapacidad",
          T ~ NA_character_
        )
    )
  
  selectables <- c(selectables)
  
  logit_data <- master_data.df %>%
    select(all_of(selectables),
           all_of(dependent_var),
           Delito_unico_categ, 
           Estado_arresto) %>%
    mutate(
      Educacion_superior =
        if_else(
          Educacion_superior %in% "Cuenta con título de educación universitaria",
          "ZCuenta con título de educación universitaria", Educacion_superior
        ),
      Sexo                  =
        if_else(
          Sexo %in% "Femenino",
          "ZFemenino", Sexo
        ),
      Color_piel_claro       =
        if_else(
          Color_piel_claro %in% "Color de piel claro",
          "ZColor de piel claro", Color_piel_claro
        ),
      LGBTQ                 =
        if_else(
          LGBTQ %in% "Pertenece a la comunidad LGBTQ",
          "ZPertenece a la comunidad LGBTQ", LGBTQ
        ),
      Etnia                 =
        if_else(
          Etnia %in% "Afromexicano o indígena",
          "ZAfromexicano o indígena", Etnia
        ),
      Edad_menor30          =
        if_else(
          Edad_menor30 %in% "Menor a 30 años",
          "ZMenor a 30 años", Edad_menor30
        ),
      vulnerabilidad_economica          =
        if_else(
          vulnerabilidad_economica %in% "Vulnerable economicamente",
          "ZVulnerable economicamente", vulnerabilidad_economica
        ),
      discapacidad          =
        if_else(
          discapacidad %in% "Reporta algún tipo de discapacidad",
          "ZReporta algún tipo de discapacidad", discapacidad
        ),
    ) %>%
    arrange(Sexo, Educacion_superior, Color_piel_claro, LGBTQ, Etnia, Edad_menor30, 
            Delito_unico_categ, Estado_arresto, vulnerabilidad_economica, discapacidad)
  
  formula <- selectables %>%
    t() %>%
    as.data.frame() %>%
    unite(., formula, sep = "+") %>%
    as.character()
  
  depVar <- dependent_var
  
  formula  <- as.formula(paste(depVar, "~", 
                               formula, 
                               "+factor(Delito_unico_categ)+factor(Estado_arresto)")
  )
  logit    <- glm(formula,
                  data   = logit_data, 
                  family = "binomial")
  
  summaryLogit <- bind_rows(
    as.data.frame(coef(logit))
  )
  
  marg_effects <- margins(logit, variables = selectables, atmeans = TRUE)
  
  # Calculate robust variance-covariance matrix
  robust_vcov <- vcovHC(logit, type = "HC1", cluster = "group", group = logit_data$Estado_arresto)
  
  
  margEff      <- as.data.frame(
    summary(marg_effects, vcov = robust_vcov)
  ) %>%
    filter(factor %in% c("SexoZFemenino", "LGBTQZPertenece a la comunidad LGBTQ", 
                         "Educacion_superiorZCuenta con título de educación universitaria", 
                         "Color_piel_claroZColor de piel claro", "EtniaZAfromexicano o indígena",
                         "Edad_menor30ZMenor a 30 años", "vulnerabilidad_economicaZVulnerable economicamente", 
                         "discapacidadZReporta algún tipo de discapacidad"))
  
  margEff$factor <-recode(margEff$factor,
                          "SexoZFemenino"                                            = "Mujer",
                          "LGBTQZPertenece a la comunidad LGBTQ"                     = "Perteneciente a \ncomunidad LGBTQ",
                          "EtniaZAfromexicano o indígena"                            = "Afromexicano/a o \nindígena",
                          "Educacion_superiorZCuenta con título de educación universitaria"  = "Con educación \nuniveristaria",
                          "Edad_menor30ZMenor a 30 años"                             = "Menor a 30 años",
                          "Color_piel_claroZColor de piel claro"                     = "Color de piel \nclaro",
                          "vulnerabilidad_economicaZVulnerable economicamente"       = "Vulnerable \neconómicamente",
                          "discapacidadZReporta algún tipo de discapacidad"          = "Persona con \ndiscapacidad"
  )
  
  data2table <- margEff %>%
    mutate(order_variable =
             case_when(
               factor == "Mujer"                                  ~ 1,
               factor == "Perteneciente a \ncomunidad LGBTQ"      ~ 2,
               factor == "Menor a 30 años"                        ~ 3,
               factor == "Con educación \nuniveristaria"          ~ 4,
               factor == "Afromexicano/a o \nindígena"            ~ 5,
               factor == "Color de piel \nclaro"                  ~ 6,
               factor == "Vulnerable \neconómicamente"            ~ 7,
               factor == "Persona con \ndiscapacidad"             ~ 8,
             ),
           dependent_var  =
             dependent_var
    )
  
  return(data2table)
  
}


## Arreglo base

Main_database_2008_federales <- Main_database_2008 %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(procedimiento_abreviado= case_when(P5_6 == "1" ~ 0, 
                            P5_6 == "2" ~ 1, 
                                      T ~ NA_real_))

# # Applying plotting function



data2plot <- logit_dataBase.fn(dependent_var = "procedimiento_abreviado")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = F)
logitPlot

