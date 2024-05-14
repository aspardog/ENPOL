
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL Hypothesis
##
## Script:            Punchline exploration
##
## Author(s):         Cristina Álvarez Venzor    (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     May 13, 2024
##
## This version:      May 13, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/Settings.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)


# ¿Qué componente se cumplió más en el índice? ----------------------------

Main_database_2008_sentenciados <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1, 
         sentenciado ==1)

indicator_components <- c("PJ_1",
                       "PJ_2",
                       "PJ_3",
                       "PJ_4",
                       "PJ_5",
                       "PJ_6",
                       "PJ_7",
                       "UAA_1",
                       "UAA_2",
                       "UAA_3",
                       "UAA_4",
                       "GDH_1",
                       "GDH_2")

labels <- c("PJ_1 El defensor te explicó cómo sería tu proceso",
            "PJ_2 El defensor te explicó los hechos por los cuales te acusa",
            "PJ_3 No incriminación por presión ante el MP",
            "PJ_4 Defensa oportuna ante el MP",
            "PJ_5 Juez No consideraba culpable antes del juicio",
            "PJ_6 Tiempo de traslado ante autoridad competente",
            "PJ_7 Juez presente en las audiencias",
            "UAA_1 Personas detenidas con uso proporcional de la autoridad",
            "UAA_2 No Corrupción ante Policía",
            "UAA_3 No Corrupción ante el MP",
            "UAA_4 No Corrupción ante el Juez",
            "GDH_1 No Tortura (Policía & MP)",
            "GDH_2 Detenciones regulares")

Main_database_2008_sentenciados <- clean_columns.fn(Main_database_2008_sentenciados, indicator_components)

data2plot <- set_data.fn(Main_database_2008_sentenciados, indicator_components, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = rep("#E2E2F7", 13),
                              title = "Cumpliento por componente")
barChart


# del criterio sobre presencia del juez en las audiencias: sacar datos sobre fecha de arresto y estado ----------------------------

data2plot <- setData.fn(data = Main_database_2008_sentenciados, Estado_arresto, PJ_7) %>% 
            mutate(group = case_when(group == 1 ~ "Juez presente",
                                     group == 0 ~ "Juez ausente",
                                     T ~ NA_character_)) %>% 
              filter(group == "Juez ausente")

colors4plot <- c("Juez presente" = "#003B88", 
                 "Juez ausente" = "#fa4d57")
  
plot <- MulipleBar.fn() + geom_hline(yintercept = mean(data2plot$Percentage), linetype = "dashed", color = "#555454")

plot + labs(title = "Ausencia del juez en audiencia por estado ")


# Año
data2plot <- setData.fn(data = Main_database_2008_sentenciados, Anio_arresto, PJ_7) %>% 
  mutate(group = case_when(group == 1 ~ "Juez presente",
                           group == 0 ~ "Juez ausente",
                           T ~ NA_character_)) %>% 
  filter(group == "Juez ausente")

colors4plot <- c("Juez presente" = "#003B88", 
                 "Juez ausente" = "#fa4d57")

MulipleBar.fn()  + geom_hline(yintercept = mean(data2plot$Percentage), linetype = "dashed", color = "#555454")

plot + labs(title = "Ausencia del juez en audiencia por año ")

# Comparar delitos de sentenciados vs procesados y sentenciados, ----------------------------


Main_database1 <- Main_database %>%
  filter(Anio_arresto >= 2011, Delito_unico == 1) %>%
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
  filter(Anio_arresto >= 2018, !is.na(Delito_unico_ungrouped_categ)) %>%
  group_by(Delito_unico_ungrouped_categ) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "Arrestos",
         Delito = Delito_unico_ungrouped_categ,
         Delito = str_wrap(Delito, width = 30)) %>%
  select(Delito,value2plot,labels,group_var) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito))

colors4plot <- c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7")


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
  expand_limits(y = c(0, 25))+
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10))+
  coord_flip()

plt + labs(title = "Sentenciado y procesados - ENPOL")



# Comparación con sólo sentenciados ---------------------------------------


Main_database1 <- Main_database1 %>% 
                  filter(sentenciado == 1)

data2plot <- Main_database1 %>%
  filter(Anio_arresto >= 2018, !is.na(Delito_unico_ungrouped_categ)) %>%
  group_by(Delito_unico_ungrouped_categ) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "Arrestos",
         Delito = Delito_unico_ungrouped_categ,
         Delito = str_wrap(Delito, width = 30)) %>%
  select(Delito,value2plot,labels,group_var) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito))

colors4plot <- c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7")


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
  expand_limits(y = c(0, 25))+
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10))+
  coord_flip()

plt + labs(title = "Sentenciados - ENPOL")



# Sobre delitos federales y el mixto (fed y estatal), mostrar dist --------


# AMpliación catalogo PPO -------------------------------------------------

# Con actualizaciones del catálogo 


data_subset.df <- data.frame(Value = Main_database_2008$tipo_prision_preventiva) %>% 
  filter(complete.cases(.))

# Count the frequency of each unique value
data2plot <- data_subset.df %>%
  group_by(Value) %>%
  summarise(Frequency = n()) %>% 
  mutate(Value = Value,
         values = Frequency/sum(Frequency),
         value2plot = values * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = case_when(Value == "Prisión Preventiva Justificada" ~ "Prisión Preventiva \nJustificada",
                            Value == "Prisión Preventiva Oficiosa" ~ "Prisión Preventiva \nOficiosa", 
                            Value == "Proceso en libertad" ~ "Proceso en libertad", 
                            T ~ NA_character_)) 
data2plot <- data2plot %>% mutate(order_var = rank(values)) 


colors4plot <- c("Prisión Preventiva \nOficiosa" = "#2a2a9A",
                 "Prisión Preventiva \nJustificada" = "#a90099",
                 "Proceso en libertad" ="#43a9a7")

plot <- barsChartN.fn(data.df                    = data2plot,
                     groupVar                   = F,  
                     categories_grouping_var    = labels,
                     colors4plot                = colors4plot, 
                     order                      = T,
                     orientation                = "horizontal")
plot + labs(title = "Inlcuyendo Robo a Casa y Posesión de Armas después de  abril de 2019 a PPO")


# Sin actualización de catálogo -------------------------------------------

delitos_PPO1    <- c("P5_11_08", "P5_11_09", "P5_11_12", "P5_11_17", "P5_11_18", "P5_11_20",
                     "P5_31_08", "P5_31_09", "P5_31_12", "P5_31_17", "P5_31_18", "P5_31_20")


subset <-  Main_database_2008 %>% 
  mutate(PPO = case_when(
    months_since_PPO_2 >= 0 & if_any(delitos_PPO1, ~ .x %in% "1") ~ 1,
    months_since_PPO_2 >= 0 & if_any(delitos_PPO1, ~ .x %in% "0") ~ 0,
    months_since_PPO_2 < 0 & months_since_PPO_1 >= 0 & if_any(delitos_PPO1, ~ .x %in% "1") ~ 1,
    months_since_PPO_2 < 0 & months_since_PPO_1 >= 0 & if_any(delitos_PPO1, ~ .x %in% "0") ~ 0,
    months_since_PPO_2 < 0 & months_since_PPO_1 < 0 ~ 0)) %>% 
  mutate(tipo_prision_preventiva = case_when(procesado == 0  & PPO == 1 ~ "Prisión Preventiva Oficiosa",
                                             procesado == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             proceso_no_en_libertad == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             procesado ==  1 & PPO == 0 ~ "Prisión Preventiva Justificada",
                                             proceso_no_en_libertad == 1  & PPO == 0 ~ "Prisión Preventiva Justificada",
                                             proceso_no_en_libertad == 0  ~ "Proceso en libertad",
                                             T ~ NA_character_))

data_subset.df <- data.frame(Value = subset$tipo_prision_preventiva) %>% 
  filter(complete.cases(.))

# Count the frequency of each unique value
data2plot <- data_subset.df %>%
  group_by(Value) %>%
  summarise(Frequency = n()) %>% 
  mutate(Value = Value,
         values = Frequency/sum(Frequency),
         value2plot = values * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = case_when(Value == "Prisión Preventiva Justificada" ~ "Prisión Preventiva \nJustificada",
                            Value == "Prisión Preventiva Oficiosa" ~ "Prisión Preventiva \nOficiosa", 
                            Value == "Proceso en libertad" ~ "Proceso en libertad", 
                            T ~ NA_character_)) 
data2plot <- data2plot %>% mutate(order_var = rank(values)) 


colors4plot <- c("Prisión Preventiva \nOficiosa" = "#2a2a9A",
                 "Prisión Preventiva \nJustificada" = "#a90099",
                 "Proceso en libertad" ="#43a9a7")

plot <- barsChartN.fn(data.df                    = data2plot,
                      groupVar                   = F,  
                      categories_grouping_var    = labels,
                      colors4plot                = colors4plot, 
                      order                      = T,
                      orientation                = "horizontal")

plot + labs(title = "Sin incluir Robo a Casa y Posesión de Armas después de  abril de 2019 a PPO")

