## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - RunMe File
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Abril 2, 2024
##
## This version:      Abril 2, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("Code/settings.R")

# Loading data
load(paste0(path2SP,
            "/National/Data_cleaning/Output/Main_database.RData"))


#3.1 ¿por qué está en este Centro penitenciario? -----------------------------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when( P3_1 == "1" ~ "cometió un delito",
                              P3_1 == "2" ~ "ayudó en la realización de un delito",
                              P3_1 == "3" ~ "no ha podido comprobar su inocencia.",
                              P3_1 == "4" ~ "acusaron falsamente de cometer un delito",
                              T ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 10)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"\nN=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 7 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -40, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "¿Por qué está en este Centro penitenciario?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt


# 3.2 ¿Qué autoridad lo(a) detuvo? ----------------------------------------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when( P3_2 == "01" ~ "Policía Municipal",
                             P3_2 == "02" ~ "Policía Estatal",
                             P3_2 == "03" ~ "Policía Federal",
                             P3_2 == "04" ~ "Policía Estatal Ministerial o Judicial",
                             P3_2 == "05" ~ "Policía Federal Ministerial",
                             P3_2 == "06" ~ "Guardia Nacional",
                             P3_2 == "07" ~ "Ejército",
                             P3_2 == "08" ~ "Marina",
                             P3_2 == "09" ~ "operativo conjunto",
                             P3_2 == "10" ~ "otro",
                             P3_2 == "11" ~ "otro",
                             P3_2 == "12" ~ "otro",
                             P3_2 == "13" ~ "otro",
                             P3_2 == "14" ~ "otro",
                             P3_2 == "15" ~ "otro",
                             T ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 10)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"\nN=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 7 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -40, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "¿Qué autoridad lo(a) detuvo?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt



# 3.2a ¿Cuáles eran las autoridades que integraban el operativo co --------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         Estado_arresto == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when( P3_2A_01 == "1" ~ "Policía Municipal",
                             P3_2A_02 == "1" ~ "Policía Estatal",
                             P3_2A_03 == "1" ~ "Policía Federal",
                             P3_2A_04 == "1" ~ "Policía Estatal Ministerial o Judicial",
                             P3_2A_05 == "1" ~ "Policía Federal Ministerial",
                             P3_2A_06 == "1" ~ "Guardia Nacional",
                             P3_2A_07 == "1" ~ "Ejército",
                             P3_2A_08 == "1" ~ "Marina",
                             P3_2A_98 == "1" ~ NA_character_,
                             P3_2A_99 == "1" ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 10)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"\nN=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 7 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -40, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "¿Cuáles eran las autoridades que integraban el operativo conjunto?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt



# 3.3 ¿En qué entidad federativa (estado) lo(a) detuvieron? ---------------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when( P3_2A_01 == "1" ~ "Policía Municipal",
                             P3_2A_02 == "1" ~ "Policía Estatal",
                             P3_2A_03 == "1" ~ "Policía Federal",
                             P3_2A_04 == "1" ~ "Policía Estatal Ministerial o Judicial",
                             P3_2A_05 == "1" ~ "Policía Federal Ministerial",
                             P3_2A_06 == "1" ~ "Guardia Nacional",
                             P3_2A_07 == "1" ~ "Ejército",
                             P3_2A_08 == "1" ~ "Marina",
                             P3_2A_98 == "1" ~ NA_character_,
                             P3_2A_99 == "1" ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 10)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"\nN=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 7 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -40, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "¿Cuáles eran las autoridades que integraban el operativo conjunto?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt


# 3.3 ¿En qué entidad federativa (estado) lo(a) detuvieron? ---------------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = P3_3) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 13)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"\nN=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 7 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -40, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "¿Cuáles eran las autoridades que integraban el operativo conjunto?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt

# 3.5 ¿En qué fecha lo(a) detuvieron? -----------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = Anio_arresto) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 13)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"\nN=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 7 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "3.5 ¿En qué fecha lo(a) detuvieron?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt


# 3.6 ¿En qué estado de la República (entidad federativa) sucedió el delito por el que lo(a) acusan? -----------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = Estado_arresto) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 13)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"\nN=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 7 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -40, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "¿En qué estado sucedió el delito por el que el acusan?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt



# 3.8 ¿En qué fecha ocurrió el delito por el que lo(a) acusan?  --------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = P3_8_A) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 7 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "¿En qué año ocurrió el delito por el que lo acusan?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt

# ¿cuánto tiempo estima que pasó entre la ocurrencia del delito y  --------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when( P3_9 == "1" ~ "Inmediatamente",
                             P3_9 == "2" ~ "Pasaron unos días (menos de una semana) ",
                             P3_9 == "3" ~ "Pasó más de una semana y menos de un mes",
                             P3_9 == "4" ~ "Pasó un mes",
                             P3_9 == "5" ~ "Pasó más de un mes y menos de un año",
                             P3_9 == "6" ~ "Pasó un año o más",
                             P3_9 == "7" ~ "Ejército",
                             P3_9 == "8" ~ "Marina",
                             P3_9 == "9" ~ "operativo conjunto",
                             T ~ NA_character_)) %>% 
    select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 15 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "¿cuánto tiempo pasó entre el delito y su detención?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt


# 3.10 ¿Lo(a) detuvieron... -----------------------------------------------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when( P3_10 == "1" ~ "realizando la conducta por el cual lo acusan",
                             P3_10 == "2" ~ "inmediatamente después de la conducta por el cual lo acusan",
                             P3_10 == "3" ~ "con una orden de detención",
                             P3_10 == "4" ~ "después de una inspección o revisión de su cuerpo o pertenencias",
                             P3_10 == "5" ~ "ninguna de las anteriores",
                             T ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,", N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 15 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "3.10 ¿Lo(a) detuvieron...",
       caption = paste0("N=", sum(data2plot$Frequency))); plt



# 3.11 Lo(a) detuvieron, --------------------------------------------------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when( P3_11 == "1" ~ "¿sacándolo(a) del lugar en donde estaba? ",
                             P3_11 == "2" ~ "¿mientras iba pasando por la calle? ",
                             P3_11 == "3" ~ "Otro",
                             T ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,", N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 15 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "3.11 ¿Lo(a) detuvieron...",
       caption = paste0("N=", sum(data2plot$Frequency))); plt

# 3.12 Al momento de realizar la inspección, ¿la autoridad... --------------------------------------------------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  select(P3_12_1,
         P3_12_2,
         P3_12_3,
         P3_12_4,
         P3_12_5) 

data2plot <- data_subset.df %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100),
            N = sum(!is.na(Column))) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "P3_12_1" ~ "lo(a) desvistió?", 
                       values == "P3_12_2" ~ "le dijo qué objeto buscaba?",
                       values == "P3_12_3" ~ "encontró el objeto que buscaba?", 
                       values == "P3_12_4" ~ "le sembró algún objeto?",
                       values == "P3_12_5" ~ "videograbó la inspección?"),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 20),
    order_var = rank(value2plot))


colors4plot <- rep("#2a2a94", length(data2plot$value2plot))


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = paste0(figure, "\n"," Nt = ",N),
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
        legend.text = element_text(family = "Lato Medium"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, 10, 0, 0),
                                 hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size=10))+
  coord_flip() +
  labs(title = "3.12 Al momento de realizar la inspección, ¿la autoridad...",
       caption = paste0("N=", data2plot$N)); plt

# 3.13 Al momento de su detención, ¿el policía o autoridad que lo(a) detuvo... --------------------------------------------------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  select(P3_13_01,
         P3_13_02,
         P3_13_03,
         P3_13_04,
         P3_13_05,
         P3_13_06,
         P3_13_07,
         P3_13_08,
         P3_13_09,
         P3_13_10,
         P3_13_11,
         P3_13_12) 

data2plot <- data_subset.df %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100),
            N = sum(!is.na(Column))) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "P3_13_01" ~ "le indicó que se detuviera o dejara de hacer lo que estaba haciendo", 
                       values == "P3_13_02" ~ "aplicó fuerza física para someterle?",
                       values == "P3_13_03" ~ "le esposó?", 
                       values == "P3_13_04" ~ "utilizó algún arma contundente para someterlo?",
                       values == "P3_13_05" ~ "utilizó algún arma no letal para someterlo?",
                       values == "P3_13_06" ~ "utilizó alguna sustancia química para someterlo?", 
                       values == "P3_13_07" ~ "le amenazó con un arma de fuego para someterlo?",
                       values == "P3_13_08" ~ "le causó alguna lesión menor?", 
                       values == "P3_13_09" ~ "le causó alguna lesión grave?",
                       values == "P3_13_10" ~ "videograbó la inspección?",
                       values == "P3_13_11" ~ "le causó alguna lesión que pusiera en riesgo su vida?", 
                       values == "P3_13_12" ~ "le disparó con un arma de fuego?"),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 30),
    order_var = rank(value2plot))


colors4plot <- rep("#2a2a94", length(data2plot$value2plot))


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = paste0(figure, "\n"," Nt = ",N),
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
        legend.text = element_text(family = "Lato Medium"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, -30, 0, 0),
                                 hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size=10))+
  coord_flip() +
  labs(title = "3.13 Al momento de su detención, ¿el policía o autoridad",
       caption = paste0("N=", data2plot$N)); plt


#3.14 Al momento de su detención, ¿el policía o autoridad... --------------------------------------------------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  select(P3_14_1,
         P3_14_2,
         P3_14_3,
         P3_14_4,
         P3_14_5,
         P3_14_6) 

data2plot <- data_subset.df %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100),
            N = sum(!is.na(Column))) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "P3_14_1" ~ "se identificó como autoridad diciéndole su nombre o número de placa?", 
                       values == "P3_14_2" ~ "le informó a qué tipo de policía o corporación pertenecía?",
                       values == "P3_14_3" ~ "estaba uniformado?", 
                       values == "P3_14_4" ~ "le dijo por qué lo detuvieron?",
                       values == "P3_14_5" ~ " le informó sobre sus derechos a guardar silencio?",
                       values == "P3_14_6" ~ "e dijo a dónde lo llevaría?" ),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 30),
    order_var = rank(value2plot))


colors4plot <- rep("#2a2a94", length(data2plot$value2plot))


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = paste0(figure, "\n"," Nt = ",N),
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
        legend.text = element_text(family = "Lato Medium"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, -30, 0, 0),
                                 hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size=10))+
  coord_flip() +
  labs(title = "3.14 Al momento de su detención, ¿el policía o autoridad...",
       caption = paste0("N=", data2plot$N)); plt

# 3.15 Al momento de su detención, ¿usted... --------------------------------------------------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  select(P3_15_1,
         P3_15_1,
         P3_15_3,
         P3_15_4,
         P3_15_5,
         P3_15_6,
         P3_15_7,
         P3_15_8,
         P3_15_9) 

data2plot <- data_subset.df %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100),
            N = sum(!is.na(Column))) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "P3_15_1" ~ "obedeció las órdenes del policía o autoridad que lo detuvo?", 
                       values == "P3_15_2" ~ "portaba alguna arma punzo cortante?",
                       values == "P3_15_3" ~ "portaba alguna arma de fuego?", 
                       values == "P3_15_4" ~ "amenazó a alguien con el arma?",
                       values == "P3_15_5" ~ "disparó el arma de fuego?",
                       values == "P3_15_6" ~ "manipuló algún objeto para usarlo como arma?",
                       values == "P3_15_7" ~ "trató de sobornar a la autoridad para evitar su detención?",
                       values == "P3_15_8" ~ "trató de defenderse físicamente??",
                       values == "P3_15_9" ~ "rató de escapar para que no lo(a) detuvieran?"),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 30),
    order_var = rank(value2plot))


colors4plot <- rep("#2a2a94", length(data2plot$value2plot))


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = paste0(figure, "\n"," Nt = ",N),
                  color = labels)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 10 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 105),
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
        legend.text = element_text(family = "Lato Medium"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, -30, 0, 0),
                                 hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size=10))+
  coord_flip() +
  labs(title = "3.15 Al momento de su detención, ¿usted...",
       caption = paste0("N=", data2plot$N)); plt

#3.16 Desde su detención y hasta antes de llegar a la Agencia del Ministerio Público o con un Juez de lo penal, ¿usted fue interrogado(a) por la policía o autoridad para dar información?  --------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when(P3_16 == "1" ~ "Sí",
                            P3_16 == "2" ~ "No",
                            T ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,", N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 10 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "3.16 ¿usted fue interrogado(a) por la policía o autoridad para dar información?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt


# 3.17 ¿la policía realizó o permitió las siguientes situaciones? --------------------------------------------------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  select(P3_17_01,
         P3_17_02,
         P3_17_03,
         P3_17_04,
         P3_17_05,
         P3_17_06,
         P3_17_07,
         P3_17_08,
         P3_17_09,
         P3_17_10,
         P3_17_11) 

data2plot <- data_subset.df %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100),
            N = sum(!is.na(Column))) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "P3_17_01" ~ "Le amenazaron con levantarle cargos falsos", 
                       values == "P3_17_02" ~ "Le amenazaron con matarlo",
                       values == "P3_17_03" ~ "Le amenazaron con hacerle daño a usted ", 
                       values == "P3_17_04" ~ "Le amenazaron con hacerle daño a su familia",
                       values == "P3_17_05" ~ "Le hicieron otro tipo de amenazas",
                       values == "P3_17_06" ~ "Le presionaron para denunciar a alguien",
                       values == "P3_17_07" ~ "Le incomunicaron o aislaron",
                       values == "P3_17_08" ~ "Le pasearon en un automóvil dando vueltas por las calles",
                       values == "P3_17_09" ~ "Le hicieron daño a su familia",
                       values == "P3_17_10" ~ "Le desvistieron",
                       values == "P3_17_11" ~ "Le vendaron los ojos o cubrieran la cabeza para que no viera"),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 30),
    order_var = rank(value2plot))


colors4plot <- rep("#2a2a94", length(data2plot$value2plot))


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = paste0(figure, "\n"," Nt = ",N),
                  color = labels)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 10 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 105),
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
        legend.text = element_text(family = "Lato Medium"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, -30, 0, 0),
                                 hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size=10))+
  coord_flip() +
  labs(title = "3.17 ¿la policía realizó o permitió las siguientes situaciones?",
       caption = paste0("N=", data2plot$N)); plt


#3.18 Después de su detención y hasta antes de llegar a la Agencia del Ministerio Público o con un Juez de lo penal, ¿la policía o autoridad realizó o permitió alguna de las siguientes AGRESIONES FÍSICAS a su persona? --------------------------------------------------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  select(P3_18_01,
         P3_18_01,
         P3_18_03,
         P3_18_04,
         P3_18_05,
         P3_18_06,
         P3_18_07,
         P3_18_08,
         P3_18_09,
         P3_18_10,
         P3_18_11,
         P3_18_12,
         P3_18_13,
         P3_18_14,
         P3_18_15) %>% 
  mutate(P3_18_11 = case_when(P3_18_11 == "1" ~ 1,
                              P3_18_11 == "2" ~ 0,
                              T ~ NA_real_) )

data2plot <- data_subset.df %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100),
            N = sum(!is.na(Column))) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "P3_18_01" ~ "Ataron su cuerpo", 
                       values == "P3_18_02" ~ "Le impidieron respirar asfixiándolo",
                       values == "P3_18_03" ~ "metieron su cabeza en agua", 
                       values == "P3_18_04" ~ "golpearon con las manos",
                       values == "P3_18_05" ~ "golpearon con objetos",
                       values == "P3_18_06" ~ "Le quemaron",
                       values == "P3_18_07" ~ "Le dieron descargas eléctricas",
                       values == "P3_18_08" ~ "Aplastaron su cuerpo",
                       values == "P3_18_09" ~ "lesiones con arma blanca",
                       values == "P3_18_10" ~ "Le encajaron agujas",
                       values == "P3_18_11" ~ "lesiones por arma de fuego",
                       values == "P3_18_12" ~ "gredieron mediante acoso sexual",
                       values == "P3_18_13" ~ "Le lastimaron sus órganos sexuales",
                       values == "P3_18_14" ~ "violación sexual",
                       values == "P3_18_15" ~ "Otra agresión física",),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 30),
    order_var = rank(value2plot))


colors4plot <- rep("#2a2a94", length(data2plot$value2plot))


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = paste0(figure," Nt = ",N),
                  color = labels)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 15 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 105),
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
        legend.text = element_text(family = "Lato Medium"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, -30, 0, 0),
                                 hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size=10))+
  coord_flip() +
  labs(title = "3.18 ¿la policía realizó agresión FÍSICA a su persona?",
       caption = paste0("N=", data2plot$N)); plt


# 3.8 ¿En qué fecha ocurrió el delito por el que lo(a) acusan?  --------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = P3_8_A) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 7 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "¿En qué año ocurrió el delito por el que lo acusan?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt

#3.19 Después de su detención, ¿a dónde fue llevado(a) por primera vez? --------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = Primer_lugar_traslado) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 15 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "3.19 Después de su detención, ¿a dónde fue llevadopor primera vez?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt

#3.20 Aproximadamente, ¿cuánto tiempo pasó entre su detención y su llegada a la Agencia del Ministerio Público o con un Juez de lo penal? --------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = Tiempo_traslado) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 15 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "3.20 ¿cuánto tiempo pasó entre su detención y su llegada al MP?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt

#3.20 Aproximadamente, ¿cuánto tiempo pasó entre su detención y su llegada a la Agencia del Ministerio Público o con un Juez de lo penal? --------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when(P3_21_1_1 == 1 ~ "Sí", 
                            P3_21_1_1 == 0 ~ "No", 
                            T ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 15 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "3.21 La policía  INTENTÓ APROPIARSE o LE PIDIÓ dinero",
       caption = paste0("N=", sum(data2plot$Frequency))); plt

#3.21 La policía o autoridad que lo(a) detuvo a cambio de dejarlo(a) ir, no golpearlo(a) o no hacer daño a su familia, etc... --------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when(P3_21_1_2 == 1 ~ "Sí", 
                            P3_21_1_2 == 0 ~ "No", 
                            T ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 15 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "3.21_2 La policía le INSINUÓ O GENERÓ las condiciones  dinero",
       caption = paste0("N=", sum(data2plot$Frequency))); plt


# 3.22 Si usted o algún familiar les daba dinero, pertenencias, regalos o les hacían favores, ¿qué le darían a cambio?--------------------------------------------------


data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  select(P3_22_1,
         P3_22_2,
         P3_22_3,
         P3_22_4,
         P3_22_5) 

data2plot <- data_subset.df %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100),
            N = sum(!is.na(Column))) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "P3_22_1" ~ "Lo dejarían ir", 
                       values == "P3_22_2" ~ "No loagredirían",
                       values == "P3_22_3" ~ "No le harían daño a su familia o amigos", 
                       values == "P3_22_4" ~ "Modificarían la versión de los hechos o la evidencia en su contra",
                       values == "P3_22_5" ~ "Otra"),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 30),
    order_var = rank(value2plot))


colors4plot <- rep("#2a2a94", length(data2plot$value2plot))


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = paste0(figure, "\n"," Nt = ",N),
                  color = labels)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 10 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 105),
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
        legend.text = element_text(family = "Lato Medium"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, -30, 0, 0),
                                 hjust = 0),
        legend.title = element_blank(),
        plot.title = element_text(size=10))+
  coord_flip() +
  labs(title = "3.22 Qué le darían a cambio",
       caption = paste0("N=", data2plot$N)); plt

#3.23 ¿Usted o su familia dieron a la autoridad el dinero, bien, regalo o hicieron el favor que les pedían? --------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when(P3_23 == 1 ~ "Sí", 
                            P3_23 == 2 ~ "No", 
                            T ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 15 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "3.23 ¿dieron a la autoridad el dinero que les pedían?",
       caption = paste0("N=", sum(data2plot$Frequency))); plt

#3.25 La autoridad que lo(a) detuvo, ¿le quitó o robó su dinero o pertenencias sin ofrecerle nada a cambio? --------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P3_3 == "Oaxaca", 
         fuero == "Sólo común") %>% 
  mutate(centro = case_when(P3_25 == 1 ~ "Sí", 
                            P3_25 == 2 ~ "No", 
                            T ~ NA_character_)) %>% 
  select(centro)


data2plot <- data_subset.df %>%
  group_by(centro) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(centro) %>% 
  rename( values = centro ) %>%
  ungroup() %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         values = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(order = row_number())

colors4plot <- rep(mainColor, 20)


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -order),
                  y     = value2plot,
                  label = paste0(figure,"N=", Frequency),
                  color = values,
                  fill  = values)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 15 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
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
                                 margin   = margin(0, -5, 0, 0),
                                 hjust = 0),
        plot.title = element_text(size=10)) +
  coord_flip() +
  labs(title = "3.25 La autoridad, ¿le quitó o robó sin ofrecerle nada a cambio? ",
       caption = paste0("N=", sum(data2plot$Frequency))); plt
