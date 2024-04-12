## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Milestone 2
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres            (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     April 4th, 2024
##
## This version:      April 8th, 2024
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

source("National/Data_cleaning/Code/settings.R")
library(ggrepel)


load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Prepare prioritary crimes variables                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Main_database1 <- Main_database %>%
  filter(Anio_arresto >= 2011, Delito_unico == 1) %>%
#  
#  mutate(tiempo_sentencia = case_when(as.numeric(P5_4_A) == 0 ~ NA_real_,
#                                      as.numeric(P5_4_A) >= 97 ~ NA_real_,
#                                      T ~ as.numeric(P5_4_A)),
#         Calderon = case_when(Anio_arresto == 2006 & Mes_arresto > 11 ~ 1,
#                              Anio_arresto > 2006 & Anio_arresto < 2012 ~ 1,
#                              Anio_arresto == 2012 & Mes_arresto < 12 ~ 1,
#                              T ~ 0),
#         Peña = case_when(Anio_arresto == 2012 & Mes_arresto > 11 ~ 1,
#                          Anio_arresto > 2012 & Anio_arresto < 2018 ~ 1,
#                          Anio_arresto == 2018 & Mes_arresto < 12 ~ 1,
#                          T ~ 0),
#         AMLO = case_when(Anio_arresto == 2018 & Mes_arresto > 11 ~ 1,
#                          Anio_arresto > 2018 ~ 1,
#                          T ~ 0),
#         # alt1 considera delitos de PPO
#    Delito_prioritario = case_when(Calderon == 1 & Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_08 == 1|P5_31_08 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_18 == 1|P5_31_18 == 1) ~ 1,
#                                        Calderon == 1 & Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
#                                        Peña == 1 & Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
#                                        Peña == 1 & Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
#                                        Peña == 1 & Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ 1,
#                                        Peña == 1 & Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
#                                        Peña == 1 & Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
#                                        Peña == 1 & Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
#                                        Peña == 1 & Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ 1,
#                                        Peña == 1 & Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ 1,
#                                        Peña == 1 & Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
#                                        Peña == 1 & Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ 1,
#                                        AMLO == 1 & Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
#                                        AMLO == 1 & Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
#                                        AMLO == 1 & Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
#                                        AMLO == 1 & Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
#                                        AMLO == 1 & Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ 1,
#                                        AMLO == 1 & Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
#                                        T ~ 0),
#    Delito_prioritario_Cald = case_when(Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_08 == 1|P5_31_08 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_18 == 1|P5_31_18 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
#                                   T ~ 0),
#    Delito_prioritario_Peña = case_when( Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ 1,
#                                   T ~ 0),
#    Delito_prioritario_AMLO = case_when( Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ 1,
#                                    Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ 1,
#                                   T ~ 0)) %>%
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


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Get data from ENVIPE                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ENVIPE <- read_dta(paste0(path2DB,"/National/Exploration/Input/Politica_criminal/ocurrencias.dta")) %>%
  pivot_longer(cols = del_1:del_15, names_to = "Delito_envipe",values_to = "Ocurrencias")  %>%
  group_by(anio, Delito_envipe) %>%
  summarise(Ocurrencias = sum(Ocurrencias, na.rm = T)) %>%
  mutate(Delito_envipe = case_when(Delito_envipe == "del_1" ~ "Robo total de vehículo", 
                                  Delito_envipe == "del_2" ~ "Robo de accesorios, refacciones o \n herramientas de vehículos", 
                                  Delito_envipe == "del_3" ~ "Pinta de barda o grafiti en su casa, \n rayones intencionales en su vehículo u otro \n tipo de vandalismo", 
                                  Delito_envipe == "del_4" ~ "Alguien entró a su casa o departamento \n sin permiso mediante el uso de la fuerza o \n por engaños y robó o intentó robar algo", 
                                  Delito_envipe == "del_5" ~ "Robo o asalto en la calle o en el \n transporte público", 
                                  Delito_envipe == "del_6" ~ "Robo en forma distinta a la anterior", 
                                  Delito_envipe == "del_7" ~ "Alguien usó su chequera, número de \n tarjeta o cuenta bancaria sin su permiso para \n realizar cargos o para extraer dinero de sus cuentas \n (fraude bancario) o le dio dinero falso", 
                                  Delito_envipe == "del_8" ~ "Entregó dinero por un producto o un \n servicio que no recibió conforme a lo acordado \n (fraude al consumidor)", 
                                  Delito_envipe == "del_9" ~ "Amenazas, presiones o engaños para \n exigirle dinero o bienes; o para que hiciera algo \n o dejara de hacerlo (extorsión)", 
                                  Delito_envipe == "del_10" ~ "Amenazas verbales de alguien plenamente \n identificado o por escrito hacia su persona \n diciendo que le va a causar un daño a usted, \n a su familia, a sus bienes o su trabajo", 
                                  Delito_envipe == "del_11" ~ "Alguien sólo por actitud abusiva o por \n una discusión lo(a) golpeó, empujó o atacó generándole \n una lesión física (moretones, fracturas, cortadas, etc.)", 
                                  Delito_envipe == "del_12" ~ "Lo secuestraron para exigir dinero o bienes", 
                                  Delito_envipe == "del_13" ~ "Alguien en contra de su voluntad lo(a) \n agredió mediante hostigamiento o intimidación sexual, manoseo, \n exhibicionismo o intento de violación", 
                                  Delito_envipe == "del_14" ~ "Fue obligado(a) mediante violencia física o \n amenaza por alguien conocido o desconocido a tener una \n actividad sexual no deseada (Violación sexual) ", 
                                  Delito_envipe == "del_15" ~ "Otro"))


ENVIPE_prioriotarios <- ENVIPE  %>% 
  arrange(anio, -Ocurrencias) %>%
  group_by(anio) %>%
  slice(1:3)

table(ENVIPE$anio,ENVIPE$Delito_envipe)

#anio   del_2 del_3 del_4 del_5 del_9
#2014     1     1     0     0     1
#2015     1     1     0     0     1
#2016     1     1     0     0     1
#2017     1     1     0     0     1
#2018     1     1     0     1     0
#2019     1     1     0     1     0
#2020     1     1     0     1     0
#2021     1     0     1     1     0
#2022     1     1     0     0     1
#2023     1     1     0     0     1

#Delitos prioritarios ENPOL robo de autopartes, danio a la propiedad, extorsión (a veces robo a transeúnte)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Create analysis                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 Delitos ENPOL 2018-2021

data2plot <- Main_database1 %>%
  filter(Anio_arresto >= 2018, !is.na(Delito_unico_ungrouped_categ)) %>%
  group_by(Delito_unico_ungrouped_categ) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "Arrestos",
         Delito = Delito_unico_ungrouped_categ) %>%
  select(Delito,value2plot,labels,group_var) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito))

colors4plot <- c("#003B88", 
                 "#ef0b4b",
                 "#f4cc21",
                 "#91288c",
                 "#f05b42",
                 "#2ba7a4",
                 "#2779bd",
                 "#eb9727",
                 "#2d3589",
                 "#d12241",
                 "#90d1eb",
                 "#003B88", 
                 "#ef0b4b",
                 "#f4cc21",
                 "#91288c",
                 "#f05b42",
                 "#2ba7a4",
                 "#2779bd",
                 "#eb9727",
                 "#2d3589",
                 "#d12241",
                 "#90d1eb",
                 "#003B88", 
                 "#ef0b4b",
                 "#f4cc21",
                 "#003B88")


plt <- ggplot(data2plot, 
              aes(x     = Delito,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = Delito)) +
  geom_bar(stat = "identity", fill = colors4plot,
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
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid")) +
  coord_flip()



ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure1_1.svg"), 
       width  = 175, 
       height = 175,
       units  = "mm",
       dpi    = 72,
       device = "svg")



## 2 Delitos ENVIPE 2018-2021


data2plot <- ENVIPE %>%
  filter(anio >= 2018) %>%
  group_by(Delito_envipe) %>%
  summarise(n = sum(Ocurrencias)) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "ENVIPE",
         Delito = Delito_envipe) %>%
  select(Delito,value2plot,labels,group_var) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito))

colors4plot <- c("#003B88", 
                 "#ef0b4b",
                 "#f4cc21",
                 "#91288c",
                 "#f05b42",
                 "#2ba7a4",
                 "#2779bd",
                 "#eb9727",
                 "#2d3589",
                 "#d12241",
                 "#90d1eb",
                 "#003B88", 
                 "#ef0b4b",
                 "#ef0b4b",
                 "#f4cc21")


plt <- ggplot(data2plot, 
              aes(x     = Delito,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = Delito)) +
  geom_bar(stat = "identity", fill = colors4plot,
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
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid")) +
  coord_flip()



ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure1_2.svg"), 
       width  = 175, 
       height = 175,
       units  = "mm",
       dpi    = 72,
       device = "svg")


##3.⁠Delitos ENVIPE en ENPOL, 2018-2021


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
  select(Anio_arresto,Delito_prioritario_ENVIPE,value2plot,labels,group_var) %>%
filter(Delito_prioritario_ENVIPE == 1)



# Creating ggplot

colors4plot <- c("#003B88")

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
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"))



ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure1_3.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")
