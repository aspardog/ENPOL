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
  geom_rect( ) +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  geom_text( x= 3.5,
            aes(y    = value2plot-4 , 
                label = paste0(figure, "\n"," N = ",Frequency)), 
            # position = "stack",
            color    = "white",
            family   = "Lato Full",
            fontface = "bold", 
            size = 4.514598) +
  scale_fill_manual(values =  c("#9c94ff","#43a9a7"))+
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
    legend.position = "none"); plot

# : Proporción de Reincidentes por el mismo delito -------------------------------------------------------------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(reincidencia = case_when(P9_1 == "1" ~ "reincidentes",
                                  P9_1 == "2" ~ "No reincidentes",
                                  T ~ NA_character_)) %>% 
  filter(reincidencia == "reincidentes") %>%
  mutate(reincidentes_tipo = case_when(Delito_gr_1_robos == 1 &
                                         P9_2_01 == "1" | P9_2_02 == "1" | P9_2_03 == "1" | 
                                         P9_2_04 == "1" | P9_2_05 == "1" | P9_2_06 == "1" |
                                         P9_2_07 == "1" ~ "Mismo delito-robos",
                                       Delito_gr_2_drogas == 1 &
                                         P9_2_08 == "1" | P9_2_09 == "1"  ~ "Mismo delito-drogas",
                                       Delito_gr_3_del_org == 1 &
                                         P9_2_20 == "1"   ~ "Mismo delito-organizada",
                                       Delito_gr_4_lesiones &
                                         P9_2_10 == "1"~ "Mismo delito-lesiones",
                                       Delito_gr_5_hom_cul &
                                         P9_2_11 == "1"~ "Mismo delito-hom_cul",
                                       Delito_gr_6_hom_dol &
                                         P9_2_12 == "1"~ "Mismo delito-homicido_dol",
                                       Delito_gr_7_armas &
                                         P9_2_13 == "1"~ "Mismo delito-armas",
                                       Delito_gr_8_viol_fam &
                                         P9_2_15 == "1"~ "Mismo delito-viol_fam",
                                       Delito_gr_9_secuestro &
                                         P9_2_17 == "1" | P9_2_23 == "1" ~ "Mismo delito_secuestro",
                                       Delito_gr_10_sexuales &
                                         P9_2_18 == "1" | P9_2_21 == "1" ~ "Mismo delito_sexuales",
                                       Delito_gr_11_extorsion &
                                         P9_2_22 == "1"~ "Mismo delito-extorsion",
                                       Delito_gr_12_fraude &
                                         P9_2_19 == "1" | P9_2_24 == "1" ~ "Mismo delito_fraude",
                                       Delito_gr_13_amenazas &
                                         P9_2_25 == "1"~ "Mismo delito_amenazas",
                                       Delito_gr_14_otro &
                                         P9_2_14 == "1" | P9_2_16 == "1" | P9_2_26 == "1"~ "Mismo delito-otro",
                                       T ~ "Distinto delito"))



data2plot <- Main_database_2008 %>% 
  select(reincidentes_tipo) %>% 
  group_by(reincidentes_tipo) %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  rename(values = reincidentes_tipo) %>% 
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
  geom_rect( ) +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  geom_text( x= 3.5,
             aes(y    = value2plot-4 , 
                 label = paste0(figure, "\n"," N = ",Frequency)), 
             # position = "stack",
             color    = "white",
             family   = "Lato Full",
             fontface = "bold", 
             size = 4.514598) +
  scale_fill_manual(values =  c("#cfb3ff","#fa4d57"))+
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
    legend.position = "none"); plot


# Tipo de reincidencia junta con no reincidente -------------------------------------------------------------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(reincidencia = case_when(P9_1 == "1" ~ "reincidentes",
                                  P9_1 == "2" ~ "No reincidentes",
                                  T ~ NA_character_),
        reincidentes_tipo = case_when( reincidencia == "reincidentes" & Delito_gr_1_robos == 1 &
                                         P9_2_01 == "1" | P9_2_02 == "1" | P9_2_03 == "1" | 
                                         P9_2_04 == "1" | P9_2_05 == "1" | P9_2_06 == "1" |
                                         P9_2_07 == "1" ~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_2_drogas == 1 &
                                         P9_2_08 == "1" | P9_2_09 == "1"  ~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_3_del_org == 1 &
                                         P9_2_20 == "1"   ~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_4_lesiones &
                                         P9_2_10 == "1"~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_5_hom_cul &
                                         P9_2_11 == "1"~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_6_hom_dol &
                                         P9_2_12 == "1"~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_7_armas &
                                         P9_2_13 == "1"~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_8_viol_fam &
                                         P9_2_15 == "1"~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_9_secuestro &
                                         P9_2_17 == "1" | P9_2_23 == "1" ~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_10_sexuales &
                                         P9_2_18 == "1" | P9_2_21 == "1" ~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_11_extorsion &
                                         P9_2_22 == "1"~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_12_fraude &
                                         P9_2_19 == "1" | P9_2_24 == "1" ~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_13_amenazas &
                                         P9_2_25 == "1"~ "Mismo delito",
                                       reincidencia == "reincidentes" & Delito_gr_14_otro &
                                         P9_2_14 == "1" | P9_2_16 == "1" | P9_2_26 == "1"~ "Mismo delito",
                                       reincidencia == "No reincidentes" ~ "No reincidentes",
                                       T ~ "Distinto delito"))



data2plot <- Main_database_2008 %>% 
  select(reincidentes_tipo) %>% 
  group_by(reincidentes_tipo) %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  rename(values = reincidentes_tipo) %>% 
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
  geom_rect( ) +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  geom_text( x= 3.5,
             aes(y    = value2plot-2.9 , 
                 label = paste0(figure, "\n"," N=",Frequency)), 
             # position = "stack",
             color    = "white",
             family   = "Lato Full",
             fontface = "bold", 
             size = 4.514598) +
  scale_fill_manual(values =  c("#cfb3ff","#fa4d57", "#43a9a7"))+
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
    legend.position = "none"); plot


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
                 label = paste0(figure, "\n"," N = ",Frequency)), 
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
    legend.position      = "none"); plot

# De las personas que tienen más de un proceso, cuántas personas cometieron un delito antes (P_5.29.1) --------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(multi_proceso = case_when(as.numeric(P5_28) >= 2 &  as.numeric(P5_28) <= 90 ~ "Más de un proceso abierto",
                                   as.numeric(P5_28) <= 1 ~ "Sólo un proceso abierto",
                                   T ~ NA_character_),
         reincidencia = case_when(P9_1 == "1" ~ "reincidentes",
                                         P9_1 == "2" ~ "No reincidentes",
                                         T ~ NA_character_)) %>% 
  filter(multi_proceso == "Más de un proceso abierto")

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
                 label = paste0(figure, "\n"," N = ",Frequency)), 
             # position = "stack",
             color    = "white",
             family   = "Lato Full",
             fontface = "bold", 
             size = 4.514598) +
  scale_fill_manual(values =  c("#2a2a9A","#9c94ff"))+
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
    legend.position      = "none"); plot

# De las personas que tienen más de un proceso, cuántas personas cometieron un delito antes (P_5.29.1) --------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(multi_proceso = case_when(as.numeric(P5_28) >= 2 &  as.numeric(P5_28) <= 90 ~ "Más de un proceso abierto",
                                   as.numeric(P5_28) <= 1 ~ "Sólo un proceso abierto",
                                   T ~ NA_character_),
         reincidencia = case_when(P9_1 == "1" ~ "reincidentes",
                                  P9_1 == "2" ~ "No reincidentes",
                                  T ~ NA_character_),
         multiproceso_reincidencia = case_when(multi_proceso == "Más de un proceso abierto" & 
                                                 reincidencia == "reincidentes"              ~ "Multi-reincidente",
                                               multi_proceso == "Más de un proceso abierto" & 
                                                 reincidencia == "No reincidentes"              ~ "Multi-Noreincidente",
                                               multi_proceso == "Sólo un proceso abierto" & 
                                                 reincidencia == "reincidentes"              ~ "Uno-reincidente",
                                               multi_proceso == "Sólo un proceso abierto" & 
                                                 reincidencia == "No reincidentes"              ~ "Uno-Noreincidente",
                                               T ~ NA))

data2plot <- Main_database_2008 %>% 
  select(multiproceso_reincidencia) %>% 
  group_by(multiproceso_reincidencia) %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  rename(values = multiproceso_reincidencia) %>% 
  mutate(
    value2plot = Frequency / sum(Frequency) * 100,
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(values, width = 20)) %>% 
  arrange(value2plot) %>% 
  mutate(ymin = c(0, head(value2plot, -1))) 
  


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
             aes(y    = value2plot-1.5, 
                 label = paste0(figure, "\n"," N = ",Frequency)), 
             # position = "stack",
             color    = "white",
             family   = "Lato Full",
             fontface = "bold", 
             size = 2) +
  scale_fill_manual(values =  c("#4e43dd","#9c94ff","#20204a" ,"#2a2a9A" ))+
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
    legend.position      = "none"); plot

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



# Proporción de personas que fueron sentenciadas por más de un delito --------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1,
         sentenciado == 1) %>% 
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
                 label = paste0(figure, "\n"," N = ",Frequency)), 
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
    legend.position      = "none"); plot



# Desdoblar el mismo delito -----------------------------------------------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(reincidencia = case_when(P9_1 == "1" ~ "reincidentes",
                                  P9_1 == "2" ~ "No reincidentes",
                                  T ~ NA_character_)) %>% 
  filter(reincidencia == "reincidentes") %>%
  mutate(reincidentes_tipo = case_when(Delito_gr_1_robos == 1 &
                                         P9_2_01 == "1" | P9_2_02 == "1" | P9_2_03 == "1" | 
                                         P9_2_04 == "1" | P9_2_05 == "1" | P9_2_06 == "1" |
                                         P9_2_07 == "1" ~ "Robos",
                                       Delito_gr_2_drogas == 1 &
                                         P9_2_08 == "1" | P9_2_09 == "1"  ~ "Drogas",
                                       Delito_gr_3_del_org == 1 &
                                         P9_2_20 == "1"   ~ "Mismo delito-organizada",
                                       Delito_gr_4_lesiones &
                                         P9_2_10 == "1"~ "Mismo delito-lesiones",
                                       Delito_gr_5_hom_cul &
                                         P9_2_11 == "1"~ "Mismo delito-hom_cul",
                                       Delito_gr_6_hom_dol &
                                         P9_2_12 == "1"~ "Homicidio doloso",
                                       Delito_gr_7_armas &
                                         P9_2_13 == "1"~ "Armas",
                                       Delito_gr_8_viol_fam &
                                         P9_2_15 == "1"~ "Mismo delito-viol_fam",
                                       Delito_gr_9_secuestro &
                                         P9_2_17 == "1" | P9_2_23 == "1" ~ "Secuestro",
                                       Delito_gr_10_sexuales &
                                         P9_2_18 == "1" | P9_2_21 == "1" ~ "Delitos Sexuales",
                                       Delito_gr_11_extorsion &
                                         P9_2_22 == "1"~ "Mismo delito-extorsion",
                                       Delito_gr_12_fraude &
                                         P9_2_19 == "1" | P9_2_24 == "1" ~ "Mismo delito_fraude",
                                       Delito_gr_13_amenazas &
                                         P9_2_25 == "1"~ "Mismo delito_amenazas",
                                       Delito_gr_14_otro &
                                         P9_2_14 == "1" | P9_2_16 == "1" | P9_2_26 == "1"~ "Otro",
                                       T ~ "Distinto delito"))


data2plot <- Main_database_2008 %>%
  # filter(!is.na(reincidentes_tipo)) %>%
  group_by(reincidentes_tipo) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         Delito = reincidentes_tipo,
         Delito = str_wrap(Delito, width = 30)) %>%
  select(Delito,value2plot,labels) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito)) %>% 
  filter(value2plot >= 1,
         Delito != "Distinto delito")

colors4plot <- rep("#E2E2F7", 7)


plt <- ggplot(data2plot, 
              aes(x     = Delito,
                  y     = value2plot,
                  label = labels,
                  color = Delito)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 3 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  #xlab("Porcentaje de criterios cumplidos")+
  scale_y_discrete() +
  scale_x_discrete( ) +
  expand_limits(y = c(0, 60))+
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10))+
  coord_flip()

plt