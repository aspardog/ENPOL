# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Piloto de Hipótesis
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##
## Dependencies:      World Justice Project
##
## Creation date:     Jun 4th, 2024
##
## This version:      Jun 5th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes and other general routines are
# loaded from the following script:
source("National/Data_cleaning/Code/settings.R")
library(readxl)
library(ggrepel)

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

snsp <- read.csv(paste0(path2DB,"/National/Exploration/Input/IDEFC_NM_abr24.csv"),check.names = F)

inegi <- read_xlsx(paste0(path2DB,"/National/Exploration/Input/INEGI_homicidios.xlsx"))


#### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Analysis                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


snsp_ <- snsp %>%
  mutate(Agosto = case_when(Anio==2021 ~ 0, T ~ Agosto),
         Septiembre = case_when(Anio==2021 ~ 0, T ~ Septiembre),
         Octubre = case_when(Anio==2021 ~ 0, T ~ Octubre),
         Noviembre = case_when(Anio==2021 ~ 0, T ~ Noviembre),
         Diciembre = case_when(Anio==2021 ~ 0, T ~ Diciembre)) %>%
  filter(Anio>=2017 & Anio<= 2021) %>%
  mutate(Incidencia = Enero+Febrero+Marzo+Abril+Mayo+Junio+Julio+Agosto+Septiembre+Octubre+Noviembre+Diciembre) %>%
  mutate(Delito=case_when(`Subtipo de delito`== "Homicidio doloso" ~ "Homicidio doloso, SESNSP",
                          `Subtipo de delito`== "Homicidio culposo" ~ "Homicidio culposo, SESNSP",
                          T ~ "Otro")) %>% 
  group_by(Delito,Anio) %>%
  summarize(Incidencia = sum(Incidencia)) %>%
  mutate(Anio=as.character(Anio),
         value2plot =  100 * Incidencia / sum(Incidencia),
                                                 labels = paste0(round(value2plot,0), "%"),
                                                 group_var =  "Homicidios_SNSP") %>%
  filter(Delito!="Otro")



snsp_2<- snsp %>%
  mutate(Agosto = case_when(Anio==2021 ~ 0, T ~ Agosto),
         Septiembre = case_when(Anio==2021 ~ 0, T ~ Septiembre),
         Octubre = case_when(Anio==2021 ~ 0, T ~ Octubre),
         Noviembre = case_when(Anio==2021 ~ 0, T ~ Noviembre),
         Diciembre = case_when(Anio==2021 ~ 0, T ~ Diciembre)) %>%
  filter(Anio>=2017 & Anio<= 2021) %>%
  mutate(Incidencia = Enero+Febrero+Marzo+Abril+Mayo+Junio+Julio+Agosto+Septiembre+Octubre+Noviembre+Diciembre) %>%
  mutate(Delito=case_when(`Subtipo de delito`== "Homicidio doloso" ~ "Homicidio doloso, SESNSP",
                          `Subtipo de delito`== "Homicidio culposo" ~ "Homicidio culposo, SESNSP",
                          T ~ "Otro")) %>% 
  group_by(Delito,Anio) %>%
  summarize(Incidencia = sum(Incidencia)) %>%
  mutate(Anio=as.character(Anio),
         value2plot =  Incidencia,
         labels = paste0(round(value2plot,0)),
         group_var =  "Homicidios_SNSP") %>%
  filter(Delito!="Otro")



enpol_1 <- Main_database %>% 
  filter(Anio_arresto>=2017 & Anio_arresto<= 2021 & sentenciado==1)  %>%
  select(Anio_arresto,Delito_gr_1_robos,Delito_gr_2_drogas,Delito_gr_3_del_org,Delito_gr_4_lesiones,Delito_gr_5_hom_cul,
         Delito_gr_6_hom_dol,Delito_gr_7_armas,Delito_gr_8_viol_fam,Delito_gr_9_secuestro,Delito_gr_10_sexuales,
         Delito_gr_11_extorsion,Delito_gr_12_fraude,Delito_gr_13_amenazas,Delito_gr_14_otro,Delito_gr_15_ns_nr)  %>% 
  pivot_longer(cols =c(Delito_gr_1_robos,Delito_gr_2_drogas,Delito_gr_3_del_org,Delito_gr_4_lesiones,Delito_gr_5_hom_cul,
                       Delito_gr_6_hom_dol,Delito_gr_7_armas,Delito_gr_8_viol_fam,Delito_gr_9_secuestro,Delito_gr_10_sexuales,
                       Delito_gr_11_extorsion,Delito_gr_12_fraude,Delito_gr_13_amenazas,Delito_gr_14_otro,Delito_gr_15_ns_nr), 
               names_to = "Tipo_de_delito", values_to = "Sentenciados") %>%
  mutate(Delito=case_when(Tipo_de_delito== "Delito_gr_6_hom_dol" ~ "Homicidio doloso, ENPOL",
                          Tipo_de_delito== "Delito_gr_5_hom_cul" ~ "Homicidio culposo, ENPOL",
                          T ~ "Otro")) %>%
group_by(Anio_arresto,Delito) %>%
  summarize(Sentenciados = sum(Sentenciados)) %>%
  mutate(value2plot =  100 * Sentenciados / sum(Sentenciados),
         labels = paste0(round(value2plot,0), "%"),
         group_var =  "Sentenciados_ENPOL") %>%
  filter(Delito!="Otro") %>%
  rename(Anio = Anio_arresto)



enpol_2 <- Main_database %>% 
  filter(Anio_arresto>=2017 & Anio_arresto<= 2021 & sentenciado == 1 & Delito_unico == 1)  %>%
  select(Anio_arresto,Delito_gr_1_robos,Delito_gr_2_drogas,Delito_gr_3_del_org,Delito_gr_4_lesiones,Delito_gr_5_hom_cul,
         Delito_gr_6_hom_dol,Delito_gr_7_armas,Delito_gr_8_viol_fam,Delito_gr_9_secuestro,Delito_gr_10_sexuales,
         Delito_gr_11_extorsion,Delito_gr_12_fraude,Delito_gr_13_amenazas,Delito_gr_14_otro,Delito_gr_15_ns_nr)  %>% 
  pivot_longer(cols =c(Delito_gr_1_robos,Delito_gr_2_drogas,Delito_gr_3_del_org,Delito_gr_4_lesiones,Delito_gr_5_hom_cul,
                       Delito_gr_6_hom_dol,Delito_gr_7_armas,Delito_gr_8_viol_fam,Delito_gr_9_secuestro,Delito_gr_10_sexuales,
                       Delito_gr_11_extorsion,Delito_gr_12_fraude,Delito_gr_13_amenazas,Delito_gr_14_otro,Delito_gr_15_ns_nr), 
               names_to = "Tipo_de_delito", values_to = "Sentenciados") %>%
  mutate(Delito=case_when(Tipo_de_delito== "Delito_gr_6_hom_dol" ~ "Homicidio doloso, ENPOL",
                          Tipo_de_delito== "Delito_gr_5_hom_cul" ~ "Homicidio culposo, ENPOL",
                          T ~ "Otro")) %>%
  group_by(Anio_arresto,Delito) %>%
  summarize(Sentenciados = sum(Sentenciados)) %>%
  mutate(value2plot =  100 * Sentenciados / sum(Sentenciados),
         labels = paste0(round(value2plot,0), "%"),
         group_var =  "Sentenciados_ENPOL_unico") %>%
  filter(Delito!="Otro") %>%
  rename(Anio = Anio_arresto)




enpol_3 <- Main_database %>% 
  filter(Anio_arresto>=2017 & Anio_arresto<= 2021 & sentenciado == 1)  %>%
  select(Anio_arresto,Delito_gr_1_robos,Delito_gr_2_drogas,Delito_gr_3_del_org,Delito_gr_4_lesiones,Delito_gr_5_hom_cul,
         Delito_gr_6_hom_dol,Delito_gr_7_armas,Delito_gr_8_viol_fam,Delito_gr_9_secuestro,Delito_gr_10_sexuales,
         Delito_gr_11_extorsion,Delito_gr_12_fraude,Delito_gr_13_amenazas,Delito_gr_14_otro,Delito_gr_15_ns_nr,FAC_PER)  %>% 
  pivot_longer(cols = c(Delito_gr_1_robos,Delito_gr_2_drogas,Delito_gr_3_del_org,Delito_gr_4_lesiones,Delito_gr_5_hom_cul,
                       Delito_gr_6_hom_dol,Delito_gr_7_armas,Delito_gr_8_viol_fam,Delito_gr_9_secuestro,Delito_gr_10_sexuales,
                       Delito_gr_11_extorsion,Delito_gr_12_fraude,Delito_gr_13_amenazas,Delito_gr_14_otro,Delito_gr_15_ns_nr), 
               names_to = "Tipo_de_delito", values_to = "Sentenciados") %>%
  mutate(Sentenciados = Sentenciados*as.numeric(FAC_PER),
         Delito = case_when(Tipo_de_delito== "Delito_gr_6_hom_dol" ~ "Homicidio doloso, ENPOL",
                          Tipo_de_delito== "Delito_gr_5_hom_cul" ~ "Homicidio culposo, ENPOL",
                          T ~ "Otro")) %>%
  group_by(Anio_arresto,Delito) %>%
  summarize(Sentenciados = sum(Sentenciados)) %>%
  mutate(value2plot = Sentenciados ,
         labels = paste0(round(value2plot,0)),
         group_var =  "Sentenciados_ENPOL") %>%
  filter(Delito!="Otro") %>%
  rename(Anio = Anio_arresto)



### Plots

colors4plot <- c("#003B88", "#43a9a7", "#a90099", "#fa4d57")


# Relativos

data2plot <- bind_rows(snsp_,enpol_1)



plt <- ggplot(data2plot, 
              aes(x     = Anio,
                  y     = value2plot,
                  label = labels,
                  group = Delito,
                  color = Delito)) +
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
  expand_limits(y = c(0, 100))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"))



ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Contraste_homicidios_1.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")



data2plot <- bind_rows(snsp_,enpol_2)

plt <- ggplot(data2plot, 
              aes(x     = Anio,
                  y     = value2plot,
                  label = labels,
                  group = Delito,
                  color = Delito)) +
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
  expand_limits(y = c(0, 100))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"))

ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Contraste_homicidios_2.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


#Absolutos

data2plot <- bind_rows(snsp_2,enpol_3)

plt <- ggplot(data2plot, 
              aes(x     = Anio,
                  y     = value2plot,
                  label = labels,
                  group = Delito,
                  color = Delito)) +
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
  expand_limits(y = c(0, 100))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"))

ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Contraste_homicidios_3.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

