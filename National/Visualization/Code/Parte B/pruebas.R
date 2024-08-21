## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Uso excesivo de la autoridad
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##                    Marcelo Torres   ()
##
## Dependencies:      World Justice Project
##
## Creation date:     Junio 16, 2024
##
## This version:      Junio 16, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline: Política criminal - Pruebas aportadas por el Ministerio Público                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
# source("Code/Settings.R")
# 
# load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.2.1. Tipos de pruebas aportadas por el MP en el proceso personas en libertad y en prisión preventiva                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pruebas_pp.fn <- function(
    
  datas.df = master_data.df
  
  ) {

Main_database_2008 <- data.df %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate( prueba_confesion = case_when(P5_15_01 == 1 ~ 1,
                                       as.numeric(P5_35_01) == 1 ~ 1,
                                       P5_15_01 == 0 ~ 0,
                                       as.numeric(P5_35_01) == 2 ~ 0,
                                       T ~ NA_real_),
          prueba_declaraciones = case_when(P5_15_02 == 1  | P5_15_03 == 1 |
                                             P5_15_04 == 1| P5_15_05 == 1 | 
                                             P5_15_06 == 1~ 1,
                                           as.numeric(P5_35_02) == 1 | as.numeric(P5_35_03) == 1 |
                                             as.numeric(P5_35_04) == 1 |as.numeric(P5_35_05) == 1 |
                                             as.numeric(P5_35_06) == 1 ~ 1,
                                           (P5_15_02 == 0 | P5_15_02 == 3) & (P5_15_03 == 0 | P5_15_03 == 3)  &
                                             P5_15_04 == 0 & (P5_15_05 == 0 | P5_15_05 == 3) == 0 & 
                                             P5_15_06 == 0 ~ 0,
                                           (as.numeric(P5_35_02) == 2 | as.numeric(P5_35_02) == 3) & (as.numeric(P5_35_03) == 2 | as.numeric(P5_35_03) == 3) &
                                             as.numeric(P5_35_04) == 2 & (as.numeric(P5_35_05) == 2 | as.numeric(P5_35_05) == 3) &
                                             as.numeric(P5_35_06) == 2 ~ 0,
                                           T ~ NA_real_),
          prueba_fisicas = case_when(P5_15_07 == 1  | P5_15_08 == 1 |
                                       P5_15_09 == 1| P5_15_10 == 1 | 
                                       P5_15_11 == 1~ 1,
                                     as.numeric(P5_35_07) == 1 | as.numeric(P5_35_08) == 1 |
                                       as.numeric(P5_35_09) == 1 |as.numeric(P5_35_10) == 1 |
                                       as.numeric(P5_35_11) == 1 ~ 1,
                                     P5_15_07 == 0  & P5_15_08 == 0 &
                                       P5_15_09 == 0 & P5_15_10 == 0 &
                                       P5_15_11 == 0 ~ 0,
                                     as.numeric(P5_35_07) == 2 & as.numeric(P5_35_08) == 2 &
                                       (as.numeric(P5_35_09) == 2 | as.numeric(P5_35_09) == 3 ) & as.numeric(P5_35_10) == 2 &
                                       as.numeric(P5_35_11) == 2 ~ 0,
                                     T ~ NA_real_),
          tipo_prueba = case_when(prueba_confesion     == 1 ~ "Confesión", 
                                 prueba_declaraciones == 1 ~ "Declaraciones",
                                 prueba_fisicas       == 1 ~ "Física",
                                 prueba_confesion     == 0 & prueba_declaraciones == 0 & prueba_fisicas == 0 ~ "Ninguna",
                                 T ~ NA_character_))


data2plot <- Main_database_2008 %>%
  select(prueba_confesion, prueba_declaraciones, prueba_fisicas) %>% 
  pivot_longer(cols = c(prueba_confesion, prueba_declaraciones, prueba_fisicas), names_to = "tipo_prueba", values_to = "value2plot") %>%
  group_by(tipo_prueba) %>%
  summarise(value2plot = mean(value2plot, na.rm = T)) %>%
  drop_na() %>% 
  rename(values = tipo_prueba) %>% 
  mutate(figure = paste0(round(value2plot*100, 0), "%"),
         # category = str_wrap(category, width = 20),
         labels = str_wrap(values, width = 20), 
         ) %>%
  mutate(
    labels = 
      case_when(
        labels == "prueba_confesion" ~ "Confesiones",
        labels == "prueba_declaraciones" ~ "Declaraciones",
        labels == "prueba_fisicas" ~ "Físicas"
      )
  )


colors4plot <- rep("#2a2a94", 3)


plot <- ggplot(data2plot,
               aes(
                 x     = labels, 
                 y     = value2plot,
                fill  = labels,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 0.05), 
            position = position_dodge(width = 0.9),
            color    = "black",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  coord_flip()+
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0,1,0.20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "right") +
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
    axis.title.x       = element_blank()); plot


ggsave(plot   = plot,
       file   = paste0(path2SP,"/National/Visualization",
                       "/Output/Politica criminal/",
                       savePath,"/Pruebas/Figure3_1.svg"), 
       width  = 189.7883, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

return(data2plot)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.2.2. Tipos de pruebas aportadas por el MP, por tipo de conclusión del proceso                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


pruebas_conclusion.fn <- function(
    
  data.df = master_data.df
  
  ){

Main_database_2008 <- data.df %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate( prueba_confesion = case_when(P5_15_01 == 1 ~ 1,
                                       as.numeric(P5_35_01) == 1 ~ 1,
                                       P5_15_01 == 0 ~ 0,
                                       as.numeric(P5_35_01) == 2 ~ 0,
                                       T ~ NA_real_),
          prueba_declaraciones = case_when(P5_15_02 == 1  | P5_15_03 == 1 |
                                             P5_15_04 == 1| P5_15_05 == 1 | 
                                             P5_15_06 == 1~ 1,
                                           as.numeric(P5_35_02) == 1 | as.numeric(P5_35_03) == 1 |
                                             as.numeric(P5_35_04) == 1 |as.numeric(P5_35_05) == 1 |
                                             as.numeric(P5_35_06) == 1 ~ 1,
                                           P5_15_02 == 0  | P5_15_03 == 0 |
                                             P5_15_04 == 0| P5_15_05 == 0 | 
                                             P5_15_06 == 0~ 0,
                                           as.numeric(P5_35_02) == 2 | as.numeric(P5_35_03) == 2 |
                                             as.numeric(P5_35_04) == 2 |as.numeric(P5_35_05) == 2 |
                                             as.numeric(P5_35_06) == 2 ~ 0,
                                           as.numeric(P5_35_02) == 3 | as.numeric(P5_35_03) == 3 |
                                             as.numeric(P5_35_04) == 3 |as.numeric(P5_35_05) == 3 |
                                             as.numeric(P5_35_06) == 3 ~ 0,
                                           T ~ NA_real_),
          prueba_fisicas = case_when(P5_15_07 == 1  | P5_15_08 == 1 |
                                       P5_15_09 == 1| P5_15_10 == 1 | 
                                       P5_15_11 == 1~ 1,
                                     as.numeric(P5_35_07) == 1 | as.numeric(P5_35_08) == 1 |
                                       as.numeric(P5_35_09) == 1 |as.numeric(P5_35_10) == 1 |
                                       as.numeric(P5_35_11) == 1 ~ 1,
                                     P5_15_07 == 0  | P5_15_08 == 0 |
                                       P5_15_09 == 0| P5_15_10 == 0 | 
                                       P5_15_11 == 0~ 0,
                                     as.numeric(P5_35_07) == 2 | as.numeric(P5_35_08) == 2 |
                                       as.numeric(P5_35_09) == 2 |as.numeric(P5_35_10) == 2 |
                                       as.numeric(P5_35_11) == 2 ~ 0,
                                     as.numeric(P5_35_09) == 3  ~ 0,
                                     T ~ NA_real_),
          tipo_prueba = case_when(prueba_confesion     == 1 ~ "Confesión", 
                                  prueba_declaraciones == 1 ~ "Declaraciones",
                                  prueba_fisicas       == 1 ~ "Física",
                                  prueba_confesion     == 0 & prueba_declaraciones == 0 & prueba_fisicas == 0 ~ "Ninguna",
                                  T ~ NA_character_), 
          juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                       P5_6 == "2" ~ "Procedimiento abreviado o juicio sumario", 
                                       T ~ NA_character_))



data2plot <- Main_database_2008 %>%
  select(juicio_abreviado, prueba_confesion, prueba_declaraciones, prueba_fisicas) %>% 
  pivot_longer(cols = c(prueba_confesion, prueba_declaraciones, prueba_fisicas), names_to = "tipo_prueba", values_to = "value2plot") %>%
  group_by(juicio_abreviado, tipo_prueba) %>%
  summarise(value2plot = mean(value2plot, na.rm = T)) %>%
  drop_na() %>% 
  rename(values = tipo_prueba, 
         category = juicio_abreviado) %>% 
  mutate(figure = paste0(round(value2plot*100, 0), "%"),
         labels = str_wrap(category, width = 20),
         category = str_wrap(values, width = 20)) 


colors4plot <- c("Juicio"     = "#2a2a94",
                 "Procedimiento\nabreviado o juicio\nsumario"        = "#a90099")


plot <- ggplot(data2plot,
               aes(
                 x     = category, 
                 y     = value2plot,
                 fill  = labels,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 0.05), 
            position = position_dodge(width = 0.9),
            color    = "black",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  coord_flip()+
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0,1,0.20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "right") +
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
    axis.title.x       = element_blank()); plot


ggsave(plot   = plot,
       file   = paste0(path2SP,"/National/Visualization",
                       "/Output/Politica criminal/",
                       savePath,"/Pruebas/Figure3_2.svg"), 
       width  = 189.7883, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

return(data2plot)
}

