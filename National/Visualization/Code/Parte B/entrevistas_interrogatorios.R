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
## Outline: Política criminal - Entrevistas e interrogatorios                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
# source("Code/Settings.R")
# 
# load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.5. Personas interrogadas durante la detención y durante la estancia en el MP                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

interrogatorio_MP.fn <- function(
    
  data.df = master_data.df
  
  ){

df <- data.df %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  select(P3_16, 
         P4_3) %>% 
  mutate(P3_16 = case_when(P3_16 == "1" ~ 1,
                           P3_16 == "2" ~ 0,
                           T ~ NA_real_), 
         P4_3 = case_when(P4_3 == "1" ~ 1,
                          P4_3 == "2" ~ 0,
                           T ~ NA_real_))
  

data2plot <- df %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100)) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "P3_16"     ~ "Antes de rendir y firmar su declaración, fue interrogado por las autoridades del MP", 
                       values == "P4_3"   ~ "Desde su detención y hasta antes de llegar a la Agencia del MP o con un Juez, fue interrogado por la policía o autoridad"),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 30),
    order_var = rank(-value2plot))


colors4plot <- rep("#2a2a94", length(data2plot$value2plot))


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = figure,
                  color = labels)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 5 ),
            color    = "#524F4C",
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
        legend.text = element_text(family = "Lato Full"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, 10, 0, 0),
                                 hjust = 0),
        axis.text.x=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, 10, 0, 0),
                                 hjust = 0),
        legend.title = element_blank())+
  coord_flip();plt


ggsave(plot   = plt,
       file   = paste0(path2SP,"/National/Visualization",
                       "/Output/Politica criminal/",
                       savePath,"/Entrevistas/Figure3_1.svg"), 
       width  = 189.7883, 
       height = 65,
       units  = "mm",
       dpi    = 72,
       device = "svg")

return(data2plot)

}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.6. Personas interrogadas en la sede ministerial y el comportamiento de la autoridad                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

interrogatorio_comportamiento.fn <- function(
    
  data.df = master_data.df
  
){


df <- data.df %>%
              filter(Anio_arresto >= 2008,
                     NSJP == 1) %>% 
              mutate(Comp_interrogatorio_1 = case_when(P4_3A_1 == "1" ~ 1,
                                                       P4_3A_1 == "2" ~ 0,
                                                       T ~ NA_real_),
                     Comp_interrogatorio_2 = case_when(P4_3A_2 == "1" ~ 1,
                                                       P4_3A_2 == "2" ~ 0,
                                                       T ~ NA_real_),
                     Comp_interrogatorio_3_4 = case_when(P4_3A_3 == "1" ~ 1,
                                                         P4_3A_4 == "1" ~ 1,
                                                         P4_3A_3 == "2" & P4_3A_4 == "2" ~ 0,
                                                         T ~ NA_real_),
                     Comp_interrogatorio_5_6 = case_when(P4_3A_5 == "1" ~ 1,
                                                         P4_3A_6 == "1" ~ 1,
                                                         P4_3A_5 == "2" & P4_3A_6 == "2" ~ 0,
                                                         T ~ NA_real_),
                     Comp_interrogatorio_7_8 = case_when(P4_3A_7 == 1 ~ 1,
                                                         P4_3A_8 == 1 ~ 1,
                                                         P4_3A_7 == 0 & P4_3A_8 == 0 ~ 0,
                                                         T ~ NA_real_),
                     Comp_interrogatorio_9 = case_when(P4_3A_9 == 1 ~ 1,
                                                       P4_3A_9 == 0 ~ 0,
                                                       T ~ NA_real_)) %>% 
                select(starts_with("Comp_"))


data2plot <- df %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100)) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "Comp_interrogatorio_1"     ~ "Estuvo presente su abogado", 
                       values == "Comp_interrogatorio_2"     ~ "Le explicaron que podía guardar silencio",
                       values == "Comp_interrogatorio_3_4"   ~ "Se realizó registro escrito o grabado",
                       values == "Comp_interrogatorio_5_6"   ~ "Fue engañado para echarse la culpa o a alguien más", 
                       values == "Comp_interrogatorio_7_8"   ~ "Fue golpeado para echarse la culpa o a alguien más", 
                       values == "Comp_interrogatorio_9"     ~ "Se declaró culpable"),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 20),
    order_var = rank(value2plot))



colors4plot <- rep("#2a2a94", length(data2plot$value2plot))


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = figure,
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
        legend.text = element_text(family = "Lato Bold"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(family = "Lato Medium",
                                 size = 3.514598*.pt,
                                 color = "Black", hjust = 0),
        legend.title = element_blank())+
  coord_flip(); plt

ggsave(plot   = plt,
       file   = paste0(path2SP,"/National/Visualization",
                       "/Output/Politica criminal/",
                       savePath,"/Entrevistas/Figure3_2.svg"), 
       width  = 189.7883, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

return(data2plot)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.8.	Prácticas de tortura según el tipo de detención en el traslado y en la estancia del MP            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tortura_detencion_MP.fn <- function(
    
  data.df = master_data.df
  
){


Main_database_2008 <- data.df %>% 
  mutate(tipo_detencion = case_when(flagrancia  == 1  ~ "Flagrancia",
                                    inspeccion  == 1  ~ "Inspección",
                                    orden_det   == 1   ~ "Orden de detención",
                                    det_ninguna == 1 ~ "Detención Irregular"))



data2plot <- Main_database_2008 %>%
  select(tipo_detencion, tortura_lugar) %>% 
  group_by(tipo_detencion, tortura_lugar) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(tipo_detencion) %>% 
  rename(values = tortura_lugar) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         value2plot = case_when(values == "Traslado y Ministerio Público" & 
                                (tipo_detencion == "Orden de detención" |
                                tipo_detencion == "Inspección" |
                                tipo_detencion == "Flagrancia") ~ (value2plot-1),
                                T ~ value2plot)) %>% 
  mutate(
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(values, width = 20),
         values = if_else(values == "Traslado y Ministerio Público",
                          "Traslado y \nMinisterio \nPúblico", 
                          if_else(values == "Ministerio Público",
                                  "Ministerio \nPúblico",
                                  values)
                          ),
         values = factor(values, 
                        levels = c("Ninguno",
                                   "Traslado",
                                   "Ministerio \nPúblico",
                                   "Traslado y \nMinisterio \nPúblico"))) %>% 
  rename(category = tipo_detencion)

colors4plot <- c("Ministerio \nPúblico"              = "#2a2a94",
                 "Traslado"                          = "#a90099",
                 "Traslado y \nMinisterio \nPúblico" = "#3273ff",
                 "Ninguno"                           = "#43a9a7")

colors4plot <- c("Ministerio \nPúblico"              = "#F43152",
                 "Traslado"                          = "#FF7E8A",
                 "Traslado y \nMinisterio \nPúblico" = "#DE003F",
                 "Ninguno"                           = "#43a9a7")

plot <- ggplot(data2plot,
               aes(x    = category,
                   y     = value2plot,
                   label = figure, 
                   fill = values)) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9) +
  geom_text(aes(y    = value2plot + 5),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%"),
                     position = "right") +
  scale_fill_manual(values = colors4plot) +
  coord_flip() +
  facet_grid(rows = vars(values), scales = "free_y", switch = "y", space = "free_y") +
  WJP_theme() + 
  theme(
    panel.spacing = unit(1, "lines"), # Espacio entre facetas
    panel.background   = element_blank(),
    plot.background    = element_blank(),
    panel.grid.major   = element_line(size     = 0.25,
                                      colour   = "#5e5c5a",
                                      linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3")
  ) +
  theme(
    axis.title.y       = element_blank(),
    axis.title.x       = element_blank(),
    axis.ticks         = element_blank(),
    axis.text.y        = element_markdown(family   = "Lato Full",
                                          face     = "bold",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0),
                                          hjust = 0.5, vjust = 0), 
    plot.caption = element_markdown(family   = "Lato Full",
                                    face     = "plain",
                                    size     = 3.514598*.pt,
                                    color    = "#524F4C", 
                                    vjust    = 0, 
                                    hjust    = 0, 
                                    margin = margin(20, 0, 0, 0)),
    axis.text.x        = element_markdown(family   = "Lato Full",
                                          face     = "bold",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
    ),
    plot.title          = element_text(family   = "Lato Full",
                                       face     = "bold",
                                       size     = 3.514598*.pt,
                                       color    = "black",
                                       margin   = margin(0, 0, 10, 0),
                                       hjust    = 0), 
    plot.subtitle      = element_text(family   = "Lato Full",
                                      face     = "plain",
                                      size     = 3.514598*.pt,
                                      color    = "black",
                                      margin   = margin(2.5, 0, 20, 0),
                                      hjust    = 0),
    legend.text        =  element_markdown(family   = "Lato Full",
                                           face     = "plain",
                                           size     = 3.514598*.pt,
                                           color    = "#524F4C"),
    legend.title       = element_markdown(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
    strip.placement = "outside",
    strip.text = element_text(
      family   = "Lato Full",
      face     = "bold",
      size     = 3.514598*.pt,
      color    = "black", 
      hjust = 0.5,
      margin = margin(0, 0, 0, 0)
    )
  );plot

ggsave(plot = plot, 
       filename = paste0(path2SP,"/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Entrevistas/",
                         "Figure3_3.svg"),
       width = 189.7883,
       height = 189.7883,
       units  = "mm",
       dpi    = 72,
       device = "svg")

return(data2plot)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.9. Prácticas de tortura según la autoidentificación de inocencia o de culpabilidad             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tortura_inocencia.fn <- function(
    
  data.df = master_data.df
  
){


  data_subset.df <- data.df %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1) %>% 
  mutate(declaro_culpable = case_when(culpabilidad == 1 ~ "Se autoidentifica como culpable", 
                                      culpabilidad == 0 ~ "Se autoidentifica como inocente",
                                      T ~ NA_character_))

data2plot <- data_subset.df %>%
  select(declaro_culpable, tortura_lugar) %>% 
  group_by(declaro_culpable, tortura_lugar) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(declaro_culpable) %>% 
  rename(values = tortura_lugar, 
         category = declaro_culpable) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(values, width = 20), 
         category = str_wrap(category, width = 20)) %>%
  mutate(
    values = 
      factor(values, levels = c("Traslado y Ministerio Público", 
                               "Ministerio Público", 
                               "Traslado",
                               "Ninguno"))
  ) 
  # %>%
  # mutate(figure = case_when(labels == "Traslado y\nMinisterio Público" &  category == "Se autoidentifica\ncomo inocente" ~ "52%",
  #                           T ~ figure)) # redondeos



colors4plot <- c("Ministerio Público"            = "#2a2a94",
                 "Traslado"                      = "#a90099",
                 "Traslado y Ministerio Público" = "#3273ff",
                 "Ninguno"                       = "#43a9a7")

colors4plot <- c("Ministerio Público"                = "#F43152",
                 "Traslado"                          = "#FF7E8A",
                 "Traslado y Ministerio Público"     = "#DE003F",
                 "Ninguno"                           = "#43a9a7")

plot <- ggplot(data2plot,
               aes(
                 x     = category,
                 y     = value2plot,
                 fill  = values,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "stack")+
  geom_text(aes(y    = value2plot), 
            position = position_stack(vjust = 0.5),
            color    = "white",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  coord_flip()+
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0,100,20),
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
    axis.title.x       = element_blank());plot


ggsave(plot   = plot,
       file   = paste0(path2SP,"/National/Visualization",
                       "/Output/Politica criminal/",
                       savePath,"/Entrevistas/Figure3_4.svg"), 
       width  = 189.7883, 
       height = 65,
       units  = "mm",
       dpi    = 72,
       device = "svg")

ggsave(plot   = plot,
       file   = paste0(path2SP,"/National/Visualization",
                       "/Output/Politica criminal/",
                       savePath,"/Entrevistas/Figure3_4_B.svg"), 
       width  = 189.7883, 
       height = 65,
       units  = "mm",
       dpi    = 72,
       device = "svg")

return(data2plot)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.10. Prácticas de tortura durante la estancia en el MP y su reflejo en las declaraciones de culpabilidad, 
## para quienes se autoidentifican como inocentes y como culpables            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


tortura_culpabilidad.fn <- function(
    
  data.df = master_data.df
  
){

Main_database_2008 <- data.df %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(tortura_mp = case_when(tortura_mp == 1 ~ "Tortura en el\nMinisterio Público", 
                                tortura_mp == 0 ~ "No tortura en\nel Ministerio Público",
                                      T ~ NA_character_), 
         identifica_culpable = case_when(culpabilidad == 1 ~ "Se autoidentifica como culpable", 
                                      culpabilidad == 0 ~ "Se autoidentifica como inocente",
                                      T ~ NA_character_),
         declaro_culpable = case_when(
           P4_6_4 == 1 ~ 1, 
           T ~ 0)
         )

data2plot <- Main_database_2008 %>%
  select(tortura_mp, identifica_culpable, declaro_culpable) %>% 
  group_by(tortura_mp, identifica_culpable) %>%
  drop_na() %>% 
  summarise(Percentage = mean(declaro_culpable, na.rm = T)) %>% 
  group_by(identifica_culpable) %>% 
  rename(category = identifica_culpable, 
         values = tortura_mp) %>% 
  mutate(value2plot = Percentage * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(category, width = 20), 
         category = str_wrap(values, width = 20)) 


colors4plot <- c("Tortura en el\nMinisterio Público"        = "#FA4D57",
                 "No tortura en el\nMinisterio Público"     = "#009AA9")

plot <- ggplot(data2plot,
               aes(
                 x     = labels, 
                 y     = value2plot,
                 fill  = category,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 10), 
            position = position_dodge(width = 0.5),
            color    = "black",
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
    axis.title.x       = element_blank()); plot

ggsave(plot   = plot,
       file   = paste0(path2SP,"/National/Visualization",
                       "/Output/Politica criminal/",
                       savePath,"/Entrevistas/Figure3_5.svg"), 
       width  = 189.7883, 
       height = 65,
       units  = "mm",
       dpi    = 72,
       device = "svg")

return(data2plot)

}

