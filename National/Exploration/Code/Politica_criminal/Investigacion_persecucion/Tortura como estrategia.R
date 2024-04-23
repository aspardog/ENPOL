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
## Creation date:     April 23th, 2024
##
## This version:      April 23th, 2024
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

#Replace wjp theme function because it doesn't work on my computer

WJP_theme <- function() {
theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x = element_text(
                                   size   = 3.514598*.pt,
                                   color  = "#524F4C"),
        axis.ticks  = element_blank(),
        plot.margin  = unit(c(0, 0, 0, 0), "points")
  ) 
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Prepare prioritary crimes variables                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Main_database1 <- Main_database %>%
  filter(Anio_arresto >= 2011, Delito_unico == 1)  %>%
  mutate(tortura_lugar = case_when(
    (tortura_mp_p == 1 | tortura_mp_f == 1) & (tortura_tra_p == 0 & tortura_tra_f == 0) ~  "Ministerio Público",
    (tortura_tra_p == 1 | tortura_tra_f == 1) & (tortura_mp_p == 0 & tortura_mp_f == 0) ~  "Traslado",
    (tortura_tra_p == 1 | tortura_tra_f == 1) & (tortura_mp_p == 1 | tortura_mp_f == 1) ~  "Traslado y Ministerio Público",
    (tortura_tra_p == 0 & tortura_tra_f == 0) & (tortura_mp_p == 0 & tortura_mp_f == 0) ~  "Ninguno",
    T ~ NA))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Plots                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Se tortura más a los no culpables
data2plot <- Main_database1  %>%
  group_by(culpabilidad,tortura_lugar) %>%
  summarise(n = n()) %>%
  filter(!is.na(culpabilidad)) %>%
  mutate(culpa = case_when(culpabilidad == 1 ~ "Se reconoce como culpable",
                           culpabilidad == 0 ~ "No se reconoce como culpable"),
         value2plot = prop.table(n) * 100,
         labels = paste0(round(value2plot,0), "%"),
         group_var = culpa,
         group_var2 = tortura_lugar) %>%
  select(value2plot,labels,group_var,group_var2) 


colors4plot <- c("#1a2689",
                 "#a90099",
                 "#ef4b4b",
                 "#fa4d57",
                 "#1a2689",
                 "#a90099",
                 "#ef4b4b",
                 "#fa4d57")

plt <- ggplot(data2plot, 
              aes(x     = group_var,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = group_var)) +
  geom_bar(position = "stack", stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot -4  ),
            color    = "#4a4a49",
            position = "stack") +
  labs(y = "% torturado") +
  scale_y_discrete() +
  scale_x_discrete( ) +
  expand_limits(y = c(0, 100)) +
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10)) +
  coord_flip()




ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Tortura/Figure1.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")





# La tortura es una estrategia o mecanismo para conseguir declaraciones como pruebas inculpatoriass

data2plot <- Main_database1  %>%
  group_by(tortura_lugar, culpabilidad) %>%
  summarise(declaro_culpa = mean(declaro_culpable, na.rm=T)) %>%
  mutate(value2plot =  100 * declaro_culpa,
         labels = paste0(round(value2plot,0), "%"),
         group_var = tortura_lugar,
         culpa = case_when(culpabilidad == 1 ~ "Se reconoce como culpable",
                           culpabilidad == 0 ~ "No se reconoce como culpable"),
         group_var2 = culpa) %>%
  select(value2plot,labels,group_var,group_var2) %>%
  filter(group_var2 == "No se reconoce como culpable")




colors4plot <- c("#1a2689",
                 "#a90099",
                 "#ef4b4b",
                 "#fa4d57")

plt <- ggplot(data2plot, 
              aes(x     = group_var,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = group_var)) +
  geom_bar(position = "dodge", stat = "identity", fill = colors4plot, color = colors4plot, width = 0.9, show.legend = T)+
  scale_fill_manual(name = "Lugar de tortura", values = colors4plot) +
  geom_text(aes(y    = value2plot + 7  ),
            color    = "#4a4a49",
            position = position_dodge(width = .9)) +
  labs(y = "% que se declaró culpable en el MP, entre quienes no se reconocen culpables") +
  scale_y_discrete() +
  scale_x_discrete( ) +
  expand_limits(y = c(0, 100)) +
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10),
        legend.position = "right") +
  coord_flip()




ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Tortura/Figure2.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# Tortura VS PPO

data2plot <- Main_database1  %>%
  group_by(tortura_lugar) %>%
  summarise(PPO = mean(PPO, na.rm=T)) %>%
  mutate(value2plot =  100 * PPO,
         labels = paste0(round(value2plot,0), "%"),
         group_var = tortura_lugar) %>%
  select(value2plot,labels,group_var) %>%
  filter(!is.na(group_var))


colors4plot <- c("#1a2689",
                 "#a90099",
                 "#ef4b4b",
                 "#fa4d57")

plt <- ggplot(data2plot, 
              aes(x     = group_var,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = group_var)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 7  ),
            color    = "#4a4a49") +
  labs(y = "% que están en PPO") +
  scale_y_discrete() +
  scale_x_discrete( ) +
  expand_limits(y = c(0, 100)) +
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10)) +
  coord_flip()




ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Tortura/Figure3.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")



