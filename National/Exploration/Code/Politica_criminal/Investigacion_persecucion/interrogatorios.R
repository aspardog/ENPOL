# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration descriptives
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvarez         (calvarez@worldjuticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 10th, 2024
##
## This version:      June 10th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:     Interrogatorios                                                                                       ----
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


# Interrogatorios por coorporación aprehensora y MP -----------------------

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)


interrogatorio <- c("P4_3A_1", 
                    "P4_3A_2",
                    "P4_3A_3",
                    "P4_3A_4",
                    "P4_3A_5", 
                    "P4_3A_6",
                    "P4_3A_7",
                    "P4_3A_8",
                    "P4_3A_9")

labels <- c("estuvo presente su abogado",
            "le explicaron que podía guardar silencio y no responder",
            "se realizó un registro escrito de lo que le preguntaron y lo que usted respondió",
            "se realizó una grabación de audio o video de lo que le preguntaron y usted respondió",
            "fue engañado(a) para echarse la culpa o aceptar hechos falsos",
            "fue engañado(a) para inculpar a alguien más",
            "usted fue golpeado(a) o maltratado(a) para echarse la culpa o aceptar hechos falsos",
            "usted fue golpeado(a) o maltratado(a) para inculpar a alguien más",
            "usted se declaró culpable") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, interrogatorio) 


data2plot <- set_data.fn(Main_database_2008, interrogatorio, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = rep("#E2E2F7", 15),
                              title = "Interrogatorio")
barChart



# Interrogatorios y comportamiento de la autoridad ----------------------------------
# Descriptivos de 3.4.a (1, 2, (3 y 4), (5 y 6), (7 y 8) , 9). 

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)


interrogatorio <- c("P4_3A_1", 
                     "P4_3A_2",
                     "P4_3A_3",
                     "P4_3A_4",
                     "P4_3A_5", 
                     "P4_3A_6",
                     "P4_3A_7",
                     "P4_3A_8",
                     "P4_3A_9")

labels <- c("estuvo presente su abogado",
            "le explicaron que podía guardar silencio y no responder",
            "se realizó un registro escrito de lo que le preguntaron y lo que usted respondió",
            "se realizó una grabación de audio o video de lo que le preguntaron y usted respondió",
            "fue engañado(a) para echarse la culpa o aceptar hechos falsos",
            "fue engañado(a) para inculpar a alguien más",
            "usted fue golpeado(a) o maltratado(a) para echarse la culpa o aceptar hechos falsos",
            "usted fue golpeado(a) o maltratado(a) para inculpar a alguien más",
            "usted se declaró culpable") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, interrogatorio) 


data2plot <- set_data.fn(Main_database_2008, interrogatorio, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = rep("#E2E2F7", 15),
                              title = "Interrogatorio")
barChart



# tortura tipo por corporación aprehensora --------------------------------

Main_database_2008 <- Main_database %>% 
                      filter(Anio_arresto >= 2008,
                             NSJP == 1,
                             Corporacion_grupos !=  "NS/NR") %>%
                      mutate(tortura_p = case_when(tortura_tra_p == 1 | tortura_mp_p == 1 ~ 1, 
                                                   tortura_tra_p == 0 & tortura_mp_p == 0 ~ 0,
                                                   T ~ NA_real_),
                             tortura_f = case_when(tortura_tra_f == 1 | tortura_mp_f == 1 ~ 1, 
                                                   tortura_tra_p == 0 & tortura_mp_p == 0 ~ 0,
                                                   T ~ NA_real_))


data2table <- data_subset.df %>%
  group_by(Corporacion_grupos) %>%
  summarise(
    tortura_p = mean(indicator_general, na.rm = T),
    `Sub-Índice de protección de derechos humanos` = mean(indicator_GDH, na.rm = T),
    `Sub-Índice de uso no arbitrario de la autoridad` = mean(indicator_UAA, na.rm = T),
    `Sub-Índice de proceso justo` = mean(indicator_PJ, na.rm = T)
  ) %>% 
  drop_na() %>%
  mutate(
    proceso_justo = 
      case_when(
        proceso_justo == 1 ~ "Proceso justo",
        proceso_justo == 0 ~ "Proceso injusto"
      )
  ) %>%
  pivot_longer(cols = !proceso_justo, names_to = "category", values_to = "value2plot") %>%
  rbind(data.frame(category = c("— — — — — — — — — — — — — — — — — — — — — —", 
                                "— — — — — — — — — — — — — — — — — — — — — —"),
                   value2plot = c(NA_real_, NA_real_),
                   proceso_justo = c("Proceso justo", "Proceso injusto"))
  ) %>%
  mutate(
    order_value =
      case_when(
        category == "Índice 13 criterios mínimos" ~ 1,
        category == "Sub-Índice de uso no arbitrario de la autoridad" ~ 4,
        category == "Sub-Índice de protección de derechos humanos" ~ 5,
        category == "Sub-Índice de proceso justo" ~ 3,
        category == "— — — — — — — — — — — — — — — — — — — — — —" ~ 2
      )
  )

justo.df <- data2table %>%
  filter(proceso_justo == "Proceso justo")

injusto.df <- data2table %>%
  filter(proceso_justo == "Proceso injusto")

colors4plot <-  c("#2a2a9A","#ef4b4b")
names(colors4plot) <- c("Proceso justo",
                        "Proceso injusto")

p <- ggplot(data2table,
            aes(x = value2plot,
                y = reorder(category, -order_value))) +
  geom_segment(data = justo.df,
               aes(x = value2plot, y = reorder(category, -order_value),
                   yend = reorder(injusto.df$category, -order_value),
                   xend = injusto.df$value2plot), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  geom_hline(yintercept = 4, linetype = "longdash", color = "black", size = 0.25) +
  geom_point(aes(x = value2plot, y = category, color = proceso_justo), size = 4, show.legend = F)  +
  geom_text(aes(x = value2plot, y = category, 
                label = paste0(round(value2plot*100,0),"%"), 
                family = "Lato Full", fontface = "bold"), 
            size= 3.514598, color = "black", vjust = -1) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colors4plot) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1),limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1), position = "top")+
  WJP_theme() +
  theme(legend.position="bottom",
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
                                 color = "Black", hjust = 0));p

























data2plot <- Main_database_2008 %>%
  select(Corporacion_grupos, tortura_tipo) %>% 
  group_by(Corporacion_grupos, tortura_tipo) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Corporacion_grupos) %>% 
  mutate(values = Corporacion_grupos,
         value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%"),
         labels = str_wrap(Corporacion_grupos, width = 20),
         order_var = case_when(values == "Ejército o Marina"  ~ 7, 
                               values == "Guardia Nacional"   ~ 6,
                               values == "Operativo Conjunto" ~ 1,
                               values == "Otra"               ~ 9,
                               values == "Policía Estatal"    ~ 2,
                               values == "Policía Estatal Ministerial o Judicial" ~ 8,
                               values == "Policía Federal"     ~ 5,
                               values == "Policía Federal Ministerial"     ~4,
                               values == "Policía Municipal"   ~ 3,
                               T ~ NA_real_),
         values = case_when(labels == "Policía Federal Ministerial"    ~ "Policía Federal\n Ministerial", 
                            labels == "Policía Estatal Ministerial o Judicial"   ~ "Policía Estatal\n Ministerial o Judicial",
                            T ~ labels))



plot <- ggplot(data2plot,
               aes(
                 x     = reorder(values, -order_var), 
                 y     = value2plot,
                 fill  = tortura_tipo,
                 label = paste0(figure)
               )) +
  geom_bar(stat = "identity", width = 0.9, position = "stack")+
  geom_text(aes(y    = value2plot-3.5 ), 
            position = "stack",
            color    = "white",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  # geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values =  c("#43a9a7", "#2e2e95","#ff003d", "#20204a")) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "left") +
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
    axis.title.x       = element_blank(),
    legend.position      = "bottom",
    legend.title = element_blank())

plot



