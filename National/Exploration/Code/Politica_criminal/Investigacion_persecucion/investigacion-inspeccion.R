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
## Creation date:     Mayo 20th, 2024
##
## This version:      Mayo  20th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:     Detonadores de la investigación: Detención                                                                                            ----
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



# Encontró objeto sin sembrar po delito -----------------------------------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  mutate(encontro_sinsembrar = case_when(P3_12_3  == 1 & P3_12_4  == 0 ~ 1,
                                         P3_12_3  == 1 & P3_12_4  == 1 ~ 0,
                                         P3_12_3  == 0 & P3_12_4  == 0 ~ 0,
                                         P3_12_3  == 0 & P3_12_4  == 1 ~ 0,
                                         P3_12_3  == 0  ~ 0,
                                         P3_12_4  == 1 ~ 0,
                                    T ~ NA_real_))


data2plot <- data_subset.df %>%
  select(Delito_unico_categ, encontro_sinsembrar) %>% 
  group_by(Delito_unico_categ, encontro_sinsembrar) %>%
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Delito_unico_categ) %>% 
  rename( values = Delito_unico_categ ) %>%
  mutate(value2plot = Frequency / sum(Frequency) * 100,
         figure = paste0(round(value2plot, 0), "%")) %>% 
  filter(encontro_sinsembrar == 1, 
         Frequency >= 10) %>% 
  mutate(order = rank(-value2plot, ties.method = "first"),
         values = case_when( values == "armas" ~ "Posesión ilegal de armas", 
                             values == "drogas" ~ "Posesión y comercio \n de drogas",
                             values == "otro" ~ "Otro delito distinto",
                             values == "org" ~ "Delincuencia organizada",
                             values == "robos" ~ "Robos",
                             values == "secuestro" ~ "Secuestro",
                             values == "hom_dol" ~ "Homicidio doloso",))


plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -value2plot),
                  y     = value2plot,
                  fill  = encontro_sinsembrar,
                  label = figure)) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9 ) +
  # scale_fill_manual(values = "#2a2a94" ) +
  geom_text(aes(y    =  value2plot + 5),
             color    = "#4a4a49",
             family   = "Lato Full",
             fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0, 100, 20),
                     labels = paste0(seq(0, 100, 20), "%"),
                     position = "right") +
  scale_x_discrete(limits = rev) +
  coord_flip() +
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10),
        plot.title = element_text(face = "bold", size = 12))

plt



# Comportamiento de las corporaciones -------------------------------------

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)


actividad <- c("P3_12_1", 
             "P3_12_2",
             "P3_12_3",
             "P3_12_4",
             "P3_12_5")

labels <- c("lo(a) desvistió",
            "le dijo qué objeto buscaba",
            "encontró el objeto ilegal",
            "le sembró algún objeto",
            "videograbó la inspección") 


data2plot <- set_data.fn(data_subset.df, actividad, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = rep("#2a2a94", 7),
                              title = "Comportamiento de las corporaciones en la detención por inspección")
barChart
