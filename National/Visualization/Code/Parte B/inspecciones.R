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
## Outline: Política criminal - Inspecciones                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/Settings.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.4.	Inspecciones y el comportamiento de las corporaciones al realizarlas                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1) %>% 
  select(P3_12_1, 
         P3_12_2,
         P3_12_3,
         P3_12_4,
         P3_12_5) 

data2plot <- data_subset.df %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
  drop_na() %>% 
  group_by(Column) %>% 
  summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100)) %>% 
  rename(values = Column, 
         value2plot = Percentage) %>% 
  mutate(
    labels = case_when(values == "P3_12_1" ~ "Lo desvistió", 
                       values == "P3_12_2" ~ "Le dijo qué objeto buscaba",
                       values == "P3_12_3" ~ "Encontró el objeto ilegal",
                       values == "P3_12_4" ~ "Le sembró algún objeto",
                       values == "P3_12_5" ~ "Videograbó la inspección",),
    figure = paste0(round(value2plot, 0), "%"),
    labels = str_wrap(labels, width = 20),
    order_var = rank(value2plot))


colors4plot <- rep("#2a2a94", 5)


plt <- ggplot(data2plot, 
              aes(x     = reorder(labels, order_var),
                  y     = value2plot,
                  label = figure,
                  color = labels)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 5 ),
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
       file   = paste0(path2SP,"National/Report/prueba/Capitulo 2/charts_and_images/inspecciones/Figure3_1.svg"), 
       width  = 189.7883, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.5.	Inspecciones en las que se encontró el objeto relacionado con el delito y no se le sembró, distribuidas por tipo de delito    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
                             values == "hom_dol" ~ "Homicidio doloso",), 
         values = str_wrap(values, width = 20))

colors4plot <- rep("#2a2a94", 7)

plt <- ggplot(data2plot, 
              aes(x     = reorder(values, -value2plot),
                  y     = value2plot,
                  label = figure)) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, 
           fill  = colors4plot, ) +
  #scale_fill_manual(values = "#2a2a94" ) +
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
        plot.title = element_text(face = "bold", size = 12)); plt

ggsave(plot   = plt,
       file   = paste0(path2SP,"National/Report/prueba/Capitulo 2/charts_and_images/inspecciones/Figure3_2.svg"), 
       width  = 189.7883, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

