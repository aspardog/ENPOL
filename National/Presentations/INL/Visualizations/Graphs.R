## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2024 - Graphs 
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    D. Cristina Álvarez Venzor     (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 21th, 2023
##
## This version:      January 21th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Load Settings                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("National/Data_cleaning/Code/settings.R")


load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Visualizaciones                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### a) Tortura

datos_porcentaje <- Main_database %>%
  group_by(Anio_arresto) %>%
  filter(Anio_arresto >= 2000) %>% 
  summarize(
    torturados_culpables = mean(tortura_generalizada == 1 & culpabilidad == 1, na.rm = T) * 100,
    torturados_no_culpables = mean(tortura_generalizada == 1 & culpabilidad == 0, na.rm = T) * 100,
    no_torturados_culpables = mean(tortura_generalizada == 0 & culpabilidad == 1, na.rm = T) * 100,
    no_torturados_no_culpables = mean(tortura_generalizada == 0 & culpabilidad == 0, na.rm = T) * 100
  )

datos_porcentaje <- datos_porcentaje %>% pivot_longer(
  cols = !Anio_arresto, 
  names_to = "condicion", 
  values_to = "porcentaje"
)

library(ggtext)
library(showtext)
font_add_google("Lato", family = "Lato")

colores <- c(torturados_culpables = "#3273ff", torturados_no_culpables =  "#2a2a9A",no_torturados_culpables = "#2c6d4f" , no_torturados_no_culpables = "#43a9a7")

p <- ggplot(datos_porcentaje, aes(x = Anio_arresto , y= porcentaje, group=condicion, color=condicion)) +
  geom_line() +
  scale_colour_manual (values = colores) +
  labs(title = "Percentage by torture and culpability condition",
       y = "",
       x = "",
       color = "Torture and culpability condition") +
  theme(
    panel.background   = element_blank(),
    plot.background    = element_blank(),
    panel.grid.major   = element_line(size     = 0.25,
                                      colour   = "#5e5c5a",
                                      linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3")) +
  theme(
    axis.title.y       = element_blank(),
    axis.title.x       = element_blank(),
    axis.ticks         = element_blank())

ggsave(paste0(path2DB,"/National/Presentations/INL/Visualizations/tortura.png"), plot = p, width = 10, height = 6, units = "in")



