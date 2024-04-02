## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration data & hipothesys
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres 
##
## Dependencies:      World Justice Project
##
## Creation date:     February 05th, 2024
##
## This version:      February 05th, 2024
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
source("Code/Settings.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)


# functions ---------------------------------------------------------------

BarSimpleChartViz <- function(data = data2plot, 
                              x_var = labels, 
                              y_var = value2plot, 
                              label_var = figure, 
                              fill_var = Value,
                              fill_colors = fill_colors,
                              order_var = order_var,
                              labels = labels, 
                              Frequency = Frequency
) {
  plt <- ggplot(data, 
                aes(x     = {{x_var}}, 
                    y     = {{y_var}},
                    label = paste0({{label_var}}, "\n", "N =", {{Frequency}}),
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9) +
    scale_fill_manual(values = {{fill_colors}}) +
    geom_text(aes(y    = {{y_var}} + 7),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    #xlab("Porcentaje de criterios cumplidos")+
    scale_y_discrete(breaks = NULL) +
    scale_x_discrete( ) +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_blank(),
          axis.title.x       = element_text(color    = "#4a4a49",
                                            family   = "Lato Full"),
          axis.text.y        = element_text(hjust = 0))
  
  return(plt)
}



count_frequency.fn <- function(column) {
  # Convert the column to a data frame
  data <- data.frame(Value = column)
  
  # Count the frequency of each unique value
  frequency_df <- data %>%
    group_by(Value) %>%
    summarise(Frequency = n()) %>% 
    mutate(Value = Value,
           values = Frequency/sum(Frequency),
           value2plot = values * 100,
           figure = paste0(round(value2plot, 0), "%"),
           labels = str_wrap(Value, width = 10)) 
  frequency_df <- frequency_df %>% mutate(order_var = rank(Value))
  return(frequency_df)
}



# graphs categóricas------------------------------------------------------------------


# Sexo --------------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$Sexo)
barChart <- BarSimpleChartViz( fill_colors = c("#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart


# Grado escolar -----------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$Escolaridad)
barChart <- BarSimpleChartViz( fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                               "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                               "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                               "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7") ) + expand_limits(y = c(0, 110))
barChart


# Estado civil ------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$P1_4)
barChart <- BarSimpleChartViz( fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

# Estado civil ------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$Estado_arresto)
barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + coord_flip()
barChart