## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - RunMe File
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Abril 2, 2024
##
## This version:      Abril 2, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Criterios mínimos                                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


data_subset.df <- master_data.df %>% 
                  filter(sentenciado = 1)

data2plot <- count_frequency.fn(data_subset.df$indicator_general)

barChart <- BarSimpleChartViz(shade_xminvalue = 5, shade_xmaxvalue = 9)
barChart
