## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Percepcion proceso justo
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Abril 24, 2024
##
## This version:      Abril 24, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading data
load(paste0(path2SP,
            "/National/Data_cleaning/Output/Main_database.RData"))

master_data.df <- Main_database %>% 
  filter(Anio_arresto >= as.numeric(2008)) %>% 
  filter(NSJP == 1)                   

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Funciones                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


BarSimpleChartViz <- function(data = data2plot, 
                              x_var = main_var, 
                              y_var = value2plot, 
                              label_var = figure, 
                              fill_var = main_var,
                              order_var = order_var,
                              labels = labels,
                              shade_xminvalue,
                              shade_xmaxvalue,
                              x_labels = NULL  # Añadido argumento para etiquetas personalizadas
) {
  plt <- ggplot(data, 
                aes(x     = reorder({{x_var}},{{order_var}}),
                    y     = {{y_var}},
                    label = {{label_var}},
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9) +
    scale_fill_gradient(low = "#2a2a9A", high = "#2a2a9A") +
    annotate('rect', xmin=0, xmax= shade_xminvalue, ymin=0, ymax=100, alpha=.1, fill="#fa4d57") +
    annotate('rect', xmin=shade_xmaxvalue, xmax= shade_xmaxvalue+1, ymin=0, ymax=100, alpha=.1, fill="#43a9a7") +
    geom_text(aes(y    = {{y_var}} + 10),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    xlab("Porcentaje de criterios cumplidos") +
    ylab("Porcentaje de personas sentenciadas") +
    scale_y_continuous() +
    scale_x_discrete(labels = x_labels) +  # Añadido para cambiar etiquetas del eje X
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_text(color    = "#4a4a49",
                                            family   = "Lato Full"),
          axis.title.x       = element_text(color    = "#4a4a49",
                                            family   = "Lato Full"),
          axis.text.y        = element_text(hjust = 0))
  
  return(plt)
}

index_setUp.fn <- function(data = master_data.df,
                           main_var){
  
  data_subset.df <- data %>%
    rename(main_var = all_of(main_var)) %>%
    group_by(main_var) %>%
    summarise(counter = n()) %>%
    drop_na %>%
    mutate(
      value2plot = counter / sum(counter),
      value2plot = value2plot*100,
      figure = paste0(round(value2plot,0), "%"),
      order_var = rank(main_var),
      labelx =paste0(round(main_var*100,0), "%")
    )
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Indicador general barras                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_general")


etiquetas <- data2plot$labelx

plot <- BarSimpleChartViz(shade_xminvalue = 6, shade_xmaxvalue = 10, x_labels = etiquetas); plot


ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso","/Indice DP",
                         "/figure1.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Logit positivo indicador general                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data2plot <- logit_dataBase.fn(dependent_var = "indicator_general_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#43a9a7")
logitPlot

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Logit negativo indicador general                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data2plot <- logit_dataBase.fn(dependent_var = "indicator_general_minlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#fa4d57")
logitPlot
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Indicador general por estado                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Proceso justo barras                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_PJ")

etiquetas <- data2plot$labelx

plot <- BarSimpleChartViz(shade_xminvalue = NA, shade_xmaxvalue = NA, x_labels = etiquetas); plot
ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso","/Indice DP",
                         "/figure2.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Uso excesivo barras                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_UAA")

etiquetas <- data2plot$labelx

plot <- BarSimpleChartViz(shade_xminvalue = NA, shade_xmaxvalue = NA, x_labels = etiquetas); plot

ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso","/Indice DP",
                         "/figure3.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Tortura barras                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_GDH")


etiquetas <- data2plot$labelx

plot <- BarSimpleChartViz(shade_xminvalue = NA, shade_xmaxvalue = NA, x_labels = etiquetas); plot

ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso","/Indice DP",
                         "/figure4.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

