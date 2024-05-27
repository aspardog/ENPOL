BarSimpleChartViz <- function(data = data2plot, 
                              x_var = main_var, 
                              y_var = value2plot, 
                              label_var = figure, 
                              fill_var = main_var,
                              order_var = order_var,
                              labels = labels,
                              shade_xminvalue,
                              shade_xmaxvalue
) {
  plt <- ggplot(data, 
                aes(x     = reorder({{x_var}},{{order_var}}),
                    y     = {{y_var}},
                    label = {{label_var}},
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9) +
    scale_fill_gradient(low = "#756ef9", high = "#b1a6ff") +
    #geom_vline(xintercept = c("0", "100"), linetype = 3, color = c("#fa4d57", "#43a9a7")) +
    annotate('rect', xmin=0, xmax= shade_xminvalue, ymin=0, ymax=60, alpha=.1, fill="#fa4d57")+
    annotate('rect', xmin=shade_xmaxvalue, xmax= shade_xmaxvalue+1, ymin=0, ymax=60, alpha=.1, fill="#43a9a7")+
    geom_text(aes(y    = {{y_var}} + 10),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    xlab("Porcentaje de criterios cumplidos")+
    scale_y_continuous() +
    scale_x_discrete(
      limits = as.character(seq(0, 1, 0.10)),
      labels = paste0(seq(0, 100, 10), "%")
                       )+
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_blank(),
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
      order_var = rank(main_var)
    )
  
}

# Indicador General

data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_general")


plot <- BarSimpleChartViz(shade_xminvalue = 6, shade_xmaxvalue = 10)

ggsave(plot   = plot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice DP/figure1.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Proceso Justo

data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_PJ")


plot <- BarSimpleChartViz(shade_xminvalue = 6, shade_xmaxvalue = 10)

ggsave(plot   = plot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice DP/figure2.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Uso no arbitrario de la autoridad

data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_UAA")


plot <- BarSimpleChartViz(shade_xminvalue = 6, shade_xmaxvalue = 10)

ggsave(plot   = plot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice DP/figure3.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Proteccion derechos humanos

data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_GDH")


plot <- BarSimpleChartViz(shade_xminvalue = 2, shade_xmaxvalue = 10)

ggsave(plot   = plot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice DP/figure4.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

