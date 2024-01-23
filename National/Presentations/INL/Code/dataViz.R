# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Data Visualization
##
## Author(s):         A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Álvarez Venzor     (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 7th, 2023
##
## This version:      June 7th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Logit Chart                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

logit_demo_panel <- function(mainData = data2plot,
                             line_color = "#003b8a",
                             line_size  = 2,
                             point_color = "#003b8a",
                             point_size   = 4) {
  
  plot <- ggplot(mainData, aes(x = reorder(factor, -order_variable), y = AME)) +
    geom_hline(yintercept = 0, lty = 1, color = "#fa4d57", lwd = 1)  +
    geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),
                   lwd = line_size, position = position_dodge(width = .7), 
                   stat = "identity", color = line_color)+
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = point_size, position = position_dodge(width = .7), color = point_color) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = 2, position = position_dodge(width = .7), color = "white") +
    labs(y = "Menos probable                               Más probable") +
    scale_y_continuous(limits = c(-0.25, 0.25),
                       breaks = seq(-0.25, 0.25, by = 0.125),
                       expand = expansion(mult = 0.025), position = "right",
                       labels = c("-25", "-12.5", "0", "+12.5", "+25"))+
    WJP_theme()+
    coord_flip() +
    theme(legend.position = "none",
          panel.background   = element_blank(),
          panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                            size = 0.5, linetype = "dashed"),
          legend.title = element_blank(),
          axis.title.y       = element_blank(),
          axis.text.y        = element_text(family = "Lato Medium",
                                            size     = 3.514598*.pt,
                                            color    = "#4a4a49",
                                            hjust    = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.grid.minor.y = element_blank())
  
  return(plot)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Line Chart                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lineChartViz <- function(data = data2plot,
                         period = period,
                         order_value = order_value,
                         category = category,
                         labels = labels,
                         colors = category) {
  # Creating ggplot
  plt <- ggplot(data2plot, 
                aes(x     = reorder(period, order_value),
                    y     = value2plot,
                    color = category,
                    label = labels,
                    group = category)) +
    geom_point(size = 2,
               show.legend = F) +
    geom_line(size  = 1,
              show.legend = F) +
    geom_text_repel(family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0,0),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) +
    scale_x_discrete("period",
                     labels = c("Implementación" = "Implementación",
                     "Un año" = " ",
                     "Dos años" = "Dos años",
                     "Tres años" = " ",
                     "Cuatro años" = "Cuatro años",
                     "Cinco años" = " ",
                     "Seis años" = "Seis años",
                     "Siete años" = " ",
                     "Ocho años" = "Ocho años",
                     "Nueve años" = " ",
                     "Diez años" = "Diez años",
                     "Once años" = " ",
                     "Doce años" = "Doce años")
    ) +
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid")
    )
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Bar Chart                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

barsChart.fn <- function(
    data.df                    = data2plot,
    labels_var                 = "labels",
    value2plot                 = "value2plot",
    grouping_var               = "group_var",
    categories_grouping_var    = categories,
    label_figures              = "figure",
    order                      = T,
    order_value                = "order_values",
    nbars                      = 6,
    colors4plot                = colors4plot
) {
  
  data2plot <- data.df %>%
    dplyr::rename(
      value2plot       = all_of(value2plot),
      grouping_var     = all_of(grouping_var),
      labels           = all_of(labels_var),
      label_figures    = all_of(label_figures),
      order_var        = any_of(order_value),
    )
  
  if(order == T) {
    
    plot <- ggplot(data2plot,
                   aes(
                     x     = reorder(labels, -order_var),
                     y     = value2plot,
                     fill  = grouping_var,
                     label = label_figures
                   ))
  } else {
    
    plot <- ggplot(data2plot,
                   aes(
                     x     = labels,
                     y     = value2plot,
                     fill  = grouping_var,
                     label = label_figures
                   ))
  }
  
  plot <- plot +
    geom_bar(stat = "identity",
             show.legend = T,
             position = position_dodge(widt = 0.9))
  if(nbars > 1){
    
    plot <- plot +
      geom_vline(xintercept = seq(1.5, nbars - 0.5, by = 1), linetype = "dashed", color = "black")
    
  } else {
    plot <- plot
  }
  plot <- plot +
    geom_text(aes(y    = value2plot + 5), 
              position = position_dodge(widt = 0.9),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold", 
              size = 3.514598)  +
    geom_vline(xintercept = 2.5, linetype = "dashed", color = "black") +
    scale_fill_manual(values = colors4plot, breaks = categories_grouping_var) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%"),
                       position = "right") +
    coord_flip() +
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
      axis.ticks         = element_blank(),
      axis.text.y        = element_markdown(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C",
                                            margin   = margin(0, 10, 0, 0),
                                            hjust = 0), 
      plot.title          = element_text(family   = "Lato Full",
                                         face     = "bold",
                                         size     = 4.920437*.pt,
                                         color    = "black",
                                         margin   = margin(0, 0, 10, 0),
                                         hjust    = 0), 
      plot.subtitle      = element_text(family   = "Lato Full",
                                        face     = "italic",
                                        size     = 4.217518*.pt,
                                        color    = "black",
                                        margin   = margin(2.5, 0, 20, 0),
                                        hjust    = 0)
    )
  
  return(plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Bar Simple Chart                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

BarSimpleChartViz <- function(data = data2plot, 
                              x_var = category, 
                              y_var = value2plot, 
                              label_var = figure, 
                              fill_var = category, 
                              fill_colors = fill_colors) {
  plt <- ggplot(data, 
                aes(x     = {{x_var}},
                    y     = {{y_var}},
                    label = {{label_var}},
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9) +
    scale_fill_manual(values = {{fill_colors}}) +
    geom_text(aes(y    = {{y_var}} + 10),
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
          axis.text.y        = element_text(hjust = 0))
  
  return(plt)
}