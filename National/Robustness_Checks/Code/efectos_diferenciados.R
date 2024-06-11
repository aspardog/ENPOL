## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Prueba de robustez: Efectos diferenciados
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

het_effects.fn <- function(
    data = master_data.df,
    mainVar
) {
  
  plots.ls <- list()
  
  for (mainVar in mainVar) {
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##
    ## 1.  Defining mainVar                                                                                 ----
    ##
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # `mainVar` is already defined in the loop
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##
    ## 2.  Set the datasets.                                                                                  ----
    ##
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    data_subset.df <- data %>%
      filter(ENPOL == 2021) %>%
      rename(mainVar = all_of(mainVar))
    
    data_subset.df_Estado <- data_subset.df %>%
      group_by(Estado_arresto) %>%
      summarise(mainVar = 
                  mean(as.numeric(mainVar), na.rm = T) * 100
      ) %>%
      drop_na() %>%
      ungroup() %>%
      mutate(
        national_mean = mean(mainVar, na.rm = T),
        Q1  = quantile(mainVar, 0.25),
        IQR = IQR(mainVar),
        Q3  = quantile(mainVar, 0.75),
        lower_bound = Q1 - 1.5 * IQR,
        upper_bound = Q3 + 1.5 * IQR,
        outliers = if_else(mainVar < lower_bound | mainVar > upper_bound, "Outliers", "Regular"),
        difference = mainVar - national_mean
      )
    
    Outliers_Estado <- data_subset.df_Estado %>%
      filter(outliers == "Outliers") %>%
      pull(Estado_arresto) 
    
    data_subset.df_Delito <- data_subset.df %>%
      group_by(Delito_unico_categ) %>%
      summarise(mainVar = 
                  mean(as.numeric(mainVar), na.rm = T) * 100
      ) %>%
      drop_na() %>%
      ungroup() %>%
      mutate(
        national_mean = mean(mainVar, na.rm = T),
        Q1  = quantile(mainVar, 0.25),
        IQR = IQR(mainVar),
        Q3  = quantile(mainVar, 0.75),
        lower_bound = Q1 - 1.5 * IQR,
        upper_bound = Q3 + 1.5 * IQR,
        outliers = if_else(mainVar < lower_bound | mainVar > upper_bound, "Outliers", "Regular"),
        difference = mainVar - national_mean
      )
    
    Outliers_Delito <- data_subset.df_Delito %>%
      filter(outliers == "Outliers") %>%
      pull(Delito_unico_categ) 
    
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ##
    ## 3.  Set the model                                                                                 ----
    ##
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    model <- lm("mainVar ~ Estado_arresto + Delito_unico_categ", 
                data =     data_subset.df)
    model_summary <- tidy(model)
    
    Estados <- model_summary %>%
      mutate(
        Estado = if_else(str_detect(term, "Estado"), 1, 0),
        significancia = if_else(p.value < 0.05, 1, 0)
      ) %>%
      filter(Estado == 1 & significancia == 1) %>%
      pull(term)
    
    Delitos <- model_summary %>%
      mutate(
        Delito = if_else(str_detect(term, "Delito"), 1, 0),
        significancia = if_else(p.value < 0.05, 1, 0)
      ) %>%
      filter(Delito == 1 & significancia == 1) %>%
      pull(term)

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 4. Plot both models                                                                               ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  colors4plot <- c("Outliers" = "#ef4b4b",
                   "Regular" = "#2a2a9A")
  
  p1 <- ggplot(data_subset.df_Estado, 
               aes(x = mainVar,
                   y = difference,
                   color = outliers,
                   label = Estado_arresto
               )
  ) + 
    geom_vline(xintercept = data_subset.df_Estado$lower_bound, lty = 1, color = "black", lwd = 0.5) +
    geom_vline(xintercept = data_subset.df_Estado$upper_bound, lty = 1, color = "black", lwd = 0.5) +
    geom_vline(xintercept = data_subset.df_Estado$national_mean, lty = 2, color = "red", lwd = 0.5) +
    geom_hline(yintercept = 0, lty = 2, color = "red", lwd = 0.5) +
    geom_point(size = 2.5,
               show.legend = T) +
    geom_text(
      family   = "Lato Full",
      fontface = "bold", size = 2.5,
      vjust = -1, show.legend = F
    ) + 
    labs(subtitle = paste0("<b>",
                           length(Outliers_Estado), "</b>",
                           " son los Estados que son outliers respecto a la distribución nacional son <br>",
                           "<b>",length(Estados),"</b>",
                           " son los Estados cuyo promedio es estadisticamente diferente a la nacional <br>",
                           "controlando por delito y Estado"
         ),
         title = "Estados",
         y = "Diferencias respecto al promedio nacional"
         ) +
    scale_x_continuous(limits = c(0,100),
                       breaks = seq(0, 100, 20),
                       labels = c("0%","20%", "40%", "60%", "80%", "100%")) +
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(legend.key = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          axis.line        = element_line(color    = "#5e5c5a", linetype = "solid"),
          legend.text = element_text(size = 8), #change legend text font size
          panel.grid.major.y = element_line(size = 0.5, 
                                            colour = "grey75", 
                                            linetype = "dotted"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(family="Lato Black", 
                                    size = 12, 
                                    color = "Black"),
          plot.subtitle = element_markdown(family="Lato Full", 
                                           size = 10, 
                                           color = "Black"),
          axis.text     = element_text(family = "Lato Full",
                                       face     = "plain",
                                       size     = 10,
                                       color    = "Black"),
          axis.title.x = element_blank(),
          axis.title     = element_text(family = "Lato Full",
                                        face     = "plain",
                                        size     = 10,
                                        color    = "Black"),
          plot.caption = element_text(family = "Lato Full",
                                      face     = "plain",
                                      size     = 8,
                                      color    = "Black", hjust = 0));p1
  
  p2 <- ggplot(data_subset.df_Delito, 
               aes(x = mainVar,
                   y = difference,
                   color = outliers,
                   label = Delito_unico_categ
               )
  ) + 
    geom_vline(xintercept = data_subset.df_Delito$lower_bound, lty = 1, color = "black", lwd = 0.5) +
    geom_vline(xintercept = data_subset.df_Delito$upper_bound, lty = 1, color = "black", lwd = 0.5) +
    geom_vline(xintercept = data_subset.df_Delito$national_mean, lty = 2, color = "red", lwd = 0.5) +
    geom_hline(yintercept = 0, lty = 2, color = "red", lwd = 0.5) +
    geom_point(size = 2.5,
               show.legend = T) +
    geom_text(
      family   = "Lato Full",
      fontface = "bold", size = 2.5,
      vjust = -1, show.legend = F
    ) + 
    labs(subtitle = paste0("<b>",
                           length(Outliers_Delito), "</b>",
                           " son los delitos que son outliers respecto a la distribución nacional son <br>",
                           "<b>",length(Delitos),"</b>",
                           " son los delitos cuyo promedio es estadisticamente diferente a la nacional <br>",
                           "controlando por delito y Estado"
         ),
         title = "Delitos",
         x = "Proporción",
         y = "Diferencias respecto al promedio nacional"
         ) +
    scale_x_continuous(limits = c(0,100),
                       breaks = seq(0, 100, 20),
                       labels = c("0%","20%", "40%", "60%", "80%", "100%")) +
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(legend.key = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          axis.line        = element_line(color    = "#5e5c5a", linetype = "solid"),
          legend.text = element_text(size = 8), #change legend text font size
          panel.grid.major.y = element_line(size = 0.5, 
                                            colour = "grey75", 
                                            linetype = "dotted"),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(family="Lato Black", 
                                    size = 12, 
                                    color = "Black",
                                    margin = margin(10,0,0,0)),
          plot.subtitle = element_markdown(family="Lato Full", 
                                           size = 10, 
                                           color = "Black",
                                           margin = margin(5,0,0,0)),
          axis.text     = element_text(family = "Lato Full",
                                       face     = "plain",
                                       size     = 10,
                                       color    = "Black"),
          axis.title     = element_text(family = "Lato Full",
                                        face     = "plain",
                                        size     = 10,
                                        color    = "Black"),
          plot.caption = element_text(family = "Lato Full",
                                      face     = "plain",
                                      size     = 8,
                                      color    = "Black", hjust = 0));p2
  
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ##
  ## 5. Merge models                                                                             ----
  ##
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  # Combine the plots and the legend using patchwork
  combined_plot <- p1 / p2 + plot_layout(guides = 'collect', nrow = 2)  + 
    plot_annotation(title = paste0("<b>",mainVar, "</b>"), 
                    caption = "Nota: Las líneas negras representan los umbrales para determinar los outliers respecto a su medida intercuartil. <br>Las líneas rojas representan el promedio nacional.",
                    theme = 
                      theme(
                        plot.title = element_markdown(family="Lato Black", 
                                                      size = 12, 
                                                      color = "Black", hjust = 0.5),
                        plot.caption = element_markdown(family = "Lato Full",
                                                    face     = "plain",
                                                    size     = 8,
                                                    color    = "Black", 
                                                    hjust = 0),
                        legend.position = "top"
                      )
                    )
  
  plots.ls[[mainVar]] <- combined_plot
  
  
  }
  print(plots.ls)
}
