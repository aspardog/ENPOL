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

#funcion con n_obs

index_setUp_edos.fn <- function(data = master_data.df,
                                main_var){
  
  data_subset.df <- data %>%
    rename(main_var = all_of(main_var)) %>%
    group_by(main_var) %>%
    summarise(counter = n()) %>%
    drop_na %>%
    mutate(
      n_obs = sum(counter),
      value2plot = counter / sum(counter),
      value2plot = value2plot*100,
      figure = paste0(round(value2plot,0), "%, N =", n_obs),
      order_var = rank(main_var),
      labelx =paste0(round(main_var*100,0), "%")
    )  
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Indicador general barras                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

indicador_general.fn <- function(
    
  data.df = master_data.df
  
){
  
  data2plot <- index_setUp_edos.fn(data = data.df,
                              main_var ="indicator_general") 
  
  
  etiquetas <- data2plot$labelx
  

  plot <- BarSimpleChartViz(data = data2plot, 
                            shade_xminvalue = 5, 
                            shade_xmaxvalue = NA_real_, 
                            x_labels = etiquetas) +
    annotate('rect', xmin=9, xmax= 11, ymin=0, ymax=100, alpha=.1, fill="#43a9a7") ; plot
  
  
  ggsave(plot = plot, 
         filename = paste0(
           path2SP,
           "/National/Graphic_reports",
           "/Output/", savePath, "/Debido proceso","/Indicador DP",
           "/indicador_DP.svg"), 
         width  = 200, 
         height = 65,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

indicador_general_infografias.fn <- function(
    data.df = master_data.df %>%
      filter(Anio_arresto >= as.numeric(2018))
){
  # Get the unique values of the 'Estado_arresto' variable
  estados <- unique(data.df$Estado_arresto)
  
  # Loop over each unique state
  for (estado in estados) {
    
    # Filter the data for the current state
    data_state <- data.df %>%
      filter(Estado_arresto == estado)
    
    # Check if there is data to plot; if not, skip to the next iteration
    if (nrow(data_state) == 0) next
    
    # Prepare the data for plotting using the filtered data
    data2plot <- index_setUp_edos.fn(data = data_state,
                                main_var = "indicator_general")
    
    # Extract labels for plotting
    etiquetas <- data2plot$labelx
    
    # Create the bar plot
    plot <- BarSimpleChartViz(data = data2plot, 
                              shade_xminvalue = NA_real_, 
                              shade_xmaxvalue = NA_real_, 
                              x_labels = etiquetas) 
    
    # Save the plot with a filename that includes the state name
    ggsave(plot = plot, 
           filename = paste0(
             path2SP,
             "/National/Graphic_reports",
             "/Output/", savePath, "/Debido proceso", "/Indicador infografia/",
             "/indicador_DP_", estado, ".svg"),  # Include the state name in the filename
           width  = 200, 
           height = 65,
           units  = "mm",
           dpi    = 72,
           device = "svg")
    
    # Optionally, print a message to indicate the plot has been saved
    message(paste("Plot saved for state:", estado))
  }
  
  # Return the last processed data for checking purposes
  return(data2plot)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Logit positivo indicador general                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

indicador_logit_positivo.fn <- function(
    
  data.df = master_data.df
  
){
  
  data2plot <- logit_dataBase.fn(
    data = data.df,
    dependent_var = "indicator_general_maxlimit"
  )
  
  logitPlot <- logit_demo_panel(mainData = data2plot, 
                                shadow = T, 
                                shadow_color = "#43a9a7")
  
  ggsave(plot   = logitPlot,
         filename = paste0(
           path2SP,
           "/National/Graphic_reports",
           "/Output/", savePath, "/Debido proceso","/Indicador DP",
           "/logit_positivo.svg"), 
         width  = 110, 
         height = 80,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Logit negativo indicador general                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

indicador_logit_negativo.fn <- function(
    
  data.df = master_data.df
  
){
  
  data2plot <- logit_dataBase.fn(
    data = data.df,
    dependent_var = "indicator_general_minlimit"
    )
  
  logitPlot <- logit_demo_panel(mainData = data2plot, 
                                shadow = T, 
                                shadow_color = "#fa4d57")
  ggsave(plot   = logitPlot,
         filename = paste0(
           path2SP,
           "/National/Graphic_reports",
           "/Output/", savePath, "/Debido proceso","/Indicador DP",
           "/logit_negativo.svg"), 
         width  = 110, 
         height = 80,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Indicador general por estado                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

indicador_map.fn <- function(
    data.df = master_data.df
){
  
  Estados <- data.df %>%
    group_by(Estado_arresto) %>%
    summarise(indice = mean(indicator_general, na.rm = T)) %>%
    drop_na() %>%
    rename(ESTADO = Estado_arresto) %>%
    mutate(
      ESTADO = 
        case_when(
          ESTADO == "Coahuila de Zaragoza" ~ "Coahuila",
          ESTADO == "Michoacán de Ocampo"  ~ "Michoacán",
          ESTADO == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
          ESTADO == "México" ~ "Estado de México",
          ESTADO == "Distrito Federal" ~ "Ciudad de México",
          TRUE ~ ESTADO
        )) 
  
  promedio_nacional <- data.df %>%
    ungroup() %>%
    summarise(indice = mean(indicator_general, na.rm = T)) %>%
    drop_na() %>%
    pull(indice)
  
  Estados <-  Estados %>% 
              add_row(
               ESTADO          = "ANacional",
              indice      = promedio_nacional)
  
  quintiles <- round(quantile(round((Estados$indice * 100), 0), probs = seq(0, 1, by = 0.2)), 0)
  
  table <- Estados %>%
    mutate(
      ` ` = "",
      `%` = round(indice * 100, 0)
    ) %>%
    arrange(ESTADO) %>%
    mutate(
      ESTADO = 
        case_when(
          ESTADO == "ANacional" ~ "Promedio Nacional",
          T ~ ESTADO
        )) %>% 
    select(
      Estado = ESTADO, ` `, `%`
    ) %>%
    flextable() %>%
    theme_zebra(
      odd_header = "transparent",
      odd_body   = "#e2e0df"
    ) %>%
    
    padding(j = 1, padding.right = 30) %>%
    padding(j = 1, padding.left  = 10) %>%
    padding(j = 3, padding.left  = 10) %>%
    
    width(j = " ", width = 0.5, unit = "mm") %>%
    width(j = "%", width = 0.75, unit = "mm") %>%
    
    bg(i = ~ `%` >= 61 & `%` <= 70, j = ' ', bg = "#99D7DD", part = "body") %>%
    bg(i = ~ `%` > 70 & `%` <= 72, j = ' ', bg = "#33AEBA", part = "body") %>%
    bg(i = ~ `%` > 72 & `%` <= 74, j = ' ', bg = "#0087A3", part = "body") %>%
    bg(i = ~ `%` > 74 & `%` <= 75, j = ' ', bg = "#00617F", part = "body") %>%
    bg(i = ~ `%` > 75, j = ' ', bg = "#004E70", part = "body") %>%
    
    align(j = 2, align = "center", part = "all") %>%
    bold(bold = FALSE, part = "header") %>%
    flextable::style(pr_t = fp_text(font.size = 12, color = "#524F4C", font.family = "Lato Full"), part = "header") %>%
    flextable::style(pr_t = fp_text(font.size = 10, color = "#524F4C", font.family = "Lato Full"), part = "body") %>%
    italic(italic = TRUE, part = "header") %>%
    surround(j = 2, border.top = fp_border("white"), border.bottom = fp_border("white"), part = "body") %>%
    bold(i = ~ Estado == "Promedio Nacional", bold = TRUE, part = "body")
  
  tpanel <- gen_grob(table, fit = "auto", scaling = "min", just = c("left", "top"), wrapping = T)
  
  mexico_map <- mapa %>%
    mutate(ESTADO = case_when(ESTADO == "Distrito Federal" ~ "Ciudad de México", TRUE ~ ESTADO)) %>%
    left_join(Estados, by = "ESTADO") %>%
    mutate(value2plot = round(indice * 100, 0)) %>%
    mutate(
      color_group = case_when(
        value2plot >= 61 & value2plot <= 70 ~ "(61%-70%]",
        value2plot > 70 & value2plot <= 72 ~ "(70%-72%]",
        value2plot > 72 & value2plot <= 74 ~ "(72%-74%]",
        value2plot > 74 & value2plot <= 75 ~ "(74%-75%]",
        value2plot > 75 ~ "(75%-85%]"
      ),
      color_group = as.factor(color_group)
    )
  
  cat_palette <- c(
    "(61%-70%]" = "#99D7DD",
    "(70%-72%]" = "#33AEBA",
    "(72%-74%]" = "#0087A3",
    "(74%-75%]" = "#00617F",
    "(75%-85%]" = "#004E70"
  )
  
  # Drawing plot
  p <- ggplot(mexico_map, aes(label = ESTADO)) +
    geom_sf(data = mexico_map, aes(fill = color_group), color = "grey65", size = 0.5) +
    geom_sf(data = mexico_map, fill = NA, color = "grey25") +
    scale_fill_manual("", values = cat_palette, na.value = "grey95", drop = F) +
    theme_minimal() +
    theme(
      plot.background = element_blank(),
      axis.text = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Leyenda
  categories <- c("(61%-70%]", "(70%-72%]", "(72%-74%]", "(74%-75%]", "(75%-85%]")
  
  leyend <- data.frame(
    Values = categories,
    Blank = ""
  )
  leyend <- flextable(leyend) %>%
    width(j = "Blank", width = 0.5, unit = "mm") %>%
    set_header_labels(Values = "Escala", Blank = " ") %>%
    bg(i = ~ Values == "(61%-70%]", j = "Blank", bg = "#99D7DD", part = "body") %>%
    bg(i = ~ Values == "(70%-72%]", j = "Blank", bg = "#33AEBA", part = "body") %>%
    bg(i = ~ Values == "(72%-74%]", j = "Blank", bg = "#0087A3", part = "body") %>%
    bg(i = ~ Values == "(74%-75%]", j = "Blank", bg = "#00617F", part = "body") %>%
    bg(i = ~ Values == "(75%-85%]", j = "Blank", bg = "#004E70", part = "body") %>%
    
    align(j = 2, align = "center", part = "all") %>%
    bold(bold = FALSE, part = "header") %>%
    flextable::style(pr_t = fp_text(font.size = 12, color = "#524F4C", font.family = "Lato Full"), part = "header") %>%
    flextable::style(pr_t = fp_text(font.size = 10, color = "#524F4C", font.family = "Lato Full"), part = "body") %>%
    italic(italic = TRUE, part = "header") %>%
    surround(j = c(1, 2), border.top = fp_border("white"), border.bottom = fp_border("white"), part = "body")
  
  leyend <- gen_grob(leyend, fit = "auto", scaling = "min", just = c("left", "top"), wrapping = T)
  
  layout <- "
  ABB
  A#C
  "
  
  viz <- wrap_elements(tpanel) + p + wrap_elements(leyend) +
    plot_layout(ncol = 3, nrow = 3, widths = c(1, 3.25, 0.4), heights = c(1, .2, 0.25), design = layout)
  plot(viz)
  
  ggsave(plot = viz, 
         filename = paste0(
           path2SP,
           "/National/Graphic_reports",
           "/Output/", savePath, "/Debido proceso", "/Indicador DP",
           "/mapa_indicador.svg"), 
         width = 189.7883, 
         height = 175,
         units = "mm",
         dpi = 72,
         device = "svg")
  
  # return(mexico_map)
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Proceso justo barras                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

indicador_proceso_justo.fn <- function(
    
  data.df = master_data.df
  
){
  
  data2plot <- index_setUp_edos.fn(data = master_data.df,
                              main_var ="indicator_PJ")
  
  etiquetas <- data2plot$labelx
  
  plot <- BarSimpleChartViz(data = data2plot, 
                            shade_xminvalue = 0, 
                            shade_xmaxvalue = 0, 
                            x_labels = etiquetas)
  
  
  ggsave(plot = plot, 
         filename = paste0(
           path2SP,
           "/National/Graphic_reports",
           "/Output/", savePath, "/Debido proceso","/Indicador DP",
           "/indicador_PJ.svg"), 
         width  = 200, 
         height = 50,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Uso excesivo barras                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

indicador_uso_fuerza.fn <- function(
    
  data.df = master_data.df
  
){
  
  data2plot <- index_setUp_edos.fn(data = master_data.df,
                              main_var ="indicator_UAA")
  
  etiquetas <- data2plot$labelx
  
  plot <- BarSimpleChartViz(data = data2plot, 
                            shade_xminvalue = 0, 
                            shade_xmaxvalue = 0, 
                            x_labels = etiquetas)
  
  ggsave(plot = plot, 
         filename = paste0(
           path2SP,
           "/National/Graphic_reports",
           "/Output/", savePath, "/Debido proceso","/Indicador DP",
           "/indicador_UEF.svg"), 
         width  = 200, 
         height = 50,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Tortura barras                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

indicador_tortura.fn <- function(
    
  data.df = master_data.df
  
){
  
  data2plot <- index_setUp_edos.fn(data = master_data.df,
                              main_var ="indicator_GDH")
  
  
  etiquetas <- data2plot$labelx
  
  plot <- BarSimpleChartViz(data = data2plot, 
                            shade_xminvalue = 0, 
                            shade_xmaxvalue = 0, 
                            x_labels = etiquetas)
  
  ggsave(plot = plot, 
         filename = paste0(
           path2SP,
           "/National/Graphic_reports",
           "/Output/", savePath, "/Debido proceso","/Indicador DP",
           "/indicador_PDH.svg"), 
         width  = 200, 
         height = 60,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

