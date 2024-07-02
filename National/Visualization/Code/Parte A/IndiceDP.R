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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Indicador general barras                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

indicador_general.fn <- function(
    
  data.df = master_data.df
  
){
  
  data2plot <- index_setUp.fn(data = data.df,
                              main_var ="indicator_general")
  
  
  etiquetas <- data2plot$labelx
  
  plot <- BarSimpleChartViz(shade_xminvalue = 6, 
                            shade_xmaxvalue = 10, 
                            x_labels = etiquetas);plot
  
  
  ggsave(plot = plot, 
         filename = paste0(
           path2SP,
           "/National/Visualization",
           "/Output/Debido proceso/",
           savePath,"/Indicador DP",
           "/indicador_DP.svg"), 
         width  = 200, 
         height = 80,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
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
           "/National/Visualization",
           "/Output/Debido proceso/",
           savePath,"/Indicador DP",
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
           "/National/Visualization",
           "/Output/Debido proceso/",
           savePath,"/Indicador DP",
           "/logit_negativo.svg"), 
         width  = 110, 
         height = 80,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
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
          T ~ ESTADO
        ))
  
  quintiles <- round(quantile(round((Estados$indice*100), 0), probs = seq(0, 1, by = 0.2)),0)
  
  table <- Estados %>%
    mutate(
      ` ` = "",
      `%` = round(indice*100, 0)
    ) %>%
    arrange(ESTADO) %>%
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
    width(j = "%", width = 0.75,   unit = "mm") %>%
    
    bg(i = ~ `%` >= quintiles[1] & `%` <= quintiles[2], j = ' ', bg = "#99D7DD", part = "body") %>%
    bg(i = ~ `%` > quintiles[2] & `%` <= quintiles[3], j = ' ', bg = "#33AEBA", part = "body") %>%
    bg(i = ~ `%` > quintiles[3] & `%` <= quintiles[4], j = ' ', bg = "#0087A3", part = "body") %>%
    bg(i = ~ `%` > quintiles[4] & `%` <= quintiles[5], j = ' ', bg = "#00617F", part = "body") %>%
    bg(i = ~ `%` > quintiles[5] & `%` <= quintiles[6], j = ' ', bg = "#004E70", part = "body") %>%
    
    
    align(j     = 2, 
          align = "center", 
          part  = "all") %>%
    bold(bold = FALSE, 
         part = "header") %>%
    flextable::style(pr_t = fp_text(font.size   = 12, 
                                    color       = "#524F4C",
                                    font.family = "Lato Full"), 
                     part = "header") %>%
    flextable::style(pr_t = fp_text(font.size   = 10, 
                                    color       = "#524F4C",
                                    font.family = "Lato Full"), 
                     part = "body") %>%
    italic(italic = TRUE, 
           part = "header") %>%
    surround(j = 2,
             border.top    = fp_border("white"),
             border.bottom = fp_border("white"),
             part = "body"
    )
  
  tpanel <- gen_grob(table, 
                     fit      = "auto",
                     scaling  = "min", 
                     just     = c("left", "top"),
                     wrapping = T)
  
  mexico_map <- mapa %>%
    left_join(Estados, by = "ESTADO") %>%
    mutate(value2plot = round(indice*100, 0)) %>%
    mutate(
      color_group = case_when(
        value2plot >= quintiles[1] & value2plot <= quintiles[2] ~ "(61%-70%]",
        value2plot > quintiles[2] & value2plot <= quintiles[3] ~ "(70%-72%]",
        value2plot > quintiles[3] & value2plot <= quintiles[4] ~ "(72%-74%]",
        value2plot > quintiles[4] & value2plot <= quintiles[5] ~ "(74%-75%]",
        value2plot > quintiles[5] & value2plot <= quintiles[6] ~ "(75%-85%]"
      ),
      color_group = as.factor(color_group)
    )
  
  cat_palette <- c("(61%-70%]"  = "#99D7DD",
                   "(70%-72%]"  = "#33AEBA",
                   "(72%-74%]"  = "#0087A3",
                   "(74%-75%]"  = "#00617f",
                   "(75%-85%]"  = "#004E70")
  # Drawing plot
  p <- ggplot(mexico_map, aes(label = ESTADO)) +
    geom_sf(data  = mexico_map,
            aes(fill = color_group),
            color = "grey65",
            size  = 0.5) +
    geom_sf(data  = mexico_map,
            fill  = NA,
            color = "grey25") +
    scale_fill_manual("",
                      values   = cat_palette,
                      na.value = "grey95",
                      drop = F) +
    # scale_y_continuous(limits = c(1445631, 5273487)) +
    # scale_x_continuous(limits = c(2581570, 5967160)) +
    theme_minimal() +
    theme(
      plot.background = element_blank(),
      axis.text       = element_blank(),
      legend.position = "none",
      panel.grid      = element_blank(),
      panel.border    = element_blank(),
      plot.margin     = margin(0,0,0,0)
    ) 
  
  
  #leyenda
  
  categories <- c("(61%-70%]",
                  "(70%-72%]",
                  "(72%-74%]",
                  "(74%-75%]",
                  "(75%-85%]")
  
  leyend <- data.frame(
    Values = categories,
    Blank = "")
  leyend <- flextable(leyend)  %>% 
    width(j = "Blank", width = 0.5, unit = "mm") %>% 
    set_header_labels(Values = "Escala", Blank = " ") %>% 
    bg(i = ~ Values == "(61%-70%]", j = "Blank", bg = "#99D7DD", part = "body") %>%
    bg(i = ~ Values == "(70%-72%]", j = "Blank", bg = "#33AEBA", part = "body") %>%
    bg(i = ~ Values == "(72%-74%]", j = "Blank", bg = "#0087A3", part = "body") %>%
    bg(i = ~ Values == "(74%-75%]", j = "Blank", bg = "#00617f", part = "body") %>%
    bg(i = ~ Values == "(75%-85%]", j = "Blank", bg = "#004E70", part = "body") %>%
    
    
    align(j     = 2, 
          align = "center", 
          part  = "all") %>%
    bold(bold = FALSE, 
         part = "header") %>%
    flextable::style(pr_t = fp_text(font.size   = 12, 
                                    color       = "#524F4C",
                                    font.family = "Lato Full"), 
                     part = "header") %>%
    flextable::style(pr_t = fp_text(font.size   = 10, 
                                    color       = "#524F4C",
                                    font.family = "Lato Full"), 
                     part = "body") %>%
    italic(italic = TRUE, 
           part = "header") %>%
    surround(j = c(1,2),
             border.top    = fp_border("white"),
             border.bottom = fp_border("white"),
             part = "body"
    )
  
  
  leyend <- gen_grob(leyend, 
                     fit      = "auto",
                     scaling  = "min", 
                     just     = c("left", "top"),
                     wrapping = T)
  
  layout <- "ABB
           A#C"
  
  viz <- wrap_elements(tpanel) + p + wrap_elements(leyend) +
    plot_layout(ncol = 3, nrow = 3, widths = c(1, 3.25,0.4), heights = c(1,.2,0.25), design = layout)
  plot(viz)
  
  ggsave(plot = plot, 
         filename = paste0(
           path2SP,
           "/National/Visualization",
           "/Output/Debido proceso/",
           savePath,"/Indicador DP",
           "/mapa_indicador.svg"), 
         width  = 189.7883, 
         height = 175,
         units  = "mm",
         dpi    = 72,
         device = "svg") 
  
  return(mexico_map)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Proceso justo barras                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

indicador_proceso_justo.fn <- function(
    
  data.df = master_data.df
  
){
  
  data2plot <- index_setUp.fn(data = master_data.df,
                              main_var ="indicator_PJ")
  
  etiquetas <- data2plot$labelx
  
  plot <- BarSimpleChartViz(shade_xminvalue = NA, shade_xmaxvalue = NA, x_labels = etiquetas); plot
  ggsave(plot = plot, 
         filename = paste0(
           path2SP,
           "/National/Visualization",
           "/Output/Debido proceso/",
           savePath,"/Indicador DP",
           "/indicador_PJ.svg"), 
         width  = 200, 
         height = 80,
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
  
  data2plot <- index_setUp.fn(data = master_data.df,
                              main_var ="indicator_UAA")
  
  etiquetas <- data2plot$labelx
  
  plot <- BarSimpleChartViz(shade_xminvalue = NA, shade_xmaxvalue = NA, x_labels = etiquetas); plot
  
  ggsave(plot = plot, 
         filename = paste0(
           path2SP,
           "/National/Visualization",
           "/Output/Debido proceso/",
           savePath,"/Indicador DP",
           "/indicador_UEF.svg"), 
         width  = 200, 
         height = 80,
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
  
  data2plot <- index_setUp.fn(data = master_data.df,
                              main_var ="indicator_GDH")
  
  
  etiquetas <- data2plot$labelx
  
  plot <- BarSimpleChartViz(shade_xminvalue = NA, shade_xmaxvalue = NA, x_labels = etiquetas); plot
  
  ggsave(plot = plot, 
         filename = paste0(
           path2SP,
           "/National/Visualization",
           "/Output/Debido proceso/",
           savePath,"/Indicador DP",
           "/indicador_PDH.svg"), 
         width  = 200, 
         height = 80,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

