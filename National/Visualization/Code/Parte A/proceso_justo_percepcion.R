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
## 1. Proceso justo en el tiempo                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

proceso_justo.fn <- function(
  
  data.df = master_data.df
    
) {
  
  data_subset.df <- data.df %>%
    filter(sentenciado == 1) %>%
    mutate(
      counter = 1,
      proceso_justo = 
        case_when(
          as.numeric(P5_26A) == 1 ~ "Proceso justo",
          as.numeric(P5_26A) == 0 ~ "Proceso injusto",
          T ~ NA_character_
        )) %>%
    ungroup() %>%
    group_by(proceso_justo) %>%
    summarise(
      value2plot = sum(counter, na.rm = T)
    ) %>%
    drop_na() 
  
  data2plot <- data_subset.df %>%
    mutate(
      n_obs = sum(value2plot),
      value2plot = value2plot / n_obs,
      value2plot = value2plot*100,
      figure = paste0(round(value2plot, 0), "%"),
      order_var = case_when(
        proceso_justo == "Proceso injusto" ~ 1,
        proceso_justo =="Proceso justo" ~ 2,
        T ~ NA_real_)
    ) %>%
    rename(
      labels = proceso_justo
    )
  
  colors4plot <-  c("#ef4b4b", "#009AA9")
  
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = labels,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "horizontal")  
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Percepcion proceso justo",
                           "/proceso_justo.svg"),
         width = 189.7883,
         height = 65,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. indicadores                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

percepcion_indicadores.fn <- function(
  
  data.df = master_data.df
    
) {
  
  data_subset.df <- data.df %>%
    filter(sentenciado == 1) %>%
    mutate(
      proceso_justo = 
        case_when(
          as.numeric(P5_26A) == 1 ~ 1,
          as.numeric(P5_26A) == 0 ~ 0,
          T ~ NA_real_
        )
    ) 
  
  data2table <- data_subset.df %>%
    group_by(proceso_justo) %>%
    summarise(
      `Índice 13 criterios mínimos` = mean(indicator_general, na.rm = T),
      `Sub-Índice de protección de derechos humanos` = mean(indicator_GDH, na.rm = T),
      `Sub-Índice de uso no arbitrario de la autoridad` = mean(indicator_UAA, na.rm = T),
      `Sub-Índice de proceso justo` = mean(indicator_PJ, na.rm = T)
    ) %>% 
    drop_na() %>%
    mutate(
      proceso_justo = 
        case_when(
          proceso_justo == 1 ~ "Proceso justo",
          proceso_justo == 0 ~ "Proceso injusto"
        )
    ) %>%
    pivot_longer(cols = !proceso_justo, names_to = "category", values_to = "value2plot") %>%
    rbind(data.frame(category = c("— — — — — — — — — — — — — — — — — — — — — —", 
                                  "— — — — — — — — — — — — — — — — — — — — — —"),
                     value2plot = c(NA_real_, NA_real_),
                     proceso_justo = c("Proceso justo", "Proceso injusto"))
    ) %>%
    mutate(
      order_value =
        case_when(
          category == "Índice 13 criterios mínimos" ~ 1,
          category == "Sub-Índice de uso no arbitrario de la autoridad" ~ 4,
          category == "Sub-Índice de protección de derechos humanos" ~ 5,
          category == "Sub-Índice de proceso justo" ~ 3,
          category == "— — — — — — — — — — — — — — — — — — — — — —" ~ 2
        )
    )
  
  justo.df <- data2table %>%
    filter(proceso_justo == "Proceso justo")
  
  injusto.df <- data2table %>%
    filter(proceso_justo == "Proceso injusto")
  
  colors4plot <-  c("#009AA9","#ef4b4b")
  names(colors4plot) <- c("Proceso justo",
                          "Proceso injusto")
  
  p <- ggplot(data2table,
              aes(x = value2plot,
                  y = reorder(category, -order_value))) +
    geom_segment(data = justo.df,
                 aes(x = value2plot, y = reorder(category, -order_value),
                     yend = reorder(injusto.df$category, -order_value),
                     xend = injusto.df$value2plot), #use the $ operator to fetch data from our "Females" tibble
                 color = "#aeb6bf",
                 size = 4.5, #Note that I sized the segment to fit the points
                 alpha = .5) +
    geom_hline(yintercept = 4, linetype = "longdash", color = "black", size = 0.25) +
    geom_point(aes(x = value2plot, y = category, color = proceso_justo), size = 4, show.legend = F)  +
    geom_text(aes(x = value2plot, y = category, 
                  label = paste0(round(value2plot*100,0),"%"), 
                  family = "Lato Full", fontface = "bold"), 
              size= 3.514598, color = "black", vjust = -1) +
    coord_cartesian(clip = "off") +
    scale_color_manual(values = colors4plot) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1),limits = c(0,1),
                       labels = scales::percent_format(accuracy = 1), position = "top")+
    WJP_theme() +
    theme(legend.position="bottom",
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
                                   color = "Black", hjust = 0));p
  
  ggsave(plot = p, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Percepcion proceso justo",
                           "/percepcion_indicadores.svg"),
         width = 189.7883,
         height = 110,
         units  = "mm",
         dpi    = 72,
         device = "svg") 
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Procedimiento abreviado                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedimiento.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- data.df %>%
    filter(sentenciado == 1) %>%
    mutate(
      counter = 1,
      procedimiento =
        case_when(
          as.numeric(P5_6) == 1 ~ "Juicio",
          as.numeric(P5_6) == 2 ~ "Procedimiento abreviado",
          T ~ NA_character_
        )) %>%
    ungroup() %>%
    group_by(procedimiento) %>%
    summarise(
      value2plot = sum(counter, na.rm = T)
    ) %>%
    drop_na() 
  
  data2plot <- data_subset.df %>%
    mutate(
      n_obs = sum(value2plot),
      value2plot = value2plot / n_obs,
      value2plot = value2plot*100,
      figure = paste0(round(value2plot, 0), "%"),
      order_var = case_when(
        procedimiento == "Proceso injusto" ~ 1,
        procedimiento =="Proceso justo" ~ 2,
        T ~ NA_real_)
    ) %>%
    rename(
      labels = procedimiento
    )
  
  colors4plot <-  c("#2a2a9A", "#2a2a9A")
  
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = labels,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "horizontal")  
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Percepcion proceso justo",
                           "/procedimiento_abreviado.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Procedimiento                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

percepcion_procedimiento.fn <- function(
  
  data.df = master_data.df  
  
) {
  
  data_subset.df <- data.df %>%
    mutate(
      proceso_justo = 
        case_when(
          as.numeric(P5_26A) == 1 ~ 1,
          as.numeric(P5_26A) == 0 ~ 0,
          T ~ NA_real_
        ),
      procedimiento =
        case_when(
          as.numeric(P5_6) == 1 ~ "Juicio",
          as.numeric(P5_6) == 2 ~ "Procedimiento abreviado",
          T ~ NA_character_
        ),
      counter = 1
    ) %>%
    group_by(procedimiento) %>%
    summarise(
      value2plot = mean(proceso_justo, na.rm = T),
      n_obs = sum(counter, na.rm = T)
    ) %>%
    drop_na()
  
  data2plot <- data_subset.df %>%
    mutate(
      value2plot = value2plot*100,
      labels = procedimiento,
      figure = paste0(round(value2plot,0), "%"),
      order_var = case_when(
        labels == "Juicio" ~ 2,
        labels =="Procedimiento abreviado" ~ 1,
        T ~ NA_real_)
    )
  
  colors4plot <-  c("#009AA9","#009AA9")
  
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = labels,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "horizontal")    
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Percepcion proceso justo",
                           "/percepcion_procedimiento_abreviado.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. Culpabilidad                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

culpabilidad.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- data.df %>%
    filter(sentenciado == 1) %>%
    mutate(
      counter = 1,
      culpabilidad = 
        case_when(
          as.numeric(P3_1) == 1 | as.numeric(P3_1) == 2 ~ "Autorreconocimiento como culpable",
          as.numeric(P3_1) == 3 | as.numeric(P3_1) == 4 ~ "Autorreconocimiento como inocente",
          T ~ NA_character_
        )
      ) %>%
    ungroup() %>%
    group_by(culpabilidad) %>%
    summarise(
      value2plot = sum(counter, na.rm = T)
    ) %>%
    drop_na() 
  
  data2plot <- data_subset.df %>%
    mutate(
      n_obs = sum(value2plot),
      value2plot = value2plot / n_obs,
      value2plot = value2plot*100,
      figure = paste0(round(value2plot, 0), "%"),
      order_var = case_when(
        culpabilidad == "Autorreconocimiento como culpable" ~ 1,
        culpabilidad =="Autorreconocimiento como inocente" ~ 2,
        T ~ NA_real_)
    ) %>%
    rename(
      labels = culpabilidad
    )
  
  colors4plot <-  c("#2a2a9A", "#2a2a9A")
  
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = labels,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "horizontal")  
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Percepcion proceso justo",
                           "/auto_culpabilidad.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6. Percepcion culpabilidad                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

percepcion_culpabilidad.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- data.df %>%
    filter(sentenciado == 1) %>%
    mutate(
      proceso_justo = 
        case_when(
          as.numeric(P5_26A) == 1 ~ 1,
          as.numeric(P5_26A) == 0 ~ 0,
          T ~ NA_real_
        ),
      culpabilidad = 
        case_when(
          as.numeric(P3_1) == 1 | as.numeric(P3_1) == 2 ~ "Autorreconocimiento como culpable",
          as.numeric(P3_1) == 3 | as.numeric(P3_1) == 4 ~ "Autorreconocimiento como inocente",
          T ~ NA_character_
        ),
      counter = 1
    ) %>%
    group_by(culpabilidad) %>%
    summarise(
      value2plot = mean(proceso_justo, na.rm = T),
      n_obs = sum(counter, na.rm = T)
    ) %>%
    drop_na()
  
  data2plot <- data_subset.df %>%
    mutate(
      value2plot = value2plot*100,
      labels = culpabilidad,
      figure = paste0(round(value2plot,0), "%"),
      order_var = case_when(
        labels == "Autorreconocimiento como culpable" ~ 2,
        labels == "Autorreconocimiento como inocente" ~ 1,
        T ~ NA_real_)
    )
  
  colors4plot <-  c("#009AA9","#009AA9")
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = labels,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "horizontal")    
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Percepcion proceso justo",
                           "/percepcion_culpabilidad.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7. Escucha                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

escucha.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- data.df %>%
    filter(sentenciado == 1) %>%
    mutate(
      counter = 1,
      escucha = 
        case_when(
          as.numeric(escuchado_x_juez) == 1 ~ "Se sintió escuchado",
          as.numeric(escuchado_x_juez) == 0 ~ "No se sintió escuchado",
          T ~ NA_character_
        )
    ) %>%
    ungroup() %>%
    group_by(escucha) %>%
    summarise(
      value2plot = sum(counter, na.rm = T)
    ) %>%
    drop_na() 
  
  data2plot <- data_subset.df %>%
    mutate(
      n_obs = sum(value2plot),
      value2plot = value2plot / n_obs,
      value2plot = value2plot*100,
      figure = paste0(round(value2plot, 0), "%"),
      order_var = case_when(
        escucha == "No se sintió escuchado" ~ 1,
        escucha =="Se sintió escuchado" ~ 2,
        T ~ NA_real_)
    ) %>%
    rename(
      labels = escucha
    )
  
  colors4plot <-  c("#2a2a9A", "#2a2a9A")
  
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = labels,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "horizontal")  
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Percepcion proceso justo",
                           "/escucha.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8. Percepcion escucha                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

percepcion_escucha.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- data.df %>%
    filter(sentenciado == 1) %>%
    mutate(
      proceso_justo = 
        case_when(
          as.numeric(P5_26A) == 1 ~ 1,
          as.numeric(P5_26A) == 0 ~ 0,
          T ~ NA_real_
        ),
      escucha = 
        case_when(
          as.numeric(escuchado_x_juez) == 1 ~ "Se sintió escuchado",
          as.numeric(escuchado_x_juez) == 0 ~ "No se sintió escuchado",
          T ~ NA_character_
        ),
      counter = 1
    ) %>%
    group_by(escucha) %>%
    summarise(
      value2plot = mean(proceso_justo, na.rm = T),
      n_obs = sum(counter, na.rm = T)
    ) %>%
    drop_na()
  
  data2plot <- data_subset.df %>%
    mutate(
      value2plot = value2plot*100,
      labels = escucha,
      figure = paste0(round(value2plot,0), "%"),
      order_var = case_when(
        labels == "No se sintió escuchado" ~ 1,
        labels == "Se sintió escuchado" ~ 2,
        T ~ NA_real_)
    )
  
  colors4plot <-  c("#009AA9", "#009AA9")
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = labels,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "horizontal")    
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Percepcion proceso justo",
                           "/percepcion_escucha.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  
}

