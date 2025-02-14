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
      figure = paste0(round(value2plot, 0), "%, N = ", n_obs),
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
                           "/National/Graphic_reports",
                           "/Output/", savePath, "/Debido proceso","/Percepcion proceso justo",
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
        ),
      counter_I13 = !is.na(indicator_general),
      counter_gdh = !is.na(indicator_GDH),
      counter_uaa = !is.na(indicator_UAA),
      counter_pj = !is.na(indicator_PJ),
    ) 
  
  data2table <- data_subset.df %>%
    group_by(proceso_justo) %>%
    summarise(
      `Índice 13 criterios mínimos` = mean(indicator_general, na.rm = T),
      `Sub-Índice de protección de derechos humanos` = mean(indicator_GDH, na.rm = T),
      `Sub-Índice de uso no arbitrario de la autoridad` = mean(indicator_UAA, na.rm = T),
      `Sub-Índice de proceso justo` = mean(indicator_PJ, na.rm = T),
      n_obs_I13 = sum(counter_I13, na.rm = T),
      n_obs_gdh = sum(counter_gdh, na.rm = T),
      n_obs_uaa = sum(counter_uaa, na.rm = T),
      n_obs_pj = sum(counter_pj, na.rm = T),
    ) %>% 
    drop_na() %>%
    mutate(
      proceso_justo = 
        case_when(
          proceso_justo == 1 ~ "Proceso justo",
          proceso_justo == 0 ~ "Proceso injusto"
        )
    ) %>%
    pivot_longer(cols = !c("proceso_justo", "n_obs_I13", "n_obs_gdh", "n_obs_uaa", "n_obs_pj"), names_to = "category", values_to = "value2plot") %>%
    rbind(data.frame(category = c("— — — — — — — — — — — — — — — — — — — — — —", 
                                  "— — — — — — — — — — — — — — — — — — — — — —"),
                     n_obs_I13 = NA,
                     n_obs_gdh = NA,
                     n_obs_uaa = NA,
                     n_obs_pj = NA,
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
        ),
      n_obs = case_when(order_value ==  1 ~ n_obs_I13,
                        order_value ==  5 ~ n_obs_gdh,
                        order_value ==  4 ~ n_obs_uaa,
                        order_value ==  3 ~ n_obs_pj)
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
                  label = paste0(round(value2plot*100,0),"%, N = ", n_obs), 
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
                           "/National/Graphic_reports",
                           "/Output/", savePath, "/Debido proceso","/Percepcion proceso justo",
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
      figure = paste0(round(value2plot, 0), "%, N = ", n_obs),
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
                           "/National/Graphic_reports",
                           "/Output/", savePath, "/Debido proceso","/Percepcion proceso justo",
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
      counter = !is.na(proceso_justo)
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
      figure = paste0(round(value2plot,0), "%, N = ",n_obs),
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
                           "/National/Graphic_reports",
                           "/Output/", savePath, "/Debido proceso","/Percepcion proceso justo",
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
          as.numeric(P3_1) == 1 | as.numeric(P3_1) == 2 ~ "Autoidentificación como culpable",
          as.numeric(P3_1) == 3 | as.numeric(P3_1) == 4 ~ "Autoidentificación como inocente",
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
      figure = paste0(round(value2plot, 0), "%, N =", n_obs),
      order_var = case_when(
        culpabilidad == "Autoidentificación como culpable" ~ 1,
        culpabilidad =="Autoidentificación como inocente" ~ 2,
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
                           "/National/Graphic_reports",
                           "/Output/", savePath, "/Debido proceso","/Percepcion proceso justo",
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
          as.numeric(P3_1) == 1 | as.numeric(P3_1) == 2 ~ "Autoidentificación como culpable",
          as.numeric(P3_1) == 3 | as.numeric(P3_1) == 4 ~ "Autoidentificación como inocente",
          T ~ NA_character_
        ),
      counter = !is.na(proceso_justo)
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
      figure = paste0(round(value2plot,0), "%, N =", n_obs),
      order_var = case_when(
        labels == "Autoidentificación como culpable" ~ 1,
        labels == "Autoidentificación como inocente" ~ 2,
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
                           "/National/Graphic_reports",
                           "/Output/", savePath, "/Debido proceso","/Percepcion proceso justo",
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
      figure = paste0(round(value2plot, 0), "%, N =", n_obs),
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
                           "/National/Graphic_reports",
                           "/Output/", savePath, "/Debido proceso","/Percepcion proceso justo",
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
      counter = !is.na(proceso_justo)
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
      figure = paste0(round(value2plot,0), "%, N =", n_obs),
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
                           "/National/Graphic_reports",
                           "/Output/", savePath, "/Debido proceso","/Percepcion proceso justo",
                           "/percepcion_escucha.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8. Percepcion resumen                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

percepcion_resumen.fn <- function(
    
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
          as.numeric(escuchado_x_juez) == 1 ~ "Se sintió <br>escuchado",
          as.numeric(escuchado_x_juez) == 0 ~ "No se sintió <br>escuchado",
          T ~ NA_character_
        ),
      counter = !is.na(proceso_justo),
      culpabilidad = 
        case_when(
          as.numeric(P3_1) == 1 | as.numeric(P3_1) == 2 ~ "Autoidentificación <br>como culpable",
          as.numeric(P3_1) == 3 | as.numeric(P3_1) == 4 ~ "Autoidentificación <br>como inocente",
          T ~ NA_character_
        ),
      procedimiento =
        case_when(
          as.numeric(P5_6) == 1 ~ "Juicio",
          as.numeric(P5_6) == 2 ~ "Procedimiento <br>abreviado",
          T ~ NA_character_
        )
    )
  
  
  vars <- c("escucha", "culpabilidad", "procedimiento")
  
  final_data <- map_dfr(vars, function(var) {
    
    data_subset.df %>%
      rename(category = all_of(var)) %>%
      group_by(category) %>%
      summarise(
        value2plot = mean(proceso_justo, na.rm = TRUE),
        n_obs = sum(counter,na.rm = T)
      ) %>%
      drop_na() %>%
      mutate(
        group = as.character(var),
        figure = paste0(round(value2plot*100, 0), "%, N =", n_obs)
      )
    
  })
  
  data2plot <- final_data %>%
    mutate(
      group = 
        case_when(
          group == "escucha"       ~ "Sentimiento de <br>escucha",
          group == "culpabilidad"  ~ "Autoidentificación <br>de culpabilidad",
          group == "procedimiento" ~ "Forma de conclusión <br>del proceso"
        ),
      
      group = factor(group, levels = c("Forma de conclusión <br>del proceso", 
                                       "Sentimiento de <br>escucha", 
                                       "Autoidentificación <br>de culpabilidad")),
      order_values =
        case_when(
          category %in% c("Autoidentificación <br>como culpable",
                          "Se sintió <br>escuchado",
                          "Procedimiento <br>abreviado") ~ 1,
          T ~ 2
        ),
      category = fct_reorder(category, order_values)
    )
  
  group_labels <- c("Sentimiento de <br>escucha" = "Sentimiento de \nescucha",
                    "Autoidentificación <br>de culpabilidad" = "Autoidentificación de \nculpabilidad",
                    "Forma de conclusión <br>del proceso" = "Forma de conclusión \ndel proceso")
  
  plot <- ggplot(
    data2plot,
    aes(x = value2plot * 100,
        y =  category
    )
  )+
    geom_segment(
      aes(xend = 0, yend = category)
    ) +
    geom_point(
      size = 4,
      color = "#009AA9"
    ) +
    #facet_grid(group ~ ., scales = "free_y", space = "free") +
    facet_grid(rows = vars(group), scales = "free_y", switch = "y", space = "free_y", labeller = labeller(group = group_labels)) +
    scale_x_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%"),
                       position = "top") +
    WJP_theme() +
    xlab("") +
    ylab("") +
    geom_text(aes(x    = value2plot * 100 + 5,
                  label = figure), 
              position = position_dodge(width = 0.9),
              color    = "black",
              family   = "Lato Full",
              fontface = "bold", 
              size = 3.514598)  + 
    theme(
      panel.spacing = unit(1, "lines"), # Espacio entre facetas
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      panel.grid.major   = element_line(size     = 0.25,
                                        colour   = "#5e5c5a",
                                        linetype = "dashed"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#D0D1D3")
    ) +
    theme(
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank(),
      axis.ticks         = element_blank(),
      axis.text.y        = element_markdown(family   = "Lato Full",
                                            face     = "bold",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C",
                                            margin   = margin(0, 10, 0, 0),
                                            hjust = 0.5, vjust = 0), 
      plot.caption = element_markdown(family   = "Lato Full",
                                      face     = "plain",
                                      size     = 3.514598*.pt,
                                      color    = "#524F4C", 
                                      vjust    = 0, 
                                      hjust    = 0, 
                                      margin = margin(20, 0, 0, 0)),
      axis.text.x        = element_markdown(family   = "Lato Full",
                                            face     = "bold",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C",
      ),
      plot.title          = element_text(family   = "Lato Full",
                                         face     = "bold",
                                         size     = 3.514598*.pt,
                                         color    = "black",
                                         margin   = margin(0, 0, 10, 0),
                                         hjust    = 0), 
      plot.subtitle      = element_text(family   = "Lato Full",
                                        face     = "plain",
                                        size     = 3.514598*.pt,
                                        color    = "black",
                                        margin   = margin(2.5, 0, 20, 0),
                                        hjust    = 0),
      legend.text        =  element_markdown(family   = "Lato Full",
                                             face     = "plain",
                                             size     = 3.514598*.pt,
                                             color    = "#524F4C"),
      legend.title       = element_markdown(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C"),
      strip.placement = "outside",
      strip.text = element_text(
        family   = "Lato Full",
        face     = "bold",
        size     = 3.514598*.pt,
        color    = "black", 
        hjust = 0.5,
        margin = margin(0, 0, 0, 0)
      )
    );plot
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Graphic_reports",
                           "/Output/", savePath, "/Debido proceso","/Percepcion proceso justo",
                           "/percepcion_resumen.svg"),
         width = 189.7883,
         height = 189.7883,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
}
