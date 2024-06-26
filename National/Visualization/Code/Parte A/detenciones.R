## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Detenciones
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
## Detenciones irregulares: Serie temporal                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

detenciones_tiempo.fn <- function(
  
  data.df = master_data.df  
  
) {
  
  data_subset.df <- data.df %>%
    filter(Anio_arresto > 2014)  %>%
    group_by(Anio_arresto) %>%
    mutate(
      counter = 1
    ) %>%
    summarise(
      value2plot = mean(det_ninguna, na.rm = T),
      n_obs = sum(counter, na.rm = T)
    ) %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           category = "detenciones_irregulares",
           year = as.numeric(Anio_arresto))
  
  # Pulling minimum and maximum available year
  minyear <- 2015
  maxyear <- 2021
  
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0(x.axis.values)
  
  
  # Defining colors4plot
  colors4plot <- mainCOLOR
  names(colors4plot) <- "detenciones_irregulares"
  
  # Saving data points
  data2plot <- data_subset.df %>% ungroup()
  
  # Applying plotting function
  chart <- LAC_lineChart(data           = data2plot,
                         target_var     = "value2plot",
                         grouping_var   = "year",
                         ngroups        = 1, 
                         labels_var     = "label",
                         colors_var     = "category",
                         colors         = colors4plot,
                         repel          = F,
                         custom.axis    = T,
                         x.breaks       = x.axis.values,
                         x.labels       = x.axis.labels,
                         sec.ticks      = sec.ticks)
  
  ggsave(plot = chart, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Detenciones",
                           "/detenciones_tiempo.svg"),
         width = 189.7883,
         height = 110,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Tiempos de traslado                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- data.df %>%
  mutate(
    counter = 1,
    n_obs = if_else(!is.na(Tiempo_traslado), 
                    sum(counter, na.rm = T), NA_real_)
  ) %>%
  ungroup() %>%
  group_by(Tiempo_traslado) %>%
  summarise(
    value2plot = sum(counter, na.rm = T),
  ) %>%
  ungroup() %>%
  drop_na() %>%
  distinct() %>%
  mutate(
    n_obs = sum(value2plot, na.rm = T),
    value2plot = value2plot/n_obs,
    value2plot = value2plot*100,
    figure = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                    "%"),
    category = Tiempo_traslado
    )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Cambios en los tiempos de traslado                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tiempos_traslado.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- data.df %>%
    filter(Anio_arresto > 2014)  %>%
    group_by(Anio_arresto) %>%
    mutate(
      counter = 1,
      n_obs = if_else(!is.na(Tiempo_traslado), 
                      sum(counter, na.rm = T), NA_real_)
    ) %>%
    ungroup() %>%
    group_by(Anio_arresto, Tiempo_traslado) %>%
    summarise(
      value2plot = sum(counter, na.rm = T)/n_obs,
      n_obs = n_obs
    ) %>%
    ungroup() %>%
    drop_na() %>%
    distinct() %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           category = Tiempo_traslado,
           year = as.numeric(Anio_arresto)) %>%
    mutate(label = if_else(category == "Hasta 30 minutos" | category == "Más de 6 horas hasta 24 horas", 
                           label, NA_character_)) %>%
    filter(category %in% c("Hasta 30 minutos", "Más de 6 horas hasta 24 horas"))
  
  
  # Pulling minimum and maximum available year
  minyear <- 2015
  maxyear <- 2021
  
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0(x.axis.values)
  
  
  # Defining colors4plot
  colors4plot <- c("Hasta 30 minutos" = "#009AA9",
                   "Más de 6 horas hasta 24 horas" = "#FA4D57"
  )
  
  # Saving data points
  data2plot <- data_subset.df %>% ungroup()
  
  # Applying plotting function
  chart <- LAC_lineChart(data           = data2plot,
                         target_var     = "value2plot",
                         grouping_var   = "year",
                         ngroups        = data_subset.df$category, 
                         labels_var     = "label",
                         colors_var     = "category",
                         colors         = colors4plot,
                         repel          = T,
                         custom.axis    = T,
                         x.breaks       = x.axis.values,
                         x.labels       = x.axis.labels,
                         sec.ticks      = sec.ticks)
  
  ggsave(plot = chart, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Detenciones",
                           "/tiempo_traslado.svg"),
         width = 189.7883,
         height = 100,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6. Cambios en el lugar de traslado                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lugar_traslado.fn <- function(
  
  data.df = master_data.df  
  
) {
  
  data_subset.df <- data.df %>%
    filter(Anio_arresto > 2014)  %>%
    group_by(Anio_arresto) %>%
    mutate(
      counter = 1,
      n_obs = if_else(!is.na(Primer_lugar_traslado), sum(counter, na.rm = T), NA_real_)
    ) %>%
    ungroup() %>%
    group_by(Anio_arresto, Primer_lugar_traslado) %>%
    summarise(
      value2plot = sum(counter, na.rm = T)/n_obs,
      n_obs = n_obs
    ) %>%
    drop_na() %>%
    distinct() %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           category = Primer_lugar_traslado,
           year = as.numeric(Anio_arresto)) %>%
    mutate(label = if_else(category == "Agencia del Ministerio Público" | category == "Instalación de la policía", 
                           label, NA_character_)) %>%
    filter(category %in% c("Agencia del Ministerio Público", "Instalación de la policía"))
  
  # Pulling minimum and maximum available year
  minyear <- 2015
  maxyear <- 2021
  
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0(x.axis.values)
  
  
  # Defining colors4plot
  colors4plot <- c("Agencia del Ministerio Público" = "#003B88",
                   "Instalación de la policía" = "#a90099")
  
  # Saving data points
  data2plot <- data_subset.df %>% ungroup()
  
  # Applying plotting function
  chart <- LAC_lineChart(data           = data2plot,
                         target_var     = "value2plot",
                         grouping_var   = "year",
                         ngroups        = data_subset.df$category, 
                         labels_var     = "label",
                         colors_var     = "category",
                         colors         = colors4plot,
                         repel          = T,
                         custom.axis    = T,
                         x.breaks       = x.axis.values,
                         x.labels       = x.axis.labels,
                         sec.ticks      = sec.ticks)
  
  ggsave(plot = chart, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Detenciones",
                           "/lugar_traslado.svg"),
         width = 189.7883,
         height = 100,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  
}
