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
  colors4plot <- mainColor
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
  
  return(data2plot)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Tiempos de traslado                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tiempo_traslado.fn <- function(
  
  data.df = master_data.df  
  
) {
  
  data_subset.df <- data.df %>%
    mutate(
      counter = 1,
      n_obs = if_else(!is.na(Tiempo_traslado), 
                      sum(counter, na.rm = T), NA_real_)
    ) %>%
    ungroup() %>%
    mutate(
      Tiempo_traslado =
        case_when(
          Tiempo_traslado %in% c("Hasta 30 minutos", 
                                 "Más de 30 minutos hasta 1 hora",
                                 "Más de 1 hora hasta 2 horas",
                                 "Más de 2 horas hasta 4 horas") ~ "Menos de 4 horas",
          Tiempo_traslado %in% c("Más de 4 horas hasta 6 horas",
                                 "Más de 6 horas hasta 24 horas") ~ "Más de 4 horas hasta 24 horas",
          Tiempo_traslado %in% c("Más de 24 horas hasta 48 horas") ~ "Más de 24 horas hasta 48 horas",
          Tiempo_traslado %in% c("Más de 48 horas hasta 72 horas",
                                 "Más de 72 horas") ~ "Más de 48 horas",
          T ~ NA_character_
        )
    ) %>%
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
  
  data2plot <- data_subset.df %>%
    mutate(
      value2plot = value2plot,
      labels = category,
      figure = paste0(round(value2plot,0), "%"),
      order_var = 
        case_when(
          Tiempo_traslado %in% c("Menos de 2 horas", "Menos de 4 horas") ~ 1,
          Tiempo_traslado %in% c("Más de 2 horas hasta 4 horas", "Más de 4 horas hasta 24 horas", "Más de 4 horas") ~ 2,
          Tiempo_traslado %in% c("Más de 4 horas hasta 6 horas", "Más de 24 horas hasta 48 horas") ~ 3,
          Tiempo_traslado %in% c("Más de 6 horas hasta 24 horas", "Más de 48 horas") ~ 4,
          Tiempo_traslado %in% c("Más de 24 horas hasta 48 horas") ~ 5,
          Tiempo_traslado %in% c("Más de 48 horas hasta 72 horas") ~ 6,
          Tiempo_traslado %in% c("Más de 72 horas") ~ 7,
          T ~ NA_real_
        ),
      figure = case_when(Tiempo_traslado == "Más de 24 horas hasta 48 horas" ~ "5%",
                         T~ figure)
    )
  
  colors4plot <- c("#2a2a9A", 
                   rep("#2a2a9A", 3))
  names(colors4plot) <- c("Menos de 4 horas",
                          "Más de 4 horas hasta 24 horas",
                          "Más de 24 horas hasta 48 horas",
                          "Más de 48 horas")
  
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = labels,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "vertical")
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Detenciones",
                           "/tiempo_traslado.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
  
}

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
    mutate(
      counter = 1,
      Tiempo_traslado =
        case_when(
          Tiempo_traslado %in% c("Hasta 30 minutos", 
                                 "Más de 30 minutos hasta 1 hora",
                                 "Más de 1 hora hasta 2 horas",
                                 "Más de 2 horas hasta 4 horas") ~ "Menos de 4 horas",
          Tiempo_traslado %in% c("Más de 4 horas hasta 6 horas",
                                 "Más de 6 horas hasta 24 horas") ~ "Más de 4 horas hasta 24 horas",
          Tiempo_traslado %in% c("Más de 24 horas hasta 48 horas") ~ "Más de 24 horas hasta 48 horas",
          Tiempo_traslado %in% c("Más de 48 horas hasta 72 horas",
                                 "Más de 72 horas") ~ "Más de 48 horas",
          T ~ NA_character_
        ),
      Tiempo_traslado = 
        case_when(
          Tiempo_traslado %in% c("Menos de 4 horas") ~ "Menos de 4 horas",
          Tiempo_traslado %in% c("Más de 4 horas hasta 24 horas", 
                                 "Más de 24 horas hasta 48 horas",
                                 "Más de 48 horas") ~ "Más de 4 horas"
        )
    ) %>%
    group_by(Anio_arresto, Tiempo_traslado) %>%
    summarise(
      value2plot = sum(counter, na.rm = T)
    ) %>%
    ungroup() %>%
    drop_na() %>%
    group_by(Anio_arresto) %>%
    mutate(
      value2plot = value2plot/sum(value2plot)
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
    mutate(label = if_else(category == "Menos de 4 horas" | category == "Más de 4 horas", 
                           label, NA_character_)) 
  
  
  
  # Pulling minimum and maximum available year
  minyear <- 2015
  maxyear <- 2021
  
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0(x.axis.values)
  
  
  # Defining colors4plot
  colors4plot <- c(
    "#009AA9",
    "#FA4D57"
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
                           "/tiempo_traslado_serie.svg"),
         width = 189.7883,
         height = 100,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## lugar de traslado                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lugar_traslado.fn <- function(
    
  data.df = master_data.df  
  
) {
  
  data_subset.df <- data.df %>%
    mutate(
      counter = 1,
      n_obs = if_else(!is.na(Primer_lugar_traslado), 
                      sum(counter, na.rm = T), NA_real_)
    ) %>%
    ungroup() %>%
    group_by(Primer_lugar_traslado) %>%
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
      category = Primer_lugar_traslado
    ) %>%
    arrange(-value2plot)
  
  data2plot <- data_subset.df %>%
    mutate(
      value2plot = value2plot,
      labels = category,
      figure = paste0(round(value2plot,0), "%"),
      order_var = row_number()
    )
  
  colors4plot <- c("#009AA9", rep("#99D7DD",6), "#EFA700", rep("#99D7DD",6))
  
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = labels,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "vertical")
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Detenciones",
                           "/lugar_traslado.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Cambios en el lugar de traslado                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lugar_traslado_nueva.fn <- function(
  
  data.df = master_data.df  
  
) {
  
  data_subset.df <- master_data.df  %>%
    mutate(MP = case_when(P3_19 == "01" ~ 1,
                          P3_19 == "02" ~ 0,
                          P3_19 == "03" ~ 0,
                          P3_19 == "04" ~ 0,
                          P3_19 == "05" ~ 0,
                          P3_19 == "06" ~ 0,
                          P3_19 == "07" ~ 0,
                          P3_19 == "08" ~ 0,
                          P3_19 == "09" ~ 0,
                          P3_19 == "10" ~ 0,
                          P3_19 == "11" ~ 0,
                          P3_19 == "12" ~ 0,
                          P3_19 == "13" ~ 0,
                          P3_19 == "14" ~ 0,
                          T ~ NA_real_),
           PO = case_when(P3_19 == "01" ~ 0,
                          P3_19 == "02" ~ 0,
                          P3_19 == "03" ~ 1,
                          P3_19 == "04" ~ 0,
                          P3_19 == "05" ~ 0,
                          P3_19 == "06" ~ 0,
                          P3_19 == "07" ~ 0,
                          P3_19 == "08" ~ 0,
                          P3_19 == "09" ~ 0,
                          P3_19 == "10" ~ 0,
                          P3_19 == "11" ~ 0,
                          P3_19 == "12" ~ 0,
                          P3_19 == "13" ~ 0,
                          P3_19 == "14" ~ 0,
                          T ~ NA_real_)
    ) %>%
    group_by(Anio_arresto) %>%
    summarize(MP = mean(MP, na.rm = T),
              PO = mean(PO, na.rm = T)) %>% 
    pivot_longer(cols = -c("Anio_arresto"), names_to = "cat" , values_to = "value2plot" ) %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           category = case_when(cat == "MP" ~ "Agencia del Ministerio Público",
                                cat == "PO" ~ "Instalación de la policía"),
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
  colors4plot <- c("Agencia del Ministerio Público" = "#009AA9",
                   "Instalación de la policía" = "#EFA700")
  
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
                           "/lugar_traslado_serie.svg"),
         width = 189.7883,
         height = 100,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Mapa tiempo de traslado                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mapa_tiempo_traslado.fn <- function(
  
  data.df = master_data.df  
  
) {
  
  # Define a vector containing unique values of Estado_arresto
  estados <- unique(data.df$Estado_arresto)
  
  # Use map_dfr to loop through each Estado_arresto value and combine the results into a dataframe
  result_df <- map_dfr(estados, ~{
    # Filter the data for the current Estado_arresto value
    filtered_data <- data.df %>%
      filter(Estado_arresto == .x) %>%
      mutate(
        Tiempo_traslado =
          case_when(
            Tiempo_traslado %in% c("Hasta 30 minutos", 
                                   "Más de 30 minutos hasta 1 hora",
                                   "Más de 1 hora hasta 2 horas",
                                   "Más de 2 horas hasta 4 horas") ~ "Menos de 4 horas",
            Tiempo_traslado %in% c("Más de 4 horas hasta 6 horas",
                                   "Más de 6 horas hasta 24 horas") ~ "Más de 4 horas hasta 24 horas",
            Tiempo_traslado %in% c("Más de 24 horas hasta 48 horas") ~ "Más de 24 horas hasta 48 horas",
            Tiempo_traslado %in% c("Más de 48 horas hasta 72 horas",
                                   "Más de 72 horas") ~ "Más de 48 horas",
            T ~ NA_character_
          ),
        Tiempo_traslado = 
          case_when(
            Tiempo_traslado %in% c("Menos de 4 horas") ~ "Menos de 4 horas",
            Tiempo_traslado %in% c("Más de 4 horas hasta 24 horas", 
                                   "Más de 24 horas hasta 48 horas",
                                   "Más de 48 horas") ~ "Más de 4 horas"
          ),
        counter = 1
      ) %>%
      group_by(Tiempo_traslado, Estado_arresto) %>%
      summarise(TT = sum(counter, na.rm = TRUE)) %>%
      ungroup() %>%
      drop_na() %>%
      mutate(
        value2plot = TT / sum(TT)
      ) %>%
      filter(Tiempo_traslado == c("Menos de 4 horas"))
    
    return(filtered_data)
  })
  
  Estados <- result_df %>%
    select(!Tiempo_traslado) %>%
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
  
  promedio_nacional <- data.df %>%
    mutate(
      Tiempo_traslado =
        case_when(
          Tiempo_traslado %in% c("Hasta 30 minutos", 
                                 "Más de 30 minutos hasta 1 hora",
                                 "Más de 1 hora hasta 2 horas",
                                 "Más de 2 horas hasta 4 horas") ~ "Menos de 4 horas",
          Tiempo_traslado %in% c("Más de 4 horas hasta 6 horas",
                                 "Más de 6 horas hasta 24 horas") ~ "Más de 4 horas hasta 24 horas",
          Tiempo_traslado %in% c("Más de 24 horas hasta 48 horas") ~ "Más de 24 horas hasta 48 horas",
          Tiempo_traslado %in% c("Más de 48 horas hasta 72 horas",
                                 "Más de 72 horas") ~ "Más de 48 horas",
          T ~ NA_character_
        ),
      Tiempo_traslado = 
        case_when(
          Tiempo_traslado %in% c("Menos de 4 horas") ~ "Menos de 4 horas",
          Tiempo_traslado %in% c("Más de 4 horas hasta 24 horas", 
                                 "Más de 24 horas hasta 48 horas",
                                 "Más de 48 horas") ~ "Más de 4 horas"
        ),
      counter = 1
    ) %>%
    group_by(Tiempo_traslado) %>%
    summarise(TT = sum(counter, na.rm = TRUE)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(
      value2plot = TT / sum(TT)
    ) %>%
    filter(Tiempo_traslado == c("Menos de 4 horas")) %>%
    pull(value2plot)
  
  Estados <- Estados %>%
    ungroup() %>% 
    add_row(
      TT              = 0,    
      ESTADO          = "ANacional",
      value2plot      = promedio_nacional
      )
  
  quintiles <- round(quantile(round((Estados$value2plot *100), 0), probs = seq(0, 1, by = 0.2)),0)
  
  table.df <- Estados %>%
    mutate(
      ` ` = "",
      `%` = round(value2plot*100, 0)
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
    
    padding(j = 2, padding.right = 30) %>%
    padding(j = 1, padding.left  = 10) %>%
    padding(j = 3, padding.left  = 10) %>%
    
    width(j = " ", width = 0.5, unit = "mm") %>%
    width(j = "%", width = 0.75,   unit = "mm") %>%
    
    bg(i = ~  `%` >= 40 & `%` < 53, j = ' ', bg = "#99D7DD", part = "body") %>%
    bg(i = ~  `%` >= 53 & `%` < 60, j = ' ', bg = "#33AEBA", part = "body") %>%
    bg(i = ~  `%` >= 60 & `%` < 65, j = ' ', bg = "#0087A3", part = "body") %>%
    bg(i = ~  `%` >= 65 & `%` < 72, j = ' ', bg = "#00617F", part = "body") %>%
    bg(i = ~  `%` >= 72 & `%` < 90, j = ' ', bg = "#004E70", part = "body") %>%
    
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
    ) %>%
    bold(i = ~ Estado == "Promedio Nacional", bold = TRUE, part = "body")
  
  
  tpanel <- gen_grob(table.df, 
                     fit      = "auto",
                     scaling  = "min", 
                     just     = c("left", "top"),
                     wrapping = T)
  
  mexico_map <- mapa %>%
    left_join(Estados, by = "ESTADO") %>%
    mutate(
      value2plot = round(value2plot*100, 0),
      color_group = case_when(
        value2plot >= 40 & value2plot < 53 ~ "T1",
        value2plot >= 53 & value2plot < 60 ~ "T2",
        value2plot >= 60 & value2plot < 65 ~ "T3",
        value2plot >= 65 & value2plot < 72 ~ "T4",
        value2plot >= 72 & value2plot < 90 ~ "T5",
      ),
      color_group = as.factor(color_group)
    )
  
  
  cat_palette <- c("T1"   = "#99D7DD",
                   "T2"  =  "#33AEBA",
                   "T3"  =  "#0087A3",
                   "T4"  =  "#00617F",
                   "T5"  =  "#004E70")
  
  
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
  
 
  categories <- c("[41%-53%]",
                  "(53%-60%]",
                  "(60%-65%]",
                  "(65%-72%]",
                  "(72%-88%]")
  
  leyend <- data.frame(
    Values = categories,
    Blank = "")
  leyend <- flextable(leyend)  %>% 
    width(j = "Blank", width = 0.5, unit = "mm") %>% 
    set_header_labels(Values = "Escala", Blank = " ") %>% 
    bg(i = ~ Values == "[41%-53%]", j = "Blank", bg = "#99D7DD", part = "body") %>%
    bg(i = ~ Values == "(53%-60%]", j = "Blank", bg = "#33AEBA", part = "body") %>%
    bg(i = ~ Values == "(60%-65%]", j = "Blank", bg = "#0087A3", part = "body") %>%
    bg(i = ~ Values == "(65%-72%]", j = "Blank", bg = "#00617F", part = "body") %>%
    bg(i = ~ Values == "(72%-88%]", j = "Blank", bg = "#004E70", part = "body") %>%  

    
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
  
  
  ggsave(plot = viz, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Detenciones",
                           "/tiempo_traslado_mapa.svg"),
         width = 189.7883,
         height = 175,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Mapa lugar de traslado                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mapa_lugar_traslado.fn <- function(
 
  data.df = master_data.df
     
) {
  
  # Define a vector containing unique values of Estado_arresto
  estados <- unique(data.df$Estado_arresto)
  
  # Use map_dfr to loop through each Estado_arresto value and combine the results into a dataframe
  result_df <- map_dfr(estados, ~{
    # Filter the data for the current Estado_arresto value
    filtered_data <- data.df %>%
      filter(Estado_arresto == .x) %>%
      mutate(
        counter = 1
      ) %>%
      group_by(Primer_lugar_traslado, Estado_arresto) %>%
      summarise(PT = sum(counter, na.rm = TRUE)) %>%
      ungroup() %>%
      drop_na() %>%
      mutate(
        value2plot = PT / sum(PT)
      ) %>%
      filter(Primer_lugar_traslado == c("Agencia del Ministerio Público", "Instalación de la policía")) %>%
      mutate(
        max = max(value2plot)
      ) %>%
      filter(value2plot == max)
    
    return(filtered_data)
  })
  
  Estados <- result_df %>%
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
  
  promedio_nacional <- data.df %>%
    mutate(
      counter = 1
    ) %>%
    ungroup() %>%
    group_by(Primer_lugar_traslado) %>%
    summarise(PT = sum(counter, na.rm = TRUE)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(
      value2plot = PT / sum(PT)
    ) %>%
    filter(Primer_lugar_traslado %in% "Agencia del Ministerio Público") %>%
    pull(value2plot)
    
  
  Estados <- Estados %>%
    ungroup() %>% 
    add_row(
      Primer_lugar_traslado = "",  
      PT              = 0,    
      ESTADO          = "ANacional",
      value2plot      = promedio_nacional,
      max             = 0)
  
  quintiles <- round(quantile(round((Estados$value2plot *100), 0), probs = seq(0, 1, by = 0.2)),0)
  
  table <- Estados %>%
    mutate(
      ` ` = "",
      `%` = round(value2plot*100, 0)
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
    
    padding(j = 2, padding.right = 30) %>%
    padding(j = 1, padding.left  = 10) %>%
    padding(j = 3, padding.left  = 10) %>%
    
    width(j = " ", width = 0.5, unit = "mm") %>%
    width(j = "%", width = 0.75,   unit = "mm") %>%
    
    bg(i = ~ `%` >= 30 & `%` < 50, j = ' ', bg = "#99D7DD", part = "body") %>%
    bg(i = ~ `%` >= 50 & `%` < 70, j = ' ', bg = "#33AEBA", part = "body") %>%
    bg(i = ~ `%` >= 70, j = ' ', bg = "#00759D", part = "body") %>%
    
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
    ) %>%
    bold(i = ~ Estado == "Promedio Nacional", bold = TRUE, part = "body")
  
  
  tpanel <- gen_grob(table, 
                     fit      = "auto",
                     scaling  = "min", 
                     just     = c("left", "top"),
                     wrapping = T)
  
  mexico_map <- mapa %>%
    left_join(Estados, by = "ESTADO") %>%
    mutate(
      value2plot = round(value2plot*100, 0),
      color_group = case_when(
        Primer_lugar_traslado == "Agencia del Ministerio Público" & value2plot >= 30 & value2plot < 47 ~ "T1",
        Primer_lugar_traslado == "Agencia del Ministerio Público" & value2plot >= 47 & value2plot < 51 ~ "T2",
        Primer_lugar_traslado == "Agencia del Ministerio Público" & value2plot >= 51 & value2plot < 61 ~ "T3",
        Primer_lugar_traslado == "Agencia del Ministerio Público" & value2plot >= 61 & value2plot < 70 ~ "T4",
        Primer_lugar_traslado == "Agencia del Ministerio Público" & value2plot >= 70 & value2plot <= 88 ~ "T5"
      ),
      color_group = as.factor(color_group)
    )
  
  cat_palette <- c("T1"  = "#99D7DD",
                   "T2"  = "#33AEBA",
                   "T3"  = "#0087A3",
                   "T4"  = "#00617f",
                   "T5"  = "#004E70")
  
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
  
  categories <- c("[30%-47%]",
                  "(47%-51%]",
                  "(51%-61%]",
                  "(61%-70%]",
                  "(70%-88%]")
  
  leyend <- data.frame(
    Values = categories,
    Blank = "")
  leyend <- flextable(leyend)  %>% 
    width(j = "Blank", width = 0.5, unit = "mm") %>% 
    set_header_labels(Values = "Escala", Blank = " ") %>% 
    bg(i = ~ Values == "[30%-47%]", j = "Blank", bg = "#99D7DD", part = "body") %>%
    bg(i = ~ Values == "(47%-51%]", j = "Blank", bg = "#33AEBA", part = "body") %>%
    bg(i = ~ Values == "(51%-61%]", j = "Blank", bg = "#0087A3", part = "body") %>%
    bg(i = ~ Values == "(61%-70%]", j = "Blank", bg = "#00617f", part = "body") %>%
    bg(i = ~ Values == "(70%-88%]", j = "Blank", bg = "#004E70", part = "body") %>%
    
    
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
  
  
   ##
  
  # viz <- wrap_elements(tpanel) + p +
  #   plot_layout(ncol = 2, nrow = 1, widths = c(1,3), heights = c(1,1))
  
  ggsave(plot = viz, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Detenciones",
                           "/lugar_traslado_mapa.svg"),
         width = 189.7883,
         height = 175,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Cuadrantes por tiempo de traslado y lugar de traslado                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Define a vector containing unique values of Estado_arresto
estados <- unique(data.df$Estado_arresto)

# Use map_dfr to loop through each Estado_arresto value and combine the results into a dataframe
result_df <- map_dfr(estados, ~{
  # Filter the data for the current Estado_arresto value
  filtered_data <- data.df %>%
    filter(Estado_arresto == .x) %>%
    mutate(
      Tiempo_traslado =
        case_when(
          Tiempo_traslado %in% c("Hasta 30 minutos", 
                                 "Más de 30 minutos hasta 1 hora",
                                 "Más de 1 hora hasta 2 horas",
                                 "Más de 2 horas hasta 4 horas") ~ "Menos de 4 horas",
          Tiempo_traslado %in% c("Más de 4 horas hasta 6 horas",
                                 "Más de 6 horas hasta 24 horas") ~ "Más de 4 horas hasta 24 horas",
          Tiempo_traslado %in% c("Más de 24 horas hasta 48 horas") ~ "Más de 24 horas hasta 48 horas",
          Tiempo_traslado %in% c("Más de 48 horas hasta 72 horas",
                                 "Más de 72 horas") ~ "Más de 48 horas",
          T ~ NA_character_
        ),
      Tiempo_traslado = 
        case_when(
          Tiempo_traslado %in% c("Menos de 4 horas") ~ "Menos de 4 horas",
          Tiempo_traslado %in% c("Más de 4 horas hasta 24 horas", 
                                 "Más de 24 horas hasta 48 horas",
                                 "Más de 48 horas") ~ "Más de 4 horas"
        ),
      counter = 1
    ) %>%
    group_by(Tiempo_traslado, Estado_arresto) %>%
    summarise(TT = sum(counter, na.rm = TRUE)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(
      value2plot = TT / sum(TT)
    ) %>%
    filter(Tiempo_traslado == c("Menos de 4 horas"))
  
  return(filtered_data)
})

Estados <- result_df %>%
  select(!Tiempo_traslado) %>%
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

promedio_nacional <- data.df %>%
  mutate(
    Tiempo_traslado =
      case_when(
        Tiempo_traslado %in% c("Hasta 30 minutos", 
                               "Más de 30 minutos hasta 1 hora",
                               "Más de 1 hora hasta 2 horas",
                               "Más de 2 horas hasta 4 horas") ~ "Menos de 4 horas",
        Tiempo_traslado %in% c("Más de 4 horas hasta 6 horas",
                               "Más de 6 horas hasta 24 horas") ~ "Más de 4 horas hasta 24 horas",
        Tiempo_traslado %in% c("Más de 24 horas hasta 48 horas") ~ "Más de 24 horas hasta 48 horas",
        Tiempo_traslado %in% c("Más de 48 horas hasta 72 horas",
                               "Más de 72 horas") ~ "Más de 48 horas",
        T ~ NA_character_
      ),
    Tiempo_traslado = 
      case_when(
        Tiempo_traslado %in% c("Menos de 4 horas") ~ "Menos de 4 horas",
        Tiempo_traslado %in% c("Más de 4 horas hasta 24 horas", 
                               "Más de 24 horas hasta 48 horas",
                               "Más de 48 horas") ~ "Más de 4 horas"
      ),
    counter = 1
  ) %>%
  group_by(Tiempo_traslado) %>%
  summarise(TT = sum(counter, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na() %>%
  mutate(
    value2plot = TT / sum(TT)
  ) %>%
  filter(Tiempo_traslado == c("Menos de 4 horas")) %>%
  pull(value2plot)

Estados_TT <- Estados %>%
  ungroup() %>% 
  add_row(
    TT              = 0,    
    ESTADO          = "ANacional",
    value2plot      = promedio_nacional
  ) %>% 
  rename( value2plot_TT = value2plot )




# Define a vector containing unique values of Estado_arresto
estados <- unique(data.df$Estado_arresto)

# Use map_dfr to loop through each Estado_arresto value and combine the results into a dataframe
result_df <- map_dfr(estados, ~{
  # Filter the data for the current Estado_arresto value
  filtered_data <- data.df %>%
    filter(Estado_arresto == .x) %>%
    mutate(
      counter = 1
    ) %>%
    group_by(Primer_lugar_traslado, Estado_arresto) %>%
    summarise(PT = sum(counter, na.rm = TRUE)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(
      value2plot = PT / sum(PT)
    ) %>%
    filter(Primer_lugar_traslado == c("Agencia del Ministerio Público", "Instalación de la policía")) %>%
    mutate(
      max = max(value2plot)
    ) %>%
    filter(value2plot == max)
  
  return(filtered_data)
})

Estados <- result_df %>%
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

promedio_nacional <- data.df %>%
  mutate(
    counter = 1
  ) %>%
  ungroup() %>%
  group_by(Primer_lugar_traslado) %>%
  summarise(PT = sum(counter, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na() %>%
  mutate(
    value2plot = PT / sum(PT)
  ) %>%
  filter(Primer_lugar_traslado %in% "Agencia del Ministerio Público") %>%
  pull(value2plot)


EstadosLT <- Estados %>%
  ungroup() %>% 
  add_row(
    Primer_lugar_traslado = "",  
    PT              = 0,    
    ESTADO          = "ANacional",
    value2plot      = promedio_nacional,
    max             = 0) %>% 
  select(ESTADO, PT ,value2plot) %>% 
  rename(value2plot_LT = value2plot)



# juntamos las dos bases 
Estados <- left_join(Estados_TT, EstadosLT, by = "ESTADO") %>% 
  mutate(category = case_when( value2plot_TT >= 0.65 &  value2plot_LT >= 0.65 ~ "I", 
                               value2plot_TT >= 0.65 &  value2plot_LT <= 0.65 ~ "II",
                               value2plot_TT <= 0.65 &  value2plot_LT >= 0.65 ~ "III",
                               value2plot_TT <= 0.65 &  value2plot_LT <= 0.65 ~ "IV",
                               T ~ NA_character_), 
         value2plot_TT = value2plot_TT*100, 
         value2plot_LT = value2plot_LT*100)

ggplot(Estados, aes(x = value2plot_TT, 
                    y = value2plot_LT, 
                    color = category)) + 
  geom_point(size = 6) + 
  geom_text_repel(aes(label = ESTADO), 
                  size = 3.5,
                  color = "black") + 
  geom_vline(xintercept = 65, color = "#a90099", linetype = "dashed", size = 0.5) + 
  geom_hline(yintercept = 65, color = "#a90099", linetype = "dashed", size = 0.5) + 
  scale_color_manual(values = c("IV" = "#Fa4d57",  # Assign your custom colors here
                                "II" = "#A68BF2", 
                                "III" = "#A68BF2",
                                "I" = "#009AA9")) + 
  labs(x = "% de personas trasladadas hasta en 4 horas",
       y = "% de personas trasladades al MP en primer lugar") +
  WJP_theme() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")  



