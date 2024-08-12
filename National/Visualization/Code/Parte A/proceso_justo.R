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
## Guardar silencio en el tiempo                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

guardar_silencio.fn <- function(
    data.df = master_data.df
) {
  
  data_subset.df <- data.df %>%
    filter(Anio_arresto > 2014) %>%
    mutate(
      guardar_silencio_detencion = 
        case_when(
          P3_14_5 == 1 ~ 1,
          P3_14_5 == 0 ~ 0
        ),
      guardar_silencio_mp = 
        case_when(
          P4_1_04 == 1 ~ 1,
          P4_1_04 == 2 ~ 0
        ),
      guardar_silencio_juez = 
        case_when(
          P5_2_4 == 1 ~ 1,
          P5_2_4 == 2 ~ 0
        )
    ) %>%
    group_by(Anio_arresto) %>%
    summarise(
      detencion = mean(guardar_silencio_detencion, na.rm = T),
      mp        = mean(guardar_silencio_mp, na.rm = T),
      juez      = mean(guardar_silencio_juez, na.rm = T),
      nobs_detencion      = sum(guardar_silencio_detencion, na.rm = T),
      nobs_mp             = sum(guardar_silencio_mp, na.rm = T),
      nobs_juez           = sum(guardar_silencio_juez, na.rm = T)
    ) %>%
    pivot_longer(cols = c(detencion, mp, juez), 
                 names_to = "category", values_to = "value2plot") %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           year  = as.numeric(Anio_arresto),
           n_obs = if_else(
             category == "detencion", nobs_detencion,
             if_else(
               category == "mp", nobs_mp,
               if_else(
                 category == "juez", nobs_juez, NA_real_
               )
             )
           )) %>%
    select(!starts_with("nobs_"))
  
  # Pulling minimum and maximum available year
  minyear <- 2015
  maxyear <- 2021
  
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0(x.axis.values)
  
  
  # Defining colors4plot
  colors4plot <- threeColors
  
  names(colors4plot) <- c("detencion", "mp", "juez")
  
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
                           savePath,"/Proceso justo",
                           "/guardar_silencio.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Información detención                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

informacion_detencion.fn <- function(
  data.df = master_data.df  
) {
  
  data_subset.df <- data.df %>%
    filter(Anio_arresto > 2014) %>%
    mutate(
      explicacion_detencion =
        case_when(
          P3_14_4 == 1 ~ 1,
          P3_14_4 == 0 ~ 0
        ),
      explicacion_mp =
        case_when(
          P4_1_03 == 1 ~ 1,
          P4_1_03 == 2 ~ 0
        ),
      explicacion_juez =
        case_when(
          P5_2_1 == 1 ~ 1,
          P5_2_1 == 2 ~ 0
        ),
      counter = 1,
    ) %>%
    group_by(Anio_arresto) %>%
    summarise(
      detencion = mean(explicacion_detencion, na.rm = T),
      mp = mean(explicacion_mp, na.rm = T),
      juez = mean(explicacion_juez, na.rm = T),
      n_obs      = sum(counter, na.rm = T)
    ) %>%
    pivot_longer(cols = c(detencion, mp, juez), names_to = "category", values_to = "value2plot") %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           year = as.numeric(Anio_arresto)
           ) 
           
  
  # Pulling minimum and maximum available year
  minyear <- 2015
  maxyear <- 2021
  
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0(x.axis.values)
  
  
  # Defining colors4plot
  colors4plot <- threeColors
  
  names(colors4plot) <- c("detencion", "mp", "juez")
  
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
                           savePath,"/Proceso justo",
                           "/informacion_detencion.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Claridad                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

claridad_actores.fn <- function(
  data.df = master_data.df  
){
  
  data_subset.df <- data.df %>%
    filter(Anio_arresto > 2014)  %>%
    mutate(
      claridad_mp =
        case_when(
          P5_17_3 == 1 | P5_17_3 == 2~ 1,
          P5_17_3 == 3 | P5_17_3 == 4 ~ 0
        ),
      claridad_juez =
        case_when(
          P5_17_2 == 1 | P5_17_2 == 2~ 1,
          P5_17_2 == 3 | P5_17_2 == 4 ~ 0
        ),
      claridad_defensor =
        case_when(
          P5_17_1 == 1 | P5_17_1 == 2~ 1,
          P5_17_1 == 3 | P5_17_1 == 4 ~ 0
        ),
      claridad_defendido =
        case_when(
          P5_17_4 == 1 | P5_17_4 == 2~ 1,
          P5_17_4 == 3 | P5_17_4 == 4 ~ 0
        ),
      counter = 1
    ) %>%
    group_by(Anio_arresto) %>%
    summarise(
      mp        = mean(claridad_mp, na.rm = T),
      juez      = mean(claridad_juez, na.rm = T),
      defensor  = mean(claridad_defensor, na.rm = T),
      defendido = mean(claridad_defendido, na.rm = T),
      n_obs      = sum(counter, na.rm = T)
    ) %>%
    pivot_longer(cols = c(mp, juez, defensor, defendido), names_to = "category", values_to = "value2plot") %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           year = as.numeric(Anio_arresto))
  
  # Pulling minimum and maximum available year
  minyear <- 2015
  maxyear <- 2021
  
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0(x.axis.values)
  
  
  # Defining colors4plot
  colors4plot <- c("#a90099","#3273FF", "#efa700", "#00B67F")
  
  names(colors4plot) <- c("mp", "juez", "defensor", "defendido")
  
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
                           savePath,"/Proceso justo",
                           "/claridad_actores.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Defensa oportuna                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

defensa_oportuna.fn <- function(
  data.df = master_data.df  
) {
  
  data_subset.df <- master_data.df %>%
    mutate( P4_1_05 = as.numeric(P4_1_05),
            P5_1 = as.numeric(P5_1),
            momento = case_when(P4_1_05 == 1 ~ "Defensa en Ministerio Público",
                                P5_1    == 1 ~ "Defensa con Juez",
                                P4_1_05 == 2 ~ "Defensa en Ministerio Público",
                                P5_1    == 2 ~ "Defensa con Juez",
                                T~ NA_character_),
            defensa = case_when(P4_1_05 == 1 ~ "Sí",
                                P5_1    == 1 ~ "Sí",
                                P4_1_05 == 2 ~ "No",
                                P5_1    == 2 ~ "No",
                                T~ NA_character_)
            )%>% 
    mutate(
      P5_4_A = as.numeric(P5_4_A),
      P5_4_M = as.numeric(P5_4_M),
      P5_4_A = case_when(
        P5_4_A >= 97 ~ NA_real_,
        T ~ P5_4_A),
      P5_4_M = case_when(
        P5_4_M >= 97 ~ NA_real_,
        T ~ P5_4_M),
      P5_4_M = P5_4_M/12,
      tiempo_sentencia = P5_4_A+P5_4_M)
  
  
  data2plot <- data_subset.df %>%
    drop_na(momento, defensa, tiempo_sentencia) %>%
    mutate(counter = 1) %>%
    group_by(momento, defensa) %>%
    summarise(mean_value = mean(tiempo_sentencia, na.rm = TRUE),
              n_obs = sum(counter, na.rm = TRUE)) %>%
    mutate(category = momento,
           group_var = case_when(defensa == "Sí" ~ "Sí",
                                 defensa == "No" ~ "No",
                                 T ~ NA_character_),
           value2plot = mean_value,
           labels = case_when(category == "Defensa en Ministerio Público"      ~ "Defensa en Ministerio Público",
                              category == "Defensa con Juez" ~ "Defensa con Juez",
                              T~NA_character_),
           figure = round(mean_value, 0),
           order_var = case_when(category ==  "Defensa en Ministerio Público" ~ 2,
                                 category == "Defensa con Juez"               ~ 1,
                                 T~NA_real_),
           order_value_bars = case_when(group_var == "Sí" ~ 1,
                                        group_var == "No" ~ 2,
                                        T ~ NA_real_)) 
    # filter(momento == "Defensa en Ministerio Público")
  
  colors4plot <- twoColors
  
  names(colors4plot) <- c("Sí",
                          "No")
  
  plot <- barsChart.fn(
    data.df                    = data2plot,
    categories_grouping_var    = data2plot$group_var,
    colors4plot                = colors4plot,
    nbars = 0, 
    orientation = "horizontal",
    percentage = F
  ) +
    labs(y = "Años de sentencia") +
    theme(
      axis.title.y = element_markdown(
        family   = "Lato Full",
        face     = "bold",
        size     = 3.514598*.pt,
        color    = "black",
        margin   = margin(0, 10, 0, 0),
        hjust    = 0.5
      )
    )
  
  ggsave(plot = plot,
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Proceso justo",
                           "/defensa_oportuna.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6. Variación tipo de defensa                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tipo_defensa.fn <- function(
  
  data.df = master_data.df
    
) {
  
  data_subset.df <- data.df %>%
    filter(Anio_arresto >= 2008,
           NSJP == 1) %>%
    mutate( P4_1_05 = as.numeric(P4_1_05),
            P5_1 = as.numeric(P5_1),
            defensa_momento = case_when(P4_1_05 == 1 & P5_1 == 1 ~ "Con defensa en MP y con asesoría previa a la audiencia inicial",
                                        P4_1_05 == 2 & P5_1 == 2 ~ "Sin defensa en MP y sin asesoría previa a la audiencia inicial",
                                        P4_1_05 == 2 & P5_1 == 1 ~ "Sin defensa en MP y con asesoría previa a la audiencia inicial",
                                        P4_1_05 == 1 & P5_1 == 2 ~ "Con defensa en MP y sin asesoría previa a la audiencia inicial",
                                        T~ NA_character_)) %>% 
    mutate(
      P5_4_A = as.numeric(P5_4_A),
      P5_4_M = as.numeric(P5_4_M),
      P5_4_A = case_when(
        P5_4_A >= 97 ~ NA_real_,
        T ~ P5_4_A),
      P5_4_M = case_when(
        P5_4_M >= 97 ~ NA_real_,
        T ~ P5_4_M),
      P5_4_M = P5_4_M/12,
      tiempo_sentencia = P5_4_A+P5_4_M)
  
  data2plot <- data_subset.df %>%
    drop_na(defensa_momento, abogado_publico, tiempo_sentencia) %>%
    mutate(counter = 1) %>%
    group_by(defensa_momento, abogado_publico) %>%
    summarise(mean_value = mean(tiempo_sentencia, na.rm = TRUE),
              n_obs = sum(counter, na.rm = TRUE)) %>%
    mutate(category = defensa_momento,
           group_var = case_when(abogado_publico == "1" ~ "Abogado público",
                                 abogado_publico == "0" ~ "Abogado privado",
                                 T ~ NA_character_),
           value2plot = mean_value,
           labels = case_when(category == "Con defensa en MP y con asesoría previa a la audiencia inicial"      ~ "Con defensa en MP <br>y con asesoría previa a <br>la audiencia inicial",
                              category == "Sin defensa en MP y sin asesoría previa a la audiencia inicial"      ~ "Sin defensa en MP <br>y sin asesoría previa a <br>la audiencia inicial",
                              category ==  "Sin defensa en MP y con asesoría previa a la audiencia inicial"                        ~ "Sin defensa en MP <br>y con asesoría previa a <br>la audiencia inicial",
                              category == "Con defensa en MP y sin asesoría previa a la audiencia inicial"            ~ "Con defensa en MP <br>y sin asesoría previa a <br>la audiencia inicial",
                              T~NA_character_),
           figure = round(mean_value, 0),
           order_var = case_when(category == "Defensa en Ministerio Público y con Juez"      ~ 3,
                                 category == "Sin defensa en Ministerio Público ni con Juez" ~ 4,
                                 category ==  "Defensa sólo con Juez"                        ~ 2,
                                 category == "Defensa sólo en Ministerio Público"            ~ 1,
                                 T~NA_real_),
           order_value_bars = case_when(group_var == "Abogado público" ~ 1,
                                        group_var == "Abogado privado" ~ 2,
                                        T ~ NA_real_))
  
  colors4plot <- twoColors
  
  names(colors4plot) <- c("Abogado público",
                          "Abogado privado")
  
  plot <- barsChart.fn(
    data.df                    = data2plot,
    categories_grouping_var    = data2plot$group_var,
    colors4plot                = colors4plot,
    nbars = 4, 
    orientation = "horizontal",
    percentage = F
  ) +
    labs(y = "Años de sentencia") +
    theme(
      axis.title.y = element_markdown(
        family   = "Lato Full",
        face     = "bold",
        size     = 3.514598*.pt,
        color    = "black",
        margin   = margin(0, 10, 0, 0),
        hjust    = 0.5
      )
    )
  
  
  ggsave(plot = plot,
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Proceso justo",
                           "/tipo_defensa.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7. Tribunal transparente                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tribunal_transparente.fn <- function(
  
  data.df = master_data.df
    
) {
  
  data_subset.df <- data.df %>%
    filter(Anio_arresto > 2014)  %>%
    filter(sentenciado == 1) %>%
    mutate(
      video =
        case_when(
          P5_19_3 == 1 ~ 1,
          P5_19_3 == 2 ~ 0
        ),
      publico =
        case_when(
          P5_16_5 == 1 | P5_16_5 == 2 | P5_16_5 == 3 ~ 1,
          P5_16_5 == 4 ~ 0
        ),
      counter = 1
    ) %>%
    group_by(Anio_arresto) %>%
    summarise(
      video = mean(video, na.rm = T),
      publico = mean(publico, na.rm = T),
      n_obs = sum(counter, na.rm = T)
    ) %>%
    pivot_longer(cols = c(video, publico), names_to = "category", values_to = "value2plot") %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           year = as.numeric(Anio_arresto))
  
  # Pulling minimum and maximum available year
  minyear <- 2015
  maxyear <- 2021
  
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0(x.axis.values)
  
  
  # Defining colors4plot
  colors4plot <- twoColors
  
  names(colors4plot) <- c("video", "publico")
  
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
                           savePath,"/Proceso justo",
                           "/tribunal_transparente.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8. Tribunal imparcial                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tribunal_imparcial.fn <- function(
  
  data.df = master_data.df  
  
) {
  
  data_subset.df <- data.df %>%
    filter(Anio_arresto > 2014)  %>%
    filter(sentenciado == 1) %>%
    mutate(
      procedimiento =
        case_when(
          as.numeric(P5_6) == 1 ~ "Juicio",
          as.numeric(P5_6) == 2 ~ "Procedimiento abreviado",
          T ~ NA_character_
        ),
      culpable_antes =
        case_when(
          P5_25 == 2 ~ 1,
          P5_25 == 1 ~ 0
        ),
      juez_diferente =
        case_when(
          P5_14 == 1 & procedimiento != "Procedimiento abreviado" ~ 1,
          P5_14 == 2 & procedimiento != "Procedimiento abreviado" ~ 0,
          T ~ NA_real_
        ),
      counter = 1
    ) %>%
    group_by(Anio_arresto) %>%
    summarise(
      culpable = mean(culpable_antes, na.rm = T),
      juez = mean(juez_diferente, na.rm = T),
      n_obs = sum(counter, na.rm = T)
    ) %>%
    pivot_longer(cols = c(culpable, juez), names_to = "category", values_to = "value2plot") %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           year = as.numeric(Anio_arresto))
  
  # Pulling minimum and maximum available year
  minyear <- 2015
  maxyear <- 2021
  
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0(x.axis.values)
  
  
  # Defining colors4plot
  colors4plot <- twoColors
  
  names(colors4plot) <- c("culpable", "juez")
  
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
                           savePath,"/Proceso justo",
                           "/tribunal_imparcial.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 9. Tribunal presente y responsivo                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tribunal_presente.fn <- function(
  
    data.df = master_data.df
  
) {
  
  data_subset.df <- master_data.df %>%
    filter(Anio_arresto > 2014)  %>%
    filter(sentenciado == 1) %>%
    mutate(
      juez_presente =
        case_when(
          P5_16_2 == 1  ~ 1,
          P5_16_2 == 2 | P5_16_2 == 3 | P5_16_2 == 4 ~ 0
        ),
      juez_control =
        case_when(
          P5_18 == 2 ~ 1,
          P5_18 == 1 | P5_18 == 3 | P5_18 == 4 | P5_18 == 5 ~ 0
        ),
      counter = 1
    ) %>%
    group_by(Anio_arresto) %>%
    summarise(
      juez_presente = mean(juez_presente, na.rm = T),
      juez_control = mean(juez_control, na.rm = T),
      n_obs = sum(counter, na.rm = T)
    ) %>%
    pivot_longer(cols = c(juez_presente, juez_control), names_to = "category", values_to = "value2plot") %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           year = as.numeric(Anio_arresto))
  
  # Pulling minimum and maximum available year
  minyear <- 2015
  maxyear <- 2021
  
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0(x.axis.values)
  
  
  # Defining colors4plot
  colors4plot <- threeColors
  
  names(colors4plot) <- c("juez_presente", "juez_control")
  
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
                           savePath,"/Proceso justo",
                           "/tribunal_presente.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 10. Tiempo condena                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tiempo_condena.fn <- function(
    
  data.df = master_data.df
  
){
  
  data_subset.df <- master_data.df %>%
    filter(sentenciado == 1) %>%
    ungroup() %>%
    mutate(
      P5_10  = case_when(
        P5_10 > 7 ~ NA_real_,
        T ~ as.numeric(P5_10)
      ),
      rapida =
        case_when(
          P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 ~ 1,
          P5_10 == 5  | P5_10 == 6 | P5_10 == 7 ~ 0
        ),
      corta =
        case_when(
          P5_10 == 5 ~ 1,
          P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 | P5_10 == 6 | P5_10 == 7 ~ 0
        ),
      media =
        case_when(
          P5_10 == 6 ~ 1,
          P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 | P5_10 == 5 | P5_10 == 7 ~ 0
        ),
      larga =
        case_when(
          P5_10 == 7 ~ 1,
          P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 | P5_10 == 5 | P5_10 == 6 ~ 0
        )
    ) %>%
    summarise(
      `Menos de seis meses`       = mean(rapida, na.rm = T),
      `Entre seis meses y un año` = mean(corta, na.rm = T),
      `Entre uno y dos años`      = mean(media, na.rm = T),
      `Más de dos años`           = mean(larga, na.rm = T)
    ) %>%
    pivot_longer(cols = c(`Menos de seis meses`, `Entre seis meses y un año`, `Entre uno y dos años`, `Más de dos años`), 
                 names_to = "category", values_to = "value2plot") %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"))
  
  data2plot <- data_subset.df %>%
    mutate(
      value2plot = value2plot,
      labels = category,
      figure = paste0(round(value2plot,0), "%"),
      order_var = case_when(
        labels == "Menos de seis meses" ~ 4,
        labels == "Entre seis meses y un año" ~ 3,
        labels == "Entre uno y dos años" ~ 2,
        labels == "Más de dos años" ~ 1,
        T ~ NA_real_)
    )
  
  colors4plot <- rep(mainColor,4)
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
                           savePath,"/Proceso justo",
                           "/tiempo_sentencia.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
}