## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Uso excesivo de la autoridad
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
## 1. Uso Excesivo de la fuerza: Serie temporal                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

uso_fuerza_tiempo.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- master_data.df %>%
    filter(Anio_arresto > 2014) %>%
    mutate(
      uso_excesivo =
        case_when(
          proporcionalidad_uso_fuerza == 0 ~ 1,
          proporcionalidad_uso_fuerza == 1 ~ 0
        ),
      counter = 1
    ) %>%
    group_by(Anio_arresto) %>%
    summarise(
      value2plot = mean(uso_excesivo, na.rm = T),
      n_obs = sum(counter, na.rm = T)
    ) %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           category = "uso_excesivo",
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
  colors4plot <- mainColor
  names(colors4plot) <- "uso_excesivo"
  
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
                           savePath,"/Uso excesivo fuerza",
                           "/uso_excesivo_tiempo.svg"),
         width = 189.7883,
         height = 68.88612,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Uso Excesivo de la fuerza: Tipo                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

controles_tipo.fn <- function(
  
  data.df = master_data.df  
  
){
  
  data_subset.df <- data.df %>%
    mutate(
      counter = 1,
    ) %>%
    ungroup()%>%
    summarise(
      control_contacto = mean(control_contacto, na.rm = T),
      control_cooperativo = mean(controles_cooperativos, na.rm = T),
      control_sometimiento = mean(sometimiento, na.rm = T),
      control_defensivo = mean(tacticas_defensivas, na.rm = T),
      control_letal = mean(fuerza_letal, na.rm = T),
      n_obs = sum(counter, na.rm = T)) %>%
    ungroup() %>%
    distinct() %>%
    drop_na() %>%
    pivot_longer(cols = starts_with("control"), 
                 names_to = "category", 
                 values_to = "value2plot")
  
  data2plot <- data_subset.df %>%
    mutate(
      value2plot = value2plot*100,
      figure = paste0(round(value2plot,0), "%"),
      labels = category,
      order_var = case_when(
        labels == "control_letal" ~ 1,
        labels == "control_defensivo" ~ 2,
        labels == "control_sometimiento" ~ 3,
        labels == "control_contacto" ~ 4,
        labels =="control_cooperativo" ~ 5,
        T ~ NA_real_),
      labels = case_when(
        labels == "control_letal" ~ "Fuerza <br>letal",
        labels == "control_defensivo" ~ "Tácticas <br>defensivas",
        labels == "control_sometimiento" ~ "Tácticas de <br>sometimiento",
        labels == "control_contacto" ~ "Control mediamente <br>contacto",
        labels =="control_cooperativo" ~ "Controles <br>cooperativos",
        T ~ NA_character_)
    )
  colors4plot <- rep(mainColor,5)
  
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
                           savePath,"/Uso excesivo fuerza",
                           "/controles.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Uso Excesivo de la fuerza: Corporación                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

uso_fuerza_corporacion.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- master_data.df %>%
    mutate(
      uso_excesivo =
        case_when(
          proporcionalidad_uso_fuerza == 0 ~ 1,
          proporcionalidad_uso_fuerza == 1 ~ 0
        )
    ) %>%
    group_by(Corporacion_grupos) %>%
    summarise(
      value2plot = mean(uso_excesivo, na.rm = T)
    ) %>%
    drop_na() %>%
    filter(Corporacion_grupos != "Guardia Nacional") %>%
    filter(Corporacion_grupos != "NS/NR") %>%
    filter(Corporacion_grupos != "Otra") %>%
    rename(group_var = Corporacion_grupos)
  
  data2plot <- data_subset.df %>%
    arrange(value2plot) %>%
    mutate(
      order_var = -row_number(),
      value2plot = value2plot*100,
      figure = paste0(round(value2plot, 0), "%"),
      labels = 
        case_when(
          group_var == "Ejército o Marina"  ~ "Ejército o Marina",
          group_var == "Operativo Conjunto" ~ "Operativo Conjunto",
          group_var == "Policía Estatal" ~ "Policía Estatal",
          group_var == "Policía Estatal Ministerial o Judicial" ~ "Policía Estatal Ministerial <br>o Judicial",
          group_var == "Policía Federal" ~ "Policía Federal",
          group_var == "Policía Federal Ministerial" ~ "Policía Federal Ministerial",
          group_var == "Policía Municipal" ~ "Policía Municipal",
          
        )
    )
  colors4plot <- rep(mainColor,7)
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = categories,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "vertical")
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Uso excesivo fuerza",
                           "/uso_excesivo_corporacion.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Tipo de acciones realizadas por las personas al momento de su detención                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

acciones_detencion.fn <- function(
    
  datas.df = master_data.df
  
) {
  
  df <- data.df %>%
    filter(Anio_arresto >= 2008,
           NSJP == 1) %>% 
    mutate(Acc_detencion_1 = case_when(P3_15_1 == 1 ~ 1,
                                       P3_15_1 == 0 ~ 0,
                                       T ~ NA_real_),
           Acc_detencion_2 = case_when(P3_15_2 == 1 ~ 1,
                                       P3_15_2 == 0 ~ 0,
                                       T ~ NA_real_),
           Acc_detencion_3 = case_when(P3_15_3 == 1 ~ 1,
                                       P3_15_3 == 0 ~ 0,
                                       T ~ NA_real_),
           Acc_detencion_4 = case_when(P3_15_4 == 1 ~ 1,
                                       P3_15_4 == 0 ~ 0,
                                       T ~ NA_real_),
           Acc_detencion_5 = case_when(P3_15_5 == 1 ~ 1,
                                       P3_15_5 == 0 ~ 0,
                                       T ~ NA_real_),
           Acc_detencion_6 = case_when(P3_15_6 == 1 ~ 1,
                                       P3_15_6 == 0 ~ 0,
                                       T ~ NA_real_),
           Acc_detencion_7 = case_when(P3_15_7 == 1 ~ 1,
                                       P3_15_7 == 0 ~ 0,
                                       T ~ NA_real_),
           Acc_detencion_8 = case_when(P3_15_8 == 1 ~ 1,
                                       P3_15_8 == 0 ~ 0,
                                       T ~ NA_real_),
           Acc_detencion_9 = case_when(P3_15_9 == 1 ~ 1,
                                       P3_15_9 == 0 ~ 0,
                                       T ~ NA_real_)) %>% 
    select(starts_with("Acc_"))
  
  
  data2plot <- df %>%
    pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
    drop_na() %>% 
    group_by(Column) %>% 
    summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100)) %>% 
    rename(values = Column, 
           value2plot = Percentage) %>% 
    mutate(
      labels = case_when(values == "Acc_detencion_1"     ~ "Obedeció las órdenes de la autoridad que lo detuvo", 
                         values == "Acc_detencion_2"     ~ "Portaba alguna arma punzo cortante",
                         values == "Acc_detencion_3"   ~ "Portaba alguna arma de fuego",
                         values == "Acc_detencion_4"   ~ "Amenazó a alguien con el arma",
                         values == "Acc_detencion_5"   ~ "Disparó el arma de fuego (sólo personas que llevaban arma de fuego)", 
                         values == "Acc_detencion_6"   ~ "Manipuló algún objeto para usarlo como arma", 
                         values == "Acc_detencion_7"   ~ "Trató de sobornar a la autoridad para evitar su detención", 
                         values == "Acc_detencion_8"   ~ "Trató de defenderse físicamente", 
                         values == "Acc_detencion_9"   ~ "Trató de escapar para que no lo detuvieran"),
      figure = paste0(round(value2plot, 0), "%"),
      labels = str_wrap(labels, width = 40),
      order_var = rank(value2plot))
  
  
  
  colors4plot <- rep("#2a2a94", 9)
  
  
  plt <- ggplot(data2plot, 
                aes(x     = reorder(labels, order_var),
                    y     = value2plot,
                    label = figure,
                    color = labels)) +
    geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
             show.legend = F, width = 0.9) +
    scale_fill_manual(values = colors4plot) +
    geom_text(aes(y    = value2plot + 10 ),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%"),
                       position = "right") +
    scale_x_discrete( ) +
    WJP_theme() +
    theme(legend.position="none",
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
                                   color = "Black", hjust = 0),
          legend.title = element_blank())+
    coord_flip(); plt
  
  ggsave(plot = plt, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Uso excesivo fuerza",
                           "/acciones_detencion.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. Uso Excesivo de la fuerza: Delito                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


uso_fuerza_delito.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- data.df %>%
    mutate(Delito_unico_categ = case_when(Delito_unico_categ == "robos" &
                                            Robo_autopartes == "1" ~  "robo-autopartes",
                                          Delito_unico_categ == "robos" &
                                            Robo_vehiculo == "1" ~  "robo-vehiculo",
                                          T~ Delito_unico_categ )) %>%
    mutate(
      uso_excesivo =
        case_when(
          proporcionalidad_uso_fuerza == 0 ~ 1,
          proporcionalidad_uso_fuerza == 1 ~ 0
        ),
      delitos_alto_impacto = 
        case_when(
          Delito_unico_categ == "hom_dol"         ~ "Homicidio doloso",
          Delito_unico_categ == "secuestro"       ~ "Secuestro",
          Delito_unico_categ == "drogas"          ~ "Posesión o comercio de drogas",
          Delito_unico_categ == "armas"           ~ "Portación ilegal de armas",
          Delito_unico_categ == "robo-autopartes" ~ "Robo de autopartes",
          Delito_unico_categ == "robo-vehiculo"   ~ "Robo de vehículo",
          Delito_unico_categ == "extorsion"       ~ "Extorsión",
          T ~ NA_character_
        )
    ) %>%
    group_by(delitos_alto_impacto) %>%
    summarise(
      value2plot = mean(uso_excesivo, na.rm = T)
    ) %>%
    drop_na() %>%
    rename(group_var = delitos_alto_impacto) 
  
  
  data2plot <- data_subset.df %>%
    arrange(value2plot) %>%
    mutate(
      order_var = -row_number(),
      value2plot = value2plot*100,
      figure = paste0(round(value2plot, 0), "%"),
      labels = 
        case_when(
          group_var == "Homicidio doloso"              ~ "Homicidio doloso",
          group_var == "Secuestro"                     ~ "Secuestro",
          group_var == "Posesión o comercio de drogas" ~ "Posesión o comercio \nde drogas",
          group_var == "Portación ilegal de armas"     ~ "Portación ilegal \nde armas",
          group_var == "Robo de autopartes"            ~ "Robo de autopartes",
          group_var == "Robo de vehículo"              ~ "Robo de vehículo",
          group_var == "Extorsión"                     ~ "Extorsión",
          
        )
    )
  colors4plot <- rep(mainColor,7)
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = categories,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "vertical")
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Uso excesivo fuerza",
                           "/uso_excesivo_delito.svg"),
         width = 189.7883,
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}
