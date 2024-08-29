## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Uso excesivo de la autoridad
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##                    Marcelo Torres   ()
##
## Dependencies:      World Justice Project
##
## Creation date:     Junio 16, 2024
##
## This version:      Junio 16, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline: Política criminal - Detenc                                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
# source("Code/Settings.R")
# 
# load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.1. Tipode de detenciones y cambios en el tiempo                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

detenciones_temporal.fn <- function(
  
  data.df = master_data.df  
  
  ){

    data_subset.df <- data.df %>% 
      filter(Anio_arresto >= 2015,
             NSJP == 1) %>% 
      mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                        orden_det   == 1 ~ "Orden de detención",
                                        inspeccion  == 1 ~ "Inspeccion",
                                        det_ninguna == 1 ~ "Irregulares",
                                        T ~ NA_character_))
    
    data2table <- data_subset.df %>%
      select(Anio_arresto, tipo_detencion) %>% 
      group_by(Anio_arresto, tipo_detencion) %>%
      drop_na() %>% 
      summarise(Frequency = n(), .groups = 'drop') %>% 
      group_by(Anio_arresto) %>% 
      mutate(values = Anio_arresto,
             value2plot = Frequency / sum(Frequency) * 100,
             figure = paste0(round(value2plot, 0), "%"),
             labels = if_else(
               Anio_arresto %in% c("2011", "2013", "2015", "2017", "2019", "2021"),
               paste0(round(value2plot, 0), "%"), NA_character_),
             group_var = tipo_detencion)
    
    
    
    colors4plot <- c("Flagrancia" = "#2a2a94" ,
                     "Orden de detención" = "#a90099", 
                     "Inspeccion" = "#3273ff",
                     "Irregulares" = "#fa4d57" )
    
    # Creating ggplot
    plot <- ggplot(data2table, 
                  aes(x     = Anio_arresto,
                      y     = value2plot,
                      label = labels,
                      group = group_var,
                      color = group_var)) +
      geom_point(size = 2,
                 show.legend = F
      ) +
      geom_line(size  = 1,
                show.legend = F
      ) +
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
                         labels = paste0(seq(0,100,20), "%"))+ #%>%
      scale_color_manual(values = colors4plot) +
      WJP_theme() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#d1cfd1"),
            axis.title.x       = element_blank(),
            axis.title.y       = element_blank(),
            axis.line.x        = element_line(color    = "#d1cfd1"),
            axis.ticks.x       = element_line(color    = "#d1cfd1",
                                              linetype = "solid"),
            legend.position      = "bottom"
      ); plot
    
    ggsave(plot   = plot,
           file   = paste0(path2SP,"/National/Visualization",
                           "/Output/Politica criminal/",
                           savePath,"/Detenciones/Figure3_1.svg"), 
           width  = 189.7883, 
           height = 80,
           units  = "mm",
           dpi    = 72,
           device = "svg")
    
    
    return(data2table)

}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.2. Tipos de detenciones por estado                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

detenciones_estado.fn <- function(
    
  data.df = master_data.df
  
  ) {

      data_subset.df <- data.df %>% 
        mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                          orden_det   == 1 ~ "Orden de detención",
                                          inspeccion  == 1 ~ "Inspeccion",
                                          det_ninguna == 1 ~ "Irregulares",
                                          T ~ NA_character_), 
               Estado_arresto = case_when(Estado_arresto == "México" ~ "Estado de México",
                                          T ~ Estado_arresto))
      redondeo <- c("APromedio nacional",
                    "Baja California",
                    "Chihuahua",
                    "Oaxaca",
                    "Tabasco")
      
      data2plot <- data_subset.df %>%
        select(Estado_arresto, tipo_detencion) %>% 
        group_by(Estado_arresto, tipo_detencion) %>%
        summarise(Frequency = n(), .groups = 'drop') %>% 
        drop_na() %>% 
        ungroup() %>%
        rbind(data_subset.df %>%
                ungroup() %>%
                group_by(tipo_detencion) %>%
                summarise(Frequency = n(), .groups = 'drop',
                          Estado_arresto = "APromedio nacional")%>%
                drop_na()
        )  %>%
        group_by(Estado_arresto) %>% 
        rename( values = Estado_arresto ) %>%
        arrange(desc(values)) %>%
        mutate(value2plot = Frequency / sum(Frequency) * 100) %>% 
        #redondeo para que sume 100 
        mutate(
          value2plot = case_when(tipo_detencion == "A)Inspeccion" & values %in% 
                                   redondeo ~ (value2plot+1),
                                 T~value2plot)
        ) %>% 
        mutate(
               figure = paste0(round(value2plot, 0), "%"),
               labels = str_wrap(values, width = 20),
               tipo_detencion = case_when(tipo_detencion == "Flagrancia" ~ "D)Flagrancia", 
                                          tipo_detencion == "Inspeccion" ~ "A)Inspeccion",
                                          tipo_detencion == "Irregulares" ~ "C)Irregulares",
                                          tipo_detencion == "Orden de detención" ~ "B)Orden de detención",
                                          T ~ NA_character_), 
               values = case_when(values == "Coahuila de Zaragoza"            ~ "Coahuila",
                                  values == "Michoacán de Ocampo"             ~ "Michoacán",
                                  values == "Distrito Federal"                ~ "Ciudad de México",
                                  values == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
                                  T ~ values)) %>%
        ungroup() %>% 
        group_by(tipo_detencion) %>%
        mutate(order = row_number()) %>%
        mutate(
          values = 
            if_else(values %in% "APromedio nacional", "Promedio nacional", values)
        ) %>%
        mutate(figure = case_when(order == 6 & tipo_detencion == "B)Orden de detención" ~ "27%",
                                  order == 13 & tipo_detencion == "D)Flagrancia" ~ "24%",
                                  order == 14 & tipo_detencion == "C)Irregulares" ~ "28%",
                                  order == 27 & tipo_detencion == "B)Orden de detención" ~ "22%",
                                  order == 31 & tipo_detencion == "D)Flagrancia" ~ "42%",
                                  order == 33 & tipo_detencion == "D)Flagrancia" ~ "35%",
                                  T ~ figure)) # redondeos
      
      
      colors4plot <- c("D)Flagrancia" = "#2a2a94" ,
                       "B)Orden de detención" = "#a90099", 
                       "A)Inspeccion" = "#3273ff",
                       "C)Irregulares" = "#fa4d57" )
      
      
      plot <- ggplot(data2plot,
                     aes(
                       x     = reorder(values, order), 
                       y     = value2plot,
                       fill  = tipo_detencion,
                       label = paste0(figure)
                     )) +
        geom_bar(stat = "identity", width = 0.9, position = "stack")+
        geom_text(aes(y    = value2plot), 
                  position = position_stack(vjust = 0.5),
                  hjust = 0.5, 
                  vjust = 0.5,
                  color    = "white",
                  family   = "Lato Full",
                  fontface = "bold", 
                  size = 3.514598)  +
        scale_fill_manual(values =  colors4plot) +
        scale_y_continuous(limits = c(0, 105),
                           breaks = seq(0,100,20),
                           labels = paste0(seq(0,100,20), "%"),
                           position = "left") +
        coord_flip()+
        theme(
          panel.background   = element_blank(),
          plot.background    = element_blank(),
          panel.grid.major   = element_line(size     = 0.25,
                                            colour   = "#5e5c5a",
                                            linetype = "dashed"),
          panel.grid.minor   = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),       
          axis.title.y       = element_blank(),
          axis.title.x       = element_blank(),
          axis.text.y=element_text(family   = "Lato Full",
                                   face     = "bold",
                                   size     = 3.514598*.pt,
                                   color    = "#524F4C",
                                   margin   = margin(0, 10, 0, 0),
                                   hjust = 0),
          legend.position      = "none",
          legend.title = element_blank())
      
      plot
      
      ggsave(plot   = plot,
             file   = paste0(path2SP,"/National/Visualization",
                             "/Output/Politica criminal/",
                             savePath,"/Detenciones/Figure3_2.svg"), 
             width  = 230, 
             height = 200,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      return(data2plot)

}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.3.3.	Tipos de detención por tiempo del proceso, solo para los de juicio --------                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

detencion_proceso_tiempo.fn <- function(
    
  data.df = master_data.df
  
  ){
    
    data_subset.df <- Main_database %>% 
      filter(Anio_arresto >= 2008,
             NSJP == 1,
             P5_6 == 1) %>% 
      mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                        orden_det   == 1 ~ "Orden de detención",
                                        inspeccion  == 1 ~ "Inspeccion",
                                        det_ninguna == 1 ~ "Irregulares",
                                        T ~ NA_character_),
             tiempo_dictar_sentencia = case_when(P5_10 == 1 ~ "Hasta seis meses",
                                                 P5_10 == 2 ~ "Hasta seis meses",
                                                 P5_10 == 3 ~ "Hasta seis meses",
                                                 P5_10 == 4 ~ "Hasta seis meses",
                                                 P5_10 == 5 ~ "Más de seis meses hasta un año",
                                                 P5_10 == 6 ~ "Más de un año hasta dos años",
                                                 P5_10 == 7 ~ "Más de dos años",
                                                 T ~ NA_character_))
    
    data2plot <- data_subset.df %>%
      select(tipo_detencion, tiempo_dictar_sentencia) %>% 
      group_by(tipo_detencion, tiempo_dictar_sentencia) %>%
      drop_na() %>% 
      summarise(Frequency = n(), .groups = 'drop') %>% 
      group_by(tipo_detencion) %>% 
      rename( values = tipo_detencion ) %>%
      arrange(desc(values)) %>%
      mutate(value2plot = Frequency / sum(Frequency) * 100,
             figure = paste0(round(value2plot, 0), "%"),
             labels = str_wrap(values, width = 20),
             order  = case_when(
                                tiempo_dictar_sentencia == "Hasta seis meses" ~ 1,
                                tiempo_dictar_sentencia == "Más de seis meses hasta un año" ~ 2,
                                tiempo_dictar_sentencia == "Más de un año hasta dos años" ~ 3,
                                tiempo_dictar_sentencia == "Más de dos años" ~ 4,
                                T ~ NA_real_))
    
    colors4plot <- c("Más de dos años"                = "#fa4d57", 
                     "Más de un año hasta dos años"   = "#3273ff", 
                     "Más de seis meses hasta un año" = "#a90099", 
                     "Hasta seis meses"               = "#2a2a94")
    
    
    plot <- ggplot(data2plot,
                   aes(
                     x     = values, 
                     y     = value2plot,
                     fill  = reorder(tiempo_dictar_sentencia, -order),
                     label = paste0(figure)
                   )) +
      geom_bar(stat = "identity", width = 0.9, position = "stack")+
      geom_text(aes(y    = value2plot), 
                position = position_stack(vjust = 0.5),
                # hjust = 0.5, 
                vjust = 1.25,
                color    = "white",
                family   = "Lato Full",
                fontface = "bold", 
                size = 3.514598)  +
      scale_fill_manual(values =  colors4plot) +
      scale_y_continuous(limits = c(0, 105),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%"),
                         position = "left") +
      coord_flip()+
      theme(
        panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),       
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, 10, 0, 0),
                                 hjust = 0),
        legend.position      = "none",
        legend.title = element_blank())
    
    plot
    
    ggsave(plot   = plot,
           file   = paste0(path2SP,"/National/Visualization",
                           "/Output/Politica criminal/",
                           savePath,"/Detenciones/Figure3_3.svg"), 
           width  = 189.7883, 
           height = 80,
           units  = "mm",
           dpi    = 72,
           device = "svg")
    
    return(data2plot)

}
