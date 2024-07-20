## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Uso excesivo de la autoridad
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Junio 16, 2024
##
## This version:      Junio 16, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline: Estudio a profundidad de la FGR                                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## B.2.2.1.  Tipos de detención de la Policía ministerial federal, 2015 a 2021                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

detenciones_FGR.fn <- function(
  
  data.df = master_data.df  
  
  ){
  

      data_subset.df <- data.df %>% 
        filter(Anio_arresto >= 2015,
               NSJP == 1, 
               Corporacion_grupos == "Policía Federal Ministerial") %>% 
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
               labels = figure,
               group_var = tipo_detencion)
      
      
      
      colors4plot <- c("Flagrancia" = "#2a2a94",
                       "Orden de detención" = "#a90099",
                       "Inspeccion" = "#3273ff",
                       "Irregulares" = "#FA4D57")
      
      # Creating ggplot
      plt <- ggplot(data2table, 
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
        )
      
      
      plt
      
      
      ggsave(plot   = plt,
             file   = paste0(path2SP,"/National/Visualization",
                             "/Output/Politica criminal/",
                             savePath,"/Estudio FGR/Figure2_1.svg"), 
             width  = 189.7883, 
             height = 80,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      
      return(data2table)

}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## B.2.2.2.  Contrastes en los tipos de detención empleados por la Policía ministerial federal y por la Guardia Nacional ynPolicía Federal                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

detencion_GN_PF.fn <- function(
    
  data.df = master_data.df
  
  ){


    Main_database_2008_FGR <- data.df %>% 
      filter(Anio_arresto >= 2008,
             NSJP == 1, 
             Corporacion_grupos == "Policía Federal Ministerial" | 
               Corporacion_grupos == "Guardia Nacional" |
               Corporacion_grupos == "Policía Federal" ) %>% 
      mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                        orden_det   == 1 ~ "Orden de detención",
                                        inspeccion  == 1 ~ "Inspeccion",
                                        det_ninguna == 1 ~ "Irregulares",
                                        T ~ NA_character_),
             Corporacion_grupos = case_when(Corporacion_grupos == "Policía Federal Ministerial" ~ "Policía Federal Ministerial",
                                            Corporacion_grupos == "Guardia Nacional" ~ "GN/Polcía Federal",
                                            Corporacion_grupos == "Policía Federal" ~ "GN/Polcía Federal",
                                            T ~ NA_character_))
    
    
    
    data2plot <- Main_database_2008_FGR %>%
      select(tipo_detencion, Corporacion_grupos) %>% 
      group_by(tipo_detencion, Corporacion_grupos) %>%
      drop_na() %>% 
      summarise(Frequency = n(), .groups = 'drop') %>% 
      group_by(Corporacion_grupos) %>% 
      mutate(values = tipo_detencion,
             value2plot = Frequency / sum(Frequency) * 100,
             figure = paste0(round(value2plot, 0), "%"),
             labels = str_wrap(tipo_detencion, width = 20))
    
    
    colors4plot <- c("GN/Polcía Federal" = "#2a2a94",
                     "Policía Federal Ministerial" = "#a90099")
    
    
    plot <- ggplot(data2plot,
                   aes(
                     x     = values, 
                     y     = value2plot,
                     fill  = Corporacion_grupos,
                     label = paste0(figure)
                   )) +
      geom_bar(stat = "identity",
               show.legend = FALSE, width = 0.9, position = "dodge")+
      geom_text(aes(y    = value2plot + 10), 
                position = position_dodge(widt = 0.9),
                color    = "#4a4a49",
                family   = "Lato Full",
                fontface = "bold", 
                size = 3.514598)  +
      geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
      scale_fill_manual(values = colors4plot) +
      scale_y_continuous(limits = c(0, 105),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%"),
                         position = "left") +
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
        axis.title.x       = element_blank())
    
    plot
    
    ggsave(plot   = plot,
           file   = paste0(path2SP,"/National/Visualization",
                           "/Output/Politica criminal/",
                           savePath,"/Estudio FGR/Figure2_2.svg"), 
           width  = 189.7883, 
           height = 80,
           units  = "mm",
           dpi    = 72,
           device = "svg")
    
    return(data2plot)

}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## B.2.2.3. Proporción de los delitos federales sentenciados                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

federales_sentenciados.fn <- function(
    
  data.df = master_data.df
  
  ){

      Main_database_2008 <- data.df %>% 
        filter(Anio_arresto >= 2008,
               NSJP == 1,
               fuero == "Sólo federal",
               sentenciado == 1) %>% 
        mutate(Delito_unico_ungrouped_categ = case_when(Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ "Robo de casa habitación",
                                                        Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ "Robo de vehículo",
                                                        Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ "Robo a negocio",
                                                        Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ "Robo en transporte público",
                                                        Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ "Robo a transeunte en vía pública",
                                                        Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ "Robo de autopartes",
                                                        Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ "Robo en forma distinta a las anteriores",
                                                        Delito_unico == 1 & (P5_11_08 == 1|P5_31_08 == 1) ~ "Posesión ilegal de drogas",
                                                        Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ "Comercio ilegal de drogas",
                                                        Delito_unico == 1 & (P5_11_10 == 1|P5_31_10 == 1) ~ "Lesiones",
                                                        Delito_unico == 1 & (P5_11_11 == 1|P5_31_11 == 1) ~ "Homicidio culposo",
                                                        Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ "Homicidio doloso",
                                                        Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ "Portación ilegal de armas",
                                                        Delito_unico == 1 & (P5_11_14 == 1|P5_31_14 == 1) ~ "Incumplimiento de obligaciones de asistencia familiar",
                                                        Delito_unico == 1 & (P5_11_15 == 1|P5_31_15 == 1) ~ "Violencia familiar",
                                                        Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ "Daño a la propiedad",
                                                        Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ "Secuestro o secuestro expres",
                                                        Delito_unico == 1 & (P5_11_18 == 1|P5_31_18 == 1) ~ "Violación sexual",
                                                        Delito_unico == 1 & (P5_11_19 == 1|P5_31_19 == 1) ~ "Fraude",
                                                        Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ "Delincuencia organizada",
                                                        Delito_unico == 1 & (P5_11_21 == 1|P5_31_21 == 1) ~ "Otros delitos sexuales",
                                                        Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ "Exotorsión",
                                                        Delito_unico == 1 & (P5_11_23 == 1|P5_31_23 == 1) ~ "Privación de la libertad",
                                                        Delito_unico == 1 & (P5_11_24 == 1|P5_31_24 == 1) ~ "Abuso de confianza",
                                                        Delito_unico == 1 & (P5_11_25 == 1|P5_31_25 == 1) ~ "Amenazas",
                                                        Delito_unico == 1 & (P5_11_26 == 1|P5_31_26 == 1) ~ "Otro",
                                                        T ~ NA_character_)) 
      
      data2plot <- Main_database_2008 %>%
        filter(!is.na(Delito_unico_ungrouped_categ)) %>%
        group_by(Delito_unico_ungrouped_categ) %>%
        summarise(n = n()) %>%
        mutate(value2plot =  100 * n / sum(n),
               labels = paste0(round(value2plot,0),"%"),
               group_var = "Arrestos",
               Delito = Delito_unico_ungrouped_categ,
               Delito = str_wrap(Delito, width = 30)) %>%
        select(Delito,value2plot,labels,group_var) %>%
        arrange(value2plot) %>%
        mutate(Delito = factor(Delito, levels = Delito)) %>% 
        filter(value2plot >= 1)
      
      colors4plot <- rep("#2a2a9A", 9)
      
      
      plt <- ggplot(data2plot, 
                    aes(x     = Delito,
                        y     = value2plot,
                        label = labels,
                        group = group_var,
                        color = Delito)) +
        geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
                 show.legend = F, width = 0.9) +
        scale_fill_manual(values = colors4plot) +
        geom_text(aes(y    = value2plot +5 ),
                  color    = "#4a4a49",
                  family   = "Lato Full",
                  fontface = "bold") +
        labs(y = "% of respondents") +
        scale_y_continuous(limits = c(0, 105),
                           expand = c(0,0),
                           breaks = seq(0,100,20),
                           labels = paste0(seq(0,100,20), "%"),
                           position = "right")+
        scale_x_discrete( ) +
        expand_limits(y = c(0, 30))+
        WJP_theme() +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "#D0D1D3"),
              axis.title.y       = element_blank(),
              axis.title.x       = element_blank(),
              axis.text.y        = element_text(hjust = 1, size = 10))+
        coord_flip()
      
      plt
      
      ggsave(plot   = plt,
             file   = paste0(path2SP,"/National/Visualization",
                             "/Output/Politica criminal/",
                             savePath,"/Estudio FGR/Figure2_3.svg"), 
             width  = 189.7883, 
             height = 80,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      return(data2plot)

}




## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## B.2.2.4. Tipos de conclusión del proceso por delitos federales y el contraste con delitos estatales                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

conclusion_federales.fn <- function(
    
  data.df = master_data.df
  
  ){


      Main_database_2008 <- data.df %>% 
        filter(Anio_arresto >= 2008,
               NSJP == 1,
               fuero != "Algunos delitos de fuero común y algunos de fuero federal") %>% 
        mutate(juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                            P5_6 == "2" ~ "Procedimiento abreviado", 
                                            T ~ NA_character_))
      
      
      
      data2plot <- Main_database_2008 %>%
        select(juicio_abreviado, fuero) %>% 
        group_by(juicio_abreviado, fuero) %>%
        drop_na() %>% 
        summarise(Frequency = n(), .groups = 'drop') %>% 
        group_by(juicio_abreviado) %>% 
        mutate(values = fuero,
               value2plot = Frequency / sum(Frequency) * 100,
               figure = paste0(round(value2plot, 0), "%"),
               labels = str_wrap(fuero, width = 20))
      
      
      colors4plot <- c("Juicio" = "#2a2a9A",
                       "Procedimiento abreviado" = "#a90099")
      
      
      plot <- ggplot(data2plot,
                     aes(
                       x     = values, 
                       y     = value2plot,
                       fill  = juicio_abreviado,
                       label = paste0(figure)
                     )) +
        geom_bar(stat = "identity",
                 show.legend = FALSE, width = 0.9, position = "dodge")+
        geom_text(aes(y    = value2plot + 10), 
                  position = position_dodge(widt = 0.9),
                  color    = "#4a4a49",
                  family   = "Lato Full",
                  fontface = "bold", 
                  size = 3.514598)  +
        geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
        scale_fill_manual(values = colors4plot) +
        scale_y_continuous(limits = c(0, 105),
                           breaks = seq(0,100,20),
                           labels = paste0(seq(0,100,20), "%"),
                           position = "left") +
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
          axis.title.x       = element_blank())
      
      plot
      
      
      ggsave(plot   = plot,
             file   = paste0(path2SP,"/National/Visualization",
                             "/Output/Politica criminal/",
                             savePath,"/Estudio FGR/Figure2_4.svg"), 
             width  = 189.7883, 
             height = 75,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      
      return(data2plot)

}
