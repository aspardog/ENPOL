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
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Tipo de control - delitos alto impacto                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

control_alto_impacto.fn <- function(
  
  data.df = master_data.df  
  
  ){
  

      Main_database_2008 <- data.df  %>% 
        mutate(Delito_unico_categ = case_when(Delito_unico_categ == "robos" &
                                                Robo_autopartes == "1" ~  "robo-autopartes",
                                              Delito_unico_categ == "robos" &
                                                Robo_vehiculo == "1" ~  "robo-vehiculo",
                                              T~ Delito_unico_categ )) %>% 
        filter(
               Delito_unico_categ == "hom_dol"           |
                 Delito_unico_categ == "extorsion"       |
                 Delito_unico_categ == "drogas"          |
                 Delito_unico_categ == "armas"           |
                 Delito_unico_categ == "robo-autopartes" |
                 Delito_unico_categ == "robo-vehiculo"   |
                 Delito_unico_categ == "secuestro"       ) 
      
      
      data2table <- Main_database_2008 %>%
        group_by(Delito_unico_categ) %>%
        summarise(
          control_contacto = mean(control_contacto, na.rm = T),
          controles_cooperativos = mean(controles_cooperativos, na.rm = T),
          tacticas_defensivas = mean(tacticas_defensivas, na.rm = T),
          sometimiento = mean(sometimiento, na.rm = T),
          fuerza_letal = mean(fuerza_letal, na.rm = T),
        ) %>% 
        drop_na()  %>%
        pivot_longer(cols = !Delito_unico_categ, names_to = "category", values_to = "value2plot") %>%
        mutate(
          category =
            case_when(
              category == "control_contacto" ~ "d)control_contacto",
              category == "controles_cooperativos" ~ "e)controles_cooperativos",
              category == "tacticas_defensivas" ~ "b)tacticas_defensivas",
              category == "sometimiento" ~ "c)sometimiento",
              category == "fuerza_letal" ~ "a)fuerza_letal")) %>% 
        ungroup() %>% 
        group_by(Delito_unico_categ) %>%
        mutate(order_var = case_when(Delito_unico_categ == "drogas"          ~ 5, 
                                     Delito_unico_categ == "hom_dol"         ~ 1,
                                     Delito_unico_categ == "secuestro"       ~ 2,
                                     Delito_unico_categ == "armas"           ~ 4,
                                     Delito_unico_categ == "extorsion"       ~ 3,
                                     Delito_unico_categ == "robo-autopartes" ~ 6,
                                     Delito_unico_categ == "robo-vehiculo"   ~ 7,
                                     T ~ NA_real_)) %>%
        mutate(
          Delito_unico_categ = case_when(
            Delito_unico_categ == "drogas" ~ "Posesión o Comercio\n de Drogas", 
            Delito_unico_categ == "armas" ~ "Portación ilegal\n de armas", 
            Delito_unico_categ == "hom_dol" ~ "Homicido Doloso",
            Delito_unico_categ == "secuestro" ~ "Secuestro",
            Delito_unico_categ == "extorsion" ~ "Extorsion",
            Delito_unico_categ == "robo-autopartes" ~ "Robo de\n autopartes",
            Delito_unico_categ == "robo-vehiculo" ~ "Robo de\n vehículos",
          )) %>% 
        arrange(order_var)
      
      
      
      colors4plot <- c("d)control_contacto"       = "#2a2a9A",
                       "e)controles_cooperativos" = "#a90099",
                       "b)tacticas_defensivas"    = "#3273ff",
                       "c)sometimiento"           = "#EFA700",
                       "a)fuerza_letal"           = "#FA4D57")
      
      p <- ggplot(data2table,
                  aes(x = value2plot, y = reorder(Delito_unico_categ, -order_var), color = category)) +
        geom_point(size = 4)  +
        geom_text_repel(aes(x = value2plot, y = Delito_unico_categ, 
                      label = paste0(round(value2plot*100,0),"%"), 
                      family = "Lato Full", fontface = "bold",
                      box.padding = 0.5, 
                      point.padding = 0.5), 
                  size= 3.514598, color = "black", vjust = -1) +
        coord_cartesian(clip = "off") +
        scale_color_manual(values = colors4plot) +
        scale_x_continuous(breaks = seq(0, 1, by = 0.1),limits = c(0,1),
                           labels = scales::percent_format(accuracy = 1), position = "top")+
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
              axis.text.y=element_text(family   = "Lato Full",
                                       face     = "bold",
                                       size     = 3.514598*.pt,
                                       color    = "#524F4C",
                                       margin   = margin(0, 10, 0, 0),
                                       hjust = 0),
              legend.title = element_blank());p
      
      
      ggsave(plot   = p,
             file   = paste0(path2SP,
                             "/National/Visualization",
                             "/Output/Politica criminal/",
                             savePath,"/Delitos victimas/Figure2_1.svg"), 
             width  = 189.7883, 
             height = 130,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      return(data2table)


}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Tipo de detención - delitos alto impacto                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

detencion_alto_impacto.fn <- function(
    
  data.df = master_data.df
  
  ){

      Main_database_2008 <- data.df  %>% 
        mutate(Delito_unico_categ = case_when(Delito_unico_categ == "robos" &
                                                Robo_autopartes == "1" ~  "robo-autopartes",
                                              Delito_unico_categ == "robos" &
                                                Robo_vehiculo == "1" ~  "robo-vehiculo",
                                              T~ Delito_unico_categ )) %>% 
        filter(
               Delito_unico_categ == "hom_dol"           |
                 Delito_unico_categ == "secuestro"       |
                 Delito_unico_categ == "drogas"          |
                 Delito_unico_categ == "armas"           |
                 Delito_unico_categ == "robo-autopartes" |
                 Delito_unico_categ == "robo-vehiculo"   |
                 Delito_unico_categ == "extorsion" ) %>%
        mutate(tipo_detencion = case_when(flagrancia  == 1 ~ "Flagrancia",
                                          orden_det   == 1 ~ "Orden de detención",
                                          inspeccion  == 1 ~ "Inspeccion",
                                          det_ninguna == 1 ~ "Irregulares",
                                          T ~ NA_character_))
      
      
      
      
      data2plot <- Main_database_2008 %>%
        select(Delito_unico_categ, tipo_detencion) %>% 
        group_by(Delito_unico_categ, tipo_detencion) %>%
        drop_na() %>% 
        summarise(Frequency = n(), .groups = 'drop') %>% 
        group_by(Delito_unico_categ) %>% 
        mutate(values = Delito_unico_categ,
               value2plot = Frequency / sum(Frequency) * 100,
               figure = paste0(round(value2plot, 0), "%"),
               labels = str_wrap(Delito_unico_categ, width = 20),
               values = case_when(labels == "drogas" ~ "Posesión o Comercio\n de Drogas", 
                                  labels == "hom_dol" ~ "Homicido Doloso",
                                  labels == "secuestro" ~ "Secuestro",
                                  labels == "extorsion" ~ "Extorsión",
                                  labels == "armas" ~ "Portación ilegal\n de armas",
                                  labels == "robo-autopartes" ~ "Robo de\n autopartes",
                                  labels == "robo-vehiculo" ~ "Robo de\n vehículos",
                                  labels == "extorsion" ~ "Extorsión",
                                  T ~ NA_character_),
               tipo_detencion = case_when(tipo_detencion == "Flagrancia" ~ "D)Flagrancia", 
                                          tipo_detencion == "Inspeccion" ~ "A)Inspeccion",
                                          tipo_detencion == "Irregulares" ~ "C)Irregulares",
                                          tipo_detencion == "Orden de detención" ~ "B)Orden de detención",
                                          T ~ NA_character_),
               order_var = case_when(labels == "drogas"          ~ 5, 
                                     labels == "hom_dol"         ~ 1,
                                     labels == "secuestro"       ~ 2,
                                     labels == "armas"           ~ 4,
                                     labels == "extorsion"       ~ 3,
                                     labels == "robo-autopartes" ~ 6,
                                     labels == "robo-vehiculo"   ~ 7,
                                     T ~ NA_real_))
      
      colors4plot <- c("A)Inspeccion"         = "#3273ff",
                       "B)Orden de detención" = "#a90099",
                       "C)Irregulares"        = "#FA4D57",
                       "D)Flagrancia"         = "#2a2a9A")
      
      plot <- ggplot(data2plot,
                     aes(
                       x     = reorder(values, -order_var), 
                       y     = value2plot,
                       fill  = tipo_detencion,
                       label = paste0(figure)
                     )) +
        geom_bar(stat = "identity", width = 0.9, position = "stack")+
        geom_text(aes(y    = value2plot -.1,
                      label = paste0(figure)), 
                  position = position_stack(vjust = 0.5),
                  color    = "white",
                  family   = "Lato Full",
                  fontface = "bold", 
                  size = 3.514598)  +
        # geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
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
          legend.title = element_blank());plot
      
      
      ggsave(plot   = plot,
             file   = paste0(path2SP,"/National/Visualization",
                             "/Output/Politica criminal/",
                             savePath,"/Delitos victimas/Figure2_2.svg"), 
             width  = 189.7883, 
             height = 85,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      return(data2plot)

}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Tipo de terminación - delitos alto impacto                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

terminacion_alto_impacto.fn <- function(
    
  data.df = master_data.df
  
  ){
  


      Main_database_2008 <- data.df %>% 
        mutate(Delito_unico_categ = case_when(Delito_unico_categ == "robos" &
                                                Robo_autopartes == "1" ~  "robo-autopartes",
                                              Delito_unico_categ == "robos" &
                                                Robo_vehiculo == "1" ~  "robo-vehiculo",
                                              T~ Delito_unico_categ )) %>% 
        filter(Anio_arresto >= 2008,
               NSJP == 1, 
               Delito_unico_categ == "hom_dol"           |
                 Delito_unico_categ == "secuestro"       |
                 Delito_unico_categ == "drogas"          |
                 Delito_unico_categ == "armas"           |
                 Delito_unico_categ == "robo-autopartes" |
                 Delito_unico_categ == "robo-vehiculo"   |
                 Delito_unico_categ == "extorsion") %>% 
        mutate(tipo_terminacion = case_when(P5_6  == 1 ~ "Jucio",
                                            P5_6  == 2 ~ "Procedimiento abreviado",
                                            T ~ NA_character_))
      
      
      data2plot <- Main_database_2008 %>%
        select(Delito_unico_categ, tipo_terminacion) %>% 
        group_by(Delito_unico_categ, tipo_terminacion) %>%
        drop_na() %>% 
        summarise(Frequency = n(), .groups = 'drop') %>% 
        group_by(Delito_unico_categ) %>% 
        mutate(values = Delito_unico_categ,
               value2plot = Frequency / sum(Frequency) * 100,
               figure = paste0(round(value2plot, 0), "%"),
               labels = str_wrap(Delito_unico_categ, width = 20),
               order_var = case_when(labels == "drogas"          ~ 5, 
                                     labels == "hom_dol"         ~ 1,
                                     labels == "secuestro"       ~ 2,
                                     labels == "armas"           ~ 4,
                                     labels == "extorsion"       ~ 3,
                                     labels == "robo-autopartes" ~ 6,
                                     labels == "robo-vehiculo"   ~ 7,
                                     T ~ NA_real_),
               values = case_when(labels == "drogas"    ~ "Posesión o Comercio\n de Drogas", 
                                  labels == "hom_dol"   ~ "Homicido Doloso",
                                  labels == "secuestro" ~ "Secuestro",
                                  labels == "armas"     ~ "Portación ilegal\n de armas",
                                  labels == "extorsion" ~ "Extorsión",
                                  labels == "robo-autopartes" ~ "Robo de\n autopartes",
                                  labels == "robo-vehiculo" ~ "Robo de\n vehículos",
                                  T ~ NA_character_))
      
      colors4plot <- c("Procedimiento abreviado"        = "#2a2a9A",
                       "Jucio"                          = "#a90099")
      
      
      plot <- ggplot(data2plot,
                     aes(
                       x     = reorder(values, -order_var), 
                       y     = value2plot,
                       fill  = tipo_terminacion,
                       label = paste0(figure)
                     )) +
        geom_bar(stat = "identity", width = 0.9, position = "stack")+
        geom_text(aes(y    = value2plot -.1,
                      label = paste0(figure)), 
                  position = position_stack(vjust = 0.5),
                  color    = "white",
                  family   = "Lato Full",
                  fontface = "bold", 
                  size = 3.514598)  +
        scale_fill_manual(values =  colors4plot) +
        scale_y_continuous(limits = c(0, 105),
                           breaks = seq(0,100,20),
                           labels = paste0(seq(0,100,20), "%"),
                           position = "left") +
        coord_flip() +
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
                             savePath,"/Delitos victimas/Figure2_3.svg"), 
             width  = 189.7883, 
             height = 85,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      return(data2plot)

}
