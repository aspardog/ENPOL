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
## 1. Personas reincidentes, no reincidentes y profesionalización                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

reincidentes.fn <- function(
    
  data.df = master_data.df
  
  ){

        Main_database_2008 <- data.df %>% 
          mutate(reincidencia = case_when(P9_1 == "1" ~ "reincidentes",
                                          P9_1 == "2" ~ "No reincidentes",
                                          T ~ NA_character_),
                 reincidentes_tipo = case_when( reincidencia == "reincidentes" & Delito_gr_1_robos == 1 &
                                                  P9_2_01 == "1" | P9_2_02 == "1" | P9_2_03 == "1" | 
                                                  P9_2_04 == "1" | P9_2_05 == "1" | P9_2_06 == "1" |
                                                  P9_2_07 == "1" ~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_2_drogas == 1 &
                                                  P9_2_08 == "1" | P9_2_09 == "1"  ~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_3_del_org == 1 &
                                                  P9_2_20 == "1"   ~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_4_lesiones &
                                                  P9_2_10 == "1"~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_5_hom_cul &
                                                  P9_2_11 == "1"~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_6_hom_dol &
                                                  P9_2_12 == "1"~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_7_armas &
                                                  P9_2_13 == "1"~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_8_viol_fam &
                                                  P9_2_15 == "1"~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_9_secuestro &
                                                  P9_2_17 == "1" | P9_2_23 == "1" ~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_10_sexuales &
                                                  P9_2_18 == "1" | P9_2_21 == "1" ~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_11_extorsion &
                                                  P9_2_22 == "1"~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_12_fraude &
                                                  P9_2_19 == "1" | P9_2_24 == "1" ~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_13_amenazas &
                                                  P9_2_25 == "1"~ "Mismo delito",
                                                reincidencia == "reincidentes" & Delito_gr_14_otro &
                                                  P9_2_14 == "1" | P9_2_16 == "1" | P9_2_26 == "1"~ "Mismo delito",
                                                reincidencia == "No reincidentes" ~ "No reincidentes",
                                                T ~ "Distinto delito"))
        
        
        
        data2plot <- Main_database_2008 %>% 
          select(reincidentes_tipo) %>% 
          group_by(reincidentes_tipo) %>% 
          summarise(Frequency = n(), .groups = 'drop') %>% 
          drop_na() %>% 
          rename(values = reincidentes_tipo) %>% 
          mutate(
            value2plot = Frequency / sum(Frequency) * 100,
            figure = paste0(round(value2plot, 0), "%"),
            labels = str_wrap(values, width = 20),
            ymin = c(0, head(value2plot, -1)))
        
        
        colors4plot <- c("Mismo delito" = "#2a2a9A", 
                         "No reincidentes" = "#a90099", 
                         "Distinto delito" = "#3273ff")
        
        selected <- c("No reincidentes")
        
        plot <- data2plot %>% 
          ggplot(aes(
            ymax=value2plot, 
            ymin=ymin, 
            xmax=4, 
            xmin=3, 
            fill=values)) +
          geom_rect( ) +
          coord_polar(theta="y") + 
          xlim(c(2, 4)) +
          geom_text( x= 3.5,
                     aes(y    = value2plot -3, 
                         label = figure), 
                     #position = "stack",
                     color    = "white",
                     family   = "Lato Full",
                     fontface = "bold", 
                     size = 4, 
                     data = data2plot %>% ungroup() %>%
                       filter(!values %in% selected)) +
          geom_text( x= 3.5,
                     aes(y    = value2plot -35, 
                         label = figure), 
                     #position = "stack",
                     color    = "white",
                     family   = "Lato Full",
                     fontface = "bold", 
                     size = 4, 
                     data = data2plot %>% ungroup() %>%
                       filter(values %in% selected)) +
          scale_fill_manual(values =  colors4plot)+
          theme_void() +
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
            legend.position = "none"); plot
        
        ggsave(plot   = plot,
               file   = paste0(path2SP,"/National/Visualization",
                               "/Output/Politica criminal/",
                               savePath,"/Delincuencia prolifica/Figure2_1.svg"), 
               width  = 189.7883, 
               height = 70,
               units  = "mm",
               dpi    = 72,
               device = "svg")
        
        
        return(data2plot)

}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Tipo delito sancionado en reincidentes                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

delito_reincidencia.fn <- function(
    
  data.df = master_data.df
  
  ){
  
  
      Main_database_2008 <- data.df %>% 
        filter(Anio_arresto >= 2008,
               NSJP == 1) %>% 
        mutate(reincidencia = case_when(P9_1 == "1" ~ "reincidentes",
                                        P9_1 == "2" ~ "No reincidentes",
                                        T ~ NA_character_)) %>% 
        filter(reincidencia == "reincidentes") %>%
        mutate(reincidentes_tipo = case_when(Delito_gr_1_robos == 1 &
                                               P9_2_01 == "1" | P9_2_02 == "1" | P9_2_03 == "1" | 
                                               P9_2_04 == "1" | P9_2_05 == "1" | P9_2_06 == "1" |
                                               P9_2_07 == "1" ~ "Robos",
                                             Delito_gr_2_drogas == 1 &
                                               P9_2_08 == "1" | P9_2_09 == "1"  ~ "Posesión o comercio\n de drogas",
                                             Delito_gr_3_del_org == 1 &
                                               P9_2_20 == "1"   ~ "Mismo delito-organizada",
                                             Delito_gr_4_lesiones &
                                               P9_2_10 == "1"~ "Mismo delito-lesiones",
                                             Delito_gr_5_hom_cul &
                                               P9_2_11 == "1"~ "Mismo delito-hom_cul",
                                             Delito_gr_6_hom_dol &
                                               P9_2_12 == "1"~ "Homicidio doloso",
                                             Delito_gr_7_armas &
                                               P9_2_13 == "1"~ "Portación ilegal\n de armas",
                                             Delito_gr_8_viol_fam &
                                               P9_2_15 == "1"~ "Mismo delito-viol_fam",
                                             Delito_gr_9_secuestro &
                                               P9_2_17 == "1" | P9_2_23 == "1" ~ "Secuestro",
                                             Delito_gr_10_sexuales &
                                               P9_2_18 == "1" | P9_2_21 == "1" ~ "Delitos sexuales",
                                             Delito_gr_11_extorsion &
                                               P9_2_22 == "1"~ "Mismo delito-extorsion",
                                             Delito_gr_12_fraude &
                                               P9_2_19 == "1" | P9_2_24 == "1" ~ "Mismo delito_fraude",
                                             Delito_gr_13_amenazas &
                                               P9_2_25 == "1"~ "Mismo delito_amenazas",
                                             Delito_gr_14_otro &
                                               P9_2_14 == "1" | P9_2_16 == "1" | P9_2_26 == "1"~ "Otro",
                                             T ~ "Distinto delito"))
      
      
      data2plot <- Main_database_2008 %>%
        group_by(reincidentes_tipo) %>%
        summarise(n = n()) %>%
        mutate(value2plot =  100 * n / sum(n),
               labels = paste0(round(value2plot,0),"%"),
               Delito = reincidentes_tipo,
               Delito = str_wrap(Delito, width = 30)) %>%
        select(Delito,value2plot,labels) %>%
        arrange(value2plot) %>%
        mutate(Delito = factor(Delito, levels = Delito)) %>% 
        filter(value2plot >= 1,
               Delito != "Distinto delito")
      
      colors4plot <- rep("#2a2a9A", 7)
      
      
      plt <- ggplot(data2plot, 
                    aes(x     = Delito,
                        y     = value2plot,
                        label = labels,
                        color = Delito)) +
        geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
                 show.legend = F, width = 0.9) +
        scale_fill_manual(values = colors4plot) +
        geom_text(aes(y    = value2plot + 5 ),
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
              axis.text.y=element_text(family   = "Lato Full",
                                       face     = "bold",
                                       size     = 3.514598*.pt,
                                       color    = "#524F4C",
                                       margin   = margin(0, 10, 0, 0),
                                       hjust = 0),
              legend.title = element_blank())+
        coord_flip()
      
      plt
      
      
      ggsave(plot   = plt,
             file   = paste0(path2SP,"/National/Visualization",
                             "/Output/Politica criminal/",
                             savePath,"/Delincuencia prolifica/Figure2_2.svg"), 
             width  = 189.7883, 
             height = 65,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      return(data2plot)

}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Proporción de personas que fueron sentenciadas por más de un delito                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

varios_delitos.fn <- function(
    
  data.df = master_data.df
  
  ){

  
      Main_database_2008 <- data.df %>% 
      filter(sentenciado == 1) %>% 
      mutate(Delito_unico = case_when( Delito_unico == 1 ~ "Sólo un delito",
                                       Delito_unico == 0 ~ "Más de un delito",
                                       T ~ NA_character_))
    
    data2plot <- Main_database_2008 %>% 
      select(Delito_unico) %>% 
      group_by(Delito_unico) %>% 
      summarise(Frequency = n(), .groups = 'drop') %>% 
      drop_na() %>% 
      rename(values = Delito_unico) %>% 
      mutate(
        value2plot = Frequency / sum(Frequency) * 100,
        figure = paste0(round(value2plot, 0), "%"),
        labels = str_wrap(values, width = 20),
        ymin = c(0, head(value2plot, -1)))
    
    selected <- c("Sólo un delito")
    
    
    plot <- data2plot %>% 
      ggplot(aes(
        ymax=value2plot, 
        ymin=ymin, 
        xmax=4, 
        xmin=3, 
        fill=values)) +
      geom_rect() +
      coord_polar(theta="y") + 
      xlim(c(2, 4)) +
      geom_text( x= 3.5,
                 aes(y    = value2plot-4 , 
                     label = paste0(figure)), 
                 # position = "stack",
                 color    = "white",
                 family   = "Lato Full",
                 fontface = "bold", 
                 size = 4.514598, 
                 data = data2plot %>% ungroup() %>%
                   filter(!values %in% selected)) +
      geom_text( x= 3.5,
                 aes(y    = value2plot-40 , 
                     label = paste0(figure)), 
                 # position = "stack",
                 color    = "white",
                 family   = "Lato Full",
                 fontface = "bold", 
                 size = 4.514598, 
                 data = data2plot %>% ungroup() %>%
                   filter(values %in% selected)) +
      scale_fill_manual(values =  c("#2a2a9A","#a90099"))+
      theme_void() +
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
        legend.position      = "none"); plot
    
    ggsave(plot   = plot,
           file   = paste0(path2SP,"/National/Visualization",
                           "/Output/Politica criminal/",
                           savePath,"/Delincuencia prolifica/Figure2_3.svg"), 
           width  = 189.7883, 
           height = 85,
           units  = "mm",
           dpi    = 72,
           device = "svg")
    
    return(data2plot)
    
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Proporción de personas que fueron sentenciadas por más de un delito                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

delito_unico.fn <- function(
    
  data.df = master_data.df
  
  ){

      delitos_data <- data.df      %>% 
        filter(sentenciado == 1,
               Delito_unico == 0)        %>% 
        select(starts_with("Delito_gr")) %>% 
        mutate_all(as.numeric) %>%
        select(Delito_gr_1_robos, 
               Delito_gr_2_drogas, 
               Delito_gr_7_armas, 
               Delito_gr_6_hom_dol,
               Delito_gr_11_extorsion)   %>% 
        mutate(total = sum(c_across(where(is.numeric)))) %>% 
        filter(total >= 2) %>% 
        mutate(Delito_gr_1_robos = case_when(Delito_gr_1_robos          == 1 ~ "Robos", 
                                             T ~ "0"), 
               Delito_gr_2_drogas = case_when(Delito_gr_2_drogas        == 1 ~ "Drogas", 
                                              T ~ "0"), 
               Delito_gr_7_armas = case_when(Delito_gr_7_armas          == 1 ~ "Armas", 
                                             T ~ "0"), 
               Delito_gr_6_hom_dol = case_when(Delito_gr_6_hom_dol       == 1 ~ "Homicidio doloso", 
                                               T ~ "0"),
               Delito_gr_11_extorsion = case_when(Delito_gr_11_extorsion == 1 ~ "Extorsión", 
                                                  T ~ "0")) %>%
        rowwise() %>%
        mutate(concat = paste(c_across(where(is.character)), collapse = " ")) %>%
        mutate(concat = str_replace_all(concat, "0", ""),
               concat = str_replace_all(concat, " ", ""),
               concat = case_when(concat == "DrogasArmas" ~ "Drogas y Armas",
                                   concat == "RobosArmas" ~ "Robos y Armas",
                                   concat == "RobosHomicidiodoloso" ~ "Robos y Homicidio doloso",
                                   concat == "RobosDrogas" ~ "Robos y Drogas",
                                   concat == "DrogasHomicidiodoloso" ~ "Drogas y Homicidio doloso",
                                   concat == "ArmasHomicidiodoloso" ~ "Armas y Homicidio doloso",
                                   concat == "RobosDrogasArmas" ~ "Robos, Drogas y Armas",
                                   concat == "RobosExtorsión" ~ "Robos y Extorsión",
                                   T ~ "Otras combinaciones"))
                            
      
      data2plot <- delitos_data %>%
        group_by(concat) %>%
        summarise(n = n()) %>%
        mutate(value2plot =  100 * n / sum(n),
               labels = paste0(round(value2plot,0),"%"),
               Delito = concat,
               Delito = str_wrap(Delito, width = 30)) %>%
        select(Delito,value2plot,labels) %>%
        arrange(value2plot) %>%
        mutate(Delito = factor(Delito, levels = Delito)) 
      
      colors4plot <- rep("#2a2a9A", 9)
      
      
      plt <- ggplot(data2plot, 
                    aes(x     = Delito,
                        y     = value2plot,
                        label = labels,
                        color = Delito)) +
        geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
                 show.legend = F, width = 0.9) +
        scale_fill_manual(values = colors4plot) +
        geom_text(aes(y    = value2plot + 5 ),
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
              axis.text.y=element_text(family   = "Lato Full",
                                       face     = "bold",
                                       size     = 3.514598*.pt,
                                       color    = "#524F4C",
                                       margin   = margin(0, 10, 0, 0),
                                       hjust = 0),
              legend.title = element_blank())+
        coord_flip()
      
      plt
      
      
      ggsave(plot   = plt,
             file   = paste0(path2SP,"/National/Visualization",
                                     "/Output/Politica criminal/",
                                     savePath,"/Delincuencia prolifica/Figure2_4.svg"), 
             width  = 189.7883, 
             height = 65,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      return(data2plot)

}


