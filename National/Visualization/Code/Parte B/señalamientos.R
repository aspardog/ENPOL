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
## Outline: Política criminal - Inspecciones                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
# source("Code/Settings.R")
# 
# load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.11. Porcentage de personas señaladas                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

señalados.fn <- function(
    
  data.df = master_data.df
  
  ){

    Main_database_2008 <- data.df %>% 
      filter(Anio_arresto >= 2008,
             NSJP == 1) %>% 
      mutate(señalado = case_when(P4_10 == "1" ~ "señalado", 
                                  P4_10 == "2" ~ "no señalado",
                                  T ~ NA_character_))
    
    
    data2plot <- Main_database_2008 %>% 
      select(señalado) %>% 
      group_by(señalado) %>% 
      summarise(Frequency = n(), .groups = 'drop') %>% 
      drop_na() %>% 
      rename(values = señalado) %>% 
      mutate(
        value2plot = Frequency / sum(Frequency) * 100,
        figure = paste0(round(value2plot, 0), "%"),
        labels = str_wrap(values, width = 20),
        ymin = c(0, head(value2plot, -1)))
    
    selected <- c("señalado")
    
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
                 aes(y    = value2plot -15, 
                     label = figure), 
                 # position = "stack",
                 color    = "white",
                 family   = "Lato Full",
                 fontface = "bold", 
                 size = 4.514598, 
                 data = data2plot %>% ungroup() %>%
                   filter(!values %in% selected)) +
      geom_text( x= 3.5,
                 aes(y    = value2plot-15 , 
                     label = figure), 
                 # position = "stack",
                 color    = "white",
                 family   = "Lato Full",
                 fontface = "bold", 
                 size = 4.514598, 
                 data = data2plot %>% ungroup() %>%
                   filter(values %in% selected)) +
      scale_fill_manual(values =  c("#2a2a94","#a90099"))+
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
        legend.position = "none");plot
    
    ggsave(plot   = plot,
           file   = paste0(path2SP,"/National/Visualization",
                           "/Output/Politica criminal/",
                           savePath,"/Señalamientos/Figure3_1.svg"), 
           width  = 189.7883, 
           height = 80,
           units  = "mm",
           dpi    = 72,
           device = "svg")
    
    return(data2plot)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.1.12. Señalamientos (P.4.13 y P4.14); agrupar las gráficas  (cris)                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

señalamientos_condiciones.fn <- function(
    
  data.df = master_data.df
  
  ){
      df <- data.df %>% 
        filter(Anio_arresto >= 2008,
               NSJP == 1) %>% 
        mutate(señalado = case_when(P4_10 == "1" ~ "señalado", 
                                    P4_10 == "2" ~ "no señalado",
                                    T ~ NA_character_)) %>% 
        mutate(parecido_señalado = case_when( P4_13 == "3" | P4_13 == "4" ~ 1,
                                              P4_13 == "1" | P4_13 == "2"  ~ 0,
                                              T ~ NA_real_),
               abogado_señalado = case_when(P4_14_1 == "1" ~ 1,
                                            P4_14_1 == "2" ~ 0,
                                            T ~ NA_real_),
               autoridad_señalado = case_when(P4_14_2 == "1" ~ 1,
                                              P4_14_2 == "2" ~ 0,
                                              T ~ NA_real_),
               responsable_señalado = case_when(P4_14_9 == "1" ~ 1,
                                                P4_14_9 == "2" ~ 0,
                                                T ~ NA_real_)) %>% 
        select(ends_with("_señalado"))
      
      data2plot <- df %>%
        pivot_longer(everything(), names_to = "Column", values_to = "Percentage") %>% 
        drop_na() %>% 
        group_by(Column) %>% 
        summarise(across(everything(), ~ mean(. == 1, na.rm = TRUE) * 100)) %>% 
        rename(values = Column, 
               value2plot = Percentage) %>% 
        mutate(
          labels = case_when(values == "abogado_señalado"     ~ "Estaba presente su abogado defensor", 
                             values == "autoridad_señalado"   ~ "Estaba presente la autoridad que lo detuvo",
                             values == "parecido_señalado"    ~ "Fue mostrado con otras personas que se parecían poco o nada",
                             values == "responsable_señalado" ~ "Resultó identificado como responsable por el testigo"),
          figure = paste0(round(value2plot, 0), "%"),
          labels = str_wrap(labels, width = 20),
          order_var = rank(value2plot)) %>%
        filter(values == "parecido_señalado" | values == "responsable_señalado")
      
      
      colors4plot <- rep("#2a2a94", length(data2plot$value2plot))
      
      
      plt <- ggplot(data2plot, 
                    aes(x     = reorder(labels, order_var),
                        y     = value2plot,
                        label = figure,
                        color = labels)) +
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
              axis.text.y=element_text(family = "Lato Medium",
                                       size = 3.514598*.pt,
                                       color = "Black", hjust = 0),
              legend.title = element_blank())+
        coord_flip(); plt
      
      ggsave(plot   = plt,
             file   = paste0(path2SP,"/National/Visualization",
                             "/Output/Politica criminal/",
                             savePath,"/Señalamientos/Figure3_2.svg"), 
             width  = 189.7883, 
             height = 80,
             units  = "mm",
             dpi    = 72,
             device = "svg")
      
      return(data2plot)

}
