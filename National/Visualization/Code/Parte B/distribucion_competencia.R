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
## 1. Delitos por tipo de fuero (barras)                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

delitos_fuero.fn <- function(
    
  data.df = master_data.df
  
  ){


    data2plot <- data.df %>%
      select(fuero) %>% 
      rename(Value =fuero) %>% 
      drop_na() %>% 
      group_by(Value) %>%
      summarise(Frequency = n()) %>% 
      mutate(Value = Value,
             values = Frequency/sum(Frequency),
             value2plot = values * 100,
             figure = paste0(round(value2plot, 0), "%"),
             labels = str_wrap(Value, width = 30),
             order_var = rank(Value)) 
    
    fill_colors = c("#3273ff", "#a90099", "#2a2a9A")
    
    plt <- data2plot %>%  
          ggplot(aes(x     = reorder(labels, order_var),
                      y     = value2plot,
                      label = figure,
                      fill  = labels)) +
      geom_bar(stat = "identity",
               show.legend = FALSE, width = 0.9) +
      scale_fill_manual(values = fill_colors) +
      geom_text(aes(y    = value2plot + 6),
                color    = "#4a4a49",
                family   = "Lato Full",
                fontface = "bold") +
      labs(y = "% of respondents") +
      scale_y_continuous(limits = c(0, 105),
                         breaks = seq(0, 100, 20),
                         labels = paste0(seq(0, 100, 20), "%"),
                         position = "left") +
      scale_x_discrete(limits = rev) +
      WJP_theme() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#D0D1D3"),
            axis.title.y       = element_blank(),
            axis.title.x       = element_blank(),
            axis.text.y        = element_text(hjust = 1, size = 10),
            plot.title = element_text(face = "bold", size = 12))
      
    plt
    
    
    ggsave(plot   = plt,
           file   = paste0(path2SP,"/National/Visualization",
                           "/Output/Politica criminal/",
                           savePath,"/Distribucion competencias/Figure3_1.svg"), 
           width  = 189.7883, 
           height = 85,
           units  = "mm",
           dpi    = 72,
           device = "svg")
    
    return(data2plot)

}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## B.2.1.2. Proporción de delitos federales respecto del total de delitos registrados por estado (mapa)                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

delitos_federales.fn <- function(
    
  data.df= master_data.df
  
  ){

    Estados <- data.df %>% 
      select(Estado_arresto, fuero) %>% 
      group_by(Estado_arresto, fuero) %>%
      summarise(Frequency = n(), .groups = 'drop') %>% 
      group_by(Estado_arresto) %>%
      mutate(Percentage = Frequency / sum(Frequency) * 100) %>% 
      filter (fuero  == "Sólo federal") %>% 
      drop_na() %>%
      select(Estado_arresto, Percentage) %>%
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
    
    promedio_nacional <- mean(Estados$Percentage)
    
    Estados <- Estados %>%
               ungroup() %>% 
                add_row(
                        ESTADO          = "ANacional",
                        Percentage      = promedio_nacional)
    
    quintiles <- quantile(round(Estados$Percentage, 0), probs = seq(0, 1, by = 0.2))
    
    
    mapa <- st_read(paste0(path2SP,"/National/Exploration/Input/shp/México_Estados.shp")) %>%
      mutate( ESTADO = 
                case_when(
                  ESTADO == "México" ~ "Estado de México",
                  ESTADO == "Distrito Federal" ~ "Ciudad de México",
                  T ~ ESTADO
                )
      )
    
    table <- Estados %>%
      mutate(
        ` ` = "",
        `%` = round(Percentage, 0)
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
      
      padding(j = 1, padding.right = 30) %>%
      padding(j = 1, padding.left  = 10) %>%
      padding(j = 3, padding.left  = 10) %>%
      
      width(j = " ", width = 0.5, unit = "mm") %>%
      width(j = "%", width = 0.75,   unit = "mm") %>%
    
      
      bg(i = ~ `%` >= 5   & `%` <= 10,   j = ' ',   bg = "#99D7DD", part = "body") %>%
      bg(i = ~ `%` > 10   & `%` <= 12.4, j = ' ',   bg = "#33AEBA", part = "body") %>%
      bg(i = ~ `%` > 12.4 & `%` <= 18.2, j = ' ',   bg = "#0087A3", part = "body") %>%
      bg(i = ~ `%` > 18.2 & `%` <= 23,   j = ' ',   bg = "#00617F", part = "body") %>%
      bg(i = ~ `%` > 23   & `%` <= 37,  j = ' ',    bg = "#004E70", part = "body") %>%  
      
      
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
      )
    
    tpanel <- gen_grob(table, 
                       fit      = "auto",
                       scaling  = "min", 
                       just     = c("left", "top"),
                       wrapping = T)
    
    mexico_map <- mapa %>%
      left_join(Estados, by = "ESTADO") %>%
      mutate(value2plot = round(Percentage), 0) %>%
      mutate(
        color_group = case_when(
          value2plot >= 5   & value2plot <= 10   ~ "5.1%-9.8%",
          value2plot > 10   & value2plot <= 12.4 ~ "9.8%-12.6%",
          value2plot > 12.4 & value2plot <= 18.2 ~ "12.6%-18.2%",
          value2plot > 18.2 & value2plot <= 23   ~ "18.2%-23.3%",
          value2plot > 23   & value2plot <= 37   ~ "23.3%-36.8%"
        ),
        color_group = as.factor(color_group)
      )
    
    cat_palette <- c( "5.1%-9.8%"  = "#99D7DD",
                      "9.8%-12.6%"  = "#33AEBA",
                      "12.6%-18.2%"  = "#0087A3",
                      "18.2%-23.3%"  = "#00617F",
                      "23.3%-36.8%" = "#004E70")
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
    
    categories <- c("[5%-10%]",
                    "(10%-13%]",
                    "(13%-18%]",
                    "(18%-23%]",
                    "(23%-37%]")
    
    leyend <- data.frame(
      Values = categories,
      Blank = "")
    leyend <- flextable(leyend)  %>% 
      width(j = "Blank", width = 0.5, unit = "mm") %>% 
      set_header_labels(Values = "Escala", Blank = " ") %>% 
      bg(i = ~ Values == "[5%-10%]", j = "Blank", bg = "#99D7DD", part = "body") %>%
      bg(i = ~ Values == "(10%-13%]", j = "Blank", bg = "#33AEBA", part = "body") %>%
      bg(i = ~ Values == "(13%-18%]", j = "Blank", bg = "#0087A3", part = "body") %>%
      bg(i = ~ Values == "(18%-23%]", j = "Blank", bg = "#00617F", part = "body") %>%
      bg(i = ~ Values == "(23%-37%]", j = "Blank", bg = "#004E70", part = "body") %>%  
      
      
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
    
    # ggsave(plot = viz, filename = "Input/map_ejemplo.svg", width = 10, height = 10)
    
    
    ggsave(plot   = viz,
           file   = paste0(path2SP,"/National/Visualization",
                           "/Output/Politica criminal/",
                           savePath,"/Distribucion competencias/Figure3_2.svg"), 
           width  = 189.7883, 
           height = 175,
           units  = "mm",
           dpi    = 72,
           device = "svg")

}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## B.2.1.3 Proporción de detenciones realizadas por corporaciones federales de sólo los delitos federales registrados por estado (mapa)                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


detenciones_federales.fn <- function(
    
  data.df = master_data.df
  
  ){

      Main_database_2008 <- data.df %>% 
        filter(fuero == "Sólo federal" )
      
      # Brecha entre el porcentaje de detenciones por tipo de fuero ------------------------------
      
      Main_database_2008 <-  Main_database_2008 %>% 
        mutate(corporacion_fuero = case_when(Corporacion_grupos == "Ejército o Marina" ~ "Corporación Federal", 
                                             Corporacion_grupos == "Guardia Nacional" ~ "Corporación Federal",
                                             Corporacion_grupos == "Policía Federal" ~ "Corporación Federal",
                                             Corporacion_grupos == "Policía Federal Ministerial" ~ "Corporación Federal",
                                             Corporacion_grupos == "Policía Estatal Ministerial o Judicial" ~ "Corporación Local",
                                             Corporacion_grupos == "Operativo Conjunto" ~ "Operativo Conjunto",
                                             Corporacion_grupos == "Policía Estatal" ~ "Corporación Local", 
                                             Corporacion_grupos == "Policía Municipal" ~ "Corporación Local",
                                             Corporacion_grupos == "Otra" ~ "Otra", 
                                             T ~ NA_character_))
      
      
      
      Estados <- Main_database_2008 %>% 
        select(Estado_arresto, corporacion_fuero) %>% 
        group_by(Estado_arresto, corporacion_fuero) %>%
        summarise(Frequency = n(), .groups = 'drop') %>% 
        group_by(Estado_arresto) %>%
        filter (corporacion_fuero  != "Operativo Conjunto", 
                corporacion_fuero != "Otra") %>% 
        mutate(Percentage = Frequency / sum(Frequency) * 100) %>% 
        select(-Frequency) %>% 
        pivot_wider(names_from = corporacion_fuero, values_from = Percentage) %>% 
        mutate(gap = round(`Corporación Federal`),0)%>% 
        select(Estado_arresto, gap) %>%
        rename(ESTADO = Estado_arresto) %>%
        drop_na() %>%
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
      
      quintiles <- round(quantile(round(Estados$gap, 0), probs = seq(0, 1, by = 0.2)),0)
      
      promedio_nacional <- mean(Estados$gap)
      
      Estados <- Estados %>%
        ungroup() %>% 
        add_row(
          ESTADO          = "ANacional",
          gap      = promedio_nacional)
      
      mapa <- st_read(paste0(path2SP,"/National/Exploration/Input/shp/México_Estados.shp")) %>%
        mutate( ESTADO = 
                  case_when(
                    ESTADO == "México" ~ "Estado de México",
                    ESTADO == "Distrito Federal" ~ "Ciudad de México",
                    T ~ ESTADO
                  )
        )
      
      table <- Estados %>%
        mutate(
          ` ` = "",
          `%` = round(gap, 0)
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
        
        padding(j = 1, padding.right = 30) %>%
        padding(j = 1, padding.left  = 10) %>%
        padding(j = 3, padding.left  = 10) %>%
        
        width(j = " ", width = 0.5, unit = "mm") %>%
        width(j = "%", width = 0.75,   unit = "mm") %>%
        
        
        bg(i = ~ `%` >= 21 & `%` <= 41, j = ' ', bg = "#FF7E8A", part = "body") %>%
        bg(i = ~ `%` > 41 & `%` <= 46, j = ' ', bg = "#FFCBD0", part = "body") %>%
        bg(i = ~ `%` > 46 & `%` <= 52, j = ' ', bg = "#FFEDCC", part = "body") %>%
        bg(i = ~ `%` > 52 & `%` <= 61, j = ' ', bg = "#99D7DD", part = "body") %>%
        bg(i = ~ `%` > 61 & `%` <= 86, j = ' ', bg = "#33AEBA", part = "body") %>%  
        bg(i = ~ Estado == "Campeche", j = ' ', bg = "#D6D3D3", part = "body") %>%
        bg(i = ~ Estado == "Yucatán", j = ' ', bg = "#D6D3D3", part = "body") %>%
        
        
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
        )
      
      tpanel <- gen_grob(table, 
                         fit      = "auto",
                         scaling  = "min", 
                         just     = c("left", "top"),
                         wrapping = T)
      
      mexico_map <- mapa %>%
        left_join(Estados, by = "ESTADO") %>%
        mutate(value2plot = round(gap), 0) %>%
        mutate(
          color_group = case_when(
            ESTADO == "Campeche" | ESTADO == "Yucatán"            ~ "none",
            value2plot >= 21 & value2plot <= 41 ~ "21%-41%",
            value2plot > 41 & value2plot <= 46 ~ "41%-46%",
            value2plot > 46 & value2plot <= 52 ~ "46%-52%",
            value2plot > 52 & value2plot <= 61 ~ "52%-61%",
            value2plot > 61 & value2plot <= 86 ~ "61%-86%",
          ),
          color_group = as.factor(color_group)
        )
      
      cat_palette <- c(
        "21%-41%"  = "#FF7E8A",
        "41%-46%"  = "#FFCBD0",
        "46%-52%"  = "#FFEDCC",
        "52%-61%"  = "#99D7DD",
        "61%-86%"  = "#33AEBA",
        "none"     = "#D6D3D3")
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
      
      categories <- c("(21%-41%]",
                      "(41%-46%]",
                      "(46%-52%]",
                      "(52%-61%]",
                      "(61%-86%]",
                      "No info.")
      
      leyend <- data.frame(
        Values = categories,
        Blank = "")
      leyend <- flextable(leyend)  %>% 
        width(j = "Blank", width = 0.5, unit = "mm") %>% 
        set_header_labels(Values = "Escala", Blank = " ") %>% 
        bg(i = ~ Values == "(21%-41%]", j = "Blank",  bg = "#FF7E8A", part = "body") %>%
        bg(i = ~ Values == "(41%-46%]", j = "Blank",  bg = "#FFCBD0", part = "body") %>%
        bg(i = ~ Values == "(46%-52%]", j = "Blank",  bg = "#FFEDCC", part = "body") %>%
        bg(i = ~ Values == "(52%-61%]", j = "Blank",  bg = "#99D7DD", part = "body") %>%
        bg(i = ~ Values == "(61%-86%]", j = "Blank",  bg = "#33AEBA", part = "body") %>%
        bg(i = ~ Values == "No info.", j = "Blank", bg = "#D6D3D3", part = "body") %>%
        
        
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
      
      
      
      
      ggsave(plot   = viz,
             file   = paste0(path2SP,"/National/Visualization",
                             "/Output/Politica criminal/",
                             savePath,"/Distribucion competencias/Figure3_3.svg"), 
             width  = 189.7883, 
             height = 175,
             units  = "mm",
             dpi    = 72,
             device = "svg")

}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## B.2.1.4 Proporción de detenciones realizadas por corporaciones estatales de sólo los delitos del fuero común registrados por estado (mapa)                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

detenciones_estatales.fn <- function(
    
  data.df = master_data.df
  
  ) {

  
    Main_database_2008 <- data.df %>% 
    filter(fuero == "Sólo común" )
    
    
    # Brecha entre el porcentaje de detenciones por tipo de fuero ------------------------------
    
    Main_database_2008 <-  Main_database_2008 %>% 
      mutate(corporacion_fuero = case_when(Corporacion_grupos == "Ejército o Marina" ~ "Corporación Federal", 
                                           Corporacion_grupos == "Guardia Nacional" ~ "Corporación Federal",
                                           Corporacion_grupos == "Policía Federal" ~ "Corporación Federal",
                                           Corporacion_grupos == "Policía Federal Ministerial" ~ "Corporación Federal",
                                           Corporacion_grupos == "Policía Estatal Ministerial o Judicial" ~ "Corporación Local",
                                           Corporacion_grupos == "Operativo Conjunto" ~ "Operativo Conjunto",
                                           Corporacion_grupos == "Policía Estatal" ~ "Corporación Local", 
                                           Corporacion_grupos == "Policía Municipal" ~ "Corporación Local",
                                           Corporacion_grupos == "Otra" ~ "Otra", 
                                           T ~ NA_character_))
    
    
    
    Estados <- Main_database_2008 %>% 
      select(Estado_arresto, corporacion_fuero) %>% 
      group_by(Estado_arresto, corporacion_fuero) %>%
      summarise(Frequency = n(), .groups = 'drop') %>% 
      group_by(Estado_arresto) %>%
      filter (corporacion_fuero  != "Operativo Conjunto", 
              corporacion_fuero != "Otra") %>% 
      mutate(Percentage = Frequency / sum(Frequency) * 100) %>% 
      select(-Frequency) %>% 
      pivot_wider(names_from = corporacion_fuero, values_from = Percentage) %>% 
      mutate(gap = round(`Corporación Local` - `Corporación Federal`),0)%>% 
      select(Estado_arresto, gap) %>%
      rename(ESTADO = Estado_arresto) %>%
      drop_na() %>%
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
    
    quintiles <- round(quantile(round(Estados$gap, 0), probs = seq(0, 1, by = 0.2)),0)
    
    promedio_nacional <- mean(Estados$gap)

    Estados <- Estados %>%
      ungroup() %>% 
      add_row(
        ESTADO          = "ANacional",
        gap      = promedio_nacional)
        
    mapa <- st_read(paste0(path2SP,"/National/Exploration/Input/shp/México_Estados.shp")) %>%
      mutate( ESTADO = 
                case_when(
                  ESTADO == "México" ~ "Estado de México",
                  ESTADO == "Distrito Federal" ~ "Ciudad de México",
                  T ~ ESTADO
                )
      )
    
    table <- Estados %>%
      mutate(
        ` ` = "",
        `%` = round(gap, 0)
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
      
      padding(j = 1, padding.right = 30) %>%
      padding(j = 1, padding.left  = 10) %>%
      padding(j = 3, padding.left  = 10) %>%
      
      width(j = " ", width = 0.5, unit = "mm") %>%
      width(j = "%", width = 0.75,   unit = "mm") %>%
      
      
      bg(i = ~ `%` >= 50 & `%` <= 76, j = ' ', bg = "#FF7E8A", part = "body") %>%
      bg(i = ~ `%` > 76 & `%` <= 82, j = ' ', bg = "#FFCBD0", part = "body") %>%
      bg(i = ~ `%` > 82 & `%` <= 86, j = ' ', bg = "#FFEDCC", part = "body") %>%
      bg(i = ~ `%` > 86 & `%` <= 88, j = ' ', bg = "#99D7DD", part = "body") %>%
      bg(i = ~ `%` > 88 & `%` <= 96, j = ' ', bg = "#33AEBA", part = "body") %>%  
      
      
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
      )
    
    tpanel <- gen_grob(table, 
                       fit      = "auto",
                       scaling  = "min", 
                       just     = c("left", "top"),
                       wrapping = T)
    
    mexico_map <- mapa %>%
      left_join(Estados, by = "ESTADO") %>%
      mutate(value2plot = round(gap), 0) %>%
      mutate(
        color_group = case_when(
          value2plot >= 50 & value2plot <= 76 ~ "50%-76%",
          value2plot > 76 & value2plot <= 82 ~ "76%-82%",
          value2plot > 82 & value2plot <= 86 ~ "82%-86%",
          value2plot > 86 & value2plot <= 88 ~ "86%-88%",
          value2plot > 88 & value2plot <= 96 ~ "88%-96%"
        ),
        color_group = as.factor(color_group)
      )
    
    cat_palette <- c("50%-76%"  = "#FF7E8A",
                     "76%-82%"  = "#FFCBD0",
                     "82%-86%"  = "#FFEDCC",
                     "86%-88%"  = "#99D7DD",
                     "88%-96%" = "#33AEBA")
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
    
    categories <- c("[50%-76%]",
                    "(76%-82%]",
                    "(82%-86%]",
                    "(86%-88%]",
                    "(88%-96%]")
    
    leyend <- data.frame(
      Values = categories,
      Blank = "")
    leyend <- flextable(leyend)  %>% 
      width(j = "Blank", width = 0.5, unit = "mm") %>% 
      set_header_labels(Values = "Escala", Blank = " ") %>% 
      bg(i = ~ Values == "[50%-76%]", j = "Blank", bg = "#FF7E8A", part = "body") %>%
      bg(i = ~ Values == "(76%-82%]", j = "Blank", bg = "#FFCBD0", part = "body") %>%
      bg(i = ~ Values == "(82%-86%]", j = "Blank", bg = "#FFEDCC", part = "body") %>%
      bg(i = ~ Values == "(86%-88%]", j = "Blank", bg = "#99D7DD", part = "body") %>%
      bg(i = ~ Values == "(88%-96%]", j = "Blank", bg = "#33AEBA", part = "body") %>%  
      
      
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
    
    ggsave(plot   = viz,
           file   = paste0(path2SP,"/National/Visualization",
                           "/Output/Politica criminal/",
                           savePath,"/Distribucion competencias/Figure3_4.svg"), 
           width  = 189.7883, 
           height = 175,
           units  = "mm",
           dpi    = 72,
           device = "svg")
}


