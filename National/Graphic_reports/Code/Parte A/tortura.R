## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Tortura
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
## Tortura: Serie temporal                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tortura_tiempo.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- master_data.df %>%
    filter(Anio_arresto > 2014)  %>%
    mutate(counter = 1) %>%
    group_by(Anio_arresto) %>%
    summarise(
      value2plot = mean(tortura_generalizada, na.rm = T),
      n_obs = sum(counter, na.rm = T)
    ) %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           category = "tortura",
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
  names(colors4plot) <- "tortura"
  
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
                           savePath,"/Tortura",
                           "/tortura_tiempo.svg"),
         width = 189.7883,
         height = 68.88612,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Tortura: RND                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tortura_RND.fn <- function(
    
  data.df = master_data.df  
  
) {
  
  data_subset.df <- master_data.df %>%
    select(years_since_RND_3, tortura_tra_p, tortura_tra_f, tortura_mp_f, tortura_mp_p) %>%
    mutate(
      across(!years_since_RND_3,
             ~case_when(
               .x == 1 ~ 1,
               .x == 0 | .x == 2 ~ 0,
               T ~ NA_real_
             )),
      group_var = 
        if_else(
          as.numeric(years_since_RND_3) < 2 & as.numeric(years_since_RND_3) > 0, "Post RND",
          if_else(
            as.numeric(years_since_RND_3) > -2 & as.numeric(years_since_RND_3) < 0, "PRE RND", 
            NA_character_
          )
        )
    ) %>%
    select(!years_since_RND_3) %>%
    mutate(
      tortura_fisica = 
        case_when(
          tortura_tra_f == 1 | tortura_mp_f == 1 ~ 1,
          tortura_tra_f == 0 & tortura_mp_f == 0 ~ 0,
          T ~ NA_real_
        ),
      tortura_psicologica = 
        case_when(
          tortura_tra_p == 1 | tortura_mp_p == 1 ~ 1,
          tortura_tra_p == 0 & tortura_mp_p == 0 ~ 0,
          T ~ NA_real_
        ),
      tortura_ambas = 
        case_when(
          tortura_fisica == 1 & tortura_psicologica == 1 ~ 1,
          tortura_fisica == 0 | tortura_psicologica == 0 ~ 0,
          T ~ NA_real_
        )
    ) %>%
    select(!tortura_tra_p) %>%
    select(!tortura_tra_f) %>%
    select(!tortura_mp_f) %>%
    select(!tortura_mp_p) %>%
    pivot_longer(cols = !group_var, 
                 names_to = "category", 
                 values_to = "value2plot") %>%
    group_by(category, group_var) %>%
    summarise(value2plot = mean(value2plot, na.rm = T)*100) %>%
    drop_na()
  
  data2plot <- data_subset.df %>%
    mutate(
      value2plot = round(value2plot, 0),
      labels = 
        case_when(
          category == "tortura_fisica" ~ "Tortura <br>física",
          category == "tortura_psicologica" ~ "Tortura <br>psicológica",
          category == "tortura_ambas" ~ "Ambas"
        ),
      figure = paste0(value2plot, "%"),
      order_var = case_when(
        labels == "Ambas" ~ 3,
        labels == "Tortura física" ~ 2,
        labels =="Tortura psicológica" ~ 1,
        T ~ NA_real_),
      order_value_bars = case_when(
        labels == "PRE RND" ~ 1,
        labels == "Post RND" ~ 2,
        T ~ NA_real_)
    )
  
  colors4plot <- c("#FA4D57",
                   "#009AA9")
  
  names(colors4plot) <- c("PRE RND",
                          "Post RND")
  
  plot <- barsChart.fn(
    data.df                    = data2plot,
    categories_grouping_var    = data2plot$group_var,
    colors4plot                = colors4plot, 
    nbars = 3
  )
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Tortura",
                           "/tortura_RND.svg"),
         width = 189.7883,
         height = 105,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Tortura tipo                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tortura_tipo.fn <- function(
  
  data.df = master_data.df  
  
) {
  
  data_subset.df <- master_data.df %>%
    select(tortura_tra_p, tortura_tra_f, tortura_mp_f, tortura_mp_p) %>%
    mutate(
      across(everything(),
             ~case_when(
               .x == 1 ~ 1,
               .x == 0 | .x == 2 ~ 0,
               T ~ NA_real_
             ))
    ) %>%
    pivot_longer(cols = everything(), 
                 names_to = "category", 
                 values_to = "value2plot") %>%
    mutate(
      group_var = 
        if_else(
          grepl("tra_", category), 
          "Traslado", "MP")
    ) %>%
    group_by(category, group_var) %>%
    summarise(value2plot = mean(value2plot, na.rm = T)*100) 
  
  
  data2plot <- data_subset.df %>%
    mutate(
      labels =
        case_when(
          category == "tortura_tra_f" | category == "tortura_mp_f" ~"Tortura física",
          category == "tortura_tra_p" | category == "tortura_mp_p" ~"Tortura psicológica",
          T ~ NA_character_),
      figure = paste0(round(value2plot,0), "%"),
      order_var = case_when(
        labels == "Tortura física" ~ 2,
        labels =="Tortura psicológica" ~ 1,
        T ~ NA_real_),
      order_value_bars = 
        case_when(
          group_var == "Traslado" ~ 1,
          group_var == "MP" ~ 2
        )
    )
  
  colors4plot <- twoColors
  
  names(colors4plot) <- c("Traslado",
                          "MP")
  
  plot <- barsChart.fn(
    data.df                    = data2plot,
    categories_grouping_var    = data2plot$group_var,
    colors4plot                = colors4plot, 
    nbars = 2
  )
  
  ggsave(plot = plot, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Tortura",
                           "/tortura_tipo.svg"),
         width = 189.7883,
         height = 70,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  return(data2plot)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Tortura Psicologica                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tortura_psicologica.fn <- function(
    
  data.df = master_data.df
  
) {
  
  data_subset.df <- data.df %>%
    select(starts_with("P3_17_"), starts_with("P4_8_")) %>%
    select(!ends_with("_15")) %>%
    mutate(
      across(everything(),
             ~case_when(
               .x == 1 ~ 1,
               .x == 0 | .x == 2 ~ 0,
               T ~ NA_real_
             ))
    ) %>%
    pivot_longer(cols = everything(), 
                 names_to = "category", 
                 values_to = "value2plot") %>%
    mutate(
      group_var = 
        if_else(
          grepl("P3_17_", category), 
          "Traslado", "MP")
    ) %>%
    mutate(counter = 1) %>%
    group_by(category, group_var) %>%
    summarise(value2plot = mean(value2plot, na.rm = T),
              n_obs = sum(counter, na.rm = T)) %>%
    filter(category != "P4_8_1") %>%
    filter(category != "P3_17_05")
  
  
  data2plot <- data_subset.df %>%
    mutate(
      labels =
        case_when(
          category == "P3_17_01" | category == "P4_8_2" ~"¿Le amenazaron con levantarle cargos falsos?",
          category == "P3_17_02" | category == "P4_8_3" ~"¿Le amenazaron con matarlo(a)?",
          category == "P3_17_03" | category == "P4_8_4" ~"¿Le amenazaron con hacerle daño a usted?",
          category == "P3_17_04" | category == "P4_8_5" ~"¿Le amenazaron con hacerle daño a su familia?",
          category == "P3_17_06" | category == "P4_8_6" ~"¿Le presionaron para denunciar a alguien?",
          category == "P3_17_07" | category == "P4_8_7" ~"¿Le incomunicaron o aislaron?",
          category == "P3_17_08" | category == "P4_8_8" ~"¿Le pasearon en un automóvil dando vueltas por <br>las calles?",
          category == "P3_17_09" | category == "P4_8_9" ~"¿Le hicieron daño a su familia?",
          category == "P3_17_10" | category == "P4_8_10" ~"¿Le desvistieron?",
          category == "P3_17_11" | category == "P4_8_11" ~"¿Le vendaron los ojos o cubrieran la cabeza <br>para que no viera?",
          T ~ NA_character_),
      order_var = 
        case_when(
          labels == "¿Le amenazaron con levantarle cargos falsos?" ~ 2,
          labels =="¿Le amenazaron con matarlo(a)?"~ 7,
          labels =="¿Le amenazaron con hacerle daño a usted?" ~ 3,
          labels =="¿Le amenazaron con hacerle daño a su familia?"~ 9,
          labels =="¿Le presionaron para denunciar a alguien?"~ 8,
          labels =="¿Le incomunicaron o aislaron?"~ 1,
          labels =="¿Le pasearon en un automóvil dando vueltas por <br>las calles?" ~ 4,
          labels =="¿Le hicieron daño a su familia?"~ 10,
          labels == "¿Le desvistieron?"~ 6,
          labels =="¿Le vendaron los ojos o cubrieran la cabeza <br>para que no viera?"~ 5,
          T ~ NA_real_
        ),
      labels =
        case_when(
          labels == "¿Le amenazaron con levantarle cargos falsos?" ~ "Lo amenazaron con  <br> **levantarle cargos falsos**",
          labels =="¿Le amenazaron con matarlo(a)?"~ "Lo amenazaron con **matarlo**",
          labels =="¿Le amenazaron con hacerle daño a usted?" ~ "Lo amenazaron con <br> **hacerle** **daño**",
          labels =="¿Le amenazaron con hacerle daño a su familia?"~ "Lo amenazaron con <br> **hacerle daño** <br>  **a su familia**",
          labels =="¿Le presionaron para denunciar a alguien?"~ "Lo **presionaron** para <br> **denunciar a alguien**",
          labels =="¿Le incomunicaron o aislaron?"~ "Lo **incomunicaron** o <br> **aislaron**",
          labels =="¿Le pasearon en un automóvil dando vueltas por <br>las calles?" ~ "Lo **pasearon** en un <br>automóvil <br> **dando vueltas** por <br> las calles",
          labels =="¿Le hicieron daño a su familia?"~ "Le **hicieron daño** a <br> **su familia**", 
          labels == "¿Le desvistieron?"~ "Lo **desvistieron**",
          labels =="¿Le vendaron los ojos o cubrieran la cabeza <br>para que no viera?"~ "Le **vendaron los ojos** o <br> **cubrieron su cabeza** <br> para que no viera",
          T ~ NA_character_
        ),
      order_value_bars = 
        case_when(
          group_var == "Traslado" ~ 1,
          group_var == "MP" ~ 2
        ),
      latestYear = "MP"
    ) %>%
    rename(year = group_var) %>%
    mutate(
      figure1 =
        if_else(year == "MP", paste0(round(value2plot*100,0), "%"),  NA_character_)
      )
  
  figure2.df <- data2plot %>% 
    mutate(
      figure2 = if_else(year == "Traslado", paste0(round(value2plot*100,0), "%"),  NA_character_)
    ) %>%
    drop_na(figure2) %>%
    select(labels, figure2)
      
  data2plot <- data2plot %>%
    left_join(figure2.df, by = "labels") %>%
    mutate(
      category = labels,
      across(labels,
             ~paste0(
               "<span style='color:#2a2a9A;font-size:3.514598mm;font-weight:bold'>", figure2, "</span>",
               "<span> | </span>", 
               "<span style='color:#a90099;font-size:3.514598mm;font-weight:bold'>", figure1, "</span>",
               "<br>",
               "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",
               labels,
               "</span>")
      ),
      labels = 
        if_else(year == "Traslado", NA_character_, labels)
    )
  
  colors4plot <- twoColors
  latestYear <- "MP"
  
  names(colors4plot) <- c("Traslado",
                          "MP")
  
  chart <- ENPOL_radarChart(data          = data2plot,
                            axis_var      = "category",         
                            target_var    = "value2plot",     
                            label_var     = "labels", 
                            order_var     = "order_var",
                            colors        = colors4plot);chart
  
  ggsave(plot = chart, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Tortura",
                           "/tortura_psicologica.svg"),
         width = 200,
         height = 210,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Tortura Física: Mecanismos                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tortura_fisica.fn <- function(
  
  data.df = master_data.df  
  
) {
  
  data_subset.df <- data.df %>%
    select(starts_with("P3_18_"), starts_with("P4_9")) %>%
    select(!ends_with("_15")) %>%
    mutate(
      across(everything(),
             ~case_when(
               .x == 1 ~ 1,
               .x == 0 | .x == 2 ~ 0,
               T ~ NA_real_
             ))
    ) %>%
    pivot_longer(cols = everything(), 
                 names_to = "category", 
                 values_to = "value2plot") %>%
    mutate(
      group_var = 
        if_else(
          grepl("P3_18_", category), 
          "Traslado", "MP"),
      counter = 1
    ) %>%
    group_by(category, group_var) %>%
    summarise(value2plot = mean(value2plot, na.rm = T),
              n_obs = sum(counter, na.rm = T))
  
  
  data2plot <- data_subset.df %>%
    mutate(
      labels =
        case_when(
          category == "P3_18_01" | category == "P4_9_01" ~"¿Ataron su cuerpo; ataron alguna parte <br>de su cuerpo a un objeto?",
          category == "P3_18_02" | category == "P4_9_02" ~"¿Le impidieron respirar asfixiándolo, <br>ahorcándolo?",
          category == "P3_18_03" | category == "P4_9_03" ~"¿Le impidieron respirar o metiendo su <br>cabeza en agua o vaciándole agua en <br>la cara?",
          category == "P3_18_04" | category == "P4_9_04" ~"¿Le patearon o golpearon con las manos?",
          category == "P3_18_05" | category == "P4_9_05" ~"¿Le golpearon con objetos?",
          category == "P3_18_06" | category == "P4_9_06" ~"¿Le quemaron (con objetos calientes, <br>fuego u otra sustancia)?",
          category == "P3_18_07" | category == "P4_9_07" ~"¿Le dieron descargas eléctricas?",
          category == "P3_18_08" | category == "P4_9_08" ~"¿Aplastaron su cuerpo o alguna parte de <br>él con algún objeto o con el cuerpo de <br>otra persona?",
          category == "P3_18_09" | category == "P4_9_09" ~"¿Le hirieron con algún cuchillo, navaja <br>u otro objeto afilado?",
          category == "P3_18_10" | category == "P4_9_10" ~"¿Le encajaron agujas en dedos u otra <br>parte del cuerpo?",
          category == "P3_18_11" | category == "P4_9_11" ~"¿Le hirieron por el disparo de un arma <br>de fuego?",
          category == "P3_18_12" | category == "P4_9_12" ~"¿Le agredieron mediante acoso sexual, <br>manoseo, exhibicionismo o intento de <br>violación?",
          category == "P3_18_13" | category == "P4_9_13" ~"¿Le lastimaron sus órganos sexuales?",
          category == "P3_18_14" | category == "P4_9_14" ~"¿Fue obligado mediante violencia física <br>o amenaza a tener una actividad sexual <br>no deseada?",
          T ~ NA_character_
        ),
      order_var = 
        case_when(
          labels == "¿Ataron su cuerpo; ataron alguna parte <br>de su cuerpo a un objeto?" ~ 6,
          labels =="¿Le impidieron respirar asfixiándolo, <br>ahorcándolo?"~ 6,
          labels =="¿Le impidieron respirar o metiendo su <br>cabeza en agua o vaciándole agua en <br>la cara?" ~ 3,
          labels =="¿Le patearon o golpearon con las manos?"~ 1,
          labels =="¿Le golpearon con objetos?"~ 4,
          labels =="¿Le quemaron (con objetos calientes, <br>fuego u otra sustancia)?"~ 12,
          labels =="¿Le dieron descargas eléctricas?" ~ 7,
          labels =="¿Aplastaron su cuerpo o alguna parte de <br>él con algún objeto o con el cuerpo de <br>otra persona?"~ 2,
          labels == "¿Le hirieron con algún cuchillo, navaja <br>u otro objeto afilado?"~ 14,
          labels =="¿Le encajaron agujas en dedos u otra <br>parte del cuerpo?"~ 13,
          labels =="¿Le hirieron por el disparo de un arma <br>de fuego?"~ 10,
          labels =="¿Le agredieron mediante acoso sexual, <br>manoseo, exhibicionismo o intento de <br>violación?"~ 9,
          labels =="¿Le lastimaron sus órganos sexuales?"~ 8,
          labels == "¿Fue obligado mediante violencia física <br>o amenaza a tener una actividad sexual <br>no deseada?"~ 11,
          T ~ NA_real_
        ),
      labels =
        case_when(
          labels == "¿Ataron su cuerpo; ataron alguna parte <br>de su cuerpo a un objeto?" ~ "Le **ataron su cuerpo** o <br>alguna parte a un <br>un objeto",
          labels =="¿Le impidieron respirar asfixiándolo, <br>ahorcándolo?"~ "**Le impidieron respirar**<br> asfixiándolo o <br>ahorcándolo",
          labels =="¿Le impidieron respirar o metiendo su <br>cabeza en agua o vaciándole agua en <br>la cara?" ~ "**Le impidieron respirar** <br> metiendo su cabeza en agua",
          labels =="¿Le patearon o golpearon con las manos?"~ "Le **patearon** <br>o **golpearon**",
          labels =="¿Le golpearon con objetos?"~ "Le **golpearon** <br>con objetos",
          labels =="¿Le quemaron (con objetos calientes, <br>fuego u otra sustancia)?"~ "Le **quemaron**",
          labels =="¿Le dieron descargas eléctricas?" ~ "Le dieron **descargas** <br> **eléctricas**",
          labels =="¿Aplastaron su cuerpo o alguna parte de <br>él con algún objeto o con el cuerpo de <br>otra persona?"~ "Le **aplastaron su cuerpo** <br> con algún objeto",
          labels == "¿Le hirieron con algún cuchillo, navaja <br>u otro objeto afilado?"~ "**Le hirieron** con algún <br>**objeto afilado**",
          labels =="¿Le encajaron agujas en dedos u otra <br>parte del cuerpo?"~ "**Le encajaron agujas** en alguna <br>parte del cuerpo",
          labels =="¿Le hirieron por el disparo de un arma <br>de fuego?"~ "Le **dispararon**",
          labels =="¿Le agredieron mediante acoso sexual, <br>manoseo, exhibicionismo o intento de <br>violación?"~ "Le agredieron mediante **manoseo**, <br>**intento de violación** o <br>**exhibicionismo**",
          labels =="¿Le lastimaron sus órganos sexuales?"~ "**Lastimaron** sus **órganos <br>sexuales**",
          labels == "¿Fue obligado mediante violencia física <br>o amenaza a tener una actividad sexual <br>no deseada?"~ "Lo **obligaron** con violencia <br>a tener una **actividad** <br>**sexual**",
          T ~ NA_character_
        ),
      order_value_bars = 
        case_when(
          group_var == "Traslado" ~ 1,
          group_var == "MP" ~ 2
        ),
      latestYear = "MP"
    ) %>%
    rename(year = group_var) %>%
    mutate(
      figure1 =
        if_else(year == "MP", paste0(round(value2plot*100,0), "%"),  NA_character_)
    )
  
  figure2.df <- data2plot %>% 
    mutate(
      figure2 = if_else(year == "Traslado", paste0(round(value2plot*100,0), "%"),  NA_character_)
    ) %>%
    drop_na(figure2) %>%
    select(labels, figure2)
  
  data2plot <- data2plot %>%
    left_join(figure2.df, by = "labels") %>%
    mutate(
      category = labels,
      across(labels,
             ~paste0(
               "<span style='color:#2a2a9A;font-size:3.514598mm;font-weight:bold'>", figure2, "</span>",
               "<span> | </span>", 
               "<span style='color:#a90099;font-size:3.514598mm;font-weight:bold'>", figure1, "</span>",
               "<br>",
               "<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",
               labels,
               "</span>")
      ),
      labels = 
        if_else(year == "Traslado", NA_character_, labels)
    )
  
  colors4plot <- twoColors
  latestYear <- "MP"
  
  names(colors4plot) <- c("Traslado",
                          "MP")
  
  chart <- ENPOL_radarChart(data          = data2plot,
                            axis_var      = "category",         
                            target_var    = "value2plot",     
                            label_var     = "labels", 
                            order_var     = "order_var",
                            colors        = colors4plot);chart
  
  ggsave(plot = chart, 
         filename = paste0(path2SP,
                           "/National/Visualization",
                           "/Output/Debido proceso/",
                           savePath,"/Tortura",
                           "/tortura_fisica.svg"),
         width = 210,
         height = 210,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
  
}
