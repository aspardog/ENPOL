paralel_trends.fn <- function(
  data = master_data.df,
  mainVar
  ){
  
  paralel_trends.ls <- list()
  
  for (i in mainVar) {
    
    mainVar <- i
    data_subset.df <- master_data.df %>%
      rename(mainVar = all_of(i)) %>%
      filter(Anio_arresto > 2010) %>%
      group_by(Anio_arresto, ENPOL) %>%
      summarise(mainVar = 
                  mean(as.numeric(mainVar), na.rm = T)*100
      ) %>%
      pivot_wider(id_cols = Anio_arresto, names_from = "ENPOL", values_from = "mainVar", names_prefix = "ENPOL_") %>%
      mutate(
        GAP = abs((ENPOL_2021-ENPOL_2016))
      ) %>%
      pivot_longer(cols = !c(Anio_arresto, GAP), names_to = "ENPOL", values_to = "mainVar") %>%
      drop_na(mainVar)
    
    data_2016 <- subset(data_subset.df, ENPOL == "ENPOL_2016") %>%
      mutate(group_var = "dotted") # Assign dotted to 2016 data for consistent line type
    
    data_2021 <- subset(data_subset.df, ENPOL == "ENPOL_2021")%>%
      mutate(group_var = if_else(
        Anio_arresto <= 2016, "dotted", "normal"
      ))
    
    GAP <- data_subset.df %>%
      ungroup() %>%
      select(GAP) %>% 
      distinct() %>%
      drop_na() %>%
      summarise(
        TC_promedio = mean(GAP, na.rm = T)
      ) %>%
      pull()
    
    Trends <- data_subset.df %>%
      ungroup() %>%
      arrange(Anio_arresto) %>%
      group_by(ENPOL) %>%
      mutate(
        lag_var = lag(mainVar, n = 1),
        difference = mainVar - lag_var,
        trend_direction = if_else(difference > 3, "Positive", 
                                  if_else(difference < -3, "Negative",
                                          "No trends")
        )
      ) %>%
      ungroup() %>%
      filter(Anio_arresto < 2017) %>%
      drop_na() %>%
      filter(trend_direction != "No trends") %>%
      group_by(Anio_arresto) %>%
      mutate(
        paralalel_trends = if_else(length(unique(trend_direction)) > 1, "mixed_trends", "same trends")
      ) %>%
      select(Anio_arresto, paralalel_trends) %>%
      distinct() %>%
      filter(paralalel_trends == "mixed_trends") %>%
      pull(paralalel_trends)
    
    p <- ggplot() +
      geom_vline(xintercept = "2016", color = "red", linetype = "dotted") + # Add vertical line at 2016
      geom_line(data = subset(data_2021, Anio_arresto > 2015), aes(x = Anio_arresto, y = mainVar, color = "2021", group = "2021"), size = 1.2, color = "#2a2a9A")  +
      geom_line(data = data_2016, aes(x = Anio_arresto, y = mainVar, color = "2016", group = group_var,  linetype = group_var), size = 1.2, show.legend = T) +
      geom_line(data = data_2021, aes(x = Anio_arresto, y = mainVar, color = "2021", linetype = group_var, group = group_var), size = 1.2, show.legend = T)  +
      scale_color_manual(values = c("2016" = "#ef4b4b", "2021" = "#2a2a9A")) +
      scale_linetype_manual(values = c("dotted" = "dotted", "normal" = "solid")) +
      labs(title = mainVar,
           subtitle = paste0("La brecha promedio es de <b>", 
                             round(GAP,0),
                             "</b> puntos porcentuales",
                             "\nExiste una divergencia en las tendencias en <b>", 
                             length(Trends), 
                             "</b> año/s entre las dos encuestas"),
           caption = "Nota: Las divergencias en las tendencias se resaltan cuando las diferencias son mayores a 5%") +
      theme_minimal() +
      theme(legend.position = "top") +
      scale_y_continuous(limits = c(0, 105),
                         expand = c(0,0),
                         breaks = seq(0,100,10),
                         labels = paste0(seq(0,100,10), "%"))   +
      WJP_theme() +
      theme(legend.key = element_blank(),
            legend.position = "top",
            legend.title = element_blank(),
            axis.line        = element_line(color    = "#5e5c5a", linetype = "solid"),
            legend.text = element_text(size = 8), #change legend text font size
            panel.grid.major.y = element_line(size = 0.5, 
                                              colour = "grey75", 
                                              linetype = "dotted"),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(family="Lato Black", 
                                      size = 12, 
                                      color = "Black"),
            plot.subtitle = element_markdown(family="Lato Full", 
                                             size = 10, 
                                             color = "Black"),
            axis.text     = element_text(family = "Lato Full",
                                         face     = "plain",
                                         size     = 10,
                                         color    = "Black"),
            axis.title     = element_text(family = "Lato Full",
                                          face     = "plain",
                                          size     = 10,
                                          color    = "Black"),
            plot.caption = element_text(family = "Lato Full",
                                        face     = "plain",
                                        size     = 8,
                                        color    = "Black", 
                                        hjust = 0))+
      guides(linetype = "none");p
    
    paralel_trends.ls[[i]] <- p

  }
  print(paralel_trends.ls)
}
