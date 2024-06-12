time_checks.fn <- function(
    data = master_data.df,
    mainVar
){
  
  time_checks.ls <- list()
  
  for (i in mainVar) {
    
    data_subset.df <- master_data.df %>%
      filter(ENPOL == 2021) %>%
      filter(Anio_arresto>=2011 & Anio_arresto<= 2021) %>%
      rename(mainVar = all_of(i)) 
    
    contraste <- data_subset.df %>% 
      filter(Anio_arresto >=2011 & Anio_arresto<= 2021) %>%
      group_by(Anio_arresto) %>%
      summarize(mainVar = mean(mainVar, na.rm=T)) #el descriptivo de cada anio SI es el intercepto mas el coeficiente del anio
    
    
    initial_model <- lm(mainVar ~ relevel(factor(Anio_arresto),"2021"), 
                        data = data_subset.df)
    final_model <- lm(mainVar ~ relevel(factor(Anio_arresto), "2021") + tiempo_sentencia + relevel(factor(Anio_arresto), "2021")*tiempo_sentencia, 
                      data = data_subset.df)
    
    
    initial_data2plot <-  as.tibble(summary(initial_model)$coefficients) %>% 
      cbind(var = row.names(summary(initial_model)$coefficients), .) %>% mutate(model="base")
    
    final_data2plot <-  as.tibble(summary(final_model)$coefficients) %>% 
      cbind(var = row.names(summary(final_model)$coefficients), .) %>% mutate(model="final")
    
    data2plot <- bind_rows(initial_data2plot, final_data2plot) %>% 
      mutate(Anio = case_when(var=="(Intercept)" ~ "2021",
                              var=="relevel(factor(Anio_arresto), \"2021\")2011" ~ "2011",
                              var=="relevel(factor(Anio_arresto), \"2021\")2012" ~ "2012",
                              var=="relevel(factor(Anio_arresto), \"2021\")2013" ~ "2013",
                              var=="relevel(factor(Anio_arresto), \"2021\")2014" ~ "2014",
                              var=="relevel(factor(Anio_arresto), \"2021\")2015" ~ "2015",
                              var=="relevel(factor(Anio_arresto), \"2021\")2016" ~ "2016",
                              var=="relevel(factor(Anio_arresto), \"2021\")2017" ~ "2017",
                              var=="relevel(factor(Anio_arresto), \"2021\")2018" ~ "2018",
                              var=="relevel(factor(Anio_arresto), \"2021\")2019" ~ "2019",
                              var=="relevel(factor(Anio_arresto), \"2021\")2020" ~ "2020")) %>%
      mutate(value2plot =  case_when(model=="base" & Anio!=2021 ~ 100*(initial_data2plot$Estimate[1]+Estimate),
                                     model=="final" & Anio!=2021 ~ 100*(final_data2plot$Estimate[1]+Estimate),
                                     Anio==2021 ~ 100*Estimate),
             labels = paste0(round(value2plot,0), "%"),
             group_var =  model) %>%
      filter(!is.na(Anio))
    
    Estimated_diffval1 <- filter(data2plot, Anio == 2021, model == "base") %>% select(value2plot) %>% unlist()
    Estimated_diffval2 <- filter(data2plot, Anio == 2021, model == "final") %>% select(value2plot) %>% unlist()
    Estimated_diff1 <- Estimated_diffval1 - Estimated_diffval2
    
    final_adjusted_data2plot <- final_data2plot %>% 
      mutate(Anio = case_when(var=="(Intercept)" ~ "2021",
                              var=="relevel(factor(Anio_arresto), \"2021\")2011" ~ "2011",
                              var=="relevel(factor(Anio_arresto), \"2021\")2012" ~ "2012",
                              var=="relevel(factor(Anio_arresto), \"2021\")2013" ~ "2013",
                              var=="relevel(factor(Anio_arresto), \"2021\")2014" ~ "2014",
                              var=="relevel(factor(Anio_arresto), \"2021\")2015" ~ "2015",
                              var=="relevel(factor(Anio_arresto), \"2021\")2016" ~ "2016",
                              var=="relevel(factor(Anio_arresto), \"2021\")2017" ~ "2017",
                              var=="relevel(factor(Anio_arresto), \"2021\")2018" ~ "2018",
                              var=="relevel(factor(Anio_arresto), \"2021\")2019" ~ "2019",
                              var=="relevel(factor(Anio_arresto), \"2021\")2020" ~ "2020")) %>%
      mutate(value2plot =  case_when(Anio!=2021 ~ 100*(final_data2plot$Estimate[1]+Estimate),
                                     Anio==2021 ~ 100*Estimate)) %>%
      filter(!is.na(Anio)) %>%
      mutate(value2plot = value2plot + Estimated_diff1,
             labels = paste0(round(value2plot,0), "%"),
             group_var =  "Estimated value")
    
    data2plot.final <- bind_rows(data2plot,final_adjusted_data2plot) %>%
      mutate(
        group_var = 
          case_when(
            group_var == "base" ~ "Valor inicial",
            group_var == "final" ~ "Valor modelo",
            group_var == "Estimated value" ~ "Valor modelo ajustado"
          )
      )
    
    GAP <- data2plot.final %>%
      filter(group_var != "Valor modelo")  %>% 
      filter(Anio != 2021) %>%
      select(var, group_var, Anio, value2plot) %>%
      distinct() %>%
      pivot_wider(id_cols = Anio, names_from = group_var, values_from = value2plot) %>%
      mutate(GAP = abs(`Valor inicial` - `Valor modelo ajustado`)) %>%
      drop_na() %>%
      summarise(
        TC_promedio = mean(GAP, na.rm = T)
      ) %>%
      pull()
    
    Trends <- data2plot.final %>%
      filter(group_var != "Valor modelo") %>%
      arrange(Anio) %>%
      group_by(group_var) %>%
      mutate(
        lag_var = lag(value2plot, n = 1),
        difference = value2plot - lag_var,
        trend_direction = if_else(difference >= 3, "Positive", 
                                  if_else(difference <= -3, "Negative",
                                          "No trends")
        )
      ) %>%
      ungroup() %>%
      drop_na() %>%
      filter(trend_direction != "No trends") %>%
      group_by(Anio) %>%
      mutate(
        paralalel_trends = if_else(length(unique(trend_direction)) > 1, 
                                   "mixed_trends", "same trends")
      ) %>%
      select(Anio, paralalel_trends) %>%
      distinct() %>%
      filter(paralalel_trends == "mixed_trends") %>%
      pull(paralalel_trends)
    
    colors4plot <- c("Valor inicial" = "#003B88", 
                     "Valor modelo ajustado" = "#43a9a7", 
                     "Valor modelo" = "#fa4d57")
    
    plt <- ggplot(data2plot.final, 
                  aes(x     = Anio,
                      y     = value2plot,
                      label = labels,
                      group = group_var,
                      color = group_var)
    ) +
      geom_point(size = 2, show.legend = T) +
      geom_line(size  = 1, show.legend = T) +
      geom_text_repel(
        size        = 3.514598,
        show.legend = F,
        min.segment.length = 1000,
        seed               = 42,
        box.padding        = 0.5,
        direction          = "y",
        force              = 5,
        force_pull         = 1) +
      scale_y_continuous(limits = c(0, 105),
                         expand = c(0,0),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%")) +
      scale_color_manual(values = colors4plot) +
      labs(title = i,
           subtitle = paste0("La brecha promedio es de <b>", 
                             round(GAP,0),
                             "</b> puntos porcentuales",
                             "\nExiste una divergencia en las tendencias en <b>", 
                             length(Trends), 
                             "</b> año/s entre el valor inicial y el modelo ajustado"),
           caption = "Nota: Las divergencias en las tendencias se resaltan cuando las diferencias son mayores a 5%") +
      WJP_theme() +
      expand_limits(y = c(0, 100)) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "#d1cfd1"),
            axis.title.x       = element_blank(),
            axis.title.y       = element_blank(),
            axis.line.x        = element_line(color    = "#d1cfd1"),
            axis.ticks.x       = element_line(color    = "#d1cfd1", linetype = "solid"),
            legend.title       = element_blank(),
            legend.position    = "top",
            legend.box         = element_blank(), 
            legend.key = element_blank(),
            plot.subtitle = element_markdown(family="Lato Full", 
                                             size = 10, 
                                             color = "Black",
                                             margin = margin(5,0,0,0)));plt
    
    time_checks.ls[[i]] <- plt
  }
  
  print(time_checks.ls)
  
}
