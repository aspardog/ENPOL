master_data.df <- data_subset.df %>%
  mutate(period = 
           case_when(
             years_since_NSJP < -1 & years_since_NSJP > -2 ~ "one_year_before",
             years_since_NSJP < -2 & years_since_NSJP > -3 ~ "two_years_before",
             years_since_NSJP < -3 & years_since_NSJP > -4 ~ "three_years_before",
             years_since_NSJP < -4 & years_since_NSJP > -5 ~ "four_years_before",
             years_since_NSJP < -5 & years_since_NSJP > -6 ~ "five_years_before",
             years_since_NSJP < -6 & years_since_NSJP > -7 ~ "six_years_before",
             years_since_NSJP < -7 & years_since_NSJP > -8 ~ "seven_years_before",
             years_since_NSJP < -8 & years_since_NSJP > -9 ~ "eight_years_before",
             years_since_NSJP < -9 & years_since_NSJP > -10 ~ "nine_years_before",
             years_since_NSJP < -10 & years_since_NSJP > -11 ~ "ten_years_before",
             years_since_NSJP > -1 & years_since_NSJP < 1  ~ "implementation_year",
             years_since_NSJP > 1 & years_since_NSJP < 2 ~ "one_year_after",
             years_since_NSJP > 2 & years_since_NSJP < 3 ~ "two_years_after",
             years_since_NSJP > 3 & years_since_NSJP < 4 ~ "three_years_after",
             years_since_NSJP > 4 & years_since_NSJP < 5 ~ "four_years_after",
             years_since_NSJP > 5 & years_since_NSJP < 6 ~ "five_years_after",
             years_since_NSJP > 6 & years_since_NSJP < 7 ~ "six_years_after",
             years_since_NSJP > 7 & years_since_NSJP < 8 ~ "seven_years_after",
             years_since_NSJP > 8 & years_since_NSJP < 9 ~ "eight_years_after",
             years_since_NSJP > 9 & years_since_NSJP < 10 ~ "nine_years_after",
             years_since_NSJP > 10 & years_since_NSJP < 11 ~ "ten_years_after",
             years_since_NSJP > 11 & years_since_NSJP < 12 ~ "eleven_years_after",
             years_since_NSJP > 12 & years_since_NSJP < 13 ~ "twelve_years_after",
             years_since_NSJP > 13 & years_since_NSJP < 14 ~ "thirteen_years_after"
           )) %>%
  filter(!is.na(period)) %>%
  arrange(years_since_NSJP) %>%
  group_by(Estado_arresto) %>%
  mutate(max_time_implementation = max(years_since_NSJP, na.rm = T),
         grupo_implementacion = 
           if_else(
             max_time_implementation > 6 & max_time_implementation < 9, "Implementación tardía",
             if_else(
               max_time_implementation > 9 & max_time_implementation < 11, "Implementación media",
               if_else(
                 max_time_implementation > 11, "Implementación temprana", NA_character_
               )
           ))
  )

variables2summarise <- audiencias <- c("P5_19_3",
                                       "P5_26",
                                       "P5_20_1",
                                       "P5_20_2",
                                       "P5_20_3",
                                       "P5_26A",
                                       "P5_26B")

data2analysis <- lapply(variables2analyze, function(vars){
  
  changes_time <- master_data.df %>% 
    group_by(period, grupo_implementacion) %>%
    summarise(
      across(all_of(variables2summarise),
             ~ mean(.x, na.rm = TRUE))) %>%
    mutate(order_value = 
             case_when(
               period == "ten_years_before"    ~ -10,
               period == "nine_years_before"   ~ -9,
               period == "eight_years_before"  ~ -8,
               period == "seven_years_before"  ~ -7,
               period == "six_years_before"    ~ -6,
               period == "five_years_before"   ~ -5,
               period == "four_years_before"   ~ -4,
               period == "three_years_before"  ~ -3,
               period == "two_years_before"    ~ -2,
               period == "one_year_before"     ~ -1,
               period == "implementation_year" ~ 0,
               period == "one_year_after"      ~ 1,
               period == "two_years_after"     ~ 2,
               period == "three_years_after"   ~ 3,
               period == "four_years_after"    ~ 4,
               period == "five_years_after"    ~ 5,
               period == "six_years_after"     ~ 6,
               period == "seven_years_after"   ~ 7,
               period == "eight_years_after"   ~ 8,
               period == "nine_years_after"    ~ 9,
               period == "ten_years_after"     ~ 10,
               period == "eleven_years_after"  ~ 11,
               period == "twelve_years_after"   ~ 12,
               period == "thirteen_years_after" ~ 13,
               
             )
    ) %>%
    arrange(order_value)
  
})
  
data2plot <- data2analysis[[1]] %>%
  pivot_longer(cols = !c(period, grupo_implementacion, order_value), names_to = "category", values_to = "value2plot") %>%
  filter(category %in% variables2summarise) %>%
  mutate(
    period = 
      case_when(
        period == "implementation_year" ~ "Implementación",
        period == "one_year_after" ~ "Un año",
        period == "two_years_after" ~ "Dos años",
        period == "three_years_after" ~ "Tres años",
        period == "four_years_after" ~ "Cuatro años",
        period == "five_years_after" ~ "Cinco años",
        period == "six_years_after" ~ "Seis años",
        period == "seven_years_after" ~ "Siete años",
        period == "eight_years_after" ~ "Ocho años",
        period == "nine_years_after" ~ "Nueve años",
        period == "ten_years_after" ~ "Diez años",
        period == "eleven_years_after" ~ "Once años",
        period == "twelve_years_after" ~ "Doce años",
        period == "thirteen_years_after"   ~ "Trece años",
        
      ),
    value2plot = value2plot*100,
    labels = if_else(
      period %in% c("Implementación", "Dos años", "Cuatro años", "Seis años", "Ocho años", "Diez años", "Doce años"),
      paste0(round(value2plot,0), "%"), NA_character_),
    period_labels =
      case_when(
        period == "Implementación" ~ "Implementación",
        period == "Un año" ~ " ",
        period == "Dos años" ~ "Dos años",
        period == "Tres años" ~ " ",
        period == "Cuatro años" ~ "Cuatro años",
        period == "Cinco años" ~ " ",
        period == "Seis años" ~ "Seis años",
        period == "Siete años" ~ " ",
        period == "Ocho años" ~ "Ocho años",
        period == "Nueve años" ~ " ",
        period == "Diez años" ~ "Diez años",
        period == "Once años" ~ " ",
        period == "Doce años" ~ "Doce años",
        period == "Trece años" ~ " ",
        
      )
  )

colors4plot <- c("Implementación media" = "#1a2580", 
                 "Implementación tardía" = "#a90099",
                 "Implementación temprana" = "#ef4b4b")
ggplot(data2plot, 
       aes(x     = reorder(period,order_value),
           y     = value2plot,
           color  = grupo_implementacion,
           group = grupo_implementacion,
           label = labels)) +
  geom_point(size = 2,
             show.legend = F) +
  geom_line(size  = 1,
            show.legend = F) +
  geom_text(aes(y = value2plot + 5),
            family      = "Lato Full",
             fontface    = "bold",
             size        = 3.514598,
             show.legend = F) +
  scale_y_continuous(limits = c(0, 105),
                     expand = c(0,0),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%")) +
  scale_x_discrete("period",
                   labels = c("Implementación" = "Implementación",
                              "Un año" = " ",
                              "Dos años" = "Dos años",
                              "Tres años" = " ",
                              "Cuatro años" = "Cuatro años",
                              "Cinco años" = " ",
                              "Seis años" = "Seis años",
                              "Siete años" = " ",
                              "Ocho años" = "Ocho años",
                              "Nueve años" = " ",
                              "Diez años" = "Diez años",
                              "Once años" = " ",
                              "Doce años" = "Doce años")
  ) +
  scale_color_manual(values = colors4plot) +
  WJP_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid")
  )

  
  # Save the plot
  ggsave(lineChart, 
         filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", variables2summarise[[i]],"_group.svg"),
         width  = 200, 
         height = 150,
         units  = "mm",
         dpi    = 72,
         device = "svg")
