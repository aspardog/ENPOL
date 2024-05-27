# Audiencias

audiencias <- c("P5_19_3",
                "P5_26",
                "P5_20_1",
                "P5_20_2",
                "P5_20_3",
                "P5_26A",
                "P5_26B")

data_subset.df <- Main_database_2008 %>%
  clean_columns.fn(., audiencias) %>%
  mutate(
    P5_2_1 = 
      case_when(
        P5_2_1 == 1 ~ 1,
        P5_2_1 == 2 ~ 0,
        T ~ NA_real_
      ),
    P5_14 = 
      case_when(
        as.numeric(P5_14) == 1 ~ 1,
        as.numeric(P5_14) == 2 ~ 0,
        T ~ NA_real_
      ),
    P5_16_5 =
      case_when(
        as.numeric(P5_16_5) == 1 ~ 1,
        as.numeric(P5_16_5) == 2 ~ 1,
        as.numeric(P5_16_5) == 3 ~ 0,
        as.numeric(P5_16_5) == 4 ~ 0,
        T ~ NA_real_
      ),
    P5_16_2 =
      case_when(
        as.numeric(P5_16_2) == 1 ~ 1,
        as.numeric(P5_16_2) == 2 ~ 1,
        as.numeric(P5_16_2) == 3 ~ 0,
        as.numeric(P5_16_2) == 4 ~ 0,
        T ~ NA_real_
      ),
    P5_18_5 = 
      case_when(
        as.numeric(P5_18) == 2 ~ 1,
        as.numeric(P5_18) == 1 ~ 1,
        as.numeric(P5_18) == 3 ~ 0,
        as.numeric(P5_18) == 4 ~ 0,
        as.numeric(P5_18) == 5 ~ 0,
        T ~ NA_real_
      ),
    P5_25_1 = 
      case_when(
        as.numeric(P5_25) == 1 ~ 1,
        as.numeric(P5_25) == 2 ~ 0,
        as.numeric(P5_25) == 3 ~ 0,
        T ~ NA_real_
      )
  )

data2plot <- event_study(data_subset.df,
                         var_analysis = 
                           c("P5_2_1",
                             "P5_19_3",
                             "P5_16_5",
                             "P5_14",
                             "P5_16_2",
                             "P5_18_5",
                             "P5_26",
                             "P5_20_1",
                             "P5_20_2",
                             "P5_20_3",
                             "P5_25_1",
                             "P5_26A",
                             "P5_26B"))

for (i in seq_along(data2plot)) {
  data2table <- as.data.frame(data2plot[[i]])
  
  dependent_var <- names(data2table[4])
  
  data2plot[[i]] <- data2table %>%
    rename(dependent_var = all_of(dependent_var)) %>%
    pivot_longer(cols = c(dependent_var), names_to = "category", values_to = "value2plot") %>%
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
          period == "twelve_years_after" ~ "Doce años"
        ),
      value2plot = value2plot * 100,
      labels = if_else(
        period %in% c("Implementación", "Dos años", "Cuatro años", "Seis años", "Ocho años", "Diez años", "Doce años"),
        paste0(round(value2plot, 0), "%"), NA_character_
      ),
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
          period == "Doce años" ~ "Doce años"
        )
    )
}

labels <- c("Tuvo contacto con el juez",
            "Hubo registro de video",
            "Hubo acceso al público",
            "El juez de control fue distinto al que sentenció",
            "El juez estuvo presente en las audiencias",
            "El juez llevaba el control de las audiencias",
            "La persona juzgada se sintió escuchada",
            "La persona estaba tras un vidrio",
            "La persona estaba esposada",
            "La persona traía uniforme penitenciario",
            "La persona consideró que el juez lo consideró culpable antes de oir las pruebas",
            "Proceso justo",
            "Sentencia justa"
            )

# Loop over each label and create corresponding plot
for (i in seq_along(data2plot)) {
  colors4plot <- c("dependent_var" = "#003B88")
  
  lineChart <- lineChartViz(data = data2plot[[1]]) +
    labs(title = labels[[i]])
  
  # Save the plot
  ggsave(lineChart, 
         filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", labels[[i]], ".svg"),
         width  = 200, 
         height = 150,
         units  = "mm",
         dpi    = 72,
         device = "svg")
}

# Series temporales

# Audiencias

audiencias <- c("P5_19_3",
                "P5_26",
                "P5_20_1",
                "P5_20_2",
                "P5_20_3",
                "P5_26A",
                "P5_26B")

data2table <- data_subset.df %>%
  group_by(Anio_arresto) %>%
  summarise(value2plot = mean(P5_19_3, na.rm = T)) %>%
  mutate(
    value2plot = value2plot * 100,
    labels = if_else(
      Anio_arresto %in% c("2008", "2010", "2012", "2014", "2016", "2018", "2020"),
      paste0(round(value2plot, 0), "%"), NA_character_
    ),
    group_var = "National",
    colors = "#003B88"
  )

colors4plot <- c("#003B88")
# Creating ggplot
plt <- ggplot(data2table, 
              aes(x     = Anio_arresto,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = group_var)) +
  geom_point(size = 2,
             show.legend = F) +
  geom_line(size  = 1,
            show.legend = F) +
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
                     labels = paste0(seq(0,100,20), "%")) %>%
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


# Loop through each variable in audiencias
for (var in audiencias) {
  # Calculate summary statistics for the current variable
  data2table <- data_subset.df %>%
    group_by(Anio_arresto) %>%
    summarise(value2plot = mean(!!sym(var), na.rm = TRUE)) %>%
    mutate(
      value2plot = value2plot * 100,
      labels = if_else(
        Anio_arresto %in% c("2008", "2010", "2012", "2014", "2016", "2018", "2020"),
        paste0(round(value2plot, 0), "%"), NA_character_
      ),
      group_var = "National"
    )
  colors4plot <- c("National" = "#003B88")
  
  # Create ggplot for the current variable
  plt <- ggplot(data2table, 
                aes(x     = Anio_arresto,
                    y     = value2plot,
                    label = labels,
                    group = group_var,
                    color = group_var)) +
    geom_point(size = 2,
               show.legend = FALSE) +
    geom_line(size  = 1,
              show.legend = FALSE) +
    geom_text_repel(family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = FALSE,
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_color_manual(values = colors4plot) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0,0),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid")
    ) +
    labs(title = paste("Variable:", var))
  
  # Save the plot
  ggsave(plt, 
         filename = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Analisis temporal/", labels[[i]], "_series.svg"),
         width  = 200, 
         height = 150,
         units  = "mm",
         dpi    = 72,
         device = "svg")
}
