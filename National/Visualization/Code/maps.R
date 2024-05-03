## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            MAPAS
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Mayo 2, 2024
##
## This version:      Mayo 2, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Tiempo de traslado: 30 minutos

data2plot <- master_data.df %>%
  mutate(
    TT_30min = 
      if_else(Tiempo_traslado ==  "Hasta 30 minutos", 1, 0)
  ) 

plot <- mapas_Estado.fn(mainData = data2plot,
                        mainVar = "TT_30min")

ggsave(plot = plot, filename = paste0(path2SP,"/National/Exploration/Output/maps/","Mapa_tt30min_cat.svg"), width = 15, height = 10)

# Tiempo de traslado: 6 horas

data2plot <- master_data.df %>%
  mutate(
    TT_6h = 
      if_else(Tiempo_traslado ==  "Más de 6 horas hasta 24 horas", 1, 0)
  ) 

plot <- mapas_Estado.fn(mainData = data2plot,
                        mainVar = "TT_6h")

ggsave(plot = plot, filename = paste0(path2SP,"/National/Exploration/Output/maps/","Mapa_tt6h_cat.svg"), width = 15, height = 10)

# Primer lugar de traslado: MP

data2plot <- master_data.df %>%
  mutate(
    LT_MP = 
      if_else(Primer_lugar_traslado ==  "Agencia del Ministerio Público", 1, 0)
  ) 

plot <- mapas_Estado.fn(mainData = data2plot,
                        mainVar = "LT_MP")

ggsave(plot = plot, filename = paste0(path2SP,"/National/Exploration/Output/maps/","Mapa_ltmp_cat.svg"), width = 15, height = 10)

# Primer lugar de traslado: Policía

data2plot <- master_data.df %>%
  mutate(
    LT_Policia = 
      if_else(Primer_lugar_traslado ==  "Instalación de la policía", 1, 0)
  ) 

plot <- mapas_Estado.fn(mainData = data2plot,
                        mainVar = "LT_Policia")
ggsave(plot = plot, filename = paste0(path2SP,"/National/Exploration/Output/maps/","Mapa_ltpolicia_cat.svg"), width = 15, height = 10)

# Otro tipo de mapa tiempo

# Define a vector containing unique values of Estado_arresto
estados <- unique(master_data.df$Estado_arresto)

# Use map_dfr to loop through each Estado_arresto value and combine the results into a dataframe
result_df <- map_dfr(estados, ~{
  # Filter the data for the current Estado_arresto value
  filtered_data <- master_data.df %>%
    filter(Estado_arresto == .x) %>%
    mutate(
      counter = 1
    ) %>%
    group_by(Tiempo_traslado, Estado_arresto) %>%
    summarise(TT = sum(counter, na.rm = TRUE)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(
      value2plot = TT / sum(TT)
    ) %>%
    filter(Tiempo_traslado == c("Hasta 30 minutos", "Más de 6 horas hasta 24 horas")) %>%
    mutate(
      max = max(value2plot)
    ) %>%
    filter(value2plot == max)
  
  return(filtered_data)
})

Estados <- result_df %>%
  rename(ESTADO = Estado_arresto) %>%
  mutate(
    ESTADO = 
      case_when(
        ESTADO == "Coahuila de Zaragoza" ~ "Coahuila",
        ESTADO == "Michoacán de Ocampo"  ~ "Michoacán",
        ESTADO == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
        ESTADO == "México" ~ "Estado de México",
        T ~ ESTADO
      ))



table <- Estados %>%
  mutate(
    ` ` = "",
    `%` = round(value2plot*100, 0)
  ) %>%
  arrange(ESTADO) %>%
  select(
    Estado = ESTADO, ` `, `%`, Tiempo_traslado
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
  
  bg(i = ~ Tiempo_traslado == "Hasta 30 minutos" & `%` >= 10 & `%` < 25, j = ' ', bg = "#7E7EDF", part = "body") %>%
  bg(i = ~ Tiempo_traslado == "Hasta 30 minutos" & `%` >= 25 & `%` < 40, j = ' ', bg = "#2a2a9A", part = "body") %>%
  bg(i = ~ Tiempo_traslado == "Hasta 30 minutos" & `%` >= 40, j = ' ', bg = "#1B1B5D", part = "body") %>%
  bg(i = ~ Tiempo_traslado == "Más de 6 horas hasta 24 horas" & `%` >= 10 & `%` < 20, j = ' ', bg = "#FC989D", part = "body") %>%
  bg(i = ~ Tiempo_traslado == "Más de 6 horas hasta 24 horas" & `%` >= 20 & `%` < 25, j = ' ', bg = "#FA4D57", part = "body") %>%
  bg(i = ~ Tiempo_traslado == "Más de 6 horas hasta 24 horas" & `%` >= 25 & `%` <= 30, j = ' ', bg = "#BE0A14", part = "body") %>%

  
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
                   fit      = "fixed",
                   scaling  = "fixed", 
                   just     = c("left", "top"),
                   wrapping = T)

mexico_map <- mapa %>%
  left_join(Estados, by = "ESTADO") %>%
  mutate(
    value2plot = round(value2plot*100, 0),
    color_group = case_when(
      Tiempo_traslado == "Hasta 30 minutos" & value2plot >= 10 & value2plot < 25 ~ "T1",
      Tiempo_traslado == "Hasta 30 minutos" & value2plot >= 25 & value2plot < 40 ~ "T2",
      Tiempo_traslado == "Hasta 30 minutos" & value2plot >= 40 ~ "T3",
      Tiempo_traslado == "Más de 6 horas hasta 24 horas" & value2plot >= 10 & value2plot < 20 ~ "T4",
      Tiempo_traslado == "Más de 6 horas hasta 24 horas" & value2plot >= 20 & value2plot < 25 ~ "T5",
      Tiempo_traslado == "Más de 6 horas hasta 24 horas" & value2plot >= 25 & value2plot <= 30 ~ "T6"
      
    ),
    color_group = as.factor(color_group)
  )

cat_palette <- c("Hasta 30 minutos" = "#2a2a9A",
                 "Más de 6 horas hasta 24 horas" = "#fa4d57")

cat_palette <- c("T1"   = "#7E7EDF",
                 "T2"  = "#2a2a9A",
                 "T3"  = "#1B1B5D",
                 "T4"  = "#FC989D",
                 "T5"  = "#FA4D57",
                 "T6" =  "#BE0A14")


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


viz <- wrap_elements(tpanel) + p +
  plot_layout(ncol = 2, nrow = 1, widths = c(1, 2), heights = c(1,2))

ggsave(plot = viz, filename = paste0(path2SP,"/National/Exploration/Output/maps/","Mapa_tt_cat.svg"), width = 15, height = 10)


#######



# Define a vector containing unique values of Estado_arresto
estados <- unique(master_data.df$Estado_arresto)

# Use map_dfr to loop through each Estado_arresto value and combine the results into a dataframe
result_df <- map_dfr(estados, ~{
  # Filter the data for the current Estado_arresto value
  filtered_data <- master_data.df %>%
    filter(Estado_arresto == .x) %>%
    mutate(
      counter = 1
    ) %>%
    group_by(Primer_lugar_traslado, Estado_arresto) %>%
    summarise(PT = sum(counter, na.rm = TRUE)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(
      value2plot = PT / sum(PT)
    ) %>%
    filter(Primer_lugar_traslado == c("Agencia del Ministerio Público", "Instalación de la policía")) %>%
    mutate(
      max = max(value2plot)
    ) %>%
    filter(value2plot == max)
  
  return(filtered_data)
})

Estados <- result_df %>%
  rename(ESTADO = Estado_arresto) %>%
  mutate(
    ESTADO = 
      case_when(
        ESTADO == "Coahuila de Zaragoza" ~ "Coahuila",
        ESTADO == "Michoacán de Ocampo"  ~ "Michoacán",
        ESTADO == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
        ESTADO == "México" ~ "Estado de México",
        T ~ ESTADO
      ))

table <- Estados %>%
  mutate(
    ` ` = "",
    `%` = round(value2plot*100, 1)
  ) %>%
  arrange(ESTADO) %>%
  select(
    Estado = ESTADO, ` `, `%`, Primer_lugar_traslado
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
  
  bg(i = ~ Primer_lugar_traslado == "Agencia del Ministerio Público" & `%` >= 30 & `%` < 50, j = ' ', bg = "#7E7EDF", part = "body") %>%
  bg(i = ~ Primer_lugar_traslado == "Agencia del Ministerio Público" & `%` >= 50 & `%` < 70, j = ' ', bg = "#2a2a9A", part = "body") %>%
  bg(i = ~ Primer_lugar_traslado == "Agencia del Ministerio Público" & `%` >= 70, j = ' ', bg = "#1B1B5D", part = "body") %>%
  bg(i = ~ Primer_lugar_traslado == "Instalación de la policía", j = ' ', bg = "#a90099", part = "body") %>%
  
  
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
                   fit      = "fixed",
                   scaling  = "fixed", 
                   just     = c("left", "top"),
                   wrapping = T)

mexico_map <- mapa %>%
  left_join(Estados, by = "ESTADO") %>%
  mutate(
    value2plot = round(value2plot*100, 0),
    color_group = case_when(
      Primer_lugar_traslado == "Agencia del Ministerio Público" & value2plot >= 30 & value2plot < 50 ~ "T1",
      Primer_lugar_traslado == "Agencia del Ministerio Público" & value2plot >= 50 & value2plot < 70 ~ "T2",
      Primer_lugar_traslado == "Agencia del Ministerio Público" & value2plot >= 70 ~ "T3",
      Primer_lugar_traslado == "Instalación de la policía" ~ "T4"
      
    ),
    color_group = as.factor(color_group)
  )

cat_palette <- c("T1"   = "#7E7EDF",
                 "T2"  = "#2a2a9A",
                 "T3"  = "#1B1B5D",
                 "T4"  = "#a90099")

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


viz <- wrap_elements(tpanel) + p +
  plot_layout(ncol = 2, nrow = 1, widths = c(1, 2), heights = c(1,2))

ggsave(plot = viz, filename = paste0(path2SP,"/National/Exploration/Output/maps/","Mapa_pt_cat.svg"), width = 15, height = 12)
