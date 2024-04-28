mapa <- st_read(paste0(path2SP,"/National/Exploration/Input/shp/México_Estados.shp")) %>%
  mutate(
    case_when(
      ESTADO == "México" ~ "Estado de México"
    )
  )

Estados <- Main_database_2008 %>%
  group_by(Estado_arresto) %>%
  summarise(indice = mean(indicator_general, na.rm = T)) %>%
  drop_na() %>%
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
    `%` = round(indice*100, 1)
  ) %>%
  arrange(ESTADO) %>%
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
  
  bg(i = ~ `%` <  60, j = ' ', bg = "#E03849", part = "body") %>%
  bg(i = ~ `%` >= 65, j = ' ', bg = "#FF7900", part = "body") %>%
  bg(i = ~ `%` >= 70, j = ' ', bg = "#FFC818", part = "body") %>%
  bg(i = ~ `%` >= 75, j = ' ', bg = "#46B5FF", part = "body") %>%
  bg(i = ~ `%` >= 80, j = ' ', bg = "#0C75B6", part = "body") %>%
  bg(i = ~ `%` >= 85, j = ' ', bg = "#18538E", part = "body") %>%
  
  
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
  mutate(value2plot = indice) %>%
  mutate(
    color_group = case_when(
      value2plot >  0.60 & value2plot <= 0.65 ~ "60%-65%",
      value2plot >  0.65 & value2plot <= 0.70 ~ "65%-70%",
      value2plot >  0.70 & value2plot <= 0.75 ~ "70%-75%",
      value2plot >  0.75 & value2plot <= 0.80 ~ "75%-80%",
      value2plot >  0.80 & value2plot <= 0.85 ~ "80%-85%",
      value2plot >  0.85 & value2plot <= 90 ~ "85%-90%"
    ),
    color_group = as.factor(color_group)
  )

cat_palette <- c("60%-65%"   = "#E03849",
                 "65%-70%"  = "#FF7900",
                 "70%-75%"  = "#FFC818",
                 "75%-80%"  = "#46B5FF",
                 "80%-85%"  = "#0C75B6",
                 "85%-90%" = "#18538E")
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
  plot_layout(ncol = 2, nrow = 1, widths = c(1, 3), heights = c(1,2))

ggsave(plot = viz, filename = "map_indice.svg", width = 10, height = 10)
