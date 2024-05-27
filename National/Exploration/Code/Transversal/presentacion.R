data_subset.df <- Main_database_2008 %>%
  mutate(
    defensa_mp = 
      case_when(
        P4_1_05 == 1 ~ "Defensa en el MP",
        P4_1_05 == 2 ~ "Ausencia de defensa en el MP",
        T ~ NA_character_
      ),
    tiempo_sentencia =
      (as.numeric(P5_4_A) * 12) + as.numeric(P5_4_M),
    años_sentencia = tiempo_sentencia/12
  ) %>%
  group_by(defensa_mp) %>%
  summarise(
    años_sentencia = mean(años_sentencia, na.rm = T)
  ) %>%
  drop_na()

data2plot <- data_subset.df %>%
  mutate(labels = defensa_mp,
         value2plot = años_sentencia,
         label_figures = round(años_sentencia, 2))

colors4plot <- c("Defensa en el MP" = "#003B88", 
                 "Ausencia de defensa en el MP" = "#fa4d57")

plot <- ggplot(data2plot,
               aes(
                 x     = labels,
                 y     = value2plot,
                 fill  = labels,
                 label = label_figures
               )) +
  geom_bar(stat = "identity",
           show.legend = F,
           position = position_dodge(widt = 0.9)) +
  geom_text(aes(y    = value2plot + 1), 
            position = position_dodge(widt = 0.9),
            color    = "black",
            family   = "Lato Full",
            fontface = "bold", 
            size = 8.435035)  +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0,20,5),
                     position = "left", name = "Años promedio de sentencia") + 
  theme(
                       
                       panel.background   = element_blank(),
                       plot.background    = element_blank(),
                       panel.grid.major   = element_line(size     = 0.25,
                                                         colour   = "#5e5c5a",
                                                         linetype = "dashed"),
                       panel.grid.minor   = element_blank(),
                       panel.grid.major.y = element_blank(),
                       panel.grid.major.x = element_line(color = "#D0D1D3")) +
  theme(
    axis.title.x       = element_blank(),
    axis.ticks         = element_blank(),
    axis.title.y        = element_markdown(family   = "Lato Full",
                                          face     = "bold",
                                          size     = 7.029196*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)
                                          ),
    axis.text.y        = element_markdown(family   = "Lato Full",
                                          face     = "bold",
                                          size     = 7.029196*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0),
                                          hjust = 0), 
    plot.caption = element_markdown(family   = "Lato Full",
                                    face     = "plain",
                                    size     = 7.029196*.pt,
                                    color    = "#524F4C", 
                                    vjust    = 0, 
                                    hjust    = 0, 
                                    margin = margin(20, 0, 0, 0)),
    axis.text.x        = element_markdown(family   = "Lato Full",
                                          face     = "bold",
                                          size     = 7.029196*.pt,
                                          color    = "#524F4C"),
    plot.title          = element_text(family   = "Lato Full",
                                       face     = "bold",
                                       size     = 8.786495*.pt,
                                       color    = "black",
                                       margin   = margin(0, 0, 10, 0),
                                       hjust    = 0), 
    plot.subtitle      = element_text(family   = "Lato Full",
                                      face     = "plain",
                                      size     = 8.786495*.pt,
                                      color    = "black",
                                      margin   = margin(2.5, 0, 20, 0),
                                      hjust    = 0),
    legend.text        =  element_markdown(family   = "Lato Full",
                                           face     = "plain",
                                           size     = 7.029196*.pt,
                                           color    = "#524F4C"),
    legend.title       = element_markdown(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 7.029196*.pt,
                                          color    = "#524F4C")
  )

ggsave(plot = plot,
       filename = "Años sentencia.svg", 
       width = 10,
       height = 7.5)
