Mixed_trends <- Main_database_completa %>%
  rename(mainVar = det_ninguna) %>%
  filter(Anio_arresto > 2010) %>%
  filter(Anio_arresto != 9998) %>%
  filter(Anio_arresto != 9999) %>%
  group_by(Anio_arresto, ENPOL) %>%
  summarise(mainVar = 
              mean(as.numeric(mainVar), na.rm = T)*100
            ) %>%
  pivot_wider(id_cols = Anio_arresto, names_from = "ENPOL", values_from = "mainVar", names_prefix = "ENPOL_") %>%
  mutate(
    GAP = abs(((ENPOL_2021-ENPOL_2016))/ENPOL_2016)*100
  ) %>%
  pivot_longer(cols = !c(Anio_arresto, GAP), names_to = "ENPOL", values_to = "mainVar") %>%
  drop_na(mainVar)

data_2016 <- subset(Mixed_trends, ENPOL == "ENPOL_2016") %>%
  mutate(group_var = "dotted") # Assign dotted to 2016 data for consistent line type

data_2021 <- subset(Mixed_trends, ENPOL == "ENPOL_2021")%>%
  mutate(group_var = if_else(
    Anio_arresto <= 2016, "dotted", "normal"
  ))

GAP <- Mixed_trends %>%
  select(Anio_arresto, GAP) %>% 
  distinct() %>%
  group_by(Anio_arresto) %>%
  summarise(
    TC_promedio = mean(GAP, na.rm = T)
  ) %>%
  pull()

ggplot() +
  geom_vline(xintercept = "2016", color = "red", linetype = "dotted") + # Add vertical line at 2016
  geom_line(data = subset(data_2021, Anio_arresto > 2015), aes(x = Anio_arresto, y = mainVar, color = "2021", group = "2021"), size = 1.2)  +
  geom_line(data = data_2016, aes(x = Anio_arresto, y = mainVar, color = "2016", group = group_var,  linetype = group_var,), size = 1.2) +
  geom_line(data = data_2021, aes(x = Anio_arresto, y = mainVar, color = "2021", linetype = group_var, group = group_var), size = 1.2)  +
  scale_linetype_manual(values = c("dotted" = "dotted", "normal" = "solid")) +
  labs(title = "Comparison of mainVar between 2016 and 2021",
       subtitle = paste0("La tasa de cambio porcentual promedio es de ", round(GAP,0), "%"),
       x = "Year of Arrest",
       y = "mainVar",
       color = "ENPOL",
       linetype = "Line Type") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 105),
                         expand = c(0,0),
                         breaks = seq(0,100,10),
                         labels = paste0(seq(0,100,10), "%"))  +
  WJP_theme() +
  theme(panel.grid.major.x = element_line(colour = "#d1cfd1"),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"),
        ggh4x.axis.ticks.length.minor = rel(1))



