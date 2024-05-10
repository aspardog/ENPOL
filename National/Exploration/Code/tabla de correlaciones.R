correlaciones <- master_data.df %>%
  filter(sentenciado == 1) %>% 
  mutate(
    mujer = if_else(
      Sexo == "Femenino", 1, 0
    ),
  ) %>%
  select(mujer, Educacion_superior, Color_piel_claro, LGBTQ, Edad_menor30, discapacidad,
         vulnerabilidad_economica)


cor(correlaciones, use = "pairwise.complete.obs")

cor(correlaciones, use = "na.or.complete")
