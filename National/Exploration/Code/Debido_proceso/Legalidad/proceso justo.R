data_subset.df <- Main_database_2008 %>%
  mutate(
    sentencia_justa = 
      case_when(
        as.numeric(P5_26B) == 1 | as.numeric(P5_26B) == 2 ~ 1,
        as.numeric(P5_26B) == 3 | as.numeric(P5_26B) == 4~ 0,
        T ~ NA_real_
      ),
    proceso_justo = 
      case_when(
        as.numeric(P5_26A) == 1 ~ 1,
        as.numeric(P5_26A) == 0 ~ 0,
        T ~ NA_real_
      ),
    juicio = 
      case_when(
        as.numeric(P5_6) == 1 ~ 1,
        as.numeric(P5_6) == 2 ~ 0,
        T ~ NA_real_
      ),
    p_abreviado =
      case_when(
        as.numeric(P5_6) == 1 ~ 0,
        as.numeric(P5_6) == 2 ~ 1,
        T ~ NA_real_
      ),
    inocencia =
      case_when(
        as.numeric(P3_1) == 1 | as.numeric(P3_1) == 2 ~ 0,
        as.numeric(P3_1) == 3 | as.numeric(P3_1) == 4 ~ 1,
        T ~ NA_real_
      ),
    culpabilidad = 
      case_when(
        as.numeric(P3_1) == 1 | as.numeric(P3_1) == 2 ~ 1,
        as.numeric(P3_1) == 3 | as.numeric(P3_1) == 4 ~ 0,
        T ~ NA_real_
      ),
    P5_6 =
      case_when(
        as.numeric(P5_6) == 1 ~ "juicio",
        as.numeric(P5_6) == 2 ~ "procedimiento abreviado",
        T ~ NA_character_
      ),
    P3_1 = 
      case_when(
        as.numeric(P3_1) == 1 | as.numeric(P3_1) == 2 ~ "culpable",
        as.numeric(P3_1) == 3 | as.numeric(P3_1) == 4 ~ "inocente",
        T ~ NA_character_
      ),
    escucha = 
      case_when(
        as.numeric(P5_26) == 1 | as.numeric(P5_26) == 2 ~ 1,
        as.numeric(P5_26) == 3 | as.numeric(P5_26) == 4~ 0,
        T ~ NA_real_
      )
  ) 

# Solicitud 1

# Extract and print the data for group1
group1 <- data_subset.df %>%
  filter(juicio == 1) %>%
  pull(proceso_justo)
n_grupo1 <- group1 %>% na.omit() %>% length()
mean_grupo1 <- mean(group1, na.rm = TRUE)

# Extract and print the data for group2
group2 <- data_subset.df %>%
  filter(p_abreviado == 1) %>%
  pull(proceso_justo)
n_grupo2 <- group2 %>% na.omit() %>% length()
mean_grupo2 <- mean(group2, na.rm = TRUE)

# Perform t-test between group1 and group2
ttest <- t.test(group1, group2)

# Store results in a tibble
result_df <- tibble(
  "dependent_var"      = paste0("Proceso justo"),
  "independent_var"    = paste0("Producto de sentencia"),
  "grupo1"             = paste0("Juicio"),
  "grupo1_mean"        = mean_grupo1,
  "grupo1_nobs"        = n_grupo1,
  "grupo2"             = paste0("Procedimiento abreviado"),
  "grupo2_mean"        = mean_grupo2,
  "grupo2_nobs"        = n_grupo2,
  "diff_mean"          = mean_grupo1 - mean_grupo2,
  "direccion"          = if_else(mean_grupo1 - mean_grupo2 > 0, "Positiva", "Negativa"),
  "pvalue"             = ttest$p.value,
  "significativo10"    = if_else(ttest$p.value < 0.1, "Si", "No"),
  "significativo5"     = if_else(ttest$p.value < 0.05, "Si", "No"),
  "significativo1"     = if_else(ttest$p.value < 0.01, "Si", "No")
)

# Solicitud 2 

# Extract and print the data for group1
group1 <- data_subset.df %>%
  filter(inocencia == 1) %>%
  pull(proceso_justo)
n_grupo1 <- group1 %>% na.omit() %>% length()
mean_grupo1 <- mean(group1, na.rm = TRUE)

# Extract and print the data for group2
group2 <- data_subset.df %>%
  filter(culpabilidad == 1) %>%
  pull(proceso_justo)
n_grupo2 <- group2 %>% na.omit() %>% length()
mean_grupo2 <- mean(group2, na.rm = TRUE)

# Perform t-test between group1 and group2
ttest <- t.test(group1, group2)

# Store results in a tibble
result_df2 <- tibble(
  "dependent_var"      = paste0("Proceso justo"),
  "independent_var"    = paste0("Percepción de culpabilidad"),
  "grupo1"             = paste0("Inocente"),
  "grupo1_mean"        = mean_grupo1,
  "grupo1_nobs"        = n_grupo1,
  "grupo2"             = paste0("Culpable"),
  "grupo2_mean"        = mean_grupo2,
  "grupo2_nobs"        = n_grupo2,
  "diff_mean"          = mean_grupo1 - mean_grupo2,
  "direccion"          = if_else(mean_grupo1 - mean_grupo2 > 0, "Positiva", "Negativa"),
  "pvalue"             = ttest$p.value,
  "significativo10"    = if_else(ttest$p.value < 0.1, "Si", "No"),
  "significativo5"     = if_else(ttest$p.value < 0.05, "Si", "No"),
  "significativo1"     = if_else(ttest$p.value < 0.01, "Si", "No")
)

resultados <- rbind(result_df,result_df2)


# Solicitud 2

data2table <- data_subset.df %>%
  group_by(proceso_justo) %>%
  summarise(
    indicador_general = mean(indicator_general, na.rm = T),
    indicador_gdh = mean(indicator_GDH, na.rm = T),
    indicador_uaa = mean(indicator_UAA, na.rm = T),
    indicador_pj = mean(indicator_PJ, na.rm = T)
  ) %>% 
  drop_na() %>%
  mutate(
    proceso_justo = 
      case_when(
        proceso_justo == 1 ~ "Proceso justo",
        proceso_justo == 0 ~ "Proceso injusto"
      )
  )

proceso_justo <- list(resultados, data2table)
openxlsx::write.xlsx(proceso_justo, "proceso_justo.xlsx")




# Solicitud 1

dato <- table(data_subset.df$sentencia_justa)/20535

proceso <- table(data_subset.df$proceso_justo)/20499

# Extract and print the data for group1
group1 <- data_subset.df %>%
  filter(juicio == 1) %>%
  pull(sentencia_justa)
n_grupo1 <- group1 %>% na.omit() %>% length()
mean_grupo1 <- mean(group1, na.rm = TRUE)

# Extract and print the data for group2
group2 <- data_subset.df %>%
  filter(p_abreviado == 1) %>%
  pull(sentencia_justa)
n_grupo2 <- group2 %>% na.omit() %>% length()
mean_grupo2 <- mean(group2, na.rm = TRUE)

# Perform t-test between group1 and group2
ttest <- t.test(group1, group2)

# Store results in a tibble
result_df <- tibble(
  "dependent_var"      = paste0("Proceso justo"),
  "independent_var"    = paste0("Producto de sentencia"),
  "grupo1"             = paste0("Juicio"),
  "grupo1_mean"        = mean_grupo1,
  "grupo1_nobs"        = n_grupo1,
  "grupo2"             = paste0("Procedimiento abreviado"),
  "grupo2_mean"        = mean_grupo2,
  "grupo2_nobs"        = n_grupo2,
  "diff_mean"          = mean_grupo1 - mean_grupo2,
  "direccion"          = if_else(mean_grupo1 - mean_grupo2 > 0, "Positiva", "Negativa"),
  "pvalue"             = ttest$p.value,
  "significativo10"    = if_else(ttest$p.value < 0.1, "Si", "No"),
  "significativo5"     = if_else(ttest$p.value < 0.05, "Si", "No"),
  "significativo1"     = if_else(ttest$p.value < 0.01, "Si", "No")
)

# Solicitud 2 

# Extract and print the data for group1
group1 <- data_subset.df %>%
  filter(inocencia == 1) %>%
  pull(sentencia_justa)
n_grupo1 <- group1 %>% na.omit() %>% length()
mean_grupo1 <- mean(group1, na.rm = TRUE)

# Extract and print the data for group2
group2 <- data_subset.df %>%
  filter(culpabilidad == 1) %>%
  pull(sentencia_justa)
n_grupo2 <- group2 %>% na.omit() %>% length()
mean_grupo2 <- mean(group2, na.rm = TRUE)

# Perform t-test between group1 and group2
ttest <- t.test(group1, group2)

# Store results in a tibble
result_df2 <- tibble(
  "dependent_var"      = paste0("Proceso justo"),
  "independent_var"    = paste0("Percepción de culpabilidad"),
  "grupo1"             = paste0("Inocente"),
  "grupo1_mean"        = mean_grupo1,
  "grupo1_nobs"        = n_grupo1,
  "grupo2"             = paste0("Culpable"),
  "grupo2_mean"        = mean_grupo2,
  "grupo2_nobs"        = n_grupo2,
  "diff_mean"          = mean_grupo1 - mean_grupo2,
  "direccion"          = if_else(mean_grupo1 - mean_grupo2 > 0, "Positiva", "Negativa"),
  "pvalue"             = ttest$p.value,
  "significativo10"    = if_else(ttest$p.value < 0.1, "Si", "No"),
  "significativo5"     = if_else(ttest$p.value < 0.05, "Si", "No"),
  "significativo1"     = if_else(ttest$p.value < 0.01, "Si", "No")
)

resultados <- rbind(result_df,result_df2)


# Solicitud 2

data2table <- data_subset.df %>%
  group_by(sentencia_justa) %>%
  summarise(
    indicador_general = mean(indicator_general, na.rm = T),
    indicador_gdh = mean(indicator_GDH, na.rm = T),
    indicador_uaa = mean(indicator_UAA, na.rm = T),
    indicador_pj = mean(indicator_PJ, na.rm = T)
  ) %>% 
  drop_na() %>%
  mutate(
    sentencia_justa = 
      case_when(
        sentencia_justa == 1 ~ "sentencia justa",
        sentencia_justa == 0 ~ "sentencia injusta"
      )
  )

sentencia_justa <- list(resultados, data2table)
openxlsx::write.xlsx(sentencia_justa, "sentencia_justa.xlsx")


dato <- table(data_subset.df$escuchado_x_juez)/19882

# Extract and print the data for group1
group1 <- data_subset.df %>%
  filter(escuchado_x_juez == 1) %>%
  pull(proceso_justo)
n_grupo1 <- group1 %>% na.omit() %>% length()
mean_grupo1 <- mean(group1, na.rm = TRUE)

group2 <- data_subset.df %>%
  filter(escuchado_x_juez == 0) %>%
  pull(proceso_justo)
n_grupo2 <- group2 %>% na.omit() %>% length()
mean_grupo2 <- mean(group2, na.rm = TRUE)


table(data_subset.df$escuchado_x_juez, data_subset.df$p_abreviado)
