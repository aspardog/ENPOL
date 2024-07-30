## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Trato diferenciado
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Julio 11, 2024
##
## This version:      Julio 11, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Trato diferenciado                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
descripcion.fn <- function(
  
    
  
){
  
  # Define the grouping variables
  grouping_vars <- c("Sexo", "Discapacidad", "Etnia", "Educacion_inferior",
                     "Color_piel_oscuro", "LGBTQ", "Edad_menor30",
                     "Vulnerabilidad_economica")
  
  data_subset.df <- master_data.df %>%
    mutate(
      Educacion_inferior = case_when(
        Educacion_superior == 0 ~ "Cuenta con título de educación universitaria",
        Educacion_superior == 1 ~ "No cuenta con título de educación universitaria",
        TRUE ~ NA_character_
      ),
      Color_piel_oscuro = case_when(
        Color_piel_oscuro == 0 ~ "Color de piel claro",
        Color_piel_oscuro == 1 ~ "Color de piel oscuro",
        TRUE ~ NA_character_
      ),
      LGBTQ = case_when(
        LGBTQ == 1 ~ "Pertenece a la comunidad LGBTQ",
        LGBTQ == 0 ~ "No pertenece a la comunidad LGBTQ",
        TRUE ~ NA_character_
      ),
      Etnia = case_when(
        Etnia == 1 ~ "Afromexicano o indígena",
        Etnia == 0 ~ "No se identifica con ninguna etnia",
        TRUE ~ NA_character_
      ),
      Edad_menor30 = case_when(
        Edad_menor30 == 1 ~ "Menor a 30 años",
        Edad_menor30 == 0 ~ "Mayor o igual a 30 años",
        TRUE ~ NA_character_
      ),
      Vulnerabilidad_economica = case_when(
        vulnerabilidad_economica == 1 ~ "Vulnerable económicamente",
        vulnerabilidad_economica == 0 ~ "No vulnerable económicamente",
        TRUE ~ NA_character_
      ),
      Discapacidad = case_when(
        discapacidad == 1 ~ "Reporta algún tipo de discapacidad",
        discapacidad == 0 ~ "No presenta discapacidad",
        TRUE ~ NA_character_
      )
    ) %>%
    select(Sexo, Educacion_inferior, Color_piel_oscuro, LGBTQ, Etnia, Edad_menor30, 
           Vulnerabilidad_economica, Discapacidad) %>%
    pivot_longer(cols = everything(), names_to = "category", values_to = "labels")
  
  colors4plot <- c("#2a2a9A", "#a90099")
  
  # Loop over the grouping variables
  for (group_var in grouping_vars) {
    # Data manipulation and summarization
    data2plot <- data_subset.df %>%
      filter(category == group_var) %>%
      mutate(
        counter = 1,
        total = sum(counter, na.rm = TRUE)
      ) %>%
      group_by(labels) %>%
      summarise(
        nobs = n(),
        value2plot = nobs / total * 100,
        figure = paste0(round(value2plot, 0), "%")
      ) %>%
      distinct() %>%
      drop_na() %>%
      ungroup() %>%
      arrange(-value2plot) %>%
      mutate(
        order_var = row_number()
      ) %>%
      mutate(
        categories = if_else(
          labels %in% c("Femenino", "No cuenta con título de educación universitaria",
                           "Color de piel oscuro", "Pertenece a la comunidad LGBTQ",
                           "Afromexicano o indígena", "Menor a 30 años", 
                           "Vulnerable económicamente", "Reporta algún tipo de discapacidad"),
          "Main", "Second")
      )
    
    if(group_var %in% "Sexo"){
      
      colors4plot <- c("Femenino"   = "#2a2a9A", 
                       "Masculino"  = "#a90099")
      
    } else if(group_var %in% "Educacion_inferior"){
      
      colors4plot <- c("No cuenta con título de educación universitaria"   = "#2a2a9A", 
                       "Cuenta con título de educación universitaria"      = "#a90099")
      
    } else if(group_var %in% "Color_piel_oscuro"){
      
      colors4plot <- c("Color de piel oscuro" = "#2a2a9A",
                       "Color de piel claro"  = "#a90099")
      
    } else if(group_var %in% "LGBTQ") {
      
      colors4plot <- c("Pertenece a la comunidad LGBTQ"     = "#2a2a9A",
                       "No pertenece a la comunidad LGBTQ"  = "#a90099")
      
    } else if(group_var %in% "Etnia"){
      
      colors4plot <- c("Afromexicano o indígena" = "#2a2a9A",
                       "No se identifica con ninguna etnia" = "#a90099")
      
    } else if (group_var %in% "Edad_menor30") {
      
      colors4plot <- c("Menor a 30 años" = "#2a2a9A",
                       "Mayor o igual a 30 años" = "#a90099")
      
    } else if (group_var %in% "Vulnerabilidad_economica") {
      
      colors4plot <- c("Vulnerable económicamente" = "#2a2a9A",
                       "No vulnerable económicamente" = "#a90099")
      
    } else {
      
      colors4plot <- c("Reporta algún tipo de discapacidad" = "#2a2a9A",
                       "No presenta discapacidad" = "#a90099")
      
    } 
    
    # Create the bar chart
    plot <- barsChart.fn(
      data.df      = data2plot,
      groupVar     = F,
      categories_grouping_var = categories,
      colors4plot = colors4plot, 
      order = TRUE,
      orientation = "horizontal"
    )  
    
    # Save the plot
    ggsave(plot = plot, 
           filename = paste0(
             path2SP,
             "/National/Visualization",
             "/Output/Trato Diferenciado/",
             savePath, "/",
             "/barCharts", group_var, ".svg"),
           width = 189.7883,
           height = 75,
           units = "mm",
           dpi = 72,
           device = "svg")
  }
  
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Trato diferenciado                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

trato_diferenciado.fn <- function(){
  
  # Define the grouping variables
  grouping_vars <- c("Sexo", "Discapacidad", "Etnia", "Educacion_inferior",
                     "Color_piel_oscuro", "LGBTQ", "Edad_menor30",
                     "Vulnerabilidad_economica")
  
  # Define the significance level
  alpha <- 0.05
  
  data_subset.df <- master_data.df %>%
    mutate(
      uso_excesivo = case_when(
        proporcionalidad_uso_fuerza == 0 ~ 1,
        proporcionalidad_uso_fuerza == 1 ~ 0
      ),
      detencion_corrupcion = case_when(
        P3_21_1 == "1" | P3_21_2 == "1" ~ 1,
        P3_21_1 == "2" & P3_21_2 == "2" ~ 0,
        TRUE ~ NA_real_
      ),
      mp_corrupcion = case_when(
        P4_15_1 == "1" | P4_15_3 == "1" ~ 1,
        P4_15_1 == "2" & P4_15_3 == "2" ~ 0,
        TRUE ~ NA_real_
      ),
      juzgado_corrupcion = case_when(
        P5_45_1 == "1" | P5_45_3 == "1" ~ 1,
        P5_45_1 == "2" & P5_45_3 == "2" ~ 0,
        TRUE ~ NA_real_
      ),
      corrupcion_general = case_when(
        detencion_corrupcion == 1 | mp_corrupcion == 1 | juzgado_corrupcion == 1 ~ 1,
        detencion_corrupcion == 0 & mp_corrupcion == 0 & juzgado_corrupcion == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      procedimiento_abreviado = case_when(
        as.numeric(P5_6) == 1 ~ 0,
        as.numeric(P5_6) == 2 ~ 1,
        TRUE ~ NA_real_
      ),
      Educacion_inferior = case_when(
        Educacion_superior == 0 ~ "Cuenta con título de educación universitaria",
        Educacion_superior == 1 ~ "No cuenta con título de educación universitaria",
        TRUE ~ NA_character_
      ),
      Color_piel_oscuro = case_when(
        Color_piel_oscuro == 0 ~ "Color de piel claro",
        Color_piel_oscuro == 1 ~ "Color de piel oscuro",
        TRUE ~ NA_character_
      ),
      LGBTQ = case_when(
        LGBTQ == 1 ~ "Pertenece a la comunidad LGBTQ",
        LGBTQ == 0 ~ "No pertenece a la comunidad LGBTQ",
        TRUE ~ NA_character_
      ),
      Etnia = case_when(
        Etnia == 1 ~ "Afromexicano o indígena",
        Etnia == 0 ~ "No se identifica con ninguna etnia",
        TRUE ~ NA_character_
      ),
      Edad_menor30 = case_when(
        Edad_menor30 == 1 ~ "Menor a 30 años",
        Edad_menor30 == 0 ~ "Mayor o igual a 30 años",
        TRUE ~ NA_character_
      ),
      Vulnerabilidad_economica = case_when(
        vulnerabilidad_economica == 1 ~ "Vulnerable económicamente",
        vulnerabilidad_economica == 0 ~ "No vulnerable económicamente",
        TRUE ~ NA_character_
      ),
      Discapacidad = case_when(
        discapacidad == 1 ~ "Reporta algún tipo de discapacidad",
        discapacidad == 0 ~ "No presenta discapacidad",
        TRUE ~ NA_character_
      ),
      ppo = 
        case_when(
          tipo_prision_preventiva %in% c("Prisión Preventiva Oficiosa") ~ 1,
          tipo_prision_preventiva %in% c("Prisión Preventiva Justificada",
                                         "Proceso en libertad") ~ 0
        )
    )
  
  # Loop over the grouping variables
  for (group_var in grouping_vars) {
    
    data2plot <- data_subset.df %>%
      rename(group_var = all_of(group_var)) %>%
      group_by(group_var) %>%
      mutate(nobs = n()) %>%
      group_by(group_var) %>%
      summarise(
        across(
          c(corrupcion_general, uso_excesivo, tortura_generalizada, 
            det_ninguna, procedimiento_abreviado, ppo),
          mean, 
          na.rm = TRUE,
          .names = "{col}_mean"
        ),
        across(
          c(corrupcion_general, uso_excesivo, tortura_generalizada, 
            det_ninguna, procedimiento_abreviado, ppo),
          sd, 
          na.rm = TRUE,
          .names = "{col}_sd"
        ),
        n_obs = mean(nobs, na.rm = TRUE),
        n_obs = as.character(n_obs)
      ) %>%
      drop_na() %>%
      pivot_longer(!c(group_var, n_obs),
                   names_to = c("category", "stat"),
                   names_pattern = "(.*)_(.*)",
                   values_to = "value") %>%
      pivot_wider(c(category, group_var, n_obs),
                  names_from = stat,
                  values_from = value) 
    
    # Data transformation for plotting
    data2plot <- data2plot %>%
      mutate(
        n_obs = as.numeric(n_obs),
        labels = case_when(
          category == "corrupcion_general" ~ "Uso de corrupción",
          category == "uso_excesivo" ~ "Uso excesivo de la fuerza",
          category == "tortura_generalizada" ~ "Tortura generalizada",
          category == "det_ninguna" ~ "Detenciones irregulares",
          category == "procedimiento_abreviado" ~ "Uso de procedimiento abreviado",
          category == "ppo" ~ "Uso de prisión preventiva oficiosa"
        ),
        lower = mean - qt(1 - alpha / 2, (n() - 1)) * sd / sqrt(n_obs),
        upper = mean + qt(1 - alpha / 2, (n() - 1)) * sd / sqrt(n_obs)
      ) %>%
      rename(values = mean) %>%
      mutate(
        figure = paste0(round(values * 100, 0), "%"),
        order_values = case_when(
          category == "corrupcion_general" ~ 3,
          category == "uso_excesivo" ~ 2,
          category == "tortura_generalizada" ~ 4,
          category == "det_ninguna" ~ 5,
          category == "procedimiento_abreviado" ~ 7,
          category == "PPO" ~ 6
        )
      ) %>%
      mutate(
        group_var = if_else(
          group_var %in% c("Femenino", "No cuenta con título de educación universitaria",
                           "Color de piel oscuro", "Pertenece a la comunidad LGBTQ",
                           "Afromexicano o indígena", "Menor a 30 años", 
                           "Vulnerable económicamente", "Reporta algún tipo de discapacidad"),
          "Main", "Second")
      )
    
    colors4plot <- c("Main"   = "#2a2a9A", 
                     "Second" = "#a90099")
    
    
    # Create and save the plot
    chart <- errorDotsChart(
      data2plot = data2plot,
      labels = "labels",
      group = "group_var",
      category = "category",
      custom_order = FALSE,
      order_values = order_values,
      figures = figure,
      values = values,
      lower = lower,
      upper = upper, 
      colors4plot = colors4plot
    ); chart
    
    ggsave(plot = chart, 
           filename = paste0(
             path2SP,
             "/National/Visualization",
             "/Output/Trato Diferenciado/",
             savePath, "/",
             "/dotCharts", group_var, ".svg"), 
           width = 250, 
           height = 125,
           units = "mm",
           dpi = 72,
           device = "svg")
  }

}

