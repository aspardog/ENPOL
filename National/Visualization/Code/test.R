## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Percepcion proceso justo
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Abril 24, 2024
##
## This version:      Abril 24, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

alpha <- 0.05

data_subset.df <- master_data.df %>%
  mutate(
    uso_excesivo =
      case_when(
        proporcionalidad_uso_fuerza == 0 ~ 1,
        proporcionalidad_uso_fuerza == 1 ~ 0
      ),
    detencion_corrupcion = case_when(P3_21_1 == "1" | P3_21_2 == "1" ~ 1,
                                     P3_21_1 == "2" & P3_21_2 == "2" ~ 0,
                                     T ~ NA_real_),
    mp_corrupcion= case_when(P4_15_1 == "1" | P4_15_3 == "1" ~ 1,
                             P4_15_1 == "2" & P4_15_3 == "2" ~ 0,
                             T ~ NA_real_),
    juzgado_corrupcion= case_when(P5_45_1 == "1" | P5_45_3 == "1" ~ 1,
                                  P5_45_1 == "2" & P5_45_3 == "2" ~ 0,
                                  T ~ NA_real_),
    corrupcion_general = case_when(detencion_corrupcion == 1 | mp_corrupcion == 1  | juzgado_corrupcion == 1 ~ 1,
                                   detencion_corrupcion == 0 & mp_corrupcion == 0  & juzgado_corrupcion == 0 ~ 0,
                                   T~ NA_real_),
    procedimiento_abreviado =
      case_when(
        as.numeric(P5_6) == 1 ~ 0,
        as.numeric(P5_6) == 2 ~ 1,
        T ~ NA_real_
      ),
    Educacion_inferior =
      case_when(
        Educacion_superior == 0 ~ "Cuenta con título de educación universitaria",
        Educacion_superior == 1 ~ "No cuenta con título de educación universitario",
        T ~ NA_character_
      ),
    Color_piel_oscuro       =
      case_when(
        Color_piel_claro      == 0 ~ "Color de piel claro",
        Color_piel_claro      == 1 ~ "Color de piel oscuro",
        T ~ NA_character_
      ),
    LGBTQ                 = 
      case_when(
        LGBTQ                 == 1 ~ "Pertenece a la comunidad LGBTQ",
        LGBTQ                 == 0 ~ "No pertenece a la comunidad LGBTQ",
        T ~ NA_character_
      ),
    Etnia                 =
      case_when(
        Etnia                 == 1 ~ "Afromexicano o indígena",
        Etnia                 == 0 ~ "No se identifica con ninguna etnia",
        T ~ NA_character_
      ),
    Edad_menor30          =
      case_when(
        Edad_menor30          == 1 ~ "Menor a 30 años",
        Edad_menor30          == 0 ~ "Mayor o igual a 30 años",
        T ~ NA_character_
      ),
    vulnerabilidad_economica  =
      case_when(
        vulnerabilidad_economica == 1 ~ "Vulnerable economicamente",
        vulnerabilidad_economica == 0 ~ "No vulnerable economicamente",
        T ~ NA_character_
      ),
    discapacidad      =
      case_when(
        discapacidad == 1 ~ "Reporta algún tipo de discapacidad",
        discapacidad == 0 ~ "No presenta discapacidad",
        T ~ NA_character_
      )
  ) %>%
  group_by(Sexo) %>%
  mutate(
   nobs = n()
  ) %>%
  group_by(Sexo) %>%
  summarise(
    across(
      c(indicator_general, corrupcion_general, uso_excesivo, tortura_generalizada, 
        det_ninguna, procedimiento_abreviado, PPO),
      mean, 
      na.rm = T,
      .names = "{col}_mean"
      ),
    across(
      c(indicator_general, corrupcion_general, uso_excesivo, tortura_generalizada, 
        det_ninguna, procedimiento_abreviado, PPO),
      sd, 
      na.rm = T,
      .names = "{col}_sd"
      ),
    n_obs = mean(nobs, na.rm = T),
    n_obs = as.character(n_obs)
    )  %>%
  drop_na() %>%
  pivot_longer(!c(Sexo,n_obs),
               names_to      = c("category", "stat"),
               names_pattern = "(.*)_(.*)",
               values_to     = "value")  %>%
  pivot_wider(c(category, Sexo, n_obs),
              names_from  = stat,
              values_from = value) 

data2plot <- data_subset.df%>%
  mutate(
    n_obs  = as.numeric(n_obs),
    labels = case_when(
      category == "indicator_general"       ~ "Índice de criterios mínimos",
      category == "corrupcion_general"      ~ "Uso de corrupción",
      category == "uso_excesivo"            ~ "Uso excesivo de la fuerza",
      category == "tortura_generalizada"    ~ "Tortura generalizada",
      category == "det_ninguna"             ~ "Detenciones irregulares",
      category == "procedimiento_abreviado" ~ "Uso de procedimiento abreviado",
      category == "PPO"                     ~ "Uso de prisión preventiva oficiosa"
    ),
    lower = mean - qt(1- alpha/2, (n() - 1))*sd/sqrt(n_obs),
    upper = mean + qt(1- alpha/2, (n() - 1))*sd/sqrt(n_obs)
  ) %>%
  rename(values = mean) %>%
  mutate(
    figure = paste0(round(values*100,0), "%"),
    order_values =
      case_when(
        category == "indicator_general"       ~ 1,
        category == "corrupcion_general"      ~ 3,
        category == "uso_excesivo"            ~ 2,
        category == "tortura_generalizada"    ~ 4,
        category == "det_ninguna"             ~ 5,
        category == "procedimiento_abreviado" ~ 7,
        category == "PPO"                     ~ 6
      )
  )
  
# Applying plotting function

colors4plot <- c("#2a2a9A", 
                 "#a90099") 

chart <- errorDotsChart(
  data2plot   = data2plot,
  labels      = "labels",
  group       = "Sexo",
  category    = "category",
  custom_order = F,
  order_values = order_values,
  figures     = figure,
  values      = values,
  lower       = lower,
  upper       = upper, 
  colors4plot = colors4plot
  );chart

ggsave(plot = chart, 
       filename = paste0(
         path2SP,
         "/National/Visualization",
         "/Output/Trato Diferenciado/",
         savePath,"/Sexo",
         "/dotChartsSexo.svg"), 
       width  = 250, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")
