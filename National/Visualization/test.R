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
    figure = paste0(round(values*100,0), "%")
  )
  
# Applying plotting function

colors4plot <- c("#2a2a9A", 
                 "#a90099") 

chart <- errorDotsChart(
  data2plot   = data2plot,
  labels      = "labels",
  group       = "Sexo",
  category    = "category",
  figures     = figure,
  values      = values,
  lower       = lower,
  upper       = upper, 
  colors4plot = colors4plot
  )

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
