## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Robustness
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Abril 2, 2024
##
## This version:      Abril 2, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Personas sentenciadas
data_subset.df <- master_data.df %>%
  filter(sentenciado == 1) %>%
  filter(Anio_arresto > 2018) %>%
  mutate(
    across(
      starts_with("P5_11_"),
      ~as.numeric(.x)
    ),
    a_delito_robo_vehiculo =
      case_when(
        P5_11_01 == 1 ~ 1,
        P5_11_01 == 0 ~ 0
      ),
    a_delito_casa =
      case_when(
        P5_11_02 == 1 ~ 1,
        P5_11_02 == 0 ~ 0
      ),    
    a_delito_robo_negocio =
      case_when(
        P5_11_03 == 1 ~ 1,
        P5_11_03 == 0 ~ 0
      ),    
    a_delito_robo_transporte =
      case_when(
        P5_11_04 == 1 ~ 1,
        P5_11_04 == 0 ~ 0
      ),    
    a_delito_robo_autopartes =
      case_when(
        P5_11_06 == 1 ~ 1,
        P5_11_06 == 0 ~ 0
      ),    
    a_delito_robo_otro =
      case_when(
        P5_11_07 == 1 ~ 1,
        P5_11_07 == 0 ~ 0
      ),    
    a_delito_posesion_drogas =
      case_when(
        P5_11_08 == 1 ~ 1,
        P5_11_08 == 0 ~ 0
      ),    
    a_delito_comercio_drogas =
      case_when(
        P5_11_09 == 1 ~ 1,
        P5_11_09 == 0 ~ 0
      ),    
    a_delito_lesiones =
      case_when(
        P5_11_10 == 1 ~ 1,
        P5_11_10 == 0 ~ 0
      ),    
    a_delito_homicidio_culposo =
      case_when(
        P5_11_11 == 1 ~ 1,
        P5_11_11 == 0 ~ 0
      ),    
    a_delito_homicidio_doloso =
      case_when(
        P5_11_12 == 1 ~ 1,
        P5_11_12 == 0 ~ 0
      ),    
    a_delito_portacion_armas =
      case_when(
        P5_11_13 == 1 ~ 1,
        P5_11_13 == 0 ~ 0
      ),    
    a_delito_asistencia_familiar =
      case_when(
        P5_11_14 == 1 ~ 1,
        P5_11_14 == 0 ~ 0
      ),    
    a_delito_violencia_familiar =
      case_when(
        P5_11_15 == 1 ~ 1,
        P5_11_15 == 0 ~ 0
      ),    
    a_delito_daño_propiedad =
      case_when(
        P5_11_16 == 1 ~ 1,
        P5_11_16 == 0 ~ 0
      ),    
    a_delito_secuestro =
      case_when(
        P5_11_17 == 1 ~ 1,
        P5_11_17 == 0 ~ 0
      ),
    a_delito_violencia_sexual =
      case_when(
        P5_11_18 == 1 ~ 1,
        P5_11_18 == 0 ~ 0
      ),
    a_delito_violencia_sexual =
      case_when(
        P5_11_18 == 1 ~ 1,
        P5_11_18 == 0 ~ 0
      ),
    a_delito_fraude =
      case_when(
        P5_11_19 == 1 ~ 1,
        P5_11_19 == 0 ~ 0
      ),
    a_delito_delincuencia_organizada =
      case_when(
        P5_11_20 == 1 ~ 1,
        P5_11_20 == 0 ~ 0
      ),
    a_delito_otros_violencia_sexual =
      case_when(
        P5_11_21 == 1 ~ 1,
        P5_11_21 == 0 ~ 0
      ),
    a_delito_extorsion =
      case_when(
        P5_11_22 == 1 ~ 1,
        P5_11_22 == 0 ~ 0
      ),
    a_delito_privacion_libertad =
      case_when(
        P5_11_23 == 1 ~ 1,
        P5_11_23 == 0 ~ 0
      ),
    a_delito_abuso_confianza =
      case_when(
        P5_11_24 == 1 ~ 1,
        P5_11_24 == 0 ~ 0
      ),
    a_delito_amenaza =
      case_when(
        P5_11_25 == 1 ~ 1,
        P5_11_25 == 0 ~ 0
      ),
    a_delito_otro =
      case_when(
        P5_11_18 == 1 ~ 1,
        P5_11_18 == 0 ~ 0
      )
  ) %>%
  ungroup() %>%
  summarise(
    across(
      starts_with("a_delito_"),
      ~mean(.x, na.rm = T)
      )
    ) %>%
  drop_na() %>%
  pivot_longer(
    everything(), names_to = "category", values_to = "values"
  ) %>%
  arrange(values)

# Homicidios

data_subset.df <- master_data.df %>%
  filter(Anio_arresto>=2018 & Anio_arresto<= 2021) %>%
  mutate(
    homicidios =
      case_when(
        P5_11_12 == 1 | P5_31_12 == 1 ~ 1,
        P5_11_12 == 0 | P5_31_12 == 0 ~ 0
      ),
    homicidios_expan = as.numeric(homicidios)*as.numeric(FAC_PER)
  ) %>%
  ungroup() %>%
  group_by(Anio_arresto) %>%
  summarise(
    sum(homicidios_expan, na.rm = T)
  )

data_subset.df <- Main_database %>%
  filter(Anio_arresto>=2018 & Anio_arresto<= 2021) %>%
  filter(NSJP == 1) %>%
  mutate(
    homicidios =
      case_when(
        P5_11_12 == 1 | P5_31_12 == 1 ~ 1,
        P5_11_12 == 0 | P5_31_12 == 0 ~ 0
      ),
    homicidios_expan = as.numeric(homicidios)*as.numeric(FAC_PER)
  ) %>%
  ungroup() %>%
  group_by(Anio_arresto) %>%
  summarise(
    sum(homicidios_expan, na.rm = T)
  )

