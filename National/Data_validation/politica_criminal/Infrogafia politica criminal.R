## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Infografias política criminal
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

# Personas en prisión preventiva

data_subset.df <- master_data.df  %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  mutate(
    procesados = case_when(
      P5_3 == "1" | P5_3 == "2" ~ 1,
      P5_3 == "3" ~ 0,
      T ~ NA)
  ) %>%
  ungroup() %>%
  summarise(
    procesados = mean(procesados, na.rm = T)
  )

# Detenciones

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  mutate(
    orden_detencion = 
      case_when(
        P3_10 == 3 ~ 1,
        P3_10 == 1 | P3_10 == 2 | P3_10 == 4 | P3_10 == 5 ~ 0
      )
  ) %>%
  ungroup() %>%
  summarise(
    orden_detencion = mean(orden_detencion, na.rm = T)
  )

# Tiempo

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  mutate(
    seis_meses_proceso = 
      case_when(
        P5_10 <= 4 ~ 1,
        P5_10 == 5 | P5_10 == 6 | P5_10 == 7 ~ 0
      )
  ) %>%
  ungroup() %>%
  summarise(
    seis_meses_proceso = mean(seis_meses_proceso, na.rm = T)
  )

# Reincidencia 

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  mutate(
    reincidencia = 
      case_when(
        P9_1 == 1 ~ 1,
        P9_1 == 2 ~ 0
      )
  ) %>%
  ungroup() %>%
  summarise(
    reincidencia = mean(reincidencia, na.rm = T)
  )

# Homicidios dolosos y robo de vehiculo

data_snsp.df <- snsp %>%
  filter(Anio > 2017 & Anio < 2022) %>%
  mutate(
    Agosto = if_else(Anio == 2021, 0, Agosto),
    Septiembre = if_else(Anio == 2021, 0, Septiembre),
    Octubre = if_else(Anio == 2021, 0, Octubre),
    Noviembre = if_else(Anio == 2021, 0, Noviembre),
    Diciembre = if_else(Anio == 2021, 0, Diciembre)  
    ) %>%
  rowwise() %>%
  mutate(
    total = sum(c(Enero, 
                  Febrero, 
                  Marzo, 
                  Abril, 
                  Mayo, 
                  Junio, 
                  Julio, 
                  Agosto, 
                  Septiembre, 
                  Octubre, 
                  Noviembre, 
                  Diciembre), 
                na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    Delito = 
      case_when(
        `Subtipo de delito`== "Homicidio doloso" ~ "Homicidio doloso, SESNSP",
        `Subtipo de delito`== "Robo de veh\xedculo automotor" ~ "Robo de vehículo, SESNSP",
        T ~ "0"
      )
  ) %>%
  filter(Delito != "0") %>%
  group_by(Delito) %>%
  summarise(
    total = sum(total, na.rm = T)
  )

data_enpol.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  mutate(
    homicidios_dolosos =
      case_when(
        P5_11_12 == 1 | P5_31_12 == 1 ~ 1,
        P5_11_12 == 0 & P5_31_12 == 0 ~ 0
      ),
    robo_vehiculo =
      case_when(
        P5_11_01 == 1 | P5_31_01 == 1 ~ 1,
        P5_11_01 == 0 & P5_31_01 == 0 ~ 0
      ),
    homicidios_dolosos_expan = as.numeric(homicidios_dolosos)*as.numeric(FAC_PER),
    robo_vehiculo_expan = as.numeric(robo_vehiculo)*as.numeric(FAC_PER)
  ) %>%
  ungroup() %>%
  summarise(
    homicidios_dolosos_expan = sum(homicidios_dolosos_expan, na.rm = T),
    robo_vehiculo_expan = sum(robo_vehiculo_expan, na.rm = T)
  )

# Sentencias alto impacto

data_subset.df <- master_data.df %>%
  filter(sentenciado == 1) %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  mutate(
    aa_homicidios_dolosos =
      case_when(
        P5_11_12 == 1 ~ 1,
        P5_11_12 == 0 ~ 0
      ),
    aa_robo_vehiculo =
      case_when(
        P5_11_01 == 1 ~ 1,
        P5_11_01 == 0 ~ 0
      ),
    aa_extorsion = 
      case_when(
        P5_11_22 == 1  ~ 1,
        P5_11_22 == 0  ~ 0
      ),
    aa_portacion_armas = 
      case_when(
        P5_11_13 == 1  ~ 1,
        P5_11_13 == 0  ~ 0
      ),
    aa_posesion_drogas = 
      case_when(
        P5_11_08 == 1 | P5_11_09 == 1 ~ 1,
        P5_11_08 == 0 & P5_11_09 == 0 ~ 0
      ),
    aa_robo_autopartes = 
      case_when(
        P5_11_06 == 1 ~ 1,
        P5_11_06 == 0 ~ 0
      )
  ) %>%
  ungroup() %>%
  summarise(
    across(
      starts_with("aa_"),
      ~mean(.x, na.rm = T)
    )
  )

# Distribucion de competencias

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  mutate(
    fuero_federal =
      case_when(
        P5_13_1 == 1 & P5_13_2 != 1  ~ 1,
        P5_33_1 == 1 & P5_33_2 != 1  ~ 1,
        P5_13_1 == 0 ~ 0,
        P5_33_1 == 0 ~ 0,
      ),
    fuero_comun =
      case_when(
        P5_13_2 == 1 & P5_13_1 != 1 & P5_13_2 == 0 ~ 1,
        P5_33_2 == 1 & P5_33_1 != 1 & P5_33_2 == 0 ~ 1,
        P5_13_2 == 0 ~ 0,
        P5_33_2 == 0  ~ 0
      ),
    ambos_fueros = 
      case_when(
        P5_13_2 == 1 & P5_13_1 == 1 ~ 1,
        P5_33_2 == 1 & P5_33_1 == 1 ~ 1,
        P5_13_2 == 0 | P5_13_1 == 0 ~ 0,
        P5_33_2 == 0 | P5_33_1 == 0 ~ 0
      ),
    ninguno = 
      case_when(
        P5_13_2 == 0 & P5_13_1 == 0 ~ 1,
        P5_33_2 == 0 & P5_33_1 == 0 ~ 1,
        P5_13_2 == 1 | P5_13_1 == 1 ~ 0,
        P5_33_2 == 1 | P5_33_1 == 1 ~ 0
      )
  ) %>%
  ungroup() %>%
  summarise(
    fuero_federal = mean(fuero_federal, na.rm = T),
    fuero_comun = mean(fuero_comun, na.rm = T)
  )

# 

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  mutate(
    corporacion_fuero = case_when(Corporacion_grupos == "Ejército o Marina" ~ "Corporación Federal", 
                                  Corporacion_grupos == "Guardia Nacional" ~ "Corporación Federal",
                                  Corporacion_grupos == "Policía Federal" ~ "Corporación Federal",
                                  Corporacion_grupos == "Policía Federal Ministerial" ~ "Corporación Federal",
                                  Corporacion_grupos == "Policía Estatal Ministerial o Judicial" ~ "Corporación Local",
                                  Corporacion_grupos == "Operativo Conjunto" ~ "Operativo Conjunto",
                                  Corporacion_grupos == "Policía Estatal" ~ "Corporación Local", 
                                  Corporacion_grupos == "Policía Municipal" ~ "Corporación Local",
                                  Corporacion_grupos == "Otra" ~ "Otra", 
                                  T ~ NA_character_),
    proporcion = 
    case_when(fuero == "Sólo común" & corporacion_fuero == "Corporación Federal" ~ 1,
              fuero == "Sólo federal" & corporacion_fuero == "Corporación Federal" ~ 0,
              fuero == "Algunos delitos de fuero común y algunos de fuero federal" & corporacion_fuero == "Corporación Federal" ~ 0,
              T ~ NA_real_)
  ) %>%
  ungroup() %>%
  summarise(
    proporcion = mean(proporcion, na.rm = T)
  )

# Tipo de detencion

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  ungroup() %>%
  summarise(
    flagrancia = mean(flagrancia, na.rm = T),
    irregulares = mean(det_ninguna, na.rm = T),
    order_detencion = mean(orden_det, na.rm = T),
    inspeccion = mean(inspeccion, na.rm = T)
  )

# Tortura detencion

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  ungroup() %>%
  select(tortura_generalizada, flagrancia, det_ninguna, orden_det, inspeccion) %>%
  pivot_longer(cols = c(flagrancia, det_ninguna, orden_det, inspeccion), names_to = "category", values_to = "values") %>%
  filter(values == 1) %>%
  group_by(category) %>%
  summarise(
    tortura_generalizada = mean(tortura_generalizada, na.rm = T)
  )

# Interrogatorio o entrevista

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  ungroup() %>%
  mutate(
    interrogatorio_detencion =
      case_when(
        P3_16 == 1 ~ 1, 
        P3_16 == 2 ~ 0
      ),
    interrogatorio_MP =
      case_when(
        P4_3 == 1 ~ 1, 
        P4_3 == 2 ~ 0
      ),
    grabacion_MP = 
      case_when(
        P4_3A_4 == 1 | P4_3A_3 == 1 ~ 1,
        P4_3A_4 == 2  & P4_3A_3 == 2 ~ 0
      ),
    silencio_MP = 
      case_when(
        P4_3A_2 == 1 ~ 1,
        P4_3A_2 == 2 ~ 0
      ),    
    abogado_MP = 
      case_when(
        P4_3A_1 == 1 ~ 1,
        P4_3A_1 == 2 ~ 0
      ),
    enganio_MP = 
      case_when(
        P4_3A_7 == 1 | P4_3A_8 == 1 ~ 1,
        P4_3A_7 == 0 & P4_3A_8 == 0 ~ 0
      ),
    culpable_MP = 
      case_when(
        P4_3A_9 == 1 ~ 1,
        P4_3A_9 == 0 ~ 0
      )
  ) %>%
  summarise(
    interrogatorio_detencion = mean(interrogatorio_detencion, na.rm = T),
    interrogatorio_MP = mean(interrogatorio_MP, na.rm = T),
    grabacion_MP = mean(grabacion_MP, na.rm = T),
    silencio_MP = mean(silencio_MP, na.rm = T),
    abogado_MP = mean(abogado_MP, na.rm = T),
    enganio_MP = mean(enganio_MP, na.rm = T),
    culpable_MP = mean(culpable_MP, na.rm = T)
  )

# Terminación

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022) %>%
  mutate(
    juicio = 
      case_when(
        P5_6 == 1 ~ 1,
        P5_6 == 2 ~ 0
      ),
    procedimiento_abreviado_normal = 
      case_when(
        P5_6 == 2 & P5_7 == 2 ~ 1,
        P5_6 == 2 & P5_7 == 1 ~ 0,
        P5_6 == 1 ~ 0
      ),
    procedimiento_abreviado_violencia = 
      case_when(
        P5_6 == 2 & P5_7 == 1 ~ 1,
        P5_6 == 2 & P5_7 == 2 ~ 0,
        P5_6 == 1 ~ 0
      ),
    juicio_tiempo = 
      case_when(
        juicio == 1 & P5_10 < 5 ~ 1,
        juicio == 1 & (P5_10 == 5 | P5_10 == 6 | P5_10 == 7) ~ 0
      ),
    procedimiento_tiempo =
      case_when(
        (procedimiento_abreviado_normal == 1 | procedimiento_abreviado_violencia == 1) & P5_10 < 5 ~ 1,
        (procedimiento_abreviado_normal == 1 | procedimiento_abreviado_violencia == 1) & (P5_10 == 5 | P5_10 == 6 | P5_10 == 7) ~ 0
      )
  ) %>%
  ungroup() %>%
  summarise(
    juicio = mean(juicio, na.rm = T),
    procedimiento_abreviado_normal = mean(procedimiento_abreviado_normal, na.rm = T),
    procedimiento_abreviado_violencia = mean(procedimiento_abreviado_violencia, na.rm = T),
    juicio_tiempo = mean(juicio_tiempo, na.rm = T),
    procedimiento_tiempo = mean(procedimiento_tiempo, na.rm = T)
  )

#Penas

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2017 & Anio_arresto < 2022)  %>%
  mutate(
    P5_4_A = as.numeric(P5_4_A),
    P5_4_M = as.numeric(P5_4_M),
    P5_4_A = case_when(
      P5_4_A >= 97 ~ NA_real_,
      T ~ P5_4_A),
    P5_4_M = case_when(
      P5_4_M >= 97 ~ NA_real_,
      T ~ P5_4_M),
    P5_4_M = P5_4_M/12,
    tiempo_sentencia = P5_4_A+P5_4_M
  ) %>%
  mutate(
    counter = 1,
    tiempo_sentencia_cat = 
      case_when(
        tiempo_sentencia >= 0 & tiempo_sentencia < 4.99 ~ "De 0 a 5 años",
        tiempo_sentencia >= 4.99 & tiempo_sentencia < 9.99 ~ "De 5 a 10 años",
        tiempo_sentencia >= 9.99 & tiempo_sentencia < 14.99 ~ "De 10 a 15 años",
        tiempo_sentencia >= 14.99 & tiempo_sentencia < 19.99 ~ "De 15 a 20 años",
        tiempo_sentencia >= 19.99 & tiempo_sentencia < 24.99 ~ "De 20 a 25 años",
        tiempo_sentencia >= 24.99 & tiempo_sentencia < 29.99 ~ "De 25 a 30 años",
        tiempo_sentencia >= 29.99 ~ "Más de 30 años"
      ),
    n_obs = if_else(!is.na(tiempo_sentencia), sum(counter, na.rm = T), NA_real_)
  ) %>%
  ungroup() %>%
  group_by(tiempo_sentencia_cat) %>%
  summarize(
    value2plot = sum(counter, na.rm = T),
  ) %>% 
  drop_na() %>%
  mutate(
    n_obs = sum(value2plot),
    value2plot = value2plot/n_obs
  )
  
