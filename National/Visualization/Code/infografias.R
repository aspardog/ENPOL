## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Infografias proceso justo
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

# Infografia 1: Autoincriminacion

data_subset.df <- master_data.df %>%
  mutate(
    guardar_silencio_detencion = 
      case_when(
        P3_14_5 == 1 ~ 1,
        P3_14_5 == 0 ~ 0
      ),
    guardar_silencio_mp = 
      case_when(
        P4_1_04 == 1 ~ 1,
        P4_1_04 == 2 ~ 0
      ),
    guardar_silencio_interrogatorio = 
      case_when(
        P4_3A_2 == 1 ~ 1,
        P4_3A_2 == 2 ~ 0
      ),
    guardar_silencio_juez = 
      case_when(
        P5_2_4 == 1 ~ 1,
        P5_2_4 == 2 ~ 0
      ),
    explicacion_detencion =
      case_when(
        P3_14_4 == 1 ~ 1,
        P3_14_4 == 0 ~ 0
      ),
    explicacion_mp =
      case_when(
        P4_1_03 == 1 ~ 1,
        P4_1_03 == 2 ~ 0
      ),
    declaracion_mp =
      case_when(
        P4_6_2 == 1 ~ 1,
        P4_6_2 == 2 ~ 0
      ),
    explicacion_juez =
      case_when(
        P5_2_1 == 1 ~ 1,
        P5_2_1 == 2 ~ 0
      ),
    explicacion_hechos =
      case_when(
        P5_22_01 == 1 | P5_42_01 == 1~ 1,
        P5_22_01 == 2 | P5_42_01 == 2~ 0
      ),
    explicacion_proceso =
      case_when(
        P5_22_02 == 1 | P5_42_02 == 1 ~ 1,
        P5_22_02 == 2 | P5_42_02 == 2 ~ 0
      ),
    claridad_mp =
      case_when(
        P5_17_3 == 1 | P5_17_3 == 2 | P5_37_3 == 1 | P5_37_3 == 2 ~ 1,
        P5_17_3 == 3 | P5_17_3 == 4 | P5_37_3 == 3 | P5_37_3 == 4 ~ 0
      ),
    claridad_juez =
      case_when(
        P5_17_2 == 1 | P5_17_2 == 2 | P5_37_2 == 1 | P5_37_2 == 2 ~ 1,
        P5_17_2 == 3 | P5_17_2 == 4 | P5_37_2 == 3 | P5_37_2 == 4 ~ 0
      ),
    claridad_defensor =
      case_when(
        P5_17_1 == 1 | P5_17_1 == 2 | P5_37_1 == 1 | P5_37_1 == 2 ~ 1,
        P5_17_1 == 3 | P5_17_1 == 4 | P5_37_1 == 3 | P5_37_1 == 4 ~ 0
      ),
    claridad_defendido =
      case_when(
        P5_17_4 == 1 | P5_17_4 == 2 | P5_37_4 == 1 | P5_37_4 == 2 ~ 1,
        P5_17_4 == 3 | P5_17_4 == 4 | P5_37_4 == 3 | P5_37_4 == 4 ~ 0
      ),
    podia_escuchar = 
      case_when(
        P5_20_4 == 1 | P5_40_4 == 1 ~ 1,
        P5_20_4 == 2 | P5_40_4 == 2 ~ 0
      ),
    Tiempo_traslado =
      case_when(
        Tiempo_traslado %in% c("Hasta 30 minutos", 
                               "Más de 30 minutos hasta 1 hora",
                               "Más de 1 hora hasta 2 horas",
                               "Más de 2 horas hasta 4 horas") ~ "Menos de 4 horas",
        Tiempo_traslado %in% c("Más de 4 horas hasta 6 horas",
                               "Más de 6 horas hasta 24 horas") ~ "Más de 4 horas hasta 24 horas",
        Tiempo_traslado %in% c("Más de 24 horas hasta 48 horas") ~ "Más de 24 horas hasta 48 horas",
        Tiempo_traslado %in% c("Más de 48 horas hasta 72 horas",
                               "Más de 72 horas") ~ "Más de 48 horas",
        T ~ NA_character_
      ),
    Tiempo_traslado = 
      case_when(
        Tiempo_traslado %in% c("Menos de 4 horas") ~ "Menos de 4 horas",
        Tiempo_traslado %in% c("Más de 4 horas hasta 24 horas", 
                               "Más de 24 horas hasta 48 horas",
                               "Más de 48 horas") ~ "Más de 4 horas"
      ),
    Tiempo_traslado = 
      case_when(
        Tiempo_traslado %in% "Menos de 4 horas" ~ 1,
        Tiempo_traslado %in% "Más de 4 horas" ~ 0
      ),
    P5_34_A = replace(P5_34_A, P5_34_A %in% c( "98", "99"), NA),
    P5_34_M = replace(P5_34_M, P5_34_M %in% c( "98", "99"), NA),
    P5_34_A = replace(P5_34_A, P5_34_A %in% c( "96"), 0),
    P5_34_M = replace(P5_34_M, P5_34_M %in% c( "96"), 0),
    P5_10 = replace(P5_10, P5_10 %in% c("8", "9"), NA),
    procesados_meses_pp = ((as.numeric(P5_34_A)*12) + as.numeric(P5_34_M)),
    mas2anios_prisionpreventiva = 
      case_when(as.numeric(P5_10) == 7 ~ 1,
                as.numeric(P5_10) == 1 | as.numeric(P5_10) == 2 | 
                  as.numeric(P5_10) == 3 | as.numeric(P5_10) == 4 |
                  as.numeric(P5_10) == 5 |as.numeric(P5_10) == 6 ~ 0,
                T ~ NA_real_),
    menos2anios_prisionpreventiva = 1-mas2anios_prisionpreventiva,
    tiempo_MP = 
      case_when(
        P4_19 == 1 | P4_19 == 2 ~ 1,
        P4_19 == 3 | P4_19 == 4 | P4_19 == 5 ~ 0,
        T ~ NA_real_
      )
  ) %>%
  ungroup() %>%
  summarise(
    silencio_detencion = mean(guardar_silencio_detencion, na.rm = T),
    silencio_mp        = mean(guardar_silencio_mp, na.rm = T),
    silencio_interrogatorio = mean(guardar_silencio_interrogatorio, na.rm = T),
    silencio_juez      = mean(guardar_silencio_juez, na.rm = T),
    explicacion_detencion = mean(explicacion_detencion, na.rm = T),
    explicacion_mp = mean(explicacion_mp, na.rm = T),
    explicacion_juez = mean(explicacion_juez, na.rm = T),
    declaracion_mp = mean(declaracion_mp, na.rm = T),
    explicacion_hechos = mean(explicacion_hechos, na.rm = T),
    explicacion_proceso = mean(explicacion_proceso, na.rm = T),
    claridad_mp        = mean(claridad_mp, na.rm = T),
    claridad_juez      = mean(claridad_juez, na.rm = T),
    claridad_defensor  = mean(claridad_defensor, na.rm = T),
    claridad_defendido = mean(claridad_defendido, na.rm = T),
    podia_escuchar = mean(podia_escuchar, na.rm = T),
    Tiempo_traslado_menor_4h = mean(Tiempo_traslado, na.rm = T),
    menos2anios_prisionpreventiva = mean(menos2anios_prisionpreventiva, na.rm = T),
    tiempo_MP = mean(tiempo_MP, na.rm = T)
  ) %>%
  pivot_longer(cols = c(silencio_detencion, 
                        silencio_mp, 
                        silencio_juez, 
                        silencio_interrogatorio, 
                        explicacion_detencion, 
                        explicacion_mp, 
                        explicacion_juez, 
                        declaracion_mp, 
                        explicacion_hechos, 
                        explicacion_proceso,
                        claridad_mp,
                        claridad_juez,
                        claridad_defensor,
                        claridad_defendido,
                        podia_escuchar,
                        Tiempo_traslado_menor_4h,
                        menos2anios_prisionpreventiva,
                        tiempo_MP), 
               names_to = "category", values_to = "value2plot") %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%")
         ) %>%
  select(!starts_with("nobs_"))

writexl::write_xlsx(x = data_subset.df, 
                    path = paste0(getwd(),
                                  "/Infografias/",
                                  "Derecho a la información, a la no autoincriminacion y a una justicia pronta.xlsx")
                    )

# Infografia 2: Defensa

data_subset.df_1 <- master_data.df %>%
  mutate(
    asesoria_abogado =
      case_when(
        P4_1_05 == 1 ~ 1,
        P4_1_05 == 2 ~ 0
      ),
    hablo_abogado =
      case_when(
        P4_1_06 == 1 ~ 1,
        P4_1_06 == 2 ~ 0
      ),
    abogado_publico =
      case_when(
        abogado_publico == 1 ~ 1,
        abogado_publico == 0 ~ 0
      ),
    ofrecio_pruebas =
      case_when(
        P4_1_07 == 1 ~ 1,
        P4_1_07 == 2 ~ 0
      ),
    presente_interrogatorio =
      case_when(
        P4_3A_1 == 1 ~ 1,
        P4_3A_1 == 2 ~ 0
      ),
    presente_señalamiento =
      case_when(
        P4_14_1 == 1 ~ 1,
        P4_14_1 == 2 ~ 0
      ),
    presente_declaracion =
      case_when(
        P4_6_3 == 1 ~ 1,
        P4_6_3 == 2 ~ 0
      ),
    satisfecho_MP =
      case_when(
        P5_24_1 == 1 | P5_24_1 == 2 ~ 1,
        P5_24_1 == 3 | P5_24_1 == 4 ~ 0
      ),
    satisfecho_juicio =
      case_when(
        P5_24_2 == 1 | P5_24_2 == 2 ~ 1,
        P5_24_2 == 3 | P5_24_2 == 4 ~ 0
      ),
    abogado_antes =
      case_when(
        P5_1 == 1 ~ 1,
        P5_1 == 2 ~ 0
      ),
    abogado_inicial = 
      case_when(
        P5_2_5 == 1 ~ 1,
        P5_2_5 == 2 ~ 0
      ),
    lugar_distinto =
      case_when(
        P5_22_03 == 1 ~ 1,
        P5_22_03 == 2 ~ 0
      ),
    testigos = 
      case_when(
        P5_22_04 == 1 ~ 1,
        P5_22_04 == 2 ~ 0
      ),
    elementos =
      case_when(
        P5_22_05 == 1 ~ 1,
        P5_22_05 == 2 ~ 0
      ),
    apelacion = 
      case_when(
        P5_22_09 == 1 ~ 1,
        P5_22_09 == 2 ~ 0
      ),
    amparo = 
      case_when(
        P5_22_10 == 1 ~ 1,
        P5_22_10 == 2 ~ 0
      ),
    counter = 1,
  ) %>%
  ungroup() %>%
  summarise(
    asesoria_abogado = mean(asesoria_abogado, na.rm = T),
    hablo_abogado = mean(hablo_abogado, na.rm = T),
    abogado_publico = mean(abogado_publico, na.rm = T),
    ofrecio_pruebas = mean(ofrecio_pruebas, na.rm = T),
    presente_interrogatorio = mean(presente_interrogatorio, na.rm = T),
    presente_señalamiento = mean(presente_señalamiento, na.rm = T),
    presente_declaracion = mean(presente_declaracion, na.rm = T),
    satisfaccion_MP = mean(satisfecho_MP, na.rm = T),
    satisfecho_juicio = mean(satisfecho_juicio, na.rm = T),
    abogado_antes = mean(abogado_antes, na.rm = T),
    abogado_inicial = mean(abogado_inicial, na.rm = T),
    lugar_distinto = mean(lugar_distinto, na.rm = T),
    testigos = mean(testigos, na.rm = T),
    elementos = mean(elementos, na.rm = T),
    apelacion = mean(apelacion, na.rm = T),
    amparo = mean(amparo, na.rm = T)
  ) %>%
  pivot_longer(cols = c(asesoria_abogado, 
                        hablo_abogado, 
                        abogado_publico, 
                        ofrecio_pruebas,
                        presente_interrogatorio,
                        presente_señalamiento,
                        presente_declaracion,
                        satisfaccion_MP,
                        satisfecho_juicio,
                        abogado_antes,
                        abogado_inicial,
                        lugar_distinto,
                        testigos,
                        elementos,
                        apelacion,
                        amparo), 
               names_to = "category", values_to = "value2plot") %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%")
         ) 

data_subset.df_2 <- master_data.df %>%
  mutate(
    asesoria_abogado =
      case_when(
        P4_1_05 == 1 ~ 1,
        P4_1_05 == 2 ~ 0
      ),
    hablo_abogado =
      case_when(
        P4_1_06 == 1 ~ 1,
        P4_1_06 == 2 ~ 0
      ),
    abogado_publico =
      case_when(
        abogado_publico == 1 ~ 1,
        abogado_publico == 0 ~ 0
      ),
    ofrecio_pruebas =
      case_when(
        P4_1_07 == 1 ~ 1,
        P4_1_07 == 2 ~ 0
      ),
    presente_interrogatorio =
      case_when(
        P4_3A_1 == 1 ~ 1,
        P4_3A_1 == 2 ~ 0
      ),
    presente_señalamiento =
      case_when(
        P4_14_1 == 1 ~ 1,
        P4_14_1 == 2 ~ 0
      ),
    presente_declaracion =
      case_when(
        P4_6_3 == 1 ~ 1,
        P4_6_3 == 2 ~ 0
      ),
    satisfecho_MP =
      case_when(
        P5_24_1 == 1 | P5_24_1 == 2 ~ 1,
        P5_24_1 == 3 | P5_24_1 == 4 ~ 0
      ),
    satisfecho_juicio =
      case_when(
        P5_24_2 == 1 | P5_24_2 == 2 ~ 1,
        P5_24_2 == 3 | P5_24_2 == 4 ~ 0
      ),
    abogado_antes =
      case_when(
        P5_1 == 1 ~ 1,
        P5_1 == 2 ~ 0
      ),
    abogado_inicial = 
      case_when(
        P5_2_5 == 1 ~ 1,
        P5_2_5 == 2 ~ 0
      ),
    lugar_distinto =
      case_when(
        P5_22_03 == 1 ~ 1,
        P5_22_03 == 2 ~ 0
      ),
    testigos = 
      case_when(
        P5_22_04 == 1 ~ 1,
        P5_22_04 == 2 ~ 0
      ),
    elementos =
      case_when(
        P5_22_05 == 1 ~ 1,
        P5_22_05 == 2 ~ 0
      ),
    apelacion = 
      case_when(
        P5_22_09 == 1 ~ 1,
        P5_22_09 == 2 ~ 0
      ),
    amparo = 
      case_when(
        P5_22_10 == 1 ~ 1,
        P5_22_10 == 2 ~ 0
      ),
    counter = 1
  ) %>%
  group_by(abogado_publico) %>%
  summarise(
    asesoria_abogado = mean(asesoria_abogado, na.rm = T),
    hablo_abogado = mean(hablo_abogado, na.rm = T),
    abogado_publico = mean(abogado_publico, na.rm = T),
    ofrecio_pruebas = mean(ofrecio_pruebas, na.rm = T),
    presente_interrogatorio = mean(presente_interrogatorio, na.rm = T),
    presente_señalamiento = mean(presente_señalamiento, na.rm = T),
    presente_declaracion = mean(presente_declaracion, na.rm = T),
    satisfaccion_MP = mean(satisfecho_MP, na.rm = T),
    satisfecho_juicio = mean(satisfecho_juicio, na.rm = T),
    abogado_antes = mean(abogado_antes, na.rm = T),
    abogado_inicial = mean(abogado_inicial, na.rm = T),
    lugar_distinto = mean(lugar_distinto, na.rm = T),
    testigos = mean(testigos, na.rm = T),
    elementos = mean(elementos, na.rm = T),
    apelacion = mean(apelacion, na.rm = T),
    amparo = mean(amparo, na.rm = T)
  ) %>%
  #filter(!is.nan(abogado_publico)) %>%
  mutate(
    abogado_publico =
      case_when(
        abogado_publico == 1 ~ "abogado publico",
        abogado_publico == 0 ~ "abogado privado"
      )
  ) %>%
  select(
    abogado_publico, satisfaccion_MP, satisfecho_juicio
  )

data_subset.df_3 <- master_data.df %>%
  mutate(
    asesoria_abogado =
      case_when(
        P4_1_05 == 1 ~ 1,
        P4_1_05 == 2 ~ 0
      ),
    abogado_antes =
      case_when(
        P5_1 == 1 ~ 1,
        P5_1 == 2 ~ 0
      ),
    abogado_inicial = 
      case_when(
        P5_2_5 == 1 ~ 1,
        P5_2_5 == 2 ~ 0
      ),
    abogado_publico =
      case_when(
        abogado_publico == 1 ~ 1,
        abogado_publico == 0 ~ 0
      )
  ) %>%
  filter(abogado_inicial == 1) %>%
  mutate(counter = 1) %>%
  group_by(abogado_publico) %>%
  summarise(proporcion_abogado_antes = sum(counter, na.rm = T)) %>%
  drop_na() %>%
  mutate(
    final_value = proporcion_abogado_antes/sum(proporcion_abogado_antes)
  )

data_subset.df_4 <- master_data.df %>%
  mutate(
    asesoria_abogado =
      case_when(
        P4_1_05 == 1 ~ 1,
        P4_1_05 == 2 ~ 0
      ),
    abogado_antes =
      case_when(
        P5_1 == 1 ~ 1,
        P5_1 == 2 ~ 0
      ),
    abogado_inicial = 
      case_when(
        P5_2_5 == 1 ~ 1,
        P5_2_5 == 2 ~ 0
      ),
    abogado_publico =
      case_when(
        abogado_publico == 1 ~ 1,
        abogado_publico == 0 ~ 0
      )
  ) %>%
  filter(abogado_antes == 1) %>%
  mutate(counter = 1) %>%
  group_by(abogado_publico) %>%
  summarise(proporcion_abogado_antes = sum(counter, na.rm = T)) %>%
  drop_na() %>%
  mutate(
    final_value = proporcion_abogado_antes/sum(proporcion_abogado_antes)
  )

data_subset.df_5 <- master_data.df %>%
  mutate(
    asesoria_abogado =
      case_when(
        P4_1_05 == 1 ~ 1,
        P4_1_05 == 2 ~ 0
      ),
    abogado_antes =
      case_when(
        P5_1 == 1 ~ 1,
        P5_1 == 2 ~ 0
      ),
    abogado_inicial = 
      case_when(
        P5_2_5 == 1 ~ 1,
        P5_2_5 == 2 ~ 0
      ),
    abogado_publico =
      case_when(
        abogado_publico == 1 ~ 1,
        abogado_publico == 0 ~ 0
      )
  ) %>%
  filter(asesoria_abogado == 1) %>%
  mutate(counter = 1) %>%
  group_by(abogado_publico) %>%
  summarise(proporcion_abogado_asesoria = sum(counter, na.rm = T)) %>%
  drop_na() %>%
  mutate(
    final_value = proporcion_abogado_asesoria/sum(proporcion_abogado_asesoria)
  )

defensa_oportuna <- list(
  data_subset.df_1,
  data_subset.df_2,
  data_subset.df_3,
  data_subset.df_4
)

openxlsx::write.xlsx(x = defensa_oportuna, 
                     file = paste0(getwd(),
                                   "/Infografias/",
                                   "Derecho a una defensa oportua y adecuada.xlsx")
                     )

# Infografia 3: Justicia

data_subset.df_1 <- master_data.df %>%
  filter(sentenciado == 1) %>%
  mutate(
    contacto_juez =
      case_when(
        P5_2_1 == 1 | P5_2_1 == 2 ~ 1,
        P5_2_1 == 4 ~ 0
      ),
    video =
      case_when(
        P5_19_3 == 1 ~ 1,
        P5_19_3 == 2 ~ 0
      ),
    publico =
      case_when(
        P5_16_5 == 1 | P5_16_5 == 2 | P5_16_5 == 3 ~ 1,
        P5_16_5 == 4 ~ 0
      ),
    procedimiento =
      case_when(
        as.numeric(P5_6) == 1 ~ "Juicio",
        as.numeric(P5_6) == 2 ~ "Procedimiento abreviado",
        T ~ NA_character_
      ),
    culpable_antes =
      case_when(
        P5_25 == 2 ~ 1,
        P5_25 == 1 ~ 0
      ),
    juez_diferente =
      case_when(
        P5_14 == 1 & procedimiento != "Procedimiento abreviado" ~ 1,
        P5_14 == 2 & procedimiento != "Procedimiento abreviado" ~ 0,
        T ~ NA_real_
      ),
    juez_presente =
      case_when(
        P5_16_2 == 1  ~ 1,
        P5_16_2 == 2 | P5_16_2 == 3 | P5_16_2 == 4 ~ 0
      ),
    juez_control =
      case_when(
        P5_18 == 2 ~ 1,
        P5_18 == 1 | P5_18 == 3 | P5_18 == 4 | P5_18 == 5 ~ 0
      ),
    vidrio = 
      case_when(
        P5_20_1 == 1 ~ 1,
        P5_20_1 == 2 ~ 0
      ),
    esposado = case_when(
      P5_20_2 == 1 ~ 1,
      P5_20_2 == 2 ~ 0
    ),
    uniforme = case_when(
      P5_20_3 == 1 ~ 1,
      P5_20_3 == 2 ~ 0
    ),
    escuchado = case_when(
      P5_26 == 1 | P5_26 == 2 ~ 1,
      P5_26 == 3 | P5_26 == 4 ~ 0
    ),
    sentencia_justa = 
      case_when(
        as.numeric(P5_26B) == 1 | as.numeric(P5_26B) == 2 ~ 1,
        as.numeric(P5_26B) == 3 | as.numeric(P5_26B) == 4 ~ 0,
        T ~ NA_real_
      ),
    counter = 1,
  ) %>%
  ungroup() %>%
  summarise(
    contacto_juez = mean(contacto_juez, na.rm = T),
    video = mean(video, na.rm = T),
    publico = mean(publico, na.rm = T),
    culpable_antes = mean(culpable_antes, na.rm = T),
    juez_diferente = mean(juez_diferente, na.rm = T),
    juez_presente = mean(juez_presente, na.rm = T),
    juez_control = mean(juez_control, na.rm = T),
    vidrio = mean(vidrio, na.rm = T),
    esposado = mean(esposado, na.rm = T),
    uniforme = mean(uniforme, na.rm = T),
    escuchado = mean(escuchado, na.rm = T),
    sentencia = mean(sentencia_justa, na.rm = T)
  ) %>%
  pivot_longer(cols = c(contacto_juez,
                        video,
                        publico,
                        culpable_antes,
                        juez_diferente,
                        juez_presente,
                        juez_control,
                        vidrio,
                        esposado,
                        uniforme,
                        escuchado,
                        sentencia), 
               names_to = "category", values_to = "value2plot") %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%")
  ) 

justicia <- list(
  data_subset.df_1
  )

openxlsx::write.xlsx(x = justicia, 
                     file = paste0(getwd(),
                                   "/Infografias/",
                                   "Derecho a tribunal.xlsx")
)
