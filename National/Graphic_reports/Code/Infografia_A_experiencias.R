data_subset.df <- master_data.df %>%
  filter(NSJP == 1) %>%
  filter(Anio_arresto >= as.numeric(2018)) %>% 
  mutate(
    `1a` = case_when(
      indicator_general == 1 ~ 1,
      indicator_general < 1  ~ 0
    ),
    `2a` = case_when(
      as.numeric(P5_26A) == 1 ~ 1,
      as.numeric(P5_26A) == 0 ~ 0,
      T ~ NA_real_
    ),
    `3a` = case_when(
      as.numeric(P5_26B) == 1 | as.numeric(P5_26B) == 2 ~ 1,
      as.numeric(P5_26B) == 3 | as.numeric(P5_26B) == 4 ~ 0,
      T ~ NA_real_
    ),
    `4a` = case_when(
      as.numeric(P3_1) == 1 | as.numeric(P3_1) == 2 ~ 0,
      as.numeric(P3_1) == 3 | as.numeric(P3_1) == 4 ~ 1,
      T ~ NA_real_
    ),
    `5.1a` = case_when(
          P3_14_4 == 1 ~ 1,
          P3_14_4 == 0 ~ 0
        ), 
    `5.2a` = case_when(
      P4_1_03 == 1 ~ 1,
      P4_1_03 == 2 ~ 0
    ),
    `5.3a` = case_when(
      P5_2_1 == 1 ~ 1,
      P5_2_1 == 2 ~ 0
    ),
    `6.1a` = 
      case_when(
        P3_14_5 == 1 ~ 1,
        P3_14_5 == 0 ~ 0
    ),
    `6.2a` = case_when(
      P4_1_04 == 1 ~ 1,
      P4_1_04 == 2 ~ 0
    ),
    `6.3a` = case_when(
      P5_2_4 == 1 ~ 1,
      P5_2_4 == 2 ~ 0
    ),
    `7.1a` = case_when(
      P4_1_05 == 1 ~ 1,
      P4_1_05 == 2 ~ 0
    ),
    `7.2a` = case_when(
      P5_2_5 == 1 ~ 1,
      P5_2_5 == 2 ~ 0
    ),
    `8.1a` = case_when(
      P5_24_1 == 1 | P5_24_1 == 2 | P5_44_1 == 1 | P5_44_1 == 2 ~ 1,
      P5_24_1 == 3 | P5_24_1 == 4 | P5_44_1 == 3 | P5_44_1 == 4 ~ 0,
    ),
    `8.2a` = case_when(
      P5_24_2 == 1 | P5_24_2 == 2 | P5_44_2 == 1 | P5_44_2 == 2 ~ 1,
      P5_24_2 == 3 | P5_24_2 == 4 | P5_44_2 == 3 | P5_44_2 == 4 ~ 0,
    ),
    `9.1a` = case_when(
      Tiempo_traslado %in% c("Hasta 30 minutos", 
                             "Más de 30 minutos hasta 1 hora",
                             "Más de 1 hora hasta 2 horas",
                             "Más de 2 horas hasta 4 horas") ~ 1,
      Tiempo_traslado %in% c("Más de 4 horas hasta 6 horas",
                             "Más de 6 horas hasta 24 horas",
                             "Más de 24 horas hasta 48 horas",
                             "Más de 48 horas hasta 72 horas",
                             "Más de 72 horas") ~ 0,
      T ~ NA_real_
    ),
    `9.2a` = case_when(
      Tiempo_traslado %in% c("Hasta 30 minutos", 
                             "Más de 30 minutos hasta 1 hora",
                             "Más de 1 hora hasta 2 horas",
                             "Más de 2 horas hasta 4 horas",
                             "Más de 24 horas hasta 48 horas",
                             "Más de 48 horas hasta 72 horas",
                             "Más de 72 horas") ~ 0,
      Tiempo_traslado %in% c("Más de 4 horas hasta 6 horas",
                             "Más de 6 horas hasta 24 horas"
                             ) ~ 1,
      T ~ NA_real_
    ),
    `10.1a` = case_when(
      Primer_lugar_traslado %in% "Agencia del Ministerio Público" ~ 1,
      Primer_lugar_traslado %in% c("Casa particular",
                                   "Centro de arraigo",
                                   "Centro de detención para migrantes",
                                   "Centro penitenciario",
                                   "Establecimiento comercial",
                                   "Hospital",
                                   "Instalación de la policía",
                                   "Juez de lo penal",
                                   "Oficina del gobierno",
                                   "Otra",
                                   "Terreno baldío",
                                   "Vehículo",
                                   "Zona militar") ~ 0
    ),
    `10.2a` = case_when(
      Primer_lugar_traslado %in% "Instalación de la policía" ~ 1,
      Primer_lugar_traslado %in% c("Casa particular",
                                   "Centro de arraigo",
                                   "Centro de detención para migrantes",
                                   "Centro penitenciario",
                                   "Establecimiento comercial",
                                   "Hospital",
                                   "Agencia del Ministerio Público",
                                   "Juez de lo penal",
                                   "Oficina del gobierno",
                                   "Otra",
                                   "Terreno baldío",
                                   "Vehículo",
                                   "Zona militar") ~ 0
    ),
    `11.1a` = case_when(
      (P5_16_2 == 2 | P5_16_2 == 3| P5_16_2 == 4) | (P5_36_2 == 2 | P5_36_2 == 3| P5_36_2 == 4) ~ 0,
      P5_16_2 == 1  | P5_36_2 == 1 ~ 1,
      T ~ NA_real_
    ),
    `11.2a` = case_when(
      P5_19_3 == 1 | P5_39_3 == 1 ~ 1,
      P5_19_3 == 2 | P5_39_3 == 2 ~ 0
    ),
    `11.6a` = case_when(
      as.numeric(P5_26) == 1 | as.numeric(P5_26) == 2 ~ 1,
      as.numeric(P5_26) == 3 | as.numeric(P5_26) == 4 ~ 0,
      T ~ NA_real_
    ),
    `11.4a` = case_when(
      P5_25 == 2 ~ 1,
      P5_25 == 1 ~ 0
    ),
    `11.5a` = case_when(
      (P5_17_2 == 1 | P5_17_2 == 2) |  (P5_37_2 == 1 | P5_37_2 == 2) ~ 1,
      (P5_17_2 == 3 | P5_17_2 == 4) |  (P5_37_2 == 3 | P5_37_2 == 4) ~ 0
    ),
    `11.3a` = case_when(
      (P5_16_5 == 1 | P5_16_5 == 3 | P5_16_5 == 2) | (P5_36_5 == 1 | P5_36_5 == 3 | P5_36_5 == 2) ~ 1,
      P5_16_5 == 4 | P5_36_5 == 4 ~ 0
    ),
    P5_10  = case_when(
      P5_10 > 7 ~ NA_real_,
      T ~ as.numeric(P5_10)
    ),
    `12.1a` =
      case_when(
        P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 ~ 1,
        P5_10 == 5  | P5_10 == 6 | P5_10 == 7 ~ 0
      ),
    `12.2a` =
      case_when(
        P5_10 == 5 ~ 1,
        P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 | P5_10 == 6 | P5_10 == 7 ~ 0
      ),
    `12.3a` =
      case_when(
        P5_10 == 6 ~ 1,
        P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 | P5_10 == 5 | P5_10 == 7 ~ 0
      ),
    `12.4a` =
      case_when(
        P5_10 == 7 ~ 1,
        P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 | P5_10 == 5 | P5_10 == 6 ~ 0
      ),
    `13.1a` = 
      case_when(
        proporcionalidad_uso_fuerza == 1 & Corporacion_grupos %in% "Policía Municipal" ~ 1,
        proporcionalidad_uso_fuerza == 0 & Corporacion_grupos %in% "Policía Municipal" ~ 0,
      ),
    `13.2a` = 
      case_when(
        proporcionalidad_uso_fuerza == 1 & Corporacion_grupos %in% "Policía Estatal" ~ 1,
        proporcionalidad_uso_fuerza == 0 & Corporacion_grupos %in% "Policía Estatal" ~ 0,
      ),
    `13.3a` =
      case_when(
        proporcionalidad_uso_fuerza == 1 & Corporacion_grupos %in% "Policía Estatal Ministerial o Judicial" ~ 1,
        proporcionalidad_uso_fuerza == 0 & Corporacion_grupos %in% "Policía Estatal Ministerial o Judicial" ~ 0,
      ),
    `13.4a` = 
      case_when(
        proporcionalidad_uso_fuerza == 1 & Corporacion_grupos %in% "Operativo Conjunto" ~ 1,
        proporcionalidad_uso_fuerza == 0 & Corporacion_grupos %in% "Operativo Conjunto" ~ 0,
      ),
    `13.5a` = 
      case_when(
        proporcionalidad_uso_fuerza == 1 & (Corporacion_grupos %in% "Policía Federal" | Corporacion_grupos %in% c("Guardia Nacional")) ~ 1,
        proporcionalidad_uso_fuerza == 0 & (Corporacion_grupos %in% "Policía Federal" | Corporacion_grupos %in% c("Guardia Nacional"))~ 0,
      ),
    `13.6a` = 
      case_when(
        proporcionalidad_uso_fuerza == 1 & (Corporacion_grupos %in% c("Policía Federal Ministerial")) ~ 1,
        proporcionalidad_uso_fuerza == 0 & (Corporacion_grupos %in% c("Policía Federal Ministerial")) ~ 0
      ),
    `13.7a` = 
      case_when(
        proporcionalidad_uso_fuerza == 1 & Corporacion_grupos %in% "Ejército o Marina" ~ 1,
        proporcionalidad_uso_fuerza == 0 & Corporacion_grupos %in% "Ejército o Marina" ~ 0,
      ),
    `14.1a` =
      case_when(
        tortura_generalizada == 1 & (tortura_tra_p == 1 | tortura_tra_f == 1) ~ 1,
        tortura_generalizada == 1 & tortura_tra_p == 0 & tortura_tra_f == 0 ~ 0,
        tortura_generalizada == 0 ~ 0
      ),
    `14.2a` = 
      case_when(
        tortura_generalizada == 1 & (tortura_mp_p == 1 | tortura_mp_f == 1) ~ 1,
        tortura_generalizada == 1 & tortura_mp_p == 0 & tortura_mp_f == 0 ~ 0,
        tortura_generalizada == 0  ~ 0
      ),
    RND = 
      if_else(
        as.numeric(years_since_RND_3) < 2 & as.numeric(years_since_RND_3) > 0, "Post RND",
        if_else(
          as.numeric(years_since_RND_3) > -2 & as.numeric(years_since_RND_3) < 0, "PRE RND", 
          NA_character_
        )
      ),
    tortura_pre_p =
      case_when(
        (tortura_tra_p == 1 | tortura_mp_p == 1) & RND == "PRE RND" ~ 1,
        tortura_tra_p == 0 & tortura_mp_p == 0 & RND == "PRE RND" ~ 0
      ),
    tortura_post_p =
      case_when(
        (tortura_tra_p == 1 | tortura_mp_p == 1) & RND == "Post RND" ~ 1,
        tortura_tra_p == 0 & tortura_mp_p == 0 & RND == "Post RND" ~ 0
      ),
    tortura_pre_f =
      case_when(
        (tortura_tra_f == 1 | tortura_mp_f == 1) & RND == "PRE RND" ~ 1,
        tortura_tra_f == 0 & tortura_mp_f == 0 & RND == "PRE RND" ~ 0
      ),
    tortura_post_f =
      case_when(
        (tortura_tra_f == 1 | tortura_mp_f == 1) & RND == "Post RND" ~ 1,
        tortura_tra_f == 0 & tortura_mp_f == 0 & RND == "Post RND" ~ 0
      ),
    `16.2a` = 
      case_when(
        sentenciado == 1 ~ 1,
        sentenciado == 0 ~ 0
      ),
    `16.1a` =
      case_when(
        procesado == 1 ~ 1,
        procesado == 0 ~ 0
      ),
    `17.1a` =
      case_when(
        tipo_prision_preventiva == "Prisión Preventiva Oficiosa" ~ 1,
        tipo_prision_preventiva == "Prisión Preventiva Justificada" | tipo_prision_preventiva == "Proceso en libertad" ~ 0
      ),
    `17.2a` = 
      case_when(
        tipo_prision_preventiva == "Prisión Preventiva Justificada" ~ 1,
        tipo_prision_preventiva == "Prisión Preventiva Oficiosa" | tipo_prision_preventiva == "Proceso en libertad" ~ 0
      )
    )  %>%
  select(Estado_arresto, `1a`, `2a`, `3a`, `4a`, `5.1a`, `5.2a`, `5.3a`, `6.1a`, `6.2a`, `6.3a`, 
         `7.1a`, `7.2a`, `8.1a`, `8.2a`, `9.1a`, `9.2a`, `10.1a`, `10.2a`, `11.1a`, `11.2a`, 
         `11.3a`, `11.4a`, `11.5a`, `11.6a`, `12.1a`, `12.2a`, `12.3a`, `12.4a`, `13.1a`, `13.2a`, 
         `13.3a`, `13.4a`, `13.5a`, `13.6a`, `13.7a`, `14.1a`, `14.2a`, `tortura_pre_p`, `tortura_post_p`, 
         `tortura_pre_f`, `tortura_post_f`,
         `16.1a`, `16.2a`, `17.1a`, `17.2a`)

National <- data_subset.df %>%
  ungroup() %>%
  summarise(
    across(
      !Estado_arresto,
      ~mean(.x, na.rm = T)
    )
  ) %>%
  mutate(
    `15.1a`  = (tortura_post_p-tortura_pre_p)/tortura_pre_p,
    `15.2a`  = (tortura_post_f-tortura_pre_f)/tortura_pre_f,
    `Estado` = "Promedio Nacional"
  ) %>%
  select(!starts_with("tortura"),
         Estado, `1a`, `2a`, `3a`, `4a`, `5.1a`, `5.2a`, `5.3a`, `6.1a`, `6.2a`, `6.3a`, 
         `7.1a`, `7.2a`, `8.1a`, `8.2a`, `9.1a`, `9.2a`, `10.1a`, `10.2a`, `11.1a`, `11.2a`, 
         `11.3a`, `11.4a`, `11.5a`, `11.6a`, `12.1a`, `12.2a`, `12.3a`, `12.4a`, `13.1a`, `13.2a`, 
         `13.3a`, `13.4a`, `13.5a`, `13.6a`, `13.7a`, `14.1a`, `14.2a`, `15.1a`, `15.2a`,
         `16.1a`, `16.2a`, `17.1a`, `17.2a`) %>%
  mutate(
    across(
      ends_with("a"),
      list(b = ~rank(-.x)),
      .names = "{sub('a$', 'b', .col)}"
    )
  ) %>%
  mutate(
    across(
      ends_with("b"),
      ~case_when(
        .x == 1 ~ NA_real_
      )
    )
  )


Estatal <- data_subset.df %>%
  drop_na(Estado_arresto) %>%
  ungroup() %>%
  group_by(Estado_arresto) %>%
  summarise(
    across(
      everything(),
      ~mean(.x, na.rm = T)
    )
  ) %>%
  mutate(
    `15.1a`  = (tortura_post_p-tortura_pre_p)/tortura_pre_p,
    `15.2a`  = (tortura_post_f-tortura_pre_f)/tortura_pre_f
  ) %>%
  select(!starts_with("tortura"),
         Estado = Estado_arresto, `1a`, `2a`, `3a`, `4a`, `5.1a`, `5.2a`, `5.3a`, `6.1a`, `6.2a`, `6.3a`, 
         `7.1a`, `7.2a`, `8.1a`, `8.2a`, `9.1a`, `9.2a`, `10.1a`, `10.2a`, `11.1a`, `11.2a`, 
         `11.3a`, `11.4a`, `11.5a`, `11.6a`, `12.1a`, `12.2a`, `12.3a`, `12.4a`, `13.1a`, `13.2a`, 
         `13.3a`, `13.4a`, `13.5a`, `13.6a`, `13.7a`, `14.1a`, `14.2a`, `15.1a`, `15.2a`,
         `16.1a`, `16.2a`, `17.1a`, `17.2a`) %>%
  mutate(
    across(
      ends_with("a"),
      list(b = ~rank(-.x)),
      .names = "{sub('a$', 'b', .col)}"
    ),
    `4b` = rank(`4a`),
    `9.2b` = rank(`9.2a`),
    `16.1b` = rank(`16.1a`),
    `15.1b` = rank(`15.1a`),
    `15.2b` = rank(`15.2b`),
    `14.1b` = rank(`14.1a`),
    `14.2b` = rank(`14.1b`)
  ) 

final_data_experiencias <- bind_rows(Estatal, National) %>%
  mutate(
    across(
      ends_with("a"),
      ~paste0(round(.x,2)*100, "%")
    ),
    `15.1a` = if_else(str_detect(`15.1a`, "-"),`15.1a`, paste0("+",`15.1a`)),
    `15.2a` = if_else(str_detect(`15.2a`, "-"),`15.2a`, paste0("+",`15.2a`))
  )

writexl::write_xlsx(x = final_data_experiencias, path = "Output/INF_A_EXPERIENCIAS.xlsx")

