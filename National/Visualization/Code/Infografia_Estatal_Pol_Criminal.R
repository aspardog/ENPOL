load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

snsp <- read.csv(paste0(path2SP,"/National/Exploration/Input/IDEFC_NM_abr24.csv"),check.names = F)

snsp_2<- snsp %>%
  mutate(Agosto = case_when(Anio==2021 ~ 0,     T ~ Agosto),
         Septiembre = case_when(Anio==2021 ~ 0, T ~ Septiembre),
         Octubre = case_when(Anio==2021 ~ 0,    T ~ Octubre),
         Noviembre = case_when(Anio==2021 ~ 0,  T ~ Noviembre),
         Diciembre = case_when(Anio==2021 ~ 0,  T ~ Diciembre)) %>%
  filter(Anio>=2018 & Anio<= 2021) %>%
  mutate(Incidencia = Enero+Febrero+Marzo+Abril+Mayo+Junio+Julio+Agosto+Septiembre+Octubre+Noviembre+Diciembre) %>%
  mutate(Delito=case_when(`Subtipo de delito`== "Homicidio doloso" ~ "Homicidio doloso, SESNSP",
                          `Subtipo de delito`== "Robo de veh\xedculo automotor" ~ "Robo de vehículo, SESNSP",
                          T ~ "Otro"),
         Anio = as.character(Anio),
         Entidad = case_when(Entidad == "Ciudad de M\xe9xico" ~ "Distrito Federal",
                             Entidad == "M\xe9xico" ~ "México",
                             Entidad == "Michoac\xe1n de Ocampo" ~ "Michoacán de Ocampo",
                             Entidad == "Nuevo Le\xf3n" ~ "Nuevo León",
                             Entidad == "Quer\xe9taro" ~ "Querétaro",
                             Entidad == "San Luis Potos\xed" ~ "San Luis Potosí",
                             Entidad == "Yucat\xe1n" ~ "Yucatán",
                             T ~ Entidad)) %>% 
  group_by(Delito, Entidad) %>%
  summarize(Incidencia = sum(Incidencia),
            )%>% 
  filter(Delito != "Otro") %>% 
  ungroup() %>%
  pivot_wider(id_cols = Entidad,names_from = Delito, values_from = Incidencia)

master_data.df <- Main_database %>%
  left_join(snsp_2, by = c("Estado_arresto" = "Entidad"))
  

data_subset.df <- master_data.df %>%
  filter(Anio_arresto >= 2018, NSJP==1) %>%
  mutate(FAC_PER = as.numeric(FAC_PER)) %>%
  mutate(corporacion_fuero = case_when(Corporacion_grupos == "Ejército o Marina" ~ "Corporación Federal", 
                                       Corporacion_grupos == "Guardia Nacional" ~ "Corporación Federal",
                                       Corporacion_grupos == "Policía Federal" ~ "Corporación Federal",
                                       Corporacion_grupos == "Policía Federal Ministerial" ~ "Corporación Federal",
                                       Corporacion_grupos == "Policía Estatal Ministerial o Judicial" ~ "Corporación Local",
                                       Corporacion_grupos == "Operativo Conjunto" ~ "Operativo Conjunto",
                                       Corporacion_grupos == "Policía Estatal" ~ "Corporación Local", 
                                       Corporacion_grupos == "Policía Municipal" ~ "Corporación Local",
                                       Corporacion_grupos == "Otra" ~ "Otra", 
                                       T ~ NA_character_),
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
         ) %>% mutate(
    `1a` = case_when(tipo_prision_preventiva == "Prision Preventiva Oficiosa" ~ 1,
                     tipo_prision_preventiva == "Prisión Preventiva Justificada" ~ 1,
                     tipo_prision_preventiva == "Proceso en libertad" ~ 0,
                     T ~ NA_real_),
    `2a` = case_when(orden_det == 0 ~ 0,
                     orden_det == 1 ~ 1,
                     T ~ NA_real_),
    `3a` = case_when(P5_10 == "1" ~ 1,
                     P5_10 == "2" ~ 1,
                     P5_10 == "3" ~ 1,
                     P5_10 == "4" ~ 1,
                     P5_10 == "5" ~ 0,
                     P5_10 == "6" ~ 0,
                     P5_10 == "7" ~ 0,
                     T ~ NA_real_),
    `4a` = case_when(P9_1 == "2" ~ 0,
                     P9_1 == "1" ~ 1,
                     T ~ NA_real_),
    `5.1a` = case_when(P5_11_02 == 1  ~ 1,
                       P5_11_04 == 1  ~ 1,
                       P5_11_05 == 1  ~ 1,
                       P5_11_06 == 1  ~ 1,
                       P5_11_22 == 1  ~ 1,
                       P5_11_02 == 0 & P5_11_04 == 0 & P5_11_05 == 0 & P5_11_06 == 0 & P5_11_22 == 0  ~ 0,
                     T ~ NA_real_),
    `6.1a` = `Homicidio doloso, SESNSP`,
    `6.3a` = `Robo de vehículo, SESNSP`,
    `6.2a` = case_when(P5_11_12 == 1 | P5_31_12 == 1 ~ 1,
                       P5_11_12 == 0 ~ 0,
                       P5_31_12 == 0 ~ 0,
                       T ~ NA_real_)*FAC_PER,
    `6.4a` = case_when(P5_11_01 == 1 | P5_31_01 == 1 ~ 1,
                       P5_11_01 == 0 ~ 0,
                       P5_31_01 == 0 ~ 0,
                       T ~ NA_real_)*FAC_PER,
    `7.1a` = case_when(P5_11_12 == 1 ~ 1,
                       P5_11_12 == 0 ~ 0,
                       T ~ NA_real_),
    `7.2a` = case_when(P5_11_17 == 1 ~ 1,
                       P5_11_17 == 0 ~ 0,
                       T ~ NA_real_),
    `7.3a` = case_when(P5_11_22 == 1 ~ 1,
                       P5_11_22 == 0 ~ 0,
                       T ~ NA_real_),
    `7.4a` = case_when(P5_11_13 == 1 ~ 1,
                       P5_11_13 == 0 ~ 0,
                       T ~ NA_real_),
    `7.5a` = case_when(P5_11_08 == 1 ~ 1,
                       P5_11_09 == 1 ~ 1,
                       P5_11_08 == 0 & P5_11_09 == 0 ~ 0, 
                       T ~ NA_real_),
    `7.6a` = case_when(P5_11_06 == 1 ~ 1,
                       P5_11_06 == 0 ~ 0,
                       T ~ NA_real_),
    `7.7a` = case_when(P5_11_01 == 1 ~ 1,
                       P5_11_01 == 0 ~ 0,
                       T ~ NA_real_),
    `8.1a` = case_when(fuero == "Sólo federal" ~ 0,
                       fuero == "Sólo común" ~ 1,
                     T ~ NA_real_),
    `8.2a` = case_when(fuero == "Sólo federal" ~ 1,
                       fuero == "Sólo común" ~ 0,
                     T ~ NA_real_),
    `9.1a` = case_when(fuero == "Sólo federal" & corporacion_fuero == "Corporación Federal" ~ 1,
                       fuero == "Sólo federal" & corporacion_fuero == "Corporación Local" ~ 0,
                       fuero == "Sólo federal" & corporacion_fuero == "Operativo Conjunto" ~ 0,
                       fuero == "Sólo federal" & corporacion_fuero == "Otra" ~ 0,
                     T ~ NA_real_),
    `9.2a` = case_when(fuero == "Sólo común" & corporacion_fuero == "Corporación Federal" ~ 0,
                       fuero == "Sólo común" & corporacion_fuero == "Corporación Local" ~ 1,
                       fuero == "Sólo común" & corporacion_fuero == "Operativo Conjunto" ~ 0,
                       fuero == "Sólo común" & corporacion_fuero == "Otra" ~ 0,
                       T ~ NA_real_),
    `9.3a` = case_when(fuero == "Sólo común" & corporacion_fuero == "Corporación Federal" ~ 1,
                       fuero == "Sólo federal" & corporacion_fuero == "Corporación Federal" ~ 0,
                       fuero == "Algunos delitos de fuero común y algunos de fuero federal" & corporacion_fuero == "Corporación Federal" ~ 0,
                       T ~ NA_real_),
    `10.1a` = case_when(flagrancia == 1 ~ 1,
                        flagrancia == 0 ~ 0,
                      T ~ NA_real_),
    `10.2a` = case_when(det_ninguna == 1 ~ 1,
                        det_ninguna == 0 ~ 0,
                        T ~ NA_real_),
    `10.3a` = case_when(orden_det == 1 ~ 1,
                        orden_det == 0 ~ 0,
                        T ~ NA_real_),
    `10.4a` = case_when(inspeccion == 1 ~ 1,
                        inspeccion == 0 ~ 0,
                        T ~ NA_real_),
    `11.1a` = case_when(P3_16 == "1" ~ 1,
                        P3_16 == "2" ~ 0,
                      T ~ NA_real_),
    `11.2a` = case_when(P4_3 == "1" ~ 1,
                        P4_3 == "2" ~ 0,
                        T ~ NA_real_),
    `12.1a` = case_when(P5_6 == "1" ~ 1, 
                        P5_6 == "2" & P5_7 == "1" ~ 0, 
                        P5_6 == "2" & P5_7 == "2" ~ 0, 
                        T ~ NA_real_),
    `12.2a` = case_when(P5_6 == "1" ~ 0, 
                        P5_6 == "2" & P5_7 == "1" ~ 1, 
                        P5_6 == "2" & P5_7 == "2" ~ 0, 
                        T ~ NA_real_),
    `12.3a` = case_when(P5_6 == "1" ~ 0, 
                        P5_6 == "2" & P5_7 == "1" ~ 0, 
                        P5_6 == "2" & P5_7 == "2" ~ 1, 
                        T ~ NA_real_),
    `13.1a` = case_when(tiempo_sentencia >= 0 & tiempo_sentencia < 4.99 ~ 1,
                        tiempo_sentencia >= 4.99 & tiempo_sentencia < 9.99 ~ 0,
                        tiempo_sentencia >= 9.99 & tiempo_sentencia < 14.99 ~ 0,
                        tiempo_sentencia >= 14.99 & tiempo_sentencia < 19.99 ~ 0,
                        tiempo_sentencia >= 20  ~ 0,
                      T ~ NA_real_),
    `13.2a` = case_when(tiempo_sentencia >= 0 & tiempo_sentencia < 4.99 ~ 0,
                        tiempo_sentencia >= 4.99 & tiempo_sentencia < 9.99 ~ 1,
                        tiempo_sentencia >= 9.99 & tiempo_sentencia < 14.99 ~ 0,
                        tiempo_sentencia >= 14.99 & tiempo_sentencia < 19.99 ~ 0,
                        tiempo_sentencia >= 20  ~ 0,
                        T ~ NA_real_),
    `13.3a` = case_when(tiempo_sentencia >= 0 & tiempo_sentencia < 4.99 ~ 0,
                        tiempo_sentencia >= 4.99 & tiempo_sentencia < 9.99 ~ 0,
                        tiempo_sentencia >= 9.99 & tiempo_sentencia < 14.99 ~ 1,
                        tiempo_sentencia >= 14.99 & tiempo_sentencia < 19.99 ~ 0,
                        tiempo_sentencia >= 20  ~ 0,
                        T ~ NA_real_),
    `13.4a` = case_when(tiempo_sentencia >= 0 & tiempo_sentencia < 4.99 ~ 0,
                        tiempo_sentencia >= 4.99 & tiempo_sentencia < 9.99 ~ 0,
                        tiempo_sentencia >= 9.99 & tiempo_sentencia < 14.99 ~ 0,
                        tiempo_sentencia >= 14.99 & tiempo_sentencia < 19.99 ~ 1,
                        tiempo_sentencia >= 20  ~ 0,
                        T ~ NA_real_),
    `14.1a` = case_when(flagrancia == 1 & tortura_generalizada == 1 ~ 1,
                        flagrancia == 1 & tortura_generalizada == 0 ~ 0,
                        T ~ NA_real_),
    `14.2a` = case_when(det_ninguna == 1 & tortura_generalizada == 1 ~ 1,
                        det_ninguna == 1 & tortura_generalizada == 0 ~ 0,
                        T ~ NA_real_),
    `14.3a` = case_when(orden_det == 1 & tortura_generalizada == 1 ~ 1,
                        orden_det == 1 & tortura_generalizada == 0 ~ 0,
                        T ~ NA_real_),
    `14.4a` = case_when(inspeccion == 1 & tortura_generalizada == 1 ~ 1,
                        inspeccion == 1 & tortura_generalizada == 0 ~ 0,
                        T ~ NA_real_),
    `15.1a` = case_when(P4_3A_3 == "1" ~ 1,
                        P4_3A_4 == "1" ~ 1,
                        P4_3A_3 == "2" & P4_3A_4 == "2" ~ 0,
                        T ~ NA_real_),
    `15.2a` = case_when(P4_3A_2 == "1" ~ 1,
                        P4_3A_2 == "2" ~ 0,
                        T ~ NA_real_),
    `15.3a` = case_when(P4_3A_1 == "1" ~ 1,
                        P4_3A_1 == "2" ~ 0,
                        T ~ NA_real_),
    `15.4a` = case_when(P4_3A_5 == "1" ~ 1,
                        P4_3A_6 == "1" ~ 1,
                        P4_3A_7 == 1 ~ 1,
                        P4_3A_8 == 1 ~ 1,
                        P4_3A_5 == "2" & P4_3A_6 == "2" & P4_3A_7 == 0 & P4_3A_8 == 0 ~ 0,
                        T ~ NA_real_),
    `15.5a` = case_when(P4_3A_9 == 1 ~ 1,
                        P4_3A_9 == 0 ~ 0,
                        T ~ NA_real_),
    `16.1a` = case_when(P5_6 == "1" & P5_10 == "1" ~ 1,
                        P5_6 == "1" & P5_10 == "2" ~ 1,
                        P5_6 == "1" & P5_10 == "3" ~ 1,
                        P5_6 == "1" & P5_10 == "4" ~ 1,
                        P5_6 == "1" & P5_10 == "5" ~ 0,
                        P5_6 == "1" & P5_10 == "6" ~ 0,
                        P5_6 == "1" & P5_10 == "7" ~ 0,
                        T ~ NA_real_),
    `16.2a` = case_when(P5_6 == "2" & P5_10 == "1" ~ 1,
                        P5_6 == "2" & P5_10 == "2" ~ 1,
                        P5_6 == "2" & P5_10 == "3" ~ 1,
                        P5_6 == "2" & P5_10 == "4" ~ 1,
                        P5_6 == "2" & P5_10 == "5" ~ 0,
                        P5_6 == "2" & P5_10 == "6" ~ 0,
                        P5_6 == "2" & P5_10 == "7" ~ 0,
                        T ~ NA_real_)
  ) %>%
  select(Estado_arresto, `1a`, `2a`, `3a`, `4a`, `5.1a`, `6.1a`, `6.3a`, `6.2a`,`6.4a`, `7.1a`,`7.2a`,`7.3a`,`7.4a`,`7.5a`,`7.6a`,`7.7a`,
         `8.1a`,`8.2a`, `9.1a`, `9.2a`, `9.3a`, `10.1a`, `10.2a`, `10.3a`, `10.4a`, `11.1a`, `11.2a`, `12.1a`, `12.2a`, `12.3a`,
         `13.1a`,`13.2a`,`13.3a`,`13.4a`, `14.1a`, `14.2a`, `14.3a`, `14.4a`, `15.1a`, `15.2a`, `15.3a`, `15.4a`, `15.5a`,
          `16.1a`, `16.2a`) 

homicidios_totales <- snsp_2 %>% summarise(homicidios = sum(`Homicidio doloso, SESNSP`)) %>% unlist()
robos_vehi_totales <- snsp_2 %>% summarise(homicidios = sum(`Robo de vehículo, SESNSP`)) %>% unlist()

porc_hom_nac <- data_subset.df %>% mutate(Estado = "Promedio Nacional") %>% group_by(Estado) %>% summarise(detenidos_hom = sum(`6.2a`))
porc_rob_nac <- data_subset.df %>% mutate(Estado = "Promedio Nacional") %>% group_by(Estado) %>% summarise(detenidos_rob = sum(`6.4a`))

porc_hom_edo <- data_subset.df %>% rename(Estado = Estado_arresto) %>% group_by(Estado) %>% summarise(detenidos_hom = sum(`6.2a`)) %>% bind_rows(porc_hom_nac) %>% filter(!is.na(Estado))
porc_rob_edo <- data_subset.df %>% rename(Estado = Estado_arresto) %>% group_by(Estado) %>% summarise(detenidos_rob = sum(`6.4a`)) %>% bind_rows(porc_rob_nac) %>% filter(!is.na(Estado))


National <- data_subset.df %>%
  ungroup() %>%
  summarise(
    across(
      ends_with("a"),
      ~mean(.x, na.rm = T)
    )
  ) %>%
  mutate(
    `Estado` = "Promedio Nacional"
  ) %>%
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
  ) %>%
  mutate(`6.1a` = homicidios_totales,
         `6.3a` = robos_vehi_totales)
  


Estatal <- data_subset.df %>%
  ungroup() %>%
  group_by(Estado_arresto) %>%
  summarise(
    across(
      ends_with("a"),
      ~mean(.x, na.rm = T)
    )
  ) %>%
  drop_na() %>%
  rename(Estado = Estado_arresto) %>%
  mutate(
    across(
      ends_with("a"),
      list(b = ~rank(-.x)),
      .names = "{sub('a$', 'b', .col)}"
    )
  )

Estatal_numbers <- data_subset.df %>%
  ungroup() %>%
  group_by(Estado_arresto) %>%
  summarise(
    across(
      ends_with("a"),
      ~sum(!is.na(.x), na.rm = T)
    )
  ) %>%
  drop_na() %>%
  mutate(
    across(
      ends_with("a"),
      ~case_when(
        .x < 30 ~ 1,
        .x >= 30 ~ 0
      )
    )
  ) %>%
  rename(Estado = Estado_arresto)

final_data_experiencias <- bind_rows(Estatal, National) %>% left_join(porc_hom_edo, by = "Estado") %>% left_join(porc_rob_edo, by="Estado") %>%
  mutate(`6.2a`= detenidos_hom/`6.1a`,`6.4a`= detenidos_rob/`6.3a`)

writexl::write_xlsx(x = final_data_experiencias, path = paste0(path2SP,"/National/Visualization/Output/Estrategias_PoliticaCriminal.xlsx"))
