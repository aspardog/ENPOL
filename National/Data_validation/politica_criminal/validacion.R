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



# Delincuencia prolífica --------------------------------------------------

# 1. Reincidentes
table(Main_database$P9_1)

data2plot <- Main_database %>% 
              filter( Anio_arresto >= 2015,
                      NSJP == 1) %>% 
              mutate(P9_1 = case_when(
                                              as.numeric(P9_1) == 1 ~ 1,
                                              as.numeric(P9_1) == 2 ~ 0,
                                              T ~ NA_real_)) %>% 
  select(P9_1)

mean(data2plot$P9_1, na.rm = TRUE)


# 1.1 Reincidetes por mismo tipo de delito


data2plot <- Main_database %>% 
  mutate(P9_1 = case_when(
    as.numeric(P9_1) == 1 ~ 1,
    as.numeric(P9_1) == 2 ~ 0,
    T ~ NA_real_)) %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1,
         P9_1 == 1) %>%  
  mutate(Delito_gr_1_robos2 = case_when( P9_2_01 == "1" ~ 1,
                                        P9_2_02 == "1" ~ 1,
                                        P9_2_03 == "1"  ~ 1,
                                        P9_2_04 == "1"  ~ 1,
                                        P9_2_05 == "1" ~ 1,
                                        P9_2_06 == "1"  ~ 1,
                                        P9_2_07 == "1"  ~ 1,
                                        T ~ 0),
         Delito_gr_2_drogas2 = case_when( P9_2_08 == "1"  ~ 1,
                                          P9_2_09 == "1"  ~ 1,
                                         T ~ 0),
         Delito_gr_3_del_org2 = case_when( P9_2_20 == "1"  ~ 1,
                                          T ~ 0),
         Delito_gr_4_lesiones2 = case_when( P9_2_10 == "1" ~ 1,
                                           T ~ 0),
         Delito_gr_5_hom_cul2 = case_when( P9_2_11 == "1"~ 1,
                                          T ~ 0),
         Delito_gr_6_hom_dol2 = case_when( P9_2_12 == "1"  ~ 1,
                                          T ~ 0),
         Delito_gr_7_armas2 = case_when( P9_2_13 == "1"  ~ 1,
                                        T ~ 0),
         Delito_gr_8_viol_fam2 = case_when( P9_2_15 == "1"  ~ 1,
                                           T ~ 0),
         Delito_gr_9_secuestro2 = case_when( P9_2_17 == "1" ~ 1,
                                             P9_2_23 == "1" ~ 1,
                                            T ~ 0),
         Delito_gr_10_sexuales2 = case_when( P9_2_18 == "1" ~ 1,
                                            P9_2_21 == "1"  ~ 1,
                                            T ~ 0),
         Delito_gr_11_extorsion2 = case_when( P9_2_22 == "1" ~ 1,
                                             T ~ 0),
         Delito_gr_12_fraude2 = case_when( P9_2_19 == "1" ~ 1,
                                           P9_2_24 == "1" ~ 1,
                                          T ~ 0),
         Delito_gr_13_amenazas2 = case_when( P9_2_25 == "1"  ~ 1,
                                            T ~ 0),
         Delito_gr_14_otro2 = case_when( P9_2_14 == "1"  ~ 1,
                                         P9_2_16 == "1" ~ 1,
                                         P9_2_26 == "1"  ~ 1,
                                        T ~ 0),
         Delito_gr_15_ns_nr2 = case_when( P9_2_98 == "1"  ~ 1,
                                          P9_2_99 == "1"  ~ 1,
                                         T ~ 0))  %>% 
        mutate(mismo = case_when( Delito_gr_1_robos == 1     & Delito_gr_1_robos2  == 1  ~ 1,
                                  Delito_gr_2_drogas == 1    & Delito_gr_2_drogas2  == 1  ~ 1,
                                  Delito_gr_3_del_org== 1    & Delito_gr_3_del_org2  == 1 ~ 1,
                                  Delito_gr_4_lesiones== 1   & Delito_gr_4_lesiones2  == 1 ~ 1,
                                  Delito_gr_5_hom_cul== 1    & Delito_gr_5_hom_cul2  == 1 ~ 1,
                                  Delito_gr_6_hom_dol== 1    & Delito_gr_6_hom_dol2  == 1 ~ 1,
                                  Delito_gr_7_armas== 1      & Delito_gr_7_armas2  == 1 ~ 1,
                                  Delito_gr_8_viol_fam== 1   & Delito_gr_8_viol_fam2  == 1 ~ 1,
                                  Delito_gr_9_secuestro== 1  & Delito_gr_9_secuestro2  == 1 ~ 1,
                                  Delito_gr_10_sexuales== 1  & Delito_gr_10_sexuales2  == 1 ~ 1,
                                  Delito_gr_11_extorsion== 1 & Delito_gr_11_extorsion2  == 1 ~ 1,
                                  Delito_gr_12_fraude== 1    & Delito_gr_12_fraude2  == 1 ~ 1,
                                  Delito_gr_13_amenazas== 1  & Delito_gr_13_amenazas2  == 1 ~ 1,
                                  Delito_gr_14_otro== 1      & Delito_gr_14_otro2  == 1 ~ 1,
                                  T ~ 0))
table(data2plot$mismo)


# 1.3 Freuencia reincidentes

data2plot <- data2plot %>%
  filter(mismo == 1) %>% 
  mutate(
    Delito_gr_categ2 = case_when(
      Delito_gr_1_robos2 == 1 ~ "robos",
      Delito_gr_2_drogas2 == 1 ~ "drogas",
      Delito_gr_3_del_org2 == 1 ~ "org",
      Delito_gr_4_lesiones2 == 1 ~ "lesiones",
      Delito_gr_5_hom_cul2 == 1 ~ "hom_cul",
      Delito_gr_6_hom_dol2 == 1 ~ "hom_dol",
      Delito_gr_7_armas2 == 1 ~ "armas",
      Delito_gr_8_viol_fam2 == 1 ~ "viol_fam",
      Delito_gr_9_secuestro2 == 1 ~ "secuestro",
      Delito_gr_10_sexuales2 == 1 ~ "sexuales",
      Delito_gr_11_extorsion2 == 1 ~ "extorsion",
      Delito_gr_12_fraude2 == 1 ~ "fraude",
      Delito_gr_13_amenazas2 == 1 ~ "amenazas",
      Delito_gr_14_otro2 == 1 ~ "otro",
      Delito_gr_15_ns_nr2 == 1 ~ "ns_nr",
      TRUE ~ NA_character_
    )) %>% 
  select(Delito_gr_categ2) %>% 
  group_by(Delito_gr_categ2) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)


# Tipo  de fuero ----------------------------------------------------------

table(Main_database$P5_13_1)
table(Main_database$P5_13_2)
table(Main_database$P5_13_8)
table(Main_database$P5_13_9)


data2plot <- Main_database %>% 
  filter( Anio_arresto >= 2015,
          NSJP == 1) %>% 
  mutate(fuero = case_when(
    as.numeric(P5_13_1) == 1 & as.numeric(P5_13_2) == 0 ~ "federal",
    as.numeric(P5_13_1) == 0 & as.numeric(P5_13_2) == 1 ~ "comun",
    as.numeric(P5_13_1) == 1 & as.numeric(P5_13_2) == 1 ~ "ambos",
    as.numeric(P5_33_1) == 1 & as.numeric(P5_33_2) == 0 ~ "federal",
    as.numeric(P5_33_1) == 0 & as.numeric(P5_33_2) == 1 ~ "comun",
    as.numeric(P5_33_1) == 1 & as.numeric(P5_33_2) == 1 ~ "ambos",
    as.numeric(P5_33_8) == 1 ~ NA_character_,
    as.numeric(P5_33_9) == 1 ~ NA_character_,
    T ~ NA_character_)) %>% 
  select(fuero) %>% 
  group_by(fuero) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)

# Delitos comun detenidos por fuero federal ----------------------------------------------------------
table(Main_database$P3_2)

data2plot <- Main_database %>% 
  filter( Anio_arresto >= 2015,
          NSJP == 1) %>% 
  mutate(fuero = case_when(
    as.numeric(P5_13_1) == 1 & as.numeric(P5_13_2) == 0 ~ "federal",
    as.numeric(P5_13_1) == 0 & as.numeric(P5_13_2) == 1 ~ "comun",
    as.numeric(P5_13_1) == 1 & as.numeric(P5_13_2) == 1 ~ "ambos",
    as.numeric(P5_33_1) == 1 & as.numeric(P5_33_2) == 0 ~ "federal",
    as.numeric(P5_33_1) == 0 & as.numeric(P5_33_2) == 1 ~ "comun",
    as.numeric(P5_33_1) == 1 & as.numeric(P5_33_2) == 1 ~ "ambos",
    as.numeric(P5_13_8) == 1 ~ NA_character_,
    as.numeric(P5_13_9) == 1 ~ NA_character_,
    T ~ NA_character_)) %>% 
  filter(P3_2 == "03" | P3_2 == "05" |P3_2 == "06" |  P3_2 == "07"| P3_2 == "08") %>% 
  select(fuero) %>% 
  group_by(fuero) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)


# Tipos de detención de la Policía Federal Ministerial, 2015 a 2021 serie de tiempo -------------------------------------------------------------------------

table(Main_database$P3_10)


data2plot <- Main_database %>% 
  filter( Anio_arresto >= 2015,
          NSJP == 1,
          P3_2 == "05" ) %>% 
  mutate(detencion = case_when(P3_10 == "1" | P3_10 == "2" ~ "flagrancia", 
                               P3_10 == "3"  ~ "orden",
                               P3_10 == "4"  ~ "inspeccion",
                               P3_10 == "5"  ~ "irregular",
                               T ~ NA_character_)) %>%
  select(detencion, Anio_arresto) %>% 
  group_by(detencion, Anio_arresto) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Anio_arresto) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)
  
# Tipos de detención según la corporación que la realizó -------------------------------------------------------------------------

table(Main_database$P3_10)


data2plot <- Main_database %>% 
  filter( Anio_arresto >= 2015,
          NSJP == 1,
          P3_2 == "05" |
          P3_2 == "06" |
          P3_2 == "03") %>% 
  mutate(detencion = case_when(P3_10 == "1" | P3_10 == "2" ~ "flagrancia", 
                               P3_10 == "3"  ~ "orden",
                               P3_10 == "4"  ~ "inspeccion",
                               P3_10 == "5"  ~ "irregular",
                               T ~ NA_character_),
         P3_2 = case_when(P3_2 == "03" ~ "06",
                          T ~ P3_2)) %>%
  select(detencion, P3_2) %>% 
  group_by(detencion, P3_2) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(P3_2) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)


# Tipos de detención, 2015 a 2021 serie de tiempo -------------------------------------------------------------------------

table(Main_database$P3_10)


data2plot <- Main_database %>% 
  filter( Anio_arresto >= 2015,
          NSJP == 1) %>% 
  mutate(detencion = case_when(P3_10 == "1" | P3_10 == "2" ~ "flagrancia", 
                               P3_10 == "3"  ~ "orden",
                               P3_10 == "4"  ~ "inspeccion",
                               P3_10 == "5"  ~ "irregular",
                               T ~ NA_character_)) %>%
  select(detencion, Anio_arresto) %>% 
  group_by(detencion, Anio_arresto) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Anio_arresto) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)


# Tipos de estado, 2015 a 2021 serie de tiempo -------------------------------------------------------------------------

table(Main_database$P3_10)


data2plot <- Main_database %>% 
  filter( Anio_arresto >= 2015,
          NSJP == 1) %>% 
  mutate(detencion = case_when(P3_10 == "1" | P3_10 == "2" ~ "flagrancia", 
                               P3_10 == "3"  ~ "orden",
                               P3_10 == "4"  ~ "inspeccion",
                               P3_10 == "5"  ~ "irregular",
                               T ~ NA_character_)) %>%
  select(detencion, Estado_arresto) %>% 
  group_by(detencion, Estado_arresto) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Estado_arresto) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)


# Señalados -------------------------------------------------------------------------


class(Main_database$P4_10)

data2plot <- Main_database %>% 
  filter( Anio_arresto >= 2015,
          NSJP == 1) %>% 
  mutate(P4_10 = case_when(
    as.numeric(P4_10) == 1 ~ 1,
    as.numeric(P4_10) == 2 ~ 0,
    T ~ NA_real_)) %>% 
  select(P4_10)

mean(data2plot$P4_10, na.rm = TRUE)

# Señalados  identificados -------------------------------------------------------------------------

table(Main_database$P4_13)

data2plot <- Main_database %>% 
  filter( Anio_arresto >= 2015,
          NSJP == 1) %>% 
  mutate(P4_13 = case_when(
    as.numeric(P4_13) == 3 ~ 1,
    as.numeric(P4_13) == 4 ~ 1,
    as.numeric(P4_13) == 1 ~ 0,
    as.numeric(P4_13) == 2 ~ 0,
    T ~ NA_real_)) %>% 
  select(P4_13)

mean(data2plot$P4_13, na.rm = TRUE)


# Prácticas de tortura según la autoidentificación de inocencia o de culpabilidad-------------------------------------------------------------------------

table(Main_database$P4_13)

data2plot <- Main_database %>% 
  filter( Anio_arresto >= 2015,
          NSJP == 1) %>% 
  mutate(culpabilidad = case_when(
    P3_1 == "1" | P3_1 == "2"  ~ 1,
    P3_1 == "3" | P3_1 == "4"  ~ 0,
    T ~ NA),
    totrura_lugar = case_when(tortura_mp == 1  & tortura_tra == 1~ "ambas",
                             tortura_mp == 0  & tortura_tra == 0 ~ "ninguna",
                             tortura_tra == 1 ~ "traslado",
                             tortura_mp == 1 ~ "mp",
                             T ~ NA_character_)) %>% 
  select(culpabilidad, totrura_lugar) %>%
  group_by(culpabilidad, totrura_lugar) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(culpabilidad) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)


# Prácticas de tortura detención------------------------------------------------------------------------

table(Main_database$P4_13)

data2plot <- Main_database %>% 
  filter( Anio_arresto >= 2015,
          NSJP == 1) %>% 
  mutate(culpabilidad = case_when(
    P3_1 == "1" | P3_1 == "2"  ~ 1,
    P3_1 == "3" | P3_1 == "4"  ~ 0,
    T ~ NA),
    totrura_lugar = case_when(tortura_mp == 1  & tortura_tra == 1~ "ambas",
                              tortura_mp == 0  & tortura_tra == 0 ~ "ninguna",
                              tortura_tra == 1 ~ "traslado",
                              tortura_mp == 1 ~ "mp",
                              T ~ NA_character_),
    detencion = case_when(P3_10 == "1" | P3_10 == "2" ~ "flagrancia", 
                          P3_10 == "3"  ~ "orden",
                          P3_10 == "4"  ~ "inspeccion",
                          P3_10 == "5"  ~ "irregular",
                          T ~ NA_character_)) %>% 
  select(detencion, totrura_lugar) %>%
  group_by(detencion, totrura_lugar) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(detencion) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)
