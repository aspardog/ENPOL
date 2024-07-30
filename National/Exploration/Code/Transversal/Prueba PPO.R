

#Prueba PPO




delitos_PPO1    <- c("P5_11_08", "P5_11_09", "P5_11_12", "P5_11_17", "P5_11_18", "P5_11_20",
                     "P5_31_08", "P5_31_09", "P5_31_12", "P5_31_17", "P5_31_18", "P5_31_20")
delitos_PPO2    <- c("P5_11_02", "P5_11_08", "P5_11_09", "P5_11_12", "P5_11_13", "P5_11_17", "P5_11_18", "P5_11_20",
                     "P5_31_02", "P5_31_08", "P5_31_09", "P5_31_12", "P5_31_13", "P5_31_17", "P5_31_18", "P5_31_20")

delitos_PPO1s    <- c("P5_11_08", "P5_11_09", "P5_11_12", "P5_11_17", "P5_11_18", "P5_11_20")
delitos_PPO2s   <- c("P5_11_02", "P5_11_08", "P5_11_09", "P5_11_12", "P5_11_13", "P5_11_17", "P5_11_18", "P5_11_20")
delitos_PPO1p    <- c("P5_31_08", "P5_31_09", "P5_31_12", "P5_31_17", "P5_31_18", "P5_31_20")
delitos_PPO2p    <- c("P5_31_02", "P5_31_08", "P5_31_09", "P5_31_12", "P5_31_13", "P5_31_17", "P5_31_18", "P5_31_20")


#PPO_p1 es la variable previa, PPO_p2 es la variable corregida con error, PPO_p3 es la variable limpia, genera resultados idénticos a PPO_p1

base_prueba <- Main_database_2008 %>% mutate(
  PPO_p1 = case_when(
  months_since_PPO_2 >= 0 & if_any(delitos_PPO2, ~ .x %in% "1") ~ 1,
  months_since_PPO_2 >= 0 & if_any(delitos_PPO2, ~ .x %in% "0") ~ 0,
  months_since_PPO_2 < 0 & months_since_PPO_1 >= 0 & if_any(delitos_PPO1, ~ .x %in% "1") ~ 1,
  months_since_PPO_2 < 0 & months_since_PPO_1 >= 0 & if_any(delitos_PPO1, ~ .x %in% "0") ~ 0,
  months_since_PPO_2 < 0 & months_since_PPO_1 < 0 ~ 0),
PPO_p2 = case_when(
  months_since_PPO_2 >= 0 & if_any(delitos_PPO2, ~ .x %in% "1") ~ 1,
  months_since_PPO_2 >= 0 & if_all(delitos_PPO2, ~ .x %in% "0") ~ 0,
  months_since_PPO_2 < 0 & months_since_PPO_1 >= 0 & if_any(delitos_PPO1, ~ .x %in% "1") ~ 1,
  months_since_PPO_2 < 0 & months_since_PPO_1 >= 0 & if_all(delitos_PPO1, ~ .x %in% "0") ~ 0,
  months_since_PPO_2 < 0 & months_since_PPO_1 < 0 ~ 0),
PPO_p3 = case_when(
    months_since_PPO_2 >= 0 & if_any(delitos_PPO2s, ~ .x %in% "1") ~ 1,
    months_since_PPO_2 >= 0 & if_all(delitos_PPO2s, ~ .x %in% "0") ~ 0,
    months_since_PPO_2 >= 0 & if_any(delitos_PPO2p, ~ .x %in% "1") ~ 1,
    months_since_PPO_2 >= 0 & if_all(delitos_PPO2p, ~ .x %in% "0") ~ 0,
    months_since_PPO_2 < 0 & months_since_PPO_1 >= 0 & if_any(delitos_PPO1s, ~ .x %in% "1") ~ 1,
    months_since_PPO_2 < 0 & months_since_PPO_1 >= 0 & if_all(delitos_PPO1s, ~ .x %in% "0") ~ 0,
    months_since_PPO_2 < 0 & months_since_PPO_1 >= 0 & if_any(delitos_PPO1p, ~ .x %in% "1") ~ 1,
    months_since_PPO_2 < 0 & months_since_PPO_1 >= 0 & if_all(delitos_PPO1p, ~ .x %in% "0") ~ 0,
    months_since_PPO_2 < 0 & months_since_PPO_1 < 0 ~ 0)) %>% 
 select(PPO_p1, PPO_p2, PPO_p3, tipo_prision_preventiva, sentenciado, months_since_PPO_1, months_since_PPO_2, P5_11_02, P5_11_08, P5_11_09, P5_11_12, P5_11_13, P5_11_17, P5_11_18, P5_31_02, P5_11_20,P5_31_08, P5_31_09, P5_31_12, P5_31_13, P5_31_17, P5_31_18, P5_31_20)

table(base_prueba$PPO_p1,base_prueba$PPO_p2, useNA = "always")
table(base_prueba$PPO_p1,base_prueba$PPO_p3, useNA = "always")

