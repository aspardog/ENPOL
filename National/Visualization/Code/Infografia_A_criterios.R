data_subset.df <- master_data.df %>%
  filter(sentenciado == 1) %>%
  filter(Anio_arresto >= as.numeric(2018)) %>% 
  mutate(
    `1a` = case_when(P5_22_02 == 2 ~ 0,
                     P5_22_02 == 1 ~ 1,
                     T ~ NA_real_),
    `2a` = case_when(P5_22_01 == 2 ~ 0,
                     P5_22_01 == 1 ~ 1,
                     T ~ NA_real_),
    `3a` = case_when(P4_7 == "04" | P4_7 == "05" ~ 0,
                     (P4_4 == "2" & P4_5 == "2") ~ 1,
                     P4_6_4 == 1 & (is.na(P4_7) == T | (P4_7 != "04" & P4_7 != "05")) ~ 1,
                     P4_6_4 == 0 ~ 1,
                     T ~ NA_real_),
    `4a` = case_when(P4_1_05 == "2" ~ 0,
                     P4_1_05 == "1" ~ 1,
                     T ~ NA_real_),
    `5a` = case_when(P5_20_2 == 1 | P5_20_1 == 1 ~ 0,
                     P5_20_2 == 2 & P5_20_1 == 2 ~ 1,
                     T ~ NA_real_),
    `7a` = case_when((P3_20 == "06" | P3_20 == "07" | P3_20 == "08" | P3_20 == "09") &
                       (P3_17_08 == 1 | (is.na(P3_17_08) == T | P3_19 != "01" & P3_19 != "02" & P3_19 !="03" & P3_19 !="13")) ~ 0,
                     (P3_20 == "06" | P3_20 == "07" | P3_20 == "08" | P3_20 == "09") & 
                       (is.na(P3_17_08) == T | P3_17_08 != 1) & (P3_19 == "01" | P3_19 == "02" | P3_19 =="03" | P3_19 !="13") ~ 1,
                     P3_20 == "01" | P3_20 == "02" | P3_20 == "03" | P3_20 == "04" | P3_20 == "05" ~ 1,
                     T ~ NA_real_),
    `6a` = case_when(P5_16_2 == 2 | P5_16_2 == 3| P5_16_2 == 4 ~ 0,
                     P5_16_2 == 1  ~ 1,
                     T ~ NA_real_),
    `8a` = case_when(proporcionalidad_uso_fuerza == 1 ~ 1,
                      proporcionalidad_uso_fuerza == 0 ~ 0,
                      T ~ NA_real_),
    `9a` = case_when(P3_21_1 == 1 | P3_21_2 == 1 ~ 0,
                      (P3_21_1 == 2 & P3_21_2 == 2) | (is.na(P3_21_1) == T & P3_21_2 == 2) | (P3_21_1 == 2 & is.na(P3_21_2) == T) ~ 1,
                      T ~ NA_real_),
    `10a` = case_when(P4_15_1 == 1 | P4_15_3 == 1 ~ 0,
                      (P4_15_1 == 2 & P4_15_3 == 2) | (is.na(P4_15_1) == T & P4_15_3 == 2) | (P4_15_1 == 2 & is.na(P4_15_3) == T) ~ 1,
                      T ~ NA_real_),
    `11a` = case_when(P5_45_1 == 1 | P5_45_3 == 1 ~ 0,
                      (P5_45_1 == 2 & P5_45_3 == 2) | (is.na(P5_45_1) == T & P5_45_3 == 2) | (P5_45_1 == 2 & is.na(P5_45_3) == T) ~ 1,
                      T ~ NA_real_),
    `12a` = case_when(tortura_generalizada == 1 ~ 0,
                      tortura_generalizada == 0 ~ 1,
                      T ~ NA_real_),
    `13a` = case_when(det_ninguna == 1 ~ 0,
                      det_ninguna == 0 ~ 1,
                      T ~ NA_real_)
  ) %>%
  select(Estado_arresto, `1a`, `2a`, `3a`, `4a`, `5a`, `6a`, `7a`, `8a`, `9a`, `10a`, `11a`, `12a`, `13a`) 

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
  )

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
      ~sum(.x, na.rm = T)
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

final_data_experiencias <- bind_rows(Estatal, National) %>%
  filter(Estado == "Promedio Nacional") %>%
  pivot_longer(cols = !Estado, names_to = "category", values_to = "values") 

writexl::write_xlsx(x = final_data_experiencias, path = "Output/INF_A_CRITERIOS.xlsx")
