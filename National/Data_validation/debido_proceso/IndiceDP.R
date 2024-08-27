## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Validation
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


# Índice Proceso Justo

data.df <- master_data.df %>% 
  filter(sentenciado == 1) %>% 
  mutate(
    PJ_1_v = case_when(
      as.numeric(P5_22_02) == 1 ~ 1,
      as.numeric(P5_22_02) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    PJ_2_v = case_when(
      as.numeric(P5_22_01) == 1 ~ 1,
      as.numeric(P5_22_01) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    PJ_3_v = case_when(
      as.numeric(P4_7) %in% c(4, 5) ~ 0,
      as.numeric(P4_7) %in% c(1, 2, 3, 6, 7, 8, 9, 10) ~ 1,
      as.numeric(P4_6_4) == 0 ~ 1,
      (as.numeric(P4_4) != 1 | as.numeric(P4_5) != 1) ~ 1,
      TRUE ~ NA_real_
    ),
    PJ_4_v = case_when(
      as.numeric(P4_1_05) == 1 ~ 1,
      as.numeric(P4_1_05) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    PJ_5_v = case_when(
      P5_20_2 == 2 & P5_20_1 == 2 ~ 1,
      P5_20_2 == 1 | P5_20_1 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    PJ_6_v = case_when(
      (P3_20 %in% c("06", "07", "08", "09")) &
        (P3_17_08 == 1 | (is.na(P3_17_08) & !P3_19 %in% c("01", "02", "03", "13"))) ~ 0,
      (P3_20 %in% c("06", "07", "08", "09")) & 
        (is.na(P3_17_08) | P3_17_08 != 1) & P3_19 %in% c("01", "02", "03", "13") ~ 1,
      P3_20 %in% c("01", "02", "03", "04", "05") ~ 1,
      TRUE ~ NA_real_
    ),
    PJ_7_v = case_when(
      as.numeric(P5_16_2) == 1 ~ 1,
      as.numeric(P5_16_2) %in% c(2, 3, 4) ~ 0,
      TRUE ~ NA_real_
    )
  )  %>%
  rowwise() %>% 
  mutate(total_PJ = mean(c_across(c(PJ_1_v, PJ_2_v, PJ_3_v, PJ_4_v, PJ_5_v, PJ_6_v, PJ_7_v )), na.rm = TRUE))

mean(master_data.df$indicator_PJ, na.rm = T)
mean(round(data.df$total_PJ, 1), na.rm = T)     




# Índice Debido Proceso

data.df <- master_data.df %>% 
  filter(sentenciado == 1) %>% 
  mutate(DP_1_v = case_when(tortura_generalizada == 1 ~ 0,
                   tortura_generalizada == 0 ~ 1,
                   T ~ NA_real_),
         DP_2_v =case_when(P3_10 == "5"  ~ 0,
                           P3_10 == "1" |
                             P3_10 == "2" |
                             P3_10 == "3" |
                             P3_10 == "4" ~ 1,
                   T ~ NA_real_)
  )  %>%
  rowwise() %>% 
  mutate(total_DP = mean(c_across(c(DP_1_v, DP_2_v)), na.rm = TRUE))

mean(master_data.df$indicator_GDH, na.rm = T)
mean(round(data.df$total_DP, 1), na.rm = T)

# Índice Debido Proceso

data.df <- master_data.df %>% 
  filter(sentenciado == 1) %>% 
  mutate(DP_1_v = case_when(tortura_generalizada == 1 ~ 0,
                            tortura_generalizada == 0 ~ 1,
                            T ~ NA_real_),
         DP_2_v =case_when(P3_10 == "5"  ~ 0,
                           P3_10 == "1" |
                             P3_10 == "2" |
                             P3_10 == "3" |
                             P3_10 == "4" ~ 1,
                           T ~ NA_real_)
  )  %>%
  rowwise() %>% 
  mutate(total_DP = mean(c_across(c(DP_1_v, DP_2_v)), na.rm = TRUE))

mean(master_data.df$indicator_GDH, na.rm = T)
mean(round(data.df$total_DP, 1), na.rm = T)



                   