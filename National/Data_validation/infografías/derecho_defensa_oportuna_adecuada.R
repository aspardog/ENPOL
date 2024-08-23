## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - RunMe File
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

source("settings.R")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Derecho a una defensa oportuna y adecuada                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Loading data
load(paste0(path2SP,
            "/National/Data_cleaning/Output/Main_database.RData"))



# Asesoría abogado --------------------------------------------------------

data2plot <- Main_database %>% 
            filter(Anio_arresto >= 2015,
                   NSJP >=  1) %>% 
            mutate(
              P4_1_05 = case_when( 
                        as.numeric(P4_1_05) ==  1 ~ 1 ,
                        as.numeric(P4_1_05) ==  2 ~  0,
                        T ~ NA_real_
                        )
            ) %>% 
            select(P4_1_05)

mean(data2plot$P4_1_05, na.rm = TRUE)


# publico  

data2plot <- Main_database %>% 
  mutate(
    P4_1_05 = case_when( 
      as.numeric(P4_1_05) ==  1 ~ 1 ,
      as.numeric(P4_1_05) ==  2 ~  0,
      T ~ NA_real_
    ),
    publico = case_when(
      as.numeric(P5_21_2) ==  1 & as.numeric(P5_21_1) ==  0  ~ 1, 
      as.numeric(P5_41_2) ==  1 & as.numeric(P5_41_1) ==  0  ~ 1,
      as.numeric(P5_21_2) ==  0 & as.numeric(P5_21_1) ==  1  ~ 0, 
      as.numeric(P5_41_2) ==  0 & as.numeric(P5_41_1) ==  1  ~ 0,
      T ~ NA_real_
    )
  ) %>%
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>%
  select(P4_1_05, publico) %>% 
  group_by(P4_1_05, publico) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(P4_1_05) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)


# hablo_abogado -----------------------------------------------------------

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P4_1_06 = case_when( 
      as.numeric(P4_1_06) ==  1 ~ 1 ,
      as.numeric(P4_1_06) ==  2 ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P4_1_06)

mean(data2plot$P4_1_06, na.rm = TRUE)



# presente_señalamiento ---------------------------------------------------


data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P4_14_1 = case_when( 
      as.numeric(P4_14_1) ==  1 ~ 1 ,
      as.numeric(P4_14_1) ==  2 ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P4_14_1)

mean(data2plot$P4_14_1, na.rm = TRUE)



# público


data2plot <- Main_database %>% 
  mutate(
    P4_14_1 = case_when( 
      as.numeric(P4_14_1) ==  1 ~ 1 ,
      as.numeric(P4_14_1) ==  2 ~  0,
      T ~ NA_real_
    ),
    publico = case_when(
      as.numeric(P5_21_2) ==  1 & as.numeric(P5_21_1) ==  0  ~ 1, 
      as.numeric(P5_41_2) ==  1 & as.numeric(P5_41_1) ==  0  ~ 1,
      as.numeric(P5_21_2) ==  0 & as.numeric(P5_21_1) ==  1  ~ 0, 
      as.numeric(P5_41_2) ==  0 & as.numeric(P5_41_1) ==  1  ~ 0,
      T ~ NA_real_
    )
  ) %>%
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>%
  select(P4_14_1, publico) %>% 
  group_by(P4_14_1, publico) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(publico) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)

# abogado-antes ---------------------------------------------------


data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_1 = case_when( 
      as.numeric(P5_1) ==  1 ~ 1 ,
      as.numeric(P5_1) ==  2 ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_1)

mean(data2plot$P5_1, na.rm = TRUE)


# público


data2plot <- Main_database %>% 
  mutate(
    P5_1 = case_when( 
      as.numeric(P5_1) ==  1 ~ 1 ,
      as.numeric(P5_1) ==  2 ~  0,
      T ~ NA_real_
    ),
    publico = case_when(
      as.numeric(P5_21_2) ==  1 & as.numeric(P5_21_1) ==  0  ~ 1, 
      as.numeric(P5_41_2) ==  1 & as.numeric(P5_41_1) ==  0  ~ 1,
      as.numeric(P5_21_2) ==  0 & as.numeric(P5_21_1) ==  1  ~ 0, 
      as.numeric(P5_41_2) ==  0 & as.numeric(P5_41_1) ==  1  ~ 0,
      T ~ NA_real_
    )
  ) %>%
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>%
  select(P5_1, publico) %>% 
  group_by(P5_1, publico) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(P5_1) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)

# abogado-inicial ---------------------------------------------------


data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_2_5 = case_when( 
      as.numeric(P5_2_5) ==  1 ~ 1 ,
      as.numeric(P5_2_5) ==  2 ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_2_5)

mean(data2plot$P5_2_5, na.rm = TRUE)


# público


data2plot <- Main_database %>% 
  mutate(
    P5_2_5 = case_when( 
      as.numeric(P5_2_5) ==  1 ~ 1 ,
      as.numeric(P5_2_5) ==  2 ~  0,
      T ~ NA_real_
    ),
    publico = case_when(
      as.numeric(P5_21_2) ==  1 & as.numeric(P5_21_1) ==  0  ~ 1, 
      as.numeric(P5_41_2) ==  1 & as.numeric(P5_41_1) ==  0  ~ 1,
      as.numeric(P5_21_2) ==  0 & as.numeric(P5_21_1) ==  1  ~ 0, 
      as.numeric(P5_41_2) ==  0 & as.numeric(P5_41_1) ==  1  ~ 0,
      T ~ NA_real_
    )
  ) %>%
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>%
  select(P5_2_5, publico) %>% 
  group_by(P5_2_5, publico) %>% 
  drop_na() %>% 
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(P5_2_5) %>% 
  mutate(value2plot = Frequency / sum(Frequency) * 100)

