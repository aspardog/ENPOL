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
## 0.  Derecho a un tribunal imparcial, transparencte y responsivo                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Loading data
load(paste0(path2SP,
            "/National/Data_cleaning/Output/Main_database.RData"))


# Silencio Detención  ----------------------------------------------------
table(Main_database$P3_14_5)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1) %>% 
  mutate(
    P3_14_5 = case_when( 
      as.numeric(P3_14_5) ==  1  ~ 1 ,
      as.numeric(P3_14_5) ==  0  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P3_14_5)

mean(data2plot$P3_14_5, na.rm = TRUE)

# Silencio MP  ----------------------------------------------------
table(Main_database$P4_1_04)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1) %>% 
  mutate(
    P4_1_04 = case_when( 
      as.numeric(P4_1_04) ==  1  ~ 1 ,
      as.numeric(P4_1_04) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P4_1_04)

mean(data2plot$P4_1_04, na.rm = TRUE)

# Silencio Juez  ----------------------------------------------------
table(Main_database$P4_3A_2)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1) %>% 
  mutate(
    P4_3A_2 = case_when( 
      as.numeric(P4_3A_2) ==  1  ~ 1 ,
      as.numeric(P4_3A_2) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P4_3A_2)

mean(data2plot$P4_3A_2, na.rm = TRUE)

# Explicación Detención  ----------------------------------------------------
table(Main_database$P3_14_4)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1) %>% 
  mutate(
    P3_14_4 = case_when( 
      as.numeric(P3_14_4) ==  1  ~ 1 ,
      as.numeric(P3_14_4) ==  0  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P3_14_4)

mean(data2plot$P3_14_4, na.rm = TRUE)

# Explicación MP  ----------------------------------------------------
table(Main_database$P4_1_03)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1) %>% 
  mutate(
    P4_1_03 = case_when( 
      as.numeric(P4_1_03) ==  1  ~ 1 ,
      as.numeric(P4_1_03) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P4_1_03)

mean(data2plot$P4_1_03, na.rm = TRUE)

# Explicación Juez  ----------------------------------------------------
table(Main_database$P5_2_1)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1) %>% 
  mutate(
    P5_2_1 = case_when( 
      as.numeric(P5_2_1) ==  1  ~ 1 ,
      as.numeric(P5_2_1) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_2_1)

mean(data2plot$P5_2_1, na.rm = TRUE)


# Claridad Detención  ----------------------------------------------------
table(Main_database$P3_14_4)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1) %>% 
  mutate(
    P3_14_4 = case_when( 
      as.numeric(P3_14_4) ==  1  ~ 1 ,
      as.numeric(P3_14_4) ==  0  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P3_14_4)

mean(data2plot$P3_14_4, na.rm = TRUE)

# Claridad MP  ----------------------------------------------------
table(Main_database$P4_1_03)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1) %>% 
  mutate(
    P4_1_03 = case_when( 
      as.numeric(P4_1_03) ==  1  ~ 1 ,
      as.numeric(P4_1_03) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P4_1_03)

mean(data2plot$P4_1_03, na.rm = TRUE)

# Claridad Juez  ----------------------------------------------------
table(Main_database$P5_2_1)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1) %>% 
  mutate(
    P5_2_1 = case_when( 
      as.numeric(P5_2_1) ==  1  ~ 1 ,
      as.numeric(P5_2_1) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_2_1)

mean(data2plot$P5_2_1, na.rm = TRUE)
