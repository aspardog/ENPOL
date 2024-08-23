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


# Contacto con el juez ----------------------------------------------------


data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_2_1 = case_when( 
      as.numeric(P5_2_1) ==  1  | as.numeric(P5_2_1) ==  2 ~ 1 ,
      as.numeric(P5_2_1) ==  4  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_2_1)

mean(data2plot$P5_2_1, na.rm = TRUE)

# Video ----------------------------------------------------

table(Main_database$P5_39_3)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_39_3 = case_when( 
      as.numeric(P5_39_3) ==  1   ~ 1 ,
      as.numeric(P5_39_3) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_39_3)

mean(data2plot$P5_39_3, na.rm = TRUE)


table(Main_database$P5_19_3)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_19_3 = case_when( 
      as.numeric(P5_19_3) ==  1   ~ 1 ,
      as.numeric(P5_19_3) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_19_3)

mean(data2plot$P5_19_3, na.rm = TRUE)

# Público ----------------------------------------------------
table(Main_database$P5_16_5)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_16_5 = case_when( 
      as.numeric(P5_16_5) ==  1  | as.numeric(P5_16_5) ==  2 | as.numeric(P5_16_5) ==  3 ~ 1 ,
      as.numeric(P5_16_5) ==  4 ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_16_5)

mean(data2plot$P5_16_5, na.rm = TRUE)

# Culpable antes ----------------------------------------------------
table(Main_database$P5_25)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_25 = case_when( 
      as.numeric(P5_25) ==  1  ~ 0 ,
      as.numeric(P5_25) ==  2  ~  1,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_25)

mean(data2plot$P5_25, na.rm = TRUE)

# Juez diferente ----------------------------------------------------
table(Main_database$P5_14)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_14 = case_when( 
      as.numeric(P5_14) ==  1  ~ 0 ,
      as.numeric(P5_14) ==  2  ~  1,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_14)

mean(data2plot$P5_14, na.rm = TRUE)

# juez presente ----------------------------------------------------
table(Main_database$P5_16_2)


data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_16_2 = case_when( 
      as.numeric(P5_16_2) ==  1    ~ 1 ,
      as.numeric(P5_16_2) ==  4  | as.numeric(P5_16_2) ==   3  | as.numeric(P5_16_2) ==  2 ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_16_2)

mean(data2plot$P5_16_2, na.rm = TRUE)

# juez control ----------------------------------------------------
table(Main_database$P5_18)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_18 = case_when( 
       as.numeric(P5_18) ==  2 ~ 1 ,
      as.numeric(P5_18) ==  1 | as.numeric(P5_18) ==  3  | as.numeric(P5_18) ==  4  |  as.numeric(P5_18) ==  5  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_18)

mean(data2plot$P5_18, na.rm = TRUE)

# vidrio ----------------------------------------------------


data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_20_1 = case_when( 
      as.numeric(P5_20_1) ==  1  ~ 1 ,
      as.numeric(P5_20_1) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_20_1)

mean(data2plot$P5_20_1, na.rm = TRUE)

# esposado ----------------------------------------------------


data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_20_2 = case_when( 
      as.numeric(P5_20_2) ==  1  ~ 1 ,
      as.numeric(P5_20_2) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_20_2)

mean(data2plot$P5_20_2, na.rm = TRUE)

# uniforme ----------------------------------------------------


data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_20_3 = case_when( 
      as.numeric(P5_20_3) ==  1  ~ 1 ,
      as.numeric(P5_20_3) ==  2  ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_20_3)

mean(data2plot$P5_20_3, na.rm = TRUE)

# escuchado ----------------------------------------------------


data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_26 = case_when( 
      as.numeric(P5_26) ==  1  | as.numeric(P5_26) ==  2 ~ 1 ,
      as.numeric(P5_26) ==  3  | as.numeric(P5_26) ==  4~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_26)

mean(data2plot$P5_26, na.rm = TRUE)

# sentencia ----------------------------------------------------


data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_26B = case_when( 
      as.numeric(P5_26B) ==  1  | as.numeric(P5_26B) ==  2 ~ 1 ,
      as.numeric(P5_26B) ==  4  | as.numeric(P5_26B) ==  3 ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_26B)

mean(data2plot$P5_26B, na.rm = TRUE)

# Proceso justo ----------------------------------------------------
table(Main_database$P5_26A)

data2plot <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP >=  1) %>% 
  mutate(
    P5_26A = case_when( 
      as.numeric(P5_26A) ==  1   ~ 1 ,
      as.numeric(P5_26A) ==  0   ~  0,
      T ~ NA_real_
    )
  ) %>% 
  select(P5_26A)

mean(data2plot$P5_26A, na.rm = TRUE)
