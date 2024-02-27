## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL Hypothesis
##
## Script:            Punchline exploration
##
## Author(s):         Marcelo Torres           (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Feb 24th, 2024
##
## This version:      Feb 26th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


source("National/Data_cleaning/Code/settings.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

library(stargazer)

#Preparing all variables


Main_database %<>% mutate(
  PJ_1 = case_when(P5_22_02 == 2 ~ 0,
                   P5_22_02 == 1 ~ 1,
                   T ~ NA_integer_),
  PJ_2 = case_when(P5_22_01 == 2 ~ 0,
                   P5_22_01 == 1 ~ 1,
                   T ~ NA_integer_),
  PJ_3 = case_when(P4_7 == "04" | P4_7 == "05" ~ 0,
                   P4_6_4 == 1 & (is.na(P4_7) == T | (P4_7 != "04" & P4_7 != "05")) ~ 1,
                   P4_6_4 == 0 ~ 1,
                   T ~ NA_integer_),
  PJ_4 = case_when(P4_1_05 == "2" ~ 0,
                   P4_1_05 == "1" ~ 1,
                   T ~ NA_integer_),
  PJ_5 = case_when(P5_25 == 1 ~ 0,
                   P5_25 == 2 ~ 1,
                   T ~ NA_integer_),
  PJ_6 = case_when((P3_20 == "06" | P3_20 == "07" | P3_20 == "08" | P3_20 == "09") &
                     (P3_17_08 == 1 | (is.na(P3_17_08) == T | P3_19 != "01" & P3_19 != "02" & P3_19 !="03")) ~ 0,
                   (P3_20 == "06" | P3_20 == "07" | P3_20 == "08" | P3_20 == "09") & 
                     (is.na(P3_17_08) == T | P3_17_08 != 1) & (P3_19 == "01" | P3_19 == "02" | P3_19 =="03") ~ 1,
                   P3_20 == "01" | P3_20 == "02" | P3_20 == "03" | P3_20 == "04" | P3_20 == "05" ~ 1,
                   T ~ NA_integer_),
  PJ_7 = case_when(P5_16_2 == 3 | P5_16_2 == 4 ~ 0,
                   P5_16_2 == 1 | P5_16_2 == 2 ~ 1,
                   T ~ NA_integer_),
  GDH_1 = case_when(tortura_generalizada == 1 ~ 0,
                    tortura_generalizada == 0 ~ 1,
                   T ~ NA_integer_),
  GDH_2 = case_when(det_ninguna == 1 ~ 0,
                   det_ninguna == 0 ~ 1,
                   T ~ NA_integer_),
  GDH_3 = case_when(PPO == 1 ~ 0,
                    PPO == 0 ~ 1,
                   T ~ NA_integer_)) %>%
  mutate( 
    PJ_1 = case_when(
      sentenciado == 1 ~ PJ_1,
      T ~ NA_integer_),
    PJ_2 = case_when(
      sentenciado == 1 ~ PJ_2,
      T ~ NA_integer_),
    PJ_3 = case_when(
      sentenciado == 1 ~ PJ_3,
      T ~ NA_integer_),
    PJ_4 = case_when(
      sentenciado == 1 ~ PJ_4,
      T ~ NA_integer_),
    PJ_5 = case_when(
      sentenciado == 1 ~ PJ_5,
      T ~ NA_integer_),
    PJ_6 = case_when(
      sentenciado == 1 ~ PJ_6,
      T ~ NA_integer_),
    PJ_7 = case_when(
      sentenciado == 1 ~ PJ_7,
      T ~ NA_integer_),
    GDH_1 = case_when(
        sentenciado == 1 ~ GDH_1,
        T ~ NA_integer_),
    GDH_2 = case_when(
          sentenciado == 1 ~ GDH_2,
          T ~ NA_integer_),
    GDH_3 = case_when(
      sentenciado == 1 ~ GDH_3,
      T ~ NA_integer_),
    )
  



# Proceso Justo

Main_database %<>% mutate(Indice_PJ = PJ_1*PJ_2*PJ_3*PJ_4*PJ_5*PJ_6*PJ_7)

table(Main_database$sentenciado)
select(Main_database,Indice_PJ,PJ_1,PJ_2,PJ_3,PJ_4,PJ_5,PJ_6,PJ_7) %>% data.frame() %>% stargazer(summary = T, type = 'text')

table(Main_database$P4_4,useNA = 'always')
table(Main_database$P4_5,useNA = 'always')
table(Main_database$P4_6_4,useNA = 'always')
table(Main_database$P4_7,useNA = 'always')

# Uso Arbitrario de la Autoridad



# Garantías de Derechos Humanos

Main_database %<>% mutate(Indice_GDH = GDH_1*GDH_2*GDH_3)

select(Main_database,Indice_GDH,GDH_1,GDH_2,GDH_3,) %>% data.frame() %>% stargazer(summary = T, type = 'text')



# General

Main_database %<>% mutate(Indice_Punchline = Indice_PJ*Indice_GDH)

select(Main_database,Indice_Punchline,Indice_PJ,Indice_GDH) %>% data.frame() %>% stargazer(summary = T, type = 'text')
