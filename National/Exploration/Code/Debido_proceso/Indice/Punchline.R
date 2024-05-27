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
                   T ~ NA_real_),
  PJ_2 = case_when(P5_22_01 == 2 ~ 0,
                   P5_22_01 == 1 ~ 1,
                   T ~ NA_real_),
  PJ_3 = case_when(P4_7 == "04" | P4_7 == "05" ~ 0,
                   (P4_4 == "2" & P4_5 == "2") ~ 1,
                   P4_6_4 == 1 & (is.na(P4_7) == T | (P4_7 != "04" & P4_7 != "05")) ~ 1,
                   P4_6_4 == 0 ~ 1,
                   T ~ NA_real_),
  PJ_4 = case_when(P4_1_05 == "2" ~ 0,
                   P4_1_05 == "1" ~ 1,
                   T ~ NA_real_),
  PJ_5 = case_when(P5_25 == 1 | P5_25 == 3 ~ 0,
                   P5_25 == 2 ~ 1,
                   T ~ NA_real_),
  PJ_6 = case_when((P3_20 == "06" | P3_20 == "07" | P3_20 == "08" | P3_20 == "09") &
                     (P3_17_08 == 1 | (is.na(P3_17_08) == T | P3_19 != "01" & P3_19 != "02" & P3_19 !="03" & P3_19 !="13")) ~ 0,
                   (P3_20 == "06" | P3_20 == "07" | P3_20 == "08" | P3_20 == "09") & 
                     (is.na(P3_17_08) == T | P3_17_08 != 1) & (P3_19 == "01" | P3_19 == "02" | P3_19 =="03" | P3_19 !="13") ~ 1,
                   P3_20 == "01" | P3_20 == "02" | P3_20 == "03" | P3_20 == "04" | P3_20 == "05" ~ 1,
                   T ~ NA_real_),
  PJ_7 = case_when(P5_16_2 == 3 | P5_16_2 == 4 ~ 0,
                   P5_16_2 == 1 | P5_16_2 == 2 ~ 1,
                   T ~ NA_real_),
  UAA_1 = case_when(proporcionalidad_uso_fuerza == 1 ~ 1,
                    proporcionalidad_uso_fuerza == 0 ~ 0,
                    T ~ NA_real_),
  UAA_2 = case_when(P3_21_1 == 1 | P3_21_2 == 1 ~ 0,
                    (P3_21_1 == 2 & P3_21_2 == 2) | (is.na(P3_21_1) == T & P3_21_2 == 2) | (P3_21_1 == 2 & is.na(P3_21_2) == T) ~ 1,
                    T ~ NA_real_),
  UAA_3 = case_when(P4_15_1 == 1 | P4_15_3 == 1 ~ 0,
                    (P4_15_1 == 2 & P4_15_3 == 2) | (is.na(P4_15_1) == T & P4_15_3 == 2) | (P4_15_1 == 2 & is.na(P4_15_3) == T) ~ 1,
                    T ~ NA_real_),
  UAA_4 = case_when(P5_45_1 == 1 | P5_45_3 == 1 ~ 0,
                    (P5_45_1 == 2 & P5_45_3 == 2) | (is.na(P5_45_1) == T & P5_45_3 == 2) | (P5_45_1 == 2 & is.na(P5_45_3) == T) ~ 1,
                    T ~ NA_real_),
  GDH_1 = case_when(tortura_generalizada == 1 ~ 0,
                    tortura_generalizada == 0 ~ 1,
                   T ~ NA_real_),
  GDH_2 = case_when(det_ninguna == 1 ~ 0,
                   det_ninguna == 0 ~ 1,
                   T ~ NA_real_),
  GDH_3 = case_when(PPO == 1 ~ 0,
                    PPO == 0 ~ 1,
                   T ~ NA_real_)) %>%
  mutate( 
    PJ_1 = case_when(
      sentenciado == 1 ~ PJ_1,
      T ~ NA_real_),
    PJ_2 = case_when(
      sentenciado == 1 ~ PJ_2,
      T ~ NA_real_),
    PJ_3 = case_when(
      sentenciado == 1 ~ PJ_3,
      T ~ NA_real_),
    PJ_4 = case_when(
      sentenciado == 1 ~ PJ_4,
      T ~ NA_real_),
    PJ_5 = case_when(
      sentenciado == 1 ~ PJ_5,
      T ~ NA_real_),
    PJ_6 = case_when(
      sentenciado == 1 ~ PJ_6,
      T ~ NA_real_),
    PJ_7 = case_when(
      sentenciado == 1 ~ PJ_7,
      T ~ NA_real_),
    UAA_1 = case_when(
      sentenciado == 1 ~ UAA_1,
      T ~ NA_real_),
    UAA_2 = case_when(
      sentenciado == 1 ~ UAA_2,
      T ~ NA_real_),
    UAA_3 = case_when(
      sentenciado == 1 ~ UAA_3,
      T ~ NA_real_),
    UAA_4 = case_when(
      sentenciado == 1 ~ UAA_4,
      T ~ NA_real_),
    GDH_1 = case_when(
        sentenciado == 1 ~ GDH_1,
        T ~ NA_real_),
    GDH_2 = case_when(
          sentenciado == 1 ~ GDH_2,
          T ~ NA_real_),
    GDH_3 = case_when(
      sentenciado == 1 ~ GDH_3,
      T ~ NA_real_)
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


Main_database %<>% mutate(Indice_UAA = UAA_1*UAA_2*UAA_3*UAA_4)

select(Main_database,Indice_UAA,UAA_1,UAA_2,UAA_3,UAA_4) %>% data.frame() %>% stargazer(summary = T, type = 'text')


# Garantías de Derechos Humanos

Main_database %<>% mutate(Indice_GDH = GDH_1*GDH_2*GDH_3)

select(Main_database,Indice_GDH,GDH_1,GDH_2,GDH_3) %>% data.frame() %>% stargazer(summary = T, type = 'text')



# General

Main_database %<>% mutate(Indice_Punchline = Indice_PJ*Indice_UAA*Indice_GDH)

select(Main_database,Indice_Punchline,Indice_PJ,Indice_UAA,Indice_GDH) %>% data.frame() %>% stargazer(summary = T, type = 'text')



# Alternativa

Main_database %<>% mutate(Indice_GDH_alt = 100*(GDH_1+GDH_2+GDH_3)/3,
                          Indice_UAA_alt = 100*(UAA_1+UAA_2+UAA_3+UAA_4)/4,
                          Indice_PJ_alt = 100*(PJ_1+PJ_2+PJ_3+PJ_4+PJ_5+PJ_6+PJ_7)/7,
                          Indice_Punchline_alt = 100*(PJ_1+PJ_2+PJ_3+PJ_4+PJ_5+PJ_6+PJ_7+UAA_1+UAA_2+UAA_3+UAA_4+GDH_1+GDH_2+GDH_3)/14)


select(Main_database,Indice_Punchline_alt,Indice_PJ_alt,Indice_UAA_alt,Indice_GDH_alt) %>% data.frame() %>% stargazer(summary = T, type = 'text')


# NA analysis

NA_counts_PJ <- Main_database %>% filter(sentenciado == 1) %>% select(PJ_1,PJ_2,PJ_3,PJ_4,PJ_5,PJ_6,PJ_7)
table(rowSums(is.na(NA_counts_PJ)))

NA_counts_UAA <- Main_database %>% filter(sentenciado == 1) %>% select(UAA_1,UAA_2,UAA_3,UAA_4)
table(rowSums(is.na(NA_counts_UAA)))

NA_counts_GDH <- Main_database %>% filter(sentenciado == 1) %>% select(GDH_1,GDH_2,GDH_3)
table(rowSums(is.na(NA_counts_GDH)))


# NA robust versions 


Main_database %<>% mutate(Indice_GDH_r = prod(GDH_1,GDH_2,GDH_3,na.rm = T),
                          Indice_UAA_r = prod(UAA_1,UAA_2,UAA_3,UAA_4,na.rm = T),
                          Indice_PJ_r = prod(PJ_1,PJ_2,PJ_3,PJ_4,PJ_5,PJ_6,PJ_7,na.rm = T),
                          Indice_Punchline_r = prod(PJ_1,PJ_2,PJ_3,PJ_4,PJ_5,PJ_6,PJ_7,UAA_1,UAA_2,UAA_3,UAA_4,GDH_1,GDH_2,GDH_3,na.rm = T))

# Note: The mean() function yields a weird error, so i use another option
Main_database %<>% mutate(GDH_NAs = rowSums(is.na(across(GDH_1:GDH_3))),
                          UAA_NAs = rowSums(is.na(across(UAA_1:UAA_4))),
                          PJ_NAs = rowSums(is.na(across(PJ_1:PJ_7))),
                          Indice_Punchline_NAs = rowSums(is.na(across(PJ_1:GDH_3))))

Main_database %<>% mutate(Indice_GDH_alt_r = 100*sum(GDH_1,GDH_2,GDH_3,na.rm = T)/(3-GDH_NAs),
                          Indice_UAA_alt_r = 100*sum(UAA_1,UAA_2,UAA_3,UAA_4,na.rm = T)/(4-UAA_NAs),
                          Indice_PJ_alt_r = 100*sum(PJ_1,PJ_2,PJ_3,PJ_4,PJ_5,PJ_6,PJ_7,na.rm = T)/(7-PJ_NAs),
                          Indice_Punchline_alt_r = 100*sum(PJ_1,PJ_2,PJ_3,PJ_4,PJ_5,PJ_6,PJ_7,UAA_1,UAA_2,UAA_3,GDH_1,GDH_2,GDH_3,na.rm = T)/(14-Indice_Punchline_NAs))

prueba<- Main_database[133,1249:1270]

filter(Main_database,sentenciado == 1) %>% select(Indice_Punchline_r,Indice_PJ_r,Indice_UAA_r,Indice_GDH_r) %>% data.frame() %>% stargazer(summary = T, type = 'text')

filter(Main_database,sentenciado == 1) %>% select(Indice_Punchline_alt_r,Indice_PJ_alt_r,Indice_UAA_alt_r,Indice_GDH_alt_r) %>% data.frame() %>% stargazer(summary = T, type = 'text')


# Sólo nuevo sistema

Data_NS <- Main_database %>% filter(NSJP==1)


filter(Main_database,sentenciado == 1) %>% select(Indice_Punchline,Indice_Punchline_alt,Indice_Punchline_r,Indice_Punchline_alt_r,
                                                  Indice_PJ,Indice_PJ_alt,Indice_PJ_r,Indice_PJ_alt_r,
                                                  Indice_UAA,Indice_UAA_alt,Indice_UAA_r,Indice_UAA_alt_r,
                                                  Indice_GDH,Indice_GDH_alt,Indice_GDH_r,Indice_GDH_alt_r,
                                                  PJ_1,PJ_2,PJ_3,PJ_4,PJ_5,PJ_6,PJ_7,UAA_1,UAA_2,UAA_3,UAA_4,GDH_1,GDH_2,GDH_3) %>% data.frame() %>% stargazer(summary = T, type = 'text')

