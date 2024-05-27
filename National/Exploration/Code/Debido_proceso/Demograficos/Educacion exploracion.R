## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Education variable definition
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres            (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     February 05th, 2024
##
## This version:      March 19th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Pre-settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:

source("National/Data_cleaning/Code/settings.R")
library(clipr)


load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Analysis                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Alternativas


Main_database <- Main_database %>%
  mutate(Educacion_alt_1 = case_when(Escolaridad == "Ninguno" ~ 0, 
                                           Escolaridad == "Preescolar" ~ 0,
                                           Escolaridad == "Primaria" ~ 0,
                                           Escolaridad == "Secundaria" ~ 0,
                                           Escolaridad == "Preparatoria o bachillerato" ~ 0,
                                           Escolaridad == "Normal básica con secundaria" ~ 0,
                                           Escolaridad == "Carrera técnica con secundaria" ~0,
                                           Escolaridad == "Carrera técnica con preparatoria" ~ 1,
                                           Escolaridad == "Licenciatura o profesional" ~ 1,
                                           Escolaridad == "Maestría o doctorado" ~ 1,
                                           T ~ NA),
         Educacion_alt_2 = case_when(Escolaridad == "Ninguno" ~ 0, 
                                     Escolaridad == "Preescolar" ~ 0,
                                     Escolaridad == "Primaria" ~ 0,
                                     Escolaridad == "Secundaria" ~ 1,
                                     Escolaridad == "Preparatoria o bachillerato" ~ 1,
                                     Escolaridad == "Normal básica con secundaria" ~ 1,
                                     Escolaridad == "Carrera técnica con secundaria" ~ 1,
                                     Escolaridad == "Carrera técnica con preparatoria" ~ 1,
                                     Escolaridad == "Licenciatura o profesional" ~ 1,
                                     Escolaridad == "Maestría o doctorado" ~ 1,
                                     T ~ NA),
         Educacion_alt_3 = case_when(Escolaridad == "Ninguno" ~ 0, 
                                     Escolaridad == "Preescolar" ~ 0,
                                     Escolaridad == "Primaria" ~ 1,
                                     Escolaridad == "Secundaria" ~ 1,
                                     Escolaridad == "Preparatoria o bachillerato" ~ 1,
                                     Escolaridad == "Normal básica con secundaria" ~ 1,
                                     Escolaridad == "Carrera técnica con secundaria" ~ 1,
                                     Escolaridad == "Carrera técnica con preparatoria" ~ 1,
                                     Escolaridad == "Licenciatura o profesional" ~ 1,
                                     Escolaridad == "Maestría o doctorado" ~ 1,
                                     T ~ NA),
         ) %>%
  mutate(
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
      T ~ NA_real_)) %>%
  filter(sentenciado == 1) %>% 
  rowwise() %>% 
  mutate(indicator_GDH = mean(c_across(c(GDH_1, GDH_2)), na.rm = TRUE),
         indicator_UAA = mean(c_across(c(UAA_1, UAA_2, UAA_3, UAA_4)), na.rm = TRUE),
         indicator_PJ = mean(c_across(c(PJ_1, PJ_2,PJ_3,PJ_4,PJ_5,PJ_6,PJ_7 )), na.rm = TRUE),
         indicator_general = mean(c_across(c(GDH_1, GDH_2,
                                             UAA_1, UAA_2, UAA_3, UAA_4,
                                             PJ_1, PJ_2,PJ_3,PJ_4,PJ_5,PJ_6,PJ_7)), na.rm = TRUE),
         indicator_GDH = round(indicator_GDH, 1),
         indicator_UAA = round(indicator_UAA, 1),
         indicator_PJ = round(indicator_PJ, 1),
         indicator_general = round(indicator_general, 1)) %>% 
  mutate(indicator_general_minlimit = case_when(indicator_general <= 0.5 ~ 1,
                                                indicator_general > 0.5 ~ 0,
                                                T ~ NA_real_),
         indicator_GDH_minlimit = case_when(indicator_GDH < 0.5 ~ 1, # este no incluye el 50%
                                            indicator_GDH > 0.5 ~ 0,
                                            T ~ NA_real_),
         indicator_UAA_minlimit = case_when(indicator_UAA <= 0.5 ~ 1,
                                            indicator_UAA > 0.5 ~ 0,
                                            T ~ NA_real_),
         indicator_PJ_minlimit = case_when(indicator_PJ <= 0.5 ~ 1,
                                           indicator_PJ > 0.5 ~ 0,
                                           T ~ NA_real_),
         indicator_general_maxlimit = case_when(indicator_general >= 0.9 ~ 1,
                                                indicator_general < 0.9 ~ 0,
                                                T ~ NA_real_),
         indicator_GDH_maxlimit = case_when(indicator_GDH >= 0.9 ~ 1,
                                            indicator_GDH < 0.9 ~ 0,
                                            T ~ NA_real_),
         indicator_UAA_maxlimit = case_when(indicator_UAA >= 0.9 ~ 1,
                                            indicator_UAA < 0.9 ~ 0,
                                            T ~ NA_real_),
         indicator_PJ_maxlimit = case_when(indicator_PJ >= 0.9 ~ 1,
                                           indicator_PJ < 0.9 ~ 0,
                                           T ~ NA_real_))




#Compare definitions


table(Main_database$Educacion_obligatoria,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Escolaridad,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Escolaridad, Main_database$Educacion_obligatoria,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$P1_19,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Estado_arresto, Main_database$Educacion_obligatoria,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Delito_unico_categ, Main_database$Educacion_obligatoria,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$tortura, Main_database$Educacion_obligatoria,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Sexo, Main_database$Educacion_obligatoria,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Colo_piel_claro, Main_database$Educacion_obligatoria,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Ingreso, Main_database$Educacion_obligatoria,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Edad_arresto, Main_database$Educacion_obligatoria,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Etnia, Main_database$Educacion_obligatoria,useNA = "always") %>% data.frame() %>% write_clip()


table(Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Escolaridad,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Escolaridad, Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$P1_19,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Estado_arresto, Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Delito_unico_categ, Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$tortura, Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Sexo, Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Colo_piel_claro, Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Ingreso, Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Edad_arresto, Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Etnia, Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()



table(Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Escolaridad,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Escolaridad, Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$P1_19,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Estado_arresto, Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Delito_unico_categ, Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$tortura, Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Sexo, Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Colo_piel_claro, Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Ingreso, Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Edad_arresto, Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Etnia, Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()


table(Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Escolaridad,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Escolaridad, Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$P1_19,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Estado_arresto, Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Delito_unico_categ, Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$tortura, Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Sexo, Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Colo_piel_claro, Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Ingreso, Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Edad_arresto, Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$Etnia, Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()


table(Main_database$indicator_PJ, Main_database$Educacion_alt_1,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$indicator_PJ, Main_database$Educacion_alt_2,useNA = "always") %>% data.frame() %>% write_clip()
table(Main_database$indicator_PJ, Main_database$Educacion_alt_3,useNA = "always") %>% data.frame() %>% write_clip()

# Distributions

g1data <- Main_database %>% 
  group_by(Escolaridad) %>%
  summarise(Tortura = mean(tortura, na.rm = T),Proceso_Justo = mean(indicator_PJ, na.rm = T),
            Tortura_l = round(mean(tortura, na.rm = T),2),Proceso_Justo_l = round(mean(indicator_PJ, na.rm = T),2)) %>%
  mutate(Escolaridad = factor(Escolaridad,levels=c("Ninguno","Preescolar","Primaria","Secundaria","Preparatoria o bachillerato",
                                                   "Normal básica con secundaria","Carrera técnica con secundaria",
                                                   "Carrera técnica con preparatoria","Licenciatura o profesional","Maestría o doctorado",
                                                   "NA"))) %>%
  filter(Escolaridad =="Ninguno" | 
           Escolaridad =="Preescolar"|
           Escolaridad =="Primaria"|
           Escolaridad =="Secundaria"|
           Escolaridad =="Preparatoria o bachillerato"|
           Escolaridad =="Licenciatura o profesional")

ggplot(g1data, aes(x=Escolaridad,y=Tortura,label=Tortura_l,colour="blue")) +
  geom_bar(stat = "identity") +
  geom_text() +
  coord_flip() +
  expand_limits(y = 1)

ggplot(g1data, aes(x=Escolaridad,y=Proceso_Justo,label=Proceso_Justo_l,colour="blue")) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  geom_text() +
  expand_limits(y = 1)



