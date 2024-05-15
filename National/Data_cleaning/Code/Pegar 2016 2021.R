## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Append ENPOL 2016
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres            (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     May 10th, 2024
##
## This version:      May 12th, 2024
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

options(max.print=2000)

source("National/Data_cleaning/Code/settings.R")
library(haven)


load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Load old ENPOL                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






#Rename and create variables we will be working with


old_enpol1 <- read_dta(paste0(path2DB,"/National/Data_cleaning/Input/ENPOL_2016_Completa.dta")) %>% 
  select(id_per,starts_with("p1"))


old_enpol2 <- read_dta(paste0(path2DB,"/National/Data_cleaning/Input/ENPOL 2016_GIZ_work_20181015.dta")) %>%  
  mutate(Delito_gr_1_robos = case_when( p5_8_1 == 1 | p5_29_1 == 1 ~ 1,
                                        p5_8_2 == 1 | p5_29_2 == 1 ~ 1,
                                        p5_8_3 == 1 | p5_29_3 == 1 ~ 1,
                                        p5_8_4 == 1 | p5_29_4 == 1 ~ 1,
                                        p5_8_5 == 1 | p5_29_5 == 1 ~ 1,
                                        p5_8_6 == 1 | p5_29_6 == 1 ~ 1,
                                        T ~ 0),
         Delito_gr_2_drogas = case_when( p5_8_7 == 1 | p5_29_7 == 1 ~ 1,
                                         p5_8_8 == 1 | p5_29_8 == 1 ~ 1,
                                         T ~ 0),
         Delito_gr_3_del_org = NA_real_,
         Delito_gr_4_lesiones = case_when( p5_8_9 == 1 | p5_29_9 == 1 ~ 1,
                                           T ~ 0),
         Delito_gr_5_hom_cul = NA_real_,
         Delito_gr_6_hom_dol = NA_real_,
         Delito_gr_7_armas = case_when( p5_8_11 == 1 | p5_29_11 == 1 ~ 1,
                                        T ~ 0),
         Delito_gr_8_viol_fam = case_when( p5_8_13 == 1 | p5_29_13 == 1 ~ 1,
                                           T ~ 0),
         Delito_gr_9_secuestro = case_when( p5_8_15 == 1 | p5_29_15 == 1 ~ 1,
                                            p5_8_22 == 1 | p5_29_22 == 1 ~ 1,
                                            T ~ 0),
         Delito_gr_10_sexuales = case_when( p5_8_16 == 1 | p5_29_16 == 1 ~ 1,
                                            p5_8_18 == 1 | p5_29_18 == 1 ~ 1,
                                            T ~ 0),
         Delito_gr_11_extorsion = case_when( p5_8_19 == 1 | p5_29_19 == 1 ~ 1,
                                             T ~ 0),
         Delito_gr_12_fraude = case_when( p5_8_17 == 1 | p5_29_17 == 1 ~ 1,
                                          p5_8_23 == 1 | p5_29_23 == 1 ~ 1,
                                          T ~ 0),
         Delito_gr_13_amenazas = case_when( p5_8_24 == 1 | p5_29_24 == 1 ~ 1,
                                            T ~ 0),
         Delito_gr_14_otro = NA_real_,
         Delito_gr_15_ns_nr = case_when( p5_8_98 == 1 | p5_29_98 == 1 ~ 1,
                                         p5_8_99 == 1 | p5_29_99 == 1 ~ 1,
                                         T ~ 0),
         Delito_gr_16_hom_2016 = case_when( p5_8_10 == 1 | p5_29_10 == 1 ~ 1,
                                            T ~ 0),
         Delito_gr_17_otro_2016 = case_when( p5_8_12 == 1 | p5_29_12 == 1 ~ 1,
                                             p5_8_14 == 1 | p5_29_14 == 1 ~ 1,
                                             p5_8_20 == 1 | p5_29_20 == 1 ~ 1,
                                             p5_8_21 == 1 | p5_29_21 == 1 ~ 1,
                                             p5_8_25 == 1 | p5_29_25 == 1 ~ 1,
                                            T ~ 0)) %>%
  rowwise() %>% 
  mutate(Delito_unico = ifelse(sum(c(Delito_gr_1_robos,
                                     Delito_gr_2_drogas,
                                     Delito_gr_3_del_org,
                                     Delito_gr_4_lesiones,
                                     Delito_gr_7_armas, 
                                     Delito_gr_8_viol_fam,
                                     Delito_gr_9_secuestro,
                                     Delito_gr_10_sexuales,
                                     Delito_gr_11_extorsion,
                                     Delito_gr_12_fraude,
                                     Delito_gr_13_amenazas,
                                     Delito_gr_15_ns_nr,
                                     Delito_gr_16_hom_2016,
                                     Delito_gr_17_otro_2016
                                     )) == 1, 1, 0)) %>%
  mutate(Delito_unico_1_robos = case_when(Delito_gr_1_robos == 1 
                                          & Delito_unico == 1 ~ 1, 
                                          T ~ 0),
         Delito_unico_2_drogas = case_when(Delito_gr_2_drogas == 1 
                                           & Delito_unico == 1 ~ 1,
                                           T ~ 0),
         Delito_unico_3_del_org = case_when(Delito_gr_3_del_org == 1 ~ 1,
                                            T ~ 0),
         Delito_unico_4_lesiones = case_when(Delito_gr_4_lesiones == 1 
                                             & Delito_unico == 1 ~ 1,
                                             T ~ 0),
         Delito_unico_7_armas = case_when(Delito_gr_7_armas == 1
                                          & Delito_unico == 1 ~ 1,
                                          T ~ 0),
         Delito_unico_8_viol_fam = case_when(Delito_gr_8_viol_fam == 1 
                                             & Delito_unico == 1 ~ 1,
                                             T ~ 0),
         Delito_unico_9_secuestro = case_when(Delito_gr_9_secuestro == 1 
                                              & Delito_unico == 1 ~ 1,
                                              T ~ 0),
         Delito_unico_10_sexuales = case_when(Delito_gr_10_sexuales == 1 
                                              & Delito_unico == 1 ~ 1,
                                              T ~ 0),
         Delito_unico_11_extorsion = case_when(  Delito_gr_11_extorsion == 1 
                                                 & Delito_unico == 1 ~ 1,
                                                 T ~ 0),
         Delito_unico_12_fraude = case_when(  Delito_gr_12_fraude == 1 
                                              & Delito_unico == 1 ~ 1,
                                              T ~ 0),
         Delito_unico_13_amenazas = case_when(  Delito_gr_13_amenazas == 1 
                                                & Delito_unico == 1 ~ 1,
                                                T ~ 0),
         Delito_unico_15_ns_nr = case_when(  Delito_gr_15_ns_nr == 1 
                                             & Delito_unico == 1 ~ 1,
                                             T ~ 0),
         Delito_unico_16_hom_2016 = case_when(  Delito_gr_16_hom_2016 == 1 
                                             & Delito_unico == 1 ~ 1,
                                             T ~ 0),
         Delito_unico_17_otro_2016 = case_when(  Delito_gr_17_otro_2016 == 1 
                                             & Delito_unico == 1 ~ 1,
                                             T ~ 0))  



old_enpol <- left_join(old_enpol1, old_enpol2, by = "id_per") %>%
  mutate(
    Delito_unico_categ = case_when(
      Delito_unico_1_robos == 1 ~ "robos",
      Delito_unico_2_drogas == 1 ~ "drogas",
      Delito_unico_3_del_org == 1 ~ "org",
      Delito_unico_4_lesiones == 1 ~ "lesiones",
      Delito_unico_7_armas == 1 ~ "armas",
      Delito_unico_8_viol_fam == 1 ~ "viol_fam",
      Delito_unico_9_secuestro == 1 ~ "secuestro",
      Delito_unico_10_sexuales == 1 ~ "sexuales",
      Delito_unico_11_extorsion == 1 ~ "extorsion",
      Delito_unico_12_fraude == 1 ~ "fraude",
      Delito_unico_13_amenazas == 1 ~ "amenazas",
      Delito_unico_15_ns_nr == 1 ~ "ns_nr",
      Delito_unico_16_hom_2016 == 1 ~ "homicidio(2016)",
      Delito_unico_17_otro_2016 == 1 ~ "otro(2016)",
      TRUE ~ NA_character_
    )) %>%
  rename(
    Anio_arresto = anio_arresto
  ) %>%
  mutate(
    Color_piel_claro = NA_real_,
    LGBTQ = NA_real_,
    Etnia = NA_real_,
    discapacidad = NA_real_,
    P5_4_A = as.character(p5_4_a),
    tortura_tra = as.character(tortura_arresto),
    tortura_mp = as.numeric(tortura_MP),
    tortura_generalizada = as.numeric(tortura_total),
    vulnerabilidad_economica = case_when(p2_18_1 == 1 | p2_18_3 == 1 | p2_18_6 == 1 ~ 0,
                                         p2_18_1 == 2 & p2_18_3 == 2 & p2_18_6 == 2 ~ 1,
                                         T ~ NA_real_),
    Estado_arresto = case_when(p3_4 == 01 ~ "Aguascalientes",
                     p3_4 == 02 ~ "Baja California" ,
                     p3_4 == 03 ~ "Baja California Sur",
                     p3_4 == 04 ~ "Campeche" ,
                     p3_4 == 05 ~ "Coahuila de Zaragoza",
                     p3_4 == 06 ~ "Colima" ,
                     p3_4 == 07 ~ "Chiapas" ,
                     p3_4 == 08 ~ "Chihuahua" ,
                     p3_4 == 09 ~ "Distrito Federal" ,
                     p3_4 == 10 ~ "Durango" ,
                     p3_4 == 11 ~ "Guanajuato" ,
                     p3_4 == 12 ~ "Guerrero" ,
                     p3_4 == 13 ~ "Hidalgo" ,
                     p3_4 == 14 ~ "Jalisco" ,
                     p3_4 == 15 ~ "México" ,
                     p3_4 == 16 ~ "Michoacán de Ocampo" ,
                     p3_4 == 17 ~ "Morelos" ,
                     p3_4 == 18 ~ "Nayarit" ,
                     p3_4 == 19 ~ "Nuevo León" ,
                     p3_4 == 20 ~ "Oaxaca" ,
                     p3_4 == 21 ~ "Puebla" ,
                     p3_4 == 22 ~ "Querétaro" ,
                     p3_4 == 23 ~ "Quintana Roo" ,
                     p3_4 == 24 ~ "San Luis Potosí" ,
                     p3_4 == 25 ~ "Sinaloa" ,
                     p3_4 == 26 ~ "Sonora" ,
                     p3_4 == 27 ~ "Tabasco" ,
                     p3_4 == 28 ~ "Tamaulipas" ,
                     p3_4 == 29 ~ "Tlaxcala" ,
                     p3_4 == 30 ~ "Veracruz de Ignacio de la Llave" ,
                     p3_4 == 31 ~ "Yucatán" ,
                     p3_4 == 32 ~ "Zacatecas" ,
                     p3_4 == 97 ~ NA_character_ ,
                     p3_4 == 99 ~ NA_character_ ,
                     T ~ NA_character_),
    Educacion_superior = case_when(p1_24_n == 98 | p1_24_n == 99 ~ NA_real_,
            p1_24_n == 0 ~ 0,
            p1_24_n == 1 ~ 0,
            p1_24_n == 2 ~ 0,
            p1_24_n == 3 ~ 0,
            p1_24_n == 4 ~ 0,
            p1_24_n == 5 ~ 0,
            p1_24_n == 6 ~ 0,
            p1_24_n == 7 ~ 0,
            p1_24_n == 8 ~ 1,
            p1_24_n == 9 ~ 1),
    Sexo = case_when(sexo == 1 ~ "Masculino",
                     sexo == 2 ~ "Femenino"),
    Eda_1 = as.numeric(p1_2) - (as.numeric(p3_6_a) - 2016),
    Edad_arresto = case_when(p1_2 < 98 ~ Eda_1,
                             T ~ NA_real_),
    Edad_menor30 = case_when(Edad_arresto < 30 ~ 1, 
                             Edad_arresto >= 30 ~ 0,
                             T ~ NA),
    tortura_tipo = case_when(tortura_psi_1 == 1 | tortura_psi_2 == 1 | tortura_psi_3 == 1 | 
                               tortura_psi_4 == 1 | tortura_psi_5 == 1 | tortura_psi_6 == 1 | 
                               tortura_psi_7 == 1 | tortura_psi_8 == 1 | tortura_psi_9 == 1 ~ "Psicológica",
                             tortura_fis_1_NV == 1 | tortura_fis_2_NV == 1 | tortura_fis_3 == 1 | 
                               tortura_fis_4_NV == 1 | tortura_fis_5_NV == 1 | tortura_fis_6 == 1 | 
                               tortura_fis_7_NV == 1 | tortura_fis_8 == 1 | tortura_fis_9 == 1  ~ "Física"),
    fuero = case_when(
      p5_9_1 == 0 & p5_9_2 == 1 ~ "Sólo común",
      p5_9_1 == 1 & p5_9_2 == 0 ~ "Sólo federal",
      p5_9_1 == 1 & p5_9_2 == 1 ~ "Algunos delitos de fuero común y algunos de fuero federal",
      p5_30_1 == 0 & p5_30_2 == 1 ~ "Sólo común",
      p5_30_1 == 1 & p5_30_2 == 0 ~ "Sólo federal",
      p5_30_1 == 1 & p5_30_2 == 1 ~ "Algunos delitos de fuero común y algunos de fuero federal",
      T ~ NA_character_),
    Corporacion_grupos = case_when(p3_2 == 1 ~ "Policía Municipal",
                                   p3_2 == 2 ~ "Policía Estatal",
                                   p3_2 == 3 ~ "Policía Federal",
                                   p3_2 == 4 ~ "Policía Ministerial o Judicial (ENPOL 2016)",
                                   p3_2 == 5 | p3_2 == 6 ~ "Ejército o Marina",
                                   p3_2 == 7 | p3_2 == 8 ~ "Otra (ENPOL 2016)",
                                   p3_2 == 98 | p3_2 == 99 ~ "NS/NR"),
    flagrancia = case_when(
      p3_3 == 1 | p3_3 == 2 ~ 1,
      p3_3 == 3| p3_3 == 4  ~ 0,
      T ~ NA),
    orden_det = case_when(
      p3_3 == 3  ~ 1,
      p3_3 == 1 | p3_10 == 2 | p3_3 == 4 ~ 0,
      T ~ NA),
    inspeccion = NA_real_,
    det_ninguna = case_when(
      p3_3 == 4  ~ 1,
      p3_3 == 1 | p3_3 == 2 | p3_3 == 3 ~ 0,
      T ~ NA),
    ENPOL = 2016
    
     ) %>%
  select(
    id_per,
    NSJP,
    Estado_arresto,
    Anio_arresto,
    Color_piel_claro,
    LGBTQ,
    Edad_arresto,
    Sexo,
    Etnia,
    discapacidad,
    vulnerabilidad_economica,
    tortura_generalizada,
    tortura_tra,
    tortura_mp,
    tortura_tipo,
    fuero,
    P5_4_A,
    Corporacion_grupos,
    Delito_unico_categ,
    Delito_unico,
    Delito_gr_1_robos,
    Delito_gr_2_drogas,
    Delito_gr_3_del_org,
    Delito_gr_4_lesiones,
    Delito_gr_7_armas, 
    Delito_gr_8_viol_fam,
    Delito_gr_9_secuestro,
    Delito_gr_10_sexuales,
    Delito_gr_11_extorsion,
    Delito_gr_12_fraude,
    Delito_gr_13_amenazas,
    Delito_gr_15_ns_nr,
    Delito_gr_16_hom_2016,
    Delito_gr_17_otro_2016,
    flagrancia,
    inspeccion,
    orden_det,
    det_ninguna,
    ENPOL
  )

Main_database <- Main_database %>%
  mutate (ENPOL = 2021) %>%
  bind_rows(., old_enpol)  %>%
  mutate(muestra_mixta = case_when( 
    ENPOL == 2016 & Anio_arresto>=2011 ~ 1,
    ENPOL == 2021 & Anio_arresto>=2017 ~ 1,
    T ~ 0))
  
  