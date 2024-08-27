

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Load Settings & Data                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("National/Visualization/Code/settings.R")

path2DB <- path2SP

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

master_data.df <- Main_database %>% filter(as.numeric(Anio_arresto) >= 2015, NSJP == 1)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## B1                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# B111 (2015-2021)
# TODO BIEN

b111 <- master_data.df %>%
  mutate(MP = case_when(P4_1_03 == "1" ~ 1,
                        P4_1_03 == "2" ~ 0,
                        T ~ NA_real_),
         Juz = case_when(P5_2_1 == "1" ~ 1,
                        P5_2_1 == "2" ~ 0,
                        T ~ NA_real_)) %>%
  group_by(Anio_arresto) %>%
  summarize(CA = round(100*mean(P3_14_4, na.rm = T)),
            MP = round(100*mean(MP, na.rm = T)),
            Juzgado = round(100*mean(Juz, na.rm = T))
            )


# B112 (2015-2021)
# TODO BIEN

b112 <- master_data.df %>%
  mutate(Juez = case_when(P5_17_2 == "1" | P5_37_2 == "1" ~ 1,
                        P5_17_2 == "2" | P5_37_2 == "2" ~ 1,
                        P5_17_2 == "3" | P5_37_2 == "3" ~ 0,
                        P5_17_2 == "4" | P5_37_2 == "4" ~ 0,
                        T ~ NA_real_),
         MP = case_when(P5_17_3 == "1" | P5_37_3 == "1" ~ 1,
                          P5_17_3 == "2" | P5_37_3 == "2" ~ 1,
                          P5_17_3 == "3" | P5_37_3 == "3" ~ 0,
                          P5_17_3 == "4" | P5_37_3 == "4" ~ 0,
                          T ~ NA_real_),
         AD = case_when(P5_17_1 == "1" | P5_37_1 == "1" ~ 1,
                          P5_17_1 == "2" | P5_37_1 == "2" ~ 1,
                          P5_17_1 == "3" | P5_37_1 == "3" ~ 0,
                          P5_17_1 == "4" | P5_37_1 == "4" ~ 0,
                          T ~ NA_real_),
         AV = case_when(P5_17_4 == "1" | P5_37_4 == "1" ~ 1,
                          P5_17_4 == "2" | P5_37_4 == "2" ~ 1,
                          P5_17_4 == "3" | P5_37_4 == "3" ~ 0,
                          P5_17_4 == "4" | P5_37_4 == "4" ~ 0,
                          T ~ NA_real_),) %>%
  group_by(Anio_arresto) %>%
  summarize(Juez = round(100*mean(Juez, na.rm = T)),
            MP = round(100*mean(MP, na.rm = T)),
            AD = round(100*mean(AD, na.rm = T)),
            AV = round(100*mean(AV, na.rm = T)))



# B113 (2015-2021)
# TODO BIEN

b113 <- master_data.df %>%
  mutate(Juz = case_when(P5_2_4 == "1" ~ 1,
                         P5_2_4 == "2" ~ 0,
                         T ~ NA_real_),
         MP = case_when(P4_1_04 == "1" ~ 1,
                        P4_1_04 == "2" ~ 0,
                         T ~ NA_real_)) %>%
  group_by(Anio_arresto) %>%
  summarize(Juz = round(100*mean(Juz, na.rm = T)),
            MP = round(100*mean(MP, na.rm = T)),
            CA = round(100*mean(P3_14_5, na.rm = T)))




# B114 (6m,1a,2a)
# TODO BIEN

b114 <- master_data.df %>%
  mutate(M6 = case_when(P5_10 == "1" ~ 1,
                        P5_10 == "2" ~ 1,
                        P5_10 == "3" ~ 1,
                        P5_10 == "4" ~ 1,
                        P5_10 == "5" ~ 0,
                        P5_10 == "6" ~ 0,
                        P5_10 == "7" ~ 0,
                         T ~ NA_real_),
         M6A1 = case_when(P5_10 == "1" ~ 0,
                        P5_10 == "2" ~ 0,
                        P5_10 == "3" ~ 0,
                        P5_10 == "4" ~ 0,
                        P5_10 == "5" ~ 1,
                        P5_10 == "6" ~ 0,
                        P5_10 == "7" ~ 0,
                        T ~ NA_real_),
         A1A2 = case_when(P5_10 == "1" ~ 0,
                        P5_10 == "2" ~ 0,
                        P5_10 == "3" ~ 0,
                        P5_10 == "4" ~ 0,
                        P5_10 == "5" ~ 0,
                        P5_10 == "6" ~ 1,
                        P5_10 == "7" ~ 0,
                        T ~ NA_real_),
         A2 = case_when(P5_10 == "1" ~ 0,
                        P5_10 == "2" ~ 0,
                        P5_10 == "3" ~ 0,
                        P5_10 == "4" ~ 0,
                        P5_10 == "5" ~ 0,
                        P5_10 == "6" ~ 0,
                        P5_10 == "7" ~ 1,
                        T ~ NA_real_),
         Stat="Nacional"
         ) %>%
  group_by(Stat) %>%
  summarize(M6 = round(100*mean(M6, na.rm = T)),
            M6A1 = round(100*mean(M6A1, na.rm = T)),
            A1A2 = round(100*mean(A1A2, na.rm = T)),
            A2 = round(100*mean(A2, na.rm = T)),)




# B115
# MP BIEN, JUEZ MAL

b115 <- master_data.df %>%
  mutate(DOJ = case_when(P5_1 == "1" ~ 1,
                        P5_1 == "2" ~ 0,
                        T ~ NA_real_),
         DOMP =  case_when(P4_1_05 == "1" ~ 1,
                           P4_1_05 == "2" ~ 0,
                           T ~ NA_real_),
         P5_4_A = as.numeric(P5_4_A),
         P5_4_M = as.numeric(P5_4_M),
         P5_4_A = case_when(
           P5_4_A >= 97 ~ NA_real_,
           T ~ P5_4_A),
         P5_4_M = case_when(
           P5_4_M >= 97 ~ NA_real_,
           T ~ P5_4_M),
         P5_4_M = P5_4_M/12,
         Años = P5_4_A+P5_4_M
  ) 

b1151 <- b115 %>%
  group_by(DOMP) %>%
  filter(!is.na(DOMP)) %>%
  summarize(Años = round(mean(Años, na.rm = T))) 
b1152 <- b115 %>%
  group_by(DOJ) %>%
  filter(!is.na(DOJ)) %>%
  summarize(Años = round(mean(Años, na.rm = T)))
b115 <- bind_rows(b1151, b1152)


#NO HAY B116


# B117 (2015-2021)
# TODO BIEN

b117 <- master_data.df %>%
  mutate(Acceso = case_when(P5_16_5 == "1"  ~ 1,
                            P5_16_5 == "2"  ~ 1,
                            P5_16_5 == "3"  ~ 1,
                            P5_16_5 == "4"  ~ 0,
                          T ~ NA_real_),
         Video = case_when(P5_19_3 == "1"  ~ 1,
                           P5_19_3 == "2"  ~ 0,
                        T ~ NA_real_),) %>%
  group_by(Anio_arresto) %>%
  summarize(Acceso = round(100*mean(Acceso, na.rm = T)),
            Video = round(100*mean(Video, na.rm = T)))


# B118 (2015-2021)
# TODO BIEN

b118 <- master_data.df %>%
  filter(sentenciado == 1) %>%
  mutate(Dist = case_when(P5_14 == "1"  ~ 1,
                          P5_14 == "2"  ~ 0,
                            T ~ NA_real_),
         Culp = case_when(P5_25 == "1"  ~ 0,
                          P5_25 == "2"  ~ 1,
                          T ~ NA_real_),) %>%
  group_by(Anio_arresto) %>%
  summarize(Dist = round(100*mean(Dist, na.rm = T)),
            Culp = round(100*mean(Culp, na.rm = T)))



# B119 (2015-2021)
# TODO BIEN

b119 <- master_data.df %>%
  filter(sentenciado == 1) %>%
  mutate(Pres = case_when(P5_16_2 == "1" | P5_36_2 == "1" ~ 1,
                          P5_16_2 == "2" | P5_36_2 == "2" ~ 0,
                          P5_16_2 == "3" | P5_36_2 == "3" ~ 0,
                          P5_16_2 == "4" | P5_36_2 == "4" ~ 0,
                          T ~ NA_real_),
         Cont = case_when(P5_18 == "1"  ~ 0,
                          P5_18 == "2"  ~ 1,
                          P5_18 == "3"  ~ 0,
                          P5_18 == "4"  ~ 0,
                          P5_18 == "5"  ~ 0,
                          T ~ NA_real_),) %>%
  group_by(Anio_arresto) %>%
  summarize(Pres = round(100*mean(Pres, na.rm = T)),
            Cont = round(100*mean(Cont, na.rm = T)))



# B211 (2015-2021)
# TODO BIEN

b211 <- master_data.df %>%
  group_by(Anio_arresto) %>%
  summarize(Fuerza = round(100*(1-mean(proporcionalidad_uso_fuerza, na.rm = T))))



# B212 (2015-2021)
# TODO BIEN

b212 <- master_data.df %>%
  mutate(Corp = case_when(P3_2 == "09" ~ "OC",
                          P3_2 == "02" ~ "PE",
                          P3_2 == "05" ~ "PFM",
                          P3_2 == "01" ~ "PM",
                          P3_2 == "07" ~ "EM",
                          P3_2 == "08" ~ "EM",
                          P3_2 == "04" ~ "PEM",
                          P3_2 == "03" ~ "PF",
                          T ~ NA_character_)
         ) %>%
  group_by(Corp) %>%
  summarize(Fuerza = round(100*(1-mean(proporcionalidad_uso_fuerza, na.rm = T))))



# B213 (2015-2021)
# TODO BIEN

b213 <- master_data.df %>%
  mutate(Sec = case_when(P5_11_17 == "1" | P5_31_17 == "1" ~ 1,
                         P5_11_17 == "0" | P5_31_17 == "0"  ~ NA_real_,
                          T ~ NA_real_),
         Ext = case_when(P5_11_22 == "1" | P5_31_22 == "1" ~ 1,
                        P5_11_22 == "0" | P5_31_22 == "0"  ~ NA_real_,
                        T ~ NA_real_),
         Por = case_when(P5_11_13 == "1" | P5_31_13 == "1" ~ 1,
                        P5_11_13 == "0" | P5_31_13 == "0"  ~ NA_real_,
                        T ~ NA_real_),
         RV = case_when(P5_11_01 == "1" | P5_31_01 == "1" ~ 1,
                        P5_11_01 == "0" | P5_31_01 == "0"  ~ NA_real_,
                        T ~ NA_real_),
         HD = case_when(P5_11_12 == "1" | P5_31_12 == "1" ~ 1,
                        P5_11_12 == "0" | P5_31_12 == "0"  ~ NA_real_,
                        T ~ NA_real_),
         RA = case_when(P5_11_06 == "1" | P5_31_06 == "1" ~ 1,
                        P5_11_06 == "0" | P5_31_06 == "0"  ~ NA_real_,
                        T ~ NA_real_),
         PCD = case_when(P5_11_08 == "1" | P5_31_08 == "1" ~ 1,
                         P5_11_09 == "1" | P5_31_09 == "1" ~ 1,
                        (P5_11_08 == "0" | P5_31_08 == "0") & (P5_11_09 == "0" | P5_31_09 == "0") ~ NA_real_,
                        T ~ NA_real_),
         Stat="Nacional"
  ) %>%
  group_by(Stat) %>%
  summarize(Sec = round(100*(1-mean(Sec*proporcionalidad_uso_fuerza, na.rm = T))),
            Ext = round(100*(1-mean(Ext*proporcionalidad_uso_fuerza, na.rm = T))),
            Por = round(100*(1-mean(Por*proporcionalidad_uso_fuerza, na.rm = T))),
            RV = round(100*(1-mean(RV*proporcionalidad_uso_fuerza, na.rm = T))),
            HD = round(100*(1-mean(HD*proporcionalidad_uso_fuerza, na.rm = T))),
            RA = round(100*(1-mean(RA*proporcionalidad_uso_fuerza, na.rm = T))),
            PCD = round(100*(1-mean(PCD*proporcionalidad_uso_fuerza, na.rm = T))))




# B221 (2015-2021)
# TODO BIEN

b221 <- master_data.df %>%
  mutate(CA = case_when(P3_21_1 == "1"  ~ 1,
                        P3_21_2 == "1"  ~ 1,
                        P3_21_1 == "2" & P3_21_1 == "2" ~ 0,
                        T ~ NA_real_),
         MP = case_when(P4_15_1 == "1" ~ 1,
                        P4_15_3 == "1" ~ 1,
                        P4_15_1 == "2" & P4_15_1 == "2" ~ 0,
                        T ~ NA_real_),
         Juz = case_when(P5_45_1 == "1" ~ 1,
                         P5_45_3 == "1" ~ 1,
                         P5_45_1 == "2" &  P5_45_3 == "2" ~ 0,
                        T ~ NA_real_),) %>%
  group_by(Anio_arresto) %>%
  summarize(CA = round(100*mean(CA, na.rm = T)),
            MP = round(100*mean(MP, na.rm = T)),
            Juz = round(100*mean(Juz, na.rm = T)))



# B222 (2015-2021)
# TODO BIEN

b222 <- master_data.df %>%
  mutate(LCA = case_when(P3_22_1 == "1" ~ 1,
                        P3_22_1 == "0" ~ 0,
                        T ~ NA_real_),
         ICA = case_when(P3_22_2 == "1" ~ 1,
                         P3_22_3 == "1" ~ 1,
                         P3_22_2 == "0" & P3_22_3 == "0" ~ 0,
                         T ~ NA_real_),
         DCA = case_when(P3_22_4 == "1" ~ 1,
                         P3_22_4 == "0" ~ 0,
                         T ~ NA_real_),
         LMP = case_when(P4_16_1 == "1" ~ 1,
                         P4_16_1 == "0" ~ 0,
                         T ~ NA_real_),
         IMP = case_when(P4_16_2 == "1" ~ 1,
                         P4_16_4 == "1" ~ 1,
                         P4_16_2 == "0" & P4_16_4 == "0" ~ 0,
                         T ~ NA_real_),
         DMP = case_when(P4_16_3 == "1" ~ 1,
                         P4_16_3 == "0" ~ 0,
                         T ~ NA_real_),
         LJU = case_when(P5_46_1 == "1" ~ 1,
                         P5_46_1 == "2" ~ 0,
                         T ~ NA_real_),
         DJU = case_when(P5_46_2 == "1" ~ 1,
                         P5_46_3 == "1" ~ 1,
                         P5_46_4 == "1" ~ 1,
                         P5_46_5 == "1" ~ 1,
                         P5_46_6 == "1" ~ 1,
                         P5_46_2 == "2" & P5_46_3 == "2" & P5_46_4 == "2" &P5_46_5 == "2" & P5_46_6 == "2"  ~ 0,
                         T ~ NA_real_),
         Stat="Nacional"
         ) %>%
  group_by(Stat) %>%
  summarize(LCA = round(100*mean(LCA, na.rm = T)),
            ICA = round(100*mean(ICA, na.rm = T)),
            DCA = round(100*mean(DCA, na.rm = T)),
            LMP = round(100*mean(LMP, na.rm = T)),
            IMP = round(100*mean(IMP, na.rm = T)),
            DMP = round(100*mean(DMP, na.rm = T)),
            LJU = round(100*mean(LJU, na.rm = T)),
            DJU = round(100*mean(DJU, na.rm = T)),)



# B311 (2015-2021)
# TODO BIEN

b311 <- master_data.df %>%
  group_by(Anio_arresto) %>%
  summarize(Tortura = round(100*mean(tortura_generalizada, na.rm = T)))



# B312 (2015-2021)
# TODO BIEN

b312 <- master_data.df %>%
  mutate(Stat="Nacional") %>%
  group_by(Stat) %>%
  summarize(TPCA = round(100*mean(tortura_tra_p, na.rm = T)),
            TPMP = round(100*mean(tortura_mp_p, na.rm = T)),
            TFCA = round(100*mean(tortura_tra_f, na.rm = T)),
            TFMP = round(100*mean(tortura_mp_f, na.rm = T))
            )



# B313 (2015-2021)
# TODO BIEN

b313 <- master_data.df %>%
  mutate(across(c("P4_8_1","P4_8_2", "P4_8_3", "P4_8_4", "P4_8_5","P4_8_6","P4_8_7",
                  "P4_8_8","P4_8_9","P4_8_10","P4_8_11"), ~ recode(.x,
                                                                                              "1" = 1,
                                                                                              "2" = 0,
                                                                                              "8" = NA_real_,
                                                                                              "9" = NA_real_)),
         Stat="Nacional") %>%
  group_by(Stat) %>%
  summarize(CA1 = round(100*mean(P3_17_01, na.rm = T)),
            CA2 = round(100*mean(P3_17_02, na.rm = T)),
            CA3 = round(100*mean(P3_17_03, na.rm = T)),
            CA4 = round(100*mean(P3_17_04, na.rm = T)),
            CA5 = round(100*mean(P3_17_05, na.rm = T)),
            CA6 = round(100*mean(P3_17_06, na.rm = T)),
            CA7 = round(100*mean(P3_17_07, na.rm = T)),
            CA8 = round(100*mean(P3_17_08, na.rm = T)),
            CA9 = round(100*mean(P3_17_09, na.rm = T)),
            CA10 = round(100*mean(P3_17_10, na.rm = T)),
            CA11 = round(100*mean(P3_17_11, na.rm = T)),
            MP1 = round(100*mean(P4_8_1, na.rm = T)),
            MP2 = round(100*mean(P4_8_2, na.rm = T)),
            MP3 = round(100*mean(P4_8_3, na.rm = T)),
            MP4 = round(100*mean(P4_8_4, na.rm = T)),
            MP5 = round(100*mean(P4_8_5, na.rm = T)),
            MP6 = round(100*mean(P4_8_6, na.rm = T)),
            MP7 = round(100*mean(P4_8_7, na.rm = T)),
            MP8 = round(100*mean(P4_8_8, na.rm = T)),
            MP9 = round(100*mean(P4_8_9, na.rm = T)),
            MP10 = round(100*mean(P4_8_10, na.rm = T)),
            MP11 = round(100*mean(P4_8_11, na.rm = T)))
  



# B314 (2015-2021)
# TODO BIEN, PERO COMENTARIOS

b314 <- master_data.df %>%
  mutate(across(c("P3_18_11", "P4_9_01","P4_9_02", "P4_9_03", "P4_9_04", "P4_9_05","P4_9_06","P4_9_07",
                  "P4_9_08","P4_9_09","P4_9_10","P4_9_11","P4_9_12","P4_9_13","P4_9_14"), ~ recode(.x,
                                                                   "1" = 1,
                                                                   "2" = 0,
                                                                   "8" = NA_real_,
                                                                   "9" = NA_real_)),
         Stat="Nacional") %>%
  group_by(Stat) %>%
  summarize(CA1 = round(100*mean(P3_18_01, na.rm = T)),
            CA2 = round(100*mean(P3_18_02, na.rm = T)),
            CA3 = round(100*mean(P3_18_03, na.rm = T)),
            CA4 = round(100*mean(P3_18_04, na.rm = T)),
            CA5 = round(100*mean(P3_18_05, na.rm = T)),
            CA6 = round(100*mean(P3_18_06, na.rm = T)),
            CA7 = round(100*mean(P3_18_07, na.rm = T)),
            CA8 = round(100*mean(P3_18_08, na.rm = T)),
            CA9 = round(100*mean(P3_18_09, na.rm = T)),
            CA10 = round(100*mean(P3_18_10, na.rm = T)),
            CA11 = round(100*mean(P3_18_11, na.rm = T)),
            CA12 = round(100*mean(P3_18_12, na.rm = T)),
            CA13 = round(100*mean(P3_18_13, na.rm = T)),
            CA14 = round(100*mean(P3_18_14, na.rm = T)),
            MP1 = round(100*mean(P4_9_01, na.rm = T)),
            MP2 = round(100*mean(P4_9_02, na.rm = T)),
            MP3 = round(100*mean(P4_9_03, na.rm = T)),
            MP4 = round(100*mean(P4_9_04, na.rm = T)),
            MP5 = round(100*mean(P4_9_05, na.rm = T)),
            MP6 = round(100*mean(P4_9_06, na.rm = T)),
            MP7 = round(100*mean(P4_9_07, na.rm = T)),
            MP8 = round(100*mean(P4_9_08, na.rm = T)),
            MP9 = round(100*mean(P4_9_09, na.rm = T)),
            MP10 = round(100*mean(P4_9_10, na.rm = T)),
            MP11 = round(100*mean(P4_9_11, na.rm = T)),
            MP12 = round(100*mean(P4_9_12, na.rm = T)),
            MP13 = round(100*mean(P4_9_13, na.rm = T)),
            MP14 = round(100*mean(P4_9_14, na.rm = T)))



# B315 (2015-2021)
# TODO BIEN

b315 <- master_data.df %>%
  filter (years_since_RND_3>=-2 & years_since_RND_3<=2)  %>%
  mutate(tortura_p = case_when(tortura_tra_p == 1 ~ 1,
                               tortura_mp_p == 1 ~ 1,
                               tortura_tra_p == 0 & tortura_mp_p == 0 ~ 0,
                               T ~ NA_real_),
         tortura_f = case_when(tortura_tra_f == 1 ~ 1,
                               tortura_mp_f == 1 ~ 1,
                               tortura_tra_f == 0 & tortura_mp_f == 0 ~ 0,
                               T ~ NA_real_),
         tortura = case_when(tortura_p == 1 & tortura_f == 1 ~ 1,
                               tortura_p == 0 | tortura_f == 0 ~ 0,
                             T ~ NA_real_),) %>%
  group_by(RND_3) %>%
  summarize(TP = round(100*mean(tortura_p, na.rm = T)),
            TF = round(100*mean(tortura_f, na.rm = T)),
            TO = round(100*mean(tortura, na.rm = T))
  )



# B321 (2015-2021)
# TODO BIEN

b321 <- master_data.df %>%
  group_by(Anio_arresto) %>%
  summarize(Tortura = round(100*mean(det_ninguna, na.rm = T)))



# B322 (2015-2021)
# TODO BIEN

b322 <- master_data.df  %>%
  mutate(M4 = case_when(P3_20 == "01" ~ 1,
                          P3_20 == "02" ~ 1,
                          P3_20 == "03" ~ 1,
                          P3_20 == "04" ~ 1,
                          P3_20 == "05" ~ 0,
                          P3_20 == "06" ~ 0,
                          P3_20 == "07" ~ 0,
                          P3_20 == "08" ~ 0,
                        P3_20 == "09" ~ 0,
                        T ~ NA_real_),
         M4M24 = case_when(P3_20 == "01" ~ 0,
                        P3_20 == "02" ~ 0,
                        P3_20 == "03" ~ 0,
                        P3_20 == "04" ~ 0,
                        P3_20 == "05" ~ 1,
                        P3_20 == "06" ~ 1,
                        P3_20 == "07" ~ 0,
                        P3_20 == "08" ~ 0,
                        P3_20 == "09" ~ 0,
                        T ~ NA_real_),
         M24M48 = case_when(P3_20 == "01" ~ 0,
                        P3_20 == "02" ~ 0,
                        P3_20 == "03" ~ 0,
                        P3_20 == "04" ~ 0,
                        P3_20 == "05" ~ 0,
                        P3_20 == "06" ~ 0,
                        P3_20 == "07" ~ 1,
                        P3_20 == "08" ~ 0,
                        P3_20 == "09" ~ 0,
                        T ~ NA_real_),
         M48 = case_when(P3_20 == "01" ~ 0,
                        P3_20 == "02" ~ 0,
                        P3_20 == "03" ~ 0,
                        P3_20 == "04" ~ 0,
                        P3_20 == "05" ~ 0,
                        P3_20 == "06" ~ 0,
                        P3_20 == "07" ~ 0,
                        P3_20 == "08" ~ 1,
                        P3_20 == "09" ~ 1,
                        T ~ NA_real_),
         Stat = "Nacional") %>%
  group_by(Stat) %>%
  summarize(M4 = round(100*mean(M4, na.rm = T)),
            M4M24 = round(100*mean(M4M24, na.rm = T)),
            M24M48 = round(100*mean(M24M48, na.rm = T)),
            M48 = round(100*mean(M48, na.rm = T))
  )



# B323 (2015-2021)
# TODO BIEN

b323 <- master_data.df  %>%
  mutate(M4 = case_when(P3_20 == "01" ~ 1,
                        P3_20 == "02" ~ 1,
                        P3_20 == "03" ~ 1,
                        P3_20 == "04" ~ 1,
                        P3_20 == "05" ~ 0,
                        P3_20 == "06" ~ 0,
                        P3_20 == "07" ~ 0,
                        P3_20 == "08" ~ 0,
                        P3_20 == "09" ~ 0,
                        T ~ NA_real_),
         M4M = case_when(P3_20 == "01" ~ 0,
                           P3_20 == "02" ~ 0,
                           P3_20 == "03" ~ 0,
                           P3_20 == "04" ~ 0,
                           P3_20 == "05" ~ 1,
                           P3_20 == "06" ~ 1,
                           P3_20 == "07" ~ 1,
                           P3_20 == "08" ~ 1,
                           P3_20 == "09" ~ 1,
                           T ~ NA_real_)
         ) %>%
  group_by(Anio_arresto) %>%
  summarize(M4 = round(100*mean(M4, na.rm = T)),
            M4M = round(100*mean(M4M, na.rm = T))
  )



# B324 (2015-2021)
# TODO BIEN

b324 <- master_data.df  %>%
  mutate(M4 = case_when(P3_20 == "01" ~ 1,
                        P3_20 == "02" ~ 1,
                        P3_20 == "03" ~ 1,
                        P3_20 == "04" ~ 1,
                        P3_20 == "05" ~ 0,
                        P3_20 == "06" ~ 0,
                        P3_20 == "07" ~ 0,
                        P3_20 == "08" ~ 0,
                        P3_20 == "09" ~ 0,
                        T ~ NA_real_)
  ) %>%
  group_by(Estado_arresto) %>%
  summarize(M4 = round(100*mean(M4, na.rm = T)))
  



# B325 (2015-2021)
# TODO BIEN

b325 <- master_data.df  %>%
  mutate(LT = case_when(P3_19 == "01" ~ "MP",
                        P3_19 == "02" ~ "Juez",
                        P3_19 == "03" ~ "Policia",
                        P3_19 == "04" ~ "Arraigo",
                        P3_19 == "05" ~ "CP",
                        P3_19 == "06" ~ "Gob",
                        P3_19 == "07" ~ "Casa",
                        P3_19 == "08" ~ "Comercio",
                        P3_19 == "09" ~ "Vehiculo",
                        P3_19 == "10" ~ "Terreno",
                        P3_19 == "11" ~ "Militar",
                        P3_19 == "12" ~ "Migrantes",
                        P3_19 == "13" ~ "Hospital",
                        P3_19 == "14" ~ "Otro",
                        T ~ NA_character_),
         Stat="Nacional"
  ) %>%
  drop_na(LT) %>%
  group_by(LT) %>%
  summarize(Freq = n()) %>%
  mutate(Perc = round(100*Freq/sum(Freq))) %>%
  arrange(Freq)



# B326 (2015-2021)
# NO CUADRA

b326 <- master_data.df  %>%
  mutate(MP = case_when(P3_19 == "01" ~ 1,
                        P3_19 == "02" ~ 0,
                        P3_19 == "03" ~ 0,
                        P3_19 == "04" ~ 0,
                        P3_19 == "05" ~ 0,
                        P3_19 == "06" ~ 0,
                        P3_19 == "07" ~ 0,
                        P3_19 == "08" ~ 0,
                        P3_19 == "09" ~ 0,
                        P3_19 == "10" ~ 0,
                        P3_19 == "11" ~ 0,
                        P3_19 == "12" ~ 0,
                        P3_19 == "13" ~ 0,
                        P3_19 == "14" ~ 0,
                        T ~ NA_real_),
         PO = case_when(P3_19 == "01" ~ 0,
                         P3_19 == "02" ~ 0,
                         P3_19 == "03" ~ 1,
                         P3_19 == "04" ~ 0,
                         P3_19 == "05" ~ 0,
                         P3_19 == "06" ~ 0,
                         P3_19 == "07" ~ 0,
                         P3_19 == "08" ~ 0,
                         P3_19 == "09" ~ 0,
                         P3_19 == "10" ~ 0,
                         P3_19 == "11" ~ 0,
                         P3_19 == "12" ~ 0,
                         P3_19 == "13" ~ 0,
                         P3_19 == "14" ~ 0,
                         T ~ NA_real_)
  ) %>%
  group_by(Anio_arresto) %>%
  summarize(MP = round(100*mean(MP, na.rm = T)),
            PO = round(100*mean(PO, na.rm = T))
  )




# B327 (2015-2021)
# TODO BIEN

b327 <- master_data.df  %>%
  mutate(MP = case_when(P3_19 == "01" ~ 1,
                        P3_19 == "02" ~ 0,
                        P3_19 == "03" ~ 0,
                        P3_19 == "04" ~ 0,
                        P3_19 == "05" ~ 0,
                        P3_19 == "06" ~ 0,
                        P3_19 == "07" ~ 0,
                        P3_19 == "08" ~ 0,
                        P3_19 == "09" ~ 0,
                        P3_19 == "10" ~ 0,
                        P3_19 == "11" ~ 0,
                        P3_19 == "12" ~ 0,
                        P3_19 == "13" ~ 0,
                        P3_19 == "14" ~ 0,
                        T ~ NA_real_)
  )%>%
  group_by(Estado_arresto) %>%
  summarize(MP = round(100*mean(MP, na.rm = T)))



# B328 (2015-2021)
# TODO BIEN

b328 <- master_data.df  %>%
  filter(sentenciado == 1) %>%
  mutate(PP = case_when(P5_9 == "1" ~ 1,
                        P5_9 == "2" ~ 0,
                        T ~ NA_real_),
         Stat="Nacional"
  ) %>%
  group_by(Stat) %>%
  summarize(PP = round(100*mean(PP, na.rm = T)))



# B329 (2015-2021)
# MAL, ESTÁ CALCULADA PARA PROCESADOS, NO SENTENCIADOS

b329 <- master_data.df  %>%
  filter(sentenciado == 1, P5_9 == "1") %>%
  mutate(PP2A = case_when(P5_10 == "1" ~ 1,
                          P5_10 == "2" ~ 1,
                          P5_10 == "3" ~ 1,
                          P5_10 == "4" ~ 1,
                          P5_10 == "5" ~ 1,
                          P5_10 == "6" ~ 1,
                          P5_10 == "7" ~ 0,
                        T ~ NA_real_),
         Stat="Nacional"
  ) %>%
  group_by(Stat) %>%
  summarize(PP2A = round(100*mean(PP2A, na.rm = T)))


# B310 (2015-2021)
# TODO BIEN

b3210 <- master_data.df  %>%
  group_by(tipo_prision_preventiva) %>%
  drop_na(tipo_prision_preventiva) %>%
  summarize(Frequency = n()) %>%
  mutate(value2plot = round(Frequency / sum(Frequency) * 100))



# B51
# TODO BIEN

b51 <- master_data.df  %>%
  filter(sentenciado == 1) %>%
  mutate(
    Stat="Nacional"
  ) %>%
  group_by(Stat) %>%
  summarize(PJ = round(100*mean(P5_26A, na.rm = T)))



# B52
# TODO BIEN

b52 <- master_data.df  %>%
  filter(sentenciado == 1) %>%
  group_by(P5_26A) %>%
  summarize(IND = round(100*mean(indicator_general, na.rm = T)),
            IPJ = round(100*mean(indicator_PJ, na.rm = T)),
            IAA = round(100*mean(indicator_UAA, na.rm = T)),
            IDH = round(100*mean(indicator_GDH, na.rm = T)),
            )



# B53
# TODO BIEN

b531 <- master_data.df %>%
  mutate(JA= case_when(P5_6=="1"~"Juicio",
                       P5_6=="2"~"Abreviado",
                       T ~ NA_character_)
  ) %>%
  group_by(JA) %>%
  summarize(Jus = round(100*mean(P5_26A, na.rm = T)),
  )

b532 <- master_data.df %>%
  group_by(escuchado_x_juez) %>%
  summarize(Jus = round(100*mean(P5_26A, na.rm = T)),
  )

b533 <- master_data.df %>%
  group_by(culpabilidad) %>%
  summarize(Jus = round(100*mean(P5_26A, na.rm = T)),
  )

b53 <- bind_rows(b531,b532,b533)

a111 <- master_data.df %>%
  filter(sentenciado == 1, as.numeric(Anio_arresto) >= 2018, P5_11_98 != "1", P5_11_99 != "1" ) %>%
  mutate(Robo_veh = case_when(P5_11_01 == "1" ~ 1,
                              P5_11_01 == "0" ~ 0,
                              T ~ NA_real_),
         Robo_cas = case_when(P5_11_02 == "1" ~ 1,
                              P5_11_02 == "0" ~ 0,
                              T ~ NA_real_),
         Robo_neg = case_when(P5_11_03 == "1" ~ 1,
                              P5_11_03 == "0" ~ 0,
                              T ~ NA_real_),
         Robo_tpu = case_when(P5_11_04 == "1" ~ 1,
                              P5_11_04 == "0" ~ 0,
                              T ~ NA_real_),
         Robo_via = case_when(P5_11_05 == "1" ~ 1,
                              P5_11_05 == "0" ~ 0,
                              T ~ NA_real_),
         Robo_aut = case_when(P5_11_06 == "1" ~ 1,
                              P5_11_06 == "0" ~ 0,
                              T ~ NA_real_),
         Robo_otr = case_when(P5_11_07 == "1" ~ 1,
                              P5_11_07 == "0" ~ 0,
                              T ~ NA_real_),
         Pos_dro = case_when(P5_11_08 == "1" ~ 1,
                             P5_11_08 == "0" ~ 0,
                             T ~ NA_real_),
         Com_dro = case_when(P5_11_09 == "1" ~ 1,
                             P5_11_09 == "0" ~ 0,
                             T ~ NA_real_),
         Lesiones = case_when(P5_11_10 == "1" ~ 1,
                              P5_11_10 == "0" ~ 0,
                              T ~ NA_real_),
         Hom_cul = case_when(P5_11_11 == "1" ~ 1,
                             P5_11_11 == "0" ~ 0,
                             T ~ NA_real_),
         Hom_dol = case_when(P5_11_12 == "1" ~ 1,
                             P5_11_12 == "0" ~ 0,
                             T ~ NA_real_),
         Armas = case_when(P5_11_13 == "1" ~ 1,
                           P5_11_13 == "0" ~ 0,
                           T ~ NA_real_),
         Incum = case_when(P5_11_14 == "1" ~ 1,
                           P5_11_14 == "0" ~ 0,
                           T ~ NA_real_),
         Violen = case_when(P5_11_15 == "1" ~ 1,
                            P5_11_15 == "0" ~ 0,
                            T ~ NA_real_),
         Daño = case_when(P5_11_16 == "1" ~ 1,
                          P5_11_16 == "0" ~ 0,
                          T ~ NA_real_),
         Secuestro = case_when(P5_11_17 == "1" ~ 1,
                               P5_11_17 == "0" ~ 0,
                              T ~ NA_real_),
         Violacion = case_when(P5_11_18 == "1" ~ 1,
                               P5_11_18 == "0" ~ 0,
                               T ~ NA_real_),
         Fraude = case_when(P5_11_19 == "1" ~ 1,
                            P5_11_19 == "0" ~ 0,
                            T ~ NA_real_),
         Del_Org = case_when(P5_11_20 == "1" ~ 1,
                             P5_11_20 == "0" ~ 0,
                             T ~ NA_real_),
         Sexuales = case_when(P5_11_21 == "1" ~ 1,
                              P5_11_21 == "0" ~ 0,
                              T ~ NA_real_),
         Extorsion = case_when(P5_11_22 == "1" ~ 1,
                               P5_11_22 == "0" ~ 0,
                              T ~ NA_real_),
         Priv_lib = case_when(P5_11_23 == "1" ~ 1,
                              P5_11_23 == "0" ~ 0,
                              T ~ NA_real_),
         Abuso_conf = case_when(P5_11_24 == "1" ~ 1,
                                P5_11_24 == "0" ~ 0,
                              T ~ NA_real_),
         Amenazas = case_when(P5_11_25 == "1" ~ 1,
                              P5_11_25 == "0" ~ 0,
                              T ~ NA_real_),
         Otro = case_when(P5_11_26 == "1" ~ 1,
                          P5_11_26 == "0" ~ 0,
                              T ~ NA_real_),
         Stat="Nacional") %>%
  group_by(Stat) %>%
  summarize(Robo_veh = sum(Robo_veh == 1),
                             Robo_cas = sum(Robo_cas == 1),
                             Robo_neg = sum(Robo_neg == 1),
                             Robo_tpu = sum(Robo_tpu == 1),
                             Robo_via = sum(Robo_via == 1),
                             Robo_aut = sum(Robo_aut == 1),
                             Robo_otr = sum(Robo_otr == 1),
                             Pos_dro = sum(Pos_dro == 1),
                             Com_dro = sum(Com_dro == 1),
                             Lesiones = sum(Lesiones == 1),
                             Hom_cul = sum(Hom_cul == 1),
                             Hom_dol = sum(Hom_dol == 1),
                             Armas = sum(Armas == 1),
                             Incum = sum(Incum == 1),
                             Violen = sum(Violen == 1),
                             Daño = sum(Daño == 1),
                             Secuestro = sum(Secuestro == 1),
                             Violacion = sum(Violacion == 1),
                             Fraude = sum(Fraude == 1),
                             Del_Org = sum(Del_Org == 1),
                             Sexuales = sum(Sexuales == 1),
                             Extorsion = sum(Extorsion == 1),
                             Priv_lib = sum(Priv_lib == 1),
                             Abuso_conf = sum(Abuso_conf == 1),
                             Amenazas = sum(Amenazas == 1),
                             Otro = sum(Otro == 1)
  ) %>% 
  pivot_longer( cols = -c("Stat"), names_to = "Delito", values_to = "n") %>%
  mutate(Porcentaje = round(100*n/sum(n))) %>% 
  arrange(Porcentaje)
  