

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

snsp <- read.csv(paste0(path2SP,"/National/Exploration/Input/IDEFC_NM_abr24.csv"),check.names = F)

master_data.df <- Main_database %>% filter(as.numeric(Anio_arresto) >= 2015, NSJP == 1)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## B1                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# B111 (2015-2021)

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




# B1114 (6m,1a,2a)

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
  summarize(Años = round(100*mean(Años, na.rm = T)))
b1152 <- b115 %>%
  group_by(DOJ) %>%
  summarize(Años = round(100*mean(Años, na.rm = T)))
b115 <- bind_rows(b1151, b1152)


#NO HAY B116



# B117 (2015-2021)

b117 <- master_data.df %>%
  mutate(Acceso = case_when(P5_16_5 == "1" | P5_36_5 == "1" ~ 1,
                            P5_16_5 == "2" | P5_36_5 == "2" ~ 1,
                            P5_16_5 == "3" | P5_36_5 == "3" ~ 1,
                            P5_16_5 == "4" | P5_36_5 == "4" ~ 0,
                          T ~ NA_real_),
         Video = case_when(P5_19_3 == "1" | P5_39_3 == "1" ~ 1,
                           P5_19_3 == "2" | P5_39_3 == "2" ~ 0,
                        T ~ NA_real_),) %>%
  group_by(Anio_arresto) %>%
  summarize(Acceso = round(100*mean(Acceso, na.rm = T)),
            Video = round(100*mean(Video, na.rm = T)))

# B118 (2015-2021)

b118 <- master_data.df %>%
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

b119 <- master_data.df %>%
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

b211 <- master_data.df %>%
  group_by(Anio_arresto) %>%
  summarize(Fuerza = round(100*(1-mean(proporcionalidad_uso_fuerza, na.rm = T))))



# B212 (2015-2021)

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

b213 <- master_data.df %>%
  mutate(Sec = case_when(P5_11_17 == "1" | P5_31_17 == "1" ~ 1,
                         P5_11_17 == "0" | P5_31_17 == "0"  ~ 0,
                          T ~ NA_character_),
         Ext = case_when(P5_11_22 == "1" | P5_31_22 == "1" ~ 1,
                        P5_11_22 == "0" | P5_31_22 == "0"  ~ 0,
                        T ~ NA_character_),
         Por = case_when(P5_11_13 == "1" | P5_31_13 == "1" ~ 1,
                        P5_11_13 == "0" | P5_31_13 == "0"  ~ 0,
                        T ~ NA_character_),
         RV = case_when(P5_11_01 == "1" | P5_31_01 == "1" ~ 1,
                        P5_11_01 == "0" | P5_31_01 == "0"  ~ 0,
                        T ~ NA_character_),
         HD = case_when(P5_11_12 == "1" | P5_31_12 == "1" ~ 1,
                        P5_11_12 == "0" | P5_31_12 == "0"  ~ 0,
                        T ~ NA_character_),
         RA = case_when(P5_11_06 == "1" | P5_31_06 == "1" ~ 1,
                        P5_11_06 == "0" | P5_31_06 == "0"  ~ 0,
                        T ~ NA_character_),
         PCD = case_when(P5_11_08 == "1" | P5_31_08 == "1" ~ 1,
                         P5_11_09 == "1" | P5_31_09 == "1" ~ 1,
                        (P5_11_08 == "0" | P5_31_08 == "0") & (P5_11_09 == "0" | P5_31_09 == "0") ~ 0,
                        T ~ NA_character_),
         Stat="Nacional"
  ) %>%
  group_by(stat) %>%
  summarize(Sec = round(100*mean(Sec, na.rm = T)),
            Ext = round(100*mean(Ext, na.rm = T)),
            Por = round(100*mean(Por, na.rm = T)),
            RV = round(100*mean(RV, na.rm = T)),
            HD = round(100*mean(HD, na.rm = T)),
            RA = round(100*mean(RA, na.rm = T)),
            PCD = round(100*mean(PCD, na.rm = T)))




# B221 (2015-2021)

b221 <- master_data.df %>%
  mutate(CA = case_when(P3_21_1 == "1" ~ 1,
                        P3_21_1 == "0" ~ 0,
                        T ~ NA_real_),
         MP = case_when(P4_15_1 == "1" ~ 1,
                        P4_15_1 == "0" ~ 0,
                        T ~ NA_real_),
         Juz = case_when(P5_45_1 == "1" ~ 1,
                        P5_45_1 == "0" ~ 0,
                        T ~ NA_real_),) %>%
  group_by(Anio_arresto) %>%
  summarize(CA = round(100*mean(CA, na.rm = T)),
            MP = round(100*mean(MP, na.rm = T)),
            Juz = round(100*mean(Juz, na.rm = T)))



# B222 (2015-2021)

b222 <- master_data.df %>%
  mutate(LCA = case_when(P3_22_1 == "1" ~ 1,
                        P3_22_1 == "0" ~ 0,
                        T ~ NA_real_),
         ICA = case_when(P3_22_2 == "1" ~ 1,
                         P3_22_3 == "1" ~ 1,
                         P3_22_2 == "0" & P3_22_3 == "0" ~ 1,
                         T ~ NA_real_),
         DCA = case_when(P3_22_4 == "1" ~ 1,
                         P3_22_4 == "0" ~ 0,
                         T ~ NA_real_),
         LMP = case_when(P4_16_1 == "1" ~ 1,
                         P4_16_1 == "0" ~ 0,
                         T ~ NA_real_),
         IMP = case_when(P4_16_2 == "1" ~ 1,
                         P4_16_4 == "1" ~ 1,
                         P4_16_2 == "0" & P3_22_4 == "0" ~ 1,
                         T ~ NA_real_),
         DMP = case_when(P4_16_3 == "1" ~ 1,
                         P4_16_3 == "0" ~ 0,
                         T ~ NA_real_),
         LJU = case_when(P5_46_1 == "1" ~ 1,
                         P5_46_1 == "0" ~ 0,
                         T ~ NA_real_),
         DJU = case_when(P5_46_2 == "1" ~ 1,
                         P5_46_3 == "1" ~ 1,
                         P5_46_4 == "1" ~ 1,
                         P5_46_5 == "1" ~ 1,
                         P5_46_6 == "1" ~ 1,
                         P5_46_2 == "0" & P5_46_3 == "0" & P5_46_4 == "0" &P5_46_5 == "0" &P5_46_6 == "0"  ~ 0,
                         T ~ NA_real_),
         Stat="Nacional"
         ) %>%
  group_by(Stat) %>%
  summarize(LCA = round(100*mean(LCA, na.rm = T)),
            ICA = round(100*mean(ICA, na.rm = T)),
            DCA = round(100*mean(DCA, na.rm = T)),
            LMP = round(100*mean(LCA, na.rm = T)),
            IMP = round(100*mean(ICA, na.rm = T)),
            DMP = round(100*mean(DCA, na.rm = T)),
            LJU = round(100*mean(LCA, na.rm = T)),
            DJU = round(100*mean(DCA, na.rm = T)),)



# B311 (2015-2021)

b311 <- master_data.df %>%
  group_by(Anio_arresto) %>%
  summarize(Tortura = round(100*mean(tortura_generalizada, na.rm = T)))



# B312 (2015-2021)

b312 <- master_data.df %>%
  mutate(Stat="Nacional") %>%
  group_by(Stat) %>%
  summarize(TPCA = round(100*mean(tortura_tra_p, na.rm = T)),
            TPMP = round(100*mean(tortura_mp_p, na.rm = T)),
            TFCA = round(100*mean(tortura_tra_f, na.rm = T)),
            TFMP = round(100*mean(tortura_mp_p, na.rm = T))
            )



# B313 (2015-2021)

b313 <- master_data.df %>%
  mutate(Stat="Nacional") %>%
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
            MP1 = round(100*mean(P4_8_01, na.rm = T)),
            MP2 = round(100*mean(P4_8_02, na.rm = T)),
            MP3 = round(100*mean(P4_8_03, na.rm = T)),
            MP4 = round(100*mean(P4_8_04, na.rm = T)),
            MP5 = round(100*mean(P4_8_05, na.rm = T)),
            MP6 = round(100*mean(P4_8_06, na.rm = T)),
            MP7 = round(100*mean(P4_8_07, na.rm = T)),
            MP8 = round(100*mean(P4_8_08, na.rm = T)),
            MP9 = round(100*mean(P4_8_09, na.rm = T)),
            MP10 = round(100*mean(P4_8_10, na.rm = T)),
            MP11 = round(100*mean(P4_8_11, na.rm = T)))
  



# B314 (2015-2021)

b314 <- master_data.df %>%
  mutate(Stat="Nacional") %>%
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
            MP1 = round(100*mean(P4_8_01, na.rm = T)),
            MP2 = round(100*mean(P4_8_02, na.rm = T)),
            MP3 = round(100*mean(P4_8_03, na.rm = T)),
            MP4 = round(100*mean(P4_8_04, na.rm = T)),
            MP5 = round(100*mean(P4_8_05, na.rm = T)),
            MP6 = round(100*mean(P4_8_06, na.rm = T)),
            MP7 = round(100*mean(P4_8_07, na.rm = T)),
            MP8 = round(100*mean(P4_8_08, na.rm = T)),
            MP9 = round(100*mean(P4_8_09, na.rm = T)),
            MP10 = round(100*mean(P4_8_10, na.rm = T)),
            MP11 = round(100*mean(P4_8_11, na.rm = T)),
            MP12 = round(100*mean(P4_8_12, na.rm = T)),
            MP13 = round(100*mean(P4_8_13, na.rm = T)),
            MP14 = round(100*mean(P4_8_14, na.rm = T)))



# B315 (2015-2021)

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
         tortura = case_when(tortura_p == 1 ~ 1,
                               tortura_f == 1 ~ 1,
                               tortura_p == 0 & tortura_f == 0 ~ 0,
                             T ~ NA_real_),) %>%
  group_by(RND_3) %>%
  summarize(TP = round(100*mean(tortura_p, na.rm = T)),
            TF = round(100*mean(tortura_f, na.rm = T)),
            TO = round(100*mean(tortura, na.rm = T))
  )



# B321 (2015-2021)

b321 <- master_data.df %>%
  group_by(Anio_arresto) %>%
  summarize(Tortura = round(100*mean(det_ninguna, na.rm = T)))



# B322 (2015-2021)

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
            M4M = round(100*mean(M4M24, na.rm = T))
  )



# B324 (2015-2021)

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
  group_by(Stat) %>%
  summarize(LT = round(100*mean(LT, na.rm = T))) %>%
  arrange(LT)



# B326 (2015-2021)

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
         PO = ccase_when(P3_19 == "01" ~ 0,
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

b329 <- master_data.df  %>%
  filter(sentenciado == 1) %>%
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

b3210 <- master_data.df  %>%
  filter(sentenciado == 1) %>%
  mutate(
         Stat="Nacional"
  ) %>%
  group_by(Stat) %>%
  summarize(PPO = round(100*mean(tipo_prision_preventiva, na.rm = T)))


