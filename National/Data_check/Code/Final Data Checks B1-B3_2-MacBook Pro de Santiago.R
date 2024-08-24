

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
         juz = case_when(P5_2_1 == "1" ~ 1,
                        P5_2_1 == "2" ~ 0,
                        T ~ NA_real_)) %>%
  group_by(Anio_arresto) %>%
  summarize(CA = mean(P3_14_4, na.rm = T),
            MP = mean(MP, na.rm = T),
            Juzgado = mean(Juz, na.rm = T)
            )

# B112 (2015-2021)

P5_17_2 P5_37_2
P5_17_3 P5_37_2
P5_17_1 P5_37_2
P5_17_4 P5_37_2


# B113 (2015-2021)

P3_14_5
P4_3A_2
P4_1_04
P5_2_4


# B1114 (6m,1a,2a)

P5_10 

# Info 2

# B115

defensa_oportuna juez
defensa_oportuna mp
Años_sentencia

#Info 3



# B117 (2016-2021)

P5_16_5 P5_36_5
P5_19_2 y/o P5_19_3 P5_39_2 y/o P5_39_3

