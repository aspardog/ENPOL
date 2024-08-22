

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Load Settings & Data                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("National/Visualization/Code/settings.R")

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

snsp <- read.csv(paste0(path2SP,"/National/Exploration/Input/IDEFC_NM_abr24.csv"),check.names = F)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## B1                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Infografia 2015 a 2021

P3_14_5
P4_6_2
P4_1_03
P5_2_1
P5_22_02 P5_42_02
P5_22_01 P5_42_01
P5_20_4 P5_40_4
P5_17_2 P5_37_2
P5_17_3 P5_37_2
P5_17_1 P5_37_2
P5_17_4 P5_37_2

P3_14_5
P4_3A_2
P4_1_04
P5_2_4
P3_20 (hasta 4 horas)
P4_19 (hasta 48 horas)
P5_10 (hasta 2 años)

# B111 (2015-2021)

P3_14_5
P4_1_03
P5_2_1

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

