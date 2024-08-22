

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

# B111




