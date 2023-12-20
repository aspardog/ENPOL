
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Exploración tortura 
##
## Author(s):         D. Cristina Álvarez Venzor     (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     December 05th, 2023
##
## This version:      December 12th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Load Settings                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("National/Data_cleaning/Code/settings.R")

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

#variable de tipo de prueba

Main_database <- Main_database %>% mutate( tipo_prueba_declaracion = case_when(P5_15_01 == "1" | P5_15_07 == "1" ~ 1,
                                                                              T ~ 0))

#torurado, no torturado

table(Main_database$tortura_generalizada, useNA = "always")

#Culpable
table(Main_database$tortura_generalizada, Main_database$culpabilidad, useNA = "always")

#Declaró culpable P4.6.4
tortura <- subset(Main_database, tortura_generalizada == 0)
table(tortura$culpabilidad, tortura$P4_6_4, useNA = "always")

tortura <- subset(Main_database, tortura_generalizada == 1)
table(tortura$culpabilidad, tortura$P4_6_4, useNA = "always")

#Declaró culpable tipo prueba
tipo <- subset(Main_database, tortura_generalizada == 0)
tipo <- subset(tipo, culpabilidad ==  0)
table(tipo$P4_6_4, tipo$tipo_prueba_declaracion, useNA = "always")

tipo <- subset(Main_database, tortura_generalizada == 1)
tipo <- subset(tipo, culpabilidad ==  0)
table(tipo$P4_6_4, tipo$tipo_prueba_declaracion, useNA = "always")


#Declaró culpable P4_3A_7
tortura <- subset(Main_database, tortura_generalizada == 0)
table(tortura$culpabilidad, tortura$P4_3A_7, useNA = "always")

tortura <- subset(Main_database, tortura_generalizada == 1)
table(tortura$culpabilidad, tortura$P4_3A_7, useNA = "always")


#Declaró culpable tipo prueba
tipo <- subset(Main_database, tortura_generalizada == 0)
tipo <- subset(tipo, culpabilidad ==  0)
table(tipo$P4_3A_7, tipo$tipo_prueba_declaracion, useNA = "always")

tipo <- subset(Main_database, tortura_generalizada == 1)
tipo <- subset(tipo, culpabilidad ==  0)
table(tipo$P4_3A_7, tipo$tipo_prueba_declaracion, useNA = "always")

######### Sólo Sentenciados

subset <-  subset(Main_database, sentenciado == 1)

#torurado, no torturado

table(subset$tortura_generalizada, useNA = "always")

#Culpable
table(subset$tortura_generalizada, subset$culpabilidad, useNA = "always")

#Declaró culpable P4.6.4
tortura <- subset(subset, tortura_generalizada == 0)
table(tortura$culpabilidad, tortura$P4_6_4, useNA = "always")

tortura <- subset(subset, tortura_generalizada == 1)
table(tortura$culpabilidad, tortura$P4_6_4, useNA = "always")

#Declaró culpable tipo prueba
tipo <- subset(subset, tortura_generalizada == 0)
tipo <- subset(tipo, culpabilidad ==  0)
table(tipo$P4_6_4, tipo$tipo_prueba_declaracion, useNA = "always")

tipo <- subset(subset, tortura_generalizada == 1)
tipo <- subset(tipo, culpabilidad ==  0)
table(tipo$P4_6_4, tipo$tipo_prueba_declaracion, useNA = "always")


#Declaró culpable P4_3A_7
tortura <- subset(subset, tortura_generalizada == 0)
table(tortura$culpabilidad, tortura$P4_3A_7, useNA = "always")

tortura <- subset(subset, tortura_generalizada == 1)
table(tortura$culpabilidad, tortura$P4_3A_7, useNA = "always")


#Declaró culpable tipo prueba
tipo <- subset(subset, tortura_generalizada == 0)
tipo <- subset(tipo, culpabilidad ==  0)
table(tipo$P4_3A_7, tipo$tipo_prueba_declaracion, useNA = "always")

tipo <- subset(subset, tortura_generalizada == 1)
tipo <- subset(tipo, culpabilidad ==  0)
table(tipo$P4_3A_7, tipo$tipo_prueba_declaracion, useNA = "always")