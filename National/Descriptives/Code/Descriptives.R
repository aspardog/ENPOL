## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Tablas de frecuencia
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 19th, 2023
##
## This version:      June 22th, 2023
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


# Remove previous files

outPaths <- c("National/Descriptives/Output/")

# Listing previous outputs
prevOutputs <- list.files(outPaths, 
                          include.dirs = F, 
                          full.names   = T, 
                          recursive    = T)

# Deleting previous outputs and remove objects used for this cleaning process
file.remove(prevOutputs)
rm(outPaths,prevOutputs)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Load Databases                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Define variables to tabulate by section and implement labels                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


Detencion <- c("P3_1",
               "P3_5_A",
               "Mes_anio_detencion",
               "P3_10",
               "P3_11",
               "P3_12_1",
               "P3_12_2",
               "P3_12_3",
               "P3_12_4",
               "P3_12_5",
               "P3_13_01",
               "P3_13_02",
               "P3_13_03",
               "P3_13_04",
               "P3_13_05",
               "P3_13_06",
               "P3_13_07",
               "P3_13_08",
               "P3_13_09",
               "P3_13_10",
               "P3_13_11",
               "P3_13_12",
               "P3_14_1",
               "P3_14_2",
               "P3_14_3",
               "P3_14_4",
               "P3_14_5",
               "P3_14_6",
               "P3_15_1",
               "P3_15_2",
               "P3_15_3",
               "P3_15_4",
               "P3_15_5",
               "P3_15_6",
               "P3_15_7",
               "P3_15_8",
               "P3_15_9",
               "P3_17_01",
               "P3_17_02",
               "P3_17_03",
               "P3_17_04",
               "P3_17_05",
               "P3_17_06",
               "P3_17_07",
               "P3_17_08",
               "P3_17_09",
               "P3_17_10",
               "P3_17_11",
               "P3_18_01",
               "P3_18_02",
               "P3_18_03",
               "P3_18_04",
               "P3_18_05",
               "P3_18_06",
               "P3_18_07",
               "P3_18_08",
               "P3_18_09",
               "P3_18_10",
               "P3_18_11",
               "P3_18_12",
               "P3_18_13",
               "P3_18_14",
               "P3_18_15",
               "P3_19",
               "P3_20",
               "P3_22_1",
               "P3_22_4")

MP <- c("P4_1_01",
        "P4_1_02",
        "P4_1_05",
        "P4_1_07",
        "P4_1_09",
        "P4_1_10",
        "P4_1_11",
        "P4_1_12",
        "P4_1_13",
        "P4_1_14",
        "P4_1_15",
        "P4_1_16",
        "P4_3",
        "P4_3A_1",
        "P4_3A_2",
        "P4_3A_3",
        "P4_3A_4",
        "P4_3A_5",
        "P4_3A_6",
        "P4_3A_7",
        "P4_3A_8",
        "P4_3A_9",
        "P4_6_1",
        "P4_6_3",
        "P4_6_4",
        "P4_6A_1",
        "P4_6A_4",
        "P4_7",
        "P4_8_1",
        "P4_8_2",
        "P4_8_3",
        "P4_8_4",
        "P4_8_5",
        "P4_8_6",
        "P4_8_7",
        "P4_8_8",
        "P4_8_9",
        "P4_8_10",
        "P4_8_11",
        "P4_9_01",
        "P4_9_02",
        "P4_9_03",
        "P4_9_04",
        "P4_9_05",
        "P4_9_06",
        "P4_9_07",
        "P4_9_08",
        "P4_9_09",
        "P4_9_10",
        "P4_9_11",
        "P4_9_12",
        "P4_9_13",
        "P4_9_14",
        "P4_9_15",
        "P4_13",
        "P4_14_1",
        "P4_14_2",
        "P4_14_5",
        "P4_14_6",
        "P4_14_7",
        "P4_16_1",
        "P4_16_3",
        "P4_19")

Proceso <- c("P5_2_5",
             "R5_2_1AX",
             "P5_4_A",
             "P5_4_M",
             "P5_6",
             "P5_7",
             "P5_8_1",
             "P5_8_2",
             "P5_8_3",
             "P5_8_4",
             "P5_8_8",
             "P5_8_9",
             "P5_9",
             "P5_10",
             "P5_14",
             "P5_16_1",
             "P5_16_2",
             "P5_16_3",
             "P5_16_4",
             "P5_16_5",
             "P5_17_1",
             "P5_17_2",
             "P5_17_3",
             "P5_17_4",
             "P5_18",
             "P5_19_1",
             "P5_19_2",
             "P5_19_3",
             "P5_20_1",
             "P5_20_2",
             "P5_20_3",
             "P5_20_4",
             "P5_22_01",
             "P5_22_02",
             "P5_22_03",
             "P5_22_04",
             "P5_22_05",
             "P5_22_06",
             "P5_22_07",
             "P5_22_08",
             "P5_22_09",
             "P5_22_10",
             "P5_22_11",
             "P5_22_12",
             "P5_22_13",
             "P5_24_1",
             "P5_44_1",
             "P5_24_2",
             "P5_25",
             "P5_26A",
             "P5_26B",
             "P5_29_1",
             "P5_29_2",
             "P5_29_8",
             "P5_29_9",
             "P5_30",
             "P5_34_A",
             "P5_34_M",
             "P5_36_1",
             "P5_36_2",
             "P5_36_3",
             "P5_36_4",
             "P5_36_5",
             "P5_37_1",
             "P5_37_2",
             "P5_37_3",
             "P5_37_4",
             "P5_39_1",
             "P5_39_2",
             "P5_39_3",
             "P5_40_1",
             "P5_40_2",
             "P5_40_3",
             "P5_40_4",
             "P5_42_01",
             "P5_42_02",
             "P5_42_03",
             "P5_42_04",
             "P5_42_05",
             "P5_42_06",
             "P5_42_07",
             "P5_42_08",
             "P5_42_09",
             "P5_42_10",
             "P5_42_11",
             "P5_42_12",
             "P5_42_13",
             "P5_44_1",
             "P5_44_2",
             "P5_46_1",
             "P5_46_2",
             "P5_46_3",
             "P5_46_4",
             "P5_46_5",
             "P5_46_6")


# Get the main question from each individual variable (e.g. "P3_12" from "P3_12_01")

Detencion_a <- Detencion %>% substr(.,1,5) %>% unique()
MP_a <- MP %>% substr(.,1,5) %>% unique()
Proceso_a <- Proceso %>% substr(.,1,5) %>% unique()


# Generate label groups

yesno_short <- c("P3_22_1",
                 "P3_22_4",
                 "P4_16_1",
                 "P4_16_3",
                 "P5_8_1",
                 "P5_8_2",
                 "P5_8_3",
                 "P5_8_4",
                 "P5_8_8",
                 "P5_8_9",
                 "P5_29_1",
                 "P5_29_2",
                 "P5_29_8",
                 "P5_29_9")

yesno_long <- c("P3_12_1",
                "P3_12_2",
                "P3_12_3",
                "P3_12_4",
                "P3_12_5","P3_13_01",
                "P3_13_02",
                "P3_13_03",
                "P3_13_04",
                "P3_13_05",
                "P3_13_06",
                "P3_13_07",
                "P3_13_08",
                "P3_13_09",
                "P3_13_10",
                "P3_13_11",
                "P3_13_12",
                "P3_14_1",
                "P3_14_2",
                "P3_14_3",
                "P3_14_4",
                "P3_14_5",
                "P3_14_6",
                "P3_15_1",
                "P3_15_2",
                "P3_15_3",
                "P3_15_4",
                "P3_15_5",
                "P3_15_6",
                "P3_15_7",
                "P3_15_8",
                "P3_15_9",
                "P3_18_01",
                "P3_18_02",
                "P3_18_03",
                "P3_18_04",
                "P3_18_05",
                "P3_18_06",
                "P3_18_07",
                "P3_18_08",
                "P3_18_09",
                "P3_18_10",
                "P3_18_11",
                "P3_18_12",
                "P3_18_13",
                "P3_18_14",
                "P3_18_15",
                "P4_1_01",
                "P4_1_02",
                "P4_1_05",
                "P4_1_07",
                "P4_1_09",
                "P4_1_10",
                "P4_1_11",
                "P4_1_12",
                "P4_1_13",
                "P4_1_14",
                "P4_1_15",
                "P4_1_16",
                "P4_3",
                "P4_3A_1",
                "P4_3A_2",
                "P4_3A_3",
                "P4_3A_4",
                "P4_3A_5",
                "P4_3A_6",
                "P4_3A_7",
                "P4_3A_8",
                "P4_3A_9",
                "P4_6_1",
                "P4_6_3",
                "P4_6_4",
                "P4_6A_1",
                "P4_6A_4",
                "P4_9_01",
                "P4_9_02",
                "P4_9_03",
                "P4_9_04",
                "P4_9_05",
                "P4_9_06",
                "P4_9_07",
                "P4_9_08",
                "P4_9_09",
                "P4_9_10",
                "P4_9_11",
                "P4_9_12",
                "P4_9_13",
                "P4_9_14",
                "P4_9_15",
                "P4_14_1",
                "P4_14_2",
                "P4_14_5",
                "P4_14_6",
                "P4_14_7",
                "P5_2_5",
                "P5_7",
                "P5_19_1",
                "P5_19_2",
                "P5_19_3",
                "P5_20_1",
                "P5_20_2",
                "P5_20_3",
                "P5_20_4",
                "P5_39_1",
                "P5_39_2",
                "P5_39_3",
                "P5_40_1",
                "P5_40_2",
                "P5_40_3",
                "P5_40_4",
                "P5_46_1",
                "P5_46_2",
                "P5_46_3",
                "P5_46_4",
                "P5_46_5",
                "P5_46_6")

yesno_longer <- c("P3_17_01",
                  "P3_17_02",
                  "P3_17_03",
                  "P3_17_04",
                  "P3_17_05",
                  "P3_17_06",
                  "P3_17_07",
                  "P3_17_08",
                  "P3_17_09",
                  "P3_17_10",
                  "P3_17_11",
                  "P4_8_1",
                  "P4_8_2",
                  "P4_8_3",
                  "P4_8_4",
                  "P4_8_5",
                  "P4_8_6",
                  "P4_8_7",
                  "P4_8_8",
                  "P4_8_9",
                  "P4_8_10",
                  "P4_8_11",
                  "P5_22_01",
                  "P5_22_02",
                  "P5_22_03",
                  "P5_22_04",
                  "P5_22_05",
                  "P5_22_06",
                  "P5_22_07",
                  "P5_22_08",
                  "P5_22_09",
                  "P5_22_10",
                  "P5_22_11",
                  "P5_22_12",
                  "P5_22_13",
                  "P5_42_01",
                  "P5_42_02",
                  "P5_42_03",
                  "P5_42_04",
                  "P5_42_05",
                  "P5_42_06",
                  "P5_42_07",
                  "P5_42_08",
                  "P5_42_09",
                  "P5_42_10",
                  "P5_42_11",
                  "P5_42_12",
                  "P5_42_13")

escala_intensidad <- c("P4_13",
                       "P5_17_1",
                       "P5_17_2",
                       "P5_17_3",
                       "P5_17_4",
                       "P5_26",
                       "P5_37_1",
                       "P5_37_2",
                       "P5_37_3",
                       "P5_37_4")

frecuencia <- c("P5_16_1",
                "P5_16_2",
                "P5_16_3",
                "P5_16_4",
                "P5_16_5",
                "P5_36_1",
                "P5_36_2",
                "P5_36_3",
                "P5_36_4",
                "P5_36_5")

escala_satisfaccion <- c("P5_24_1",
                         "P5_24_2",
                         "P5_44_1",
                         "P5_44_2")


# Apply generic labels

Main_database %<>% 
  mutate(across(any_of(c(Detencion,MP,Proceso)), ~ factor(.,ordered = TRUE))) %>%
  mutate(across(any_of(yesno_short), ~ recode(.,
                                              "0" = "No",
                                              "1" = "Sí"))) %>%
  mutate(across(any_of(yesno_long), ~ recode(.,
                                             "2" = "No",
                                             "1" = "Sí",
                                             "8" = "No Sabe",
                                             "9" = "No Responde",
  ))) %>%
  mutate(across(any_of(yesno_longer), ~ recode(.,
                                               "2" = "No",
                                               "1" = "Sí",
                                               "3" = "No aplica",
                                               "8" = "No Sabe",
                                               "9" = "No Responde",
  ))) %>%
  mutate(across(any_of(escala_intensidad), ~ recode(.,
                                                    "1" = "Mucho",
                                                    "2" = "Algo",
                                                    "3" = "Poco",
                                                    "4" = "Nada",
                                                    "8" = "No Sabe",
                                                    "9" = "No Responde",
  ))) %>%
  mutate(across(any_of(frecuencia), ~ recode(.,
                                             "1" = "Siempre",
                                             "2" = "La mayoría de las veces",
                                             "3" = "Pocas veces",
                                             "4" = "Nunca",
                                             "5" = "No aplica",
                                             "6" = "Nunca tuve audiencias",
                                             "8" = "No Sabe",
                                             "9" = "No Responde",
  ))) %>%
  mutate(across(any_of(escala_satisfaccion), ~ recode(.,
                                                      "1" = "Muy satisfecho",
                                                      "2" = "Algo satisfecho",
                                                      "3" = "Algo insatisfecho",
                                                      "4" = "Muy insatisfecho",
                                                      "5" = "No aplica",
                                                      "8" = "No Sabe",
                                                      "9" = "No Responde",
  ))) 

# Apply specific labels

Main_database %<>% 
  mutate(P3_1 = recode(P3_1, "1" = "Porque cometió un delito",
                       "2" = "Porque ayudó en la realización de un delito",
                       "3" = "Porque no ha podido comprobar su inocencia",
                       "4" = "Lo(a) acusaron falsamente de cometer un delito, lo(a) inculparon, lo(a) confundieron, dicen que cometió un delito",
                       "5" = "Otro",
                       "8" = "No sabe",
                       "9" = "No responde"),
         P3_10 = recode(P3_10, "1" = "Realizando la conducta o acto por el cual lo(a) acusan?",
                        "2" = "Inmediatamente después de la conducta o el acto por el cual lo(a) acusan?",
                        "3" = "Con una orden de detención?",
                        "4" = "Después de una inspección o revisión de su cuerpo o pertenencias?",
                        "5" = "Ninguna de las anteriores",
                        "8" = "No sabe",
                        "9" = "No responde"),
         P3_11 = recode(P3_11, "1" = "¿sacándolo(a) del lugar en 1 donde estaba?",
                        "2" = "¿mientras iba pasando por la calle?",
                        "3" = "Otro",
                        "8" = "No sabe",
                        "9" = "No responde"),
         P3_19 = recode(P3_19, "01" = "A la Agencia del Ministerio Público",
                        "02" = "Con un Juez de lo penal",
                        "03" = "A una instalación de la policía distinta a una Agencia del Ministerio Público (barandilla, comisaría, comandancia, C4, etc.)",
                        "04" = "A un centro de arraigo",
                        "05" = "A un Centro penitenciario",
                        "06" = "A una oficina del gobierno distinta a una Agencia del Ministerio Público (presidencia municipal, regiduría, etc.)",
                        "07" = "A una casa particular",
                        "08" = "A un establecimiento comercial (tienda, restaurante, hotel, etc.)",
                        "09" = "Lo(a) mantuvieron en un vehículo",
                        "10" = "A un terreno baldío",
                        "11" = "Zona militar (cuartel de la Marina o cuartel militar)",
                        "12" = "Centro de detención para migrantes",
                        "13" = "Hospital, clínica o centro de salud",
                        "14" = "Otro",
                        "98" = "No sabe",
                        "99" = "No responde"),
         P3_20 = recode(P3_20, "01" = "Hasta 30 minutos",
                        "02" = "Más de 30 minutos hasta 1 hora",
                        "03" = "Más de 1 hora hasta 2 horas",
                        "04" = "Más de 2 horas hasta 4 horas",
                        "05" = "Más de 4 horas hasta 6 horas",
                        "06" = "Más de 6 horas hasta 24 horas",
                        "07" = "Más de 24 horas hasta 48 horas",
                        "08" = "Más de 48 horas hasta 72 horas",
                        "09" = "Más de 72 horas",
                        "98" = "No sabe",
                        "99" = "No responde"),
         P4_7 = recode(P4_7, "01" = "Porque reconocí los hechos de manera voluntaria",
                       "02" = "Porque me lo recomendó mi abogado",
                       "03" = "Porque me lo recomendó alguien en el Ministerio Público",
                       "04" = "Porque me presionaron o amenazaron para hacerlo",
                       "05" = "Porque me agredieron físicamente",
                       "06" = "Para proteger a alguien más",
                       "07" = "Porque los interrogatorios fueron muy extensos",
                       "08" = "Porque me dijeron que podía salir libre",
                       "09" = "Porque me convencieron de que era culpable",
                       "10" = "Otro",
                       "98" = "No sabe",
                       "99" = "No responde"),
         P4_19 = recode(P4_19, "1" = "24 horas o menos",
                        "2" = "Más de 24 minutos hasta 48 horas",
                        "3" = "Más de 48 hora hasta 72 horas",
                        "4" = "Más de 72 horas hasta 96 horas",
                        "5" = "Más de 96 horas",
                        "8" = "No sabe",
                        "9" = "No responde"),
         R5_2_1AX = recode(R5_2_1AX, "1" = "No ha tenido audiencia por causa de COVID-19",
                           "2" = "Percepción de incumplimiento de Juez en las audiencias",
                           "3" = "Otras irregularidades percibidas",
                           "8" = "No sabe por qué no ha tenido audiencias"),
         P5_6 = recode(P5_6, "1" = "Un juicio",
                       "2" = "Un procedimiento abreviado o juicio sumario (no tuvo juicio porque renunció al juicio oral, aceptó el procedimiento abreviado, aceptó la responsabilidad en el delito, aceptó solo ser sentenciado con base en las pruebas que rpesentó el MP)",
                       "8" = "No sabe",
                       "9" = "No responde"),
         P5_9 = recode(P5_9, "1" = "privado(a) de la libertad en un Centro penitenciario (prisión preventiva)",
                       "2" = "en libertad?",
                       "8" = "No sabe",
                       "9" = "No responde"),
         P5_10 = recode(P5_10, "1" = "Hasta una semana",
                        "2" = "Más de una semana hasta dos semanas",
                        "3" = "Más de dos semanas hasta un mes",
                        "4" = "Más de un mes hasta seis meses",
                        "5" = "Más de seis meses hasta un año",
                        "6" = "Más de un año hasta dos años",
                        "7" = "Más de dos años",
                        "8" = "No sabe",
                        "9" = "No responde"),
         P5_14 = recode(P5_14, "1" = "Sí, eran diferentes",
                        "2" = "No, era el mismo",
                        "3" = "Nunca vio a ningún juez",
                        "8" = "No sabe",
                        "9" = "No responde"),
         P5_18 = recode(P5_18, "1" = "Su abogado defensor",
                        "2" = "El Juez",
                        "3" = "El Fiscal o Agente del Ministerio Público",
                        "4" = "El Fiscal o Agente del Ministerio Público y el defensor por igual",
                        "5" = "El secretario de acuerdos",
                        "8" = "No sabe",
                        "9" = "No responde"),
         P5_25 = recode(P5_25, "1" = "Antes del juicio",
                        "2" = "Después de que le presentaron las pruebas",
                        "3" = "Nunca vio a ningún juez durante su proceso",
                        "8" = "No sabe",
                        "9" = "No responde"),
         Mes_anio_detencion = paste0(P3_5_M,"-",P3_5_A))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Create excel files                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Detencion

for (i in Detencion_a) {
  
  # Get individual variables from questions (whenever a question maps into separate variables)
  i_1 <- grep(i, Detencion, value = TRUE)
  
  # Initialize dataframe
  df_i<-c()
  
  for (j in i_1) {
    
    # Calculate proportions for individual variables
    
    df_j <- Main_database %>% with(., table(get(j))) %>%
      prop.table()
    cj <- tibble(j)
    df_j <- cbind(cj,df_j)  %>% pivot_wider(., names_from = Var1, values_from = Freq) %>% rename(Pregunta = j)
    
    # Create one single dataframe from each question (by binding the proportions for each variable that
    # stems from he question), replacing NAs with 0 (here a NA value indicates nobody selected that option).
    
    df_i <- bind_rows(df_i,df_j)  %>% replace(is.na(.), 0)
    
    if (j == "P3_1") {
      
      break
      
    }else {
      
      next
      
    }
  }
  
  
  # Writing the excel file
  
  write.xlsx(as.data.frame(df_i), 
             file      = file.path(paste0(path2DB, 
                                          "/National/Descriptives/Output/Detenciones/",
                                          "Detencion",".xlsx"),
                                   fsep = "/"),  
             sheetName = paste0(i),
             append    = T,
             row.names = F)
}





# MP

for (i in MP_a) {
  
  # Get individual variables from questions (whenever a question maps into separate variables)
  i_1 <- grep(i, MP, value = TRUE)
  
  # Initialize dataframe
  df_i<-c()
  
  for (j in i_1) {
    
    # Calculate proportions for individual variables
    
    df_j <- Main_database %>% with(., table(get(j))) %>%
      prop.table()
    cj <- tibble(j)
    df_j <- cbind(cj,df_j)  %>% pivot_wider(., names_from = Var1, values_from = Freq) %>% rename(Pregunta = j)
    
    # Create one single dataframe from each question (by binding the proportions for each variable that
    # stems from he question), replacing NAs with 0 (here a NA value indicates nobody selected that option).
    
    df_i <- bind_rows(df_i,df_j) %>% replace(is.na(.), 0)
    
    if (j == "P4_3") {
      
      break
      
    }else {
      
      next
      
    }
  }
  
  
  # Writing the excel file
  
  write.xlsx(as.data.frame(df_i), 
             file      = file.path(paste0(path2DB,
                                          "/National/Descriptives/Output/MP/",
                                          "MP",".xlsx"),
                                   fsep = "/"),  
             sheetName = paste0(i),
             append    = T,
             row.names = F)
}





# Proceso

for (i in Proceso_a) {
  
  # Get individual variables from questions (whenever a question maps into separate variables)
  i_1 <- grep(i, Proceso, value = TRUE)
  
  # Initialize dataframe
  df_i<-c()
  
  for (j in i_1) {
    
    # Calculate proportions for individual variables
    
    df_j <- Main_database %>% with(., table(get(j))) %>%
      prop.table()
    cj <- tibble(j)
    df_j <- cbind(cj,df_j)  %>% 
      pivot_wider(., names_from = Var1, values_from = Freq) %>% rename(Pregunta = j)
    
    # Create one single dataframe from each question (by binding the proportions for each variable that
    # stems from he question), replacing NAs with 0 (here a NA value indicates nobody selected that option).
    
    df_i <- bind_rows(df_i,df_j) %>% replace(is.na(.), 0)
    
    
  }
  
  
  # Writing the excel file
  
  write.xlsx(as.data.frame(df_i), 
             file      = file.path(paste0(path2DB,
                                          "/National/Descriptives/Output/Jueces/",
                                          "Proceso",".xlsx"),
                                   fsep = "/"),  
             sheetName = paste0(i),
             append    = T,
             row.names = F)
}

