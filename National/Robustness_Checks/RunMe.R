## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - RunMe File
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 4, 2024
##
## This version:      June 4, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/settings.R")
source("Code/tendencias_paralelas.R")
# Loading data
load(paste0(path2SP,
            "/National/Data_cleaning/Output/Main_database_completa.RData"))

master_data.df <- Main_database_completa %>% 
  filter(Anio_arresto >= as.numeric(2008)) %>% 
  filter(NSJP == 1) %>%
  filter(Anio_arresto != 9998) %>%
  filter(Anio_arresto != 9999) %>%
  mutate(
    presion_mp = 
      case_when(
        as.numeric(P4_7) == 4 | as.numeric(P4_7) == 5 ~ 1,
        as.numeric(P4_7) < 11 &  (as.numeric(P4_7) != 4 | as.numeric(P4_7) != 5) ~ 0
      ),
    explicacion_mp = 
      case_when(
        P4_1_03 == 1 ~ 1,
        P4_1_03 == 2 ~ 0
      ),
    explicacion_juez = 
      case_when(
        P5_2_1 == 1 ~ 1,
        P5_2_1 == 2 ~ 0
      ),
    claridad_defensor =
      case_when(
        P5_17_1 == 1 | P5_17_1 == 2~ 1,
        P5_17_1 == 3 | P5_17_1 == 4 ~ 0
      ),
    claridad_juez = 
      case_when(
        P5_17_2 == 1 | P5_17_2 == 2~ 1,
        P5_17_2 == 3 | P5_17_2 == 4 ~ 0
      ),
    claridad_mp = 
      case_when(
        P5_17_3 == 1 | P5_17_3 == 2~ 1,
        P5_17_3 == 3 | P5_17_3 == 4 ~ 0
      ),
    rapida = 
      case_when(
        P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 ~ 1,
        P5_10 == 5  | P5_10 == 6 | P5_10 == 7 ~ 0
      ),
    corta = 
      case_when(
        P5_10 == 5 ~ 1,
        P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 | P5_10 == 6 | P5_10 == 7 ~ 0
      ),
    media =
      case_when(
        P5_10 == 6 ~ 1,
        P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 | P5_10 == 5 | P5_10 == 7 ~ 0
      ),
    larga =
      case_when(
        P5_10 == 7 ~ 1,
        P5_10 == 1  | P5_10 == 2 | P5_10 == 3 | P5_10 == 4 | P5_10 == 5 | P5_10 == 6 ~ 0
      ),
    video = 
      case_when(
        P5_19_3 == 1 ~ 1,
        P5_19_3 == 2 ~ 0
      ),
    publico = 
      case_when(
        P5_16_5 == 1 | P5_16_5 == 2 | P5_16_5 == 3 ~ 1,
        P5_16_5 == 4 ~ 0
      ),
    juez_diferente = 
      case_when(
        P5_14 == 1 ~ 1,
        P5_14 == 2 ~ 0
      ),
    juez_presente = 
      case_when(
        P5_16_2 == 1  ~ 1,
        P5_16_2 == 2 | P5_16_2 == 3 | P5_16_2 == 4 ~ 0
      ),
    juez_control = 
      case_when(
        P5_18 == 2 ~ 1,
        P5_18 == 1 | P5_18 == 3 | P5_18 == 4 | P5_18 == 5 ~ 0
      ),
    juez_escucha =
      case_when(
        P5_26 == 1 | P5_26 == 2 ~ 1,
        P5_26 == 3 | P5_26 == 4 ~ 0
      ),
    detencion_corrupcion = case_when(P3_21_1 == "1" | P3_21_2 == "1" ~ 1,
                                     P3_21_1 == "2" & P3_21_2 == "2" ~ 0,
                                     T ~ NA_real_),
    mp_corrupcion = case_when(P4_15_1 == "1" | P4_15_3 == "1" ~ 1,
                             P4_15_1 == "2" & P4_15_3 == "2" ~ 0,
                             T ~ NA_real_),
    juzgado_corrupcion = case_when(P5_45_1 == "1" | P5_45_3 == "1" ~ 1,
                                  P5_45_1 == "2" & P5_45_3 == "2" ~ 0,
                                  T ~ NA_real_),
    corrupcion_general = case_when(detencion_corrupcion == 1 | mp_corrupcion == 1  | juzgado_corrupcion == 1 ~ 1,
                                   detencion_corrupcion == 0 & mp_corrupcion == 0  & juzgado_corrupcion == 0 ~ 0,
                                   T~ NA_real_),
    primer_mp = 
      case_when(
        Primer_lugar_traslado == "Agencia del Ministerio Público" ~ 1,
        T ~ 0
      ),
    primer_policia = 
      case_when(
        Primer_lugar_traslado == "Instalación de la policía" ~ 1,
        T ~ 0 
      ),
    tiempo_30m =
      case_when(
        Tiempo_traslado == "Hasta 30 minutos" ~ 1,
        T ~ 0 
      ),
    tiempo_6h =
      case_when(
        Tiempo_traslado == "Más de 6 horas hasta 24 horas" ~ 1,
        T ~ 0 
      )
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Proceso justo                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Figure 2.1 --------------------------------------------------------------


mainVar <- c("presion_mp", 
             "explicacion_mp", 
             "explicacion_juez", 
             "claridad_defensor", 
             "claridad_juez", 
             "claridad_mp",
             "rapida", "corta", "media", "larga")

figure2_1 <- list(
  Paralalel_trends.pl <- paralel_trends.fn(
    mainVar = mainVar
  )
)
names(figure2_1) <- c("Tendencias paralelas")

### Figure 2.3 --------------------------------------------------------------

mainVar <- c("video", "publico", "juez_diferente", 
             "juez_presente", "juez_control", "juez_escucha")

# Cambia la percepcion de derecho de presunción de inocencia

figure2_3 <- list(
  Paralalel_trends.pl <- paralel_trends.fn(
    mainVar = mainVar
  )
)
names(figure2_3) <- c("Tendencias paralelas")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Corrupcion                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Figure 3.2 --------------------------------------------------------------

mainVar <- c("detencion_corrupcion", "mp_corrupcion", 
             "juzgado_corrupcion", "corrupcion_general")

figure3_2 <- list(
  Paralalel_trends.pl <- paralel_trends.fn(
    mainVar = mainVar
  )
)
names(figure3_2) <- c("Tendencias paralelas")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Detenciones                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Figure 4.2
mainVar <- c("det_ninguna", 
             "primer_mp", "primer_policia", 
             "tiempo_30m", "tiempo_6h")

figure4_2_1 <- list(
  Paralalel_trends.pl <- paralel_trends.fn(
    mainVar = mainVar
  )
)

names(figure4_2_1) <- c("Tendencias paralelas")
