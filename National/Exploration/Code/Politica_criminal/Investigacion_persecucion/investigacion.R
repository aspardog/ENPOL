## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration descriptives
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     February 05th, 2024
##
## This version:      February 05th, 2024
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
source("Code/Settings.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)


Main_database_2008 <- Main_database_2008 %>% 
  mutate(P3_10 = case_when(P3_10 == "1" ~ "Flagrancia",
                           P3_10 == "2" ~ "Flagrancia",
                           P3_10 == "3" ~ "Orden de detención",
                           P3_10 == "4" ~ "Inspección",
                           P3_10 == "5" ~ "Otra", 
                           T ~ NA_character_),
         pruebas_forenses = case_when(P4_2_1 == 1 | P4_2_2 == 1 ~ "1",
                                      P4_2_1 == 2 | P4_2_2 == 2 ~ "0",
                                      T ~ NA_character_),
         encontro_objeto_sinsembrar = case_when(P3_12_3 == 1 & P3_12_4 == 0 ~ 1,
                                            P3_12_3 == 1 & P3_12_4 == 1 ~ 0,
                                            P3_12_3 == 0 & P3_12_4 == 1 ~ 0,
                                            P3_12_3 == 0 & P3_12_4 == 0 ~ 0,
                                            T ~ NA_real_
                                            ))



table(Main_database_2008$pruebas_forenses)

# ¿Qué tipo de pruebas ha presentado la parte acusadora contra usted? - procesada

pruebas_procesada <- c("P5_35_01",
                    "P5_35_02",
                    "P5_35_03",
                    "P5_35_04",
                    "P5_35_05",
                    "P5_35_06",
                    "P5_35_07",
                    "P5_35_08",
                    "P5_35_09",
                    "P5_35_10",
                    "P5_35_11")

labels <- c("Su confesión o declaración que usted hizo",
                       "Declaraciones de la víctima",
                       "Declaraciones de testigos",
                       "Declaraciones de conocidos sobre sus antecedentes",
                       "Declaraciones de cómplices",
                       "Declaraciones de otras personas detenidas",
                       "Registros telefónicos, grabaciones, fotografías, textos",
                       "Objetos relacionados con la comisión de un delito como ropa, armas, droga",
                       "Huellas digitales, sangre, cabello dejado en la escena del crimen, ADN",
                       "Las evaluaciones psicológicas realizadas en el Centro de Observación y Clasificación",
                       "Otro tipo de prueba")

Main_database_2008 <- clean_columns.fn(Main_database_2008, pruebas_procesada)

data2plot <- set_data.fn(Main_database_2008, pruebas_procesada, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#003B88"),
                              title = "Tipo de pruebas ha presentado la parte acusadora vs. procesados")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","pruebas_procesados_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")

### Pruebas sentenciados

pruebas_sentenciados <- c("P5_15_01",
                       "P5_15_02",
                       "P5_15_03",
                       "P5_15_04",
                       "P5_15_05",
                       "P5_15_06",
                       "P5_15_07",
                       "P5_15_08",
                       "P5_15_09",
                       "P5_15_10",
                       "P5_15_11")

labels <- c("Su confesión o declaración que usted hizo",
            "Declaraciones de la víctima",
            "Declaraciones de testigos",
            "Declaraciones de conocidos sobre sus antecedentes",
            "Declaraciones de cómplices",
            "Declaraciones de otras personas detenidas",
            "Registros telefónicos, grabaciones, fotografías, textos",
            "Objetos relacionados con la comisión de un delito como ropa, armas, droga",
            "Huellas digitales, sangre, cabello dejado en la escena del crimen, ADN",
            "Las evaluaciones psicológicas realizadas en el Centro de Observación y Clasificación",
            "Otro tipo de prueba")

Main_database_2008 <- clean_columns.fn(Main_database_2008, pruebas_sentenciados)

data2plot <- set_data.fn(Main_database_2008, pruebas_sentenciados, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#003B88"),
                              title = "Tipo de pruebas ha presentado la parte acusadora vs. sentenciados")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","pruebas_sentenciados_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Encontró objeto sin sembrado por delito suceptible a inspección

delitos_inspeccion <- c("Robo_vehiculo",
             "Robo_casa_hab",
             "Robo_negocio",
             "Robo_transporte_pub",
             "Robo_transeunte",
             "Robo_autopartes",
             "Robo_otros",
             "Posesion_drogas",
             "Comercio_drogas",
             "Lesiones",
             "Hom_culposo",
             "Hom_doloso",
             "Portacion_armas",
             "Danio_prop",
             "Secuestro",
             "Violacion_sexual",
             "Fraude",
             "Delincuencia_org",
             "Abuso_de_conf",
             "Otros",
             "No_sabe",
             "No_responde")


Main_database_2008 <- Main_database_2008 %>% 
  mutate(delitos_inspeccion = (case_when(Robo_vehiculo == 1 ~ "Robo_vehiculo",
                                         Robo_casa_hab == 1 ~ "Robo_casa_hab",
                                         Robo_negocio == 1 ~ "Robo_negocio",
                                         Robo_transporte_pub == 1 ~ "Robo_transporte_pub",
                                         Robo_autopartes == 1 ~ "Robo_autopartes",
                                         Posesion_drogas == 1 ~ "Posesion_drogas",
                                         Comercio_drogas == 1 ~ "Comercio_drogas",
                                         Lesiones == 1 ~ "Lesiones",
                                         Hom_culposo == 1 ~ "Hom_culposo",
                                         Hom_doloso == 1 ~ "Hom_doloso",
                                         Portacion_armas == 1 ~ "Portacion_armas",
                                         Danio_prop == 1 ~ "Danio_prop",
                                         Secuestro == 1 ~ "Secuestro",
                                         Violacion_sexual == 1 ~ "Violacion_sexual",
                                         Fraude == 1 ~ "Fraude",
                                         Delincuencia_org == 1 ~ "Delincuencia_org",
                                         Abuso_de_conf == 1 ~ "Abuso_de_conf",
                                         Otros == 1 ~ "Otros",
                                         T ~ NA_character_)))
         
 delitos_inspección <- as.data.frame(table(Main_database_2008$encontro_objeto_sinsembrar, Main_database_2008$delitos_inspeccion, useNA = "always"))

delitos_inspección <- delitos_inspección %>% 
  pivot_wider(
    names_from = Var2,
    values_from = Freq
  )


# Estrategia investigación (Inspecciones efectivas y arbitrarias)

inspeccion_efectiva <- c("P3_12_1",
                          "P3_12_2",
                          "P3_12_3",
                          "P3_12_4",
                          "P3_12_5")

valores(Main_database_2008, inspeccion_efectiva)

labels <- c("Al momento de realizar la inspección, ¿la autoridad… lo desvistió?",
              "Al momento de realizar la inspección, ¿la autoridad…le dijo qué objeto buscaba?",
              "Al momento de realizar la inspección, encontró el objeto que buscaba o algún otro objeto ilegal?",
              "Al momento de realizar la inspección, le sembró algún objeto?",
             " Al momento de realizar la inspección, videograbó la inspección")

Main_database_2008 <- clean_columns.fn(Main_database_2008, inspeccion_efectiva)

data2plot <- set_data.fn(Main_database_2008, inspeccion_efectiva, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#003B88"),
                              title = "Estrategia investigación (Inspecciones efectivas y arbitrarias)")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","estrategia_investigacion_inspecciones_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Estrategia investigación (Señalamiento de un tercero)

señalamiento <- c("P4_10",
                         "P4_11_01",
                         "P4_11_02",
                         "P4_11_03",
                         "P4_11_04",
                         "P4_11_05")

valores(Main_database_2008, señalamiento)

labels <- c("En su caso, ¿hubo alguien que haya dicho que lo(la) vio cometer el delito 
            (sin importar si esta persona mintió, se confundió o dijo la verdad)?",
              "La(s) persona(s) que lo identificó(aron) o señaló(aron),…era un conocido?",
              "La(s) persona(s) que lo identificó(aron) o señaló(aron),…era un desconocido?",
              "La(s) persona(s) que lo identificó(aron) o señaló(aron),…era una persona detenida?",
              "La(s) persona(s) que lo identificó(aron) o señaló(aron),…era la víctima del delito?",
              "La(s) persona(s) que lo identificó(aron) o señaló(aron),…era un policía u otra autoridad?")

Main_database_2008 <- clean_columns.fn(Main_database_2008, señalamiento)

data2plot <- set_data.fn(Main_database_2008, señalamiento, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#003B88"),
                              title = "Estrategia investigación (Señalamiento de un tercero)")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","estrategia_investigacion_señalamientos_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Estrategia investigación (Interrogatorias)

interrogatorio <- c("P3_16",
                  "P4_3")

valores(Main_database_2008, interrogatorio)

labels <- c("Desde su detención y hasta antes de llegar a la Agencia del MP o con un Juez de lo penal, 
  ¿usted fue interrogado(a) por la policía o autoridad para dar información?",
            "Antes de rendir y firmar su declaración, 
            ¿fue interrogado(a) por las autoridades de la Agencia del MP para darles información?")

Main_database_2008 <- clean_columns.fn(Main_database_2008, interrogatorio)

data2plot <- set_data.fn(Main_database_2008, interrogatorio, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#003B88"),
                              title = "Estrategia investigación (interrogatorio)")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","estrategia_investigacion_interrogatorio_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")