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

Main_database_2015 <- Main_database %>% 
  filter(Anio_arresto >= 2015,
         NSJP == 1)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Debido proceso                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# 1.1 Legalidad ---------------------------------------------------------------

### 1.1.1. Capacidad legal  ----------------------------------------------------

capacidad_legal <- c("P3_14_4", 
                     "P3_14_5",
                     "P3_14_6",
                     "P4_1_03",
                     "P4_1_04", 
                     "P4_1_13",
                     "P4_1_14",
                     "P4_1_15",
                     "P4_1_16",
                     "P4_6_2",
                     "P4_6A_2",
                     "P5_17_1",
                     "P5_17_2",
                     "P5_17_3",
                     "P5_17_4",
                     "P5_20_4",
                     "P5_22_01",
                     "P5_22_02")

labels <- c("Al momento de su detención, ¿el policía o autoridad le dijo por qué lo detuvieron?",
            "Al momento de su detención, ¿el policía o autoridad le informó sobre sus derechos a 
            guardar silencio y a no declarar sin la presencia de su abogado?",
            "Al momento de su detención, ¿el policía o autoridad le dijo a dónde lo llevaría?",
            "Al llegar al MP¿le dijeron de qué le acusaban?",
              "¿el agente del Mp le explicó sus derechos como a guardar silencio, 
            no echarse la culpa, tener abogado, ofrecer pruebas, etc?",
              "Al llegar al MP ¿la autoridad contactó al consulado de su país?",
              "Al llegar al MP ¿necesitaba un traductor por no hablar español?",
              "Al llegar al MP¿tuvo el apoyo de un traductor?",
              "Al llegar al MP¿tuvo el apoyo de alguna persona en 
            la Agencia del MP por no saber leer o escribir?",
              "Al momento de rendir o firmar su declaración ante el M.P.… 
            ¿le leyeron o le dieron a leer su declaración?",
             " Una vez que leyó o le dieron a leer su declaración… ¿entendió su declaración?",
            "En general, ¿qué tan claro fue ... durante las audiencias?
            Su abogado defensor al defenderlo",
            "En general, ¿qué tan claro fue ... durante las audiencias? 
            el juez al explicar por qué tomaba sus decisiones",
            "En general, ¿qué tan claro fue ... durante las audiencias? el Fiscal o MP al acusarlo",
            "En general, ¿qué tan claro fue ... durante las audiencias? la víctima o el abogado de la víctima",
            "Durante las audiencias, ¿usted… podía escuchar lo que se decía?",
             " ¿Alguno de sus abogados defensores… le explicó los hechos por los que se le acusaba?",
              "¿Alguno de sus abogados defensores… le explicó cómo sería su proceso?") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, capacidad_legal) %>%
  mutate(
    P5_2_1 = 
      case_when(
        P5_2_1 == 1 ~ 1,
        P5_2_1 == 2 ~ 0,
        T ~ NA_real_
      )
    )
Main_database_2015 <- clean_columns.fn(Main_database_2015, capacidad_legal) %>%
  mutate(
    P5_2_1 = 
      case_when(
        P5_2_1 == 1 ~ 1,
        P5_2_1 == 2 ~ 0,
        T ~ NA_real_
      )
    )

capacidad_legal <- c(capacidad_legal,
                     "P5_2_1")
labels          <- c(labels,
                     "En su primer encuentro con el juez, ¿le dijo de qué lo acusaban?")

data2plot <- set_data.fn(Main_database_2008, capacidad_legal, labels)

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
                              title = "Información / Capacidad legal a partir de 2008")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","capacidad_legal_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")

data2plot <- set_data.fn(Main_database_2015, capacidad_legal, labels)

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
                              title = "Información / Capacidad legal a partir de 2015")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","capacidad_legal_2015.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")

### 1.1.2. Derecho a la no discriminación -----------------------------------

derecho_no_discriminación <- c("P3_14_5",
                               "P4_3A_2",
                               "P4_3A_7",
                               "P4_6_1",
                               "P5_2_4",
                               "P4_3A_6",
                               "P4_3A_8")

labels <- c("Al momento de su detención, ¿el policía o autoridad le informó sobre sus 
            derechos a guardar silencio y a no declarar sin la presencia de su abogado?",
            "En ese interrogatorio, ¿le explicaron que podía guardar silencio y no responder?",
            "¿usted fue golpeado(a) o maltratado(a) para echarse la culpa o aceptar hechos falsos?",
            "Al momento de rendir o firmar su declaración ante el Ministerio Público… 
            ¿los policías o autoridades lo presionaron a dar otra versión de los hechos?",
            "En su primer encuentro con el juez, ¿le informó sobre su derecho a guardar 
            silencio y a no declarar sin la presencia de su abogado?",
              "¿fue engañado(a) para inculpar a alguien más?",
              "¿usted fue golpeado(a) o maltratado(a) para inculpar a alguien más?") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, derecho_no_discriminación) %>%
  mutate(
    P4_7_4 = 
      case_when(
        as.numeric(P4_7) == 4 ~ 1,
        (as.numeric(P4_7) > 4 & as.numeric(P4_7) < 11) | as.numeric(P4_7) < 4 ~ 0,
        T ~ NA_real_
      ),
    P4_7_5 = 
      case_when(
        as.numeric(P4_7) == 5 ~ 1,
        (as.numeric(P4_7) > 5 & as.numeric(P4_7) < 11) | as.numeric(P4_7) < 5 ~ 0,
        T ~ NA_real_
      ),
    P5_2_4 = 
      case_when(
        P5_2_4 == 1 ~ 1,
        P5_2_4 == 2 ~ 0,
        T ~ NA_real_
      )
  )
Main_database_2015 <- clean_columns.fn(Main_database_2015, derecho_no_discriminación) %>%
  mutate(
    P4_7_4 = 
      case_when(
        as.numeric(P4_7) == 4 ~ 1,
        (as.numeric(P4_7) > 4 & as.numeric(P4_7) < 11) | as.numeric(P4_7) < 4 ~ 0,
        T ~ NA_real_
      ),
    P4_7_5 = 
      case_when(
        as.numeric(P4_7) == 5 ~ 1,
        (as.numeric(P4_7) > 5 & as.numeric(P4_7) < 11) | as.numeric(P4_7) < 5 ~ 0,
        T ~ NA_real_
      ),
    P5_2_4 = 
      case_when(
        P5_2_4 == 1 ~ 1,
        P5_2_4 == 2 ~ 0,
        T ~ NA_real_
      )
  )

derecho_no_discriminación <- c(capacidad_legal,
                               "P4_7_4",
                               "P4_7_5",
                               "P5_2_4")
labels          <- c(labels,
                     "Principal razón por la que se declaró culpable... Porque me presionaron o amenazaron para hacerlo",
                     "Principal razón por la que se declaró culpable... Porque me agredieron físicamente",
                     "En su primer encuentro con el juez, ¿le informó sobre su derecho a guardar silencio y a no declarar sin la presencia de su abogado?")

data2plot <- set_data.fn(Main_database_2008, derecho_no_discriminación, labels)

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
                              title = "Derecho a la no discriminación a partir de 2008")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","derecho_no_discriminación_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


data2plot <- set_data.fn(Main_database_2015, derecho_no_discriminación, labels)

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
                              title = "Derecho a la no discriminación a partir de 2015")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","derecho_no_discriminación_2015.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")

### 1.1.3. Defensa adecuada -----------------------------------

defensa_adecuada <- c("P4_1_05",
                      "P4_1_06",
                      "P4_1_07",
                      "P4_3A_1",
                      "P4_6_3",
                      "P4_14_1",
                      "P5_1",
                      "P5_2_5",
                      "P5_22_03",
                      "P5_22_04",
                      "P5_22_05",
                      "P5_22_06",
                      "P5_22_09",
                      "P5_22_10",
                      "P5_22_11")

labels <- c("¿tuvo la asesoría de un abogado?",
               "¿habló a solas con su abogado?",
               "¿su abogado ofreció pruebas de su inocencia?",
               "En ese interrogatorio, ¿estuvo presente su abogado?",
               "Al momento de rendir o firmar su declaración ante el Ministerio Público… ¿estuvo presente su abogado?",
              " Al momento de ser presentado(a) por la autoridad ante la(s) víctima(s) 
            o testigo(s) para que lo(a) identificaran, … ¿estaba presente su abogado defensor?",
               "Antes de llegar con un juez de lo penal (juez de Control) 
            por primera vez, ¿tuvo la asesoría de un abogado defensor?",
               "En su primer encuentro con el juez (de Control)… ¿estuvo presente su abogado defensor?",
               "¿Algunos de sus abogados defensores… le preguntó si en el momento en que se cometió el delito, usted estaba en otro lugar?",
               "¿Algunos de sus abogados defensores… llamó a comparecer a testigos que apoyaran su caso?",
               "¿Alguno de sus abogados defensores… presentó elementos que permitieron demostrar su inocencia?",
               "¿Alguno de sus abogados defensores preguntó si fabricaron evidencia en su contra?",
               "¿Alguno de sus abogados defensores presentó una apelación?",
               "¿Alguno de sus abogados defensores presentó un juicio de amparo?",
               "¿Alguno de sus abogados defensores contradijo las pruebas que la parte acusadora presentó contra usted?") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, defensa_adecuada)
Main_database_2015 <- clean_columns.fn(Main_database_2015, defensa_adecuada)

data2plot <- set_data.fn(Main_database_2008, defensa_adecuada, labels)

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
                              title = "Defensa adecuada a partir de 2008")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","defensa_adecuada_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


data2plot <- set_data.fn(Main_database_2015, defensa_adecuada, labels)

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
                              title = "Defensa adecuada a partir de 2015")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","defensa_adecuada_2015.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


### 1.1.4. Presunción de inocencia ------------------------------------------


presuncion_inocencia <- c("P5_20_4",
                      "P5_25")

labels <- c("Durante las audiencias, ¿usted… estaba detrás de una reja o vidrio?",
              "¿Siente que el juez le consideraba culpable antes del juicio o 
            después de que le presentaron las pruebas?") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, presuncion_inocencia)
Main_database_2015 <- clean_columns.fn(Main_database_2015, presuncion_inocencia)

data2plot <- set_data.fn(Main_database_2008, presuncion_inocencia, labels)

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
                              title = "Presunción de inocencia a partir de 2008")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","presuncion_inocencia_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


data2plot <- set_data.fn(Main_database_2015, presuncion_inocencia, labels)

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
                              title = "Presunción de inocencia a partir de 2015")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","presuncion_inocencia_2015.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


### 1.1.5. Justicia pronta ------------------------------------------

### 1.1.6. Imparcialidad ------------------------------------------


imparcialidad <- c("P5_19_1",
                   "P5_19_2",
                   "P5_19_3",
                   "P5_20_4",
                   "P5_26",
                   "P5_26A",
                   "P5_26B")

labels <- c("¿Cómo se registraba lo que se decía durante las audiencias?, 
            Una persona escribía en una máquina de escribir o computadora",
            "¿Cómo se registraba lo que se decía durante las audiencias?, 
            Una persona grababa el audio",
            "¿Cómo se registraba lo que se decía durante las audiencias?, 
            Había videograbación",
            "Durante las audiencias, ¿usted… estaba detrás de una reja o vidrio?",
            "¿Qué tanto se sintió escuchado por el juez durante su juicio?",
            "Tomando en cuenta todo el proceso, ¿considera que fue tratado… 
            de manera justa? De manera injusta",
             "Tratando de evaluar que tan justa fue su sentencia, 
            ¿usted diría que fue… muy justa/algo justa/poco justa/nada justa") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, imparcialidad)
Main_database_2015 <- clean_columns.fn(Main_database_2015, imparcialidad)

data2plot <- set_data.fn(Main_database_2008, imparcialidad, labels)

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
                              title = "Imparcialidad a partir de 2008")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","imparcialidad_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


data2plot <- set_data.fn(Main_database_2015, imparcialidad, labels)

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
                              title = "Imparcialidad a partir de 2015")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","imparcialidad_2015.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# 1.2. Uso arbitrario de la autoridad ------------------------------------------

### 1.2.1. Uso excesivo de la fuerza  ----------------------------------------------------

### 1.2.2. Corrupción  ----------------------------------------------------

corrupcion <- c("P3_22_1",
                   "P4_16_1",
                   "P5_46_1")

labels <- c("Liberación a cambio de dinero, etc. en la policía",
            "Liberación a cambio de dinero, etc. en el MP",
            "Liberación a cambio de dinero, etc. en el juzgado") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, corrupcion)
Main_database_2015 <- clean_columns.fn(Main_database_2015, corrupcion)

data2plot <- set_data.fn(Main_database_2008, corrupcion, labels)

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
                              title = "Uso indebido de autoridad a través de corrupción a partir de 2008")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","corrupcion_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


data2plot <- set_data.fn(Main_database_2015, corrupcion, labels)

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
                              title = "Uso indebido de autoridad a través de corrupción a partir de 2015")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","corrupcion_2015.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")



# 1.3. Respeto a los derechos humanos ------------------------------------------

### 1.3.1. Integridad personal  ----------------------------------------------------

### 1.3.2. Libertad personal  ----------------------------------------------------



### 1.3.3. Derecho a un recurso efectivo  ----------------------------------------------------


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Política criminal                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# 2.1. Estrategia Política de Investigación ------------------------------------------

### 2.1.1. Delitos prioritarios con sentencia  ----------------------------------------------------

### 2.1.2. Competencia  ----------------------------------------------------

### 2.1.3. Estrategia de investigación   ----------------------------------------------------

### 2.1.4. Estrategia de persecuión   ----------------------------------------------------

### 2.1.5. Limitación del arbitrio judicial   ----------------------------------------------------


# 2.2. Desempeño institucional ------------------------------------------

### 2.2.1. Carga de trabajo  ----------------------------------------------------

### 2.2.2. Eficiencia  ----------------------------------------------------

### 2.2.3. Eficacia   ----------------------------------------------------

# 2.3. Cordinación ------------------------------------------

### 2.2.1. Contrapesos procesales (defensa)  ----------------------------------------------------

### 2.2.2. Autoridades externas en la investigación  ----------------------------------------------------

### 2.2.3. Abuso de coordinación   ----------------------------------------------------

### 2.2.4. Autoridades externas   ----------------------------------------------------

