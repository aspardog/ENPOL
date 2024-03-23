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



for (i in capacidad_legal) {
  tabla_excel_fn(dataset = Main_database_2008, var_prop = i, var1 = "abogado_publico", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "capacidad_legal", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en ",i))
}

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
    P4_7_4_5 = 
      case_when(
        as.numeric(P4_7) == 4 | as.numeric(P4_7) == 5 ~ 1,
        ((as.numeric(P4_7) > 4 & as.numeric(P4_7) < 11) | as.numeric(P4_7)) |((as.numeric(P4_7) > 5 & as.numeric(P4_7) < 11) | as.numeric(P4_7)) < 5 ~ 0,
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
    P4_7_4_5 = 
      case_when(
        as.numeric(P4_7) == 4 | as.numeric(P4_7) == 5 ~ 1,
        ((as.numeric(P4_7) > 4 & as.numeric(P4_7) < 11) | as.numeric(P4_7)) |((as.numeric(P4_7) > 5 & as.numeric(P4_7) < 11) | as.numeric(P4_7)) < 5 ~ 0,
        T ~ NA_real_
      ),
    P5_2_4 = 
      case_when(
        P5_2_4 == 1 ~ 1,
        P5_2_4 == 2 ~ 0,
        T ~ NA_real_
      )
  )

derecho_no_discriminación <- c(derecho_no_discriminación,
                               "P4_7_4",
                               "P4_7_5",
                               "P4_7_4_5",
                               "P5_2_4")
labels          <- c(labels,
                     "Principal razón por la que se declaró culpable... Porque me presionaron o amenazaron para hacerlo",
                     "Principal razón por la que se declaró culpable... Porque me agredieron físicamente",
                     "Principal razón por la que se declaró culpable... ambas",
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

for (i in derecho_no_discriminación) {
  tabla_excel_fn(dataset = Main_database_2008, var_prop = i, var1 = "abogado_publico", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "derecho_no_discriminación", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en ",i))
}


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

labels <- c("Al momento de llegar a la Agencia del MP, ¿tuvo la asesoría de un abogado?",
            "Al momento de llegar a la Agencia del MP, ¿habló a solas con su abogado?",
            "Al momento de llegar a la Agencia del MP, ¿su abogado ofreció pruebas de su inocencia?",
            "Al momento de llegar a la Agencia del MP, En ese interrogatorio, ¿estuvo presente su abogado?",
            "Al momento de rendir o firmar su declaración ante el MP… ¿estuvo presente su abogado?",
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

Main_database_2015 <- clean_columns.fn(Main_database_2015, defensa_adecuada)
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


for (i in defensa_adecuada) {
  tabla_excel_fn(dataset = Main_database_2008, var_prop = i, var1 = "abogado_publico", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "defensa_adecuada", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en ",i))
}


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

for (i in presuncion_inocencia) {
  tabla_excel_fn(dataset = Main_database_2008, var_prop = i, var1 = "abogado_publico", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "presuncion_inocencia", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en ",i))
}
### 1.1.5. Justicia pronta ------------------------------------------

justicia_pronta <- c("P5_20_4",
                     "P4_19")

for (i in justicia_pronta) {
  tabla_excel_fn(dataset = Main_database_2008, var_prop = i, var1 = NA, var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "justicia_pronta", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en ",i))
}



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


Main_database_2008 <- clean_columns.fn(Main_database_2008, imparcialidad) %>%
  mutate(
    P5_14 = 
      case_when(
        as.numeric(P5_14) == 1 ~ 1,
        as.numeric(P5_14) == 2 ~ 0,
        T ~ NA_real_
      )
  )
Main_database_2015 <- clean_columns.fn(Main_database_2015, imparcialidad) %>%
  mutate(
    P5_14 = 
      case_when(
        as.numeric(P5_14) == 1 ~ 1,
        as.numeric(P5_14) == 2 ~ 0,
        T ~ NA_real_
      )
  )

imparcialidad <- c(imparcialidad,
                   "P5_14")
labels <- c(labels,
           "¿El Juez que lo(a) sentenció fue diferente del primer
           Juez que inició el juicio cuando usted llegó al Centro Penitenciario?")


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

for (i in imparcialidad) {
  tabla_excel_fn(dataset = Main_database_2008, var_prop = i, var1 = "abogado_publico", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "imparcialidad", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en ",i))
}

# 1.2. Uso arbitrario de la autoridad ------------------------------------------

### 1.2.1. Uso excesivo de la fuerza  ----------------------------------------------------

### 1.2.2. Corrupción  ----------------------------------------------------

corrupcion <- c("P3_22_1",
                   "P4_16_1",
                   "P5_46_1")

corrupcion <-  c("P3_21_1",
                 "P3_21_2",
                 "P3_22_1",
                 "P3_22_2",
                 "P3_22_3",
                 "P3_22_4",
                 "P3_23",
                 "P4_15_1",
                 "P4_15_2",
                 "P4_15_3",
                 "P4_16_1",
                 "P4_16_2",
                 "P4_16_3",
                 "P4_16_4",
                 "P4_17",
                 "P5_45_1",
                 "P5_45_2",
                 "P5_45_3",
                 "P5_46_1",
                 "P5_46_2",
                 "P5_46_3",
                 "P5_46_4",
                 "P5_46_5",
                 "P5_46_6",
                 "P5_47")

valores(Main_database_2008, corrupcion)


labels <- c("La policía o autoridad que lo(a) detuvo a cambio de dejarlo(a) ir, ¿LE PIDIÓ DE FORMA DIRECTA dinero",
            "La policía o autoridad que lo(a) detuvo a cambio de dejarlo(a) ir, ¿le INSINUÓ O GENERÓ las condiciones para que le diera dinero" ,
            "Liberación a cambio de dinero, etc. en la policía o autoridad que lo detuvo",
            " No lo(a) agredirían a cambio de dinero en la policí o autoridad que lo detuvoa",
            " No le harían daño a su familia o amigos a cambio de dinero en la policía o autoridad que lo detuvo",
            " Modificarían la versión de los hechos o la evidencia en su contra a cambio de dinero  o en la policía o autoridad que lo detuvo",
            "¿Usted o su familia dieron a la policía o autoridad que lo detuvo el dinero que les pedían?",
            "Durante su estancia en la Agencia del MP, INTENTARON APROPIARSE o LE PIDIERON DE FORMA DIRECTA dinero?",
            "Durante su estancia en la Agencia del MP, las autoridades del MP, ¿una persona que no es autoridad LE PIDIÓ a usted o a algún familiar dinero autoridades de MP?",
            "Durante su estancia en la Agencia del MP, ¿las autoridades del MP, LE INSINUARON O GENERARON las condiciones para que usted o algún familiar proporcionara dinero?",
            "Liberación a cambio de dinero, etc. en el MP",
            "No lo(a) golpearían a cambio de dinero en la agencia del MP ",
            "Modificarían su versión de los hechos a cambio de dinero en la agncia del MP",
            "No le harían daño a su familia o amigos a cambio de dinero en la agencia del MP",
            "¿Usted o su familia dieron a la autoridad el dinero, bien, regalo o hicieron el favor que les pedían ?", 
            "Durante su estancia en el juzgado…  ¿las autoridades INTENTARON APROPIARSE o LE PIDIERON DE FORMA DIRECTA dinero?",
            "Durante su estancia en el juzgado… ¿una tercera persona, abogado o coyote LE PIDIÓ dinero?",
            "Durante su estancia en el juzgado… ¿las autoridades del juzgado le INSINUARON o GENERARON las condiciones para que les proporcionara dinero?" ,
            "Liberación a cambio de dinero, etc. en la estancia en el juzgado",
            "Disminuirían la gravedad de los delitos a cambio de dinero en la estancia en el juzgado ",
            "Modificarían su versión de los hechos a cambio de dinero en la estancia en el juzgado",
            "Acelerarían o harían más lento el proceso a cambio de dinero en la estancia en el juzgado",
            "Permitirían o negarían la presentación de pruebas a cambio de dinero en la estancia en el juzgado",
            "Disminuirían la sentencia a cambio de dinero en la estancia en el juzagdo",
            "¿Usted o su familia dieron a la autoridad el dinero, bien, regalo o hicieron el favor que les pedían?" ) 

Main_database_2008 <- clean_columns.fn(Main_database_2008, corrupcion)

data2plot <- set_data.fn(Main_database_2008, corrupcion, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#E2E2F7","#E2E2F7", "#003B88", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7", "#003B88", "#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#003B88","#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#003B88"),
                              title = "Uso indebido de autoridad a través de corrupción a partir de 2008")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","corrupcion_completa_2008.svg"), 
       width  = 300, 
       height = 650,
       units  = "mm",
       dpi    = 72,
       device = "svg")

Main_database_2015 <- clean_columns.fn(Main_database_2015, corrupcion)

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


for (i in corrupcion) {
  tabla_excel_fn(dataset = Main_database_2008, var_prop = i, var1 = "Estado_arresto", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "corrupción_estado", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en liberación a cambio de dienro ",i))
}

for (i in corrupcion) {
  tabla_excel_fn(dataset = Main_database_2008, var_prop = i, var1 = "Delito_unico_categ", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "corrupción_delito", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en liberación a cambio de dienro ",i))
}

#Por año por autoridad correspondiente

for (i in corrupcion) {
  tabla_excel_fn(dataset = Main_database_2008, var_prop = i, var1 = "Anio_arresto", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "corrupción_años", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en liberación a cambio de dienro ",i))
}


### 1.2.2.1. Graficas finales --------------------------------------------------

Main_database_2008 <- Main_database_2008 %>% 
  mutate( detencion_corrupcion = case_when(P3_21_1 == "1" | P3_21_2 == "1" ~ 1,
                                           P3_21_1 == "2" & P3_21_2 == "2" ~ 0,
                                           T ~ NA_real_),
  mp_corrupcion= case_when(P4_15_1 == "1" | P4_15_3 == "1" ~ 1,
                           P4_15_1 == "2" & P4_15_3 == "2" ~ 0,
                           T ~ NA_real_),
  juzgado_corrupcion= case_when(P5_45_1 == "1" | P5_45_3 == "1" ~ 1,
                                P5_45_1 == "2" & P5_45_3 == "2" ~ 0,
                                T ~ NA_real_),
  corrupcion_general = case_when(detencion_corrupcion == 1 | mp_corrupcion == 1  | juzgado_corrupcion == 1 ~ 1,
                                 detencion_corrupcion == 0 & mp_corrupcion == 0  & juzgado_corrupcion == 0 ~ 0,
                                 T~ NA_real_))
####  Maduración del sistema --------------------------------------------------

## Línea del tiempo

colors4plot <- c("dependent_var" = "#fa4d57",
                 "dependent_cmpl" = "#003B88")

data2plot <- lineChartData.fn(dependent_var = "corrupcion_general")

lineChart <- lineChartViz(data = data2plot)

lineChart

#Por año por momento

momento <- c("detencion_corrupcion", "mp_corrupcion", "juzgado_corrupcion", "corrupcion_general")


for (i in momento) {
  tabla_excel_fn(dataset = Main_database_2008, var_prop = i, var1 = "Anio_arresto", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "corrupción_serie_tiempo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en liberación a cambio de dienro ",i))
}


####  Logit sociodemográfico --------------------------------------------------

data2plot <- logit_dataBase.fn(dependent_var = "corrupcion_general")

logitPlot <- logit_demo_panel(mainData = data2plot)

ggsave(plot   = logitPlot,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","corrupcion-general_sociodemográficos.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

####  Barras nivel corrupción por etapa --------------------------------------------------

corrupcion_autoridad <-  c("detencion_corrupcion",
                     "mp_corrupcion",
                     "juzgado_corrupcion")
labels <-  c("La policía o autoridad que lo detuvo a cambio de dejarlo ir, 
             no golpearlo o no hacer daño a su familia, etc le pidió directamente o 
             insunuó condiciones para que le diera dinero",
             "La autoridad del MP a cambio de dejarlo ir, 
             no golpearlo o no hacer daño a su familia, etc le pidió o 
             insunuó condiciones para que le diera dinero",
             "Autoridades del juzgado a cambio de dejarlo ir, 
             no golpearlo o no hacer daño a su familia, etc le pidió o 
             insunuó condiciones para que le diera dinero")

Main_database_2008 <- clean_columns.fn(Main_database_2008, corrupcion_autoridad)

data2plot <- set_data.fn(Main_database_2008, corrupcion_autoridad, labels)

data2plot <- data2plot %>% mutate(order_var = case_when(Columna == "detencion_corrupcion" ~ 1,
                                                        Columna == "P3_22_1" ~ 2,
                                                        Columna == "P3_22_2" ~ 5,
                                                        Columna == "P3_22_3" ~ 4,
                                                        Columna == "P3_22_2" ~ 3,
                                                        T ~ NA_real_))

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#003B88","#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7"),
                              title = "Casos de corrupción por autoridad correspondiente")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","corrupcion_general_maduracion.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")

####  Barras nivel corrupción por etapa --------------------------------------------------


debido_juzgado <- c("P5_46_1",
                   "P5_46_2",
                   "P5_46_3",
                   "P5_46_4",
                   "P5_46_5",
                   "P5_46_6")

Main_database_2008 <- clean_columns.fn(Main_database_2008, debido_juzgado)


Main_database_2008 <- Main_database_2008 %>% 
  mutate(integridad_autoridad = case_when(P3_22_2 == 1 | P3_22_3 == 1 ~ 1,
                                        P3_22_2 == 0 & P3_22_3 == 0 ~ 0,
                                        T ~ NA_real_),
         integridad_mp= case_when(P4_16_2 == 1 | P4_16_4 == 1 ~ 1,
                                P4_16_2 == 0 & P4_16_4 == 0 ~ 0,
                                T ~ NA_real_ ),
         deibido_juzago = case_when(P5_46_2 == 1 | P5_46_3 == 1 | P5_46_4 == 1 | P5_46_5 == 1 | P5_46_6 == 1 ~ 1,
                                    P5_46_2 == 0 & P5_46_3 == 0 & P5_46_4 == 0 & P5_46_5 == 0 & P5_46_6 == 0 ~ 0,
                                    T ~ NA_real_))

corrupcion_cambio <-  c("P3_22_1",
                     "P4_16_1",
                     "P5_46_1",
                     "integridad_autoridad",
                     "integridad_mp",
                     "P3_22_4",
                     "P4_16_3",
                     "deibido_juzago")

labels <-  c("Autoridad en la detención",
             "Autoridad en el Ministerio Público",
             "Autoridad en el juzgado",
             "Autoridad en la detención",
             "Autoridad en el Ministerio Público",
             "Autoridad en la detención",
             "Autoridad en el Ministerio Público",
             "Autoridad en el juzgado")

category <- c("Libertad",
              "Libertad",
              "Libertad",
              "Integridad",
              "Integridad",
              "Debido Proceso",
              "Debido Proceso",
              "Debido Proceso" )

data2plot <- set_data_multiple.fn(Main_database_2008, corrupcion_cambio, labels, category)

data2plot <- data2plot %>% mutate(order_var = case_when(Columna == "detencion_corrupcion" ~ 1,
                                                        Columna == "P3_22_1" ~ 2,
                                                        Columna == "P3_22_2" ~ 5,
                                                        Columna == "P3_22_3" ~ 4,
                                                        Columna == "P3_22_2" ~ 3,
                                                        T ~ NA_real_))

barChart <- BarSimpleChartViz_grid(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = category,
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#E2E2F7","#F4ECF7", "#D4E6F1","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7"),
                              title = "Elemento de cambio en los actos de corrupción por autoridad")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","corrupcion_nivel_etapa.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")



### 1.2.2.1. Corrupción por subtipo autoridad --------------------------------------------------




corrupcion_det <-  c("detencion_corrupcion",
                     "P3_22_1",
                     "P3_22_2",
                     "P3_22_3",
                     "P3_22_4")
labels <-  c("La policía o autoridad que lo detuvo a cambio de dejarlo ir, 
             no golpearlo o no hacer daño a su familia, etc le pidió o 
             insunuó condiciones para que le diera dinero",
             "Si usted daba dinero, lo dejarían ir",
             "Si usted daba dinero, no lo agrederían",
             "Si usted daba dinero, No le harían daño a su familia o amigos",
             "Si usted daba dinero, Modificarían la versión de los hechos o la evidencia en su contra")

Main_database_2008 <- clean_columns.fn(Main_database_2008, corrupcion_det)

data2plot <- set_data.fn(Main_database_2008, corrupcion_det, labels)

data2plot <- data2plot %>% mutate(order_var = case_when(Columna == "detencion_corrupcion" ~ 1,
                                                        Columna == "P3_22_1" ~ 2,
                                                        Columna == "P3_22_2" ~ 5,
                                                        Columna == "P3_22_3" ~ 4,
                                                        Columna == "P3_22_2" ~ 3,
                                                        T ~ NA_real_))

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#003B88","#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7"),
                              title = "Corrupción en la autoridad que detuvo")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","corrupcion_autoridad_detuvo.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


### 1.2.2.2. Corrupción por subtipo MP --------------------------------------------------

corrupcion_mp <-  c("mp_corrupcion",
                     "P4_16_1",
                     "P4_16_2",
                     "P4_16_3",
                     "P4_16_4")
labels <-  c("La autoridad del MP a cambio de dejarlo ir, 
             no golpearlo o no hacer daño a su familia, etc le pidió o 
             insunuó condiciones para que le diera dinero",
             "Si usted daba dinero, lo dejarían ir",
             "Si usted daba dinero, No lo golpearían",
             "Si usted daba dinero, Modificarían la versión de los hechos",
             "Si usted daba dinero, No le harían daño a su familia o amigos")

Main_database_2008 <- clean_columns.fn(Main_database_2008, corrupcion_mp)

data2plot <- set_data.fn(Main_database_2008, corrupcion_mp, labels)

data2plot <- data2plot %>% mutate(order_var = case_when(Columna == "mp_corrupcion" ~ 1,
                                                        Columna == "P4_16_1" ~ 2,
                                                        Columna == "P4_16_2" ~ 4,
                                                        Columna == "P4_16_3" ~ 3,
                                                        Columna == "P4_16_4" ~ 5,
                                                        T ~ NA_real_))

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#003B88","#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7"),
                              title = "Corrupción en la estancia en el MP")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","corrupcion_cambio_autoridad.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")

### 1.2.2.2. Corrupción por subtipo Juzgado --------------------------------------------------

corrupcion_juzgado <-  c("juzgado_corrupcion",
                         "P5_46_1",
                         "P5_46_2",
                         "P5_46_3",
                         "P5_46_4",
                         "P5_46_5",
                         "P5_46_6")
labels <-  c("Autoridades del juzgado a cambio de dejarlo ir, 
             no golpearlo o no hacer daño a su familia, etc le pidió o 
             insunuó condiciones para que le diera dinero",
             "Si usted daba dinero, lo dejarían en libertad",
             "Si usted daba dinero, Disminuirían la gravedad de los delitos",
             "Si usted daba dinero, Modificarían la versión de los hechos",
             "Si usted daba dinero, Acelerarían o harían más lento el proceso",
             "Si usted daba dinero, Permitirían o negarían la presentación de pruebas",
             "Si usted daba dinero, Disminuirían la sentencia")

Main_database_2008 <- clean_columns.fn(Main_database_2008, corrupcion_juzgado)

data2plot <- set_data.fn(Main_database_2008, corrupcion_juzgado, labels)

data2plot <- data2plot %>% mutate(order_var = case_when(Columna == "juzgado_corrupcion" ~ 1,
                                                        Columna == "P5_46_1" ~ 2,
                                                        Columna == "P5_46_2" ~ 3,
                                                        Columna == "P5_46_3" ~ 5,
                                                        Columna == "P5_46_4" ~ 6,
                                                        Columna == "P5_46_5" ~ 7,
                                                        Columna == "P5_46_6" ~ 4,
                                                        T ~ NA_real_))

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#003B88","#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7"),
                              title = "Corrupción en la estancia en el Juzagdo")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","corrupcion_estancia_juzgado.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# 1.3. Respeto a los derechos humanos ------------------------------------------

### 1.3.1. Integridad personal  ----------------------------------------------------

### 1.3.2. Libertad personal  ----------------------------------------------------


derecho_libertad <- c("P5_9",
                   "P5_26B")

labels <- c("Usted llevó su juicio … privado de la libertad",
            "Tratando de evaluar que tan justa fue su sentencia, ¿usted diría que fue…?") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, derecho_libertad)
Main_database_2015 <- clean_columns.fn(Main_database_2015, derecho_libertad)

data2plot <- set_data.fn(Main_database_2008, derecho_libertad, labels)

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
                              title = "Derecho a la libertad  a partir de 2008")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","derecho_libertad_2008.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")


data2plot <- set_data.fn(Main_database_2015, derecho_libertad, labels)

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
                              title = "Derecho a la libertad a partir de 2015")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","derecho_libertad_2015.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")





### 1.3.3. Derecho a un recurso efectivo  ----------------------------------------------------

### 1.3.4. Tortura  ----------------------------------------------------

### 1.3.4. Traslado_física  ----------------------------------------------------

traslado_fisica <-  c("P3_18_01",
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
                      "P3_18_14")
labels <-  c("¿Ataron su cuerpo; ataron alguna parte de su cuerpo a un objeto?",
             "¿Le impidieron respirar asfixiándolo, ahorcándolo?",
             "¿Le impidieron respirar o metiendo su cabeza en agua o vaciándole agua en la cara (tehuacán)?",
             "¿Le patearon o golpearon con las manos (abiertas o cerradas)?",
             "¿Le golpearon con objetos?",
             "¿Le quemaron (con objetos calientes, fuego u otra sustancia)?",
             "¿Le dieron descargas eléctricas (toques eléctricos, chicharra)?",
             "¿Aplastaron su cuerpo o alguna parte de él con algún objeto o con el cuerpo de otra persona?",
             "¿Le hirieron con algún cuchillo, navaja u otro objeto afilado?",
             "¿Le encajaron agujas en dedos u otra parte del cuerpo?",
             "¿Le hirieron por el disparo de un arma de fuego?",
             "¿Le agredieron mediante acoso sexual, manoseo, exhibicionismo o intento de violación?",
             "¿Le lastimaron sus órganos sexuales?",
             "¿Fue obligado mediante violencia física o amenaza a tener una actividad sexual no deseada?")

Main_database_2008 <- clean_columns.fn(Main_database_2008, traslado_fisica)

data2plot <- set_data.fn(Main_database_2008, traslado_fisica, labels)

data2plot <- data2plot %>% mutate( category = label,
                                     group_var = "Traslado",
                                     order_var = case_when(category == "¿Ataron su cuerpo; ataron alguna parte de su cuerpo a un objeto?" ~ 1,
                                                            category =="¿Le impidieron respirar asfixiándolo, ahorcándolo?"~ 2,
                                                            category =="¿Le impidieron respirar o metiendo su cabeza en agua o vaciándole agua en la cara (tehuacán)?" ~ 3,
                                                            category =="¿Le patearon o golpearon con las manos (abiertas o cerradas)?"~ 4,
                                                            category =="¿Le golpearon con objetos?"~ 5,
                                                            category =="¿Le quemaron (con objetos calientes, fuego u otra sustancia)?"~ 6,
                                                            category =="¿Le dieron descargas eléctricas (toques eléctricos, chicharra)?" ~ 7,
                                                            category =="¿Aplastaron su cuerpo o alguna parte de él con algún objeto o con el cuerpo de otra persona?"~ 8,
                                                            category == "¿Le hirieron con algún cuchillo, navaja u otro objeto afilado?"~ 9,
                                                            category =="¿Le encajaron agujas en dedos u otra parte del cuerpo?"~ 10,
                                                            category =="¿Le hirieron por el disparo de un arma de fuego?"~ 11,
                                                            category =="¿Le agredieron mediante acoso sexual, manoseo, exhibicionismo o intento de violación?"~ 12,
                                                            category =="¿Le lastimaron sus órganos sexuales?"~ 13,
                                                            category == "¿Fue obligado mediante violencia física o amenaza a tener una actividad sexual no deseada?"~ 14,
                                                          T ~ NA_real_))



### 1.3.4. estancia_mp-fisica  ----------------------------------------------------

mp_fisica <-  c("P4_9_01",
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
                "P4_9_14")
labels <-  c("¿Ataron su cuerpo; ataron alguna parte de su cuerpo a un objeto?",
             "¿Le impidieron respirar asfixiándolo, ahorcándolo?",
             "¿Le impidieron respirar o metiendo su cabeza en agua o vaciándole agua en la cara (tehuacán)?",
             "¿Le patearon o golpearon con las manos (abiertas o cerradas)?",
             "¿Le golpearon con objetos?",
             "¿Le quemaron (con objetos calientes, fuego u otra sustancia)?",
             "¿Le dieron descargas eléctricas (toques eléctricos, chicharra)?",
             "¿Aplastaron su cuerpo o alguna parte de él con algún objeto o con el cuerpo de otra persona?",
             "¿Le hirieron con algún cuchillo, navaja u otro objeto afilado?",
             "¿Le encajaron agujas en dedos u otra parte del cuerpo?",
             "¿Le hirieron por el disparo de un arma de fuego?",
             "¿Le agredieron mediante acoso sexual, manoseo, exhibicionismo o intento de violación?",
             "¿Le lastimaron sus órganos sexuales?",
             "¿Fue obligado mediante violencia física o amenaza a tener una actividad sexual no deseada?")

Main_database_2008 <- clean_columns.fn(Main_database_2008, mp_fisica)

data2plot2 <- set_data.fn(Main_database_2008, mp_fisica, labels)

data2plot2 <- data2plot2 %>% mutate( category = label,
                                   group_var = "Ministerio Público",
                                   order_var = case_when(category == "¿Ataron su cuerpo; ataron alguna parte de su cuerpo a un objeto?" ~ 1,
                                                          category =="¿Le impidieron respirar asfixiándolo, ahorcándolo?"~ 2,
                                                          category =="¿Le impidieron respirar o metiendo su cabeza en agua o vaciándole agua en la cara (tehuacán)?" ~ 3,
                                                          category =="¿Le patearon o golpearon con las manos (abiertas o cerradas)?"~ 4,
                                                          category =="¿Le golpearon con objetos?"~ 5,
                                                          category =="¿Le quemaron (con objetos calientes, fuego u otra sustancia)?"~ 6,
                                                          category =="¿Le dieron descargas eléctricas (toques eléctricos, chicharra)?" ~ 7,
                                                          category =="¿Aplastaron su cuerpo o alguna parte de él con algún objeto o con el cuerpo de otra persona?"~ 8,
                                                        category == "¿Le hirieron con algún cuchillo, navaja u otro objeto afilado?"~ 9,
                                                        category =="¿Le encajaron agujas en dedos u otra parte del cuerpo?"~ 10,
                                                        category =="¿Le hirieron por el disparo de un arma de fuego?"~ 11,
                                                        category =="¿Le agredieron mediante acoso sexual, manoseo, exhibicionismo o intento de violación?"~ 12,
                                                        category =="¿Le lastimaron sus órganos sexuales?"~ 13,
                                                        category == "¿Fue obligado mediante violencia física o amenaza a tener una actividad sexual no deseada?"~ 14,
                                             T ~ NA_real_))

data2plot <- bind_rows(data2plot, data2plot2)

data2plot <- data2plot %>% rename(value2plot = PorcentajeUnos) %>% 
  mutate(label =  str_wrap(label, width = 10),
         category = str_wrap(category, width = 10),
         labels = str_wrap(labels, width = 10))

colors4bars <- c("Traslado" = "#003B88", 
                 "Ministerio Público" = "#fa4d57")

categories <-  data2plot$category

barsPlot <- barsChart.fn(data.df = data2plot, 
                         labels_var = "labels", 
                         value2plot = "value2plot", 
                         grouping_var = "group_var", 
                         categories_grouping_var = categories, 
                         label_figures = "figure", 
                         order = T, order_value = "legend_order", 
                         nbars = 14, 
                         colors4plot = colors4bars)


ggsave(plot   = barsPlot,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","tortura_fisica.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")




### 1.3.4. Traslado_psicológica  ----------------------------------------------------

traslado_psicologica <-  c("P3_17_01",
                      "P3_17_02",
                      "P3_17_03",
                      "P3_17_04",
                      "P3_17_06",
                      "P3_17_07",
                      "P3_17_08",
                      "P3_17_09",
                      "P3_17_10",
                      "P3_17_11")
labels <-  c("¿Le amenazaron con levantarle cargos falsos?",
             "¿Le amenazaron con matarlo(a)?",
             "¿Le amenazaron con hacerle daño a usted?",
             "¿Le amenazaron con hacerle daño a su familia?",
             "¿Le presionaron para denunciar a alguien?",
             "¿Le incomunicaron o aislaron?",
             "¿Le pasearon en un automóvil dando vueltas por las calles?",
             "¿Le hicieron daño a su familia?",
             "¿Le desvistieron?",
             "¿Le vendaron los ojos o cubrieran la cabeza para que no viera?")

Main_database_2008 <- clean_columns.fn(Main_database_2008, traslado_psicologica)

data2plot <- set_data.fn(Main_database_2008, traslado_psicologica, labels)

data2plot <- data2plot %>% mutate( category = label,
                                   group_var = "Traslado",
                                   order_var = case_when(category == "¿Le amenazaron con levantarle cargos falsos?" ~ 1,
                                                         category =="¿Le amenazaron con matarlo(a)?"~ 2,
                                                         category =="¿Le amenazaron con hacerle daño a usted?" ~ 3,
                                                         category =="¿Le amenazaron con hacerle daño a su familia?"~ 4,
                                                         category =="¿Le presionaron para denunciar a alguien?"~ 5,
                                                         category =="¿Le incomunicaron o aislaron?"~ 6,
                                                         category =="¿Le pasearon en un automóvil dando vueltas por las calles?" ~ 7,
                                                         category == "¿Le hicieron daño a su familia?"~ 8,
                                                         category == "¿Le desvistieron?"~ 9,
                                                         category =="¿Le vendaron los ojos o cubrieran la cabeza para que no viera?"~ 10,
                                                         T ~ NA_real_))



### 1.3.4. estancia_mp-psicologica  ----------------------------------------------------

mp_psicologica <-  c(
                           "P4_8_2",
                           "P4_8_3",
                           "P4_8_4",
                           "P4_8_5",
                           "P4_8_6",
                           "P4_8_7",
                           "P4_8_8",
                           "P4_8_9",
                           "P4_8_10",
                           "P4_8_11")
labels <-  c("¿Le amenazaron con levantarle cargos falsos?",
             "¿Le amenazaron con matarlo(a)?",
             "¿Le amenazaron con hacerle daño a usted?",
             "¿Le amenazaron con hacerle daño a su familia?",
             "¿Le presionaron para denunciar a alguien?",
             "¿Le incomunicaron o aislaron?",
             "¿Le pasearon en un automóvil dando vueltas por las calles?",
             "¿Le hicieron daño a su familia?",
             "¿Le desvistieron?",
             "¿Le vendaron los ojos o cubrieran la cabeza para que no viera?")

Main_database_2008 <- clean_columns.fn(Main_database_2008, mp_psicologica)

data2plot2 <- set_data.fn(Main_database_2008, mp_psicologica, labels)

data2plot2 <- data2plot2 %>% mutate( category = label,
                                   group_var = "Ministerio Público",
                                   order_var = case_when(category == "¿Le amenazaron con levantarle cargos falsos?" ~ 1,
                                                         category =="¿Le amenazaron con matarlo(a)?"~ 2,
                                                         category =="¿Le amenazaron con hacerle daño a usted?" ~ 3,
                                                         category =="¿Le amenazaron con hacerle daño a su familia?"~ 4,
                                                         category =="¿Le presionaron para denunciar a alguien?"~ 5,
                                                         category =="¿Le incomunicaron o aislaron?"~ 6,
                                                         category =="¿Le pasearon en un automóvil dando vueltas por las calles?" ~ 7,
                                                         category == "¿Le hicieron daño a su familia?"~ 8,
                                                         category == "¿Le desvistieron?"~ 9,
                                                         category =="¿Le vendaron los ojos o cubrieran la cabeza para que no viera?"~ 10,
                                                         T ~ NA_real_))

data2plot <- bind_rows(data2plot, data2plot2)

data2plot <- data2plot %>% rename(value2plot = PorcentajeUnos) %>% 
  mutate(label =  str_wrap(label, width = 10),
         category = str_wrap(category, width = 10),
         labels = str_wrap(labels, width = 10))

colors4bars <- c("Traslado" = "#003B88", 
                 "Ministerio Público" = "#fa4d57")

categories <-  data2plot$category

barsPlot <- barsChart.fn(data.df = data2plot, 
                         labels_var = "labels", 
                         value2plot = "value2plot", 
                         grouping_var = "group_var", 
                         categories_grouping_var = categories, 
                         label_figures = "figure", 
                         order = T, order_value = "legend_order", 
                         nbars = 10, 
                         colors4plot = colors4bars)





ggsave(plot   = barsPlot,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","tortura_psicologica.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Política criminal                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# 2.1. Estrategia Política de Investigación ------------------------------------------

### 2.1.1. Delitos prioritarios con sentencia  ----------------------------------------------------

### 2.1.2. Competencia  ----------------------------------------------------

### 2.1.3. Estrategia de investigación   ----------------------------------------------------

investigacion_inspeccion <-  c("P3_12_1",
                    "P3_12_2",
                    "P3_12_3",
                    "P3_12_4",
                    "P3_12_5")

valores(Main_database_2008, investigacion_inspeccion)


labels <- c("Al momento de realizar la inspección, ¿la autoridad… lo desvistió?",
            "Al momento de realizar la inspección, ¿la autoridad…le dijo qué objeto buscaba?",
            "Al momento de realizar la inspección, encontró el objeto que buscaba o algún otro objeto ilegal?",
            "Al momento de realizar la inspección, le sembró algún objeto?",
            "Al momento de realizar la inspección, videograbó la inspección") 

Main_database_2008 <- clean_columns.fn(Main_database_2008, investigacion_inspeccion)

data2plot <- set_data.fn(Main_database_2008, investigacion_inspeccion, labels)

barChart <- BarSimpleChartViz(data = data2plot, 
                              x_var = labels, 
                              y_var = PorcentajeUnos, 
                              label_var = figure, 
                              fill_var = Columna, 
                              Observaciones = Observaciones,
                              order_var = order_var,
                              fill_colors = c("#E2E2F7","#E2E2F7", "#003B88", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7", "#003B88", "#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#003B88","#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#003B88"),
                              title = "Estrategia investigación (Inspecciones efectivas y arbitrarias) a partir de 2008")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","corrupcion_completa_2008.svg"), 
       width  = 300, 
       height = 650,
       units  = "mm",
       dpi    = 72,
       device = "svg")




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

