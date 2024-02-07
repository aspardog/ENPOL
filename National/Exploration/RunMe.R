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

Main_database <- clean_columns.fn(Main_database, capacidad_legal)

for (i in capacidad_legal) {
  tabla_excel_fn(var_prop = i, var1 = NA, var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "capacidad_legal", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en ",i))
}

for (i in capacidad_legal) {
  tabla_excel_fn(var_prop = i, var1 = "Estado_arresto", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "capacidad_legal_por_estado", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en ",i))
}

for (i in capacidad_legal) {
  tabla_excel_fn(var_prop = i, var1 = "Delito_unico_categ", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "capacidad_legal_por_delito", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron sí en ",i))
}

for (i in capacidad_legal) {
  tabla_excel_fn(var_prop = i, var1 = "SEXO.x", var2 = NA , var3 = NA, 
                 varfilter = NA, filtervalue = NA, 
                 carpeta = "Legalidad", seccion = "capacidad_legal", nombre = paste0("delito_",i),
                 Dato = paste0("Proporción de personas que reportaron sí en ",i))
}

data2plot <- set_data.fn(Main_database, capacidad_legal, labels)

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
                              title = "Información / Capacidad legal")
barChart

ggsave(plot   = barChart,
       file   = paste0(path2SP, "/National/Exploration/Input/Debido_proceso/Legalidad","/desc_","capacidad_legal.svg"), 
       width  = 300, 
       height = 390,
       units  = "mm",
       dpi    = 72,
       device = "svg")

### 1.1.2. Derecho a la no discriminación -----------------------------------

### 1.1.3. Defensa adecuada -----------------------------------

### 1.1.4. Presunción de inocencia ------------------------------------------

### 1.1.5. Justicia pronta ------------------------------------------

### 1.1.6. Imparcialidad ------------------------------------------


# 1.2. Uso arbitrario de la autoridad ------------------------------------------

### 1.2.1. Uso excesivo de la fuerza  ----------------------------------------------------

### 1.2.2. Corrupción  ----------------------------------------------------


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

