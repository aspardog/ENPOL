## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            SOCIO-DEMOGRAPHICS DESCRIPTIVES
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres
##
## Dependencies:      World Justice Project
##
## Creation date:     Marzo 11th, 2024
##
## This version:      Marzo 11th, 2024
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


# Sexo --------------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$Sexo)
barChart <- BarSimpleChartViz( fill_colors = c("#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart
ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/sexo.svg"), 
       width  = 95, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Grado escolar -----------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$Escolaridad) %>% 
  mutate( order_var = case_when(Value == "Ninguno" ~ 1,
                                Value == "Preescolar" ~ 2,
                                Value == "Primaria" ~ 3,
                                Value == "Secundaria" ~ 4,
                                Value == "Preparatoria o bachillerato" ~ 5,
                                Value == "Carrera técnica con secundaria" ~ 6,
                                Value == "Carrera técnica con preparatoria" ~ 7,
                                Value == "Normal básica con secundaria" ~ 8,
                                Value == "Licenciatura o profesional" ~ 9,
                                Value == "Maestría o doctorado" ~ 10,
                                T ~ NA_real_))


barChart <- BarSimpleChartViz( fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                               "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                               "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                               "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7") ) + expand_limits(y = c(0, 60))
barChart

ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/grado_escolar.svg"), 
       width  = 200, 
       height = 105,
       units  = "mm",
       dpi    = 72,
       device = "svg")
# País de NAcimiento ------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$P1_4) %>% 
  mutate( labels = case_when( Value == "1" ~ "México",
                              Value == "2" ~ "Estados Unidos",
                              Value == "3" ~ "Otro"))


barChart <- BarSimpleChartViz( fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart
ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/pais_nacimiento.svg"), 
       width  = 95, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# Estado de arresto ------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$Estado_arresto)
barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + coord_flip()
barChart
ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/estado_arresto.svg"), 
       width  = 100, 
       height = 200,
       units  = "mm",
       dpi    = 72,
       device = "svg")
# Años de edad ------------------------------------------------------------

Main_database_2008$AgeBin <- cut(as.numeric(Main_database_2008$P1_3), breaks = seq(from = 5, to = 100, by = 5), right = FALSE)


data2plot <- count_frequency.fn(Main_database_2008$AgeBin)
barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 30))
barChart
ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/edad.svg"), 
       width  = 200, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Tipo de centro penitenciario ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$P1_1)  %>% 
  mutate( labels = case_when( Value == "1" ~ "Centro varonil",
                              Value == "2" ~ "Centro femenil",
                              Value == "3" ~ "Mixto"))

barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 65))
barChart
ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/centro_penitenciario.svg"), 
       width  = 91.37955, 
       height = 76.9697,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Estado civil ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$P1_7)  %>% 
  mutate( labels = case_when( Value == "1" ~ "Con pareja en unión libre",
                              Value == "2" ~ "Separado de unión libre",
                              Value == "3" ~ "Separado de matrimonio",
                              Value == "4" ~ "Casado",
                              Value == "5" ~ "Soltero",
                              Value == "6" ~ "Divorciado",
                              Value == "7" ~ "Viudo", 
                              T~NA_character_),
          labels = str_wrap(labels, width = 10)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 50))
barChart


ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/edo_civil.svg"), 
       width  = 95, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")
# Lengua indígena ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$P1_12)  %>% 
  mutate( labels = case_when( Value == "1" ~ "Sí",
                              Value == "2" ~ "No",
                              T ~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/lengua_indigena.svg"), 
       width  = 95, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")
# PErtenencia indígena ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$P1_15)  %>% 
  mutate( labels = case_when( Value == "1" ~ "Afromexicano(a) o afrodescendiente",
                              Value == "2" ~ "Indígena",
                              Value == "3" ~ "Ninguno",
                              T ~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/pertenencia_indigena.svg"), 
       width  = 200, 
       height = 95,
       units  = "mm",
       dpi    = 76.9697,
       device = "svg")

# Género ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$P1_22)  %>% 
  mutate( labels = case_when( Value == "1" ~ "Hombre",
                              Value == "2" ~ "Mujer",
                              Value == "3" ~ "Mujer trans",
                              Value == "4" ~ "Hombre trans",
                              Value == "5" ~ "Otro",
                              T ~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/genero.svg"), 
       width  = 95, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Preferencia sexual ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$P1_23)  %>% 
  mutate( labels = case_when( Value == "1" ~ "Bisexual",
                              Value == "2" ~ "Homosexual",
                              Value == "3" ~ "Heterosexual",
                              Value == "4" ~ "Otro",
                              T ~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/preferencia_sexual.svg"), 
       width  = 95, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Pertenece a LGBTQ ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$LGBTQ)  %>% 
  mutate( labels = case_when( Value == 1 ~ "Sí",
                              Value == 0 ~ "No",
                              T ~ NA_character_),
          Value = as.character(Value)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

# Pertenece a LGBTQ ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$LGBTQ)  %>% 
  mutate( labels = case_when( Value == 1 ~ "Sí",
                              Value == 0 ~ "No",
                              T ~ NA_character_),
          Value = as.character(Value)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/LGBTQ.svg"), 
       width  = 95, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")
# Color de piel- promedio ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$Color_piel_promedio)  %>% 
  mutate( Value = as.character(Value),
          Value = case_when(Value == "1" ~ "A",
                            Value == "2" ~ "B",
                            Value == "3" ~ "C",
                            Value == "4" ~ "D",
                            Value == "5" ~ "E",
                            Value == "6" ~ "F",
                            Value == "7" ~ "G",
                            Value == "8" ~ "H",
                            Value == "9" ~ "I",
                            Value == "10" ~ "J",
                            Value == "11" ~ "K",
                            T~ NA_character_),
           labels = Value)

barChart <- BarSimpleChartViz(fill_colors = c("#312c29","#3d230b", "#49372c","#674f42", "#796250","#95765a",
                                              "#b3987d","#dfc19b", "#e0b8b2","#f0d1cf", "#f9edec")) + expand_limits(y = c(0,60))
barChart

ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/color_piel.svg"), 
       width  = 200, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# # Color de piel- binaria ------------------------------------------------------------
# 
# 
# data2plot <- count_frequency.fn(Main_database_2008$Colo_piel_claro)  %>% 
#   mutate( labels = case_when( Value == 1 ~ "Sí",
#                               Value == 0 ~ "No",
#                               T ~ NA_character_),
#           Value = as.character(Value)) %>% 
#   filter(complete.cases(.), 
#          Value != "99")
# 
# 
# barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
# barChart



# Mujer --------------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$Sexo) %>% 
  mutate( labels = case_when(Value == "Femenino" ~ "Sí",
                             Value == "Masculino" ~ "No",
                             T ~ NA_character_),
          order_var = case_when(Value == "Femenino" ~ 2,
                                Value == "Masculino" ~ 1,
                                T ~ NA_real_))

barChart <- BarSimpleChartViz( fill_colors = c("#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/mujer.svg"), 
       width  = 91.37955, 
       height = 76.9697,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Pertenencia étnica ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$Etnia)  %>% 
  mutate( Value = as.character(Value),
          labels = case_when( Value == "1" ~ "Sí",
                              Value == "0" ~ "No",
                              T ~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7")) + expand_limits(y = c(0, 110))
barChart


ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/etnia.svg"), 
       width  = 91.37955, 
       height = 76.9697,
       units  = "mm",
       dpi    = 72,
       device = "svg")
# PErtenencia menor a 30 años ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$Edad_menor30)  %>% 
  mutate( Value = as.character(Value),
          labels = case_when( Value == "1" ~ "Sí",
                              Value == "0" ~ "No",
                              T ~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7")) + expand_limits(y = c(0, 110))
barChart


ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/menor_treinta.svg"), 
       width  = 91.37955, 
       height = 76.9697,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# con grado universitario ------------------------------------------------------------


Main_database_2008 <-  Main_database_2008 %>% 
  mutate( con_educacion_universitaria = case_when( Escolaridad == "Ninguno" ~ 0,
                                                   Escolaridad == "Preescolar" ~ 0,
                                                   Escolaridad == "Primaria" ~ 0,
                                                   Escolaridad == "Secundaria" ~ 0,
                                                   Escolaridad == "Normal básica con secundaria" ~ 0,
                                                   Escolaridad == "Carrera técnica con preparatoria" ~ 0,
                                                   Escolaridad == "Carrera técnica con secundaria" ~ 0,
                                                   Escolaridad == "Licenciatura o profesional" ~ 1,
                                                   Escolaridad == "Maestría o doctorado" ~ 1,
                                                   T ~ NA_real_))

data2plot <- count_frequency.fn(Main_database_2008$con_educacion_universitaria)  %>% 
  mutate( Value = as.character(Value),
          labels = case_when( Value == "1" ~ "Sí",
                              Value == "0" ~ "No",
                              T ~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7")) + expand_limits(y = c(0, 110))
barChart


ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/grado_universitario.svg"), 
       width  = 91.37955, 
       height = 76.9697,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# Por categoría del delito ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$Delito_unico_categ)  %>% 
  mutate( Value = as.character(Value),
          order_var = case_when( Value == "amenazas" ~ 15, 
                                 Value == "armas" ~7 , 
                                 Value == "drogas" ~ 6, 
                                 Value == "extorsion" ~ 10, 
                                 Value == "fraude" ~ 13, 
                                 Value == "hom_cul" ~ 8 , 
                                 Value == "hom_dol" ~ 2, 
                                 Value == "lesiones" ~ 12, 
                                 Value == "ns_nr" ~ 14, 
                                 Value == "org" ~ 9, 
                                 Value == "otro" ~ 5 , 
                                 Value == "robos" ~ 1, 
                                 Value == "secuestro" ~ 4, 
                                 Value == "sexuales" ~ 3, 
                                 Value == "viol_fam" ~ 10,
                                 T~ NA_real_),
          Value = case_when( Value == "amenazas" ~ "Amenazas", 
                              Value == "armas" ~ "Armas", 
                              Value == "drogas" ~ "Drogas", 
                              Value == "extorsion" ~ "Extorsión", 
                              Value == "fraude" ~ "Fraude", 
                              Value == "hom_cul" ~ "Homicidio culposo", 
                              Value == "hom_dol" ~ "Homicidio doloso", 
                              Value == "lesiones" ~ "Lesiones", 
                              Value == "ns_nr" ~ "No sabe", 
                              Value == "org" ~ "Crimen organizado", 
                              Value == "otro" ~ "Otro", 
                              Value == "robos" ~ "Robo", 
                              Value == "secuestro" ~ "Secuestro", 
                              Value == "sexuales" ~ "Sexuales", 
                              Value == "viol_fam" ~ "Violencia Familiar",
                              T~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartVizFlip(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                                  "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                                  "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                                  "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                                  "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                                  "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 50))
barChart

ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/delito.svg"), 
       width  = 100, 
       height = 200,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Por culpabilidad ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$culpabilidad)  %>% 
  mutate( labels = case_when( Value == 1 ~ "Se reconoce culpable",
                              Value == 0 ~ "Se reconoce inocente",
                              T ~ NA_character_),
          Value = as.character(Value)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7")) + expand_limits(y = c(0, 70))
barChart


ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/culpabilidad.svg"), 
       width  = 95, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")
# Por sentenciado o procesado ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$sentenciado)  %>% 
  mutate( labels = case_when( Value == 1 ~ "Sentenciado",
                              Value == 0 ~ "Procesado",
                              T ~ NA_character_),
          Value = as.character(Value)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 70))
barChart


ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/sentenciado.svg"), 
       width  = 95, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# Por tuvo COVID ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$P1_24_8)  %>% 
  mutate( labels = case_when( Value == 1 ~ "Sí tuvo COVID",
                              Value == 2 ~ "No tuvo COVID",
                              T ~ NA_character_),
          Value = as.character(Value)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

# Por reporta discapacidad ------------------------------------------------------------

Main_database_2008 <-  Main_database_2008 %>% 
  mutate(discapacidad = case_when(P1_31_1 == "1" ~ 1,
                                  P1_31_2 == "1" ~ 1,
                                  P1_31_3 == "1" ~ 1,
                                  P1_31_4 == "1" ~ 1,
                                  P1_31_1 == "2" ~ 0,
                                  P1_31_2 == "2" ~ 0,
                                  P1_31_3 == "2" ~ 0,
                                  P1_31_4 == "2" ~ 0,
                                  T ~ NA_real_))


data2plot <- count_frequency.fn(Main_database_2008$discapacidad)  %>% 
  mutate( labels = case_when( Value == 1 ~ "Sí",
                              Value == 0 ~ "No",
                              T ~ NA_character_),
          Value = as.character(Value)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/discapcidad.svg"), 
       width  = 95, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Por años de sentencia ------------------------------------------------------------


Main_database_2008$SentenciaBin <- cut(
  as.numeric(Main_database_2008$P5_4_A),
  breaks = c(seq(from = 0, to = 50, by = 5), Inf),
  right = FALSE
)

data2plot <- count_frequency.fn(Main_database_2008$SentenciaBin)
barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 65))
barChart
ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/años_sentencia.svg"), 
       width  = 200, 
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Por estado de nacimiento ------------------------------------------------------------


Main_database_2008 <- Main_database_2008 %>% 
  mutate( estado_nacimiento  = case_when( P1_5 == "01" ~ "Aguascalientes",
                                          P1_5 == "02" ~ "Baja California",
                                          P1_5 == "03" ~ "Baja California Sur",
                                          P1_5 == "04" ~ "Campeche",
                                          P1_5 == "05" ~ "Coahuila",
                                          P1_5 == "06" ~ "Colima",
                                          P1_5 == "07" ~ "Chiapas",
                                          P1_5 == "08" ~ "Chihuahua",
                                          P1_5 == "09" ~ "Ciudad de México",
                                          P1_5 == "10" ~ "Durango",
                                          P1_5 == "11" ~ "Guanajuato",
                                          P1_5 == "12" ~ "Guerrero",
                                          P1_5 == "13" ~ "Hidalgo",
                                          P1_5 == "14" ~ "Jalisco",
                                          P1_5 == "15" ~ "México",
                                          P1_5 == "16" ~ "Michoacán",
                                          P1_5 == "17" ~ "Morelos",
                                          P1_5 == "18" ~ "Nayarit",
                                          P1_5 == "19" ~ "Nuevo León",
                                          P1_5 == "20" ~ "Oaxaca",
                                          P1_5 == "21" ~ "Puebla",
                                          P1_5 == "22" ~ "Querétaro",
                                          P1_5 == "23" ~ "Quintana Roo",
                                          P1_5 == "24" ~ "San Luis Potosí",
                                          P1_5 == "25" ~ "Sinaloa",
                                          P1_5 == "26" ~ "Sonora",
                                          P1_5 == "27" ~ "Tabasco",
                                          P1_5 == "28" ~ "Tamaulipas",
                                          P1_5 == "29" ~ "Tlaxcala",
                                          P1_5 == "30" ~ "Veracruz",
                                          P1_5 == "31" ~ "Yucatán",
                                          P1_5 == "32" ~ "Zacatecas",
                                          T~ NA_character_
  ))

data2plot <- count_frequency.fn(Main_database_2008$estado_nacimiento)
barChart <- BarSimpleChartVizFlip(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                                  "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                                  "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                                  "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                                  "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                                  "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(10, 45))
barChart


ggsave(plot   = barChart,
       file   = paste0( path2SP, "/National/Presentations/INL_ABRIL/Visualizations/estado_nacimiento.svg"), 
       width  = 105, 
       height = 200,
       units  = "mm",
       dpi    = 72,
       device = "svg")




# PARTE 2 -----------------------------------------------------------------

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Prepare prioritary crimes variables                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



Main_database1 <- Main_database %>%
  filter(Anio_arresto >= 2011, Delito_unico == 1) %>%
mutate(Delito_prioritario_ENVIPE = case_when(Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
                                             Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
                                             Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ 1,
                                             Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ 1,
                                             Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ 1,
                                             Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ 1,
                                             T ~ 0),
       Delito_unico_ungrouped_categ = case_when(Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ "Robo de casa habitación",
                                                Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ "Robo de vehículo",
                                                Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ "Robo a negocio",
                                                Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ "Robo en transporte público",
                                                Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ "Robo a transeunte en vía pública",
                                                Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ "Robo de autopartes",
                                                Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ "Robo en forma distinta a las anteriores",
                                                Delito_unico == 1 & (P5_11_08 == 1|P5_31_08 == 1) ~ "Posesión ilegal de drogas",
                                                Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ "Comercio ilegal de drogas",
                                                Delito_unico == 1 & (P5_11_10 == 1|P5_31_10 == 1) ~ "Lesiones",
                                                Delito_unico == 1 & (P5_11_11 == 1|P5_31_11 == 1) ~ "Homicidio culposo",
                                                Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ "Homicidio doloso",
                                                Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ "Portación ilegal de armas",
                                                Delito_unico == 1 & (P5_11_14 == 1|P5_31_14 == 1) ~ "Incumplimiento de obligaciones de asistencia familiar",
                                                Delito_unico == 1 & (P5_11_15 == 1|P5_31_15 == 1) ~ "Violencia familiar",
                                                Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ "Daño a la propiedad",
                                                Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ "Secuestro o secuestro expres",
                                                Delito_unico == 1 & (P5_11_18 == 1|P5_31_18 == 1) ~ "Violación sexual",
                                                Delito_unico == 1 & (P5_11_19 == 1|P5_31_19 == 1) ~ "Fraude",
                                                Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ "Delincuencia organizada",
                                                Delito_unico == 1 & (P5_11_21 == 1|P5_31_21 == 1) ~ "Otros delitos sexuales",
                                                Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ "Exotorsión",
                                                Delito_unico == 1 & (P5_11_23 == 1|P5_31_23 == 1) ~ "Privación de la libertad",
                                                Delito_unico == 1 & (P5_11_24 == 1|P5_31_24 == 1) ~ "Abuso de confianza",
                                                Delito_unico == 1 & (P5_11_25 == 1|P5_31_25 == 1) ~ "Amenazas",
                                                Delito_unico == 1 & (P5_11_26 == 1|P5_31_26 == 1) ~ "Otro",
                                                T ~ NA_character_)) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Get data from ENVIPE                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ENVIPE <- read_dta(paste0(path2DB,"/National/Exploration/Input/Politica_criminal/ocurrencias.dta")) %>%
  pivot_longer(cols = del_1:del_15, names_to = "Delito_envipe",values_to = "Ocurrencias")  %>%
  group_by(anio, Delito_envipe) %>%
  summarise(Ocurrencias = sum(Ocurrencias, na.rm = T)) %>%
  mutate(Delito_envipe = case_when(Delito_envipe == "del_1" ~ "Robo total de vehículo", 
                                   Delito_envipe == "del_2" ~ "Robo de accesorios, refacciones o \n herramientas de vehículos", 
                                   Delito_envipe == "del_3" ~ "Pinta de barda o grafiti en su casa, \n rayones intencionales en su vehículo u otro \n tipo de vandalismo", 
                                   Delito_envipe == "del_4" ~ "Alguien entró a su casa o departamento \n sin permiso mediante el uso de la fuerza o \n por engaños y robó o intentó robar algo", 
                                   Delito_envipe == "del_5" ~ "Robo o asalto en la calle o en el \n transporte público", 
                                   Delito_envipe == "del_6" ~ "Robo en forma distinta a la anterior", 
                                   Delito_envipe == "del_7" ~ "Alguien usó su chequera, número de \n tarjeta o cuenta bancaria sin su permiso para \n realizar cargos o para extraer dinero de sus cuentas \n (fraude bancario) o le dio dinero falso", 
                                   Delito_envipe == "del_8" ~ "Entregó dinero por un producto o un \n servicio que no recibió conforme a lo acordado \n (fraude al consumidor)", 
                                   Delito_envipe == "del_9" ~ "Amenazas, presiones o engaños para \n exigirle dinero o bienes; o para que hiciera algo \n o dejara de hacerlo (extorsión)", 
                                   Delito_envipe == "del_10" ~ "Amenazas verbales de alguien plenamente \n identificado o por escrito hacia su persona \n diciendo que le va a causar un daño a usted, \n a su familia, a sus bienes o su trabajo", 
                                   Delito_envipe == "del_11" ~ "Alguien sólo por actitud abusiva o por \n una discusión lo(a) golpeó, empujó o atacó generándole \n una lesión física (moretones, fracturas, cortadas, etc.)", 
                                   Delito_envipe == "del_12" ~ "Lo secuestraron para exigir dinero o bienes", 
                                   Delito_envipe == "del_13" ~ "Alguien en contra de su voluntad lo(a) \n agredió mediante hostigamiento o intimidación sexual, manoseo, \n exhibicionismo o intento de violación", 
                                   Delito_envipe == "del_14" ~ "Fue obligado(a) mediante violencia física o \n amenaza por alguien conocido o desconocido a tener una \n actividad sexual no deseada (Violación sexual) ", 
                                   Delito_envipe == "del_15" ~ "Otro"))


ENVIPE_prioriotarios <- ENVIPE  %>% 
  arrange(anio, -Ocurrencias) %>%
  group_by(anio) %>%
  slice(1:3)

table(ENVIPE$anio,ENVIPE$Delito_envipe)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Create analysis                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 Delitos ENPOL 2018-2021

data2plot <- Main_database1 %>%
  filter(Anio_arresto >= 2018, !is.na(Delito_unico_ungrouped_categ)) %>%
  group_by(Delito_unico_ungrouped_categ) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "Arrestos",
         Delito = Delito_unico_ungrouped_categ,
         Delito = str_wrap(Delito, width = 30)) %>%
  select(Delito,value2plot,labels,group_var) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito))

colors4plot <- c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7")


plt <- ggplot(data2plot, 
              aes(x     = Delito,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = Delito)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot +.9 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  #xlab("Porcentaje de criterios cumplidos")+
  scale_y_discrete() +
  scale_x_discrete( ) +
  expand_limits(y = c(0, 25))+
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10))+
  coord_flip()

plt



ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure1_1_revised.svg"), 
       width  = 100, 
       height = 225,
       units  = "mm",
       dpi    = 72,
       device = "svg")



## 2 Delitos ENVIPE 2018-2021


data2plot <- ENVIPE %>%
  filter(anio >= 2018) %>%
  group_by(Delito_envipe) %>%
  summarise(n = sum(Ocurrencias)) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "ENVIPE",
         Delito = Delito_envipe,
         Delito = str_wrap(Delito, width = 50)) %>%
  select(Delito,value2plot,labels,group_var) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito))

colors4plot <- c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                 "#E2E2F7","#E2E2F7", "#E2E2F7")

plt <- ggplot(data2plot, 
              aes(x     = Delito,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = Delito)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 4  ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  #xlab("Porcentaje de criterios cumplidos")+
  scale_y_discrete() +
  scale_x_discrete( ) +
  expand_limits(y = c(0, 25))+
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y        = element_text(hjust = 1, size = 10))+
  coord_flip()

plt



ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure1_2_revised.svg"), 
       width  = 100, 
       height = 225,
       units  = "mm",
       dpi    = 72,
       device = "svg")


##3.⁠Delitos ENVIPE en ENPOL, 2018-2021


data2plot <- Main_database1 %>%
  filter(Anio_arresto >= 2018) %>%
  filter(Delito_unico_ungrouped_categ == "Robo de vehículo" |
           Delito_unico_ungrouped_categ == "Robo de autopartes" |
           Delito_unico_ungrouped_categ == "Daño a la propiedad" |
           Delito_unico_ungrouped_categ == "Robo de casa habitación" |
           Delito_unico_ungrouped_categ == "Robo a transeunte en vía pública" |
           Delito_unico_ungrouped_categ == "Robo en transporte público" |
           Delito_unico_ungrouped_categ == "Robo en forma distinta a las anteriores" |
           Delito_unico_ungrouped_categ == "Fraude" |
           Delito_unico_ungrouped_categ == "Extorsión" |
           Delito_unico_ungrouped_categ == "Amenazas" |
           Delito_unico_ungrouped_categ == "Lesiones" |
           Delito_unico_ungrouped_categ == "Secuestro o secuestro expres" |
           Delito_unico_ungrouped_categ == "Otros delitos sexualesl" |
           Delito_unico_ungrouped_categ == "Violación sexual"
  ) %>%
  group_by(Anio_arresto,Delito_prioritario_ENVIPE) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0), "%"),
         group_var =  "Delito_prioritario_ENVIPE",
  ) %>%
  select(Anio_arresto,Delito_prioritario_ENVIPE,value2plot,labels,group_var) %>%
  filter(Delito_prioritario_ENVIPE == 1)



# Creating ggplot

colors4plot <- c("#003B88")

plt <- ggplot(data2plot, 
              aes(x     = Anio_arresto,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = group_var)) +
  geom_point(size = 2,
             show.legend = F) +
  geom_line(size  = 1,
            show.legend = F) +
  geom_text_repel(
    size        = 3.514598,
    show.legend = F,
    
    # Additional options from ggrepel package:
    min.segment.length = 1000,
    seed               = 42,
    box.padding        = 0.5,
    direction          = "y",
    force              = 5,
    force_pull         = 1) +
  scale_y_continuous(limits = c(0, 105),
                     expand = c(0,0),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%")) %>%
  scale_color_manual(values = colors4plot) +
  WJP_theme() +
  expand_limits(y = c(0, 100))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"))



ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Milestone2/Figure1_3.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

