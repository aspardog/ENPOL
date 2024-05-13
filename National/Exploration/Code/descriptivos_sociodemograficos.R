## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration data & hipothesys
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres 
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


# functions ---------------------------------------------------------------

BarSimpleChartViz <- function(data = data2plot, 
                              x_var = labels, 
                              y_var = value2plot, 
                              label_var = figure, 
                              fill_var = Value,
                              fill_colors = fill_colors,
                              order_var = order_var,
                              labels = labels, 
                              Frequency = Frequency
) {
  plt <- ggplot(data, 
                aes(x     = reorder({{x_var}},{{order_var}}), 
                    y     = {{y_var}},
                    label = paste0({{label_var}}, "\n", "N =", {{Frequency}}),
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9) +
    scale_fill_manual(values = {{fill_colors}}) +
    geom_text(aes(y    = {{y_var}} + 7),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    #xlab("Porcentaje de criterios cumplidos")+
    scale_y_discrete(breaks = NULL) +
    scale_x_discrete( ) +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_blank(),
          axis.title.x       = element_blank(),
          axis.text.y        = element_blank())
  
  return(plt)
}


BarSimpleChartVizFlip <- function(data = data2plot, 
                                  x_var = Value, 
                                  y_var = value2plot, 
                                  label_var = figure, 
                                  fill_var = Value,
                                  fill_colors = fill_colors,
                                  order_var = order_var,
                                  labels = Value, 
                                  Frequency = Frequency
) {
  plt <- ggplot(data, 
                aes(x     = reorder({{x_var}},{{order_var}}), 
                    y     = {{y_var}},
                    label = paste0({{label_var}}, ", ", "N =", {{Frequency}}),
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9) +
    scale_fill_manual(values = {{fill_colors}}) +
    geom_text(aes(y    = {{y_var}} + 7),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    #xlab("Porcentaje de criterios cumplidos")+
    coord_flip() +
    scale_y_discrete(breaks = NULL) +
    scale_x_discrete(limits = rev) +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_blank(),
          axis.title.x       = element_blank(),
          axis.text.y        = element_text(hjust = 1, size = 10))
  
  return(plt)
}


count_frequency.fn <- function(column) {
  # Convert the column to a data frame
  data <- data.frame(Value = column) %>% 
          filter(complete.cases(.))
  
  # Count the frequency of each unique value
  frequency_df <- data %>%
    group_by(Value) %>%
    summarise(Frequency = n()) %>% 
    mutate(Value = Value,
           values = Frequency/sum(Frequency),
           value2plot = values * 100,
           figure = paste0(round(value2plot, 0), "%"),
           labels = str_wrap(Value, width = 10)) 
  frequency_df <- frequency_df %>% mutate(order_var = rank(Value))
  return(frequency_df)
}



# graphs categóricas------------------------------------------------------------------


# Sexo --------------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$Sexo)
barChart <- BarSimpleChartViz( fill_colors = c("#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart


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
                                               "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7") ) + expand_limits(y = c(0, 55))
barChart


# País de NAcimiento ------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$P1_4) %>% 
            mutate( labels = case_when( Value == "1" ~ "México",
                                       Value == "2" ~ "Estados Unidos",
                                       Value == "3" ~ "Otro"))


barChart <- BarSimpleChartViz( fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

# Estado de arresto ------------------------------------------------------------

data2plot <- count_frequency.fn(Main_database_2008$Estado_arresto)
barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + coord_flip()
barChart

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

# Estado civil ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$P1_7)  %>% 
  mutate( labels = case_when( Value == "1" ~ "con su pareja en unión libre",
                              Value == "2" ~ "separado(a) de una unión libre",
                              Value == "3" ~ "separado(a) de un matrimonio",
                              Value == "4" ~ "está casado(a)",
                              Value == "5" ~ "está soltero(a)",
                              Value == "6" ~ "está divorciado(a)",
                              Value == "7" ~ "está viudo(a)", 
                              T~NA_character_),
          labels = str_wrap(labels, width = 10)) %>% 
          filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 65))
barChart

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


# PErtenencia indígena ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$P1_15)  %>% 
  mutate( labels = case_when( Value == "1" ~ "afromexicano(a) o afrodescendiente",
                              Value == "2" ~ "indígena",
                              Value == "3" ~ "ninguno",
                              T ~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

# Género ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$P1_22)  %>% 
  mutate( labels = case_when( Value == "1" ~ "hombre",
                              Value == "2" ~ "mujer",
                              Value == "3" ~ "mujer trans",
                              Value == "4" ~ "hombre trans",
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
                            T~ NA_character_))

barChart <- BarSimpleChartViz(fill_colors = c("#312c29","#3d230b", "#49372c","#674f42", "#796250","#95765a",
                                              "#b3987d","#dfc19b", "#e0b8b2","#f0d1cf", "#f9edec")) + expand_limits(y = c(0,60))
barChart

# Color de piel- binaria ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$Colo_piel_claro)  %>% 
  mutate( labels = case_when( Value == 1 ~ "Sí",
                              Value == 0 ~ "No",
                              T ~ NA_character_),
          Value = as.character(Value)) %>% 
  filter(complete.cases(.), 
         Value != "99")


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

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

# PErtenencia étnica ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$Etnia)  %>% 
  mutate( Value = as.character(Value),
          labels = case_when( Value == "1" ~ "Sí",
                              Value == "0" ~ "No",
                              T ~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7")) + expand_limits(y = c(0, 110))
barChart

# PErtenencia menor a 30 años ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$Edad_menor30)  %>% 
  mutate( Value = as.character(Value),
          labels = case_when( Value == "1" ~ "Sí",
                              Value == "0" ~ "No",
                              T ~ NA_character_)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7")) + expand_limits(y = c(0, 110))
barChart


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

# Por categoría del delito ------------------------------------------------------------
PENDIENTE

data2plot <- count_frequency.fn(Main_database_2008$Delito_unico_categ)  %>% 
  mutate( Value = as.character(Value)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartVizFlip(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7",
                                              "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7", "#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 50))
barChart

# Por culpabilidad ------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$culpabilidad)  %>% 
  mutate( labels = case_when( Value == 1 ~ "Se reconoce culpable",
                              Value == 0 ~ "Se reconoce inocente",
                              T ~ NA_character_),
          Value = as.character(Value)) %>% 
  filter(complete.cases(.))


barChart <- BarSimpleChartViz(fill_colors = c("#E2E2F7","#E2E2F7", "#E2E2F7")) + expand_limits(y = c(0, 70))
barChart

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

# Por estado de nacimiento ------------------------------------------------------------


Main_database_2008 <- Main_database_2008 %>% 
                      mutate( estado_nacimiento  = case_when( P1_5 == "01" ~ "AGUASCALIENTES",
                                                              P1_5 == "02" ~ "BAJA CALIFORNIA",
                                                              P1_5 == "03" ~ "BAJA CALIFORNIA SUR",
                                                              P1_5 == "04" ~ "CAMPECHE",
                                                              P1_5 == "05" ~ "COAHUILA",
                                                              P1_5 == "06" ~ "COLIMA",
                                                              P1_5 == "07" ~ "CHIAPAS",
                                                              P1_5 == "08" ~ "CHIHUAHUA",
                                                              P1_5 == "09" ~ "CIUDAD DE MEXICO",
                                                              P1_5 == "10" ~ "DURANGO",
                                                              P1_5 == "11" ~ "GUANAJUATO",
                                                              P1_5 == "12" ~ "GUERRERO",
                                                              P1_5 == "13" ~ "HIDALGO",
                                                              P1_5 == "14" ~ "JALISCO",
                                                              P1_5 == "15" ~ "MEXICO",
                                                              P1_5 == "16" ~ "MICHOACAN",
                                                              P1_5 == "17" ~ "MORELOS",
                                                              P1_5 == "18" ~ "NAYARIT",
                                                              P1_5 == "19" ~ "NUEVO LEON",
                                                              P1_5 == "20" ~ "OAXACA",
                                                              P1_5 == "21" ~ "PUEBLA",
                                                              P1_5 == "22" ~ "QUERETARO",
                                                              P1_5 == "23" ~ "QUINTANA ROO",
                                                              P1_5 == "24" ~ "SAN LUIS POTOSI",
                                                              P1_5 == "25" ~ "SINALOA",
                                                              P1_5 == "26" ~ "SONORA",
                                                              P1_5 == "27" ~ "TABASCO",
                                                              P1_5 == "28" ~ "TAMAULIPAS",
                                                              P1_5 == "29" ~ "TLAXCALA",
                                                              P1_5 == "30" ~ "VERACRUZ",
                                                              P1_5 == "31" ~ "YUCATAN",
                                                              P1_5 == "32" ~ "ZACATECAS",
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


# Vulnerabilidad socioeconómica --------------------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$vulnerabilidad_economica) %>% 
              mutate(Value = case_when(Value == 1 ~ "Vulnerable económicamente", 
                                       Value == 0 ~ "No vulnerable económicamente", 
                                       T ~ NA_character_), 
                     labels = Value)
barChart <- BarSimpleChartViz( fill_colors = c("#E2E2F7","#E2E2F7")) + expand_limits(y = c(0, 110))
barChart
