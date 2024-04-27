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

# Funtions ----------------------------------------------------------------


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Data2Plot                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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
           labels = str_wrap(Value, width = 25)) 
  frequency_df <- frequency_df %>% mutate(order_var = rank(Value))
  return(frequency_df)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Bar graph categorical                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
    scale_y_discrete() +
    scale_x_discrete( ) +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_blank(),
          axis.title.x       = element_blank(),
          axis.text.y        = element_blank(),
          axis.text.x       =element_text(size=10))
  
  return(plt)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Graph to columns categorical                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


BarCategoricalBars.fn <- function(data = Main_database_2008,
                               column1,
                               column2) {
  # Data manipulation
  data2plot <- data %>%
    select({{column1}}, {{column2}}) %>%
    group_by({{column1}}, {{column2}}) %>%
    summarise(Frequency = n(), .groups = 'drop') %>%
    group_by({{column1}}) %>%
    mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
    ungroup() %>%
    mutate(figure = paste0(round(Percentage, 0), "%"),
           labels = str_wrap(paste({{column1}}, {{column2}}, sep=" - "), width = 20),
           Value = str_wrap({{column1}}, width = 20)) %>%
    filter(complete.cases(.))
  
  
  # Plot
  plot <- ggplot(data2plot,
                 aes(
                   x     = Value, 
                   y     = Percentage,
                   fill  = {{column2}},
                   label =  paste0(figure, "\n", "N =", Frequency),
                 )) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9, position = "dodge") +
    geom_text(aes(y = Percentage + 10), 
              position = position_dodge(width = 0.9),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold", 
              size = 3.514598)  +
    geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
    scale_fill_manual(values = colors4plot) +
    scale_y_continuous(limits = c(0, 105),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%"),
                       position = "right") +
    coord_flip() +
    theme(
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      panel.grid.major   = element_line(size = 0.25,
                                        colour = "#5e5c5a",
                                        linetype = "dashed"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#D0D1D3"),       
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank())
  
  return(list(plot = plot, data = data2plot))
}
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Graph to columns categorical   Names                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


BarCategoricalBarsNames.fn <- function(data = Main_database_2008,
                                  column1,
                                  column2) {
  # Data manipulation
  data2plot <- data %>%
    select({{column1}}, {{column2}}) %>%
    drop_na() %>%
    group_by({{column1}}, {{column2}}) %>%
    summarise(Frequency = n(), .groups = 'drop') %>%
    group_by({{column1}}) %>%
    mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
    ungroup() %>%
    mutate(figure = paste0(round(Percentage, 0), "%"),
           labels = str_wrap(paste({{column1}}, {{column2}}, sep=" - "), width = 20),
           Value = str_wrap({{column1}}, width = 20)) %>%
    filter(complete.cases(.)) %>%
    arrange({{column1}}, desc(Percentage))
  
  
  # Plot
  plot <- ggplot(data2plot,
                 aes(
                   x     = Value, 
                   y     = Percentage,
                   fill  = {{column2}},
                   label =  paste0(figure, ", ", "N =", Frequency, ", ",{{column2}}),
                 )) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9, position = "dodge") +
    geom_text(aes(y = 50), 
              position = position_dodge(width = 0.9),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold", 
              size = 3.514598)  +
    geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
    scale_fill_manual(values = colors4plot) +
    scale_y_continuous(limits = c(0, 105),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%"),
                       position = "right") +
    coord_flip() +
    theme(
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      panel.grid.major   = element_line(size = 0.25,
                                        colour = "#5e5c5a",
                                        linetype = "dashed"),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#D0D1D3"),       
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank())
  
  return(list(plot = plot, data = data2plot))
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Graph to columns categorical several horizontal                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. GRÁFICOS                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Proceso no en libertad (sentenciadas)---------------------------------------------------

Main_database_2008 <-  Main_database_2008 %>% 
                      mutate(proceso_no_en_libertad_character = case_when( proceso_no_en_libertad == 1 ~ "Proceso privado de la libertad", 
                                                                 proceso_no_en_libertad == 0 ~ "Proceso en libertad",
                                                                 T ~ NA_character_))



data2plot <- count_frequency.fn(Main_database_2008$proceso_no_en_libertad_character)


barChart <- BarSimpleChartViz(fill_colors = c("#3273ff","#003B88")) + expand_limits(y = c(0, 120))
barChart

# Proceso no en libertad (sentenciadas y procesadas)---------------------------------------------------

Main_database_2008 <-  Main_database_2008 %>% 
  mutate(tipo_prision_preventiva = case_when(procesado == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             proceso_no_en_libertad == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             procesado ==  1 & PPO == 0 ~ "Prisión Preventiva Justificada",
                                             proceso_no_en_libertad == 1  & PPO == 0 ~ "Prisión Preventiva Justificada",
                                             proceso_no_en_libertad == 0  ~ "Proceso en libertad",
                                             T ~ NA_character_))%>% 
  mutate(proceso_privado_libertad_todos = case_when( tipo_prision_preventiva == "Prisión Preventiva Oficiosa" ~ "Proceso privado de la liberted", 
                                                     tipo_prision_preventiva == "Prisión Preventiva Justificada" ~ "Proceso privado de la liberted", 
                                                     tipo_prision_preventiva == "Proceso en libertad" ~ "Proceso en libertad",
                                                       T ~ NA_character_))




data2plot <- count_frequency.fn(Main_database_2008$proceso_privado_libertad_todos)


barChart <- BarSimpleChartViz(fill_colors = c("#3273ff","#003B88")) + expand_limits(y = c(0, 120))
barChart


# PP vs. PPO vs. Libertad (sentenciados)--------------------------------------------------------------


Main_database_2008 <-  Main_database_2008 %>% 
  mutate(tipo_prision_preventiva = case_when(proceso_no_en_libertad == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             proceso_no_en_libertad == 1  & PPO == 0 ~ "Prisión Preventiva Justificada",
                                             proceso_no_en_libertad == 0  & PPO == 0 ~ "Proceso en libertad",
                                             T ~ NA_character_))

data2plot <- count_frequency.fn(Main_database_2008$tipo_prision_preventiva)


barChart <- BarSimpleChartViz(fill_colors = c("#003B88","#003B88", "#3273ff")) + expand_limits(y = c(0, 100))
barChart

# PP vs. PPO vs. Libertad sentenciadas y procesadas --------------------------------------------------------------


Main_database_2008 <-  Main_database_2008 %>% 
  mutate(tipo_prision_preventiva = case_when(procesado == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             proceso_no_en_libertad == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             procesado ==  1 & PPO == 0 ~ "Prisión Preventiva Justificada",
                                             proceso_no_en_libertad == 1  & PPO == 0 ~ "Prisión Preventiva Justificada",
                                             proceso_no_en_libertad == 0  & PPO == 0 ~ "Proceso en libertad",
                                             T ~ NA_character_))

data2plot <- count_frequency.fn(Main_database_2008$tipo_prision_preventiva)


barChart <- BarSimpleChartViz(fill_colors = c("#003B88","#003B88", "#3273ff")) + expand_limits(y = c(0, 100))
barChart

# PP vs. PPO --------------------------------------------------------------

Main_database_2008 <-  Main_database_2008 %>% 
  mutate(tipo_prision_preventiva = case_when(proceso_no_en_libertad == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             proceso_no_en_libertad == 1  & PPO == 0 ~ "Prisión Preventiva No Oficiosa",
                                             T ~ NA_character_))

data2plot <- count_frequency.fn(Main_database_2008$tipo_prision_preventiva)


barChart <- BarSimpleChartViz(fill_colors = c("#003B88","#003B88")) + expand_limits(y = c(0, 100))
barChart

# PPO por tipo de juicio --------------------------------------------------

Main_database_2008 <- Main_database_2008 %>% 
                      mutate(juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                                          P5_6 == "2" ~ "Procedimiento abreviado o juicio sumario", 
                                                          T ~ NA_character_))




# Use group_by to group data by two columns
data2plot <- Main_database_2008 %>%
  select(tipo_prision_preventiva, juicio_abreviado) %>% 
  group_by(tipo_prision_preventiva, juicio_abreviado) %>%
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(tipo_prision_preventiva) %>%  # Group again by the first column to calculate percentages within each group of column1
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
  ungroup() %>%  # Remove grouping
  mutate(figure = paste0(round(Percentage, 0), "%"),
         labels = str_wrap(paste(tipo_prision_preventiva, juicio_abreviado, sep=" - "), width = 20),
         tipo_prision_preventiva = str_wrap(tipo_prision_preventiva, width = 20)) %>% # Create labels combining both columns
  filter(complete.cases(.))

colors4plot <- c("Juicio" = "#003B88", 
                 "Procedimiento abreviado o juicio sumario" = "#fa4d57")

plot <- ggplot(data2plot,
               aes(
                 x     = (tipo_prision_preventiva), 
                 y     = Percentage,
                 fill  = juicio_abreviado,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = Percentage + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "right") +
  coord_flip() +
  theme(
    panel.background   = element_blank(),
    plot.background    = element_blank(),
    panel.grid.major   = element_line(size     = 0.25,
                                      colour   = "#5e5c5a",
                                      linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3"),       
    axis.title.y       = element_blank(),
    axis.title.x       = element_blank())
plot


# Características sociodemográficas ---------------------------------------

Main_database_2008 <-  Main_database_2008 %>% 
  mutate(tipo_prision_preventiva = case_when(proceso_no_en_libertad == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             proceso_no_en_libertad == 1  & PPO == 0 ~ "Prisión Preventiva No Oficiosa",
                                             T ~ NA_character_))


# Menor a 30 años ---------------------------------------------------------

colors4plot <- c("Prisión Preventiva No Oficiosa" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")

Main_database_2008 <- Main_database_2008 %>% 
                      mutate(Edad_menor30 = case_when(Edad_menor30 == 1 ~ "Menor de 30 años", 
                                                      Edad_menor30 == 0 ~ "Mayor o igual a 30 años",
                                                      T ~ NA_character_))
  
plot <- BarCategoricalBars.fn(column1 = Edad_menor30, column2 = tipo_prision_preventiva )
plot
# Mujer  ---------------------------------------------------------

colors4plot <- c("Prisión Preventiva No Oficiosa" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")


plot <- BarCategoricalBars.fn(column1 = Sexo, column2 = tipo_prision_preventiva )
plot

# Etnia  ---------------------------------------------------------

colors4plot <- c("Prisión Preventiva No Oficiosa" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")

Main_database_2008 <- Main_database_2008 %>% 
  mutate(Etnia = case_when(Etnia == 1 ~ "Pertenece a una etnia", 
                           Etnia == 0 ~ "No pertenece a una etnia",
                                  T ~ NA_character_))

plot <- BarCategoricalBars.fn(column1 = Etnia, column2 = tipo_prision_preventiva )
plot

# Etnia  ---------------------------------------------------------

colors4plot <- c("Prisión Preventiva No Oficiosa" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")

Main_database_2008 <- Main_database_2008 %>% 
  mutate(Etnia = case_when(Etnia == 1 ~ "Se identifica como Afroamericano(a)/indígena", 
                           Etnia == 0 ~ "No se identifica como Afroamericano(a)/indígena",
                           T ~ NA_character_))

plot <- BarCategoricalBars.fn(column1 = Etnia, column2 = tipo_prision_preventiva )
plot

# LGBTQ  ---------------------------------------------------------

colors4plot <- c("Prisión Preventiva No Oficiosa" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")

Main_database_2008 <- Main_database_2008 %>% 
  mutate(LGBTQ = case_when(LGBTQ == 1 ~ "Pertenece a la comunidad LGBTQ", 
                           LGBTQ == 0 ~ "No pertenece a la comunidad LGBTQ",
                           T ~ NA_character_))

plot <- BarCategoricalBars.fn(column1 = LGBTQ, column2 = tipo_prision_preventiva )
plot



# Tiempo en prisión preventiva --------------------------------------------

#procesados meses en prisión preventiva
Main_database_2008 <- Main_database_2008 %>% 
                      mutate(P5_34_A = replace(P5_34_A, P5_34_A %in% c( "98", "99"), NA),
                             P5_34_M = replace(P5_34_M, P5_34_M %in% c( "98", "99"), NA),
                             P5_34_A = replace(P5_34_A, P5_34_A %in% c( "96"), 0),
                             P5_34_M = replace(P5_34_M, P5_34_M %in% c( "96"), 0),
                             P5_10 = replace(P5_10, P5_10 %in% c("8", "9"), NA)) %>% 
                      mutate(procesados_meses_pp = ((as.numeric(P5_34_A)*12) + as.numeric(P5_34_M))) %>% 
                      mutate(mas2anios_prisionpreventiva = case_when(as.numeric(P5_10) == 7 ~ 1,
                                                                     procesados_meses_pp  > 24   ~ 1,
                                                                     as.numeric(P5_10) == 1 | as.numeric(P5_10) == 2 | 
                                                                       as.numeric(P5_10) == 3 | as.numeric(P5_10) == 4 |
                                                                       as.numeric(P5_10) == 5 |as.numeric(P5_10) == 6 ~ 0,
                                                                     procesados_meses_pp  <= 24 ~ 0,
                                                                     T ~ NA_real_)) 
                                                   



Main_database_2008 <- Main_database_2008 %>% 
  mutate(mas2anios_prisionpreventiva = case_when(mas2anios_prisionpreventiva == 1 ~ "Más de 2 años", 
                                                 mas2anios_prisionpreventiva == 0 ~ "Menos, o hasta, 2 años",
                           T ~ NA_character_))

data2plot <- count_frequency.fn(Main_database_2008$mas2anios_prisionpreventiva)

barChart <- BarSimpleChartViz(fill_colors = c("#3273ff","#003B88")) + expand_limits(y = c(0, 120))
barChart


#procesados meses en prisión preventiva 1 año
Main_database_2008 <- Main_database_2008 %>% 
  mutate(P5_34_A = replace(P5_34_A, P5_34_A %in% c( "98", "99"), NA),
         P5_34_M = replace(P5_34_M, P5_34_M %in% c( "98", "99"), NA),
         P5_34_A = replace(P5_34_A, P5_34_A %in% c( "96"), 0),
         P5_34_M = replace(P5_34_M, P5_34_M %in% c( "96"), 0),
         P5_10 = replace(P5_10, P5_10 %in% c("8", "9"), NA)) %>% 
  mutate(procesados_meses_pp = ((as.numeric(P5_34_A)*12) + as.numeric(P5_34_M))) %>% 
  mutate(unoanio_prisionpreventiva = case_when(as.numeric(P5_10) == 6 |  as.numeric(P5_10) == 7  ~ 1,
                                                 procesados_meses_pp  >= 12   ~ 1,
                                                 as.numeric(P5_10) == 1 | as.numeric(P5_10) == 2 | 
                                                   as.numeric(P5_10) == 3 | as.numeric(P5_10) == 4 |
                                                   as.numeric(P5_10) == 5  ~ 0,
                                                 procesados_meses_pp  < 12 ~ 0,
                                                 T ~ NA_real_)) 

Main_database_2008 <- Main_database_2008 %>% 
  mutate(unoanio_prisionpreventiva = case_when(unoanio_prisionpreventiva == 1 ~ "1 año o más", 
                                               unoanio_prisionpreventiva == 0 ~ "Menos de 1 año ",
                                                 T ~ NA_character_))

data2plot <- count_frequency.fn(Main_database_2008$unoanio_prisionpreventiva)

barChart <- BarSimpleChartViz(fill_colors = c("#3273ff","#003B88")) + expand_limits(y = c(0, 120))
barChart



# Tiempo en prisión por tipo de pp ----------------------------------------

colors4plot <- c("Prisión Preventiva Justificada" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")
plot <- BarCategoricalBars.fn(column1 = mas2anios_prisionpreventiva, column2 = tipo_prision_preventiva )
plot



# Dificultad probarotoria --------------------------------------------
Main_database_2008 <-  Main_database_2008 %>% 
  mutate(tipo_prision_preventiva = case_when(procesado == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             proceso_no_en_libertad == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             procesado ==  1 & PPO == 0 ~ "Prisión Preventiva Justificada",
                                             proceso_no_en_libertad == 1  & PPO == 0 ~ "Prisión Preventiva Justificada",
                                             proceso_no_en_libertad == 0  & PPO == 0 ~ "Proceso en libertad",
                                             T ~ NA_character_))

# Confesión ---------------------------------------------------------------


Main_database_2008 <- Main_database_2008 %>% 
                      mutate(prueba_confesion = case_when(P5_15_01 == 1 ~ 1,
                                                          as.numeric(P5_35_01) == 1 ~ 1,
                                                          P5_15_01 == 0 ~ 0,
                                                          as.numeric(P5_35_01) == 2 ~ 0,
                                                          T ~ NA_real_))

Main_database_2008 <- Main_database_2008 %>% 
  mutate(prueba_confesion = case_when(prueba_confesion == 1 ~ "Confesión como prueba", 
                           prueba_confesion == 0 ~ "No confesión como prueba",
                           T ~ NA_character_))

colors4plot <- c("Prisión Preventiva Justificada" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")

plot <- BarCategoricalBars.fn(column1 = prueba_confesion, column2 = tipo_prision_preventiva )
plot


# Declaraciones -----------------------------------------------------------

Main_database_2008 <- Main_database_2008 %>% 
  mutate(prueba_declaraciones = case_when(P5_15_02 == 1  | P5_15_03 == 1 |
                                        P5_15_04 == 1| P5_15_05 == 1 | 
                                        P5_15_06 == 1~ 1,
                                      as.numeric(P5_35_02) == 1 | as.numeric(P5_35_03) == 1 |
                                        as.numeric(P5_35_04) == 1 |as.numeric(P5_35_05) == 1 |
                                        as.numeric(P5_35_06) == 1 ~ 1,
                                      P5_15_02 == 0  | P5_15_03 == 0 |
                                        P5_15_04 == 0| P5_15_05 == 0 | 
                                        P5_15_06 == 0~ 0,
                                      as.numeric(P5_35_02) == 2 | as.numeric(P5_35_03) == 2 |
                                        as.numeric(P5_35_04) == 2 |as.numeric(P5_35_05) == 2 |
                                        as.numeric(P5_35_06) == 2 ~ 0,
                                      as.numeric(P5_35_02) == 3 | as.numeric(P5_35_03) == 3 |
                                        as.numeric(P5_35_04) == 3 |as.numeric(P5_35_05) == 3 |
                                        as.numeric(P5_35_06) == 3 ~ 0,
                                      T ~ NA_real_))

Main_database_2008 <- Main_database_2008 %>% 
  mutate(prueba_declaraciones = case_when(prueba_declaraciones == 1 ~ "Declaraciones como prueba", 
                                          prueba_declaraciones == 0 ~ "No declaraciones como prueba",
                                      T ~ NA_character_))

colors4plot <- c("Prisión Preventiva Justificada" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")

plot <- BarCategoricalBars.fn(column1 = prueba_declaraciones, column2 = tipo_prision_preventiva )
plot


# Física -----------------------------------------------------------

Main_database_2008 <- Main_database_2008 %>% 
  mutate(prueba_fisicas = case_when(P5_15_07 == 1  | P5_15_08 == 1 |
                                            P5_15_09 == 1| P5_15_10 == 1 | 
                                            P5_15_11 == 1~ 1,
                                          as.numeric(P5_35_07) == 1 | as.numeric(P5_35_08) == 1 |
                                            as.numeric(P5_35_09) == 1 |as.numeric(P5_35_10) == 1 |
                                            as.numeric(P5_35_11) == 1 ~ 1,
                                          P5_15_07 == 0  | P5_15_08 == 0 |
                                            P5_15_09 == 0| P5_15_10 == 0 | 
                                            P5_15_11 == 0~ 0,
                                          as.numeric(P5_35_07) == 2 | as.numeric(P5_35_08) == 2 |
                                            as.numeric(P5_35_09) == 2 |as.numeric(P5_35_10) == 2 |
                                            as.numeric(P5_35_11) == 2 ~ 0,
                                          as.numeric(P5_35_09) == 3  ~ 0,
                                          T ~ NA_real_))

Main_database_2008 <- Main_database_2008 %>% 
  mutate(prueba_fisicas = case_when(prueba_fisicas == 1 ~ "Eviencia física como prueba", 
                                    prueba_fisicas == 0 ~ "No evidencia física como prueba",
                                          T ~ NA_character_))

colors4plot <- c("Prisión Preventiva Justificada" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")

plot <- BarCategoricalBars.fn(column1 = prueba_fisicas, column2 = tipo_prision_preventiva )
plot



# Tipo de prueba ----------------------------------------------------------


Main_database_2008 <- Main_database_2008 %>% 
                      mutate(tipo_prueba = case_when(prueba_confesion == 1 & prueba_declaraciones == 0 & prueba_fisicas == 0 ~ "Confesión", 
                                                     prueba_confesion == 0 & prueba_declaraciones == 1 & prueba_fisicas == 0 ~ "Declaraciones",
                                                     prueba_confesion == 0 & prueba_declaraciones == 0 & prueba_fisicas == 1 ~ "Física",
                                                     prueba_confesion == 1 & prueba_declaraciones == 1 & prueba_fisicas == 0 ~ "Confesión y Declaraciones",
                                                     prueba_confesion == 1 & prueba_declaraciones == 0 & prueba_fisicas == 1 ~ "Confesión y Física",
                                                     prueba_confesion == 0 & prueba_declaraciones == 1 & prueba_fisicas == 1 ~ "Declaraciones y Física",
                                                     prueba_confesion == 1 & prueba_declaraciones == 1 & prueba_fisicas == 1 ~ "Todas",
                                                     prueba_confesion == 0 & prueba_declaraciones == 0 & prueba_fisicas == 0 ~ "Ninguna",
                                                     T ~ NA_character_))

colors4plot <- c("Confesión" = "#20204a", 
                 "Confesión y Declaraciones" = "#20204a", 
                 "Confesión y Física" = "#2a2a94", 
                 "Declaraciones" = "#4e43dd", 
                 "Declaraciones y Física" = "#756ef9", 
                 "Física" = "#9c94ff",
                 "Todas" = "#756ef9", 
                 "Ninguna" = "#9c94ff")

plot <- BarCategoricalBarsNames.fn(column1 = tipo_prision_preventiva, column2 = tipo_prueba )
plot


# Tipo de prueba  por tipo de juicio----------------------------------------------------------

Main_database_2008 <- Main_database_2008 %>% 
  mutate(juicio_abreviado = case_when(P5_6 == "1" ~ "Juicio", 
                                      P5_6 == "2" ~ "Procedimiento abreviado o juicio sumario", 
                                      T ~ NA_character_))

colors4plot <- c("Prisión Preventiva Justificada" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")

plot <- BarCategoricalBars.fn(column1 = culpabilidad, column2 = tipo_prision_preventiva )
plot



# Culpabilidad ------------------------------------------------------------

Main_database_2008 <- Main_database_2008 %>% 
  mutate(culpabilidad = case_when(culpabilidad == 1 ~ "Se considera culpable", 
                                  culpabilidad == 0 ~ "Se considera inocente",
                                    T ~ NA_character_))

colors4plot <- c("Prisión Preventiva Justificada" = "#003B88", 
                 "Prisión Preventiva Oficiosa" = "#fa4d57")

plot <- BarCategoricalBars.fn(column1 = culpabilidad, column2 = tipo_prision_preventiva )
plot


# Línea del tiempo ------------------------------------------------------------


data <- as.data.frame(table(Main_database_2008$PPO, Main_database_2008$Anio_arresto))

data <- as.data.frame(table(Main_database_2008$proceso_no_en_libertad, Main_database_2008$Anio_arresto))

# Línea del tiempo ------------------------------------------------------------

