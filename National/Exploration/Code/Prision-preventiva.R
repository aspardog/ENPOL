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
          axis.text.x       =element_text(size=7))
  
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
                   label = figure
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
## 2. GRÁFICOS                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Proceso no en libertad ---------------------------------------------------

Main_database_2008 <-  Main_database_2008 %>% 
                      mutate(proceso_no_en_libertad_character = case_when( proceso_no_en_libertad == 1 ~ "Proceso privado de la libertad", 
                                                                 proceso_no_en_libertad == 0 ~ "Proceso en libertad",
                                                                 T ~ NA_character_))


data2plot <- count_frequency.fn(Main_database_2008$proceso_no_en_libertad_character)


barChart <- BarSimpleChartViz(fill_colors = c("#3273ff","#003B88")) + expand_limits(y = c(0, 120))
barChart



# PP vs. PPO vs. Libertad --------------------------------------------------------------


Main_database_2008 <-  Main_database_2008 %>% 
  mutate(tipo_prision_preventiva = case_when(proceso_no_en_libertad == 1  & PPO == 1 ~ "Prisión Preventiva Oficiosa", 
                                             proceso_no_en_libertad == 1  & PPO == 0 ~ "Prisión Preventiva No Oficiosa",
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