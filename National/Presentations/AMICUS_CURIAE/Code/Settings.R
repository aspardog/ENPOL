# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Settings Presentations
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    Cristina Álvarez Venzor     (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     April  12th, 2024
##
## This version:       April  12th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages

if ('pacman' %in% rownames(installed.packages()) == FALSE) {
  install.packages(pacman,
                   dependencies = TRUE)
}
library(pacman)

p_load(char = c(
  
  # Visualizations
  "showtext", "ggtext", "ggsankey", "ggwaffle", "ggplotify", "gridExtra", "patchwork", "ggh4x", "ggrepel",
  
  # Data Loading
  "haven", "foreign", "openxlsx", "readxl", "writexl", "xlsx",
  
  # Utilities
  "margins", "english", "quarto", "kableExtra", "sysfonts", "magrittr",
  
  # Good 'ol Tidyverse
  "tidyverse"
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Dropbox Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# OnDrive/SharePoint path

# You can find out your username by inputting Sys.info()["user"] on the R Console.

if (Sys.info()["user"] == "marcelo") {
  path2SP <- paste0("/Users/marcelo/Library/CloudStorage/OneDrive-SharedLibraries-WorldJusticeProject/Mexico - ENPOL/Data/")
  
} else if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- paste0("/Users/santiagopardo/OneDrive - World Justice Project/ENPOL/Data")
  
} else if (Sys.info()["user"] == "USER VERO"){
  path2SP <- paste0("PATH VERO")
  
} else if (Sys.info()["user"] == "arturoluna"){
  path2SP <- paste0("/Users/arturoluna/Dropbox/ENPOL/")
  
} else if (Sys.info()["user"] == "cristinaalvarez"){
  path2SP <- paste0("/Users/cristinaalvarez/Library/CloudStorage/OneDrive-WorldJusticeProject/ENPOL/Data/")
  
} else{
  path2SP <- "PLEASE INSERT YOUR PERSONAL PATH TO THE  WJP - DATA ANALYTICS DIRECTORY"
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Fonts                                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Loading fonts
path2fonts <- paste0(path2SP, "6. Country Reports/0. Fonts/")
font_add(family     = "Lato Full",
         regular    = paste0(path2fonts, "Lato-Regular.ttf"),
         italic     = paste0(path2fonts, "Lato-LightItalic.ttf"),
         bold       = paste0(path2fonts, "Lato-Bold.ttf"),
         bolditalic = paste0(path2fonts, "Lato-BoldItalic.ttf"))
font_add(family  = "Lato Light",
         regular = paste0(path2fonts, "Lato-Light.ttf"))
font_add(family  = "Lato Black",
         regular = paste0(path2fonts, "Lato-Black.ttf"))
font_add(family  = "Lato Black Italic",
         regular = paste0(path2fonts, "Lato-BlackItalic.ttf"))
font_add(family  = "Lato Medium",
         regular = paste0(path2fonts, "Lato-Medium.ttf"))
showtext_auto()


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  WJP theme                                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining a ggplot WJP theme
WJP_theme <- function() {
  theme(panel.background   = element_blank(),
        plot.background    = element_blank(),
        panel.grid.major   = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.grid.minor   = element_blank(),
        axis.title.y       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(0, 10, 0, 0)),
        axis.title.x       = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C",
                                          margin   = margin(10, 0, 0, 0)),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C"),
        axis.text.x = element_text(family = "Lato Full",
                                   face   = "plain",
                                   size   = 3.514598*.pt,
                                   color  = "#524F4C"),
        axis.ticks  = element_blank(),
        plot.margin  = unit(c(0, 0, 0, 0), "points")
  ) 
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5.  Color Palette                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mainCOLOR      <- c("#2a2a9A")
countryPalette <- c("#2a2a94", "#a90099", "#3273ff", "#fa4d57", "#9d61f2", "#43a9a7", "#efa700", "#2c6d4f")
binPalette     <- c("#003b8a", "#fa4d57")
barsPalette    <- c("#2a2a9A", "#E2E2F7")
glinesPalette  <- c("#2a2a94", "#a90099", "#3273ff")
rosePalette    <- c("#20204a", "#12006b", "#2e2e95", "#4e43dd", "#756ef9", "#9c94ff", "#b1a6ff",
                    "#cfb3ff", "#e2a4ff", "#f2aadc", "#ffd7f5")


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
                    label = {{label_var}},
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
## 2.  Graph to columns categorical                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


BarCategoricalBars.fn <- function(data = Main_database_2008,
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
    filter(complete.cases(.))
  
  
  # Plot
  plot <- ggplot(data2plot,
                 aes(
                   x     = Value, 
                   y     = Percentage,
                   fill  = {{column2}},
                   label =  figure,
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
      axis.title.x       = element_text(size = 10))
  
  return(list(plot = plot, data = data2plot))
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Graph to columns categorical                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


BarCategoricalBarsVer.fn <- function(data = Main_database_2008,
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
    filter(complete.cases(.))
  
  
  # Plot
  plot <- ggplot(data2plot,
                 aes(
                   x     = Value, 
                   y     = Percentage,
                   fill  = {{column2}},
                   label =  figure,
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
                       position = "left") +
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
  
  return(plot)
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
                   label =  figure,
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
      axis.title.x       = element_text(size = 10))
  
  return(list(plot = plot, data = data2plot))
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6. Convertir variables a numeric                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

clean_columns.fn <- function(data, columnas, numero_interes) {
  data %>%
    mutate(across(all_of(columnas), ~{
      # Convertir a numérico
      columna_numerica <- as.numeric(.)
      
      # Aplicar reglas de recodificación
      if (all(columna_numerica %in% c(1, 0, NA), na.rm = TRUE)) {
        return(columna_numerica)
      } else if (all(columna_numerica %in% c(1, 2, 8, 9, NA), na.rm = TRUE)) {
        return(case_when(
          columna_numerica == 1 ~ 1,
          columna_numerica == 2 ~ 0,
          columna_numerica == 8 | columna_numerica == 9 ~ NA_real_,
          TRUE ~ columna_numerica
        ))
      } else if (all(columna_numerica %in% c(1, 2, 3, 4, 8, 9, NA), na.rm = TRUE)) {
        return(case_when(
          columna_numerica == 1 ~ 1,
          columna_numerica == 2 ~ 1,
          columna_numerica == 3 ~ 0,
          columna_numerica == 4 ~ 0,
          columna_numerica == 8 | columna_numerica == 9 ~ NA_real_,
          TRUE ~ columna_numerica
        ))
      } else {
        return(case_when(
          columna_numerica == numero_interes ~ 1,
          !is.na(columna_numerica) ~ 0,
          TRUE ~ NA_real_
        ))
      }
    }))
}
