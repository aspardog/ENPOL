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
## Creation date:     January 22th, 2024
##
## This version:      January 22th, 2024
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


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7.  logit database                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

logit_dataBase.fn <- function(data = Main_database,
                              selectables = c("Sexo", 
                                              # "Educacion_obligatoria", 
                                              # "Colo_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30", 
                                              "Ingreso_inseguro"),
                              dependent_var
) {
  
  master_data.df <- Main_database %>%
    #filter(Anio_arresto >= 2015) %>%
    filter(NSJP == 1) %>%
    filter(Delito_unico == 1) %>%
    mutate(
      # Educacion_obligatoria = 
      #   case_when(
      #     Educacion_obligatoria == 1 ~ "Título de bachiller o más",
      #     Educacion_obligatoria == 0 ~ "No cuenta con título de bachiller"
      #   ),
      # Colo_piel_claro       =
      #   case_when(
      #     Colo_piel_claro      == 1 ~ "Color de piel clara",
      #     Colo_piel_claro      == 0 ~ "Color de piel oscura",
      #   ),
      LGBTQ                 = 
        case_when(
          LGBTQ                 == 1 ~ "Pertenece a la comunidad LGBTQ",
          LGBTQ                 == 0 ~ "No pertenece a la comunidad LGBTQ"
        ),
      Etnia                 =
        case_when(
          Etnia                 == 1 ~ "Afroamericano o indígena",
          Etnia                 == 0 ~ "No se identifica con ninguna etnia"
        ),
      Ingreso_inseguro      =
        case_when(
          Ingreso_inseguro      == 1 ~ "Financieramente inseguro",
          Ingreso_inseguro      == 0 ~ "Financieramente seguro"
        ),
      Edad_menor30          =
        case_when(
          Edad_menor30          == 1 ~ "Menor a 30 años",
          Edad_menor30          == 0 ~ "Mayor a 30 años"
        )
    )
  
  selectables <- selectables
  
  logit_data <- master_data.df %>%
    select(all_of(selectables),
           all_of(dependent_var)) %>%
    mutate(
      # Educacion_obligatoria =
      #   if_else(
      #     Educacion_obligatoria %in% "No cuenta con título de bachiller",
      #     "ZNo cuenta con título de bachiller", Educacion_obligatoria
      #   ),
      Sexo                  =
        if_else(
          Sexo %in% "Femenino",
          "ZFemenino", Sexo
        ),
      # Colo_piel_claro       =
      #   if_else(
      #     Colo_piel_claro %in% "Color de piel oscura",
      #     "ZColor de piel oscura", Colo_piel_claro
      #   ),
      LGBTQ                 =
        if_else(
          LGBTQ %in% "Pertenece a la comunidad LGBTQ",
          "ZPertenece a la comunidad LGBTQ", LGBTQ
        ),
      Etnia                 =
        if_else(
          Etnia %in% "Afroamericano o indígena",
          "ZAfroamericano o indígena", Etnia
        ),
      Ingreso_inseguro      =
        if_else(
          Ingreso_inseguro %in% "Financieramente inseguro",
          "ZFinancieramente inseguro", Ingreso_inseguro
        ),
      Edad_menor30          =
        if_else(
          Edad_menor30 %in% "Menor a 30 años",
          "ZMenor a 30 años", Edad_menor30
        ),
    ) %>%
    arrange(Sexo, LGBTQ, Etnia, Edad_menor30, Ingreso_inseguro)
  
  formula <- selectables %>%
    t() %>%
    as.data.frame() %>%
    unite(., formula, sep = "+") %>%
    as.character()
  
  depVar <- dependent_var
  
  formula  <- as.formula(paste(depVar, "~", formula))
  logit    <- glm(formula,
                  data   = logit_data, 
                  family = "binomial")
  
  summaryLogit <- bind_rows(
    as.data.frame(coef(logit))
  )
  
  margEff      <- as.data.frame(
    margins_summary(logit, data = logit$data)
  )
  
  margEff$factor <-recode(margEff$factor,
                          "SexoZFemenino" = "Mujer",
                          "LGBTQZPertenece a la comunidad LGBTQ"                     = "Perteneciente a \ncomunidad \nLGBTQ",
                          "Ingreso_inseguroZFinancieramente inseguro"                = "Financieramente \ninseguro/a",
                          "EtniaZAfroamericano o indígena"                           = "Afroamericano/a \nó indígena",
                          # "Educacion_obligatoriaZNo cuenta con título de bachiller"  = "Sin diploma bachiller",
                          "Edad_menor30ZMenor a 30 años"                             = "Menor a 30 años"
                          # "Colo_piel_claroZColor de piel oscura"                     = "Color de piel oscura"
  )
  
  data2table <- margEff %>%
    mutate(order_variable =
             case_when(
               factor == "Mujer"                              ~ 1,
               factor == "Perteneciente a \ncomunidad \nLGBTQ"  ~ 2,
               factor == "Menor a 30 años"                    ~ 3,
               # factor == "Sin diploma bachiller"              ~ 4,
               factor == "Financieramente \ninseguro/a"         ~ 4,
               factor == "Afroamericano/a \nó indígena"         ~ 5
               # factor == "Color de piel oscura"               ~ 7
             ),
           dependent_var  =
             dependent_var
    )
  
  return(data2table)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7.  logit Chart                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

logit_demo_panel_min <- function(mainData = data2plot,
                                 line_color = "#003b8a",
                                 line_size  = 2,
                                 point_color = "#003b8a",
                                 point_size   = 4) {
  
  plot <- ggplot(mainData, aes(x = reorder(factor, -order_variable), y = AME)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#fa4d57", alpha = 0.1) + # Red shadow
    geom_hline(yintercept = 0, lty = 1, color = "#fa4d57", lwd = 1) +
    geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),
                   lwd = line_size, position = position_dodge(width = .7), 
                   stat = "identity", color = line_color) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = point_size, position = position_dodge(width = .7), color = point_color) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = 2, position = position_dodge(width = .7), color = "white") +
    labs(y = "Menos probable        Más probable") +
    scale_y_continuous(limits = c(-0.15, 0.15),
                       breaks = seq(-0.12, 0.12, by = 0.06),
                       expand = expansion(mult = 0.025), position = "right",
                       labels = c("-10 p.p.", "-5 p.p.", "0", "+5 p.p.", "+10 p.p.")) +
    WJP_theme() +
    coord_flip() +
    theme(legend.position = "none",
          panel.background   = element_blank(),
          panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                            size = 0.5, linetype = "dashed"),
          legend.title = element_blank(),
          axis.title.y       = element_blank(),
          axis.text.y        = element_text(family = "Lato Medium",
                                            size     = 3.514598*.pt,
                                            color    = "#4a4a49",
                                            hjust    = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.grid.minor.y = element_blank())
  
  return(plot)
}

logit_demo_panel_max <- function(mainData = data2plot,
                                 line_color = "#003b8a",
                                 line_size  = 2,
                                 point_color = "#003b8a",
                                 point_size   = 4) {
  
  plot <- ggplot(mainData, aes(x = reorder(factor, -order_variable), y = AME)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#43a9a7", alpha = 0.1) + # Red shadow
    geom_hline(yintercept = 0, lty = 1, color = "#fa4d57", lwd = 1) +
    geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),
                   lwd = line_size, position = position_dodge(width = .7), 
                   stat = "identity", color = line_color) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = point_size, position = position_dodge(width = .7), color = point_color) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = 2, position = position_dodge(width = .7), color = "white") +
    labs(y = "Menos probable                               Más probable") +
    scale_y_continuous(limits = c(-0.15, 0.15),
                       breaks = seq(-0.10, 0.10, by = 0.05),
                       expand = expansion(mult = 0.025), position = "right",
                       labels = c("-10 p.p.", "-5 p.p.", "0", "+5 p.p.", "+10 p.p.")) +
    WJP_theme() +
    coord_flip() +
    theme(legend.position = "none",
          panel.background   = element_blank(),
          panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                            size = 0.5, linetype = "dashed"),
          legend.title = element_blank(),
          axis.title.y       = element_blank(),
          axis.text.y        = element_text(family = "Lato Medium",
                                            size     = 3.514598*.pt,
                                            color    = "#4a4a49",
                                            hjust    = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.grid.minor.y = element_blank())
  
  return(plot)
}
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8.  Line Chart Data Base                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lineChartData.fn <-function(data = Main_database,
                            dependent_var = dependent_var){
  
  dependent_var <- dependent_var
  
  event_study_tipo <- event_study(data.df = data, 
                                  var_analysis = dependent_var,
                                  var_groups = c("National"))
  
  data2table <- event_study_tipo[[1]]
  
  data2plot <- data2table %>%
    filter(order_value > -1) %>%
    rename(dependent_var = all_of(dependent_var)) %>%
    mutate(
      dependent_cmpl = 1 - dependent_var
    ) %>%
    drop_na() %>%
    pivot_longer(cols = c(dependent_var, dependent_cmpl), names_to = "category", values_to = "value2plot") %>%
    mutate(
      period = 
        case_when(
          period == "implementation_year" ~ "Implementación",
          period == "one_year_after" ~ "Un año",
          period == "two_years_after" ~ "Dos años",
          period == "three_years_after" ~ "Tres años",
          period == "four_years_after" ~ "Cuatro años",
          period == "five_years_after" ~ "Cinco años",
          period == "six_years_after" ~ "Seis años",
          period == "seven_years_after" ~ "Siete años",
          period == "eight_years_after" ~ "Ocho años",
          period == "nine_years_after" ~ "Nueve años",
          period == "ten_years_after" ~ "Diez años",
          period == "eleven_years_after" ~ "Once años",
          period == "twelve_years_after" ~ "Doce años",
        ),
      value2plot = value2plot*100,
      labels = if_else(
        period %in% c("Implementación", "Dos años", "Cuatro años", "Seis años", "Ocho años", "Diez años", "Doce años"),
        paste0(round(value2plot,0), "%"), NA_character_),
      period_labels =
        case_when(
          period == "Implementación" ~ "Implementación",
          period == "Un año" ~ " ",
          period == "Dos años" ~ "Dos años",
          period == "Tres años" ~ " ",
          period == "Cuatro años" ~ "Cuatro años",
          period == "Cinco años" ~ " ",
          period == "Seis años" ~ "Seis años",
          period == "Siete años" ~ " ",
          period == "Ocho años" ~ "Ocho años",
          period == "Nueve años" ~ " ",
          period == "Diez años" ~ "Diez años",
          period == "Once años" ~ " ",
          period == "Doce años" ~ "Doce años",
          
        )
    )
  
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 10.  Simple bars Data Base                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

count_frequency.fn <- function(column) {
  # Convert the column to a data frame
  data <- data.frame(Value = column)
  
  # Count the frequency of each unique value
  frequency_df <- data %>%
    group_by(Value) %>%
    summarise(Frequency = n()) %>% 
    mutate(Value = Value*100,
           values = Frequency/sum(Frequency),
           value2plot = values * 100,
           figure = paste0(round(value2plot, 0), "%"),
           labels = paste0(Value, "%")) 
  frequency_df <- frequency_df %>% mutate(order_var = rank(Value))
  return(frequency_df)
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Bar Simple Chart                                                                                ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
BarSimpleChartViz <- function(data = data2plot, 
                              x_var = Value, 
                              y_var = value2plot, 
                              label_var = figure, 
                              fill_var = Value,
                              order_var = order_var,
                              labels = labels,
                              shade_xminvalue,
                              shade_xmaxvalue
                              ) {
  plt <- ggplot(data, 
                aes(x     = reorder({{x_var}},{{order_var}}),
                    y     = {{y_var}},
                    label = {{label_var}},
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9) +
    scale_fill_gradient(low = "#756ef9", high = "#b1a6ff") +
    geom_vline(xintercept = c("0", "100"), linetype = 3, color = c("#fa4d57", "#43a9a7")) +
    annotate('rect', xmin=0, xmax= shade_xminvalue, ymin=0, ymax=60, alpha=.1, fill="#fa4d57")+
    annotate('rect', xmin=shade_xmaxvalue, xmax= shade_xmaxvalue+1, ymin=0, ymax=60, alpha=.1, fill="#43a9a7")+
    geom_text(aes(y    = {{y_var}} + 10),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    xlab("Porcentaje de criterios cumplidos")+
    scale_y_continuous(breaks = NULL) +
    scale_x_discrete() +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_blank(),
          axis.title.x       = element_text(color    = "#4a4a49",
                                            family   = "Lato Full"),
          axis.text.y        = element_text(hjust = 0))
  
  return(plt)
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 11.  Histogram                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



HistGraph.fn <- function(column_name, binsize, title) {
  graph <- Indicators_database %>%
    ggplot(aes_string(x = column_name)) +
    geom_histogram(binwidth = binsize, fill = "#b1a6ff", color = "#e9ecef", alpha = 0.9) +
    geom_vline(xintercept = c(0,0.25, 0.5, 0.75,1), linetype = 3, color = "#a90099") +
    ggtitle(title) +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(hjust = 0))
  return(graph)
}




