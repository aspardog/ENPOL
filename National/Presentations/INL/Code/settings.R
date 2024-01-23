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
                                              "Educacion_obligatoria", 
                                              "Colo_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30", 
                                              "Ingreso_inseguro"),
                              dependent_var
) {
  
  master_data.df <- Main_database %>%
    filter(Anio_arresto >= 2015) %>%
    filter(NSJP == 1) %>%
    filter(Delito_unico == 1) %>%
    mutate(
      Educacion_obligatoria = 
        case_when(
          Educacion_obligatoria == 1 ~ "Título de bachiller o más",
          Educacion_obligatoria == 0 ~ "No cuenta con título de bachiller"
        ),
      Colo_piel_claro       =
        case_when(
          Colo_piel_claro      == 1 ~ "Color de piel clara",
          Colo_piel_claro      == 0 ~ "Color de piel oscura",
        ),
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
      Educacion_obligatoria =
        if_else(
          Educacion_obligatoria %in% "No cuenta con título de bachiller",
          "ZNo cuenta con título de bachiller", Educacion_obligatoria
        ),
      Sexo                  =
        if_else(
          Sexo %in% "Femenino",
          "ZFemenino", Sexo
        ),
      Colo_piel_claro       =
        if_else(
          Colo_piel_claro %in% "Color de piel oscura",
          "ZColor de piel oscura", Colo_piel_claro
        ),
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
    arrange(Sexo, Educacion_obligatoria, Colo_piel_claro, LGBTQ, Etnia, Edad_menor30, Ingreso_inseguro)
  
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
                          "LGBTQZPertenece a la comunidad LGBTQ"                     = "Perteneciente a \ncomunidad LGBTQ",
                          "Ingreso_inseguroZFinancieramente inseguro"                = "Financieramente inseguro/a",
                          "EtniaZAfroamericano o indígena"                           = "Afroamericano/a o indígena",
                          "Educacion_obligatoriaZNo cuenta con título de bachiller"  = "Sin diploma bachiller",
                          "Edad_menor30ZMenor a 30 años"                             = "Menor a 30 años",
                          "Colo_piel_claroZColor de piel oscura"                     = "Color de piel oscura"
  )
  
  data2table <- margEff %>%
    mutate(order_variable =
             case_when(
               factor == "Mujer"                              ~ 1,
               factor == "Perteneciente a \ncomunidad LGBTQ"  ~ 2,
               factor == "Menor a 30 años"                    ~ 3,
               factor == "Sin diploma bachiller"              ~ 4,
               factor == "Financieramente inseguro/a"         ~ 5,
               factor == "Afroamericano/a o indígena"         ~ 6,
               factor == "Color de piel oscura"               ~ 7
             ),
           dependent_var  =
             dependent_var
    )
  
  return(data2table)
  
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
## 9.  Group bars Data Base                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


groupBarData.fn <- function(data = Main_database, 
                            group_var = group_var, 
                            prop_var = prop_var) { 
  data2table <- data %>%
    filter(Anio_arresto >= 2008) %>% 
    group_by({{group_var}}) %>%
    filter(!is.na({{group_var}}) | {{group_var}} != "NS/NR" | {{group_var}} != "Otra") %>% 
    summarize(
      sí = mean({{prop_var}} == 1, na.rm = TRUE),
      no = mean({{prop_var}} == 0, na.rm = TRUE)
    ) %>% 
    arrange(sí) %>%
    mutate(legend_order = row_number()) %>% 
    pivot_longer(
      cols = !c({{group_var}}, legend_order), 
      names_to = "group_var", 
      values_to = "values"
    ) %>%
    rename(category = {{group_var}}) %>% 
    mutate(value2plot = values * 100,
           figure = paste0(round(value2plot, 0), "%"),
           labels = category) %>%
    filter(!is.na(category) & category != "NS/NR" & category != "Otra")
  
  return(data2table)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 10.  Simple bars Data Base                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

simpleBarData.fn <- function(data = Main_database, 
                             group_var = group_var) {
  data2table <- data %>%
    filter(Anio_arresto >= 2008) %>% 
    group_by({{group_var}}) %>%
    summarize(frequency = n()) %>%
    filter(!is.na({{group_var}}) | {{group_var}} != "NS/NR" | {{group_var}} != "Otra") %>% 
    mutate(values = frequency/sum(frequency),
           value2plot = values * 100,
           figure = paste0(round(value2plot, 0), "%"),
           labels = {{group_var}}) %>%
    rename(category = {{group_var}})
  
  return(data2table)
}


