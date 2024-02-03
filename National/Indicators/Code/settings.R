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
  "showtext", "ggtext", "gridExtra",
  
  # Data Loading
  "haven", "foreign", "openxlsx", "readxl", "writexl", "xlsx",
  
  # Utilities
  "margins", "english", "quarto", "kableExtra", "sysfonts", "magrittr", "caret",
  
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
## 5.  Normalización                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

normalizacion <- function(codebook = Codebook.df, 
                          data2normalize = Main_database) {
  
  vars2normalize <- codebook %>%
    select(Variable) %>% 
    pull()
  
  
  data_subset.df <- Main_database %>%
    select(all_of(vars2normalize)) %>%
    mutate(
      across(
        everything(),
        ~if_else(
          .x %in% c(6,8,9), NA, .x 
        )
      )
    )
  
  vars2orient2scale <- codebook %>%
    filter(Direccion %in% "Negativa") %>%
    filter(Escala %in% "Escala 2") %>%
    select(Variable) %>%
    pull()
  
  vars2orient4scale <- codebook %>%
    filter(Direccion %in% "Negativa") %>%
    filter(Escala %in% "Escala 4") %>%
    select(Variable) %>%
    pull()
  
  dataOriented <- data_subset.df %>%
    mutate(
      across(
        everything(),
        ~as.numeric(.x)
      ),
      across(
        all_of(vars2orient2scale),
        ~case_when(
          .x == 1 ~ 2,
          .x == 2 ~ 1,
          T ~ NA_real_
        )),
      across( 
        all_of(vars2orient4scale),
        ~case_when(
          .x == 1 ~ 4,
          .x == 2 ~ 3,
          .x == 3 ~ 2,
          .x == 1 ~ 4,
          T ~ NA_real_
        )
      )
    )
  
  max_values <- codebook %>% 
    filter(Variable %in% all_of(vars2normalize)) %>%
    mutate(Escala = 
             case_when(
               Escala == "Escala 2" ~ 2,
               Escala == "Escala 3" ~ 3,
               Escala == "Escala 4" ~ 4,
             )
    ) %>%
    pull(Escala)
  
  max_values <- lapply(vars2normalize, function(vars2normalize){
    
    codebook %>% 
      filter(Variable %in% vars2normalize) %>%
      mutate(max_value = 
               case_when(
                 Escala == "Escala 2" ~ 2,
                 Escala == "Escala 3" ~ 3,
                 Escala == "Escala 4" ~ 4,
               )) %>%
      pull(max_value)
  })
  
  dataOriented[nrow(dataOriented) + 1,] <- c(max_values)
  dataOriented[nrow(dataOriented) + 1,] <- c(rep(list(1), ncol(dataOriented)))
  
  
  process    <- preProcess(dataOriented, method = c("range"))
  normalized <- predict(process, dataOriented)
  
  final_data.df <- normalized[1:(nrow(normalized) - 2), ] %>%
    rename_all(~ paste0(., "_norm"))
  
  master_data.df <- bind_cols(data2normalize, final_data.df)
  
  return(master_data.df)
  
}
