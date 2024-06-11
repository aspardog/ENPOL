## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages
library(pacman)

# Notes: ggsankey and ggwaffle need to be installed from Github's developer version. Run the following lines 
# of code in order to install: 
# devtools::install_github("davidsjoberg/ggsankey")
# devtools::install_github("liamgilbey/ggwaffle")
#devtools::install_github("ctoruno/WJPr")

p_load(char = c(
  # Visualizations
  "showtext", "ggtext", "ggsankey", "ggwaffle", "ggplotify", "gridExtra", 
  "patchwork", "ggh4x", "ggrepel", "sf", "flextable",  "officer", "cowplot",
  
  # Data Loading and Saving
  "haven", "readxl", "writexl", "openxlsx",
  
  # Utilities
  "margins", "quarto", "kableExtra", "WJPr", "sandwich", "broom",
  
  # Good 'ol Tidyverse
  "tidyverse",
  
  # Estimattions
  "sandwich"
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
twoCOLORS      <- c("#2a2a9A", "#a90099")
threeColors    <- c("#2a2a9A", "#a90099", "#43a9a7")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6.  Funciones adicionales                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

