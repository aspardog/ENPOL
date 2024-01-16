## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Descriptivos 
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    D. Cristina Álvarez Venzor     (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     January 12th, 2023
##
## This version:      January 12th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Load Settings                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("National/Data_cleaning/Code/settings.R")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Set function                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

