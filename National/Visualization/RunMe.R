## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - RunMe File
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Abril 2, 2024
##
## This version:      Abril 2, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/settings.R")

# Loading functions for sections
source("Code/S00.R")
source("Code/S01.R")
source("Code/S02.R")
source("Code/S03.R")
source("Code/S04.R")
source("Code/S05.R")


# Loading plotting functions from GitHub
source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/loading.R")
loadVIZ(set = "LAC")

# Loading data
load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData"))
master_data.df <- Main_database %>% 
  filter(Anio_arresto >= as.numeric(2008)) %>% 
  filter(NSJP == 1) 

# select proper filters
# Tenemos 3 filtros y para cada visualización se decide qué filtros usar
# sentenciado = 1
# Anio_arresto >= 2011


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Índice de deibido proceso                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Figure 1.1 cumplimiento 13 criterios -----------------------------------------------------------

### Figure 1.2.1 cumplimiento menos del 50%  de los 13 criterios -----------------------------------------------------------

### Figure 1.2.2 cumplimiento más del 90%   de los 13 criterios -----------------------------------------------------------

### Figure 2.1 cumplimiento Proceso Justo -----------------------------------------------------------

### Figure 2.2.1 cumplimiento menos del 50%  de Proceso Justo -----------------------------------------------------------

### Figure 2.2.2 cumplimiento más del 90%   de Proceso Justo -----------------------------------------------------------

### Figure 3.1 cumplimiento 13 Uso no arbitrario de la autoridad -----------------------------------------------------------

### Figure 3.2.1 cumplimiento menos del 50%  Uso no arbitrario de la autoridad -----------------------------------------------------------

### Figure 3.2.2 cumplimiento más del 90%   Uso no arbitrario de la autoridad -----------------------------------------------------------

### Figure 4.1 cumplimiento 13 criterios -----------------------------------------------------------

### Figure 4.2.1 cumplimiento menos del 50%  de los 13 criterios -----------------------------------------------------------

### Figure 4.2.2 cumplimiento más del 90%   de los 13 criterios -----------------------------------------------------------


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Debido proceso                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1.1 Proceso justo -----------------------------------------------------------

#### Figure 1.1 Proceso justo -----------------------------------------------------------

### 1.2	Uso no arbitrario de la autoridad -----------------------------------------------------------




## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Política criminal                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
