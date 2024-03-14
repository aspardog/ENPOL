## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration descriptives
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres
##
## Dependencies:      World Justice Project
##
## Creation date:     Marzo 11th, 2024
##
## This version:      Marzo 11th, 2024
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

load(paste0(path2SP,"/National/Presentations/INL_MARZO/Input/Indicators_database.RData"))

# filter database ---------------------------------------------------------

Indicators_database <- Indicators_database %>% 
  mutate(eliminar = case_when( na_UAA >= 2 |
         na_GDH >= 2 |
         na_PJ >= 3 ~ 1,
         T~ 0),
         indicator_GDH = round(indicator_GDH, 1),
         indicator_UAA = round(indicator_UAA, 1),
         indicator_PJ = round(indicator_PJ, 1),
         indicator_general = round(indicator_general, 1)) %>% 
  filter(eliminar != 1)


# Barras ------------------------------------------------------------------

data2plot <- count_frequency.fn(Indicators_database$indicator_general)

barChart <- BarSimpleChartViz(shade_xminvalue = 5, shade_xmaxvalue = 10)
barChart
ggsave(plot   = barChart,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Bar_indicator_general.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")



data2plot <- count_frequency.fn(Indicators_database$indicator_GDH)

barChart <- BarSimpleChartViz(shade_xminvalue = 2, shade_xmaxvalue = 3)
barChart
ggsave(plot   = barChart,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Bar_indicator_GDH.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")



data2plot <- count_frequency.fn(Indicators_database$indicator_UAA)
data2plot <- data2plot %>% filter(!is.na(Value))

barChart <- BarSimpleChartViz(shade_xminvalue = 4, shade_xmaxvalue = 7)
barChart
ggsave(plot   = barChart,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Bar_indicator_UAA.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

data2plot <- count_frequency.fn(Indicators_database$indicator_PJ)
data2plot <- data2plot %>% filter(!is.na(Value))

barChart <- BarSimpleChartViz(shade_xminvalue = 6, shade_xmaxvalue = 11)
barChart
ggsave(plot   = barChart,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Bar_indicatorPJ.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# Logit -----------------------------------------------------------------

load(paste0(path2SP,"/National/Presentations/INL_MARZO/Input/Main_database.RData"))

Main_database <- Main_database %>% 
  mutate(eliminar = case_when( na_UAA >= 2 |
                                 na_GDH >= 2 |
                                 na_PJ >= 3 ~ 1,
                               T~ 0)) %>% 
  filter(eliminar != 1)


# General almenos del 50% de los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_general_minlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Logit_min_indicator_general.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


data2plot <- logit_dataBase.fn(dependent_var = "indicator_general_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)

# GDH menos del 50% de los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_GDH_minlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Logit_min_indicator_GDH.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# PJ menos del 50% de los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_PJ_minlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Logit_min_indicator_PJ.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# UAA menos del 50% de los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_UAA_minlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Logit_min_indicator_UAA.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# General todos los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_general_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)

ggsave(plot   = logitPlot,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Logit_max_indicator_general.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# GDH todos los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_GDH_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Logit_max_indicator_GDH.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# PJ todos los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_PJ_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Logit_max_indicator_PJ.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# UAA todos los criterios  ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_UAA_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0( path2SP, "National/Presentations/INL_MARZO/Visualizations/Logit_max_indicator_UAA.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")
