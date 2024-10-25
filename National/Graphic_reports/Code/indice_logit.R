## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Uso excesivo de la autoridad
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Abril 24, 2024
##
## This version:      Abril 24, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Índice general barras Barras                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Logit índice fondo rojo o verde                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# General todos los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_general_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#43a9a7")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure1_1.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# General almenos del 50% de los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_general_minlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#fa4d57")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure1_2.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# PJ todos los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_PJ_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#43a9a7")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure2_1.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")




# PJ menos del 50% de los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_PJ_minlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#fa4d57")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure2_2.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

# UAA todos los criterios  ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_UAA_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#43a9a7")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure3_1.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")



# UAA menos del 50% de los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_UAA_minlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#fa4d57")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure3_2.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# GDH todos los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_GDH_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#43a9a7")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure4_1.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# GDH menos del 50% de los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_GDH_minlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#fa4d57")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure4_2.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")