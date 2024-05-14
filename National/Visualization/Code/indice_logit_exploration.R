## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Exploración 
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


# Loading plotting functions from GitHub
source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/loading.R")
loadVIZ(set = "ENPOL")

# Loading data
load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData"))

master_data.df <- Main_database %>% 
  filter(Anio_arresto >= as.numeric(2008)) %>% 
  filter(NSJP == 1) 



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Logit índice fondo rojo o verde                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


logit_dataBase.fn <- function(data = Main_database,
                              selectables = c("Sexo", 
                                              # "Educacion_superior", 
                                              "Color_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30", 
                                              "vulnerabilidad_economica",
                                              "discapacidad"),
                              dependent_var
) {
  
  master_data.df <- data %>%
    filter(Anio_arresto >= 2008) %>%
    filter(NSJP == 1) %>%
    filter(Delito_unico == 1) %>%
    mutate(
      # Educacion_superior = 
      #   case_when(
      #     Educacion_superior == 1 ~ "Cuenta con título de educación universitaria",
      #     Educacion_superior == 0 ~ "No cuenta con título de educación universitario",
      #     T ~ NA_character_
      #   ),
      Color_piel_claro       =
        case_when(
          Color_piel_claro      == 1 ~ "Color de piel claro",
          Color_piel_claro      == 0 ~ "Color de piel oscuro",
          T ~ NA_character_
        ),
      LGBTQ                 = 
        case_when(
          LGBTQ                 == 1 ~ "Pertenece a la comunidad LGBTQ",
          LGBTQ                 == 0 ~ "No pertenece a la comunidad LGBTQ",
          T ~ NA_character_
        ),
      Etnia                 =
        case_when(
          Etnia                 == 1 ~ "Afromexicano o indígena",
          Etnia                 == 0 ~ "No se identifica con ninguna etnia",
          T ~ NA_character_
        ),
      Edad_menor30          =
        case_when(
          Edad_menor30          == 1 ~ "Menor a 30 años",
          Edad_menor30          == 0 ~ "Mayor o igual a 30 años",
          T ~ NA_character_
        ),
      vulnerabilidad_economica  =
        case_when(
          vulnerabilidad_economica == 1 ~ "Vulnerable economicamente",
          vulnerabilidad_economica == 0 ~ "No vulnerable economicamente",
          T ~ NA_character_
        ),
      discapacidad      =
        case_when(
          discapacidad == 1 ~ "Reporta algún tipo de discapacidad",
          discapacidad == 0 ~ "No presenta discapacidad",
          T ~ NA_character_
        )
    )
  
  selectables <- c(selectables)
  
  logit_data <- master_data.df %>%
    select(all_of(selectables),
           all_of(dependent_var),
           Delito_unico_categ, 
           Estado_arresto) %>%
    mutate(
      # Educacion_superior =
      #   if_else(
      #     Educacion_superior %in% "Cuenta con título de educación universitaria",
      #     "ZCuenta con título de educación universitaria", Educacion_superior
      #   ),
      Sexo                  =
        if_else(
          Sexo %in% "Femenino",
          "ZFemenino", Sexo
        ),
      Color_piel_claro       =
        if_else(
          Color_piel_claro %in% "Color de piel claro",
          "ZColor de piel claro", Color_piel_claro
        ),
      LGBTQ                 =
        if_else(
          LGBTQ %in% "Pertenece a la comunidad LGBTQ",
          "ZPertenece a la comunidad LGBTQ", LGBTQ
        ),
      Etnia                 =
        if_else(
          Etnia %in% "Afromexicano o indígena",
          "ZAfromexicano o indígena", Etnia
        ),
      Edad_menor30          =
        if_else(
          Edad_menor30 %in% "Menor a 30 años",
          "ZMenor a 30 años", Edad_menor30
        ),
      vulnerabilidad_economica          =
        if_else(
          vulnerabilidad_economica %in% "Vulnerable economicamente",
          "ZVulnerable economicamente", vulnerabilidad_economica
        ),
      discapacidad          =
        if_else(
          discapacidad %in% "Reporta algún tipo de discapacidad",
          "ZReporta algún tipo de discapacidad", discapacidad
        ),
    ) %>%
    arrange(Sexo, Color_piel_claro, LGBTQ, Etnia, Edad_menor30, 
            Delito_unico_categ, Estado_arresto, vulnerabilidad_economica, discapacidad)
  
  formula <- selectables %>%
    t() %>%
    as.data.frame() %>%
    unite(., formula, sep = "+") %>%
    as.character()
  
  depVar <- dependent_var
  
  formula  <- as.formula(paste(depVar, "~", 
                               formula, 
                               "+factor(Delito_unico_categ)+factor(Estado_arresto)")
  )
  logit    <- glm(formula,
                  data   = logit_data, 
                  family = "binomial")
  
  summaryLogit <- bind_rows(
    as.data.frame(coef(logit))
  )
  
  marg_effects <- margins(logit, variables = selectables, atmeans = TRUE)
  
  # Calculate robust variance-covariance matrix
  robust_vcov <- vcovHC(logit, type = "HC1", cluster = "group", group = logit_data$Estado_arresto)
  
  
  margEff      <- as.data.frame(
    summary(marg_effects, vcov = robust_vcov)
  ) %>%
    filter(factor %in% c("SexoZFemenino", "LGBTQZPertenece a la comunidad LGBTQ", 
                         "Color_piel_claroZColor de piel claro", "EtniaZAfromexicano o indígena",
                         "Edad_menor30ZMenor a 30 años", "vulnerabilidad_economicaZVulnerable economicamente", 
                         "discapacidadZReporta algún tipo de discapacidad"))
  
  margEff$factor <-recode(margEff$factor,
                          "SexoZFemenino"                                            = "Mujer",
                          "LGBTQZPertenece a la comunidad LGBTQ"                     = "Perteneciente a \ncomunidad LGBTQ",
                          "EtniaZAfromexicano o indígena"                            = "Afromexicano/a o \nindígena",
                          "Edad_menor30ZMenor a 30 años"                             = "Menor a 30 años",
                          "Color_piel_claroZColor de piel claro"                     = "Color de piel \nclaro",
                          "vulnerabilidad_economicaZVulnerable economicamente"       = "Vulnerable \neconómicamente",
                          "discapacidadZReporta algún tipo de discapacidad"          = "Persona con \ndiscapacidad"
  )
  
  data2table <- margEff %>%
    mutate(order_variable =
             case_when(
               factor == "Mujer"                                  ~ 1,
               factor == "Perteneciente a \ncomunidad LGBTQ"      ~ 2,
               factor == "Menor a 30 años"                        ~ 3,
               # factor == "Con educación \nuniveristaria"          ~ 4,
               factor == "Afromexicano/a o \nindígena"            ~ 4,
               factor == "Color de piel \nclaro"                  ~ 5,
               factor == "Vulnerable \neconómicamente"            ~ 6,
               factor == "Persona con \ndiscapacidad"             ~ 7,
             ),
           dependent_var  =
             dependent_var
    )
  
  return(data2table)
  
}

# # Applying plotting function
# data2plot <- logit_dataBase.fn(dependent_var = "indicator_GDH_maxlimit")
# 
# #Elegir el color rojo: "#fa4d57" o verde: "#43a9a7"
# logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#43a9a7")
# 
# # See plot
# logitPlot
# 
# ggsave(plot   = logitPlot,
#        file   = paste0(path2SP,
#                        "/National/Visualization",
#                        "/Output/Debido proceso/Indice_logits/figure4.svg"), 
#        width  = 175, 
#        height = 85,
#        units  = "mm",
#        dpi    = 72,
#        device = "svg")


# General todos los criterios ----------------------------------


data2plot <- logit_dataBase.fn(dependent_var = "indicator_general_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#43a9a7")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure1_1_exp.svg"), 
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
                       "/Output/Debido proceso/Indice_logits/figure1_2_exp.svg"), 
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
                       "/Output/Debido proceso/Indice_logits/figure2_1_exp.svg"), 
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
                       "/Output/Debido proceso/Indice_logits/figure2_2_exp.svg"), 
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
                       "/Output/Debido proceso/Indice_logits/figure3_1_exp.svg"), 
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
                       "/Output/Debido proceso/Indice_logits/figure3_2_exp.svg"), 
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
                       "/Output/Debido proceso/Indice_logits/figure4_1_exp.svg"), 
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
                       "/Output/Debido proceso/Indice_logits/figure4_2_exp.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")







