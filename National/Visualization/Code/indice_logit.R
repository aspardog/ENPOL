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


logit_dataBase.fn <- function(data = Main_database,
                              selectables = c("Sexo", 
                                              "Educacion_superior", 
                                              "Color_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30"),
                              dependent_var
) {
  
  master_data.df <- data %>%
    filter(Anio_arresto >= 2008) %>%
    filter(NSJP == 1) %>%
    filter(Delito_unico == 1) %>%
    mutate(
      Educacion_superior = 
        case_when(
          Educacion_superior == 1 ~ "Cuenta con título de educación universitaria",
          Educacion_superior == 0 ~ "No cuenta con título de educación universitario",
          T ~ NA_character_
        ),
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
          Etnia                 == 1 ~ "Afroamericano o indígena",
          Etnia                 == 0 ~ "No se identifica con ninguna etnia",
          T ~ NA_character_
        ),
      Edad_menor30          =
        case_when(
          Edad_menor30          == 1 ~ "Menor a 30 años",
          Edad_menor30          == 0 ~ "Mayor o igual a 30 años",
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
      Educacion_superior =
        if_else(
          Educacion_superior %in% "Cuenta con título de educación universitaria",
          "ZCuenta con título de educación universitaria", Educacion_superior
        ),
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
          Etnia %in% "Afroamericano o indígena",
          "ZAfroamericano o indígena", Etnia
        ),
      Edad_menor30          =
        if_else(
          Edad_menor30 %in% "Menor a 30 años",
          "ZMenor a 30 años", Edad_menor30
        ),
    ) %>%
    arrange(Sexo, Educacion_superior, Color_piel_claro, LGBTQ, Etnia, Edad_menor30, 
            Delito_unico_categ, Estado_arresto)
  
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
  
  margEff      <- as.data.frame(
    margins_summary(logit, data = logit$data)
  ) %>%
    filter(factor %in% c("SexoZFemenino", "LGBTQZPertenece a la comunidad LGBTQ", 
                         "Educacion_superiorZCuenta con título de educación universitaria", 
                         "Color_piel_claroZColor de piel claro", "EtniaZAfroamericano o indígena",
                         "Edad_menor30ZMenor a 30 años"))
  
  margEff$factor <-recode(margEff$factor,
                          "SexoZFemenino" = "Mujer",
                          "LGBTQZPertenece a la comunidad LGBTQ"                     = "Perteneciente a \ncomunidad LGBTQ",
                          "EtniaZAfroamericano o indígena"                           = "Afroamericano/a o indígena",
                          "Educacion_superiorZCuenta con título de educación universitaria"  = "Con educación univeristaria \n o más",
                          "Edad_menor30ZMenor a 30 años"                             = "Menor a 30 años",
                          "Color_piel_claroZColor de piel claro"                     = "Color de piel claro"
  )
  
  data2table <- margEff %>%
    mutate(order_variable =
             case_when(
               factor == "Mujer"                              ~ 1,
               factor == "Perteneciente a \ncomunidad LGBTQ"  ~ 2,
               factor == "Menor a 30 años"                    ~ 3,
               factor == "Con educación univeristaria \n o más" ~ 4,
               factor == "Afroamericano/a o indígena"         ~ 5,
               factor == "Color de piel claro"               ~ 6
             ),
           dependent_var  =
             dependent_var
    )
  
  return(data2table)
  
}

# Applying plotting function
data2plot <- logit_dataBase.fn(dependent_var = "indicator_general")

#Elegir el color rojo: "#fa4d57" o verde: "#43a9a7"
logitPlot <- logit_demo_panel(mainData = data2plot, shadow = "#43a9a7")

# See plot
logitPlot

ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Uso excesivo/figure4.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

