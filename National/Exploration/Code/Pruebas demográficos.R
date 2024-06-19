# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Oaxaca from Enpol
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##
## Dependencies:      World Justice Project
##
## Creation date:     Jun 14th, 2024
##
## This version:      Jun 17th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes and other general routines are
# loaded from the following script:
source("National/Data_cleaning/Code/settings.R")
library(readxl)
library(ggrepel)
library(sandwich)

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 


#FILTRO 2016 EN ADELANTE

Main_database <- Main_database %>%
  filter(Anio_arresto >= 2008) %>%
  mutate(
    uso_excesivo =
      case_when(
        proporcionalidad_uso_fuerza == 0 ~ 1,
        proporcionalidad_uso_fuerza == 1 ~ 0
      ),
    detencion_corrupcion = case_when(P3_21_1 == "1" | P3_21_2 == "1" ~ 1,
                                         P3_21_1 == "2" & P3_21_2 == "2" ~ 0,
                                         T ~ NA_real_),
    mp_corrupcion= case_when(P4_15_1 == "1" | P4_15_3 == "1" ~ 1,
                             P4_15_1 == "2" & P4_15_3 == "2" ~ 0,
                             T ~ NA_real_),
    juzgado_corrupcion= case_when(P5_45_1 == "1" | P5_45_3 == "1" ~ 1,
                                  P5_45_1 == "2" & P5_45_3 == "2" ~ 0,
                                  T ~ NA_real_),
    corrupcion_general = case_when(detencion_corrupcion == 1 | mp_corrupcion == 1  | juzgado_corrupcion == 1 ~ 1,
                                   detencion_corrupcion == 0 & mp_corrupcion == 0  & juzgado_corrupcion == 0 ~ 0,
                                   T~ NA_real_),
    prision_preventiva = 
      case_when(
        tipo_prision_preventiva == "Prisión Preventiva Oficiosa" ~ 1,
        tipo_prision_preventiva == "Prisión Preventiva Justificada" ~ 0,
        tipo_prision_preventiva == "Proceso en libertad" ~ 0
      ),
    procedimiento_abreviado = 
      case_when(
      P5_6 == "1" ~ 0,
      P5_6 == "2" ~ 1,
      T ~ NA_real_
    )
  )


#### Funcion Logit

logit_dataBase.fn <- function(data = Main_database,
                              selectables = c("Sexo", 
                                              "Educacion_superior", 
                                              "Color_piel_oscuro", 
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
      Educacion_superior = 
        case_when(
          Educacion_superior == 1 ~ "Cuenta con título de educación universitaria",
          Educacion_superior == 0 ~ "No cuenta con título de educación universitario",
          T ~ NA_character_
        ),
      Color_piel_oscuro       =
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
      Educacion_superior =
        if_else(
          Educacion_superior %in% "No cuenta con título de educación universitario",
          "ZNo cuenta con título de educación universitario", Educacion_superior
        ),
      Sexo                  =
        if_else(
          Sexo %in% "Femenino",
          "ZFemenino", Sexo
        ),
      Color_piel_oscuro     =
        if_else(
          Color_piel_oscuro %in% "Color de piel oscuro",
          "ZColor de piel oscuro", Color_piel_oscuro
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
    arrange(Sexo, Educacion_superior, Color_piel_oscuro, LGBTQ, Etnia, 
            Edad_menor30, Delito_unico_categ, Estado_arresto, 
            vulnerabilidad_economica, discapacidad)
  
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
                         "Educacion_superiorZNo cuenta con título de educación universitario", 
                         "Color_piel_oscuroZColor de piel oscuro", 
                         "EtniaZAfromexicano o indígena",
                         "Edad_menor30ZMenor a 30 años", 
                         "vulnerabilidad_economicaZVulnerable economicamente", 
                         "discapacidadZReporta algún tipo de discapacidad"))
  
  margEff$factor <-recode(margEff$factor,
                          "SexoZFemenino"                                                       = "Mujer",
                          "LGBTQZPertenece a la comunidad LGBTQ"                                = "Perteneciente a \ncomunidad LGBTQ",
                          "EtniaZAfromexicano o indígena"                                       = "Afromexicano/a o \nindígena",
                          "Educacion_superiorZNo cuenta con título de educación universitario"  = "Sin educación \nuniveristaria",
                          "Edad_menor30ZMenor a 30 años"                                        = "Menor a 30 años",
                          "Color_piel_oscuroZColor de piel oscuro"                              = "Color de piel \noscuro",
                          "vulnerabilidad_economicaZVulnerable economicamente"                  = "Vulnerable \neconómicamente",
                          "discapacidadZReporta algún tipo de discapacidad"                     = "Persona con \ndiscapacidad"
  )
  
  data2table <- margEff %>%
    mutate(order_variable =
             case_when(
               factor == "Mujer"                                  ~ 1,
               factor == "Perteneciente a \ncomunidad LGBTQ"      ~ 2,
               factor == "Menor a 30 años"                        ~ 3,
               factor == "Sin educación \nuniveristaria"          ~ 4,
               factor == "Afromexicano/a o \nindígena"            ~ 5,
               factor == "Color de piel \noscuro"                 ~ 6,
               factor == "Vulnerable \neconómicamente"            ~ 7,
               factor == "Persona con \ndiscapacidad"             ~ 8,
             ),
           dependent_var  =
             dependent_var
    )
  
  return(data2table)
  
}


#### Funcion tests


test_sociodem <- function(variable){

  
  data2plot <- logit_dataBase.fn(dependent_var = variable)
  
  descriptivos <- data.frame(factor = character(),
                             dif_desc = double())
  
  des <-  Main_database %>%
    group_by(Sexo) %>%
    summarise(ind = mean(indicator_general_minlimit,na.rm = T))
  
  descriptivos_i <- data.frame(factor = char("Mujer"),
                               dif_desc = des$ind[1] - des$ind[2])
  
  descriptivos <- bind_rows(descriptivos,descriptivos_i)
  
  des <-  Main_database %>%
    group_by(LGBTQ) %>%
    summarise(ind = mean(indicator_general_minlimit,na.rm = T))
  
  descriptivos_i <- data.frame(factor = char("Perteneciente a \ncomunidad LGBTQ"),
                               dif_desc = des$ind[2] - des$ind[1])
  
  descriptivos <- bind_rows(descriptivos,descriptivos_i)
  
  des <-  Main_database %>%
    group_by(Edad_menor30) %>%
    summarise(ind = mean(indicator_general_minlimit,na.rm = T))
  
  descriptivos_i <- data.frame(factor = char("Menor a 30 años"),
                               dif_desc = des$ind[2] - des$ind[1])
  
  descriptivos <- bind_rows(descriptivos,descriptivos_i)
  
  des <-  Main_database %>%
    group_by(Educacion_superior) %>%
    summarise(ind = mean(indicator_general_minlimit,na.rm = T))
  
  descriptivos_i <- data.frame(factor = char("Sin educación \nuniveristaria"),
                               dif_desc = des$ind[1] - des$ind[2])
  
  descriptivos <- bind_rows(descriptivos,descriptivos_i)
  
  des <-  Main_database %>%
    group_by(Etnia) %>%
    summarise(ind = mean(indicator_general_minlimit,na.rm = T))
  
  descriptivos_i <- data.frame(factor = char("Afromexicano/a o \nindígena"),
                               dif_desc = des$ind[2] - des$ind[1])
  
  descriptivos <- bind_rows(descriptivos,descriptivos_i)
  
  des <-  Main_database %>%
    group_by(Color_piel_claro) %>%
    summarise(ind = mean(indicator_general_minlimit,na.rm = T))
  
  descriptivos_i <- data.frame(factor = char("Color de piel \noscuro"),
                               dif_desc = des$ind[1] - des$ind[2])
  
  descriptivos <- bind_rows(descriptivos,descriptivos_i)
  
  des <-  Main_database %>%
    group_by(vulnerabilidad_economica) %>%
    summarise(ind = mean(indicator_general_minlimit,na.rm = T))
  
  descriptivos_i <- data.frame(factor = char("Vulnerable \neconómicamente"),
                               dif_desc = des$ind[2] - des$ind[1])
  
  descriptivos <- bind_rows(descriptivos,descriptivos_i)
  
  des <-  Main_database %>%
    group_by(discapacidad) %>%
    summarise(ind = mean(indicator_general_minlimit,na.rm = T))
  
  descriptivos_i <- data.frame(factor = char("Persona con \ndiscapacidad"),
                               dif_desc = des$ind[2] - des$ind[1])
  
  descriptivos <- bind_rows(descriptivos,descriptivos_i)
  
  data2plot <- left_join(data2plot, descriptivos, by = "factor" ) %>%
    select(-SE, -z, -p, -order_variable,-dependent_var)
  
  write.xlsx(as.data.frame(data2plot), 
             file      = file.path(paste0(path2DB,"/National/Exploration/Output/Pruebas_sociodemográficos.xlsx")),
             sheetName = variable,
             append    = T,
             row.names = F)
  
}


#####. Tests



# Probabilidad que se cumpla menos de 50% criterios minimos  (logit de sociodemograficos)

test_sociodem("indicator_general_minlimit")

# Probabilidad que se cumpla mas del 90% de los criterios  (logit de sociodemograficos)

test_sociodem("indicator_general_maxlimit")

# El uso excesivo de la fuerza por cortes socio-demograficos 

test_sociodem("uso_excesivo")

#	Practicas de corrupcion por cualquier autoridad en el proceso, por cortes socio-demográficos 

test_sociodem("corrupcion_general")

# Practicas de tortura experimentadas por cortes socio-demograficos (Logit)

test_sociodem("tortura_generalizada")

# Detenciones irregulares por cortes socio-demográficos (Logit)

test_sociodem("det_ninguna")

# Uso la prisión preventiva oficiosa por cortes socio-demográficos (Logit)

test_sociodem("prision_preventiva")

# Uso del procedimiento abreviado por cortes socio-demográficos (Logit)

test_sociodem("procedimiento_abreviado")

# Uso del juicio oral por cortes socio-demográficos (Logit)

#test_sociodem()

