## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration data & hipothesys
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres            (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     February 05th, 2024
##
## This version:      March 19th, 2024
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

source("National/Data_cleaning/Code/settings.R")
library(ggrepel)

#Loading fonts
path2fonts <- paste0(path2SP, "/National/Visualization/Fonts/")
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




load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 


Main_database_or <- Main_database %>% 
  filter(Anio_arresto >= 2011)

Main_database<- Main_database %>% mutate(PP = case_when(sentenciado == 1 & proceso_en_libertad == 1 ~ 0,
                                                        sentenciado == 1 & proceso_en_libertad == 0 ~ 1,
                                                        sentenciado == 0 ~ 1,
                                                        T ~  NA_real_),
                                         P5_4_A = as.numeric(P5_4_A),
                                         sentencia_severa_0_5 = case_when(P5_4_A >= 0 & P5_4_A < 5 ~ 1,
                                                                         P5_4_A >= 5 ~ 0,
                                                                         T ~ NA_real_),
                                         sentencia_severa_5_10 = case_when(P5_4_A >= 5 & P5_4_A < 10 ~ 1,
                                                                          P5_4_A >= 10 ~ 0,
                                                                          P5_4_A < 5 ~ 0,
                                                                          T ~ NA_real_),
                                         sentencia_severa_10_30 = case_when(P5_4_A >= 10 & P5_4_A < 30 ~ 1,
                                                                           P5_4_A >= 30 ~ 0,
                                                                           P5_4_A < 10 ~ 0,
                                                                           T ~ NA_real_),
                                         sentencia_severa_30 = case_when(P5_4_A >= 30 ~ 1,
                                                                        P5_4_A < 30 ~ 0,
                                                                        T ~ NA_real_),
                                         proc_abreviado = case_when(P5_6 == "1" ~ 0,
                                                                    P5_6 == "2" ~ 1,
                                                                    T ~ NA_real_),
                                         tiempo_sentencia = case_when(P5_4_A == 0 ~ NA_real_,
                                                                      P5_4_A >= 97 ~ NA_real_,
                                                                      T ~ P5_4_A)) %>%
  arrange(years_since_NSJP) %>%
  group_by(Estado_arresto) %>%
  mutate(max_time_implementation = max(years_since_NSJP, na.rm = T),
         grupo_implementacion = 
           if_else(
             max_time_implementation > 6 & max_time_implementation < 9, "Implementación tardía",
             if_else(
               max_time_implementation > 9 & max_time_implementation < 11, "Implementación media",
               if_else(
                 max_time_implementation > 11, "Implementación temprana", NA_character_
               )
             )))
  


Main_database <- Main_database %>% 
  filter(Anio_arresto >= 2011,
         NSJP == 1)




## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Set functions                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 0.  ols Chart  

ols_dataBase.fn <- function(data = Main_database,
                              selectables = c("Sexo", 
                                              "Educacion_obligatoria", 
                                              "Colo_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30", 
                                              "Ingreso_inseguro",
                                              "Estado_arresto",
                                              "Delito_unico_categ",
                                              "Anio_arresto",
                                              "proc_abreviado"),
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
  
  ols_data <- master_data.df %>%
    select(all_of(selectables),
           all_of(dependent_var),
    ) %>%
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
    arrange(Sexo, Educacion_obligatoria, Colo_piel_claro, LGBTQ, Etnia, Edad_menor30, Ingreso_inseguro, factor(Delito_unico_categ), factor(Estado_arresto), factor(Anio_arresto), proc_abreviado)
  
  formula <- selectables %>%
    t() %>%
    as.data.frame() %>%
    unite(., formula, sep = "+") %>%
    as.character()
  
  depVar <- dependent_var
  
  formula  <- as.formula(paste(depVar, "~", formula))
  ols    <- lm(formula,
                  data   = ols_data)
  
  sum_ols <- summary(ols)
  margEff      <- as.data.frame(sum_ols$coefficients) %>% 
    rownames_to_column() %>%
    mutate(factor = rowname,
           lower = Estimate-(1.96 * `Std. Error`),
           upper = Estimate+(1.96 * `Std. Error`),
           AME = Estimate)
  
    margEff <- margEff[2:8,] 
    

  margEff$factor <-recode(margEff$factor,
                          "SexoZFemenino" = "Mujer",
                          "SexoZMasculino" = "Hombre",
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



ols_demo_panel <- function(mainData = data2plot,
                             line_color = "#003b8a",
                             line_size  = 2,
                             point_color = "#003b8a",
                             point_size   = 4) {
  
  plot <- ggplot(mainData, aes(x = reorder(factor, -order_variable), y = AME)) +
    geom_hline(yintercept = 0, lty = 1, color = "#fa4d57", lwd = 1)  +
    geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),
                   lwd = line_size, position = position_dodge(width = .7), 
                   stat = "identity", color = line_color)+
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = point_size, position = position_dodge(width = .7), color = point_color) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = 2, position = position_dodge(width = .7), color = "white") +
    labs(y = "Menos probable                               Más probable") +
    scale_y_continuous(limits = c(-2, 2),
                       breaks = seq(-2, 2, by = 0.5),
                       expand = expansion(mult = 0.5), position = "right",
                       labels = c("-2", "-2.5", "-1", "-.5", "0", "+.5", "+1","+1.5","+2"))+
    WJP_theme()+
    coord_flip() +
    theme(legend.position = "none",
          panel.background   = element_blank(),
          panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                            size = 0.5, linetype = "dashed"),
          legend.title = element_blank(),
          axis.title.y       = element_blank(),
          axis.text.y        = element_text(family = "Lato Medium",
                                            size     = 3.514598*.pt,
                                            color    = "#4a4a49",
                                            hjust    = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.grid.minor.y = element_blank())
  
  return(plot)
}


## 1.  Logit Chart                                                                                   ----


logit_dataBase.fn <- function(data = Main_database,
                              selectables = c("Sexo", 
                                              "Educacion_obligatoria", 
                                              "Colo_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30", 
                                              "Ingreso_inseguro",
                                              "Estado_arresto",
                                              "Delito_unico_categ"),
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
           all_of(dependent_var),
           ) %>%
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
    arrange(Sexo, Educacion_obligatoria, Colo_piel_claro, LGBTQ, Etnia, Edad_menor30, Ingreso_inseguro, factor(Delito_unico_categ), factor(Estado_arresto))
  
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
  ) %>%
    filter(factor == "SexoZFemenino" |
             factor == "LGBTQZPertenece a la comunidad LGBTQ" |
             factor == "Ingreso_inseguroZFinancieramente inseguro" |
             factor == "EtniaZAfroamericano o indígena" |
             factor == "Educacion_obligatoriaZNo cuenta con título de bachiller" |
             factor == "Edad_menor30ZMenor a 30 años" |
             factor == "Colo_piel_claroZColor de piel oscura")
  
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



logit_demo_panel <- function(mainData = data2plot,
                             line_color = "#003b8a",
                             line_size  = 2,
                             point_color = "#003b8a",
                             point_size   = 4) {
  
  plot <- ggplot(mainData, aes(x = reorder(factor, -order_variable), y = AME)) +
    geom_hline(yintercept = 0, lty = 1, color = "#fa4d57", lwd = 1)  +
    geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),
                   lwd = line_size, position = position_dodge(width = .7), 
                   stat = "identity", color = line_color)+
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = point_size, position = position_dodge(width = .7), color = point_color) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = 2, position = position_dodge(width = .7), color = "white") +
    labs(y = "Menos probable                               Más probable") +
    scale_y_continuous(limits = c(-0.25, 0.25),
                       breaks = seq(-0.25, 0.25, by = 0.125),
                       expand = expansion(mult = 0.025), position = "right",
                       labels = c("-25", "-12.5", "0", "+12.5", "+25"))+
    WJP_theme()+
    coord_flip() +
    theme(legend.position = "none",
          panel.background   = element_blank(),
          panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                            size = 0.5, linetype = "dashed"),
          legend.title = element_blank(),
          axis.title.y       = element_blank(),
          axis.text.y        = element_text(family = "Lato Medium",
                                            size     = 3.514598*.pt,
                                            color    = "#4a4a49",
                                            hjust    = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.grid.minor.y = element_blank())
  
  return(plot)
}


## 2.  Line Chart                                                                                  ----

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
    drop_na() %>%
    pivot_longer(cols = c(dependent_var), names_to = "category", values_to = "value2plot") %>%
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


lineChartViz <- function(data = data2plot,
                         period = period,
                         order_value = order_value,
                         category = category,
                         labels = labels,
                         colors = category) {
  # Creating ggplot
  plt <- ggplot(data2plot, 
                aes(x     = reorder(period, order_value),
                    y     = value2plot,
                    color = category,
                    label = labels,
                    group = category)) +
    geom_point(size = 2,
               show.legend = F) +
    geom_line(size  = 1,
              show.legend = F) +
    geom_text_repel(family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0,0),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) +
    scale_x_discrete("period",
                     labels = c("Implementación" = "Implementación",
                                "Un año" = " ",
                                "Dos años" = "Dos años",
                                "Tres años" = " ",
                                "Cuatro años" = "Cuatro años",
                                "Cinco años" = " ",
                                "Seis años" = "Seis años",
                                "Siete años" = " ",
                                "Ocho años" = "Ocho años",
                                "Nueve años" = " ",
                                "Diez años" = "Diez años",
                                "Once años" = " ",
                                "Doce años" = "Doce años")
    ) +
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid")
    )
  
}




## 3. OLS                                                                             ----


ols_dataBase.fn <- function(data = Main_database,
                              selectables = c("Sexo", 
                                              "Educacion_obligatoria", 
                                              "Colo_piel_claro", 
                                              "LGBTQ", 
                                              "Etnia", 
                                              "Edad_menor30", 
                                              "Ingreso_inseguro",
                                              "Estado_arresto",
                                              "Delito_unico_categ"),
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
  
  ols_data <- master_data.df %>%
    select(all_of(selectables),
           all_of(dependent_var),
    ) %>%
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
    arrange(Sexo, Educacion_obligatoria, Colo_piel_claro, LGBTQ, Etnia, Edad_menor30, Ingreso_inseguro, factor(Delito_unico_categ), factor(Estado_arresto))
  
  formula <- selectables %>%
    t() %>%
    as.data.frame() %>%
    unite(., formula, sep = "+") %>%
    as.character()
  
  depVar <- dependent_var
  
  formula  <- as.formula(paste(depVar, "~", formula))
  ols    <- lm(formula,
                  data   = ols_data)
  
  summaryOls <- bind_rows(
    as.data.frame(coef(ols))
  )
  
  margEff      <- as.data.frame(
    margins_summary(ols, data = ols$data)
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



ols_demo_panel <- function(mainData = data2plot,
                             line_color = "#003b8a",
                             line_size  = 2,
                             point_color = "#003b8a",
                             point_size   = 4) {
  
  plot <- ggplot(mainData, aes(x = reorder(factor, -order_variable), y = AME)) +
    geom_hline(yintercept = 0, lty = 1, color = "#fa4d57", lwd = 1)  +
    geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),
                   lwd = line_size, position = position_dodge(width = .7), 
                   stat = "identity", color = line_color)+
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = point_size, position = position_dodge(width = .7), color = point_color) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = 2, position = position_dodge(width = .7), color = "white") +
    labs(y = "Menos probable                               Más probable") +
    scale_y_continuous(limits = c(-0.25, 0.25),
                       breaks = seq(-0.25, 0.25, by = 0.125),
                       expand = expansion(mult = 0.025), position = "right",
                       labels = c("-25", "-12.5", "0", "+12.5", "+25"))+
    WJP_theme()+
    coord_flip() +
    theme(legend.position = "none",
          panel.background   = element_blank(),
          panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                            size = 0.5, linetype = "dashed"),
          legend.title = element_blank(),
          axis.title.y       = element_blank(),
          axis.text.y        = element_text(family = "Lato Medium",
                                            size     = 3.514598*.pt,
                                            color    = "#4a4a49",
                                            hjust    = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.grid.minor.y = element_blank())
  
  return(plot)
}


## 4. Timeline



timeline <- function(data, group_var) {
  # Create the new data frame
  data2plot <- data %>%
    group_by(Anio_arresto) %>%
    summarise(value2plot = 100 * mean({{ group_var }}, na.rm = T)) %>%
    mutate(labels = if_else(
      Anio_arresto %in% c("2008", "2010", "2012", "2014", "2016", "2018", "2020"),
      paste0(round(value2plot,0), "%"), NA_character_),
      group_var = "National",
      colors = "#003B88"
      )
  
  
  colors4plot <- c("#003B88")
  
  # Creating ggplot
  
  plt <- ggplot(data2plot, 
                aes(x     = Anio_arresto,
                    y     = value2plot,
                    label = labels,
                    group = group_var,
                    color = group_var)) +
    geom_point(size = 2,
               show.legend = F) +
    geom_line(size  = 1,
              show.legend = F) +
    geom_text_repel(family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0,0),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) %>%
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid")
          
    )
  return(plt)
}


# 5. Timeline grupos

timeline_groups <- function(data, group_var) {
  # Create the new data frame
  data2plot <- data %>%
    filter(!is.na(grupo_implementacion)) %>%
    group_by(Anio_arresto,grupo_implementacion) %>%
    summarise(value2plot = 100 * mean({{ group_var }}, na.rm = T)) %>%
    mutate(labels = if_else(
      Anio_arresto %in% c("2008", "2010", "2012", "2014", "2016", "2018", "2020"),
      paste0(round(value2plot,0), "%"), NA_character_),
      group_var = "National",
      colors = "#003B88"
    )
  
  
  colors4plot <- c("Implementación media" = "#1a2580", 
                   "Implementación tardía" = "#a90099",
                   "Implementación temprana" = "#ef4b4b")
  
  # Creating ggplot
  
  plt <- ggplot(data2plot, 
                aes(x     = Anio_arresto,
                    y     = value2plot,
                    label = labels,
                    group = grupo_implementacion,
                    color = grupo_implementacion)) +
    geom_point(size = 2,
               show.legend = F) +
    geom_line(size  = 1,
              show.legend = F) +
    geom_text_repel(family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0,0),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) %>%
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid")
          
    )
  return(plt)
}




## 6. Timeline var continua



timeline_cont <- function(data, group_var) {
  # Create the new data frame
  data2plot <- data %>%
    group_by(Anio_arresto) %>%
    summarise(value2plot = mean({{ group_var }}, na.rm = T)) %>%
    mutate(labels = if_else(
      Anio_arresto %in% c("2008", "2010", "2012", "2014", "2016", "2018", "2020"),
      paste0(round(value2plot,0)), NA_character_),
      group_var = "National",
      colors = "#003B88"
    )
  
  
  colors4plot <- c("#003B88")
  
  # Creating ggplot
  
  plt <- ggplot(data2plot, 
                aes(x     = Anio_arresto,
                    y     = value2plot,
                    label = labels,
                    group = group_var,
                    color = group_var)) +
    geom_point(size = 2,
               show.legend = F) +
    geom_line(size  = 1,
              show.legend = F) +
    geom_text_repel(family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0,0),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) %>%
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid")
          
    )
  return(plt)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Libertad                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




# 3.2 (7-10) Libertad  ---------------------------------------------------------------

#3.2.7. Cambios en la proporción de personas en prisión preventiva (Línea de tiempo)



lineChart <- timeline(Main_database, PPO)

ggsave(plot   = lineChart,
       file   = paste0("National/Exploration/Output/Figure3_2_7/Figure3_2_7_1.svg"), 
       width  = 190, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


lineChart <- timeline_groups(Main_database, PPO)

ggsave(plot   = lineChart,
       file   = paste0("National/Exploration/Output/Figure3_2_7/Figure3_2_7_2.svg"), 
       width  = 190, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

#3.2.8. Trato diferenciado en la prisión preventiva oficiosa (Logit)


data2plot <- logit_dataBase.fn(dependent_var = "PPO")

logitPlot <- logit_demo_panel(mainData = data2plot, line_size = 2)

ggsave(plot   = logitPlot,
       file   = paste0("National/Exploration/Output/Figure3_2_8/Figure3_2_8.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


#3.2.9. Cambios en la severidad de las sentencias (Línea de tiempo)


lineChart <- timeline_cont(Main_database,tiempo_sentencia)

ggsave(plot   = lineChart,
       file   = paste0("National/Exploration/Output/Figure3_2_9/Figure3_2_9_1.svg"), 
       width  = 190, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


Base_homicidio <- Main_database %>% filter(Delito_unico_6_hom_dol==1)
lineChart <- timeline_cont(Base_homicidio,tiempo_sentencia)

ggsave(plot   = lineChart,
       file   = paste0("National/Exploration/Output/Figure3_2_9/Figure3_2_9_2.svg"), 
       width  = 190, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


#3.2.10. Trato diferenciado en la severidad de la sentencia (Logit)


data2plot <- ols_dataBase.fn(dependent_var = "tiempo_sentencia")

OlsPlot <- ols_demo_panel(mainData = data2plot, line_size = 2)

ggsave(plot   = OlsPlot,
       file   = paste0("National/Exploration/Output/Figure3_2_10/Figure3_2_10_1.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")


# Regresiones robustez:



lm3_2_9 <- lm(tiempo_sentencia ~ proc_abreviado + years_since_NSJP + factor(Delito_unico_categ) + factor(Estado_arresto) + factor(Anio_arresto), data = Main_database)
stargazer::stargazer(lm3_2_9,type = "text")



# PPO vs severidad

  data2plot <- Main_database %>%
    group_by(tiempo_sentencia) %>%
    summarise(value2plot = mean(PPO, na.rm = T)) %>%
    mutate(labels = if_else(
      tiempo_sentencia %in% c("0","10","15","20","30","40","50","60","70","80","90","100"),
      paste0(round(value2plot,2)), NA_character_),
      group_var = "PPO",
      colors = "#003B88"
    )
  
  
  colors4plot <- c("#003B88")
  
  # Creating ggplot
  
  plt <- ggplot(data2plot, 
                aes(x     = tiempo_sentencia,
                    y     = value2plot,
                    label = labels,
                    group = group_var,
                    color = group_var)) +
    geom_point(size = 2,
               show.legend = F) +
    geom_line(size  = 1,
              show.legend = F) +
    geom_text_repel(family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0,0),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) %>%
    scale_color_manual(values = colors4plot) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid")
          
    )

  ggsave(plot   = plt,
         file   = paste0("National/Exploration/Output/Figure3_2_adicional.svg"), 
         width  = 175, 
         height = 85,
         units  = "mm",
         dpi    = 72,
         device = "svg")

mod_PPO <-  lm(PPO ~ as.numeric(Anio_arresto) + tiempo_sentencia + factor(Estado_arresto) + factor(Delito_unico_categ) ,data=Main_database)
  
stargazer::stargazer(mod_PPO, type="text")
  