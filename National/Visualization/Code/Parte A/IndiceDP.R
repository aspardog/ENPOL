## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Percepcion proceso justo
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

# Loading data
load(paste0(path2SP,
            "/National/Data_cleaning/Output/Main_database.RData"))

master_data.df <- Main_database %>% 
  filter(Anio_arresto >= as.numeric(2008)) %>% 
  filter(NSJP == 1)                   

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Funciones                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


BarSimpleChartViz <- function(data = data2plot, 
                              x_var = main_var, 
                              y_var = value2plot, 
                              label_var = figure, 
                              fill_var = main_var,
                              order_var = order_var,
                              labels = labels,
                              shade_xminvalue,
                              shade_xmaxvalue,
                              x_labels = NULL  # Añadido argumento para etiquetas personalizadas
) {
  plt <- ggplot(data, 
                aes(x     = reorder({{x_var}},{{order_var}}),
                    y     = {{y_var}},
                    label = {{label_var}},
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9) +
    scale_fill_gradient(low = "#2a2a9A", high = "#2a2a9A") +
    annotate('rect', xmin=0, xmax= shade_xminvalue, ymin=0, ymax=100, alpha=.1, fill="#fa4d57") +
    annotate('rect', xmin=shade_xmaxvalue, xmax= shade_xmaxvalue+1, ymin=0, ymax=100, alpha=.1, fill="#43a9a7") +
    geom_text(aes(y    = {{y_var}} + 10),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    xlab("Porcentaje de criterios cumplidos") +
    ylab("Porcentaje de personas sentenciadas") +
    scale_y_continuous(labels = function(y) paste0(y, "%")) +
    scale_x_discrete(labels = x_labels) +  # Añadido para cambiar etiquetas del eje X
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_text(color    = "#4a4a49",
                                            family   = "Lato Full"),
          axis.title.x       = element_text(color    = "#4a4a49",
                                            family   = "Lato Full"),
          axis.text.y        = element_text(hjust = 0))
  
  return(plt)
}

index_setUp.fn <- function(data = master_data.df,
                           main_var){
  
  data_subset.df <- data %>%
    rename(main_var = all_of(main_var)) %>%
    group_by(main_var) %>%
    summarise(counter = n()) %>%
    drop_na %>%
    mutate(
      value2plot = counter / sum(counter),
      value2plot = value2plot*100,
      figure = paste0(round(value2plot,0), "%"),
      order_var = rank(main_var),
      labelx =paste0(round(main_var*100,0), "%")
    )
  
}



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



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Indicador general barras                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_general")


etiquetas <- data2plot$labelx

plot <- BarSimpleChartViz(shade_xminvalue = 6, shade_xmaxvalue = 10, x_labels = etiquetas); plot


ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso","/Indice DP",
                         "/figure1.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Logit positivo indicador general                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data2plot <- logit_dataBase.fn(dependent_var = "indicator_general_maxlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#43a9a7")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure_generalmax.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Logit negativo indicador general                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data2plot <- logit_dataBase.fn(dependent_var = "indicator_general_minlimit")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = T, shadow_color = "#fa4d57")
logitPlot
ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Indice_logits/figure_generalmin.svg"), 
       width  = 110, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Indicador general por estado                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Proceso justo barras                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_PJ")

etiquetas <- data2plot$labelx

plot <- BarSimpleChartViz(shade_xminvalue = NA, shade_xmaxvalue = NA, x_labels = etiquetas); plot
ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso","/Indice DP",
                         "/figure2.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Uso excesivo barras                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_UAA")

etiquetas <- data2plot$labelx

plot <- BarSimpleChartViz(shade_xminvalue = NA, shade_xmaxvalue = NA, x_labels = etiquetas); plot

ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso","/Indice DP",
                         "/figure3.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##  Tortura barras                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
data2plot <- index_setUp.fn(data = master_data.df,
                            main_var ="indicator_GDH")


etiquetas <- data2plot$labelx

plot <- BarSimpleChartViz(shade_xminvalue = NA, shade_xmaxvalue = NA, x_labels = etiquetas); plot

ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso","/Indice DP",
                         "/figure4.svg"), 
       width  = 200, 
       height = 80,
       units  = "mm",
       dpi    = 72,
       device = "svg")

