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
## 1. Uso Excesivo de la fuerza: Serie temporal                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2010) %>%
  mutate(
    uso_excesivo =
      case_when(
        proporcionalidad_uso_fuerza == 0 ~ 1,
        proporcionalidad_uso_fuerza == 1 ~ 0
      )
  ) %>%
  group_by(Anio_arresto) %>%
  summarise(
    value2plot = mean(uso_excesivo, na.rm = T)
  ) %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
         category = "uso_excesivo",
         year = as.numeric(Anio_arresto))

# Pulling minimum and maximum available year
minyear <- 2011
maxyear <- 2021


# Creating a vector for yearly axis
x.axis.values <- seq(minyear, maxyear, by = 2)
sec.ticks     <- seq(minyear, maxyear, by = 1)
x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))


# Defining colors4plot
colors4plot <- mainCOLOR
names(colors4plot) <- "uso_excesivo"

# Saving data points
data2plot <- data_subset.df %>% ungroup()

# Applying plotting function
chart <- LAC_lineChart(data           = data2plot,
                       target_var     = "value2plot",
                       grouping_var   = "year",
                       ngroups        = 1, 
                       labels_var     = "label",
                       colors_var     = "category",
                       colors         = colors4plot,
                       repel          = F,
                       custom.axis    = T,
                       x.breaks       = x.axis.values,
                       x.labels       = x.axis.labels,
                       sec.ticks      = sec.ticks)

ggsave(plot = chart, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/Uso excesivo/figure1.svg"),
       width = 189.7883,
       height = 68.88612,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Uso Excesivo de la fuerza: Logit                                                          ----
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
  
  master_data.df <- Main_database %>%
    filter(Anio_arresto >= 2015) %>%
    filter(NSJP == 1) %>%
    filter(Delito_unico == 1) %>%
    mutate(
      Educacion_superior = 
        case_when(
          Educacion_superior == 1 ~ "Cuenta con título de educación universitaria",
          Educacion_superior == 0 ~ "No cuenta con título de educación universitario",
        ),
      Color_piel_claro       =
        case_when(
          Color_piel_claro      == 1 ~ "Color de piel claro",
          Color_piel_claro      == 0 ~ "Color de piel oscuro",
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
      Edad_menor30          =
        case_when(
          Edad_menor30          == 1 ~ "Menor a 30 años",
          Edad_menor30          == 0 ~ "Mayor o igual a 30 años"
        )
    )
  
  selectables <- selectables
  
  logit_data <- master_data.df %>%
    select(all_of(selectables),
           all_of(dependent_var)) %>%
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
    arrange(Sexo, Educacion_superior, Color_piel_claro, LGBTQ, Etnia, Edad_menor30)
  
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
  )
  
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
               factor == "Con educación univeristaria \n o más"              ~ 4,
               factor == "Afroamericano/a o indígena"         ~ 5,
               factor == "Color de piel claro"               ~ 6
             ),
           dependent_var  =
             dependent_var
    )
  
  return(data2table)
  
}

# Applying plotting function
data2plot <- logit_dataBase.fn(dependent_var = "proporcionalidad_uso_fuerza")

logitPlot <- logit_demo_panel(mainData = data2plot)

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



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Uso Excesivo de la fuerza: Corporación                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  mutate(
    uso_excesivo =
      case_when(
        proporcionalidad_uso_fuerza == 0 ~ 1,
        proporcionalidad_uso_fuerza == 1 ~ 0
      )
  ) %>%
  group_by(Corporacion_grupos) %>%
  summarise(
    value2plot = mean(uso_excesivo, na.rm = T)
  ) %>%
  drop_na() %>%
  filter(Corporacion_grupos != "Guardia Nacional") %>%
  filter(Corporacion_grupos != "NS/NR") %>%
  filter(Corporacion_grupos != "Otra") %>%
  rename(group_var = Corporacion_grupos)

data2plot <- data_subset.df %>%
  arrange(value2plot) %>%
  mutate(
    order_var = row_number(),
    value2plot = value2plot*100,
    figure = paste0(round(value2plot, 0), "%"),
    labels = 
      case_when(
        group_var == "Ejército o Marina"  ~ "Ejército o Marina",
        group_var == "Operativo Conjunto" ~ "Operativo Conjunto",
        group_var == "Policía Estatal" ~ "Policía Estatal",
        group_var == "Policía Estatal Ministerial o Judicial" ~ "Policía Estatal Ministerial <br>o Judicial",
        group_var == "Policía Federal" ~ "Policía Federal",
        group_var == "Policía Federal Ministerial" ~ "Policía Federal Ministerial",
        group_var == "Policía Municipal" ~ "Policía Municipal",
        
      )
  )

colors4plot <- mainCOLOR
plot <- barsChart.fn(data.df                    = data2plot,
                     groupVar                   = F,   
                     categories_grouping_var    = categories,
                     colors4plot                = colors4plot, 
                     order                      = T,
                     orientation                = "vertical",
                     title                      = NULL,
                     subtitle                   = NULL,
                     note                       = NULL)

ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/Uso excesivo/figure3.svg"),
       width = 189.7883,
       height = 105,
       units  = "mm",
       dpi    = 72,
       device = "svg")
