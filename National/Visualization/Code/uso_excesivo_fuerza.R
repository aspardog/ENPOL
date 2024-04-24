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
