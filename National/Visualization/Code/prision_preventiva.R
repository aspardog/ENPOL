## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - Detenciones
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
## 1. Prisión preventiva: proporción                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  filter(sentenciado == 1) %>%
  mutate(
    prision_preventiva =
      case_when(
        P5_9 == 1 ~ "En prisión preventiva",
        P5_9 == 2 ~ "En libertad"
      ),
    counter = 1,
  ) %>%
  group_by(counter) %>%
  mutate(
    n_obs = n()
  ) %>%
  group_by(prision_preventiva) %>%
  summarise(
    value2plot = sum(counter, na.rm = T)/n_obs
    ) %>%
  ungroup() %>%
  distinct() %>%
  drop_na()

data2plot <- data_subset.df %>%
  mutate(
    value2plot = value2plot*100,
    labels = prision_preventiva,
    figure = paste0(round(value2plot,0), "%"),
    order_var = case_when(
      labels == "En prisión preventiva" ~ 2,
      labels =="En libertad" ~ 1,
      T ~ NA_real_)
      )

colors4plot <- rep(mainCOLOR,2)
plot <- barsChart.fn(data.df                    = data2plot,
                     groupVar                   = F,   
                     categories_grouping_var    = labels,
                     colors4plot                = colors4plot, 
                     order                      = T,
                     orientation                = "horizontal")

ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/Prision preventiva/figure1.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Prisión preventiva: debido proceso                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  mutate(
    P5_34_A = replace(P5_34_A, P5_34_A %in% c( "98", "99"), NA),
    P5_34_M = replace(P5_34_M, P5_34_M %in% c( "98", "99"), NA),
    P5_34_A = replace(P5_34_A, P5_34_A %in% c( "96"), 0),
    P5_34_M = replace(P5_34_M, P5_34_M %in% c( "96"), 0),
    P5_10 = replace(P5_10, P5_10 %in% c("8", "9"), NA)) %>% 
  mutate(
    procesados_meses_pp = ((as.numeric(P5_34_A)*12) + as.numeric(P5_34_M)),
    counter = 1
    ) %>% 
  mutate(
    mas2anios_prisionpreventiva = 
      case_when(as.numeric(P5_10) == 7 ~ 1,
                procesados_meses_pp  > 24   ~ 1,
                as.numeric(P5_10) == 1 | as.numeric(P5_10) == 2 | 
                  as.numeric(P5_10) == 3 | as.numeric(P5_10) == 4 |
                  as.numeric(P5_10) == 5 |as.numeric(P5_10) == 6 ~ 0,
                procesados_meses_pp  <= 24 ~ 0,
                T ~ NA_real_)
    ) %>% 
  mutate(
    mas2anios_prisionpreventiva = 
      case_when(mas2anios_prisionpreventiva == 1 ~ "Más de 2 años",
                mas2anios_prisionpreventiva == 0 ~ "Menos, o hasta, 2 años",
                                                 T ~ NA_character_)) %>%
  group_by(mas2anios_prisionpreventiva) %>%
  summarise(
    value2plot = sum(counter, na.rm = T)
  ) %>%
  drop_na() %>%
  mutate(
    value2plot = value2plot/sum(value2plot)
  ) %>%
  rename(group_var = mas2anios_prisionpreventiva)

data2plot <- data_subset.df %>%
  mutate(
    value2plot = value2plot*100,
    labels = group_var,
    figure = paste0(round(value2plot,0), "%"),
    order_var = case_when(
      labels == "Más de 2 años" ~ 1,
      labels == "Menos, o hasta, 2 años" ~ 2,
      T ~ NA_real_)
  )

colors4plot <- c("Menos, o hasta, 2 años" =mainCOLOR,
                 "Más de 2 años" ="#ef4b4b")

plot <- barsChart.fn(data.df                    = data2plot,
                     groupVar                   = F,  
                     categories_grouping_var    = labels,
                     colors4plot                = colors4plot, 
                     order                      = T,
                     orientation                = "horizontal")

ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/Prision preventiva/figure2.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Prisión preventiva: series de tiempo                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2015)  %>%
  group_by(Anio_arresto) %>%
  mutate(
    counter = 1,
    n_obs = if_else(!is.na(tipo_prision_preventiva), sum(counter, na.rm = T), NA_real_)
  ) %>%
  ungroup() %>%
  group_by(Anio_arresto, tipo_prision_preventiva) %>%
  summarise(
    value2plot = sum(counter, na.rm = T)/n_obs
  ) %>%
  drop_na() %>%
  distinct() %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
         category = tipo_prision_preventiva,
         year = as.numeric(Anio_arresto)) %>%
  mutate(label = if_else(category == "Prisión Preventiva Oficiosa" | category == "Prisión Preventiva Justificada", 
                         label, NA_character_)) %>%
  filter(category %in% c("Prisión Preventiva Oficiosa", "Prisión Preventiva Justificada"))


# Pulling minimum and maximum available year
minyear <- 2016
maxyear <- 2021


# Creating a vector for yearly axis
x.axis.values <- seq(minyear, maxyear, by = 2)
sec.ticks     <- seq(minyear, maxyear, by = 1)
x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))


# Defining colors4plot
colors4plot <- c("Prisión Preventiva Oficiosa" = "#003B88",
                 "Prisión Preventiva Justificada" = "#a90099"
)

# Saving data points
data2plot <- data_subset.df %>% ungroup()

# Applying plotting function
chart <- LAC_lineChart(data           = data2plot,
                       target_var     = "value2plot",
                       grouping_var   = "year",
                       ngroups        = data_subset.df$category, 
                       labels_var     = "label",
                       colors_var     = "category",
                       colors         = colors4plot,
                       repel          = T,
                       custom.axis    = T,
                       x.breaks       = x.axis.values,
                       x.labels       = x.axis.labels,
                       sec.ticks      = sec.ticks)

ggsave(plot = chart, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/Prision preventiva/figure3.svg"),
       width = 189.7883,
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Prision preventiva: Logit                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Applying plotting function
data_subset.df <- master_data.df %>%
  mutate(
    prision_preventiva = 
      case_when(
        tipo_prision_preventiva == "Prisión Preventiva Oficiosa" ~ 1,
        tipo_prision_preventiva == "Prisión Preventiva Justificada" ~ 0,
        tipo_prision_preventiva == "Proceso en libertad" ~ 0
      )
  )

data2plot <- logit_dataBase.fn(data = data_subset.df,
                               dependent_var = "prision_preventiva")

logitPlot <- logit_demo_panel(mainData = data2plot, shadow = F)

ggsave(plot   = logitPlot,
       file   = paste0(path2SP,
                       "/National/Visualization",
                       "/Output/Debido proceso/Prision preventiva/figure4.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")
