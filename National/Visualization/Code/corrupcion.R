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
  mutate( detencion_corrupcion = case_when(P3_21_1 == "1" | P3_21_2 == "1" ~ 1,
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
                                         T~ NA_real_)) %>%
  pivot_longer(cols = c(detencion_corrupcion, mp_corrupcion, juzgado_corrupcion), 
               names_to = "group_var", values_to = "value2plot"
  ) %>%
  group_by(Anio_arresto, group_var) %>%
  summarise(
    value2plot = mean(value2plot, na.rm = T)
  ) %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
         category = group_var,
         year = as.numeric(Anio_arresto))

# Pulling minimum and maximum available year
minyear <- 2011
maxyear <- 2021


# Creating a vector for yearly axis
x.axis.values <- seq(minyear, maxyear, by = 2)
sec.ticks     <- seq(minyear, maxyear, by = 1)
x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))


# Defining colors4plot
colors4plot <- threeColors
names(colors4plot) <- c("detencion_corrupcion", "juzgado_corrupcion", "mp_corrupcion")

# Saving data points
data2plot <- data_subset.df %>% ungroup()

# Applying plotting function
chart <- LAC_lineChart(data           = data2plot,
                       target_var     = "value2plot",
                       grouping_var   = "year",
                       ngroups        = data2plot$category, 
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
                         "/Output/Debido proceso/Corrupcion/figure1.svg"),
       width = 189.7883,
       height = 68.88612,
       units  = "mm",
       dpi    = 72,
       device = "svg")
