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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Proceso justo en el tiempo                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2010)  %>%
  mutate(
    guardar_silencio_detencion = 
      case_when(
        P3_14_5 == 1 ~ 1,
        P3_14_5 == 0 ~ 0
      ),
    guardar_silencio_mp = 
      case_when(
        P4_1_04 == 1 ~ 1,
        P4_1_04 == 2 ~ 0
      ),
    guardar_silencio_juez = 
      case_when(
        P5_2_4 == 1 ~ 1,
        P5_2_4 == 2 ~ 0
      )
  ) %>%
  group_by(Anio_arresto) %>%
  summarise(
    detencion = mean(guardar_silencio_detencion, na.rm = T),
    mp = mean(guardar_silencio_mp, na.rm = T),
    juez = mean(guardar_silencio_juez, na.rm = T)
  ) %>%
  pivot_longer(cols = c(detencion, mp, juez), names_to = "category", values_to = "value2plot") %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
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

names(colors4plot) <- c("detencion", "mp", "juez")

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
                         "/Output/Debido proceso/Proceso justo/figure1.svg"),
       width = 189.7883,
       height = 145,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Presion para autoincriminarse                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2010)  %>%
  mutate(
    presion_mp = 
      case_when(
        as.numeric(P4_7) == 4 | as.numeric(P4_7) == 5 ~ 1,
        as.numeric(P4_7) < 11 &  (as.numeric(P4_7) != 4 | as.numeric(P4_7) != 5) ~ 0
      ),
    presion_juez = 
      case_when(
        P5_7 == 1 ~ 1,
        P5_7 == 2 ~ 0
      )
  ) %>%
  group_by(Anio_arresto) %>%
  summarise(
    mp = mean(presion_mp, na.rm = T),
    juez = mean(presion_juez, na.rm = T)
  ) %>%
  pivot_longer(cols = c(mp, juez), names_to = "category", values_to = "value2plot") %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
         year = as.numeric(Anio_arresto))

# Pulling minimum and maximum available year
minyear <- 2011
maxyear <- 2021


# Creating a vector for yearly axis
x.axis.values <- seq(minyear, maxyear, by = 2)
sec.ticks     <- seq(minyear, maxyear, by = 1)
x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))


# Defining colors4plot
colors4plot <- c("#a90099","#43a9a7")

names(colors4plot) <- c("mp", "juez")

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
                         "/Output/Debido proceso/Proceso justo/figure2.svg"),
       width = 189.7883,
       height = 145,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Información detención                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2010)  %>%
  mutate(
    explicacion_detencion = 
      case_when(
        P3_14_4 == 1 ~ 1,
        P3_14_4 == 0 ~ 0
      ),
    explicacion_mp = 
      case_when(
        P4_1_03 == 1 ~ 1,
        P4_1_03 == 2 ~ 0
      ),
    explicacion_juez = 
      case_when(
        P5_2_1 == 1 ~ 1,
        P5_2_1 == 2 ~ 0
      )
  ) %>%
  group_by(Anio_arresto) %>%
  summarise(
    detencion = mean(explicacion_detencion, na.rm = T),
    mp = mean(explicacion_mp, na.rm = T),
    juez = mean(explicacion_juez, na.rm = T)
  ) %>%
  pivot_longer(cols = c(detencion, mp, juez), names_to = "category", values_to = "value2plot") %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
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

names(colors4plot) <- c("detencion", "mp", "juez")

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
                         "/Output/Debido proceso/Proceso justo/figure3.svg"),
       width = 189.7883,
       height = 145,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Claridad                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2010)  %>%
  mutate(
    claridad_mp = 
      case_when(
        P5_17_3 == 1 | P5_17_3 == 2~ 1,
        P5_17_3 == 3 | P5_17_3 == 4 ~ 0
      ),
    claridad_juez = 
      case_when(
        P5_17_2 == 1 | P5_17_2 == 2~ 1,
        P5_17_2 == 3 | P5_17_2 == 4 ~ 0
      )
  ) %>%
  group_by(Anio_arresto) %>%
  summarise(
    mp = mean(claridad_mp, na.rm = T),
    juez = mean(claridad_juez, na.rm = T)
  ) %>%
  pivot_longer(cols = c(mp, juez), names_to = "category", values_to = "value2plot") %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
         year = as.numeric(Anio_arresto))

# Pulling minimum and maximum available year
minyear <- 2011
maxyear <- 2021


# Creating a vector for yearly axis
x.axis.values <- seq(minyear, maxyear, by = 2)
sec.ticks     <- seq(minyear, maxyear, by = 1)
x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))


# Defining colors4plot
colors4plot <- c("#a90099","#43a9a7")

names(colors4plot) <- c("mp", "juez")

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
                         "/Output/Debido proceso/Proceso justo/figure4.svg"),
       width = 189.7883,
       height = 145,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. Defensa oportuna                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6. Variación tipo de defensa                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7. Tribunal transparente                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2010)  %>%
  mutate(
    video = 
      case_when(
        P5_19_3 == 1 ~ 1,
        P5_19_3 == 2 ~ 0
      ),
    publico = 
      case_when(
        P5_16_5 == 1 | P5_16_5 == 2 | P5_16_5 == 3 ~ 1,
        P5_16_5 == 4 ~ 0
      )
  ) %>%
  group_by(Anio_arresto) %>%
  summarise(
    video = mean(video, na.rm = T),
    publico = mean(publico, na.rm = T)
  ) %>%
  pivot_longer(cols = c(video, publico), names_to = "category", values_to = "value2plot") %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
         year = as.numeric(Anio_arresto))

# Pulling minimum and maximum available year
minyear <- 2011
maxyear <- 2021


# Creating a vector for yearly axis
x.axis.values <- seq(minyear, maxyear, by = 2)
sec.ticks     <- seq(minyear, maxyear, by = 1)
x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))


# Defining colors4plot
colors4plot <- twoCOLORS

names(colors4plot) <- c("video", "publico")

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
                         "/Output/Debido proceso/Proceso justo/figure7.svg"),
       width = 189.7883,
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 8. Tribunal imparcial                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2010)  %>%
  mutate(
    culpable_antes = 
      case_when(
        P5_25 == 1 ~ 1,
        P5_25 == 2 ~ 0
      ),
    juez_diferente = 
      case_when(
        P5_14 == 1 ~ 1,
        P5_14 == 2 ~ 0
      )
  ) %>%
  group_by(Anio_arresto) %>%
  summarise(
    culpable = mean(culpable_antes, na.rm = T),
    juez = mean(juez_diferente, na.rm = T)
  ) %>%
  pivot_longer(cols = c(culpable, juez), names_to = "category", values_to = "value2plot") %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
         year = as.numeric(Anio_arresto))

# Pulling minimum and maximum available year
minyear <- 2011
maxyear <- 2021


# Creating a vector for yearly axis
x.axis.values <- seq(minyear, maxyear, by = 2)
sec.ticks     <- seq(minyear, maxyear, by = 1)
x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))


# Defining colors4plot
colors4plot <- twoCOLORS

names(colors4plot) <- c("culpable", "juez")

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
                         "/Output/Debido proceso/Proceso justo/figure8.svg"),
       width = 189.7883,
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 9. Tribunal presente y responsivo                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  filter(Anio_arresto > 2010)  %>%
  mutate(
    juez_presente = 
      case_when(
        P5_16_2 == 1  ~ 1,
        P5_16_2 == 2 | P5_16_2 == 3 | P5_16_2 == 4 ~ 0
      ),
    juez_control = 
      case_when(
        P5_18 == 2 ~ 1,
        P5_18 == 1 | P5_18 == 3 | P5_18 == 4 | P5_18 == 5 ~ 0
      ),
    juez_escucha =
      case_when(
        P5_26 == 1 | P5_26 == 2 ~ 1,
        P5_26 == 3 | P5_26 == 4 ~ 0
      )
  ) %>%
  group_by(Anio_arresto) %>%
  summarise(
    juez_presente = mean(juez_presente, na.rm = T),
    juez_control = mean(juez_control, na.rm = T),
    juez_escucha = mean(juez_escucha, na.rm = T)
  ) %>%
  pivot_longer(cols = c(juez_presente, juez_control, juez_escucha), names_to = "category", values_to = "value2plot") %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
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

names(colors4plot) <- c("juez_presente", "juez_control", "juez_escucha")

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
                         "/Output/Debido proceso/Proceso justo/figure9.svg"),
       width = 189.7883,
       height = 100,
       units  = "mm",
       dpi    = 72,
       device = "svg")
