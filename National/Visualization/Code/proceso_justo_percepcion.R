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
    proceso_justo = 
      case_when(
        as.numeric(P5_26A) == 1 ~ 1,
        as.numeric(P5_26A) == 0 ~ 0,
        T ~ NA_real_
      )) %>%
  group_by(Anio_arresto) %>%
  summarise(
    value2plot = mean(proceso_justo, na.rm = T)
  ) %>%
  mutate(value2plot = value2plot*100,
         label = paste0(format(round(value2plot, 0),
                               nsmall = 0),
                        "%"),
         category = "proceso_justo",
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
names(colors4plot) <- "proceso_justo"

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
                         "/Output/Debido proceso/Proceso justo percepcion/figure1.svg"),
       width = 189.7883,
       height = 68.88612,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. indicadores                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  filter(sentenciado == 1) %>%
  mutate(
    proceso_justo = 
      case_when(
        as.numeric(P5_26A) == 1 ~ 1,
        as.numeric(P5_26A) == 0 ~ 0,
        T ~ NA_real_
      )
  ) 

data2table <- data_subset.df %>%
  group_by(proceso_justo) %>%
  summarise(
    `Índice 13 criterios mínimos` = mean(indicator_general, na.rm = T),
    `Sub-Índice de protección de derechos humanos` = mean(indicator_GDH, na.rm = T),
    `Sub-Índice de uso no arbitrario de la autoridad` = mean(indicator_UAA, na.rm = T),
    `Sub-Índice de proceso justo` = mean(indicator_PJ, na.rm = T)
    ) %>% 
  drop_na() %>%
  mutate(
    proceso_justo = 
      case_when(
        proceso_justo == 1 ~ "Proceso justo",
        proceso_justo == 0 ~ "Proceso injusto"
      )
  ) %>%
  pivot_longer(cols = !proceso_justo, names_to = "category", values_to = "value2plot") %>%
  rbind(data.frame(category = c("— — — — — — — — — — — — — — — — — — — — — —", 
                                "— — — — — — — — — — — — — — — — — — — — — —"),
                   value2plot = c(NA_real_, NA_real_),
                   proceso_justo = c("Proceso justo", "Proceso injusto"))
  ) %>%
  mutate(
    order_value =
      case_when(
        category == "Índice 13 criterios mínimos" ~ 1,
        category == "Sub-Índice de uso no arbitrario de la autoridad" ~ 4,
        category == "Sub-Índice de protección de derechos humanos" ~ 5,
        category == "Sub-Índice de proceso justo" ~ 3,
        category == "— — — — — — — — — — — — — — — — — — — — — —" ~ 2
      )
  )

justo.df <- data2table %>%
  filter(proceso_justo == "Proceso justo")

injusto.df <- data2table %>%
  filter(proceso_justo == "Proceso injusto")

colors4plot <-  c("#2a2a9A","#ef4b4b")
names(colors4plot) <- c("Proceso justo",
                 "Proceso injusto")

p <- ggplot(data2table,
            aes(x = value2plot,
                y = reorder(category, -order_value))) +
  geom_segment(data = justo.df,
               aes(x = value2plot, y = reorder(category, -order_value),
                   yend = reorder(injusto.df$category, -order_value),
                   xend = injusto.df$value2plot), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  geom_hline(yintercept = 4, linetype = "longdash", color = "black", size = 0.25) +
  geom_point(aes(x = value2plot, y = category, color = proceso_justo), size = 4, show.legend = F)  +
  geom_text(aes(x = value2plot, y = category, 
                label = paste0(round(value2plot*100,0),"%"), 
                family = "Lato Full", fontface = "bold"), 
            size= 3.514598, color = "black", vjust = -1) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = colors4plot) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1),limits = c(0,1),
                     labels = scales::percent_format(accuracy = 1), position = "top")+
  WJP_theme() +
  theme(legend.position="bottom",
        panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                          size = 0.5),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(family = "Lato Bold"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(family = "Lato Medium",
                                 size = 3.514598*.pt,
                                 color = "Black", hjust = 0));p

ggsave(plot = p, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/Proceso justo percepcion/figure2.svg"),
       width = 189.7883,
       height = 110,
       units  = "mm",
       dpi    = 72,
       device = "svg")
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Procedimiento                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  filter(sentenciado == 1) %>%
  mutate(
    proceso_justo = 
      case_when(
        as.numeric(P5_26A) == 1 ~ 1,
        as.numeric(P5_26A) == 0 ~ 0,
        T ~ NA_real_
      ),
    procedimiento =
      case_when(
        as.numeric(P5_6) == 1 ~ "Juicio",
        as.numeric(P5_6) == 2 ~ "Procedimiento abreviado",
        T ~ NA_character_
      )
  ) %>%
  group_by(procedimiento) %>%
  summarise(
    value2plot = mean(proceso_justo, na.rm = T)
  ) %>%
  drop_na()
  
data2plot <- data_subset.df %>%
  mutate(
    value2plot = value2plot*100,
    labels = procedimiento,
    figure = paste0(round(value2plot,0), "%"),
    order_var = case_when(
      labels == "Juicio" ~ 2,
      labels =="Procedimiento abreviado" ~ 1,
      T ~ NA_real_)
  )

colors4plot <-  c("#2a2a9A","#ef4b4b")
plot <- barsChart.fn(data.df                    = data2plot,
                     groupVar                   = F,   
                     categories_grouping_var    = labels,
                     colors4plot                = colors4plot, 
                     order                      = T,
                     orientation                = "horizontal")    

ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/Proceso justo percepcion/figure3.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Culpabilidad                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- master_data.df %>%
  filter(sentenciado == 1) %>%
  mutate(
    proceso_justo = 
      case_when(
        as.numeric(P5_26A) == 1 ~ 1,
        as.numeric(P5_26A) == 0 ~ 0,
        T ~ NA_real_
      ),
    culpabilidad = 
      case_when(
        as.numeric(P3_1) == 1 | as.numeric(P3_1) == 2 ~ "Autoreconocimiento como culpable",
        as.numeric(P3_1) == 3 | as.numeric(P3_1) == 4 ~ "Autoreconocimiento como inocente",
        T ~ NA_character_
      )
  ) %>%
  group_by(culpabilidad) %>%
  summarise(
    value2plot = mean(proceso_justo, na.rm = T)
  ) %>%
  drop_na()
  
data2plot <- data_subset.df %>%
  mutate(
    value2plot = value2plot*100,
    labels = culpabilidad,
    figure = paste0(round(value2plot,0), "%"),
    order_var = case_when(
      labels == "Autoreconocimiento como culpable" ~ 2,
      labels == "Autoreconocimiento como inocente" ~ 1,
      T ~ NA_real_)
  )

colors4plot <-  c("#2a2a9A","#ef4b4b")
plot <- barsChart.fn(data.df                    = data2plot,
                     groupVar                   = F,   
                     categories_grouping_var    = labels,
                     colors4plot                = colors4plot, 
                     order                      = T,
                     orientation                = "horizontal")    

ggsave(plot = plot, 
       filename = paste0(path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/Proceso justo percepcion/figure4.svg"),
       width = 189.7883,
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")
