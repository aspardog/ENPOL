# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Piloto de Hipótesis
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##
## Dependencies:      World Justice Project
##
## Creation date:     Jun 5th, 2024
##
## This version:      Jun 5th, 2024
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

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

Main_database <- Main_database %>% 
  mutate(
    P5_4_A = as.numeric(P5_4_A),
    tiempo_sentencia = case_when(P5_4_A == 0 ~ NA_real_,
                                 P5_4_A >= 97 ~ NA_real_,
                                 T ~ P5_4_A))


# Probar que el descriptivo y la regresion sin controles dan los mismos resultados:

base_prueba <- Main_database %>% filter(Anio_arresto>=2011 & Anio_arresto<= 2021)

prueba <- lm(det_ninguna ~ relevel(factor(Anio_arresto),"2021"), data = base_prueba)

contraste <- Main_database %>% 
  filter(Anio_arresto>=2011 & Anio_arresto<= 2021) %>%
  group_by(Anio_arresto) %>%
  summarize(det_ninguna = mean(det_ninguna, na.rm=T)) #el descriptivo de cada anio SI es el intercepto mas el coeficiente del anio


# Controlar por tiempo de sentencia

prueba2 <- lm(det_ninguna ~ relevel(factor(Anio_arresto), "2021") + tiempo_sentencia, data = base_prueba)
prueba3 <- lm(det_ninguna ~ relevel(factor(Anio_arresto), "2021") + tiempo_sentencia + I(tiempo_sentencia^2), data = base_prueba)
prueba4 <- lm(det_ninguna ~ relevel(factor(Anio_arresto), "2021") + tiempo_sentencia + relevel(factor(Anio_arresto), "2021")*tiempo_sentencia, data = base_prueba)

stargazer::stargazer(prueba,prueba2,prueba3,prueba4, type="text")


# Plot


data2plot1 <-  as.tibble(summary(prueba)$coefficients) %>% cbind(var = row.names(summary(prueba)$coefficients), .) %>% mutate(model="base")
data2plot2 <-  as.tibble(summary(prueba2)$coefficients) %>% cbind(var = row.names(summary(prueba2)$coefficients), .) %>% mutate(model="2")
data2plot3 <-  as.tibble(summary(prueba3)$coefficients) %>% cbind(var = row.names(summary(prueba3)$coefficients), .) %>% mutate(model="3")
data2plot4 <-  as.tibble(summary(prueba4)$coefficients) %>% cbind(var = row.names(summary(prueba4)$coefficients), .) %>% mutate(model="4")

base_values <- c(data2plot1$Estimate[1],data2plot2$Estimate[1],data2plot3$Estimate[1],data2plot4$Estimate[1])

data2plot <- bind_rows(data2plot1,data2plot4) %>% 
  mutate(Anio = case_when(var=="(Intercept)" ~ "2021",
                          var=="relevel(factor(Anio_arresto), \"2021\")2011" ~ "2011",
                          var=="relevel(factor(Anio_arresto), \"2021\")2012" ~ "2012",
                          var=="relevel(factor(Anio_arresto), \"2021\")2013" ~ "2013",
                          var=="relevel(factor(Anio_arresto), \"2021\")2014" ~ "2014",
                          var=="relevel(factor(Anio_arresto), \"2021\")2015" ~ "2015",
                          var=="relevel(factor(Anio_arresto), \"2021\")2016" ~ "2016",
                          var=="relevel(factor(Anio_arresto), \"2021\")2017" ~ "2017",
                          var=="relevel(factor(Anio_arresto), \"2021\")2018" ~ "2018",
                          var=="relevel(factor(Anio_arresto), \"2021\")2019" ~ "2019",
                          var=="relevel(factor(Anio_arresto), \"2021\")2020" ~ "2020")) %>%
  mutate(value2plot =  case_when(model=="base" & Anio!=2021 ~ 100*(base_values[1]+Estimate),
                                 model=="4" & Anio!=2021 ~ 100*(base_values[4]+Estimate),
                                 Anio==2021 ~ 100*Estimate),
         labels = paste0(round(value2plot,0), "%"),
         group_var =  model) %>%
  filter(!is.na(Anio))

Estimated_diffval1 <- filter(data2plot, Anio == 2021, model == "base") %>% select(value2plot) %>% unlist()
Estimated_diffval2 <- filter(data2plot, Anio == 2021, model == "4") %>% select(value2plot) %>% unlist()
Estimated_diff1 <- Estimated_diffval1 - Estimated_diffval2

data2plot_5 <- data2plot4 %>% 
  mutate(Anio = case_when(var=="(Intercept)" ~ "2021",
                          var=="relevel(factor(Anio_arresto), \"2021\")2011" ~ "2011",
                          var=="relevel(factor(Anio_arresto), \"2021\")2012" ~ "2012",
                          var=="relevel(factor(Anio_arresto), \"2021\")2013" ~ "2013",
                          var=="relevel(factor(Anio_arresto), \"2021\")2014" ~ "2014",
                          var=="relevel(factor(Anio_arresto), \"2021\")2015" ~ "2015",
                          var=="relevel(factor(Anio_arresto), \"2021\")2016" ~ "2016",
                          var=="relevel(factor(Anio_arresto), \"2021\")2017" ~ "2017",
                          var=="relevel(factor(Anio_arresto), \"2021\")2018" ~ "2018",
                          var=="relevel(factor(Anio_arresto), \"2021\")2019" ~ "2019",
                          var=="relevel(factor(Anio_arresto), \"2021\")2020" ~ "2020")) %>%
  mutate(value2plot =  case_when(Anio!=2021 ~ 100*(base_values[4]+Estimate),
                                 Anio==2021 ~ 100*Estimate)) %>%
  filter(!is.na(Anio)) %>%
  mutate(value2plot = value2plot + Estimated_diff1,
         labels = paste0(round(value2plot,0), "%"),
         group_var =  "Estimated value")

data2plot <- bind_rows(data2plot,data2plot_5)

colors4plot <- c("#003B88", "#43a9a7", "#fa4d57")

plt <- ggplot(data2plot, 
              aes(x     = Anio,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = group_var)) +
  geom_point(size = 2,
             show.legend = F) +
  geom_line(size  = 1,
            show.legend = F) +
  geom_text_repel(
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
  expand_limits(y = c(0, 100))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"))

ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Grafica_prueba_hip_sentencia_1.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")








###### EJEMPLO ADICIONAL

#Repetir con orden de det 


prueba <- lm(orden_det ~ relevel(factor(Anio_arresto),"2021"), data = base_prueba)

contraste <- Main_database %>% 
  filter(Anio_arresto>=2011 & Anio_arresto<= 2021) %>%
  group_by(Anio_arresto) %>%
  summarize(orden_det = mean(orden_det, na.rm=T)) #el descriptivo de cada anio SI es el intercepto mas el coeficiente del anio


# Controlar por tiempo de sentencia

prueba2 <- lm(orden_det ~ relevel(factor(Anio_arresto), "2021") + tiempo_sentencia, data = base_prueba)
prueba3 <- lm(orden_det ~ relevel(factor(Anio_arresto), "2021") + tiempo_sentencia + I(tiempo_sentencia^2), data = base_prueba)
prueba4 <- lm(orden_det ~ relevel(factor(Anio_arresto), "2021") + tiempo_sentencia + relevel(factor(Anio_arresto), "2021")*tiempo_sentencia, data = base_prueba)

stargazer::stargazer(prueba,prueba2,prueba3,prueba4, type="text")


# Plot


data2plot1 <-  as.tibble(summary(prueba)$coefficients) %>% cbind(var = row.names(summary(prueba)$coefficients), .) %>% mutate(model="base")
data2plot2 <-  as.tibble(summary(prueba2)$coefficients) %>% cbind(var = row.names(summary(prueba2)$coefficients), .) %>% mutate(model="2")
data2plot3 <-  as.tibble(summary(prueba3)$coefficients) %>% cbind(var = row.names(summary(prueba3)$coefficients), .) %>% mutate(model="3")
data2plot4 <-  as.tibble(summary(prueba4)$coefficients) %>% cbind(var = row.names(summary(prueba4)$coefficients), .) %>% mutate(model="4")

base_values <- c(data2plot1$Estimate[1],data2plot2$Estimate[1],data2plot3$Estimate[1],data2plot4$Estimate[1])

data2plot <- bind_rows(data2plot1,data2plot4) %>% 
  mutate(Anio = case_when(var=="(Intercept)" ~ "2021",
                          var=="relevel(factor(Anio_arresto), \"2021\")2011" ~ "2011",
                          var=="relevel(factor(Anio_arresto), \"2021\")2012" ~ "2012",
                          var=="relevel(factor(Anio_arresto), \"2021\")2013" ~ "2013",
                          var=="relevel(factor(Anio_arresto), \"2021\")2014" ~ "2014",
                          var=="relevel(factor(Anio_arresto), \"2021\")2015" ~ "2015",
                          var=="relevel(factor(Anio_arresto), \"2021\")2016" ~ "2016",
                          var=="relevel(factor(Anio_arresto), \"2021\")2017" ~ "2017",
                          var=="relevel(factor(Anio_arresto), \"2021\")2018" ~ "2018",
                          var=="relevel(factor(Anio_arresto), \"2021\")2019" ~ "2019",
                          var=="relevel(factor(Anio_arresto), \"2021\")2020" ~ "2020")) %>%
  mutate(value2plot =  case_when(model=="base" & Anio!=2021 ~ 100*(base_values[1]+Estimate),
                                 model=="4" & Anio!=2021 ~ 100*(base_values[4]+Estimate),
                                 Anio==2021 ~ 100*Estimate),
         labels = paste0(round(value2plot,0), "%"),
         group_var =  model) %>%
  filter(!is.na(Anio))

Estimated_diffval1 <- filter(data2plot, Anio == 2021, model == "base") %>% select(value2plot) %>% unlist()
Estimated_diffval2 <- filter(data2plot, Anio == 2021, model == "4") %>% select(value2plot) %>% unlist()
Estimated_diff1 <- Estimated_diffval1 - Estimated_diffval2

data2plot_5 <- data2plot4 %>% 
  mutate(Anio = case_when(var=="(Intercept)" ~ "2021",
                          var=="relevel(factor(Anio_arresto), \"2021\")2011" ~ "2011",
                          var=="relevel(factor(Anio_arresto), \"2021\")2012" ~ "2012",
                          var=="relevel(factor(Anio_arresto), \"2021\")2013" ~ "2013",
                          var=="relevel(factor(Anio_arresto), \"2021\")2014" ~ "2014",
                          var=="relevel(factor(Anio_arresto), \"2021\")2015" ~ "2015",
                          var=="relevel(factor(Anio_arresto), \"2021\")2016" ~ "2016",
                          var=="relevel(factor(Anio_arresto), \"2021\")2017" ~ "2017",
                          var=="relevel(factor(Anio_arresto), \"2021\")2018" ~ "2018",
                          var=="relevel(factor(Anio_arresto), \"2021\")2019" ~ "2019",
                          var=="relevel(factor(Anio_arresto), \"2021\")2020" ~ "2020")) %>%
  mutate(value2plot =  case_when(Anio!=2021 ~ 100*(base_values[4]+Estimate),
                                 Anio==2021 ~ 100*Estimate)) %>%
  filter(!is.na(Anio)) %>%
  mutate(value2plot = value2plot + Estimated_diff1,
         labels = paste0(round(value2plot,0), "%"),
         group_var =  "Estimated value")

data2plot <- bind_rows(data2plot,data2plot_5)

colors4plot <- c("#003B88", "#43a9a7", "#fa4d57")

plt <- ggplot(data2plot, 
              aes(x     = Anio,
                  y     = value2plot,
                  label = labels,
                  group = group_var,
                  color = group_var)) +
  geom_point(size = 2,
             show.legend = F) +
  geom_line(size  = 1,
            show.legend = F) +
  geom_text_repel(
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
  expand_limits(y = c(0, 100))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"))



ggsave(plot   = plt,
       file   = paste0("National/Exploration/Output/Grafica_prueba_hip_sentencia_2.svg"), 
       width  = 175, 
       height = 85,
       units  = "mm",
       dpi    = 72,
       device = "svg")
