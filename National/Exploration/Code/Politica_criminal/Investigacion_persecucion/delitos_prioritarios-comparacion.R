


Main_database1 <- data.df  %>%
  mutate(Delito_prioritario_ENVIPE = case_when(Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ 1,
                                               Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ 1,
                                               Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ 1,
                                               Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ 1,
                                               Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ 1,
                                               Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ 1,
                                               T ~ 0),
         Delito_unico_ungrouped_categ = case_when(Delito_unico == 1 & (P5_11_01 == 1|P5_31_01 == 1) ~ "Robo de casa habitación",
                                                  Delito_unico == 1 & (P5_11_02 == 1|P5_31_02 == 1) ~ "Robo de vehículo",
                                                  Delito_unico == 1 & (P5_11_03 == 1|P5_31_03 == 1) ~ "Robo a negocio",
                                                  Delito_unico == 1 & (P5_11_04 == 1|P5_31_04 == 1) ~ "Robo en transporte público",
                                                  Delito_unico == 1 & (P5_11_05 == 1|P5_31_05 == 1) ~ "Robo a transeunte en vía pública",
                                                  Delito_unico == 1 & (P5_11_06 == 1|P5_31_06 == 1) ~ "Robo de autopartes",
                                                  Delito_unico == 1 & (P5_11_07 == 1|P5_31_07 == 1) ~ "Robo en forma distinta a las anteriores",
                                                  Delito_unico == 1 & (P5_11_08 == 1|P5_31_08 == 1) ~ "Posesión ilegal de drogas",
                                                  Delito_unico == 1 & (P5_11_09 == 1|P5_31_09 == 1) ~ "Comercio ilegal de drogas",
                                                  Delito_unico == 1 & (P5_11_10 == 1|P5_31_10 == 1) ~ "Lesiones",
                                                  Delito_unico == 1 & (P5_11_11 == 1|P5_31_11 == 1) ~ "Homicidio culposo",
                                                  Delito_unico == 1 & (P5_11_12 == 1|P5_31_12 == 1) ~ "Homicidio doloso",
                                                  Delito_unico == 1 & (P5_11_13 == 1|P5_31_13 == 1) ~ "Portación ilegal de armas",
                                                  Delito_unico == 1 & (P5_11_14 == 1|P5_31_14 == 1) ~ "Incumplimiento de obligaciones de asistencia familiar",
                                                  Delito_unico == 1 & (P5_11_15 == 1|P5_31_15 == 1) ~ "Violencia familiar",
                                                  Delito_unico == 1 & (P5_11_16 == 1|P5_31_16 == 1) ~ "Daño a la propiedad",
                                                  Delito_unico == 1 & (P5_11_17 == 1|P5_31_17 == 1) ~ "Secuestro o secuestro expres",
                                                  Delito_unico == 1 & (P5_11_18 == 1|P5_31_18 == 1) ~ "Violación sexual",
                                                  Delito_unico == 1 & (P5_11_19 == 1|P5_31_19 == 1) ~ "Fraude",
                                                  Delito_unico == 1 & (P5_11_20 == 1|P5_31_20 == 1) ~ "Delincuencia organizada",
                                                  Delito_unico == 1 & (P5_11_21 == 1|P5_31_21 == 1) ~ "Otros delitos sexuales",
                                                  Delito_unico == 1 & (P5_11_22 == 1|P5_31_22 == 1) ~ "Extorsión",
                                                  Delito_unico == 1 & (P5_11_23 == 1|P5_31_23 == 1) ~ "Privación de la libertad",
                                                  Delito_unico == 1 & (P5_11_24 == 1|P5_31_24 == 1) ~ "Abuso de confianza",
                                                  Delito_unico == 1 & (P5_11_25 == 1|P5_31_25 == 1) ~ "Amenazas",
                                                  Delito_unico == 1 & (P5_11_26 == 1|P5_31_26 == 1) ~ "Otro",
                                                  T ~ NA_character_)) 

data2plot_ENPOL <- Main_database1 %>%
  filter(Anio_arresto >= 2018, !is.na(Delito_unico_ungrouped_categ), sentenciado == 1) %>%
  group_by(Delito_unico_ungrouped_categ) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "Arrestos",
         Delito = Delito_unico_ungrouped_categ,
         # Delito = str_wrap(Delito, width = 30)
         ) %>%
  select(Delito,value2plot,labels,group_var, n) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito),
         group_var = "ENPOL")


ENVIPE <- read_dta(paste0(path2SP,"/National/Exploration/Input/Politica_criminal/ocurrencias.dta")) %>%
  pivot_longer(cols = del_1:del_15, names_to = "Delito_envipe",values_to = "Ocurrencias")  %>%
  group_by(anio, Delito_envipe) %>%
  summarise(Ocurrencias = sum(Ocurrencias, na.rm = T)) %>%
  mutate(Delito_envipe = case_when(Delito_envipe == "del_1" ~ "Robo de vehículo", 
                                   Delito_envipe == "del_2" ~ "Robo de autopartes", 
                                   Delito_envipe == "del_3" ~ "Pinta de barda u otro\n tipo de vandalismo", 
                                   Delito_envipe == "del_4" ~ "Entraron sin permiso y robó\n o intentó robar algo", 
                                   Delito_envipe == "del_5" ~ "Robo en la calle o en el\n transporte público", 
                                   Delito_envipe == "del_6" ~ "Otro tipo de robo", 
                                   Delito_envipe == "del_7" ~ "Fraude bancario", 
                                   Delito_envipe == "del_8" ~ "Fraude al consumidor", 
                                   Delito_envipe == "del_9" ~ "Extorsión", 
                                   Delito_envipe == "del_10" ~ "Amenazas", 
                                   Delito_envipe == "del_11" ~ "Lesiones", 
                                   Delito_envipe == "del_12" ~ "Secuestro", 
                                   Delito_envipe == "del_13" ~ "Otros delitos sexuales", 
                                   Delito_envipe == "del_14" ~ "Violación sexual ", 
                                   Delito_envipe == "del_15" ~ "Otro"))


data2plot_ENVIPE <- ENVIPE %>%
  filter(anio >= 2018) %>%
  group_by(Delito_envipe) %>%
  summarise(n = sum(Ocurrencias)) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "ENVIPE",
         Delito = Delito_envipe,
         Delito = str_wrap(Delito, width = 30)) %>%
  select(Delito,value2plot,labels,group_var, n) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito))


data2plot <- bind_rows(data2plot_ENVIPE, data2plot_ENPOL)

data2plot <- data2plot %>% 
              mutate(category = case_when(Delito == "Entraron sin permiso y robó o intentó a robar algo" ~  "Robo a casa habitación",
                                          Delito == "Robo a casa habitación"                             ~  "Robo a casa habitación",
                                          Delito == "Pinta de barda u otro tipo de\nvandalismo" ~  "Daño a la propiedad",
                                          Delito == "Daño a la propiedad"                      ~  "Daño a la propiedad",
                                          Delito == "Robo en la calle o en el\ntransporte público" ~  "Robo en la calle o en el transporte público",
                                          Delito == "Robo a transeunte en vía pública"            ~  "Robo en la calle o en el transporte público",
                                          Delito == "Robo en transporte público"                  ~  "Robo en la calle o en el transporte público",
                                          Delito == "Extorsión"                                   ~  "Extorsión",
                                          Delito == "Fraude bancario"                            ~  "Fraude",
                                          Delito == "Fraude al consumidor"                      ~  "Fraude",
                                          Delito == "Fraude"                                    ~  "Fraude",
                                          Delito == "Otro tipo de robo"                            ~  "Otro tipo de robo",
                                          Delito == "Robo en forma distinta a las anteriores"      ~  "Otro tipo de robo",
                                          Delito == "Robo de vehículo"                             ~  "Robo de vehículo o autopartes",
                                          Delito == "Robo de autopartes"                             ~  "Robo de vehículo o autopartes",
                                          Delito == "Lesiones"                                     ~  "Lesiones",
                                          Delito == "Violación sexual"                             ~  "Violación sexual",
                                          Delito == "Secuestro"                                    ~  "Secuestro",
                                          Delito == "Secuestro o secuestro expres"                 ~  "Secuestro",
                                          Delito == "Privación de la libertad"                 ~  "Secuestro",
                                          Delito == "Otro"                                         ~  "Otro",
                                          Delito == "Amenazas"                                    ~  "Amenazas")) %>% 
        rename(figure = labels,
               labels = group_var) %>% 
        drop_na() %>% 
        select(category, labels, value2plot) %>% 
        group_by(category, labels) %>% 
        summarise(value2plot = sum(value2plot)) %>%
        mutate(figure = paste0(round(value2plot,0),"%"))  %>% 
        mutate(order_value = case_when(
          category == "Robo de autopartes"                          ~  1,
          category == "Daño a la propiedad"                         ~  2,
          category == "Robo en la calle o en el transporte público" ~  3,
          category == "Extorsión"                                   ~  4,
          category == "Fraude"                                       ~  5,
          category == "Otro tipo de robo"                            ~  8,
          category == "Robo de vehículo o autopartes"                 ~  0,
          category == "Lesiones"                                     ~  10,
          category == "Violación sexual"                             ~  11,
          category == "Secuestro"                                    ~  12,
          category == "Otro"                                         ~  13,
          category == "Amenazas"                                    ~  7))


colors4plot <- c("#2a2a9A", "#a90099")

names(colors4plot) <- c("ENPOL",
                        "ENVIPE")

plot <- ggplot(data2plot,
               aes(
                 x     = reorder(category, -order_value), 
                 y     = value2plot,
                 fill  = labels,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = value2plot + 1), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  coord_flip()+
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = seq(0,20,10),
                     labels = paste0(seq(0,20,10), "%"),
                     position = "right") +
  theme(
    panel.background   = element_blank(),
    plot.background    = element_blank(),
    panel.grid.major   = element_line(size     = 0.25,
                                      colour   = "#5e5c5a",
                                      linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3"),       
    axis.title.y       = element_blank(),
    axis.title.x       = element_blank(),
    axis.text.y=element_text(family   = "Lato Full",
                             face     = "bold",
                             size     = 3.514598*.pt,
                             color    = "#524F4C",
                             margin   = margin(0, 10, 0, 0),
                             hjust = 0));plot


library(ggbump)
library(tidyverse)



data2plot <- data2plot %>% 
            mutate(labels = case_when(labels == "ENPOL" ~ 2019,
                                      labels == "ENVIPE" ~ 2020),
                   value2plot = case_when(category == "Violación sexual" & labels == 2020 ~ 0.43, 
                                          category == "Fraude" & labels == 2020 ~ 11.5, 
                                          T ~ value2plot))


library(tidyverse)
library(countrycode)
library(rdbnomics)
library(ggbump)
library(MetBrewer)
library(scales)

countryPalette <- c("#2a2a94", "#a90099", "#3273ff", "#fa4d57", "#9d61f2", "#43a9a7", "#efa700", "#2c6d4f","#37C5CB", "#e2a4ff" )
selected <-  c("Robo de autopartes" ,"Daño a la propiedad", "Robo en la calle o en el transporte público" ,"Extorsión", "Fraude",
               "Robo de vehículo", "Lesiones", "Violación sexual", "Secuestro", "Amenazas", "Otro tipo de robo" ) 

plot <- data2plot %>% 
  ggplot(aes(x = labels, y = value2plot, group = category)) +
  geom_bump(linewidth = 0.6, color = "gray90", smooth = 6) +
  geom_bump(aes(color = category), linewidth = 0.9, smooth = 6,
            data = ~. |> filter(category %in% selected)) +
  geom_point(color = "white", size = 4) +
  geom_point(color = "gray90", size = 2) +
  geom_point(aes(color = category), size = 2, 
             data = ~. |> filter(category %in% selected)) +
  geom_text(aes(label = category, y = value2plot - .1 ), x = 2020.05, hjust = 0,
            color = "gray50", family = "Lato Full", size = 3,
            data = data2plot %>% ungroup() %>% slice_max(order_by = labels, by = category) %>%
              filter(!category %in% selected)) +
  geom_text(aes(label = category, y = value2plot + .2  ), x = 2020.05, hjust = 0,
            color = "black", family = "Lato Full", size = 3,
            data = data2plot %>% ungroup() %>% slice_max(order_by = labels, by = category) %>%
              filter(category %in% selected)) +
  scale_color_manual(values = countryPalette) +
  scale_x_continuous(limits = c(2018.9, 2020.5), expand = c(0.01,0),
                     breaks = c(2019, 2020), labels = c("ENPOL", "ENVIPE")) +
  scale_y_continuous(breaks = seq(from = 22, to = -1, by = -2), expand = c(0.02,0),
                  labels = number_format(suffix = "%")) +
  theme_minimal(base_family = "Lato Full", base_size = 12) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title.position = "plot", 
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.x       = element_text(color = "black")); plot
plot
