## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("Code/settings.R")

# Loading data
load(paste0(path2SP,
            "/National/Data_cleaning/Output/Main_database.RData"))



# Indígenas en prisión preventiva -----------------------------------------

data.df <- Main_database %>% 
            filter(Anio_arresto >= 2000) %>% 
            mutate(indigena = case_when(P1_15 == 2 ~ "indígena",
                                        P1_15 == 1 ~ "no_indígena",
                                        P1_15 == 3 ~ "no_indígena",
                                        T ~ NA_character_),
                   prision_preventiva = case_when(P5_9 == "1" ~ "prisión_prev",
                                                  P5_9 == "2" ~ "libertad",
                                                  T ~ NA_character_))

table(data.df$indigena)               
table(data.df$indigena, data.df$Sexo)
table(data.df$indigena, data.df$sentenciado)
table(data.df$indigena, data.df$prision_preventiva)
table(data.df$indigena, data.df$P4_1_14)
table(data.df$indigena, data.df$P4_1_15)
table(data.df$indigena, data.df$P5_11_12)


data_ind.df <- data.df %>% 
  filter(indigena == "indígena" ) 
table(data_ind.df$Sexo, data_ind.df$sentenciado)
table(data_ind.df$Sexo, data_ind.df$prision_preventiva)
table(data_ind.df$P4_1_14)
table(data_ind.df$Delito_gr_1_robos)


data_ind.df <- data.df %>% 
  filter(indigena == "indígena",
         P5_11_12 == 1) %>% 
  mutate(P5_4_A = as.numeric(P5_4_A),
         P5_4_M = as.numeric(P5_4_M),
         P5_4_A = case_when(
           P5_4_A >= 97 ~ NA_real_,
           T ~ P5_4_A),
         P5_4_M = case_when(
           P5_4_M >= 97 ~ NA_real_,
           T ~ P5_4_M),
         P5_4_M = P5_4_M/12,
         Años = P5_4_A+P5_4_M)

mean(data_ind.df$Años, na.rm = TRUE)


vars <- c(
  "Robo de casa habitación",
  "Robo de vehículo",
  "Robo a negocio",
  "Robo en transporte público",
  "Robo a transeúnte en vía pública",
  "Robo de autopartes",
  "Otras formas de robo",
  "Posesión ilegal de drogas",
  "Comercio ilegal de drogas",
  "Lesiones",
  "Homicidio culposo",
  "Homicidio doloso",
  "Portación ilegal de armas",
  "Incumplimiento de obligaciones de asistencia familiar",
  "Violencia familiar",
  "Daño a la propiedad",
  "Secuestro o secuestro exprés",
  "Violación sexual",
  "Fraude",
  "Delincuencia organizada",
  "Otros delitos sexuales",
  "Extorsión",
  "Privación de la libertad",
  "Abuso de confianza",
  "Amenazas",
  "Otro"
)

Main_database1 <- data_NOind.df  %>%
  filter(
         sentenciado == 1
  ) %>%
  mutate(
    `Robo de vehículo`= 
      case_when(
        P5_11_01 == 1 ~ 1
      ),
    `Robo de casa habitación`  = 
      case_when(
        P5_11_02 == 1 ~ 1
      ),
    `Robo a negocio` =
      case_when(
        P5_11_03 == 1 ~ 1
      ),
    `Robo en transporte público` =
      case_when(
        P5_11_04 == 1 ~ 1
      ),
    `Robo a transeúnte en vía pública` =
      case_when(
        P5_11_05 == 1 ~ 1
      ),
    `Robo de autopartes` =
      case_when(
        P5_11_06 == 1 ~ 1
      ),
    `Otras formas de robo` =
      case_when(
        P5_11_07 == 1 ~ 1
      ),
    `Posesión ilegal de drogas` = 
      case_when(
        P5_11_08 == 1 ~ 1
      ),
    `Comercio ilegal de drogas` =
      case_when(
        P5_11_09 == 1 ~ 1
      ),
    `Lesiones` =
      case_when(
        P5_11_10 == 1 ~ 1
      ),
    `Homicidio culposo` =
      case_when(
        P5_11_11 == 1 ~ 1
      ),
    `Homicidio doloso` =
      case_when(
        P5_11_12 == 1 ~ 1
      ),
    `Portación ilegal de armas` =
      case_when(
        P5_11_13 == 1 ~ 1
      ),
    `Incumplimiento de obligaciones de asistencia familiar` =
      case_when(
        P5_11_14 == 1 ~ 1
      ),
    `Violencia familiar` =
      case_when(
        P5_11_15 == 1 ~ 1
      ),
    `Daño a la propiedad` =
      case_when(
        P5_11_16 == 1 ~ 1
      ),
    `Secuestro o secuestro exprés` =
      case_when(
        P5_11_17 == 1 ~ 1
      ),
    `Violación sexual` = 
      case_when(
        P5_11_18 == 1 ~ 1
      ),
    `Fraude` = 
      case_when(
        P5_11_19 == 1 ~ 1
      ),
    `Delincuencia organizada` =
      case_when(
        P5_11_20 == 1 ~ 1
      ),
    `Otros delitos sexuales` =
      case_when(
        P5_11_21 == 1 ~ 1
      ),
    `Extorsión` =
      case_when(
        P5_11_22 == 1 ~ 1
      ),
    `Privación de la libertad` =
      case_when(
        P5_11_23 == 1 ~ 1
      ),
    `Abuso de confianza` =
      case_when(
        P5_11_24 == 1 ~ 1
      ),
    `Amenazas` =
      case_when(
        P5_11_25 == 1 ~ 1
      ),
    `Otro` =
      case_when(
        P5_11_26 == 1 ~ 1
      )
  ) %>%
  pivot_longer(
    cols = c(all_of(vars)), 
    names_to = "Delito_unico_ungrouped_categ", 
    values_to = "value2filter"
  ) %>%
  filter(
    value2filter == 1
  )

data2plot <- Main_database1 %>%
  group_by(Delito_unico_ungrouped_categ) %>%
  summarise(n = n()) %>%
  drop_na() %>%
  mutate(value2plot =  100 * (n / sum(n)),
         labels = paste0(round(value2plot,0),"%"),
         group_var = "Arrestos",
         Delito = Delito_unico_ungrouped_categ,
         Delito = str_wrap(Delito, width = 30)) %>%
  select(Delito,value2plot,labels,group_var, n) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito)) 

colors4plot <- rep(mainCOLOR, 26)


plt <- ggplot(data2plot, 
              aes(x     = Delito,
                  y     = value2plot,
                  label = paste0(labels,", N = ",n),
                  group = Delito,
                  color = Delito,
                  fill  = Delito)) +  # Añadido fill para asegurar coincidencia
  geom_bar(stat = "identity", show.legend = F, width = 0.9, color = "white") +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 2 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 30),
                     breaks = seq(0, 100, 10),
                     labels = paste0(seq(0, 100, 10), "%"),
                     position = "right") +
  scale_x_discrete() +
  expand_limits(y = c(0, 30)) +
  WJP_theme() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#D0D1D3"),
        axis.title.y       = element_blank(),
        axis.title.x       = element_blank(),
        axis.text.y=element_text(family   = "Lato Full",
                                 face     = "bold",
                                 size     = 3.514598*.pt,
                                 color    = "#524F4C",
                                 margin   = margin(0, 10, 0, 0),
                                 hjust = 0)) +
  coord_flip(); plt

data_NOind.df <- data.df %>% 
  filter(indigena == "no_indígena" ) 
table(data_NOind.df$Sexo, data_NOind.df$sentenciado)
table(data_NOind.df$Sexo, data_NOind.df$prision_preventiva)
table(data.df$indigena, data.df$prision_preventiva , data.df$sentenciado)

data_NOind.df <- data.df %>% 
  filter(indigena == "no_indígena",
         P5_11_12 == 1) %>% 
  mutate(P5_4_A = as.numeric(P5_4_A),
         P5_4_M = as.numeric(P5_4_M),
         P5_4_A = case_when(
           P5_4_A >= 97 ~ NA_real_,
           T ~ P5_4_A),
         P5_4_M = case_when(
           P5_4_M >= 97 ~ NA_real_,
           T ~ P5_4_M),
         P5_4_M = P5_4_M/12,
         Años = P5_4_A+P5_4_M)

mean(data_NOind.df$Años, na.rm = TRUE)

