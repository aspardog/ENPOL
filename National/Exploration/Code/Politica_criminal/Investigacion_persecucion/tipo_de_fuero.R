## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration data & hipothesys
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres 
##
## Dependencies:      World Justice Project
##
## Creation date:     February 05th, 2024
##
## This version:      February 05th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Pre-settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/Settings.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

Main_database_2008 <- Main_database %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1)



# Funtions ----------------------------------------------------------------


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Data2Plot                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


count_frequency.fn <- function(column) {
  # Convert the column to a data frame
  data <- data.frame(Value = column) %>% 
    filter(complete.cases(.))
  
  # Count the frequency of each unique value
  frequency_df <- data %>%
    group_by(Value) %>%
    summarise(Frequency = n()) %>% 
    mutate(Value = Value,
           values = Frequency/sum(Frequency),
           value2plot = values * 100,
           figure = paste0(round(value2plot, 0), "%"),
           labels = str_wrap(Value, width = 25)) 
  frequency_df <- frequency_df %>% mutate(order_var = rank(Value))
  return(frequency_df)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. GRÁFICOS                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Tipo de fuero -----------------------------------------------------------


data2plot <- count_frequency.fn(Main_database_2008$fuero)


barChart <- BarSimpleChartViz(fill_colors = c("#3273ff","#003B88", "#fa4d57")) + expand_limits(y = c(0, 100))
barChart

# Tipo de fuero  y corporación de detención -----------------------------------------------------------


# Use group_by to group data by two columns
data2plot <- Main_database_2008 %>%
  select(Corporacion_grupos, fuero) %>% 
  group_by(Corporacion_grupos, fuero) %>%
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Corporacion_grupos) %>%  # Group again by the first column to calculate percentages within each group of column1
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
  ungroup() %>%  # Remove grouping
  mutate(figure = paste0(round(Percentage, 0), "%"),
         labels = str_wrap(paste(Corporacion_grupos, fuero, sep=" - "), width = 20),
         Corporacion_grupos = str_wrap(Corporacion_grupos, width = 20)) %>% # Create labels combining both columns
 filter(complete.cases(.),
        Corporacion_grupos != "NS/NR") %>% 
  mutate(order_var = case_when(Corporacion_grupos == "Ejército o Marina" ~ 1, 
                               Corporacion_grupos == "Guardia Nacional" ~ 2,
                               Corporacion_grupos == "Policía Federal" ~ 3, 
                               Corporacion_grupos == "Policía Federal\nMinisterial" ~ 4,
                               Corporacion_grupos == "Policía Estatal\nMinisterial o\nJudicial" ~ 8,                               Corporacion_grupos == "Operativo Conjunto" ~ 5,
                               Corporacion_grupos == "Policía Estatal" ~ 5, 
                               Corporacion_grupos == "Policía Municipal" ~ 7,
                               Corporacion_grupos == "Otra" ~ 6, 
                               T ~ NA_real_))
  
colors4plot <- c("Sólo común" = "#003B88", 
                 "Sólo federal" = "#fa4d57",
                 "Algunos delitos de fuero común y algunos de fuero federal" = "#3273ff")

plot <- ggplot(data2plot,
               aes(
                 x     = reorder(Corporacion_grupos,order_var), 
                 y     = Percentage,
                 fill  = fuero,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = Percentage + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "right") +
  coord_flip() +
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
    axis.title.x       = element_blank())

# Tipo de fuero  y tipo de corporación -----------------------------------------------------------


Main_database_2008 <-  Main_database_2008 %>% 
  mutate(corporacion_fuero = case_when(Corporacion_grupos == "Ejército o Marina" ~ "Corporación Federal", 
                               Corporacion_grupos == "Guardia Nacional" ~ "Corporación Federal",
                               Corporacion_grupos == "Policía Federal" ~ "Corporación Federal",
                               Corporacion_grupos == "Policía Federal Ministerial" ~ "Corporación Federal",
                               Corporacion_grupos == "Policía Estatal Ministerial o Judicial" ~ "Corporación Local",
                               Corporacion_grupos == "Operativo Conjunto" ~ "Operativo Conjunto",
                               Corporacion_grupos == "Policía Estatal" ~ "Corporación Local", 
                               Corporacion_grupos == "Policía Municipal" ~ "Corporación Local",
                               Corporacion_grupos == "Otra" ~ "Otra", 
                               T ~ NA_character_))

# Use group_by to group data by two columns
data2plot <- Main_database_2008 %>%
  select(corporacion_fuero, fuero) %>% 
  group_by(corporacion_fuero, fuero) %>%
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(corporacion_fuero) %>%  # Group again by the first column to calculate percentages within each group of column1
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
  ungroup() %>%  # Remove grouping
  mutate(figure = paste0(round(Percentage, 0), "%"),
         labels = str_wrap(paste(corporacion_fuero, fuero, sep=" - "), width = 20),
         corporacion_fuero = str_wrap(corporacion_fuero, width = 20)) %>% # Create labels combining both columns
  filter(complete.cases(.),
         corporacion_fuero != "NS/NR") %>% 
  mutate(order_var = case_when(corporacion_fuero == "Otra" ~ 1, 
                               corporacion_fuero == "Operativo Conjunto" ~ 2, 
                               corporacion_fuero == "Corporación Local" ~ 3,
                               corporacion_fuero == "Corporación Federal" ~ 4,
                               T ~ NA_real_))

colors4plot <- c("Sólo común" = "#003B88", 
                 "Sólo federal" = "#fa4d57",
                 "Algunos delitos de fuero común y algunos de fuero federal" = "#3273ff")

plot <- ggplot(data2plot,
               aes(
                 x     = reorder(corporacion_fuero, order_var), 
                 y     = Percentage,
                 fill  = fuero,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = Percentage + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  # geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 105),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "right") +
  coord_flip() +
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
    axis.title.x       = element_blank())

# Tipo de fuero  y delito -----------------------------------------------------------

Main_database_2008 <- Main_database_2008 %>% 
              Mutate




# Use group_by to group data by two columns
data2plot <- Main_database_2008 %>%
  select(Delito_unico_categ, fuero) %>% 
  group_by(Delito_unico_categ, fuero) %>%
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Delito_unico_categ) %>%  # Group again by the first column to calculate percentages within each group of column1
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
  ungroup() %>%  # Remove grouping
  mutate(figure = paste0(round(Percentage, 0), "%"),
         labels = str_wrap(paste(Delito_unico_categ, fuero, sep=" - "), width = 20),
         Delito_unico_categ = str_wrap(Delito_unico_categ, width = 20)) %>% # Create labels combining both columns
  filter(complete.cases(.),
         Delito_unico_categ != "ns_nr")

colors4plot <- c("Sólo común" = "#003B88", 
                 "Sólo federal" = "#fa4d57",
                 "Algunos delitos de fuero común y algunos de fuero federal" = "#3273ff")

plot <- ggplot(data2plot,
               aes(
                 x     = Delito_unico_categ, 
                 y     = Percentage,
                 fill  = fuero,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = Percentage + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  # geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "right") +
  coord_flip() +
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
    axis.title.x       = element_blank())

# Tipo de fuero  e interrogatroio -----------------------------------------------------------

Main_database_2008 <- Main_database_2008 %>% 
                      mutate(aut_interrogaron = case_when(P4_3C_01 == "1" ~ "Policía Estatal Ministerial o Judicial",
                                                          P4_3C_02 == "1" ~ "El Agente del Ministerio Público o Fiscalía",
                                                          P4_3C_03 == "1" ~ "Policía Federal Ministerial (PGR o FGR)",
                                                          P4_3C_04 == "1" ~ "Policía Municipal",
                                                          P4_3C_05 == "1" ~ "Policía Estatal",
                                                          P4_3C_06 == "1" ~ "Policía Federal",
                                                          P4_3C_07 == "1" ~ "Guardia Nacional",
                                                          P4_3C_08 == "1" ~ "Ejército",
                                                          P4_3C_09 == "1" ~ "Marina",
                                                          P4_3C_10 == "1" ~ "Personal ministerial",
                                                          P4_3C_11 == "1" ~ "Otro",
                                                          T ~NA_character_))


Main_database_2008 <- Main_database_2008 %>%
                      rowwise() %>%
                      mutate(aut_inter_unico = 
                               ifelse(sum(as.numeric(c(P4_3C_01,
                                            P4_3C_02,
                                            P4_3C_03,
                                            P4_3C_04,
                                            P4_3C_05,
                                            P4_3C_06,
                                            P4_3C_07,
                                            P4_3C_08,
                                            P4_3C_09,
                                            P4_3C_10,
                                            P4_3C_11))) == 1, 1, 0),
                             aut_interrogaron = case_when(P4_3C_01 == "1" &  aut_inter_unico == 1 ~ "Policía Estatal Ministerial o Judicial",
                                                          P4_3C_02 == "1" &  aut_inter_unico == 1 ~ "El Agente del Ministerio Público o Fiscalía",
                                                          P4_3C_03 == "1" &  aut_inter_unico == 1 ~ "Policía Federal Ministerial (PGR o FGR)",
                                                          P4_3C_04 == "1" &  aut_inter_unico == 1 ~ "Policía Municipal",
                                                          P4_3C_05 == "1" &  aut_inter_unico == 1 ~ "Policía Estatal",
                                                          P4_3C_06 == "1" &  aut_inter_unico == 1 ~ "Policía Federal",
                                                          P4_3C_07 == "1" &  aut_inter_unico == 1 ~ "Guardia Nacional",
                                                          P4_3C_08 == "1" &  aut_inter_unico == 1 ~ "Ejército",
                                                          P4_3C_09 == "1" &  aut_inter_unico == 1 ~ "Marina",
                                                          P4_3C_10 == "1" &  aut_inter_unico == 1 ~ "Personal ministerial",
                                                          P4_3C_11 == "1" &  aut_inter_unico == 1 ~ "Otro",
                                                          aut_inter_unico ==  0 ~ "Diversos",
                                                          T ~ NA_character_))

# Use group_by to group data by two columns
data2plot <- Main_database_2008 %>%
  select(aut_interrogaron, fuero) %>% 
  group_by(aut_interrogaron, fuero) %>%
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(aut_interrogaron) %>%  # Group again by the first column to calculate percentages within each group of column1
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
  ungroup() %>%  # Remove grouping
  mutate(figure = paste0(round(Percentage, 0), "%"),
         labels = str_wrap(paste(aut_interrogaron, fuero, sep=" - "), width = 20),
         aut_interrogaron = str_wrap(aut_interrogaron, width = 20)) %>% # Create labels combining both columns
  filter(complete.cases(.),
         aut_interrogaron != "ns_nr")

colors4plot <- c("Sólo común" = "#003B88", 
                 "Sólo federal" = "#fa4d57",
                 "Algunos delitos de fuero común y algunos de fuero federal" = "#3273ff")

plot <- ggplot(data2plot,
               aes(
                 x     = aut_interrogaron, 
                 y     = Percentage,
                 fill  = fuero,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = Percentage + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  # geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "right") +
  coord_flip() +
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
    axis.title.x       = element_blank())

# Tipo de fuero  e interrogatroio  agrupado -----------------------------------------------------------




Main_database_2008 <- Main_database_2008 %>%
  mutate(aut_interrogaron_fuero = case_when(aut_interrogaron ==  "Policía Estatal Ministerial o Judicial" ~ "Corporación Local",
                                            aut_interrogaron == "El Agente del Ministerio Público o Fiscalía"  ~ "Corporación Local",
                                            aut_interrogaron == "Policía Federal Ministerial (PGR o FGR)" ~ "Corporación Federal",
                                            aut_interrogaron == "Policía Municipal"  ~ "Corporación Local",
                                            aut_interrogaron == "Policía Estatal" ~ "Corporación Local",
                                            aut_interrogaron == "Policía Federal" ~ "Corporación Federal",
                                            aut_interrogaron == "Guardia Nacional" ~ "Corporación Federal",
                                            aut_interrogaron == "Ejército" ~ "Corporación Federal",
                                            aut_interrogaron == "Marina" ~ "Corporación Federal",
                                            aut_interrogaron == "Personal ministerial"  ~ "Corporación Local",
                                            aut_interrogaron == "Otro" ~ "Otro",
                                            aut_interrogaron ==  "Diversos" ~ "Otro",
                                      T ~ NA_character_))

# Use group_by to group data by two columns
data2plot <- Main_database_2008 %>%
  select(aut_interrogaron_fuero, fuero) %>% 
  group_by(aut_interrogaron_fuero, fuero) %>%
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(aut_interrogaron_fuero) %>%  # Group again by the first column to calculate percentages within each group of column1
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>%
  ungroup() %>%  # Remove grouping
  mutate(figure = paste0(round(Percentage, 0), "%"),
         labels = str_wrap(paste(aut_interrogaron_fuero, fuero, sep=" - "), width = 20),
         aut_interrogaron_fuero = str_wrap(aut_interrogaron_fuero, width = 20)) %>% # Create labels combining both columns
  filter(complete.cases(.),
         aut_interrogaron_fuero != "ns_nr")

colors4plot <- c("Sólo común" = "#003B88", 
                 "Sólo federal" = "#fa4d57",
                 "Algunos delitos de fuero común y algunos de fuero federal" = "#3273ff")

plot <- ggplot(data2plot,
               aes(
                 x     = aut_interrogaron_fuero, 
                 y     = Percentage,
                 fill  = fuero,
                 label = figure
               )) +
  geom_bar(stat = "identity",
           show.legend = FALSE, width = 0.9, position = "dodge")+
  geom_text(aes(y    = Percentage + 10), 
            position = position_dodge(widt = 0.9),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold", 
            size = 3.514598)  +
  # geom_vline(xintercept = 4.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colors4plot) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "right") +
  coord_flip() +
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
    axis.title.x       = element_blank())