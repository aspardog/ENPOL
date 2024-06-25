## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Política Criminal parte 3
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##                    Marcelo Torres            (mtorres@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     April 23th, 2024
##
## This version:      April 23th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline: Señalamientos                                                                                   ----
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


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Comportamiento policial y tortura                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

delitos_data <- Main_database      %>% 
  filter(Anio_arresto >= 2008,
         NSJP == 1,
         !is.na(tortura_tra)) %>% 
  select( starts_with("P3_14_"), tortura_tra ) %>% 
  mutate(total = sum(c_across(where(is.numeric)))) %>% 
  # filter(total >= 2) %>% 
  mutate(P3_14_1 = case_when(P3_14_1 == 0 ~ "No se identificó", 
                            P3_14_1 == 1 ~ "0",
                            T ~ NA_character_),
        P3_14_2 = case_when(P3_14_2 == 0 ~ "No mencionó corporación", 
                            P3_14_2 == 1 ~ "0",
                            T ~ NA_character_), 
        P3_14_3 = case_when(P3_14_3 == 0 ~ "No unifrmado", 
                            P3_14_3 == 1 ~ "0",
                            T ~ NA_character_), 
        P3_14_4 = case_when(P3_14_4 == 0 ~ "No dijo detuvieron", 
                            P3_14_4 == 1 ~ "0",
                            T ~ NA_character_), 
        P3_14_5 = case_when(P3_14_5 == 0 ~ "No informó derechos", 
                            P3_14_5 == 1 ~ "0",
                            T ~ NA_character_), 
        P3_14_6 = case_when(P3_14_6 == 0 ~ "No dijo llevaría", 
                            P3_14_6 == 1 ~ "0",
                            T ~ NA_character_)) %>% 
  rowwise() %>%
  mutate(concat = paste(c_across(where(is.character)), collapse = " ")) %>%
  mutate(concat = str_replace_all(concat, "0", ""),
         concat = str_replace_all(concat, " ", ""))
         
         
         
data2plot <- delitos_data %>%
  group_by(concat) %>%
  summarise(n = n()) %>%
  mutate(value2plot =  100 * n / sum(n),
         labels = paste0(round(value2plot,0),"%"),
         Delito = concat,
         Delito = str_wrap(Delito, width = 30)) %>%
  select(Delito,value2plot,labels, n) %>%
  arrange(value2plot) %>%
  mutate(Delito = factor(Delito, levels = Delito)) 

write_csv(data2plot, "")

colors4plot <- rep("#E2E2F7", 9)


plt <- ggplot(data2plot, 
              aes(x     = Delito,
                  y     = value2plot,
                  label = labels,
                  color = Delito)) +
  geom_bar(stat = "identity", fill = colors4plot, color = colors4plot,
           show.legend = F, width = 0.9) +
  scale_fill_manual(values = colors4plot) +
  geom_text(aes(y    = value2plot + 5 ),
            color    = "#4a4a49",
            family   = "Lato Full",
            fontface = "bold") +
  labs(y = "% of respondents") +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0,100,20),
                     labels = paste0(seq(0,100,20), "%"),
                     position = "right") +
  scale_x_discrete( ) +
  WJP_theme() +
  theme(legend.position="none",
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
                                 color = "Black", hjust = 0),
        legend.title = element_blank())+
  coord_flip()

plt