# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL 
##
## Script:            Exploration descriptives
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvares         (calvarez@worldjuticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Mayo 20th, 2024
##
## This version:      MYO  20th, 2024
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




# Porcentaje de delitos federales por estado ------------------------------

Estados <- Main_database_2008 %>% 
  select(Estado_arresto, fuero) %>% 
  group_by(Estado_arresto, fuero) %>%
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Estado_arresto) %>%
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>% 
  filter (fuero  == "Sólo federal") %>% 
  drop_na() %>%
  select(Estado_arresto, Percentage) %>%
  rename(ESTADO = Estado_arresto) %>%
  mutate(
    ESTADO = 
      case_when(
        ESTADO == "Coahuila de Zaragoza" ~ "Coahuila",
        ESTADO == "Michoacán de Ocampo"  ~ "Michoacán",
        ESTADO == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
        ESTADO == "México" ~ "Estado de México",
        T ~ ESTADO
      ))

quintiles <- quantile(round(Estados$Percentage, 0), probs = seq(0, 1, by = 0.2))


mapa <- st_read(paste0(path2SP,"/National/Exploration/Input/shp/México_Estados.shp")) %>%
  mutate( ESTADO = 
            case_when(
              ESTADO == "México" ~ "Estado de México",
              T ~ ESTADO
            )
  )

table <- Estados %>%
  mutate(
    ` ` = "",
    `%` = round(Percentage, 0)
  ) %>%
  arrange(ESTADO) %>%
  select(
    Estado = ESTADO, ` `, `%`
  ) %>%
  flextable() %>%
  theme_zebra(
    odd_header = "transparent",
    odd_body   = "#e2e0df"
  ) %>%
  
  padding(j = 1, padding.right = 30) %>%
  padding(j = 1, padding.left  = 10) %>%
  padding(j = 3, padding.left  = 10) %>%
  
  width(j = " ", width = 0.5, unit = "mm") %>%
  width(j = "%", width = 0.75,   unit = "mm") %>%
  
  # bg(i = ~ `%` <  60, j = ' ', bg = "#E03849", part = "body") %>%
  # bg(i = ~ `%` >= 65, j = ' ', bg = "#FF7900", part = "body") %>%
  # bg(i = ~ `%` >= 70, j = ' ', bg = "#FFC818", part = "body") %>%
  # bg(i = ~ `%` >= 75, j = ' ', bg = "#46B5FF", part = "body") %>%
  # bg(i = ~ `%` >= 80, j = ' ', bg = "#0C75B6", part = "body") %>%
  # bg(i = ~ `%` >= 85, j = ' ', bg = "#18538E", part = "body") %>%
  # 

  bg(i = ~ `%` <= quintiles[1], j = ' ', bg = "#ffd7f5", part = "body") %>%
  bg(i = ~ `%` > quintiles[1] & `%` <= quintiles[2], j = ' ', bg = "#e2a4ff", part = "body") %>%
  bg(i = ~ `%` > quintiles[2] & `%` <= quintiles[3], j = ' ', bg = "#9c94ff", part = "body") %>%
  bg(i = ~ `%` > quintiles[3] & `%` <= quintiles[4], j = ' ', bg = "#4e43dd", part = "body") %>%
  bg(i = ~ `%` > quintiles[4] & `%` <= quintiles[5], j = ' ', bg = "#2a2a9A", part = "body") %>%
  bg(i = ~ `%` > quintiles[5] & `%` <= quintiles[6], j = ' ', bg = "#20204a", part = "body") %>%  
  
    
  align(j     = 2, 
        align = "center", 
        part  = "all") %>%
  bold(bold = FALSE, 
       part = "header") %>%
  flextable::style(pr_t = fp_text(font.size   = 12, 
                                  color       = "#524F4C",
                                  font.family = "Lato Full"), 
                   part = "header") %>%
  flextable::style(pr_t = fp_text(font.size   = 10, 
                                  color       = "#524F4C",
                                  font.family = "Lato Full"), 
                   part = "body") %>%
  italic(italic = TRUE, 
         part = "header") %>%
  surround(j = 2,
           border.top    = fp_border("white"),
           border.bottom = fp_border("white"),
           part = "body"
  )

tpanel <- gen_grob(table, 
                   fit      = "fixed",
                   scaling  = "fixed", 
                   just     = c("left", "top"),
                   wrapping = T)

mexico_map <- mapa %>%
  left_join(Estados, by = "ESTADO") %>%
  mutate(value2plot = round(Percentage), 0) %>%
  mutate(
    color_group = case_when(
      value2plot <= quintiles[1] ~ "0%-5.1%",
      value2plot > quintiles[1] & value2plot <= quintiles[2] ~ "5.1%-9.8%",
      value2plot > quintiles[2] & value2plot <= quintiles[3] ~ "9.8%-12.6%",
      value2plot > quintiles[3] & value2plot <= quintiles[4] ~ "12.6%-18.2%",
      value2plot > quintiles[4] & value2plot <= quintiles[5] ~ "18.2%-23.3%",
      value2plot > quintiles[5] & value2plot <= quintiles[6] ~ "23.3%-36.8%"
    ),
    color_group = as.factor(color_group)
  )

cat_palette <- c("0%-5.1%"   = "#ffd7f5",
                 "5.1%-9.8%"  = "#e2a4ff",
                 "9.8%-12.6%"  = "#9c94ff",
                 "12.6%-18.2%"  = "#4e43dd",
                 "18.2%-23.3%"  = "#2a2a9A",
                 "23.3%-36.8%" = "#20204a")
# Drawing plot
p <- ggplot(mexico_map, aes(label = ESTADO)) +
  geom_sf(data  = mexico_map,
          aes(fill = color_group),
          color = "grey65",
          size  = 0.5) +
  geom_sf(data  = mexico_map,
          fill  = NA,
          color = "grey25") +
  scale_fill_manual("",
                    values   = cat_palette,
                    na.value = "grey95",
                    drop = F) +
  # scale_y_continuous(limits = c(1445631, 5273487)) +
  # scale_x_continuous(limits = c(2581570, 5967160)) +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    axis.text       = element_blank(),
    legend.position = "none",
    panel.grid      = element_blank(),
    panel.border    = element_blank(),
    plot.margin     = margin(0,0,0,0)
  ) 

categories <- c("0%-5%",
                "5%-10%",
                "10%-13%",
                "13%-18%",
                "18%-23%",
                "23%-37%")

leyend <- data.frame(
  Values = categories,
  Blank = "")
leyend <- flextable(leyend)  %>% 
  width(j = "Blank", width = 0.5, unit = "mm") %>% 
  set_header_labels(Values = "Escala", Blank = " ") %>% 
  bg(i = ~ Values == "0%-5%", j = "Blank", bg = "#ffd7f5", part = "body") %>%
  bg(i = ~ Values == "5%-10%", j = "Blank", bg = "#e2a4ff", part = "body") %>%
  bg(i = ~ Values == "10%-13%", j = "Blank", bg = "#9c94ff", part = "body") %>%
  bg(i = ~ Values == "13%-18%", j = "Blank", bg = "#4e43dd", part = "body") %>%
  bg(i = ~ Values == "18%-23%", j = "Blank", bg = "#2a2a9A", part = "body") %>%
  bg(i = ~ Values == "23%-37%", j = "Blank", bg = "#20204a", part = "body") %>%  
  
  
  align(j     = 2, 
        align = "center", 
        part  = "all") %>%
  bold(bold = FALSE, 
       part = "header") %>%
  flextable::style(pr_t = fp_text(font.size   = 12, 
                                  color       = "#524F4C",
                                  font.family = "Lato Full"), 
                   part = "header") %>%
  flextable::style(pr_t = fp_text(font.size   = 10, 
                                  color       = "#524F4C",
                                  font.family = "Lato Full"), 
                   part = "body") %>%
  italic(italic = TRUE, 
         part = "header") %>%
  surround(j = c(1,2),
           border.top    = fp_border("white"),
           border.bottom = fp_border("white"),
           part = "body"
  )


leyend <- gen_grob(leyend, 
                   fit      = "auto",
                   scaling  = "min", 
                   just     = c("left", "top"),
                   wrapping = T)

layout <- "ABB
           ##C"

viz <- wrap_elements(tpanel) + p + wrap_elements(leyend) +
  plot_layout(ncol = 3, nrow = 2, widths = c(1, 3,0.5), heights = c(1,0.25,0.25), design = layout)
plot(viz)

ggsave(plot = viz, filename = "map_indice.svg", width = 10, height = 10)
