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
         NSJP == 1,
         fuero == "Sólo común" )

#NOTA: VER FILTROS




# Brecha entre el porcentaje de detenciones por tipo de fuero ------------------------------

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



Estados <- Main_database_2008 %>% 
  select(Estado_arresto, corporacion_fuero) %>% 
  group_by(Estado_arresto, corporacion_fuero) %>%
  summarise(Frequency = n(), .groups = 'drop') %>% 
  group_by(Estado_arresto) %>%
  filter (corporacion_fuero  != "Operativo Conjunto", 
          corporacion_fuero != "Otra") %>% 
  mutate(Percentage = Frequency / sum(Frequency) * 100) %>% 
  select(-Frequency) %>% 
  pivot_wider(names_from = corporacion_fuero, values_from = Percentage) %>% 
  mutate(gap = round(`Corporación Local` - `Corporación Federal`),0)%>% 
  select(Estado_arresto, gap) %>%
  rename(ESTADO = Estado_arresto) %>%
  drop_na() %>%
  mutate(
    ESTADO = 
      case_when(
        ESTADO == "Coahuila de Zaragoza" ~ "Coahuila",
        ESTADO == "Michoacán de Ocampo"  ~ "Michoacán",
        ESTADO == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
        ESTADO == "México" ~ "Estado de México",
        ESTADO == "Distrito Federal" ~ "Ciudad de México",
        T ~ ESTADO
      ))

quintiles <- round(quantile(round(Estados$gap, 0), probs = seq(0, 1, by = 0.2)),0)


mapa <- st_read(paste0(path2SP,"/National/Exploration/Input/shp/México_Estados.shp")) %>%
  mutate( ESTADO = 
            case_when(
              ESTADO == "México" ~ "Estado de México",
              ESTADO == "Distrito Federal" ~ "Ciudad de México",
              T ~ ESTADO
            )
  )

table <- Estados %>%
  mutate(
    ` ` = "",
    `%` = round(gap, 0)
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
  

  bg(i = ~ `%` >= quintiles[1] & `%` <= quintiles[2], j = ' ', bg = "#C0392B", part = "body") %>%
  bg(i = ~ `%` > quintiles[2] & `%` <= quintiles[3], j = ' ', bg = "#F1948A", part = "body") %>%
  bg(i = ~ `%` > quintiles[3] & `%` <= quintiles[4], j = ' ', bg = "#F9E79F", part = "body") %>%
  bg(i = ~ `%` > quintiles[4] & `%` <= quintiles[5], j = ' ', bg = "#7DCEA0", part = "body") %>%
  bg(i = ~ `%` > quintiles[5] & `%` <= quintiles[6], j = ' ', bg = "#186A3B", part = "body") %>%  
  
  
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
  mutate(value2plot = round(gap), 0) %>%
  mutate(
    color_group = case_when(
      value2plot >= quintiles[1] & value2plot <= quintiles[2] ~ "50%-76%",
      value2plot > quintiles[2] & value2plot <= quintiles[3] ~ "76%-82%",
      value2plot > quintiles[3] & value2plot <= quintiles[4] ~ "82%-86%",
      value2plot > quintiles[4] & value2plot <= quintiles[5] ~ "86%-88%",
      value2plot > quintiles[5] & value2plot <= quintiles[6] ~ "88%-96%"
    ),
    color_group = as.factor(color_group)
  )

cat_palette <- c("50%-76%"  = "#C0392B",
                 "76%-82%"  = "#F1948A",
                 "82%-86%"  = "#F9E79F",
                 "86%-88%"  = "#7DCEA0",
                 "88%-96%" = "#186A3B")
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

categories <- c("[50%-76%]",
                "(76%-82%]",
                "(82%-86%]",
                "(86%-88%]",
                "(88%-96%]")

leyend <- data.frame(
  Values = categories,
  Blank = "")
leyend <- flextable(leyend)  %>% 
  width(j = "Blank", width = 0.5, unit = "mm") %>% 
  set_header_labels(Values = "Escala", Blank = " ") %>% 
  bg(i = ~ Values == "[50%-76%]", j = "Blank", bg = "#C0392B", part = "body") %>%
  bg(i = ~ Values == "(76%-82%]", j = "Blank", bg = "#F1948A", part = "body") %>%
  bg(i = ~ Values == "(82%-86%]", j = "Blank", bg = "#F9E79F", part = "body") %>%
  bg(i = ~ Values == "(86%-88%]", j = "Blank", bg = "#7DCEA0", part = "body") %>%
  bg(i = ~ Values == "(88%-96%]", j = "Blank", bg = "#186A3B", part = "body") %>%  
  
  
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

