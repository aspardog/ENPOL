## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL Hypothesis
##
## Script:            Study Event by type of detention
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 5th, 2023
##
## This version:      November 5th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Infographic analysis                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

hyp_detenciones_event <- function(data.df= Main_database) {

  master_data.df <- data.df %>%
    select(P3_3, orden_det, flagrancia, flagrancia_const, inspeccion, det_ninguna, 
           months_since_NSJP, years_since_NSJP, 
           Corporacion_grupos, Sexo, starts_with("Del_"), traslados_30= P3_20_01, traslados_6h = P3_20_06) %>%
    mutate(
      Estado =
        case_when(
          P3_3 == "98" ~ NA_character_,
          P3_3 == "99" ~ NA_character_,
          T ~ P3_3),
      Corporacion_grupos = 
        case_when(
          Corporacion_grupos == "NS/NR" ~ NA_character_,
          T ~ Corporacion_grupos)
    ) %>%
    mutate(
      across(starts_with("Del_"),
            as.numeric)
    ) %>% 

    select(!c(P3_3)) %>%
    mutate(before_nsjp = 
             if_else(years_since_NSJP < 0, 1, 0),
           after_nsjp  = 
             if_else(years_since_NSJP < 0, 0, 1)
    ) %>%
    mutate(period = 
             case_when(
               years_since_NSJP < -1 & years_since_NSJP > -2 ~ "one_year_before",
               years_since_NSJP < -2 & years_since_NSJP > -3 ~ "two_years_before",
               years_since_NSJP < -3 & years_since_NSJP > -4 ~ "three_years_before",
               years_since_NSJP < -4 & years_since_NSJP > -5 ~ "four_years_before",
               years_since_NSJP < -5 & years_since_NSJP > -6 ~ "five_years_before",
               years_since_NSJP < -6 & years_since_NSJP > -7 ~ "six_years_before",
               years_since_NSJP < -7 & years_since_NSJP > -8 ~ "seven_years_before",
               years_since_NSJP < -8 & years_since_NSJP > -9 ~ "eight_years_before",
               years_since_NSJP < -9 & years_since_NSJP > -10 ~ "nine_years_before",
               years_since_NSJP < -10 & years_since_NSJP > -11 ~ "ten_years_before",
               years_since_NSJP > -1 & years_since_NSJP < 1  ~ "implementation_year",
               years_since_NSJP > 1 & years_since_NSJP < 2 ~ "one_year_after",
               years_since_NSJP > 2 & years_since_NSJP < 3 ~ "two_years_after",
               years_since_NSJP > 3 & years_since_NSJP < 4 ~ "three_years_after",
               years_since_NSJP > 4 & years_since_NSJP < 5 ~ "four_years_after",
               years_since_NSJP > 5 & years_since_NSJP < 6 ~ "five_years_after",
               years_since_NSJP > 6 & years_since_NSJP < 7 ~ "six_years_after",
               years_since_NSJP > 7 & years_since_NSJP < 8 ~ "seven_years_after",
               years_since_NSJP > 8 & years_since_NSJP < 9 ~ "eight_years_after",
               years_since_NSJP > 9 & years_since_NSJP < 10 ~ "nine_years_after",
               years_since_NSJP > 10 & years_since_NSJP < 11 ~ "ten_years_after",
               years_since_NSJP > 11 & years_since_NSJP < 12 ~ "eleven_years_after",
               years_since_NSJP > 13 & years_since_NSJP < 14 ~ "twelve_years_after",
               years_since_NSJP > 14 & years_since_NSJP < 15 ~ "thirteen_years_after"
             )) %>%
    filter(!is.na(period)) %>%
    arrange(years_since_NSJP) %>%
    mutate(National = "National")
  
  variables2analyze <- c("National","Estado", "Corporacion_grupos", "Sexo",
                         names(master_data.df %>% select(starts_with("Del_"))))
  
  data2analysis <- lapply(variables2analyze, function(vars){
    
    data_subset.df <- master_data.df %>%
      rename(group = all_of({{vars}}))
    
    changes_time <- data_subset.df %>% 
      group_by(period, group) %>%
      summarise(orden_det    = mean(orden_det, na.rm = T),
                flagrancia   = mean(flagrancia, na.rm = T),
                inspeccion   = mean(inspeccion, na.rm = T),
                irregular    = mean(det_ninguna, na.rm = T),
                traslados_30 = mean(traslados_30, na.rm = T),
                traslados_6h = mean(traslados_6h, na.rm = T)) %>%
      mutate(order_value = 
               case_when(
                 period == "ten_years_before"    ~ -10,
                 period == "nine_years_before"   ~ -9,
                 period == "eight_years_before"  ~ -8,
                 period == "seven_years_before"  ~ -7,
                 period == "six_years_before"    ~ -6,
                 period == "five_years_before"   ~ -5,
                 period == "four_years_before"   ~ -4,
                 period == "three_years_before"  ~ -3,
                 period == "two_years_before"    ~ -2,
                 period == "one_year_before"     ~ -1,
                 period == "implementation_year" ~ 0,
                 period == "one_year_after"      ~ 1,
                 period == "two_years_after"     ~ 2,
                 period == "three_years_after"   ~ 3,
                 period == "four_years_after"    ~ 4,
                 period == "five_years_after"    ~ 5,
                 period == "six_years_after"     ~ 6,
                 period == "seven_years_after"   ~ 7
                 # period == "eight_years_after"   ~ 8,
                 # period == "nine_years_after"    ~ 9,
                 # period == "ten_years_after"     ~ 10,
                 # period == "eleven_years_after"  ~ 11,
                 # period == "twelve_years_after"   ~ 12
               )
      ) %>%
      arrange(all_of(vars), order_value) %>%
      filter(order_value > -6)
    
    change_pct_rate <-  changes_time %>%
      select(!order_value) %>%
      pivot_longer(cols = c(orden_det, flagrancia, inspeccion, irregular, traslados_30, traslados_6h),
                   names_to = "type", values_to = "values") %>%
      pivot_wider(id_cols = c(type, group), names_from = "period", values_from = "values") %>%
      select(type, group, implementation_year, ends_with("after")) %>%
      mutate(first_year_rate_change_pct     = ((one_year_after       - implementation_year)/implementation_year),
             second_year_rate_change_pct    = ((two_years_after      - one_year_after)/one_year_after),
             third_year_rate_change_pct     = ((three_years_after    - two_years_after)/two_years_after),
             fourth_year_rate_change_pct    = ((four_years_after     - three_years_after)/three_years_after),
             fifth_year_rate_change_pct     = ((five_years_after     - four_years_after)/four_years_after),
             sixth_year_rate_change_pct     = ((six_years_after      - five_years_after)/five_years_after),
             seventh_year_rate_change_pct   = ((seven_years_after    - six_years_after)/six_years_after)
             # eighth_year_rate_change_pct    = ((eight_years_after    - seven_years_after)/seven_years_after),
             # nineth_year_rate_change_pct    = ((nine_years_after     - eight_years_after)/eight_years_after),
             # tenth_year_rate_change_pct     = ((ten_years_after      - nine_years_after)/nine_years_after),
             # eleventh_year_rate_change_pct  = ((eleven_years_after   - ten_years_after)/ten_years_after),
             # twelfth_year_rate_change_pct   = ((twelve_years_after   - eleven_years_after)/eleven_years_after),
      ) %>%
      group_by(type, group) %>%
      summarise(rate_change_pct = mean(c(first_year_rate_change_pct, second_year_rate_change_pct, third_year_rate_change_pct, 
                                         fourth_year_rate_change_pct, fifth_year_rate_change_pct, sixth_year_rate_change_pct, 
                                         seventh_year_rate_change_pct), na.rm = T) *100)
    
    listResults <- list(changes_time, change_pct_rate )
    names(listResults) <- c(paste0(vars, "_time"),
                            paste0(vars, "_rate"))
    return(listResults)
    
  })
  
  openxlsx::write.xlsx(unlist(data2analysis, recursive = F),
                       "National/Hypothesis/Output/Detenciones/Tipo/hyp_detenciones_infografia.xlsx")
  return(data2analysis)
}
# 
data2analyze <- hyp_detenciones_event()
 
data2plot <- data2analyze[[1]][["National_time"]] %>%
  select(period, group, starts_with("traslados"), order_value) %>%
  pivot_longer(cols = c(traslados_30, traslados_6h),
               names_to = "traslados", values_to = "value") %>%
  mutate(colors =
           case_when(
             traslados == "traslados_30"  ~ "Traslados en 30 minutos",
             traslados == "traslados_6h" ~  "Traslados en 6 a 24 horas"
           ))

traslados <- ggplot(data = data2plot,
       aes(x = reorder(period, order_value),
           y = value,
           group = traslados,
           color = colors)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = "implementation_year", color = "red") +
  labs(x = "Year", y = "Outcome") +
  theme_bw(base_size=16) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.text.x        = element_text(angle = 45),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"), 
        legend.position = "top", legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0,0),
                     breaks = seq(0,0.5,0.1),
                     labels = paste0(seq(0,50,10), "%"))
ggsave(plot = traslados, paste0(path2DB, "/National/Hypothesis/Output/Detenciones/Tipo/traslados_seven.svg"), width = 10, height = 7)

data2plot <- data2analyze[[4]][["Sexo_time"]] %>%
  select(period, group, starts_with("traslados"), order_value) %>%
  pivot_longer(cols = c(traslados_30, traslados_6h),
               names_to = "traslados", values_to = "value") %>%
  pivot_wider(id_cols = c(period, order_value), names_from = c("group", "traslados"), values_from = "value") %>%
  pivot_longer(cols = c("Femenino_traslados_30", "Femenino_traslados_6h", "Masculino_traslados_30", "Masculino_traslados_6h"),
               names_to = "traslados", values_to = "value") %>%
  mutate(colors =
           case_when(
             traslados == "Femenino_traslados_30"  ~ "Traslados en 30 minutos: Mujer",
             traslados == "Femenino_traslados_6h" ~  "Traslados en 6 a 24 horas: Mujer",
             traslados == "Masculino_traslados_30"  ~ "Traslados en 30 minutos: Hombre",
             traslados == "Masculino_traslados_6h" ~  "Traslados en 6 a 24 horas: Hombre"
           ))

traslados_sexo <- ggplot(data = data2plot,
                    aes(x = reorder(period, order_value),
                        y = value,
                        group = traslados,
                        color = colors)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = "implementation_year", color = "red") +
  labs(x = "Year", y = "Outcome") +
  theme_bw(base_size=16) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.text.x        = element_text(angle = 45),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"), 
        legend.position = "top", legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0,0),
                     breaks = seq(0,0.5,0.1),
                     labels = paste0(seq(0,50,10), "%"))

ggsave(plot = traslados_sexo, paste0(path2DB, "/National/Hypothesis/Output/Detenciones/Tipo/traslados_sexo_seven.svg"), width = 15, height = 7)

data2plot <- data2analyze[[1]][["National_time"]] %>%
  select(period, group, !starts_with("traslados"), order_value) %>%
  pivot_longer(cols = c("orden_det", "flagrancia", "inspeccion", "irregular"),
               names_to = "detention", values_to = "value") %>%
  mutate(colors =
           case_when(
             detention == "orden_det"  ~ "Orden de detencion",
             detention == "flagrancia" ~ "Flagrancia",
             detention == "inspeccion"  ~ "Inspeccion",
             detention == "irregular"  ~ "Detenciones irregulares"
           ))

traslados <- ggplot(data = data2plot,
                    aes(x = reorder(period, order_value),
                        y = value,
                        group = detention,
                        color = colors)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = "implementation_year", color = "red") +
  labs(x = "Year", y = "Outcome") +
  theme_bw(base_size=16) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#d1cfd1"),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.text.x        = element_text(angle = 45),
        axis.line.x        = element_line(color    = "#d1cfd1"),
        axis.ticks.x       = element_line(color    = "#d1cfd1",
                                          linetype = "solid"), 
        legend.position = "top", legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0,0),
                     breaks = seq(0,0.5,0.1),
                     labels = paste0(seq(0,50,10), "%"))
ggsave(plot = traslados, paste0(path2DB, "/National/Hypothesis/Output/Detenciones/Tipo/detenciones_twelve.svg"), width = 15, height = 7)

# 
# 
# detenciones_estado <- data2analyze[[2]][["Estado_change_rate"]] %>%
#   filter(type == "orden_det") %>%
#   arrange(-rate_change_pct)
# 
# detenciones_corporacion <- data2analyze[[3]][["Corporacion_grupos_change_rate"]] %>%
#   filter(type == "orden_det") %>%
#   arrange(rate_change_pct)
