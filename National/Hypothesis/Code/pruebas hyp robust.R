# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Piloto de Hipótesis
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 13th, 2023
##
## This version:      July 288th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Creating a function that will generate the excel files for hypothesis testing                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

prueba_rob <- function(seccion,subseccion,hypo_name,database,dep_var,indep_var) {
  
  dummy_dep = max(select(database,dep_var), na.rm = T) == 1
  dummy_ind = max(select(database,indep_var), na.rm = T) == 1
  
  if (dummy_dep == TRUE & dummy_ind == TRUE ) {
    
    # Barchart depvar vs Estado
    
    dp1 <- database %>% group_by(Estado) %>%
      summarise(dep_var = mean(!!sym(dep_var), na.rm = T))
    
    p1 <- ggplot(dp1, aes(x = dep_var, y = Estado)) + 
      geom_col() 
    
    # Barchart depvar vs Delito
    
    dp2 <- database %>% group_by(Delito_unico_categ) %>%
      summarise(dep_var = mean(!!sym(dep_var), na.rm = T))
    
    p2 <- ggplot(dp2, aes(x = dep_var, y = Delito_unico_categ)) + 
      geom_col() 
    
    # Barchart indepvar vs Estado
    
    dp3 <- database %>% group_by(Estado) %>%
      summarise(indep_var = mean(!!sym(indep_var), na.rm = T))
    
    p3 <- ggplot(dp3, aes(x = indep_var, y = Estado)) + 
      geom_col() 
    
    # Barchart indepvar vs Delito
    
    dp4 <- database %>% group_by(Delito_unico_categ) %>%
      summarise(indep_var = mean(!!sym(indep_var), na.rm = T))
    
    p4 <- ggplot(dp4, aes(x = indep_var, y = Delito_unico_categ)) + 
      geom_col() 
    
  } else if (dummy_dep == TRUE & dummy_ind == FALSE ) {
    
    # Barchart depvar vs Estado
    
    dp1 <- database %>% group_by(Estado) %>%
      summarise(dep_var = mean(!!sym(dep_var), na.rm = T))
    
    p1 <- ggplot(dp1, aes(x = dep_var, y = Estado)) + 
      geom_col() 
    
    # Barchart depvar vs Delito
    
    dp2 <- database %>% group_by(Delito_unico_categ) %>%
      summarise(dep_var = mean(!!sym(dep_var), na.rm = T))
    
    p2 <- ggplot(dp2, aes(x = dep_var, y = Delito_unico_categ)) + 
      geom_col()
    
    # Boxplot indepvar vs Estado
    p3 <- ggplot(database, aes(x = !!sym(indep_var), y = Estado)) + 
      geom_boxplot() 
    
    # Boxplot indepvar vs Delito
    p4 <- ggplot(database, aes(x = !!sym(indep_var), y = Delito_unico_categ)) + 
      geom_boxplot() 


  } else if (dummy_dep == FALSE & dummy_ind == TRUE ) {
    
    # Boxplot depvar vs Estado
    p1 <- ggplot(database, aes(x = !!sym(dep_var), y = Estado)) + 
      geom_boxplot() 
    
    # Boxplot depvar vs Delito
    p2 <- ggplot(database, aes(x = !!sym(dep_var), y = Delito_unico_categ)) + 
      geom_boxplot() 
    
    # Barchart indepvar vs Estado
    
    dp3 <- database %>% group_by(Estado) %>%
      summarise(indep_var = mean(!!sym(indep_var), na.rm = T))
    
    p3 <- ggplot(dp3, aes(x = indep_var, y = Estado)) + 
      geom_col() 
    
    # Barchart indepvar vs Delito
    
    dp4 <- database %>% group_by(Delito_unico_categ) %>%
      summarise(indep_var = mean(!!sym(indep_var), na.rm = T))
    
    p4 <- ggplot(dp4, aes(x = indep_var, y = Delito_unico_categ)) + 
      geom_col() 
    
  } else if (dummy_dep == FALSE & dummy_ind == FALSE ) {
    
    # Boxplot depvar vs Estado
    p1 <- ggplot(database, aes(x = !!sym(dep_var), y = Estado)) + 
      geom_boxplot()
    
    # Boxplot depvar vs Delito
    p2 <- ggplot(database, aes(x = !!sym(dep_var), y = Delito_unico_categ)) + 
      geom_boxplot()
    
    # Boxplot indepvar vs Estado
    p3 <- ggplot(database, aes(x = !!sym(indep_var), y = Estado)) + 
      geom_boxplot()
    
    # Boxplot indepvar vs Delito
    p4 <- ggplot(database, aes(x = !!sym(indep_var), y = Delito_unico_categ)) + 
      geom_boxplot()
    
  }
  
  # Save Output to be interpreted
  
  ggsave(file.path(paste0(path2SP,
                          "/National/Hypothesis/Output/",
                          seccion,"/",
                          subseccion,"/",
                          paste0(hypo_name,"_dep"),".png"),
                   fsep = "/"), arrangeGrob(p1, p2))
  
  
  ggsave(file.path(paste0(path2SP,
                          "/National/Hypothesis/Output/",
                          seccion,"/",
                          subseccion,"/",
                          paste0(hypo_name,"_indep"),".png"),
                   fsep = "/"), arrangeGrob(p3, p4))

}







# Ejemplo: Preparar bases

data_subset_tortura.df <- Main_database %>%
  filter(NSJP == 1) %>% 
  mutate(
    Estado             = case_when(P3_3 == "98" ~ NA_character_,
                                   P3_3 == "99" ~ NA_character_,
                                   T ~ P3_3),
    Sexo               = case_when(SEXO.x == "1" ~ "Masculino",
                                   SEXO.x == "2" ~ "Femenino",
                                   T ~ NA_character_),
    Corporacion_grupos = case_when(Corporacion_grupos == "NS/NR" ~ NA_character_,
                                   T ~ Corporacion_grupos),
    one_year_limit     = if_else(months_since_RND_3 <= 12 & months_since_RND_3 >= -12, 1, 0, 0)
  ) %>%
  select(tortura_tra_p, tortura_tra_f, AA_tortura_generalizada = tortura_generalizada, falsa_culpabilidad, prueba_inculpatoria,
         culpabilidad, declaro_culpable,
         months_since_RND_3, one_year_limit, Delito_unico, Delito_unico_categ,
         Estado, Sexo, fuero, Corporacion_grupos) 



data_subset_policia.df <- Main_database %>% 
  filter(NSJP == 1) %>% 
  mutate(Estado = case_when(P3_3 == "98" ~ NA_character_,
                            P3_3 == "99" ~ NA_character_,
                            T ~ P3_3),
         Corporacion_grupos = case_when(Corporacion_grupos == "NS/NR" ~ NA_character_,
                                        T ~ Corporacion_grupos)) %>%
  select(orden_det, inspeccion, flagrancia, flagrancia_const, det_ninguna, detencion_no_inmediata, 
         months_since_NSJP, years_since_NSJP, 
         Corporacion_grupos, Estado, Sexo, Delito_unico_categ, Delito_unico, LGBTQ, Edad, 
         Traslados_30 = P3_20_01, Traslados_6h = P3_20_06, Traslado_MP = P3_19_01, 
         proporcionalidad_uso_fuerza)

data_list.df <- data_subset_policia.df %>%
  rename(AA_years_since_NSJP = years_since_NSJP) 


 # Correr ejemplos

prueba_rob(seccion = "Detenciones",
           subseccion = "Tortura",
           hypo_name = paste0("hyp_tortura_","falsa_culpabilidad"),
           database = data_subset_tortura.df,
           dep_var = "falsa_culpabilidad",
           indep_var = "AA_tortura_generalizada")


prueba_rob(seccion = "Detenciones",
           subseccion = "Policia",
           hypo_name = paste0("hyp_detenciones_", "proporcionalidad_fuerza"),
           database = data_list.df,
           dep_var = "proporcionalidad_uso_fuerza",
           indep_var = "AA_years_since_NSJP")
