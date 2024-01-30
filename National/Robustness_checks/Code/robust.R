# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Piloto de Hipótesis
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##
## Dependencies:      World Justice Project
##
## Creation date:     Jan 3th, 2024
##
## This version:      Jane 29th, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Creating a function that will generate the graphs for robustness testing                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

prueba_rob <- function(seccion,hypo_name,database,dep_var,indep_var) {
  
  onevar = is.na(indep_var)
  
  if (onevar == T) {
    
  dummy_dep = max(select(database,dep_var), na.rm = T) == 1
  
  if (dummy_dep == TRUE) {
    
    # Barchart depvar vs Estado
    dp1 <- database %>% group_by(Estado_arresto) %>%
      summarise(dep_var = mean(!!sym(dep_var), na.rm = T))
    
    p1 <- ggplot(dp1, aes(x = dep_var, y = Estado_arresto)) + 
      geom_col() 
    
    # Barchart depvar vs Delito
    dp2 <- database %>% group_by(Delito_unico_categ) %>%
      summarise(dep_var = mean(!!sym(dep_var), na.rm = T))
    
    p2 <- ggplot(dp2, aes(x = dep_var, y = Delito_unico_categ)) + 
      geom_col() 
  
  } else {
    
    # Boxplot depvar vs Estado
    p1 <- ggplot(database, aes(x = !!sym(dep_var), y = Estado_arresto)) + 
      geom_boxplot() 
    
    # Boxplot depvar vs Delito
    p2 <- ggplot(database, aes(x = !!sym(dep_var), y = Delito_unico_categ)) + 
      geom_boxplot() 
  }
    
  
  # Save Output to be interpreted
  
  ggsave(file.path(paste0(path2SP,
                          "/National/Robustness_Checks/Output/",
                          seccion,"/",
                          paste0(hypo_name,"_dep"),".png"),
                   fsep = "/"), arrangeGrob(p1, p2))
  
  } else {
  
  dummy_dep = max(select(database,dep_var), na.rm = T) == 1
  dummy_ind = max(select(database,indep_var), na.rm = T) == 1
  
  if (dummy_dep == TRUE & dummy_ind == TRUE ) {
    
    # Barchart depvar vs Estado
    dp1 <- database %>% group_by(Estado_arresto) %>%
      summarise(dep_var = mean(!!sym(dep_var), na.rm = T))
    
    p1 <- ggplot(dp1, aes(x = dep_var, y = Estado_arresto)) + 
      geom_col() 
    
    # Barchart depvar vs Delito
    dp2 <- database %>% group_by(Delito_unico_categ) %>%
      summarise(dep_var = mean(!!sym(dep_var), na.rm = T))
    
    p2 <- ggplot(dp2, aes(x = dep_var, y = Delito_unico_categ)) + 
      geom_col() 
    
    # Barchart indepvar vs Estado
    dp3 <- database %>% group_by(Estado_arresto) %>%
      summarise(indep_var = mean(!!sym(indep_var), na.rm = T))
    
    p3 <- ggplot(dp3, aes(x = indep_var, y = Estado_arresto)) + 
      geom_col() 
    
    # Barchart indepvar vs Delito
    dp4 <- database %>% group_by(Delito_unico_categ) %>%
      summarise(indep_var = mean(!!sym(indep_var), na.rm = T))
    
    p4 <- ggplot(dp4, aes(x = indep_var, y = Delito_unico_categ)) + 
      geom_col() 
    
  } else if (dummy_dep == TRUE & dummy_ind == FALSE ) {
    
    # Barchart depvar vs Estado
    dp1 <- database %>% group_by(Estado_arresto) %>%
      summarise(dep_var = mean(!!sym(dep_var), na.rm = T))
    
    p1 <- ggplot(dp1, aes(x = dep_var, y = Estado_arresto)) + 
      geom_col() 
    
    # Barchart depvar vs Delito
    dp2 <- database %>% group_by(Delito_unico_categ) %>%
      summarise(dep_var = mean(!!sym(dep_var), na.rm = T))
    
    p2 <- ggplot(dp2, aes(x = dep_var, y = Delito_unico_categ)) + 
      geom_col()
    
    # Boxplot indepvar vs Estado
    p3 <- ggplot(database, aes(x = !!sym(indep_var), y = Estado_arresto)) + 
      geom_boxplot() 
    
    # Boxplot indepvar vs Delito
    p4 <- ggplot(database, aes(x = !!sym(indep_var), y = Delito_unico_categ)) + 
      geom_boxplot() 


  } else if (dummy_dep == FALSE & dummy_ind == TRUE ) {
    
    # Boxplot depvar vs Estado
    p1 <- ggplot(database, aes(x = !!sym(dep_var), y = Estado_arresto)) + 
      geom_boxplot() 
    
    # Boxplot depvar vs Delito
    p2 <- ggplot(database, aes(x = !!sym(dep_var), y = Delito_unico_categ)) + 
      geom_boxplot() 
    
    # Barchart indepvar vs Estado
    dp3 <- database %>% group_by(Estado_arresto) %>%
      summarise(indep_var = mean(!!sym(indep_var), na.rm = T))
    
    p3 <- ggplot(dp3, aes(x = indep_var, y = Estado_arresto)) + 
      geom_col() 
    
    # Barchart indepvar vs Delito
    dp4 <- database %>% group_by(Delito_unico_categ) %>%
      summarise(indep_var = mean(!!sym(indep_var), na.rm = T))
    
    p4 <- ggplot(dp4, aes(x = indep_var, y = Delito_unico_categ)) + 
      geom_col() 
    
  } else if (dummy_dep == FALSE & dummy_ind == FALSE ) {
    
    # Boxplot depvar vs Estado
    p1 <- ggplot(database, aes(x = !!sym(dep_var), y = Estado_arresto)) + 
      geom_boxplot()
    
    # Boxplot depvar vs Delito
    p2 <- ggplot(database, aes(x = !!sym(dep_var), y = Delito_unico_categ)) + 
      geom_boxplot()
    
    # Boxplot indepvar vs Estado
    p3 <- ggplot(database, aes(x = !!sym(indep_var), y = Estado_arresto)) + 
      geom_boxplot()
    
    # Boxplot indepvar vs Delito
    p4 <- ggplot(database, aes(x = !!sym(indep_var), y = Delito_unico_categ)) + 
      geom_boxplot()
    
  }
  
  # Save Output to be interpreted
  
  ggsave(file.path(paste0(path2SP,
                          "/National/Robustness_Checks/Output/",
                          seccion,"/",
                          paste0(hypo_name,"_dep"),".png"),
                   fsep = "/"), arrangeGrob(p1, p2))
  
  
  ggsave(file.path(paste0(path2SP,
                          "/National/Robustness_Checks/Output/",
                          seccion,"/",
                          paste0(hypo_name,"_indep"),".png"),
                   fsep = "/"), arrangeGrob(p3, p4))

}}



#### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Settings                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes and other general routines are
# loaded from the following script:
source("National/Data_cleaning/Code/settings.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

# Remove previous files

outPath <- c(paste0(path2SP,"/National/Robustness_checks/Output/"))
  

#Listing previous outputs
prevOutputs <- list.files(outPath,
                          include.dirs = F,
                          full.names   = T,
                          recursive    = T)

# Deleting previous outputs and remove objects used for this cleaning process
nombre_archivos <- sub(".*/([^/]+\\.png)$", "\\1", prevOutputs)
cat(paste0("El siguiente archivo fue eliminado: ", nombre_archivos))

file.remove(prevOutputs)





## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Runnning the tests.                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


data <- Main_database %>% filter(NSJP == 1, Anio_arresto>=2015)   %>%
  mutate(Mujer = case_when(Sexo == "Masculino" ~ 0,
                           Sexo == "Femenino" ~ 1),
         Operativo_Conjunto = case_when(Corporacion_grupos == "Operativo Conjunto" ~ 1,
                                        is.na(Corporacion_grupos) == T ~ NA,
                                        T ~ 0),
         Ejercito_o_Marina = case_when(Corporacion_grupos == "Ejército o Marina" ~ 1,
                                       is.na(Corporacion_grupos) == T ~ NA,
                                       T ~ 0),
         Policia_Federal = case_when(Corporacion_grupos == "Policía Federal" ~ 1,
                                     is.na(Corporacion_grupos) == T ~ NA,
                                     T ~ 0), 
         Policia_Federal_Ministerial = case_when(Corporacion_grupos == "Policía Federal Ministerial" ~ 1,
                                                 is.na(Corporacion_grupos) == T ~ NA,
                                                 T ~ 0),
         Policia_Estatal = case_when(Corporacion_grupos == "Policía Estatal" ~ 1,
                                     is.na(Corporacion_grupos) == T ~ NA,
                                     T ~ 0),
         Policia_Municipal = case_when(Corporacion_grupos == "Policía Municipal" ~ 1,
                                       is.na(Corporacion_grupos) == T ~ NA,
                                       T ~ 0)) %>%
  pivot_longer(cols = c("Tiempo_traslado","Primer_lugar_traslado"),
               names_to = "dummy_names",
               values_to = "dummy_levels") %>%
  mutate(dummy_value = 1) %>%
  pivot_wider(names_from = c(dummy_names, dummy_levels),
              values_from = dummy_value, 
              values_fill = 0)



## Figura 1.1

prueba_rob(seccion = "Uso excesivo de la fuerza",
           hypo_name = paste0("Figura1_1"),
           database = data,
           dep_var = "proporcionalidad_uso_fuerza",
           indep_var = "years_since_NSJP")


## Figura 1.2

data2 <- data %>%
  filter(Delito_unico == 1) 


sociodems <- c("Mujer","Educacion_obligatoria","Colo_piel_claro","LGBTQ","Etnia","Ingreso_inseguro","Edad_menor30")

for (i in sociodems) {
prueba_rob(seccion = "Uso excesivo de la fuerza",
           hypo_name = paste0("Figura1_2",i),
           database = data2,
           dep_var = "proporcionalidad_uso_fuerza",
           indep_var = i)
}



## Figra 1.3


categories <- c("Operativo_Conjunto","Ejercito_o_Marina","Policia_Federal", "Policia_Federal_Ministerial","Policia_Estatal",
                "Policia_Municipal")

for (i in categories) {
prueba_rob(seccion = "Uso excesivo de la fuerza",
           hypo_name = paste0("Figura1_3",i),
           database = data2,
           dep_var = "proporcionalidad_uso_fuerza",
           indep_var = i)
}

data2$Poli

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Integridad Personal                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Figura 2.1

prueba_rob(seccion = "Violación a la integridad personal",
           hypo_name = paste0("Figura2_1"),
           database = data,
           dep_var = "tortura_generalizada",
           indep_var = "years_since_NSJP")


## Figura 2.2

for (i in sociodems) {
prueba_rob(seccion = "Violación a la integridad personal",
           hypo_name = paste0("Figura2_2",i),
           database = data2,
           dep_var = "tortura_generalizada",
           indep_var = i)
}




## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Libertad                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Figura 3.1


prueba_rob(seccion = "Violación a la libertad",
           hypo_name = paste0("Figura3_1"),
           database = data,
           dep_var = "det_ninguna",
           indep_var = "years_since_NSJP")



## Figura 3.2

for (i in sociodems) {
  prueba_rob(seccion = "Violación a la libertad",
             hypo_name = paste0("Figura3_2",i),
             database = data2,
             dep_var = "det_ninguna",
             indep_var = i)
}


## Figura 3.3

Tiempo_traslado <- c("Tiempo_traslado_Hasta 30 minutos", "Tiempo_traslado_Más de 30 minutos hasta 1 hora", 
                     "Tiempo_traslado_Más de 1 hora hasta 2 horas", "Tiempo_traslado_Más de 2 horas hasta 4 horas",
                     "Tiempo_traslado_Más de 4 horas hasta 6 horas", "Tiempo_traslado_Más de 6 horas hasta 24 horas", 
                     "Tiempo_traslado_Más de 24 horas hasta 48 horas","Tiempo_traslado_Más de 48 horas hasta 72 horas",  
                      "Tiempo_traslado_Más de 72 horas")

for (i in Tiempo_traslado ) {
prueba_rob(seccion = "Violación a la ilbertad",
             hypo_name = paste0("Figura3_3",i),
             database = data,
             dep_var = i,
             indep_var = NA)
}
   

## Figura 3.4


prueba_rob(seccion = "Violación a la libertad",
           hypo_name = paste0("Figura3_4"),
           database = data2,
           dep_var = "Tiempo_traslado_Hasta 30 minutos",
           indep_var = "years_since_NSJP")

prueba_rob(seccion = "Violación a la libertad",
           hypo_name = paste0("Figura3_4"),
           database = data2,
           dep_var = "Tiempo_traslado_Más de 6 horas hasta 24 horas",
           indep_var = "years_since_NSJP")


## Figura 3.5

Primer_lugar_traslado <- c("Primer_lugar_traslado_Agencia del Ministerio Público", 
                            "Primer_lugar_traslado_Instalación de la policía", 
                            "Primer_lugar_traslado_Juez de lo penal",
                           "Primer_lugar_traslado_Centro penitenciario", 
                           "Primer_lugar_traslado_Oficina del gobierno", 
                           "Primer_lugar_traslado_Casa particular", 
                           "Primer_lugar_traslado_Hospital", 
                           "Primer_lugar_traslado_Terreno baldío",       
                           "Primer_lugar_traslado_Vehículo", 
                           "Primer_lugar_traslado_Centro de arraigo", 
                           "Primer_lugar_traslado_Otra",
                           "Primer_lugar_traslado_Establecimiento comercial", 
                           "Primer_lugar_traslado_Zona militar",
                           "Primer_lugar_traslado_Centro de detención para migrantes")

for (i in Primer_lugar_traslado ) {
prueba_rob(seccion = "Violación a la libertad",
           hypo_name = paste0("Figura3_5",i),
           database = data2,
           dep_var = i,
           indep_var = NA)
}

