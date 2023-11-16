## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Project:           ENPOL Hypothesis
##
## Script:            Hypothesis generation
##
## Author(s):         Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Alvarez         (mtorres@worldjusticeproject.org)
##                    Marcelo Torres           (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 4th, 2023
##
## This version:      November 4th, 2023
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
source("National/Hypothesis/Code/settings.R")
source("National/Hypothesis/Code/pruebas_hip.R")
source("National/Hypothesis/Code/hyp_individual_detencion_tipo.R")

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 

# Remove previous files

outPaths <- c("National/Hypothesis/Output/")

# Listing previous outputs
prevOutputs <- list.files(outPaths, 
                          include.dirs = F, 
                          full.names   = T, 
                          recursive    = T)
# Deleting previous outputs and remove objects used for this cleaning process
file.remove(prevOutputs)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Hypothesis                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Detenciones                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Tipo ----

# Create the database to be used

data_subset_tipo.df <- Main_database %>% 
  filter(NSJP == 1) %>% 
  mutate(Estado = case_when(P3_3 == "98" ~ NA_character_,
                            P3_3 == "99" ~ NA_character_,
                            T ~ P3_3),
         Corporacion_grupos = case_when(Corporacion_grupos == "NS/NR" ~ NA_character_,
                                        T ~ Corporacion_grupos),
         Robo_vehiculo = coalesce(P5_11_01, P5_31_01),
         Robo_casa_hab = coalesce(P5_11_02, P5_31_02),
         Robo_negocio = coalesce(P5_11_03, P5_31_03),
         Robo_transporte_pub = coalesce(P5_11_04, P5_31_04),
         Robo_transeunte = coalesce(P5_11_05, P5_31_05),
         Robo_autopartes = coalesce(P5_11_06, P5_31_06),
         Robo_otros = coalesce(P5_11_07, P5_31_07),
         Posesion_drogas = coalesce(P5_11_08, P5_31_08),
         Comercio_drogas = coalesce(P5_11_09, P5_31_09),
         Lesiones = coalesce(P5_11_10, P5_31_10),
         Hom_culposo = coalesce(P5_11_11, P5_31_11),
         Hom_doloso = coalesce(P5_11_12, P5_31_12),
         Portacion_armas = coalesce(P5_11_13, P5_31_13),
         Incum_asis_fam = coalesce(P5_11_14, P5_31_14),
         Violencia_fam = coalesce(P5_11_15, P5_31_15),
         Danio_prop = coalesce(P5_11_16, P5_31_16),
         Secuestro = coalesce(P5_11_17, P5_31_17),
         Violacion_sexual = coalesce(P5_11_18, P5_31_18),
         Fraude = coalesce(P5_11_19, P5_31_19),
         Delincuencia_org = coalesce(P5_11_20, P5_31_20),
         Otros_sexuales = coalesce(P5_11_21, P5_31_21),
         Extorsion = coalesce(P5_11_22, P5_31_22),
         Privacion_de_libertad = coalesce(P5_11_23, P5_31_23),
         Abuso_de_conf = coalesce(P5_11_24, P5_31_24),
         Amenazas = coalesce(P5_11_25, P5_31_25),
         Otros = coalesce(P5_11_26, P5_31_26),
         No_sabe = coalesce(P5_11_98, P5_31_98),
         No_responde = coalesce(P5_11_99, P5_31_99)) %>%
  select(orden_det, inspeccion, flagrancia, flagrancia_const, det_ninguna, detencion_no_inmediata, 
         months_since_NSJP, years_since_NSJP, Corporacion_grupos, Estado, Sexo, 
         Robo_vehiculo, Robo_casa_hab, Robo_negocio, Robo_transporte_pub, Robo_transeunte, Robo_autopartes, Robo_otros, Posesion_drogas,
         Comercio_drogas, Lesiones, Hom_culposo, Hom_doloso, Portacion_armas, Incum_asis_fam, Violencia_fam, Danio_prop, Secuestro, Fraude,
         Violacion_sexual, Delincuencia_org, Otros_sexuales, Extorsion, Privacion_de_libertad, Abuso_de_conf, Amenazas, Otros, No_sabe, No_responde,
         Traslados_30 = P3_20_01, LGBTQ, Edad, Traslado_MP = P3_19_01, proporcionalidad_uso_fuerza)

tipo <- c("flagrancia", 
          "orden_det", 
          "inspeccion", 
          "det_ninguna") # List of dependent variables from the first analysis

independent_time_vars <- c("months_since_NSJP", 
                           "years_since_NSJP")    # List of independent time variables

result_list <- lapply(tipo, function(tipo) {
  lapply(independent_time_vars, function(independent_vars) {
    
    var_type <- if_else(independent_vars == "months_since_NSJP", "months", "years")
    
    data_list.df <- data_subset_tipo.df %>%
      rename(target = all_of(tipo),
             independent = all_of(independent_vars))
    
    result <- prueba_hip(
      seccion = "Detenciones",
      subseccion = "Tipo",
      hypo_name = paste0("hyp_detenciones_", tipo, "_", var_type),
      type = "logit",
      database = data_list.df,
      dep_var = target,
      indep_var = independent,
      group_vars = c("Corporacion_grupos", "Estado", "Sexo", "Robo_vehiculo", "Robo_casa_hab", "Robo_negocio",  
                     "Robo_transporte_pub", "Robo_transeunte", "Robo_autopartes", "Robo_otros", "Posesion_drogas",  
                     "Comercio_drogas", "Lesiones", "Hom_culposo", "Hom_doloso", "Portacion_armas", 
                     "Violencia_fam", "Danio_prop", "Secuestro", "Violacion_sexual", "Delincuencia_org", 
                     "Otros_sexuales", "Extorsion", "Privacion_de_libertad","Abuso_de_conf", "Amenazas")
    )
    
    return(result)
  })
})

tipo <- c("Traslados_30", 
          "Traslado_MP") # List of dependent variables from the first analysis

independent_time_vars <- c("months_since_NSJP", 
                           "years_since_NSJP")    # List of independent time variables

result_list <- lapply(tipo, function(tipo) {
  lapply(independent_time_vars, function(independent_vars) {
    
    var_type <- if_else(independent_vars == "months_since_NSJP", "months", "years")
    
    data_list.df <- data_subset_tipo.df %>%
      rename(target = all_of(tipo),
             independent = all_of(independent_vars))
    
    result <- prueba_hip(
      seccion = "Detenciones",
      subseccion = "Tipo",
      hypo_name = paste0("hyp_detenciones_", tipo, "_", var_type),
      type = "logit",
      database = data_list.df,
      dep_var = target,
      indep_var = independent,
      group_vars = c("Corporacion_grupos", "Sexo", "LGBTQ")
    )
    
    return(result)
  })
})


flag_const <- c("flagrancia_const", 
                "detencion_no_inmediata")     # List of dependent variables from the second analysis

tipo_flag_const <- list(
  
  result1_2 <- lapply(independent_time_vars, function(independent_vars) {
    
    data_list.df <- data_subset_tipo.df %>%
      filter(flagrancia == 1) %>%
      rename(independent = all_of(independent_vars))
    
    var_type <- if_else(independent_vars == "months_since_NSJP", "months", "years")
    
    result   <- prueba_hip(seccion = "Detenciones", 
                           subseccion = "Tipo", 
                           hypo_name = paste0("hyp_detenciones_flagrancia_const_", var_type),
                           type = "logit", 
                           database = data_list.df, 
                           dep_var = flagrancia_const, 
                           indep_var = independent,
                           group_vars = c("Corporacion_grupos", "Estado", "Sexo", "Robo_vehiculo", "Robo_casa_hab", "Robo_negocio",  
                                         "Robo_transporte_pub", "Robo_transeunte", "Robo_autopartes", "Robo_otros", "Posesion_drogas",  
                                         "Comercio_drogas", "Lesiones", "Hom_culposo", "Hom_doloso", "Portacion_armas", 
                                         "Violencia_fam", "Danio_prop", "Secuestro", "Violacion_sexual", "Delincuencia_org", 
                                         "Otros_sexuales", "Extorsion", "Privacion_de_libertad","Abuso_de_conf", "Amenazas"))
    return(result)
  }), 
  result3_4 <- lapply(flag_const, function(flag_const){
    
    if(flag_const == "flagrancia_const"){
      
      data_list.df <- data_subset_tipo.df %>%
        filter(flagrancia == 1) %>%
        rename(target = all_of(flag_const))
      
    } else {
      
      data_list.df <- data_subset_tipo.df %>%
        filter(inspeccion == 1) %>%
        rename(target = all_of(flag_const))
      
    }
    
    result3_4 <- prueba_hip(seccion = "Detenciones", 
                            subseccion = "Tipo",  
                            hypo_name = paste0("hyp_detenciones_",flag_const), 
                            type = "means", 
                            database = data_list.df, 
                            dep_var = target, 
                            indep_var = Sexo, 
                            group_vars = c("Corporacion_grupos", "Estado", "Robo_vehiculo", "Robo_casa_hab", "Robo_negocio",  
                                           "Robo_transporte_pub", "Robo_transeunte", "Robo_autopartes", "Robo_otros", "Posesion_drogas",  
                                           "Comercio_drogas", "Lesiones", "Hom_culposo", "Hom_doloso", "Portacion_armas", "Incum_asis_fam",
                                           "Violencia_fam", "Danio_prop", "Secuestro", "Violacion_sexual", "Delincuencia_org", 
                                           "Otros_sexuales", "Extorsion", "Privacion_de_libertad","Abuso_de_conf", "Amenazas"))
    return(result3_4)
  }), 
  result3_4 <- lapply(flag_const, function(flag_const){
    
    if(flag_const == "flagrancia_const"){
      
      data_list.df <- data_subset_tipo.df %>%
        filter(flagrancia == 1) %>%
        rename(target = all_of(flag_const))
      
    } else {
      
      data_list.df <- data_subset_tipo.df %>%
        filter(inspeccion == 1) %>%
        rename(target = all_of(flag_const))
      
    }
    
    result3_4 <- prueba_hip(seccion = "Detenciones", 
                            subseccion = "Tipo",  
                            hypo_name = paste0("hyp_detenciones_",flag_const), 
                            type = "means", 
                            database = data_list.df, 
                            dep_var = target, 
                            indep_var = Sexo, 
                            group_vars = c("Corporacion_grupos", "Estado", "Robo_vehiculo", "Robo_casa_hab", "Robo_negocio",  
                                           "Robo_transporte_pub", "Robo_transeunte", "Robo_autopartes", "Robo_otros", "Posesion_drogas",  
                                           "Comercio_drogas", "Lesiones", "Hom_culposo", "Hom_doloso", "Portacion_armas", "Incum_asis_fam",
                                           "Violencia_fam", "Danio_prop", "Secuestro", "Violacion_sexual", "Delincuencia_org", 
                                           "Otros_sexuales", "Extorsion", "Privacion_de_libertad","Abuso_de_conf", "Amenazas"))
    return(result3_4)
  })
  
  
)

infographic_data <- hyp_detenciones_event()

tipo_list <- list(result_list, tipo_flag_const, infographic_data)

### Tortura ----

# Create the database to be used

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
    one_year_limit     = if_else(months_since_RND_3 <= 12 & months_since_RND_3 >= -12, 1, 0)
    ) %>%
  select(tortura_tra_p, tortura_tra_f, RND_1, RND_2, RND_3, Estado, Sexo, fuero, Corporacion_grupos, one_year_limit) 

dependent_vars <- c("tortura_tra_p", "tortura_tra_f")

list_tortura <- lapply(dependent_vars,
                       function(dependent_vars){
                       
                       var_type <- if_else(dependent_vars == "tortura_tra_p", "tortura_psicologica_traslado", 
                                           if_else(dependent_vars == "tortura_tra_f","tortura_fisica_traslado",NA_character_))
                       
                       data_list.df <- data_subset_tortura.df %>%
                         rename(target = all_of(dependent_vars))
                       
                       result1 <- prueba_hip(seccion = "Detenciones", 
                                             subseccion = "Tortura", 
                                             hypo_name = paste0("hyp_",var_type,"_RND_+-1_año"), 
                                             type = "means", 
                                             database = data_list.df %>% filter(one_year_limit == 1), 
                                             dep_var = target, 
                                             indep_var = RND_3, 
                                             group_vars = c("Estado", "Sexo", "fuero", "Corporacion_grupos"))  
                       
                       result2 <- prueba_hip(seccion = "Detenciones", 
                                             subseccion = "Tortura", 
                                             hypo_name = paste0("hyp_",var_type,"_sin_limite"), 
                                             type = "means", 
                                             database = data_list.df, 
                                             dep_var = target, 
                                             indep_var = RND_3, 
                                             group_vars = c("Estado", "Sexo", "fuero", "Corporacion_grupos"))
                       
                       return(list(result1, result2))
         
       })


### Abuso policial ----

data_subset_policia.df <- Main_database %>% 
  filter(NSJP == 1) %>% 
  mutate(
    Estado = case_when(P3_3 == "98" ~ NA_character_,
                       P3_3 == "99" ~ NA_character_,
                       T ~ P3_3),
    Sexo = case_when(SEXO.x == "1" ~ "Masculino",
                     SEXO.x == "2" ~ "Femenino",
                     T ~ NA_character_)) %>%
  select(tortura, flagrancia, LGBTQ, Estado) 

abuso <- c("tortura", 
           "flagrancia")

list_abuso <- lapply(abuso, function(abuso) {

  data_list.df <- data_subset_policia.df %>%
    rename(target = all_of(abuso))
  
  result1 <- prueba_hip(seccion = "Detenciones", 
                        subseccion = "Abuso_policial_lgtbq", 
                        hypo_name = paste0("hyp_", abuso), 
                        type = "means", 
                        database = data_list.df, 
                        dep_var = target, 
                        indep_var = LGBTQ, 
                        group_vars = c("Estado"))
  }
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. MP                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Jueces                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### PPO ----

data_subset_ppo.df <- Main_database %>%
  filter(NSJP == 1) %>% 
  select(percepcion_persona_juzgadora, percepcion_trato_justo_juez, satisfecho_abogado_juicio, sentencia_justa, 
         proceso_no_en_libertad, PPO, culpabilidad)

ppo <- c("percepcion_persona_juzgadora", 
         "percepcion_trato_justo_juez", 
         "satisfecho_abogado_juicio", 
         "sentencia_justa") # Please insert here your dependent var related to this subsection

independent_process_vars <- c("PPO", 
                              "proceso_no_en_libertad", 
                              "culpabilidad") # Please include here your independent var related to this subsection

list_ppo <- lapply(ppo, function(ppo) {
  lapply(independent_process_vars, function(independent_vars) {
    
    var_type <- if_else(independent_vars == "PPO", "PPO", 
                        if_else(independent_vars == "proceso_no_en_libertad","no_en_libertad","culpabilidad"))
    
    data_list.df <- data_subset_ppo.df %>%
      rename(target      = all_of(ppo),
             independent = all_of(independent_vars))
    
    result <- prueba_hip(seccion = "Jueces", 
                         subseccion = "PPO", 
                         hypo_name = paste0("hyp_", ppo, "_", var_type),
                         type = "means", 
                         database = data_list.df, 
                         dep_var = target, 
                         indep_var = independent, 
                         group_vars = NA)
    
    return(result)
    })
}
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Policía                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tipo <- c("proporcionalidad_uso_fuerza") # List of dependent variables from the first analysis

independent_time_vars <- c("months_since_NSJP", 
                           "years_since_NSJP")    # List of independent time variables

result_list <- lapply(tipo, function(tipo) {
  lapply(independent_time_vars, function(independent_vars) {
    
    var_type <- if_else(independent_vars == "months_since_NSJP", "months", "years")
    
    data_list.df <- data_subset_tipo.df %>%
      rename(target = all_of(tipo),
             independent = all_of(independent_vars))
    
    result <- prueba_hip(
      seccion = "Detenciones",
      subseccion = "Policia",
      hypo_name = paste0("hyp_detenciones_", tipo, "_", var_type),
      type = "logit",
      database = data_list.df,
      dep_var = target,
      indep_var = independent,
      group_vars = c("Corporacion_grupos", "Sexo", "LGBTQ")
    )
    
    return(result)
  })
})

