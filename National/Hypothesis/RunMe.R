args = commandArgs(trailingOnly=TRUE)
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
source("Code/settings.R")
source("Code/pruebas_hip.R")
source("Code/event_study.R")
#source("National/Hypothesis/Code/hyp_individual_detencion_tipo.R")

load(paste0(path2SP,"/National/Data_cleaning/Output/Main_database.RData")) 

# Remove previous files

section     <- args[1]
cat(section)
subsection  <- args[2]
cat(subsection)

if(section == "All") {
  
  outPaths <- c(paste0(path2SP,"/National/Hypothesis/Output/"))

} else if(section == "Detenciones"){
  
  outPathsSection <- c(paste0(path2SP,"/National/Hypothesis/Output/Detenciones/"))
  
  if(subsection == "Tipo") {
    
    outPaths <- c(paste0(outPathsSection,"Tipo/"))
    
  } else if (subsection == "Tortura") {
    
    outPaths <- c(paste0(outPathsSection,"Tortura/"))
                  
  } else if (subsection == "Policia") {
    
    outPaths <- c(paste0(outPathsSection,"Policia/"))
    
  } else {
    
    outPaths <- c(paste0(outPathsSection,"Abuso_policial_lgtbq/"))
                  
  }
  
} else {
  "Nothing"
}

#Listing previous outputs
prevOutputs <- list.files(outPaths,
                          include.dirs = F,
                          full.names   = T,
                          recursive    = T)

# Deleting previous outputs and remove objects used for this cleaning process
nombre_archivos <- sub(".*/([^/]+\\.xlsx)$", "\\1", prevOutputs)
cat(paste0("El siguiente archivo fue eliminado: ", nombre_archivos))

file.remove(prevOutputs)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Hypothesis                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if(section %in% c("Detenciones", "All")) {

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1. Detenciones                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Tipo ----
  if (subsection %in% c("Tipo", "All")) {
    
    # Create the database to be used
    
    data_subset_tipo.df <- Main_database %>% 
      filter(NSJP == 1) %>% 
      mutate(Estado = case_when(P3_3 == "98" ~ NA_character_,
                                P3_3 == "99" ~ NA_character_,
                                T ~ P3_3),
             Corporacion_grupos = case_when(Corporacion_grupos == "NS/NR" ~ NA_character_,
                                            T ~ Corporacion_grupos)) %>%
      select(orden_det, inspeccion, flagrancia, flagrancia_const, det_ninguna, detencion_no_inmediata, 
             months_since_NSJP, years_since_NSJP, Corporacion_grupos, Estado, Sexo, starts_with("Del_"),
             Traslados_30 = P3_20_01, Traslados_6h = P3_20_06, LGBTQ, Edad, Traslado_MP = P3_19_01, 
             proporcionalidad_uso_fuerza, Delito_unico)
    
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
          rename(target_var = all_of(tipo),
                 AA_main_independent_var = all_of(independent_vars))
        
        result <- prueba_hip(
          seccion = "Detenciones",
          subseccion = "Tipo",
          hypo_name = paste0("hyp_detenciones_", tipo, "_", var_type),
          type = "logit",
          database = data_list.df,
          dep_var = target_var,
          indep_var = AA_main_independent_var,
          group_vars = c("Corporacion_grupos", "Estado", "Sexo", "Delito_unico",
                         "Del_Robo_vehiculo", "Del_Robo_casa_hab", "Del_Robo_negocio",  
                         "Del_Robo_transporte_pub", "Del_Robo_transeunte", "Del_Robo_autopartes", "Del_Robo_otros", 
                         "Del_Posesion_drogas", "Del_Comercio_drogas", "Del_Lesiones", "Del_Hom_culposo", "Del_Hom_doloso", 
                         "Del_Portacion_armas", "Del_Incum_asis_fam", "Del_Violencia_fam", "Del_Danio_prop", "Del_Secuestro", 
                         "Del_Violacion_sexual", "Del_Delincuencia_org", "Del_Otros_sexuales", "Del_Extorsion", 
                         "Del_Privacion_de_libertad","Del_Abuso_de_conf", "Del_Amenazas")
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
          rename(target_var = all_of(tipo),
                 AA_main_independent_var = all_of(independent_vars))
        
        result <- prueba_hip(
          seccion = "Detenciones",
          subseccion = "Tipo",
          hypo_name = paste0("hyp_detenciones_", tipo, "_", var_type),
          type = "logit",
          database = data_list.df,
          dep_var = target_var,
          indep_var = AA_main_independent_var,
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
          rename(AA_main_independent_var = all_of(independent_vars))
        
        var_type <- if_else(independent_vars == "months_since_NSJP", "months", "years")
        
        result   <- prueba_hip(seccion = "Detenciones", 
                               subseccion = "Tipo", 
                               hypo_name = paste0("hyp_detenciones_flagrancia_const_", var_type),
                               type = "logit", 
                               database = data_list.df, 
                               dep_var = flagrancia_const, 
                               indep_var = AA_main_independent_var,
                               group_vars = c("Corporacion_grupos", "Estado", "Sexo", "Delito_unico",
                                              "Del_Robo_vehiculo", "Del_Robo_casa_hab", "Del_Robo_negocio",  
                                              "Del_Robo_transporte_pub", "Del_Robo_transeunte", "Del_Robo_autopartes", "Del_Robo_otros", 
                                              "Del_Posesion_drogas", "Del_Comercio_drogas", "Del_Lesiones", "Del_Hom_culposo", "Del_Hom_doloso", 
                                              "Del_Portacion_armas", "Del_Incum_asis_fam", "Del_Violencia_fam", "Del_Danio_prop", "Del_Secuestro", 
                                              "Del_Violacion_sexual", "Del_Delincuencia_org", "Del_Otros_sexuales", "Del_Extorsion", 
                                              "Del_Privacion_de_libertad","Del_Abuso_de_conf", "Del_Amenazas"))
        return(result)
      }), 
      result3_4 <- lapply(flag_const, function(flag_const){
        
        if(flag_const == "flagrancia_const"){
          
          data_list.df <- data_subset_tipo.df %>%
            filter(flagrancia == 1) %>%
            rename(AA_target_var = all_of(flag_const))
          
        } else {
          
          data_list.df <- data_subset_tipo.df %>%
            filter(inspeccion == 1) %>%
            rename(AA_target_var = all_of(flag_const))
          
        }
        
        result3_4 <- prueba_hip(seccion = "Detenciones", 
                                subseccion = "Tipo",  
                                hypo_name = paste0("hyp_detenciones_",flag_const), 
                                type = "means", 
                                database = data_list.df, 
                                dep_var = AA_target_var, 
                                indep_var = Sexo, 
                                group_vars = c("Corporacion_grupos", "Estado", "Sexo", "Delito_unico",
                                               "Del_Robo_vehiculo", "Del_Robo_casa_hab", "Del_Robo_negocio",  
                                               "Del_Robo_transporte_pub", "Del_Robo_transeunte", "Del_Robo_autopartes", "Del_Robo_otros", 
                                               "Del_Posesion_drogas", "Del_Comercio_drogas", "Del_Lesiones", "Del_Hom_culposo", "Del_Hom_doloso", 
                                               "Del_Portacion_armas", "Del_Incum_asis_fam", "Del_Violencia_fam", "Del_Danio_prop", "Del_Secuestro", 
                                               "Del_Violacion_sexual", "Del_Delincuencia_org", "Del_Otros_sexuales", "Del_Extorsion", 
                                               "Del_Privacion_de_libertad","Del_Abuso_de_conf", "Del_Amenazas"))
        return(result3_4)
      }), 
      result3_4 <- lapply(flag_const, function(flag_const){
        
        if(flag_const == "flagrancia_const"){
          
          data_list.df <- data_subset_tipo.df %>%
            filter(flagrancia == 1) %>%
            rename(AA_target_var = all_of(flag_const))
          
        } else {
          
          data_list.df <- data_subset_tipo.df %>%
            filter(inspeccion == 1) %>%
            rename(AA_target_var = all_of(flag_const))
          
        }
        
        result3_4 <- prueba_hip(seccion = "Detenciones", 
                                subseccion = "Tipo",  
                                hypo_name = paste0("hyp_detenciones_",flag_const), 
                                type = "means", 
                                database = data_list.df, 
                                dep_var = AA_target_var, 
                                indep_var = Sexo, 
                                group_vars = c("Corporacion_grupos", "Estado", "Sexo", "Delito_unico",
                                               "Del_Robo_vehiculo", "Del_Robo_casa_hab", "Del_Robo_negocio",  
                                               "Del_Robo_transporte_pub", "Del_Robo_transeunte", "Del_Robo_autopartes", "Del_Robo_otros", 
                                               "Del_Posesion_drogas", "Del_Comercio_drogas", "Del_Lesiones", "Del_Hom_culposo", "Del_Hom_doloso", 
                                               "Del_Portacion_armas", "Del_Incum_asis_fam", "Del_Violencia_fam", "Del_Danio_prop", "Del_Secuestro", 
                                               "Del_Violacion_sexual", "Del_Delincuencia_org", "Del_Otros_sexuales", "Del_Extorsion", 
                                               "Del_Privacion_de_libertad","Del_Abuso_de_conf", "Del_Amenazas"))
        return(result3_4)
      })
      
      
    )
    
    event_study_tipo <- event_study(data.df = data_subset_tipo.df, 
                                    var_analysis = c("flagrancia", 
                                                     "orden_det", 
                                                     "inspeccion", 
                                                     "det_ninguna"),
                                    var_groups = c("National","Estado", "Corporacion_grupos", "Sexo"),
                                    section = "Detenciones",
                                    subsection = "Tipo",
                                    name = "hyp_detenciones_event")
    
    event_study_traslados <- event_study(data.df = data_subset_tipo.df, 
                                         var_analysis = c("Traslados_30", 
                                                          "Traslados_6h"),
                                         var_groups = c("National","Estado", "Corporacion_grupos", "Sexo"),
                                         section = "Detenciones",
                                         subsection = "Tipo",
                                         name = "hyp_traslados_event")
    
    
  } else if (subsection %in% c("Tortura", "All")) {
    
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
        one_year_limit     = if_else(months_since_RND_3 <= 12 & months_since_RND_3 >= -12, 1, 0, 0)
      ) %>%
      select(tortura_tra_p, tortura_tra_f, months_since_RND_3, Estado, Sexo, fuero, Corporacion_grupos, one_year_limit, RND_3) 
    
    dependent_vars <- c("tortura_tra_p", "tortura_tra_f")
    
    list_tortura <- lapply(dependent_vars,
                           function(dependent_vars){
                             
                             var_type <- if_else(dependent_vars == "tortura_tra_p", "tortura_psicologica_traslado", 
                                                 if_else(dependent_vars == "tortura_tra_f","tortura_fisica_traslado",NA_character_))
                             
                             data_list.df <- data_subset_tortura.df %>%
                               rename(AA_target_var = all_of(dependent_vars))
                             
                             result1 <- prueba_hip(seccion = "Detenciones", 
                                                   subseccion = "Tortura", 
                                                   hypo_name = paste0("hyp_",var_type,"_RND_+-1_año"), 
                                                   type = "means", 
                                                   database = data_list.df %>% filter(one_year_limit == 1), 
                                                   dep_var = AA_target_var, 
                                                   indep_var = months_since_RND_3, 
                                                   group_vars = c("Estado", "Sexo", "fuero", "Corporacion_grupos"))  
                             
                             # result2 <- prueba_hip(seccion = "Detenciones",
                             #                       subseccion = "Tortura",
                             #                       hypo_name = paste0("hyp_",var_type,"_sin_limite"),
                             #                       type = "means",
                             #                       database = data_list.df,
                             #                       dep_var = AA_target_var,
                             #                       indep_var = months_since_RND_3,
                             #                       group_vars = c("Estado", "Sexo", "fuero", "Corporacion_grupos"))
                             
                             return(list(result1))
                             
                           })
    
    ### Policia ----
    
  } else if (subsection %in% c("Policia", "All")) {
    
    tipo <- c("proporcionalidad_uso_fuerza") # List of dependent variables from the first analysis
    
    
    independent_time_vars <- c("months_since_NSJP", 
                               "years_since_NSJP") 
    
    data_subset_tipo.df <- Main_database %>% 
      filter(NSJP == 1) %>% 
      mutate(Estado = case_when(P3_3 == "98" ~ NA_character_,
                                P3_3 == "99" ~ NA_character_,
                                T ~ P3_3),
             Corporacion_grupos = case_when(Corporacion_grupos == "NS/NR" ~ NA_character_,
                                            T ~ Corporacion_grupos)) %>%
      select(orden_det, inspeccion, flagrancia, flagrancia_const, det_ninguna, detencion_no_inmediata, 
             months_since_NSJP, years_since_NSJP, Corporacion_grupos, Estado, Sexo, starts_with("Del_"),
             Traslados_30 = P3_20_01, Traslados_6h = P3_20_06, LGBTQ, Edad, Traslado_MP = P3_19_01, 
             proporcionalidad_uso_fuerza, Delito_unico)
    
    result_list_policia <- lapply(tipo, function(tipo) {
      
      lapply(independent_time_vars, function(independent_vars) {
        
        var_type <- if_else(independent_vars == "months_since_NSJP", "months", "years")
        
        data_list.df <- data_subset_tipo.df %>%
          rename(target_var = all_of(tipo),
                 AA_main_independent_var = all_of(independent_vars))
        
        result <- prueba_hip(
          seccion = "Detenciones",
          subseccion = "Policia",
          hypo_name = paste0("hyp_detenciones_", tipo, "_", var_type),
          type = "logit",
          database = data_list.df,
          dep_var = target_var,
          indep_var = AA_main_independent_var,
          group_vars = c("Corporacion_grupos", "Sexo", "LGBTQ")
        )
        
        return(result)
      })
    })
    
    } else {
    
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
          rename(AA_target_var = all_of(abuso))
        
        result1 <- prueba_hip(seccion = "Detenciones", 
                              subseccion = "Abuso_policial_lgtbq", 
                              hypo_name = paste0("hyp_", abuso), 
                              type = "means", 
                              database = data_list.df, 
                              dep_var = AA_target_var, 
                              indep_var = LGBTQ, 
                              group_vars = c("Estado"))
      })  
      
  }

} else if(section %in% c("MP", "All")) {

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. MP                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

} else {


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Jueces                                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  if(subsection %in% c("PPO", "All")) {
    
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
                 AA_main_independent_var = all_of(independent_vars))
        
        result <- prueba_hip(seccion = "Jueces", 
                             subseccion = "PPO", 
                             hypo_name = paste0("hyp_", ppo, "_", var_type),
                             type = "means", 
                             database = data_list.df, 
                             dep_var = target, 
                             indep_var = AA_main_independent_var, 
                             group_vars = NA)
        
        return(result)
      })
    }
    )
    
  } else {
    
    
  }

}
cat(paste0("Se terminó de producir los archivos para la sección: ", section, "/", subsection))