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

prueba_hip <- function(seccion,subseccion,hypo_name,type,database,dep_var,indep_var,group_vars) {
  
  
  nogroups = is.na(group_vars)
  
  
  # Function for logit
  
  if (type == "logit") {
    
    logit = glm(substitute(dep_var ~ indep_var), database, family = "binomial")
    
    margins = summary(margins(logit)) %>% 
      mutate(Significativo10=if_else(p<0.1,"Sí", "No" ),
             Significativo5=if_else(p<0.05,"Sí", "No" ),
             Significativo1=if_else(p<0.01,"Sí", "No" ))
    
    nobs = nobs(logit)  
    
    # Saving results as data frame
    
    df = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                 ~Significativo10, ~Significativo5, ~Significativo1, ~IC_inferior, ~IC_superior, ~Observaciones, ~Interpretación,
                 hypo_name, paste0(substitute(dep_var)), paste0(substitute(indep_var)), "-", 
                 margins$AME, "Logit",margins$Significativo10, margins$Significativo5, margins$Significativo1, margins$lower,
                 margins$upper,nobs,"")
    
    # Writing the excel file
    
    write.xlsx(as.data.frame(df), 
               file      = file.path(paste0(path2SP,
                                            "/National/Hypothesis/Output/",
                                            seccion,"/",
                                            subseccion,"/",
                                            hypo_name,".xlsx"),
                                     fsep = "/"),  
               sheetName = "Resultados Generales",
               append    = T,
               row.names = F)
    
    # In case there are subgroups for which we need to test the hypothesis, continue creating the tabs, 
    # if not, stop
    
    if (any(nogroups) == FALSE) {
      
      # Regression with all the variables
      
      logit_full = glm(as.formula(paste(paste(deparse(substitute(dep_var)), deparse(substitute(indep_var)),
                                              sep = " ~ "), paste(paste0(group_vars,":",deparse(substitute(indep_var))), collapse = " + "),
                                        paste(group_vars, collapse = " + "), sep = " + ")),
                       database, family = "binomial")
      
      
      modelo_full = summary(logit_full)$coefficients %>% 
        data.frame() %>%
        as_tibble(rownames = NA) %>% 
        rownames_to_column() %>%
        mutate(Significativo10=if_else(Pr...z..<0.1,"Sí", "No" ),
               Significativo5=if_else(Pr...z..<0.05,"Sí", "No" ),
               Significativo1=if_else(Pr...z..<0.01,"Sí", "No" ))
      
      margins_full = summary(margins(logit_full)) %>% 
        tibble() %>%
        mutate(Significativo10=if_else(p<0.1,"Sí", "No" ),
               Significativo5=if_else(p<0.05,"Sí", "No" ),
               Significativo1=if_else(p<0.01,"Sí", "No" ))
      
      nobs_full = nobs(logit_full)  
      
      # Saving results as data frame
      
      df_full1 = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                         ~Significativo10, ~Significativo5, ~Significativo1, ~Observaciones, ~Interpretación)
      
      for (j in seq_along(modelo_full$Estimate)) { 
        
        df_full1_j = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~b, ~Método, 
                             ~Significativo10, ~Significativo5, ~Significativo1, ~Observaciones, ~Interpretación,
                             hypo_name, paste0(substitute(dep_var)), paste0(modelo_full$rowname[j]), "-",
                             modelo_full$Estimate[j], "Logit", modelo_full$Significativo10[j], modelo_full$Significativo5[j], 
                             modelo_full$Significativo1[j], nobs_full,"")
        
        df_full1 = rbind(df_full1,df_full1_j)
        
      }
      
      df_full2 = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                         ~Significativo10, ~Significativo5, ~Significativo1, ~IC_inferior, ~IC_superior, ~Observaciones, ~Interpretación)
      
      
      for (j in seq_along(margins_full$factor)) { 
        
        df_full2_j = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                             ~Significativo10, ~Significativo5, ~Significativo1, ~IC_inferior, ~IC_superior, ~Observaciones, ~Interpretación,
                             hypo_name, paste0(substitute(dep_var)), paste0(margins_full$factor[j]), "-",
                             margins_full$AME[j], "Logit", margins_full$Significativo10[j], margins_full$Significativo5[j], margins_full$Significativo1[j],
                             margins_full$lower[j], margins_full$upper[j], nobs_full, "")
        
        df_full2 = rbind(df_full2,df_full2_j)
      }
      
      
      # Writing the excel files for marginal effects and full model 
      
      write.xlsx(as.data.frame(df_full1), 
                 file      = file.path(paste0(path2SP,
                                              "/National/Hypothesis/Output/",
                                              seccion,"/",
                                              subseccion,"/",
                                              hypo_name,".xlsx"),
                                       fsep = "/"),  
                 sheetName = "Resultados Generales (modelo full)",
                 append    = T,
                 row.names = F)
      
      write.xlsx(as.data.frame(df_full2), 
                 file      = file.path(paste0(path2SP,
                                              "/National/Hypothesis/Output/",
                                              seccion,"/",
                                              subseccion,"/",
                                              hypo_name,".xlsx"),
                                       fsep = "/"),  
                 sheetName = "Resultados Generales (efectos mg full)",
                 append    = T,
                 row.names = F)
      
      
      for (i in group_vars) {
        
        groups_i = database %>% pull(i) %>% unique() %>% sort()
        
        rm(df)
        df = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                     ~Significativo10, ~Significativo5, ~Significativo1, ~IC_inferior, ~IC_superior, ~Observaciones, ~Interpretación)
        
        for (j in groups_i) {
          
          database_j = database %>% filter(!!sym(i) == j)
          
          if (nrow(database_j) == 0 | is.na(j) == TRUE | j == "NA") {
            
            next
            
          } else {
            
            logit_j = glm(substitute(dep_var ~ indep_var), data = database_j, family = "binomial")
            
            margins_j = summary(margins(logit_j)) %>% 
              mutate(Significativo10=if_else(p<0.1,"Sí", "No" ),
                     Significativo5=if_else(p<0.05,"Sí", "No" ),
                     Significativo1=if_else(p<0.01,"Sí", "No" ))
            
            nobs_j = nobs(logit_j)  
            
            # Saving results as data frame
            
            df_j = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                           ~Significativo10, ~Significativo5, ~Significativo1, ~IC_inferior, ~IC_superior, ~Observaciones, ~Interpretación,
                           paste0(hypo_name,"_",i), paste0(substitute(dep_var)), paste0(substitute(indep_var)),
                           paste0(j), margins_j$AME, "Logit", margins_j$Significativo10, margins_j$Significativo5, 
                           margins_j$Significativo1, margins_j$lower, margins_j$upper, nobs_j, "")
            
            df = bind_rows(df,df_j)
            
          }
        }
        
        # Writing the excel file
        
        write.xlsx(as.data.frame(df), 
                   file      = file.path(paste0(path2SP,
                                                "/National/Hypothesis/Output/",
                                                seccion,"/",
                                                subseccion,"/",
                                                hypo_name,".xlsx"),
                                         fsep = "/"),
                   sheetName = paste0("Por ",i),
                   append    = T,
                   row.names = F)
      }
    }
  } else if (type == "ols") {
    
    #Function for ols
    
    ols = lm(substitute(dep_var ~ indep_var), database)
    
    margins = summary(margins(ols)) %>% 
      mutate(Significativo10=if_else(p<0.1,"Sí", "No" ),
             Significativo5=if_else(p<0.05,"Sí", "No" ),
             Significativo1=if_else(p<0.01,"Sí", "No" ))
    
    nobs =nobs(ols)
    
    # Saving results as data frame
    
    df = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                 ~Significativo10, ~Significativo5, ~Significativo1, ~IC_inferior, ~IC_superior, ~Observaciones, ~Interpretación,
                 hypo_name, paste0(substitute(dep_var)), paste0(substitute(indep_var)), "-", 
                 margins$AME, "Regresión OLS", margins$Significativo10, margins$Significativo5, margins$Significativo1,
                 margins$lower, margins$upper, nobs, "")
    
    # Writing the excel file
    
    write.xlsx(as.data.frame(df), 
               file      = file.path(paste0(path2SP, 
                                            "/National/Hypothesis/Output/",
                                            seccion,"/",
                                            subseccion,"/",
                                            hypo_name,".xlsx"),
                                     fsep = "/"),
               sheetName = "Resultados Generales",
               append    = T,
               row.names = F)
    
    # In case there are subgroups for which we need to test the hypothesis, continue creating the tabs, 
    # if not, stop
    
    if (any(nogroups == FALSE))   {
      
      
      
      # Regression with all the vartiables
      
      ols_full = lm(as.formula(paste(paste(deparse(substitute(dep_var)), deparse(substitute(indep_var)), sep = " ~ "), paste(group_vars, collapse = " + "),
                                     paste(paste0(group_vars,":",deparse(substitute(indep_var))), collapse = " + "), sep =" + ")),
                    database)
      
      modelo_full = summary(ols_full)$coefficients %>% 
        data.frame() %>%
        as_tibble(rownames = NA) %>% 
        rownames_to_column() %>%
        mutate(Significativo10=if_else(Pr...z..<0.1,"Sí", "No" ),
               Significativo5=if_else(Pr...z..<0.05,"Sí", "No" ),
               Significativo1=if_else(Pr...z..<0.01,"Sí", "No" ))
      
      margins_full = summary(margins(ols_full)) %>% 
        tibble() %>%
        mutate(Significativo10=if_else(p<0.1,"Sí", "No" ),
               Significativo5=if_else(p<0.05,"Sí", "No" ),
               Significativo1=if_else(p<0.01,"Sí", "No" ))
      
      nobs_full = nobs(ols_full)
      
      # Saving results as data frame
      
      df_full1 = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                         ~Significativo10, ~Significativo5, ~Significativo1, ~Observaciones, ~Interpretación)
      
      for (j in seq_along(modelo_full$Estimate)) { 
        
        df_full1_j = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~b, ~Método, 
                             ~Significativo10, ~Significativo5, ~Significativo1, ~Observaciones, ~Interpretación,
                             hypo_name, paste0(substitute(dep_var)), paste0(modelo_full$rowname[j]), "-",
                             modelo_full$Estimate[j], "Regresión OLS", modelo_full$Significativo10[j], modelo_full$Significativo5[j],
                             modelo_full$Significativo1[j], nobs_full, "")
        
        df_full1 = rbind(df_full1,df_full1_j)
        
      }
      
      df_full2 = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                         ~Significativo10, ~Significativo5, ~Significativo1, ~IC_inferior, ~IC_superior, ~Observaciones, ~Interpretación)
      
      
      for (j in seq_along(margins_full$factor)) { 
        
        df_full2_j = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                             ~Significativo10, ~Significativo5, ~Significativo1, ~IC_inferior, ~IC_superior, ~Observaciones, ~Interpretación,
                             hypo_name, paste0(substitute(dep_var)), paste0(margins_full$factor[j]), "-",
                             margins_full$AME[j], "Regresión OLS", margins_full$Significativo10[j], margins_full$Significativo5[j], 
                             margins_full$Significativo1[j], margins_full$lower[j], margins_full$upper[j], nobs_full, "")
        
        df_full2 = rbind(df_full2,df_full2_j)
      }
      
      # Writing the excel files for marginal effects and full model 
      
      write.xlsx(as.data.frame(df_full1), 
                 file      = file.path(paste0(path2SP,
                                              "/National/Hypothesis/Output/",
                                              seccion,"/",
                                              subseccion,"/",
                                              hypo_name,".xlsx"),
                                       fsep = "/"),  
                 sheetName = "Resultados Generales (modelo completo)",
                 append    = T,
                 row.names = F)
      
      write.xlsx(as.data.frame(df_full2), 
                 file      = file.path(paste0(path2SP,
                                              "/National/Hypothesis/Output/",
                                              seccion,"/",
                                              subseccion,"/",
                                              hypo_name,".xlsx"),
                                       fsep = "/"),  
                 sheetName = "Resultados Generales (efectos mg completo)",
                 append    = T,
                 row.names = F)
      
      for (i in group_vars) {
        
        groups_i = database %>% pull(i) %>% unique() %>% sort()
        
        rm(df)
        df = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                     ~Significativo10, ~Significativo5, ~Significativo1, ~IC_inferior, ~IC_superior, ~Observaciones, ~Interpretación)
        
        for (j in groups_i) {
          
          database_j = database %>% filter(!!sym(i) == j)
          
          if (nrow(database_j) == 0 | is.na(j) == TRUE | j == "NA") {
            
            next
            
          } else {
            
            ols_j = lm(substitute(dep_var ~ indep_var), data = database_j)
            
            margins_j = summary(margins(ols_j)) %>% 
              mutate(Significativo10=if_else(p<0.1,"Sí", "No" ),
                     Significativo5=if_else(p<0.05,"Sí", "No" ),
                     Significativo1=if_else(p<0.01,"Sí", "No" ))
            
            nobs_j = nobs(ols_j)
            
            # Saving results as data frame
            
            df_j = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Grupo, ~EMP_indep_var, ~Método, 
                           ~Significativo10, ~Significativo5, ~Significativo1, ~IC_inferior, ~IC_superior, ~Observaciones, ~Interpretación,
                           paste0(hypo_name,"_",i), paste0(substitute(dep_var)), paste0(substitute(indep_var)),
                           paste0(j), margins_j$AME, "Regresión OLS", margins_j$Significativo10, margins_j$Significativo5, 
                           margins_j$Significativo1, margins_j$lower, margins_j$upper, nobs_j, "")
            
            df = bind_rows(df,df_j)
            
          }
        }
        
        # Writing the excel file
        
        write.xlsx(as.data.frame(df), 
                   file      = file.path(paste0(path2SP,
                                                "/National/Hypothesis/Output/",
                                                seccion,"/",
                                                subseccion,"/",
                                                hypo_name,".xlsx"),
                                         fsep = "/"),
                   sheetName = paste0("Por ",i),
                   append    = T,
                   row.names = F)
      }
    }
    
    # Running test for difference in means
    
  } else if (type == "means") {
    
    indep_groups = database %>% pull({{indep_var}}) %>% unique() %>% sort()
    
    ttest = t.test(database %>% filter({{indep_var}} == indep_groups[1]) %>% pull({{dep_var}}), 
                   database %>% filter({{indep_var}} == indep_groups[2]) %>% pull({{dep_var}}), 
                   var.equal = F)
    
    ttest_sig = tibble(c(ttest$p.value)) %>% 
      mutate(Significativo10 = if_else(ttest$p.value < 0.1,"Sí", "No" ),
             Significativo5 = if_else(ttest$p.value< 0.05,"Sí", "No" ),
             Significativo1 = if_else(ttest$p.value< 0.01,"Sí", "No" ))
    
    n1 = length(database %>% filter({{indep_var}} == indep_groups[1], !is.na({{dep_var}})) %>% pull({{dep_var}}))
    n2 = length(database %>% filter({{indep_var}} == indep_groups[2], !is.na({{dep_var}})) %>% pull({{dep_var}}))
    
    # Saving results as data frame
    
    
    df = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~'Grupo 1', 
                 ~'Media Grupo 1', ~'Observaciones Grupo 1', ~'Grupo 2', ~'Media Grupo 2', ~'Observaciones Grupo 2',
                 ~Método, ~Significativo10, ~Significativo5, ~Significativo1, ~Interpretación,
                 hypo_name, paste0(substitute(dep_var)), paste0(substitute(indep_var)), 
                 paste0(substitute(indep_var)," = ",indep_groups[1]), ttest$estimate[1], n1,
                 paste0(substitute(indep_var)," = ",indep_groups[2]), ttest$estimate[2], n2,
                 "Diferencia de medias", ttest_sig$Significativo10, ttest_sig$Significativo5, 
                 ttest_sig$Significativo1,"")
    
    # Writing the excel file
    
    write.xlsx(as.data.frame(df), 
               file      = file.path(paste0(path2SP,
                                            "/National/Hypothesis/Output/",
                                            seccion,"/",
                                            subseccion,"/",
                                            hypo_name,".xlsx"),
                                     fsep = "/"),  
               sheetName = "Resultados Generales",
               append    = T,
               row.names = F)
    
    # In case there are subgroups for which we need to test the hypothesis, continue creating the tabs, 
    # if not, stop
    
    if (any(nogroups == FALSE))   {
      
      
      for (i in group_vars) {
        
        groups_i = database %>% pull(i) %>% unique() %>% sort()
        
        rm(df)
        
        df = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Corte, ~'Grupo 1', 
                     ~'Media Grupo 1', ~'Observaciones Grupo 1', ~'Grupo 2',  ~'Media Grupo 2', ~'Observaciones Grupo 2',
                     ~Método, ~Significativo10, ~Significativo5, ~Significativo1, ~Interpretación)
        
        for (j in groups_i) {
          
          database_j = database %>% filter(!!sym(i) == j)
          
          print(i)
          print(j)
          
          if (nrow(filter(database_j %>% filter({{indep_var}} == indep_groups[1]))) <= 3 |
              nrow(filter(database_j %>% filter({{indep_var}} == indep_groups[2]))) <= 3 | 
              is.na(j) == TRUE | j == "NA") {
            
            next
            
          } else {
            
            ttest_j = t.test(database_j %>% filter({{indep_var}} == indep_groups[1]) %>% pull({{dep_var}}), 
                             database_j %>% filter({{indep_var}} == indep_groups[2]) %>% pull({{dep_var}}), 
                             var.equal = F)
            
            ttest_sig_j = tibble(c(ttest_j$p.value)) %>%
              mutate(Significativo10=if_else(ttest_j$p.value < 0.1,"Sí", "No" ),
                     Significativo5=if_else(ttest_j$p.value < 0.05,"Sí", "No" ),
                     Significativo1=if_else(ttest_j$p.value < 0.01,"Sí", "No" ))
            
            
            n1_j = length(database_j %>% filter({{indep_var}} == indep_groups[1], !is.na({{dep_var}})) %>% pull({{dep_var}}))
            n2_j = length(database_j %>% filter({{indep_var}} == indep_groups[2], !is.na({{dep_var}})) %>% pull({{dep_var}}))
            
            # Saving results as data frame
            
            df_j = tribble(~Hip, ~Variable_Dependiente, ~Variable_Independiente, ~Corte, ~'Grupo 1', 
                           ~'Media Grupo 1', ~'Observaciones Grupo 1', ~'Grupo 2', ~'Media Grupo 2', ~'Observaciones Grupo 2',
                           ~Método, ~Significativo10, ~Significativo5, ~Significativo1, ~Interpretación,
                           hypo_name, paste0(substitute(dep_var)), paste0(substitute(indep_var)), 
                           paste0(i," = ",j),
                           paste0(substitute(indep_var)," = ",indep_groups[1]), ttest_j$estimate[1], n1_j,
                           paste0(substitute(indep_var)," = ",indep_groups[2]), ttest_j$estimate[2], n2_j,
                           "Diferencia de medias", ttest_sig_j$Significativo10, ttest_sig_j$Significativo5, 
                           ttest_sig_j$Significativo1,"")
            
            df = bind_rows(df,df_j)
            
          }
        }
        
        # Writing the excel file
        
        write.xlsx(as.data.frame(df), 
                   file      = file.path(paste0(path2SP,
                                                "/National/Hypothesis/Output/",
                                                seccion,"/",
                                                subseccion,"/",
                                                hypo_name,".xlsx"),
                                         fsep = "/"),
                   sheetName = paste0("Por ",i),
                   append    = T,
                   row.names = F)
      }
    }
    
  } else {
    
    stop("Select a valid type: \"logit\", \"ols\", or \"means\".")
    
  }
  
  return(df)
}

