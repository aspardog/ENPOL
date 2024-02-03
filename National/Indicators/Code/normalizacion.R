## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Re-orienting, normalizing, aggregating                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

normalizingvars<- function(vars){
  
  oriented <- vars
  
  for(i in vars){
    
    oriented[[i]]<- if_else(oriented[[i]] %in% c(6,8,9), NA_real_, oriented[[i]])
    
  }
  
  ro<- c()
  
  ro2<- c()
  
  gppro<- intersect(vars, ro)
  gppro2<- intersect(vars, ro2)
  
  for(i in gppro){
    
    oriented[[i]]<- if_else(oriented[[i]] == 1, 4, 
                           if_else(oriented[[i]] == 2, 3, 
                                  if_else(oriented[[i]] == 3, 2, 
                                         if_else(oriented[[i]] == 4, 1, NA_real_))))
  }
  
  for(i in gppro2){
    
    oriented[[i]]<- ifelse(oriented[[i]] == 1, 2, 
                           ifelse(oriented[[i]] == 2, 1, NA_real_))
  }
  
  ## 1.4 Normalize indicators ==================================================================================
  cols_oriented <- names(oriented)[2:length(oriented)]
  max_values <- lapply(cols_oriented, function(col_name){
    
    codebook.df %>% 
      filter(Variable %in% col_name) %>%
      mutate(escala = 
               case_when(
                 Scale == "Escala 2" ~ 2,
                 Scale == "Escala 3" ~ 3,
                 Scale == "Escala 4" ~ 4,
               )) %>%
      pull(escala)
  })
  
  oriented[nrow(oriented) + 1,] <- c("maxs", max_values)
  oriented[nrow(oriented) + 1,] <- c("mins", rep(list(1), ncol(oriented)-1))
  
  
  process    <- preProcess(oriented, method = c("range"))
  normalized <- predict(process, oriented)
  
  normalized <- slice(normalized, 1:(n() - 2))
  
  return(normalized)
  
}
