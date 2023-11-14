## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Descriptivos 
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    D. Cristina Álvarez Venzor     (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 22th, 2023
##
## This version:      Octubre 30th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Load Settings                                                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("National/Data_cleaning/Code/settings.R")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Remove previous files                                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


Detenciones <- paste0(path2DB,"/National/Descriptives/Output/Detenciones/Detenciones")
LGBTQ <- paste0(path2DB,"/National/Descriptives/Output/Detenciones/LGBTQ")
Policia <- paste0(path2DB,"/National/Descriptives/Output/Detenciones/Policia")
Tortura <- paste0(path2DB,"/National/Descriptives/Output/Detenciones/Tortura")
PPO <- paste0(path2DB,"/National/Descriptives/Output/Detenciones/PPO")
Inspecciones <- paste0(path2DB,"/National/Descriptives/Output/Detenciones/Inspecciones")

carpetas <- c(Detenciones, LGBTQ, Policia, Tortura)

# Función para eliminar todos los archivos en una carpeta
limpiar_carpeta <- function(carpeta) {
  archivos <- list.files(path = carpeta, full.names = TRUE)
  print(archivos)
  for (archivo in archivos) {
    file.remove(archivo)
  }}

# Recorre la lista de carpetas y elimina los archivos de cada una
for (carpeta in carpetas) {
  limpiar_carpeta(carpeta)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Function to create the table and write the excel file                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



tabla_excel_fn <- function(dataset,var_prop,var1,var2,var3,varfilter,filtervalue,carpeta,seccion,nombre,Dato){
  
  if (is.na(varfilter) == T) {
    
    # Without filter variable
    
    if (is.na(var3) == T) {
      
      if (is.na(var2) == T) {
        
        if (is.na(var1) == T) {
          
          # No cross-cut (only works for var_prop variables that take a value of 0 and 1)
          
          df <-
            dataset %>% 
            mutate(vara = 1) %>%
            group_by(vara) %>%
            summarise(Prop = mean({{var_prop}}, na.rm = T)) %>%
            cbind(Dato,.) %>%
            select(-vara)
          
        } else {
          
          # 1 variable
          
          df <-
            dataset %>% 
            group_by(.data[[var1]]) %>%
            summarise(Prop = mean({{var_prop}}, na.rm = T)) %>%
            pivot_wider(names_from = .data[[var1]], values_from = Prop) %>%
            cbind(Dato,.)
          if (var1 == "Sexo"){
            df <-  df %>% 
              mutate(Gap = df$Masculino - df$Femenino)}
          
        }
        
      } else {
        
        # 2 variables
        
        variables <- c(var1,var2)
        
        df<-
          dataset %>% 
          group_by(across(variables)) %>%
          summarise(Prop = mean({{var_prop}}, na.rm = T)) %>%
          pivot_wider(names_from = .data[[var1]], values_from = Prop) %>%
          cbind(Dato,.) %>% 
          arrange(by_group = .data[[var2]])
        
      } 
      
    } else {
      
      # 3 variables
      
      variables <- c(var2,var3)
      
      groups_i = subset %>% pull(.data[[var1]]) %>% unique() %>% sort()
      
      df = tibble()
      
      for(i in groups_i) {
        
        df_i <-
          dataset %>% 
          filter(.data[[var1]] == i) %>%
          group_by(across(variables)) %>%
          summarise(Prop = mean({{var_prop}}, na.rm = T)) %>%
          mutate('{var1}' := i) %>%
          arrange(.data[[var2]],.data[[var3]]) %>%
          pivot_wider(names_from = .data[[var2]], values_from = Prop)
        
        if (nrow(df_i) == 0){
          df_i = tibble()
        } else{
          df_i %<>% 
            cbind(Dato,.) %>% 
            arrange(by_group = .data[[var3]]) 
        }
        
        df = bind_rows(df,df_i) %>%
          select(Dato, .data[[var1]], .data[[var3]], sort(colnames(.)))
        
      }
    }
    
  } else {
    
    # With filter variable
    
    if (is.na(var3) == T) {
      
      if (is.na(var2) == T) {
        
        if (is.na(var1) == T) {
          
          # No cross-cut  (only works for var_prop variables that take a value of 0 and 1)
          
          df <-
            dataset %>% 
            filter(.data[[varfilter]] == filtervalue) %>%
            mutate(vara = 1) %>%
            group_by(vara) %>%
            summarise(Prop = mean({{var_prop}}, na.rm = T)) %>%
            cbind(Dato,.)  %>%
            select(-vara)
          
        } else {
          
          # 1 variable
          
          df <-
            dataset %>% 
            filter(.data[[varfilter]]  == filtervalue) %>%
            group_by(.data[[var1]]) %>%
            summarise(Prop = mean({{var_prop}}, na.rm = T)) %>%
            pivot_wider(names_from = .data[[var1]], values_from = Prop) %>%
            cbind(Dato,.)
          if (var1 == "Sexo"){
            df <-  df %>% 
              mutate(Gap = df$Masculino - df$Femenino)}
        }
        
      } else {
        
        # 2 variables
        
        variables <- c(var1,var2)
        
        df<-
          dataset %>% 
          filter(.data[[varfilter]]  == filtervalue) %>%
          group_by(across(variables)) %>%
          summarise(Prop = mean({{var_prop}}, na.rm = T)) %>%
          pivot_wider(names_from = .data[[var1]], values_from = Prop) %>%
          cbind(Dato,.) %>% 
          arrange(by_group = .data[[var2]])
      } 
      
    } else {
      
      # 3 variables
      
      variables <- c(var2,var3)
      
      groups_i = subset %>% pull(.data[[var1]]) %>% unique() %>% sort()
      
      df = tibble()
      
      for(i in groups_i){
        
        df_i <-
          dataset  %>% 
          filter(.data[[varfilter]]  == filtervalue) %>%
          filter(.data[[var1]] == i) %>%
          group_by(across(variables)) %>%
          summarise(Prop = mean({{var_prop}}, na.rm = T)) %>%
          mutate('{var1}' := i) %>%
          arrange(.data[[var2]],.data[[var3]]) %>%
          pivot_wider(names_from = .data[[var2]], values_from = Prop) 
        
        
        if (nrow(df_i)==0){
          df_i = tibble()
        } else{
          df_i %<>% 
            cbind(Dato,.) %>% 
            arrange(by_group = .data[[var3]]) 
        }
        
        df = bind_rows(df,df_i) %>%
          select(Dato, .data[[var1]], .data[[var3]], sort(colnames(.)))
        
        
      }
      
    }
  }
  
  write.xlsx(as.data.frame(df), 
             file      = file.path(paste0(path2DB,
                                          "/National/Descriptives/Output/Detenciones/", 
                                          carpeta,"/desc_", seccion,".xlsx"),
                                   fsep = "/"),  
             sheetName = paste0(nombre),
             append    = T,
             row.names = F)
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Load Database and prepare data                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load(paste0(path2DB,"/National/Data_cleaning/Output/Main_database.RData")) 


delitos <- c("Robo_vehiculo",
             "Robo_casa_hab",
             "Robo_negocio",
             "Robo_transporte_pub",
             "Robo_transeunte",
             "Robo_autopartes",
             "Robo_otros",
             "Posesion_drogas",
             "Comercio_drogas",
             "Lesiones",
             "Hom_culposo",
             "Hom_doloso",
             "Portacion_armas",
             "Incum_asis_fam",
             "Violencia_fam",
             "Danio_prop",
             "Secuestro",
             "Violacion_sexual",
             "Fraude",
             "Delincuencia_org",
             "Otros_sexuales",
             "Extorsion",
             "Privacion_de_libertad",
             "Abuso_de_conf",
             "Amenazas",
             "Otros",
             "No_sabe",
             "No_responde")

delito_grupos <- c("Delito_gr_1_robos",
                   "Delito_gr_2_drogas",
                   "Delito_gr_3_del_org",
                   "Delito_gr_4_lesiones",
                   "Delito_gr_5_hom_cul",
                   "Delito_gr_6_hom_dol",
                   "Delito_gr_7_armas",
                   "Delito_gr_8_viol_fam",
                   "Delito_gr_9_secuestro",
                   "Delito_gr_10_sexuales",
                   "Delito_gr_11_extorsion",
                   "Delito_gr_12_fraude",
                   "Delito_gr_13_amenazas",
                   "Delito_gr_14_otro",
                   "Delito_gr_15_ns_nr")

delito_unico_grupos <- c("Delito_unico_1_robos",
                         "Delito_unico_2_drogas",
                         "Delito_unico_3_del_org",
                         "Delito_unico_4_lesiones",
                         "Delito_unico_5_hom_cul",
                         "Delito_unico_6_hom_dol",
                         "Delito_unico_7_armas",
                         "Delito_unico_8_viol_fam",
                         "Delito_unico_9_secuestro",
                         "Delito_unico_10_sexuales",
                         "Delito_unico_11_extorsion",
                         "Delito_unico_12_fraude",
                         "Delito_unico_13_amenazas",
                         "Delito_unico_14_otro",
                         "Delito_unico_15_ns_nr")


subset <- Main_database %>%
  rename(Estado_arresto = P3_6,
         Anio_arresto = P3_5_A,
         Mes_arresto = P3_5_M) %>%
  
  # Tipo de delito
  mutate(Robo_vehiculo = coalesce(P5_11_01, P5_31_01),
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
  
  # Tipo de tortura psicológica
  mutate(across(c("P3_17_01","P3_17_02", "P3_17_03", "P3_17_04", "P3_17_05", "P3_17_06","P3_17_07", "P3_17_08",
                  "P3_17_09", "P3_17_10","P3_17_11"), ~ recode(.x,
                                                               "1" = 1, 
                                                               "2" = 0, 
                                                               "8" = NA_real_, 
                                                               "9" = NA_real_))) %>%
  
  # Tipo de tortura física
  mutate(across(c("P3_18_01", "P3_18_02", "P3_18_03", "P3_18_04", "P3_18_05", "P3_18_06", "P3_18_07", "P3_18_08", "P3_18_09", 
                  "P3_18_10", "P3_18_12", "P3_18_13", "P3_18_14", "P3_18_15"), ~ recode(.x, 
                                                                                        "1" = 1,
                                                                                        "2" = 0,
                                                                                        "8" = NA_real_,
                                                                                        "9" = NA_real_))) %>%
  
  # Grupos de P5_25
  mutate(Antes_del_juicio = case_when(P5_25 == "1" ~ 1,
                                      P5_25 == "2" 
                                      | P5_25 == "3" 
                                      | P5_25 == "8" 
                                      | P5_25 == "9" ~ 0 ,
                                      T ~ NA),
         Despues_de_las_pruebas = case_when(P5_25 == "2" ~ 1,
                                            P5_25 == "1" 
                                            | P5_25 == "3" 
                                            | P5_25 == "8" 
                                            | P5_25 == "9" ~ 0 ,
                                            T ~ NA),
         Nunca_vio_al_juez = case_when(P5_25 == "3" ~ 1,
                                       P5_25 == "1" 
                                       | P5_25 == "2" 
                                       | P5_25 == "8" 
                                       | P5_25 == "9" ~ 0 ,
                                       T ~ NA),
         
         
         # RND-related
         pre_RND = case_when(fecha_RND_fed <= fecha_arresto ~ 0,
                             fecha_RND_fed > fecha_arresto ~ 1,
                             T ~ NA),
         inter_RND = case_when(fecha_RND_fed < fecha_arresto & fecha_arresto <= fecha_RND_com ~ 1,
                               fecha_RND_fed >= fecha_arresto ~ 0,
                               fecha_RND_com < fecha_arresto ~ 0,
                               T ~ NA),
         post_RND = case_when(fecha_RND_com < fecha_arresto ~ 1,
                              fecha_RND_com >= fecha_arresto ~ 0,
                              T ~ NA),
         solo_fuero_f = case_when(fuero == "Sólo federal" ~ 1,
                                  fuero == "Sólo común" ~ 0,
                                  fuero == "Algunos delitos de fuero común y algunos de fuero federal" ~ 0,
                                  T ~ NA),
         solo_fuero_c = case_when(fuero == "Sólo federal" ~ 0,
                                  fuero == "Sólo común" ~ 1,
                                  fuero == "Algunos delitos de fuero común y algunos de fuero federal" ~ 0,
                                  T ~ NA),
         ambos_fueros = case_when(fuero == "Sólo federal" ~ 0,
                                  fuero == "Sólo común" ~ 0,
                                  fuero == "Algunos delitos de fuero común y algunos de fuero federal" ~ 1,
                                  T ~ NA),
         Post_2008 = case_when(as.numeric(Anio_arresto) >= 2008 ~ 1,
                               T ~ NA_real_),
         
         # Detencion no inmediata
         detencion_no_inmediata = case_when(P3_9 == "1" ~ 0,
                                            P3_9 == "2" | P3_9 == "3"| P3_9 == "4"| P3_9 == "5"| P3_9 == "6" ~ 1,
                                            T ~ NA),
         
         # Grupos de P3_16
         P3_16_1 = case_when(P3_16 == "1" ~ 1,
                             P3_16 == "2" | P3_16 == "8" | P3_16 == "9" ~ 0,
                             T ~ NA),
         P3_16_2 = case_when(P3_16 == "2" ~ 1,
                             P3_16 == "1" | P3_16 == "8" | P3_16 == "9" ~ 0,
                             T ~ NA),
         P3_16_8 = case_when(P3_16 == "8" ~ 1,
                             P3_16 == "1" | P3_16 == "2" | P3_16 == "9" ~ 0,
                             T ~ NA),
         P3_16_9 = case_when(P3_16 == "9" ~ 1,
                             P3_16 == "1" | P3_16 == "2" | P3_16 == "8" ~ 0,
                             T ~ NA),

         # Grupos de P3_21
         P3_21_1_1 = case_when(P3_21_1 == "1" ~ 1,
                               is.na(P3_21_1) == TRUE ~ NA,
                               T ~ 0),
         P3_21_1_2 = case_when(P3_21_1 == "2" ~ 1,
                               is.na(P3_21_1) == TRUE ~ NA,
                               T ~ 0),
         P3_21_1_8 = case_when(P3_21_1 == "8" ~ 1,
                               is.na(P3_21_1) == TRUE ~ NA,
                               T ~ 0),
         P3_21_1_9 = case_when(P3_21_1 == "9" ~ 1,
                               is.na(P3_21_1) == TRUE ~ NA,
                               T ~ 0)) %>%
  
  mutate(
    # Eventos de inspeccion
    across(c("P3_12_1","P3_12_2", "P3_12_3", "P3_12_4", "P3_12_5"), ~ recode(.x,
                                                                             "1" = 1,
                                                                             "2" = 0,
                                                                             "8" = NA_real_,
                                                                             "9" = NA_real_))) %>%
  mutate(
    # Eventos de detención
    across(c("P3_13_01","P3_13_02", "P3_13_03", "P3_13_04", "P3_13_05","P3_13_06","P3_13_07",
             "P3_13_08","P3_13_09","P3_13_10","P3_13_11","P3_13_12", "P3_14_1", "P3_14_2",
             "P3_14_3", "P3_14_4", "P3_14_5", "P3_14_6", "P3_15_1", "P3_15_2", "P3_15_3",
             "P3_15_4", "P3_15_5", "P3_15_6", "P3_15_7", "P3_15_8", "P3_15_9"), ~ recode(.x,
                                                                                         "1" = 1,
                                                                                         "2" = 0,
                                                                                         "8" = NA_real_,
                                                                                         "9" = NA_real_)))  %>% 
  filter(Post_2008 == 1)  %>% 
  
  rowwise() %>% mutate(Delito_unico = ifelse(sum(c(Delito_gr_1_robos,
                                                   Delito_gr_2_drogas,
                                                   Delito_gr_3_del_org,
                                                   Delito_gr_4_lesiones,
                                                   Delito_gr_5_hom_cul,
                                                   Delito_gr_6_hom_dol,
                                                   Delito_gr_7_armas, 
                                                   Delito_gr_8_viol_fam,
                                                   Delito_gr_9_secuestro,
                                                   Delito_gr_10_sexuales,
                                                   Delito_gr_11_extorsion,
                                                   Delito_gr_12_fraude,
                                                   Delito_gr_13_amenazas,
                                                   Delito_gr_14_otro,
                                                   Delito_gr_15_ns_nr)) <= 1, 1, 0)) %>% 
  
  mutate(Delito_unico_1_robos = case_when(Delito_gr_1_robos == 1 
                                          & Delito_unico == 1 ~ 1,
                                          T ~ 0),
         Delito_unico_2_drogas = case_when(Delito_gr_2_drogas == 1 
                                           & Delito_unico == 1 ~ 1,
                                           T ~ 0),
         Delito_unico_3_del_org = case_when(Delito_gr_3_del_org == 1 
                                            & Delito_unico == 1 ~ 1,
                                            T ~ 0),
         Delito_unico_4_lesiones = case_when(Delito_gr_4_lesiones == 1 
                                             & Delito_unico == 1 ~ 1,
                                             T ~ 0),
         Delito_unico_5_hom_cul = case_when(Delito_gr_5_hom_cul == 1 
                                            & Delito_unico == 1 ~ 1,
                                            T ~ 0),
         Delito_unico_6_hom_dol = case_when(Delito_gr_6_hom_dol == 1 
                                            & Delito_unico == 1 ~ 1,
                                            T ~ 0),
         Delito_unico_7_armas = case_when(Delito_gr_7_armas == 1
                                          & Delito_unico == 1 ~ 1,
                                          T ~ 0),
         Delito_unico_8_viol_fam = case_when(Delito_gr_8_viol_fam == 1 
                                             & Delito_unico == 1 ~ 1,
                                             T ~ 0),
         Delito_unico_9_secuestro = case_when(Delito_gr_9_secuestro == 1 
                                              & Delito_unico == 1 ~ 1,
                                              T ~ 0),
         Delito_unico_10_sexuales = case_when(Delito_gr_10_sexuales == 1 
                                              & Delito_unico == 1 ~ 1,
                                              T ~ 0),
         Delito_unico_11_extorsion = case_when(  Delito_gr_11_extorsion == 1 
                                                 & Delito_unico == 1 ~ 1,
                                                 T ~ 0),
         Delito_unico_12_fraude = case_when(  Delito_gr_12_fraude == 1 
                                              & Delito_unico == 1 ~ 1,
                                              T ~ 0),
         Delito_unico_13_amenazas = case_when(  Delito_gr_13_amenazas == 1 
                                                & Delito_unico == 1 ~ 1,
                                                T ~ 0),
         Delito_unico_14_otro = case_when(  Delito_gr_14_otro == 1 
                                            & Delito_unico == 1 ~ 1,
                                            T ~ 0),
         Delito_unico_15_ns_nr = case_when(  Delito_gr_15_ns_nr == 1 
                                             & Delito_unico == 1 ~ 1,
                                             T ~ 0))

rm(Main_database)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Create tables                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 3.1 Detención ---------------------------------------------------------------



#### 3.1.1 Sentenciadas ------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "sentenciadas", nombre = "Nacional", 
               Dato = "Proporción de personas que ya fueron sentenciadas por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "sentenciadas", nombre = "Año", 
               Dato = "Proporción de personas que ya fueron sentenciadas por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "sentenciadas", nombre = "Estado", 
               Dato = "Proporción de personas que ya fueron sentenciadas por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "sentenciadas", nombre = "Año_estado",                
               Dato = "Proporción de personas que ya fueron sentenciadas por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "sentenciadas", nombre = "Sexo",
               Dato = "Proporción de personas que ya fueron sentenciadas por sexo")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "sentenciadas", nombre = "Sexo_año",
               Dato = "Proporción de personas que ya fueron sentenciadas por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Sexo", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "sentenciadas", nombre = "Sexo_estado",
               Dato = "Proporción de personas que ya fueron sentenciadas por sexo y estado de arresto")

#### 3.1.2 Flagrancias ------------------------------------------------------------

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Nacional",
               Dato = "Proporción de personas detenidas en flagrancia")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Año",
               Dato = "Proporción de personas detenidas en flagrancia por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Estado",
               Dato = "Proporción de personas detenidas en flagrancia por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Año_estado",
               Dato = "Proporción de personas detenidas en flagrancia por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Sexo",
               Dato = "Proporción de personas detenidas en flagrancia por sexo")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Sexo_año",
               Dato = "Proporción de personas detenidas en flagrancia por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Sexo", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Sexo_estado",
               Dato = "Proporción de personas detenidas en flagrancia por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Corporación_año",
               Dato = "Proporción de personas detenidas en flagrancia por corporación que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Corporación-grupos",
               Dato = "Proporción de personas detenidas en flagrancia por corporación que detiene")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Corporación-grupos_año",
               Dato = "Proporción de personas detenidas en flagrancia por corporación que detiene y año de arresto (post 2008)")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Edad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Edad",
               Dato = "Proporción de personas detenidas en flagrancia por edad")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Escolaridad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Escolaridad",
               Dato = "Proporción de personas detenidas en flagrancia por escolaridad")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Ingreso", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Nivel_ingreso",
               Dato = "Proporción de personas detenidas en flagrancia por nivel de ingreso")

#### 3.1.3 Orden de aprehensión ------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Nacional",
               Dato = "Proporción de personas detenidas con una orden de detención")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Año",
               Dato = "Proporción de personas detenidas con una orden de detención por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Estado",
               Dato = "Proporción de personas detenidas con una orden de detención por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Año_estado",
               Dato = "Proporción de personas detenidas con una orden de detención por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Sexo",
               Dato = "Proporción de personas detenidas con una orden de detención por sexo")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Sexo_año",
               Dato = "Proporción de personas detenidas con una orden de detención por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Sexo", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Sexo_estado",
               Dato = "Proporción de personas detenidas con una orden de detención por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Corporacion_año",
               Dato = "Proporción de personas detenidas con una orden de detención por corporación que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Corporacion-grupos",
               Dato = "Proporción de personas detenidas con una orden de detención por corporación que detiene")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Corporacion-grupos_año",
               Dato = "Proporción de personas detenidas con una orden de detención por corporación que detiene y año de arresto (Post 2008)")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Edad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Edad",
               Dato = "Proporción de personas detenidas con una orden de detención por edad")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Escolaridad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Escolaridad",
               Dato = "Proporción de personas detenidas con una orden de detención por escolaridad")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Ingreso", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Nivel_ingreso",
               Dato = "Proporción de personas detenidas con una orden de detención por nivel de ingresos")

#### 3.1.3 Después inspección ------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Nacional",
               Dato = "Proporción de personas detenidas con una orden de detención")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Año",
               Dato = "Proporción de personas detenidas con una orden de detención por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Estado",
               Dato = "Proporción de personas detenidas con una orden de detención por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Año_estado",
               Dato = "Proporción de personas detenidas con una orden de detención por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Sexo",
               Dato = "Proporción de personas detenidas con una orden de detención por sexo")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Sexo_año",
               Dato = "Proporción de personas detenidas con una orden de detención por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Sexo", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Sexo_estado",
               Dato = "Proporción de personas detenidas con una orden de detención por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Corporacion_año",
               Dato = "Proporción de personas detenidas con una orden de detención por corporación que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Corporacion-grupos",
               Dato = "Proporción de personas detenidas con una orden de detención por corporación que detiene")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Corporacion-grupos_año",
               Dato = "Proporción de personas detenidas con una orden de detención por corporación que detiene y año de arresto (Post 2008)")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Edad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Edad",
               Dato = "Proporción de personas detenidas con una orden de detención por edad")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Escolaridad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Escolaridad",
               Dato = "Proporción de personas detenidas con una orden de detención por escolaridad")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Ingreso", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Nivel_ingreso",
               Dato = "Proporción de personas detenidas con una orden de detención por nivel de ingresos")

#### 3.1.4 Detención ninguna de las anteriores ------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Nacional",
               Dato = "Proporción de personas detenidas con una orden de detención")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Año",
               Dato = "Proporción de personas detenidas con una orden de detención por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Estado",
               Dato = "Proporción de personas detenidas con una orden de detención por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Año_estado",
               Dato = "Proporción de personas detenidas con una orden de detención por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Sexo",
               Dato = "Proporción de personas detenidas con una orden de detención por sexo")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Sexo_año",
               Dato = "Proporción de personas detenidas con una orden de detención por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Sexo", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Sexo_estado",
               Dato = "Proporción de personas detenidas con una orden de detención por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Corporacion_año",
               Dato = "Proporción de personas detenidas con una orden de detención por corporación que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Corporacion-grupos",
               Dato = "Proporción de personas detenidas con una orden de detención por corporación que detiene")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Corporacion-grupos_año",
               Dato = "Proporción de personas detenidas con una orden de detención por corporación que detiene y año de arresto (Post 2008)")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Edad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Edad",
               Dato = "Proporción de personas detenidas con una orden de detención por edad")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Escolaridad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Escolaridad",
               Dato = "Proporción de personas detenidas con una orden de detención por escolaridad")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Ingreso", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Nivel-ingreso",
               Dato = "Proporción de personas detenidas con una orden de detención por nivel de ingresos")

#### 3.1.5 Fuero ------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = solo_fuero_f, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Fuero", nombre = "Fuero-federal",
               Dato = "Proporción de personas detenidas y acusadas únicamente de delitos de fuero federal")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_c, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Fuero", nombre = "Fuerro-comun",
               Dato = "Proporción de personas detenidas y acusadas únicamente de delitos de fuero común")

tabla_excel_fn(dataset = subset, var_prop = ambos_fueros, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Fuero", nombre = "Fuerro-ambos",
               Dato = "Proporción de personas detenidas y acusadas de delitos de ambos fueros")

tabla_excel_fn(dataset = subset, var_prop = pre_RND, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = "fuero", filtervalue = "Algunos delitos de fuero común y algunos de fuero federal", 
               carpeta = "Detenciones", seccion = "Fuero", nombre = "Fuero-ambos_antes-fed",
               Dato = "Proporción de personas que fueron acusadas de delitos de ambos fueros, detenidas antes de la implementación del RND para delitos de fuero federal")

tabla_excel_fn(dataset = subset, var_prop = inter_RND, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = "fuero", filtervalue = "Algunos delitos de fuero común y algunos de fuero federal", 
               carpeta = "Detenciones", seccion = "Fuero", nombre = "Fuero-ambos_entre-fed",
               Dato = "Proporción de personas que fueron acusadas de delitos de ambos fueros, detenidas entre la implementación del RND para delitos de fuero federal y la implementación para delitos del fuero común")

tabla_excel_fn(dataset = subset, var_prop = post_RND, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = "fuero", filtervalue = "Algunos delitos de fuero común y algunos de fuero federal", 
               carpeta = "Detenciones", seccion = "Fuero", nombre = "Fuero-ambos_desps-comun",
               Dato = "Proporción de personas que fueron acusadas de delitos de ambos fueros, detenidas después de la implementación del RND para delitos de fuero común")

#### 3.1.6 Tipo justificación ------------------------------------------------------------


# P3_10, P3_11 was done manually

table(subset$P3_10,subset$P3_11) %>% prop.table()

# P3_10, P3_11 by authority was done manually

Autoridades <- sort(unique(subset$P3_2))

for (i in Autoridades) {
  subset %>% filter(P3_2 == i) %>% with(table(P3_10,P3_11)) %>% prop.table() %>% print()
}

tabla_excel_fn(dataset = subset, var_prop = flagrancia_P3_10_1, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Flag_autoridad",
               Dato = "Proporción de personas que fueron detenidas en flagrancia (P3_10==1), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = flagrancia_P3_10_2, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Flag_autoridad2",
               Dato = "Proporción de personas que fueron detenidas en flagrancia (P3_10==2), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Orden-det_autoridad",
               Dato = "Proporción de personas que fueron detenidas con orden de detención (P3_10==3), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Inspeccion_autoridad",
               Dato = "Proporción de personas que fueron detenidas después de una inspección (P3_10==4), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Det-ninguna_autoridad",
               Dato = "Proporción de personas que fueron detenidas por ninguna de las anteriores (P3_10==5), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = flagrancia_P3_10_1, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Flag_corporacion",
               Dato = "Proporción de personas que fueron detenidas en flagrancia (P3_10==1), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = flagrancia_P3_10_2, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Flag_corporacion2",
               Dato = "Proporción de personas que fueron detenidas en flagrancia (P3_10==2), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Ord-det_corporacion",
               Dato = "Proporción de personas que fueron detenidas con orden de detención (P3_10==3), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Inspeccion_corporacion",
               Dato = "Proporción de personas que fueron detenidas después de una inspección (P3_10==4), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Det-ninguna_corporacion",
               Dato = "Proporción de personas que fueron detenidas por ninguna de las anteriores (P3_10==5), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = flagrancia_P3_10_1, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Flag_corporacion_año",
               Dato = "Proporción de personas que fueron detenidas en flagrancia (P3_10==1), por autoridad que detiene y año (post 2008)")

tabla_excel_fn(dataset = subset, var_prop = flagrancia_P3_10_2, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Flag_corporacion2_año",
               Dato = "Proporción de personas que fueron detenidas en flagrancia (P3_10==2), por autoridad que detiene y año (post 2008)")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Ord-det_corporacion_año",
               Dato = "Proporción de personas que fueron detenidas con orden de detención (P3_10==3), por autoridad que detiene y año (post 2008)")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Inspeccion_corporacion_año",
               Dato = "Proporción de personas que fueron detenidas después de una inspección (P3_10==4), por autoridad que detiene y año (post 2008)")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "Tipo_justificacion", nombre = "Det-ninguna_corporacion_año",
               Dato = "Proporción de personas que fueron detenidas por ninguna de las anteriores (P3_10==5), por autoridad que detiene y año (post 2008)")

#### 3.1.7 Irregulares ------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_11_1, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "Sacando-lugar_autoridad",
               Dato = "Proporción de personas que fueron detenidas sacándolo del lugar donde se encontraba (P3_11==1), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_11_2, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "Pasando-calle_autoridad",
               Dato = "Proporción de personas que fueron detenidas mientras iba pasando por la calle (P3_11==2), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_11_3, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "Otro_autoridad",
               Dato = "Proporción de personas que fueron detenidas Otro (P3_11==3), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_11_8, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "No-sabe_autoridad",
               Dato = "Proporción de personas que fueron detenidas No sabe (P3_11==8), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_11_9, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "No-responde_autoridad",
               Dato = "Proporción de personas que fueron detenidas No responde (P3_11==9), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_11_1, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "Sacando-lugar_corporacion",
               Dato = "Proporción de personas que fueron detenidas sacándolo del lugar donde se encontraba (P3_11==1), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_11_2, var1 = "Corporacion_grupos", var2 = NA, var3 = NA,
               varfilter = NA, filtervalue = NA,
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "Pasando-calle_corporacion", 
               Dato = "Proporción de personas que fueron detenidas mientras iba pasando por la calle (P3_11==2), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_11_3, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "Otro_corporacion",
               Dato = "Proporción de personas que fueron detenidas Otro (P3_11==3), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_11_8, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "No-sabe_corporacion",
               Dato = "Proporción de personas que fueron detenidas No sabe (P3_11==8), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_11_9, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "No-responde_corporacion",
               Dato = "Proporción de personas que fueron detenidas No responde (P3_11==9), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_11_1, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "Sacando-lugar_corporacion_año",
               Dato = "Proporción de personas que fueron detenidas sacándolo del lugar donde se encontraba (P3_11==1), por autoridad que detiene y año (post 2008)")

tabla_excel_fn(dataset = subset, var_prop = P3_11_2, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "Pasando-calle_corporacion_año",
               Dato = "Proporción de personas que fueron detenidas mientras iba pasando por la calle (P3_11==2), por autoridad que detiene y año (post 2008)")

tabla_excel_fn(dataset = subset, var_prop = P3_11_3, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "Otro_corporacion_año",
               Dato = "Proporción de personas que fueron detenidas Otro (P3_11==3), por autoridad que detiene y año (post 2008)")

tabla_excel_fn(dataset = subset, var_prop = P3_11_8, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "No-sabe_corporacion_año",
               Dato = "Proporción de personas que fueron detenidas No sabe (P3_11==8), por autoridad que detiene y año (post 2008)")

tabla_excel_fn(dataset = subset, var_prop = P3_11_9, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "Irregulares", nombre = "No-responde_corporacion_año",
               Dato = "Proporción de personas que fueron detenidas No responde (P3_11==9), por autoridad que detiene  y año (post 2008)")


#### 3.1.8 Apego de la flagrancia a la constitucionalidad --------------------------


tabla_excel_fn(dataset = subset, var_prop = flagrancia_const, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Flagrancia_constitucional", nombre = "Si_estado",
               Dato = "Proporción de personas detenidas en flagrancia constitucional, por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia_const, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Flagrancia_constitucional", nombre = "Si_autoridad",
               Dato = "Proporción de personas detenidas en flagrancia constitucional, por autoridad que detiene")



### 3.2 Tortura -----------------------------------------------------------------



#### 3.2.1 Prevalencia de tortura --------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "Nacional",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado")

tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "Estado",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado")

tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = "RND_3", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "Aprobacion-RND",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado, por RND_3")

tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = "RND_3", var2 = "fuero", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "Aprobacion_RND_fuero",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado, por RND_3 y fuero")

tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = "RND_3", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "Aprobacion_RND_estado",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado, por RND_3 y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "Sexo",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado, por sexo")

tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "LGBTQ+",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado, por género")

tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = "fuero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "Fuero",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado, por fuero")

tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = "RND_3", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "Aprobacion-RND_corporacion",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado, por RND_3 y corporación que detiene")


#### 3.2.2 Torura psicologica -------------------------------------------------------



tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Nacional",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado")

tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Cargos-falsos",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Amenaza-matarlo",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a)")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Amenaza-daño-usted",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Amenaza-daño-familia",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Otra-amenaza",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Presion-denuncia",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Incomunicaron",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Vueltas",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Hicieron-daño-familia",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Desvistieron",
               Dato = "Proporción de personas que reportaron que Le desvistieron")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Cubrir-ojos",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera")

tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado_genero", nombre = "Cargos-falsos_genero",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado_genero", nombre = "Amenaza-matarlo_genero",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a), por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado_genero", nombre = "Amenaza-daño-usted_genero",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado_genero", nombre = "Amenaza-daño-familia_genero",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado_genero", nombre = "Otra-amenaza_genero",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado_genero", nombre = "Presion-denuncia_genero",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado_genero", nombre = "Incomunicaron_genero",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado_genero", nombre = "Vueltas_genero",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado_genero", nombre = "Hicieron-daño_genero",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado_genero", nombre = "Desvistieron_genero",
               Dato = "Proporción de personas que reportaron que Le desvistieron, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslad_genero", nombre = "Cburieron-ojos_genero",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera , por género")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "RND_3", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica", nombre = "RND",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por RND_3")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "RND_3", var2 = "fuero", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica", nombre = "RND_fuero",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por RND_3 y fuero")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "RND_3", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica", nombre = "RND_estado",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por RND_3 y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "RND_3", var2 = "genero", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica", nombre = "RND_genero",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por RND_3 y genero")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,  
               carpeta = "Tortura", seccion = "Tortura_psicologica", nombre = "Estado",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica", nombre = "Sexo",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por sexo")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica", nombre = "Genero",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por género")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "fuero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica", nombre = "Fuero",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por fuero")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "RND_3", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica", nombre = "RND_corporacion",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por RND_3 y corporación que detiene")

#### 3.2.3 Tortura física ----------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Nacional",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Ataron",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Asfixiaron",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Impidieron-respirar",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Patearon",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Golpearon",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Quemaron",
               Dato = "Proporción de personas que reportaron que Le quemaron..")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Descargas",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Aplastaron",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Hirieron",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Agujas",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Dispararon",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Acoso",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Organos-sexuales",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Actividad-sexual",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Otra",
               Dato = "Proporción de personas que reportaron que Otra agresión física, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Ataron_genero",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Asfixiaron_genero",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Impidieron-respirar_genero",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Patearon_genero",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Golpearon_genero",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Quemaron_genero",
               Dato = "Proporción de personas que reportaron que Le quemaron..")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Descargas_genero",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Aplastaron_genero",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Hirieron_genero",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Agujas_genero",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Dispararon_genero",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Acoso_genero",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Organos-sexuales_genero",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Actividad-sexual_genero",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado_genero", nombre = "Otra_genero",
               Dato = "Proporción de personas que reportaron que Otra agresión física, por género")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = "RND_3", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "RND",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado, por RND_3")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = "RND_3", var2 = "fuero", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "RND_fueron",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado, por RND_3 y fuero")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = "RND_3", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "RND_estado",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado, por RND_3 y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = "RND_3", var2 = "genero", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "RND_genero",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado, por RND_3 y genero")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Estado",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Sexo",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado, por sexo")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Genero",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado, por género")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = "fuero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Fuero",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado, por fuero")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = "RND_3", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "RND_corporacion",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado, por RND_3 y corporación que detiene")






### 3.3 LGBTQ -------------------------------------------------------------------




#### 3.3.1 Abuso policial y LGBTQ+ -------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "LGBTQ", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "LGBTQ", seccion = "Flagrancia-LGBTQ", nombre = "Nacional",
               Dato = "Proporción de personas detenidas en flagrancia, por identificación LGBTQ+")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "LGBTQ", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "LGBTQ", seccion = "Flagrancia-LGBTQ", nombre = "LGBTQ_estado",
               Dato = "Proporción de personas detenidas en flagrancia, por identificación LGBTQ+ y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = "LGBTQ", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "LGBTQ", seccion = "Tortura-LGBTQ", nombre = "Nacional",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado, por identificación LGBTQ+")

tabla_excel_fn(dataset = subset, var_prop = tortura, var1 = "LGBTQ", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "LGBTQ", seccion = "Tortura-LGBTQ", nombre = "LGBTQ_estado",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado, por identificación LGBTQ+ y estado de arresto")






### 3.4 Policía -----------------------------------------------------------------



tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tipo-detencion_año", nombre = "Flagrancia_año",
               Dato = "Proporción de personas detenidas por flagrancia, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion_año", nombre = "Orden-det_año",
               Dato = "Proporción de personas detenidas por con orden de detención, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion_año", nombre = "Inspeccion_año",
               Dato = "Proporción de personas detenidas después de una inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion_año", nombre = "Det-ninguna_año",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia/orden de detención/inspección), por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion_año_estado", nombre = "Flagrancia_año_estado",
               Dato = "Proporción de personas detenidas por flagrancia, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA,
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tipo-detencion_año_estado", nombre = "Orden-det_año_estado", 
               Dato = "Proporción de personas detenidas por con orden de detención, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion_año_estado", nombre = "Inspeccion_año_estado",
               Dato = "Proporción de personas detenidas después de una inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion_año_estado", nombre = "Det-ninguna_año_estado",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia/orden de detención/inspección), por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion_año_estado_sexo", nombre = "Flagrancia_año_estado_sexo",
               Dato = "Proporción de personas detenidas por flagrancia, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion_año_estado_sexo", nombre = "Orden-det_año_estado_sexo",
               Dato = "Proporción de personas detenidas por con orden de detención, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion_año_estado_sexo", nombre = "Inspeccion_año_estado_sexo",
               Dato = "Proporción de personas detenidas después de una inspección, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion_año_estado_sexo", nombre = "Det-ninguna_año_estado_sexo",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia/orden de detención/inspección), por año y estado de arresto y sexo")


#### 3.4.1 Inspecciones ------------------------------------------------------------



for (i in delito_grupos) { 
  tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Inspeccion-nacional", nombre = paste0("Nacional-",i),
                 Dato = paste0("Proporción de personas detenidas por ",i," después de una inspección"))
}

for (i in delitos) { 
  tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Inspeccion-estado", nombre = paste0("Estado-",i),
                 Dato = paste0("Proporción de personas detenidas por ",i,"(entre otros) después de una inspección"))
}

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Inspeccion", nombre = "Nacional",
               Dato = "Proporción de personas detenidas después de una inspección")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Inspeccion", nombre = "Autoridad",
               Dato = "Proporción de personas detenidas después de una inspección, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Estado_arresto", var2 ="P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Inspeccion", nombre = "Estado_autoridad",
               Dato = "Proporción de personas detenidas después de una inspección, por estado y autoridad que detiene")

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Inspeccion_sexo_delito-grupos", nombre = paste0("Sexo",i),
                 Dato = paste0("Proporción de personas detenidas después de una inspección entre las acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1",
                 carpeta = "Policia", seccion = "Inspeccion_sexo_estado", nombre = paste0("Sexo_estado",i),
                 Dato = paste0("Proporción de personas detenidas después de una inspección entre las acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "genero", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Inspeccion_delito_genero", nombre = paste0("Genero",i),
                 Dato = paste0("Proporción de personas detenidas después de una inspección entre las acusadas (entre otros) por ",i," por genero y delito"))
}

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "genero", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Inspeccion_delito-grupo_genero", nombre = paste0("Genero",i),
                 Dato = paste0("Proporción de personas detenidas después de una inspección entre las acusadas (entre otros) por ",i," por genero y grupo de delito"))
}


####3.4.2 Flagrancias -------------------------------------------------------------



tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Flagrancia", nombre = "Nacional",
               Dato = "Proporción de personas detenidas en flagrancia")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Edad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Flagrancia", nombre = "Edad",
               Dato = "Proporción de personas detenidas en flagrancia")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Escolaridad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Flagrancia", nombre = "Escolaridad",
               Dato = "Proporción de personas detenidas en flagrancia")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Flagrancia", nombre = "Autoridad",
               Dato = "Proporción de personas detenidas en flagrancia, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Estado_arresto", var2 ="P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Flagrancia", nombre = "Estado_autoridad",
               Dato = "Proporción de personas detenidas en flagrancia, por estado y autoridad que detiene")


for (i in delito_grupos) { 
  tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Flagrancia_delito-grupo", nombre = paste0("Flagrancia",i),
                 Dato = paste0("Proporción de personas detenidas por ",i," en flagrancia"))
}

for (i in delitos) { 
  tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Flagrancia_delito_estado", nombre = paste0("Estado",i),
                 Dato = paste0("Proporción de personas detenidas por ",i,"(entre otros) en flagrancia, por estado de arresto"))
}


for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Flagrancia_delito-grupo_sexo", nombre = paste0("Sexo",i),
                 Dato = paste0("Proporción de personas detenidas en flagrancia entre las acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Flagrancia_delito_estado_sexo", nombre = paste0("Estado_sexo",i),
                 Dato = paste0("Proporción de personas detenidas en flagrancia entre las acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "genero", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Flagrancia_delito_genero", nombre = paste0("Genero",i),
                 Dato = paste0("Proporción de personas detenidas en flagrancia entre las acusadas (entre otros) por ",i," por género"))
}

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "genero", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Flagrancia_delito-grupo_genero", nombre = paste0("Genero",i),
                 Dato = paste0("Proporción de personas detenidas en flagrancia entre las acusadas (entre otros) por ",i," por género"))
}


#### 3.4.2 Orden de detención -------------------------------------------------------------

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,  
               carpeta = "Policia", seccion = "Orde-det", nombre = "Autoridad",
               Dato = "Proporción de personas detenidas por orden de detención, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Estado_arresto", var2 ="P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Orde-det", nombre = "Estado_autoridad",
               Dato = "Proporción de personas detenidas por orden de detención, por estado y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,  
               carpeta = "Policia", seccion = "Orden-det", nombre = "Nacional",
               Dato = "Proporción de personas detenidas por orden de detención")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Edad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Orde-det", nombre = "Edad",
               Dato = "Proporción de personas detenidas por orden de detención, por edad")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Escolaridad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Orde-det", nombre = "Escolaridad",
               Dato = "Proporción de personas detenidas por orden de detención, por escolaridad")

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Orde-det_delito-grupo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas detenidas por orden de detención entre las acusadas (entre otros) por ",i))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Orde-det_delito_estado", nombre = paste0("Estado",i),
                 Dato = paste0("Proporción de personas detenidas por orden de detención entre las acusadas (entre otros) por ",i," por estado de arresto"))
}


for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Orde-det_delito-grupo_sexo", nombre = paste0("Sexo",i),
                 Dato = paste0("Proporción de personas detenidas por orden de detención entre las acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1",  
                 carpeta = "Policia", seccion = "Orde-det_delito_estado_sexo", nombre = paste0("Estado_sexo",i),
                 Dato = paste0("Proporción de personas detenidas por orden de detención entre las acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}


for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "genero", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Orde-det_delito_genero", nombre = paste0("Genero",i),
                 Dato = paste0("Proporción de personas detenidas por orden de detención entre las acusadas (entre otros) por ",i," por género"))
}

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "genero", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Orde-det_delito-grupo_genero", nombre = paste0("Genero",i),
                 Dato = paste0("Proporción de personas detenidas por orden de detención entre las acusadas (entre otros) por ",i," por género"))
}


#### 3.4.3 Detención ninguna de las anteriores -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Det-ninguna", nombre = "Nacional",
               Dato = "Proporción de personas detenidas por ninguna de las anteriores")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Edad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Det-ninguna", nombre = "Edad",
               Dato = "Proporción de personas detenidas por ninguna de las anteriores, por edad")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Escolaridad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Det-ninguna", nombre = "Escolaridad",
               Dato = "Proporción de personas detenidas por ninguna de las anteriores, por escolaridad")

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1,
                 carpeta = "Policia", seccion = "Det-ninguna_delito-grupo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas detenidas por ninguna de las anteriores entre las acusadas (entre otros) por ",i))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Det-ninguna_delito_estado", nombre = paste0("Estado",i),
                 Dato = paste0("Proporción de personas detenidas por ninguna de las anteriores entre las acusadas (entre otros) por ",i," por estado de arresto"))
}

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Det-ninguna", nombre = "Autoridad",
               Dato = "Proporción de personas detenidas por ninguna de las anteriores, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Estado_arresto", var2 ="P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Det-ninguna", nombre = "Estado_autodiad",
               Dato = "Proporción de personas detenidas por ninguna de las anteriores, por estado y autoridad que detiene")

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Det-ninguna_delito-grupo_sexo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas detenidas por ninguna de las anteriores entre las acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Det-ninguna_delito_estado_sexo", nombre = paste0("Estado_sexo",i),
                 Dato = paste0("Proporción de personas detenidas por ninguna de las anteriores entre las acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}


for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "genero", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Det-ninguna_delito_genero", nombre = paste0("Genero",i),
                 Dato = paste0("Proporción de personas detenidas por ninguna de las anteriores entre las acusadas (entre otros) por ",i," por género"))
}

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "genero", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Det-ninguna_delito-grupo_genero", nombre = paste0("Genero",i),
                 Dato = paste0("Proporción de personas detenidas por ninguna de las anteriores entre las acusadas (entre otros) por ",i," por género"))
}


#### 3.4.4 Detención no inmediata -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,  
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Nacional",
               Dato = "Proporción de personas cuya detención no fue inmediata (P3_9)")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = "flagrancia", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Flagrancia",
               Dato = "Proporción de personas detenidas en flagrancia cuya detención no fue inmediata (P3_9)")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = "orden_det", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Orden-detencion",
               Dato = "Proporción de personas detenidas con orden de detención cuya detención no fue inmediata (P3_9)")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = "inspeccion", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Inspeccion",
               Dato = "Proporción de personas detenidas después de una inspección cuya detención no fue inmediata (P3_9)")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = "det_ninguna", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Ninguna",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia / orden de detención / inspección) cuya detención no fue inmediata (P3_9)")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = "flagrancia", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_estado", nombre = "Flagrancia_estado",
               Dato = "Proporción de personas detenidas en flagrancia cuya detención no fue inmediata (P3_9), por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = "orden_det", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_estado", nombre = "Orden-detencion_estado",
               Dato = "Proporción de personas detenidas con orden de detención cuya detención no fue inmediata (P3_9), por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = "inspeccion", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediat_estadoa", nombre = "Inspeccion_estado",
               Dato = "Proporción de personas detenidas después de una inspección cuya detención no fue inmediata (P3_9), por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = "det_ninguna", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_estado", nombre = "Ninguna_estado",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia / orden de detención / inspección) cuya detención no fue inmediata (P3_9), por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = "flagrancia", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_sexo", nombre = "Flagrancia_sexo",
               Dato = "Proporción de personas detenidas en flagrancia cuya detención no fue inmediata (P3_9), por sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = "orden_det", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_sexo", nombre = "Orden-detencion_sexo",
               Dato = "Proporción de personas detenidas con orden de detención cuya detención no fue inmediata (P3_9), por sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = "inspeccion", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_sexo", nombre = "Inspecion_sexo",
               Dato = "Proporción de personas detenidas después de una inspección cuya detención no fue inmediata (P3_9), por sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = "det_ninguna", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_sexo", nombre = "Ninguna_sexo",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia / orden de detención / inspección) cuya detención no fue inmediata (P3_9), por sexo")


tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = "flagrancia", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_estado_sexo", nombre = "Flagrancia_estado_sexo",
               Dato = "Proporción de personas detenidas en flagrancia cuya detención no fue inmediata (P3_9), por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = "orden_det", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_estado_sexo", nombre = "Orden-det_estado_sexo",
               Dato = "Proporción de personas detenidas con orden de detención cuya detención no fue inmediata (P3_9), por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = "inspeccion", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_estado_sexo", nombre = "Inspeccion_estado_sexo",
               Dato = "Proporción de personas detenidas después de una inspección cuya detención no fue inmediata (P3_9), por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = "det_ninguna", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata_estado_sexo", nombre = "Ninguna_estado_sexo",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia / orden de detención / inspección) cuya detención no fue inmediata (P3_9), por estado de arresto y sexo")


#### 3.4.5 Atoridad acción -------------------------------------------------------------

tabla_excel_fn(dataset = subset, var_prop = P3_13_01, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Detuviera",
               Dato = "Proporción de personas que la autoridad le indicó que se detuviera...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_02, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Sujeto",
               Dato = "Proporción de personas que reportan que la autoridad le sujetó...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_03, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Esposo",
               Dato = "Proporción de personas que reportan que la autoridad le esposó...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_04, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Arma-contundente",
               Dato = "Proporción de personas que reportan que la autoridad utilizó algún arma contundente...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_05, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Arma-noletal",
               Dato = "Proporción de personas que reportan que la autoridad utilizó algún arma no letal...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_06, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Sustancia",
               Dato = "Proporción de personas que reportan que la autoridad utilizó alguna sustancia química...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_07, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Amenaza-arma",
               Dato = "Proporción de personas que reportan que la autoridad le amenazó con un arma de fuego...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_08, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Lesion-menor",
               Dato = "Proporción de personas que reportan que la autoridad le causó alguna lesión menor...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_09, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Lesion-grave",
               Dato = "Proporción de personas que reportan que la autoridad le causó alguna lesión grave...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_10, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Lesion-riesgo",
               Dato = "Proporción de personas que reportan que la autoridad  le causó alguna lesión que pusiera en riesgo su vida...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_11, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Disparo",
               Dato = "Proporción de personas que reportan que la autoridad le disparó...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_12, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_nacional", nombre = "Hirio",
               Dato = "Proporción de personas que reportan que la autoridad le hirió...")

tabla_excel_fn(dataset = subset, var_prop = P3_13_01, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Detuviera_estado",
               Dato = "Proporción de personas que la autoridad le indicó que se detuviera..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_02, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Sujeto_estado", 
               Dato = "Proporción de personas que reportan que la autoridad le sujetó..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_03, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Esposo_estado",
               Dato = "Proporción de personas que reportan que la autoridad le esposó..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_04, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Arma-contudente_estado",
               Dato = "Proporción de personas que reportan que la autoridad utilizó algún arma contundente..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_05, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Arma-noletal_estado",
               Dato = "Proporción de personas que reportan que la autoridad utilizó algún arma no letal..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_06, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Sustancia_estado",
               Dato = "Proporción de personas que reportan que la autoridad utilizó alguna sustancia química..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_07, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Ameanza-arma_estado",
               Dato = "Proporción de personas que reportan que la autoridad le amenazó con un arma de fuego..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_08, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Lesion-menor_estado",
               Dato = "Proporción de personas que reportan que la autoridad le causó alguna lesión menor..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_09, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Lesion-grave_estado",
               Dato = "Proporción de personas que reportan que la autoridad le causó alguna lesión grave..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_10, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Lesion-riesgo_estado",
               Dato = "Proporción de personas que reportan que la autoridad  le causó alguna lesión que pusiera en riesgo su vida..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_11, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Disparo_estado",
               Dato = "Proporción de personas que reportan que la autoridad le disparó..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_12, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_estado", nombre = "Hirio_estado",
               Dato = "Proporción de personas que reportan que la autoridad le hirió..., por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_01, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Detuviera_sexo",
               Dato = "Proporción de personas que la autoridad le indicó que se detuviera..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_02, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Sujeto_sexo",
               Dato = "Proporción de personas que reportan que la autoridad le sujetó..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_03, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Esposo_sexo",
               Dato = "Proporción de personas que reportan que la autoridad le esposó..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_04, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Arma-contundente_sexo",
               Dato = "Proporción de personas que reportan que la autoridad utilizó algún arma contundente..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_05, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Arma-noletal_sexo",
               Dato = "Proporción de personas que reportan que la autoridad utilizó algún arma no letal..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_06, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Sustancia_sexo",
               Dato = "Proporción de personas que reportan que la autoridad utilizó alguna sustancia química..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_07, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Amenaza-arma_sexo",
               Dato = "Proporción de personas que reportan que la autoridad le amenazó con un arma de fuego..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_08, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Lesion-menor_sexo",
               Dato = "Proporción de personas que reportan que la autoridad le causó alguna lesión menor..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_09, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Lesion-grave_sexo",
               Dato = "Proporción de personas que reportan que la autoridad le causó alguna lesión grave..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_10, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Lesion-riesgo_sexo",
               Dato = "Proporción de personas que reportan que la autoridad  le causó alguna lesión que pusiera en riesgo su vida..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_11, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Disparo_sexo",
               Dato = "Proporción de personas que reportan que la autoridad le disparó..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_12, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo", nombre = "Hirio_sexo",
               Dato = "Proporción de personas que reportan que la autoridad le hirió..., por sexo ")

tabla_excel_fn(dataset = subset, var_prop = P3_13_01, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Detuviera_sexo_estado",
               Dato = "Proporción de personas que la autoridad le indicó que se detuviera..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_02, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Sujeto_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad le sujetó..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_03, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Esposo_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad le esposó..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_04, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Arma-contundente_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad utilizó algún arma contundente..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_05, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Arma-noletal_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad utilizó algún arma no letal..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_06, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Sustancia_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad utilizó alguna sustancia química..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_07, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Amenaza-arma_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad le amenazó con un arma de fuego..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_08, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Lesion-menor_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad le causó alguna lesión menor..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_09, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Lesion-grave_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad le causó alguna lesión grave..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_10, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Lesion-riesgo_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad  le causó alguna lesión que pusiera en riesgo su vida..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_11, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Disparo_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad le disparó..., por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_13_12, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Autoridad-accion_sexo_estado", nombre = "Hirio_sexo_estado",
               Dato = "Proporción de personas que reportan que la autoridad le hirió..., por sexo y estado de arresto")

#### 3.4.6 D55 ---------------------------------------------------------------------


for (j in c("P3_13_01", "P3_13_02", "P3_13_03", "P3_13_04", "P3_13_05", "P3_13_06", "P3_13_07", "P3_13_08", "P3_13_09",
            "P3_13_10", "P3_13_11", "P3_13_12")){
  
  df_j = tibble()
  
  for(i in delito_grupos) {
    
    Dato = paste0("Proporción de personas que reportaron \"sí\" a ",j,", acusadas (entre otros) de ",i,", por sexo")
    
    df_i<-
      subset %>% 
      filter(.data[[i]]  == 1) %>%
      group_by(across(c("Estado_arresto","Sexo"))) %>%
      summarise(Prop = mean(.data[[j]] , na.rm = T)) %>%
      pivot_wider(names_from = Estado_arresto, values_from = Prop) %>%
      cbind(Dato,.) %>% 
      arrange(by_group = Sexo)
    
    df_j = bind_rows(df_j,df_i)
    
  }
  
  
  write.xlsx(as.data.frame(df_j), 
             file      = file.path(paste0("National/Descriptives/Output/Detenciones/Policia/Autoridad-accion_estado_sexo_delito-grupos.xlsx"),
                                   fsep = "/"),  
             sheetName = paste0("Estado_sexo",j),
             append    = T,
             row.names = F)
}


#### 3.4.6 D56 ---------------------------------------------------------------------



for (j in c("P3_13_01", "P3_13_02", "P3_13_03", "P3_13_04", "P3_13_05", "P3_13_06", "P3_13_07", "P3_13_08", "P3_13_09",
            "P3_13_10", "P3_13_11", "P3_13_12")){
  
  df_j = tibble()
  
  for(i in delitos) {
    
    Dato = paste0("Proporción de personas que reportaron \"sí\" a ",j,", acusadas (entre otros) de ",i,", por sexo, y estado de arresto")
    
    df_i<-
      subset %>% 
      filter(.data[[i]]  == "1") %>%
      group_by(across(c("Estado_arresto","Sexo"))) %>%
      summarise(Prop = mean(.data[[j]] , na.rm = T)) %>%
      pivot_wider(names_from = Estado_arresto, values_from = Prop) %>%
      cbind(Dato,.) %>% 
      arrange(by_group = Sexo)
    
    df_j = bind_rows(df_j,df_i)
    
  }
  
  
  write.xlsx(as.data.frame(df_j), 
             file      = file.path(paste0("National/Descriptives/Output/Detenciones/Policia/Autoridad-accion_sexo_delito.xlsx"),
                                   fsep = "/"),  
             sheetName = paste0("Delito_estado_sexo",j),
             append    = T,
             row.names = F)
}

#### 3.4.7 D57 -----------------------------------------------------------------



#for (j in c("P3_13_01", "P3_13_02", "P3_13_03", "P3_13_04", "P3_13_05", "P3_13_06", "P3_13_07", "P3_13_08", "P3_13_09",
#            "P3_13_10", "P3_13_11", "P3_13_12")){
  
#  df_j = tibble()
  
#  for(i in delito_grupos) {
    
#    Dato = paste0("Proporción de personas que reportaron \"sí\" a ",j,", acusadas (entre otros) de ",i,", por sexo, y estado de arresto")
    
#    df_i<-
#      subset %>% 
#      filter(.data[[i]]  == 1) %>%
#      group_by(across(c("Estado_arresto","Sexo"))) %>%
#      summarise(Prop = mean(.data[[j]] , na.rm = T)) %>%
#      pivot_wider(names_from = Estado_arresto, values_from = Prop) %>%
#      cbind(Dato,.) %>% 
#      arrange(by_group = Sexo)
#    
#    df_j = bind_rows(df_j,df_i)
    
#  }
  
  
#  write.xlsx(as.data.frame(df_j), 
#             file      = file.path(paste0("National/Descriptives/Output/Detenciones/Policia/Autoridad-accion_estado_sexo_delito-grupos2.xlsx"),
#                                   fsep = "/"),  
#             sheetName = paste0("Estado_delito-grupos_Sexo_2",j),
#             append    = T,
#             row.names = F)
#}



#### 3.4.8 Controles cooperativos  --------




for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = controles_cooperativos, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Controles_cooperativos_delito-grupo_sexo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron controles cooperativos, acusadas (entre otros) por ",i," por sexo"))
}

for (i in delito_unico_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = controles_cooperativos, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Controles_cooperativos_delito-grupo-unico", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas con un único delito que reportaron controles cooperativos, acusadas por ",i," "))
}

for (i in delito_unico_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = controles_cooperativos, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Controles_cooperativos_delito-grupo-unicosexo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas con un único delito que reportaron controles cooperativos, acusadas  por ",i,", por sexo "))
}


for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = controles_cooperativos, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Controles_cooperativos_delito_sexo_estado", nombre = paste0("estado_sexo",i),
                 Dato = paste0("Proporción de personas que reportaron controles cooperativos, acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}




#### 3.4.9 Controles contacto  --------

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = control_contacto, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Controles_contacto_delito-grupo_sexo", nombre = paste0("Sexo_",i),
                 Dato = paste0("Proporción de personas que reportaron controles de contacto, acusadas (entre otros) por ",i," por sexo"))
}

for (i in delito_unico_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = control_contacto, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Controles_cooperativos_delito-grupo-unico", nombre = paste0("",i), 
                 Dato = paste0("Proporción de personas con un único delito que reportaron controles cooperativos, acusadas por ",i," "))
}

for (i in delito_unico_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = control_contacto, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Controles_cooperativos_delito-grupo-unico-sexo", nombre = paste0("",i), 
                 Dato = paste0("Proporción de personas con un único delito que reportaron controles cooperativos, acusadas  por ",i,", por sexo "))
}



for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = control_contacto, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Controles_contacto_delito_sexo_estado", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron controles de contacto, acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}

#### 3.4.10 Controles sometimiento  --------

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = sometimiento, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Controles_sometimiento_delito-grupo_sexo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron técnicas de sometimiento, acusadas (entre otros) por ",i," por sexo"))
}


for (i in delito_unico_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = sometimiento, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1,
                 carpeta = "Policia", seccion = "Controles_sometimiento_delito-grupo-unico", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas con un único delito que reportaron técnicas de sometimiento, acusadas por ",i,""))
}

for (i in delito_unico_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = sometimiento, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1,
                 carpeta = "Policia", seccion = "Controles_sometimiento_delito-grupo-unico_sexo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron técnicas de sometimiento, acusadas (entre otros) por ",i," por sexo"))
}


for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = sometimiento, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Controles_contacto_delito_sexo_estado", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron técnicas de sometimiento, acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}




#### 3.4.11 Tácticas defensivas  --------
for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = tacticas_defensivas, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Tacticas-defensivas_delito-grupo_sexo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron tacticas defensivas, acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = tacticas_defensivas, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Tacticas-defensivas_delito_sexo_estado", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron tacticas defensivas, acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}

for (i in delito_unico_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = tacticas_defensivas, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1,
                 carpeta = "Policia", seccion = "Tacticas-defensivas_delito-grupo-unico", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas con un único delito que reportaron tacticas defensivas, acusadas por ",i,""))
}

for (i in delito_unico_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = tacticas_defensivas, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1,
                 carpeta = "Policia", seccion = "Tacticas-defensivas_delito-grupo-unico_sexo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas con un único delito que reportaron tacticas defensivas, acusadas por ",i," por sexo"))
}

#### 3.4.12 Fuerza Letal --------

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = fuerza_letal, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, 
                 carpeta = "Policia", seccion = "Fuerza-letal_delito-grupo_sexo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron fuerza letal, acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = fuerza_letal, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Policia", seccion = "Fuerza-letal_delito_estado_sexo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron fuerza letal, acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}


for (i in delito_unico_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = fuerza_letal, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1,
                 carpeta = "Policia", seccion = "Fuerza-letal_delito-grupo_sexo", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas que reportaron fuerza letal, acusadas (entre otros) por ",i," por sexo"))
}

for (i in delito_unico_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = fuerza_letal, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1,
                 carpeta = "Policia", seccion = "Fuerza-letal_delito-grupo-unico", nombre = paste0("",i),
                 Dato = paste0("Proporción de personas con un único delito que reportaron fuerza letal, acusadas por ",i,""))
}


#### 3.4.13 Policia hizo -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_nacional", nombre = "Identifico",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó...")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_nacional", nombre = "Informo",
               Dato = "Proporción de personas que reportaron que la autoridad le informó...")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_nacional", nombre = "Uniformado",
               Dato = "Proporción de personas que reportaron que la autoridad estaba uniformado?")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_nacional", nombre = "Dijo-detencion",
               Dato = "Proporción de personas que reportaron que le dijeron por qué lo(a) detuvieron?")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_nacional", nombre = "Informo-derechos",
               Dato = "Proporción de personas que reportaron la autoridad le informó sobre sus derechos...")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_nacional", nombre = "Dijo-donde",
               Dato = "Proporción de personas que reportaron que la autoridad le dijo a dónde...")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año", nombre = "Identifico_año",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año", nombre = "Informo_año",
               Dato = "Proporción de personas que reportaron que la autoridad le informó..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año", nombre = "Uniformado_año",
               Dato = "Proporción de personas que reportaron que la autoridad estaba uniformado?, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año", nombre = "Dijo_año",
               Dato = "Proporción de personas que reportaron que le dijeron por qué lo(a) detuvieron?, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año", nombre = "Informo-derechos_año",
               Dato = "Proporción de personas que reportaron la autoridad le informó sobre sus derechos..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año", nombre = "Dijo-donde_año",
               Dato = "Proporción de personas que reportaron que la autoridad le dijo a dónde..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_estado", nombre = "Identifico_año_estado",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó..., por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_estado", nombre = "Informo_año_estado",
               Dato = "Proporción de personas que reportaron que la autoridad le informó..., por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Policia-hizo_año_estado", nombre = "Uniformado_año_estado",
               Dato = "Proporción de personas que reportaron que la autoridad estaba uniformado?, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_estado", nombre = "Dijo-detencion_año_estado",
               Dato = "Proporción de personas que reportaron que le dijeron por qué lo(a) detuvieron?, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_estado", nombre = "Informo-derechos_año_estado",
               Dato = "Proporción de personas que reportaron la autoridad le informó sobre sus derechos..., por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_estado", nombre = "Dijo-donde_año_estado",
               Dato = "Proporción de personas que reportaron que la autoridad le dijo a dónde..., por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo", nombre = "Identifico_año_sexo",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó..., por sexo, añode arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo", nombre = "Informo_año_sexo",
               Dato = "Proporción de personas que reportaron que la autoridad le informó..., por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo", nombre = "Uniformado_año_sexo",
               Dato = "Proporción de personas que reportaron que la autoridad estaba uniformado?, por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo", nombre = "Dijeron-detencion_año_sexo",
               Dato = "Proporción de personas que reportaron que le dijeron por qué lo(a) detuvieron?, por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo", nombre = "Informo-derecho_año_sexo",
               Dato = "Proporción de personas que reportaron la autoridad le informó sobre sus derechos..., por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo", nombre = "Dijo-donde_año_sexo",
               Dato = "Proporción de personas que reportaron que la autoridad le dijo a dónde..., por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo_estado", nombre = "Identifico_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó..., por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo_estado", nombre = "Informo_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que la autoridad le informó..., por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo_estado", nombre = "Uniformado_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que la autoridad estaba uniformado?, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo_estado", nombre = "Dijo-detencion_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que le dijeron por qué lo(a) detuvieron?, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo_estado", nombre = "Informo-derechos_año_sexo_estado",
               Dato = "Proporción de personas que reportaron la autoridad le informó sobre sus derechos..., por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_sexo_estado", nombre = "Dijo-donde_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que la autoridad le dijo a dónde..., por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad", nombre = "Identifico_autoridad",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Policia-hizo_autoridad", nombre = "Informo_autoridad",
               Dato = "Proporción de personas que reportaron que la autoridad le informó..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Policia-hizo_autoridad", nombre = "Uniformado_autoridad",
               Dato = "Proporción de personas que reportaron que la autoridad estaba uniformado?, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad", nombre = "Dijeron-detencion_autoridad",
               Dato = "Proporción de personas que reportaron que le dijeron por qué lo(a) detuvieron?, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad", nombre = "Informo-derechos_autoridad",
               Dato = "Proporción de personas que reportaron la autoridad le informó sobre sus derechos..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad", nombre = "Dijo-donde_autoridad",
               Dato = "Proporción de personas que reportaron que la autoridad le dijo a dónde..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_estado", nombre = "Identifico_autoridad_estado",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó..., por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_estado", nombre = "Informo_autoridad_estado",
               Dato = "Proporción de personas que reportaron que la autoridad le informó..., por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_estado", nombre = "Uniformado_autoridad_estado",
               Dato = "Proporción de personas que reportaron que la autoridad estaba uniformado?, por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_estado", nombre = "Dijo-detencion_autoridad_estado",
               Dato = "Proporción de personas que reportaron que le dijeron por qué lo(a) detuvieron?, por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_estado", nombre = "Informo-derechos_autoridad_estado",
               Dato = "Proporción de personas que reportaron la autoridad le informó sobre sus derechos..., por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_estado", nombre = "Dijo-donde_autoridad_estado",
               Dato = "Proporción de personas que reportaron que la autoridad le dijo a dónde..., por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año", nombre = "Identifico_autoridad_año",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó..., por autoridad que detiene, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año", nombre = "Informo_autoridad_año",
               Dato = "Proporción de personas que reportaron que la autoridad le informó..., por autoridad que detiene, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año", nombre = "Uniformado_autoridad_año",
               Dato = "Proporción de personas que reportaron que la autoridad estaba uniformado?, por autoridad que detiene, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año", nombre = "Dijo-detencion_autoridad_año",
               Dato = "Proporción de personas que reportaron que le dijeron por qué lo(a) detuvieron?, por autoridad que detiene, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año", nombre = "Informo-derechos_autoridad_año",
               Dato = "Proporción de personas que reportaron la autoridad le informó sobre sus derechos..., por autoridad que detiene, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año", nombre = "Dijo-donde_autoridad_año",
               Dato = "Proporción de personas que reportaron que la autoridad le dijo a dónde..., por autoridad que detiene, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año_estado", nombre = "Identifico_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año_estado", nombre = "Informo_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que la autoridad le informó..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año_estado", nombre = "Uniformado_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que la autoridad estaba uniformado?, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año_estado", nombre = "Dijo-detencion_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que le dijeron por qué lo(a) detuvieron?, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año_estado", nombre = "Informo-derechos_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron la autoridad le informó sobre sus derechos..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_autoridad_año_estado", nombre = "Dijo-donde_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que la autoridad le dijo a dónde..., por autoridad que detiene, año y estado de arresto")

#### 3.4.14 Momento detencion -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_15_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion", nombre = "Obedecer",
               Dato = "Proporción de personas que reportaron obedecer las órdenes...")

tabla_excel_fn(dataset = subset, var_prop = P3_15_2, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion", nombre = "Arma-punzo",
               Dato = "Proporción de personas que portaban algún arma punzo cortante...")

tabla_excel_fn(dataset = subset, var_prop = P3_15_3, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion", nombre = "Arma-fuego",
               Dato = "Proporción de personas que portaban algún arma de fuego...")

tabla_excel_fn(dataset = subset, var_prop = P3_15_4, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion", nombre = "Amenazar",
               Dato = "Proporción de personas que amenazaron a alguien con el arma...")

tabla_excel_fn(dataset = subset, var_prop = P3_15_5, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion", nombre = "Disparar",
               Dato = "Proporción de personas que dispararon el arma...")

tabla_excel_fn(dataset = subset, var_prop = P3_15_6, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion", nombre = "Manipularon",
               Dato = "Proporción de personas que manipularon algún objeto...")

tabla_excel_fn(dataset = subset, var_prop = P3_15_7, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion", nombre = "Sobornar",
               Dato = "Proporción de personas que trataron de sobornar...")

tabla_excel_fn(dataset = subset, var_prop = P3_15_8, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion", nombre = "Defenderse",
               Dato = "Proporción de personas que trataron de defenderse...")

tabla_excel_fn(dataset = subset, var_prop = P3_15_9, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion", nombre = "Escapar",
               Dato = "Proporción de personas que trataron de escapar...")

tabla_excel_fn(dataset = subset, var_prop = P3_15_1, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año", nombre = "Obedecer_año",
               Dato = "Proporción de personas que reportaron obedecer las órdenes..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_2, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año", nombre = "Arma-punzo_año",
               Dato = "Proporción de personas que portaban algún arma punzo cortante..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_3, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año", nombre = "Arma-fuego_año",
               Dato = "Proporción de personas que portaban algún arma de fuego..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_4, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año", nombre = "Amenazaron_año",
               Dato = "Proporción de personas que amenazaron a alguien con el arma..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_5, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año", nombre = "Dispararon_año",
               Dato = "Proporción de personas que dispararon el arma..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_6, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año", nombre = "Manipularon_año",
               Dato = "Proporción de personas que manipularon algún objeto..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_7, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año", nombre = "Sobornar_año",
               Dato = "Proporción de personas que trataron de sobornar..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_8, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año", nombre = "Defenderse_año",
               Dato = "Proporción de personas que trataron de defenderse..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_9, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion_año", nombre = "Escapar_año",
               Dato = "Proporción de personas que trataron de escapar..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_estado", nombre = "Obedecer_año_estado",
               Dato = "Proporción de personas que reportaron obedecer las órdenes..., NA")

tabla_excel_fn(dataset = subset, var_prop = P3_15_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_estado", nombre = "Arma-punzo_año_estado",
               Dato = "Proporción de personas que portaban algún arma punzo cortante..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_3, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_estado", nombre = "Arma-fuego_año_estado",
               Dato = "Proporción de personas que portaban algún arma de fuego..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_4, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_estado", nombre = "Amenazaron_año_estado",
               Dato = "Proporción de personas que amenazaron a alguien con el arma..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_5, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_estado", nombre = "Dispararon_año_estado",
               Dato = "Proporción de personas que dispararon el arma..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_6, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_estado", nombre = "Manipularon_año_estado",
               Dato = "Proporción de personas que manipularon algún objeto..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_7, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_estado", nombre = "Sobornar_año_estado",
               Dato = "Proporción de personas que trataron de sobornar..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_estado", nombre = "Defenderse_año_estado",
               Dato = "Proporción de personas que trataron de defenderse..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion_año_estado", nombre = "Escapar_año_estado",
               Dato = "Proporción de personas que trataron de escapar..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_1, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo", nombre = "Obedecer_año_sexo",
               Dato = "Proporción de personas que reportaron obedecer las órdenes..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_2, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo", nombre = "Armar-punzo_año_sexo",
               Dato = "Proporción de personas que portaban algún arma punzo cortante..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_3, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo", nombre = "Arma-fuego_año_sexo",
               Dato = "Proporción de personas que portaban algún arma de fuego..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_4, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo", nombre = "Amenazaron_año_sexo",
               Dato = "Proporción de personas que amenazaron a alguien con el arma..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_5, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo", nombre = "Dispararon_año_sexo",
               Dato = "Proporción de personas que dispararon el arma..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_6, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo", nombre = "Manipularon_año_sexo",
               Dato = "Proporción de personas que manipularon algún objeto..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_7, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo", nombre = "Sobornar_año_sexo",
               Dato = "Proporción de personas que trataron de sobornar..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_8, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo", nombre = "Defenderse_año_sexo",
               Dato = "Proporción de personas que trataron de defenderse..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_9, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo", nombre = "Escapar_año_sexo",
               Dato = "Proporción de personas que trataron de escapar..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo_estado", nombre = "Obedecer_año_sexo_estado",
               Dato = "Proporción de personas que reportaron obedecer las órdenes..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo_estado", nombre = "Arma-punzo_año_sexo_estado",
               Dato = "Proporción de personas que portaban algún arma punzo cortante..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_3, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo_estado", nombre = "Arma-feugo_año_sexo_estado",
               Dato = "Proporción de personas que portaban algún arma de fuego..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_4, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo_estado", nombre = "Amenazaron_año_sexo_estado",
               Dato = "Proporción de personas que amenazaron a alguien con el arma..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_5, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo_estado", nombre = "Dispararon_año_sexo_estado",
               Dato = "Proporción de personas que dispararon el arma..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_6, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo_estado", nombre = "Manipularon_año_sexo_estado",
               Dato = "Proporción de personas que manipularon algún objeto..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_7, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo_estado", nombre = "Sobornar_año_sexo_estado",
               Dato = "Proporción de personas que trataron de sobornar..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo_estado", nombre = "Defenderse_año_sexo_estado",
               Dato = "Proporción de personas que trataron de defenderse..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_año_sexo_estado", nombre = "Escapar_año_sexo_estado",
               Dato = "Proporción de personas que trataron de escapar..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_15_1, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Obedecer_autoridad",
               Dato = "Proporción de personas que reportaron obedecer las órdenes..., por y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_2, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Arma-punzo_autoridad",
               Dato = "Proporción de personas que portaban algún arma punzo cortante..., por y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_3, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Arma-fuego_autoridad",
               Dato = "Proporción de personas que portaban algún arma de fuego..., por y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_4, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Amenzaron_autoridad",
               Dato = "Proporción de personas que amenazaron a alguien con el arma..., por y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_5, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Dispararon_autoridad",
               Dato = "Proporción de personas que dispararon el arma..., por y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_6, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Manipularon_autoridad",
               Dato = "Proporción de personas que manipularon algún objeto..., por y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_7, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Sobornar_autoridad",
               Dato = "Proporción de personas que trataron de sobornar..., por y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_8, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Defenderse_autoridad",
               Dato = "Proporción de personas que trataron de defenderse..., por y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_9, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Escapar_autoridad",
               Dato = "Proporción de personas que trataron de escapar..., por y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_1, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Obedecer_autoridad_estado",
               Dato = "Proporción de personas que reportaron obedecer las órdenes..., por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_2, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Arma-punzo_autoridad_estado",
               Dato = "Proporción de personas que portaban algún arma punzo cortante..., por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_3, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Arma-fuego_autoridad_estado",
               Dato = "Proporción de personas que portaban algún arma de fuego..., por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_4, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Amenazaron_autoridad_estado",
               Dato = "Proporción de personas que amenazaron a alguien con el arma..., por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_5, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Dispararon_autoridad_estado",
               Dato = "Proporción de personas que dispararon el arma..., por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_6, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Manipularon_autoridad_estado",
               Dato = "Proporción de personas que manipularon algún objeto..., por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_7, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Sorbornar_autoridad_estado",
               Dato = "Proporción de personas que trataron de sobornar..., por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_8, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Defenserse_autoridad_estado",
               Dato = "Proporción de personas que trataron de defenderse..., por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_9, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Escapar_autoridad_estado",
               Dato = "Proporción de personas que trataron de escapar..., por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_1, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_año", nombre = "Obedecer_autoridad_año",
               Dato = "Proporción de personas que reportaron obedecer las órdenes..., por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_2, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_año", nombre = "Arma-punzo_autoridad_año",
               Dato = "Proporción de personas que portaban algún arma punzo cortante..., por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_3, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_año", nombre = "Arma-fuego_autoridad_año",
               Dato = "Proporción de personas que portaban algún arma de fuego..., por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_4, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_año", nombre = "Amenazaron_autoridad_año",
               Dato = "Proporción de personas que amenazaron a alguien con el arma..., por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_5, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_año", nombre = "Dispararon_autoridad_año",
               Dato = "Proporción de personas que dispararon el arma..., por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_6, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_año", nombre = "Manipularon_autoridad_año",
               Dato = "Proporción de personas que manipularon algún objeto..., por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_7, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_año", nombre = "Sorbornar_autoridad_año",
               Dato = "Proporción de personas que trataron de sobornar..., por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_8, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_año", nombre = "Defenderse_autoridad_año",
               Dato = "Proporción de personas que trataron de defenderse..., por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_9, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_año", nombre = "Escapar_autoridad_año",
               Dato = "Proporción de personas que trataron de escapar..., por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Obedecer_autoridad_estado",
               Dato = "Proporción de personas que reportaron obedecer las órdenes..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Arma-punzo_autoridad_estado_año",
               Dato = "Proporción de personas que portaban algún arma punzo cortante..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_3, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Arma-fuego_autoridad_estado_año",
               Dato = "Proporción de personas que portaban algún arma de fuego..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_4, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Amenazaron_autoridad_estado_año",
               Dato = "Proporción de personas que amenazaron a alguien con el arma..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_5, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Dispararon_autoridad_estado_año",
               Dato = "Proporción de personas que dispararon el arma..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_6, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Manipularon_autoridad_estado_año",
               Dato = "Proporción de personas que manipularon algún objeto..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_7, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Sobornaron_autoridad_estado_año",
               Dato = "Proporción de personas que trataron de sobornar..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Obedecer_autoridad_estado_año",
               Dato = "Proporción de personas que trataron de defenderse..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Escapar_autoridad_estado_año",
               Dato = "Proporción de personas que trataron de escapar..., por estado y año de arresto y autoridad que detiene")

#### 3.4.15 Interrogado -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_16_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado", nombre = "Si",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_16, nacional")

tabla_excel_fn(dataset = subset, var_prop = P3_16_2, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado", nombre = "No",
               Dato = "Proporción de personas que respondieron \"no\" a P3_16, nacional")

tabla_excel_fn(dataset = subset, var_prop = P3_16_8, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado", nombre = "No-sabe",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_16, nacional")

tabla_excel_fn(dataset = subset, var_prop = P3_16_9, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado", nombre = "No-responde",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_16, nacional")

tabla_excel_fn(dataset = subset, var_prop = P3_16_1, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año", nombre = "Si_año",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_16, por año")

tabla_excel_fn(dataset = subset, var_prop = P3_16_2, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año", nombre = "No_año",
               Dato = "Proporción de personas que respondieron \"no\" a P3_16, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_8, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año", nombre = "No-sabe_año",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_16, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_9, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año", nombre = "No-responde_año",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_16, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_estado", nombre = "Si_año_estado",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_16, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_estado", nombre = "No_año_estado",
               Dato = "Proporción de personas que respondieron \"no\" a P3_16, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_estado", nombre = "No-sabe_año_estado",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_16, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_estado", nombre = "No-responde_año_estado",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_16, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_1, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_sexo", nombre = "Si_año_sexo",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_16, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_2, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_sexo", nombre = "No_año_sexo",
               Dato = "Proporción de personas que respondieron \"no\" a P3_16, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_8, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_sexo", nombre = "No-sabe_año_sexo",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_16, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_9, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_sexo", nombre = "No-responde_año_sexo",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_16, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Interrogado_año_sexo_estado", nombre = "Si_año_sexo_estado",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_16, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_sexo_estado", nombre = "No_año_sexo_estado",
               Dato = "Proporción de personas que respondieron \"no\" a P3_16, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_sexo_estado", nombre = "No-sabe_año_sexo_estado",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_16, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_año_sexo_estado", nombre = "No-responde_año_sexo_estado",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_16, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_1, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad", nombre = "Si_autoridad",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_16, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_16_2, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad", nombre = "No_autoridad",
               Dato = "Proporción de personas que respondieron \"no\" a P3_16, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_16_8, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad", nombre = "No-sabe_autoridad",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_16, por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_16_9, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad", nombre = "No-responde_autoridad",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_16, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_16_1, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad_estado", nombre = "Si_autoridad_estado",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_16, por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_2, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad_estado", nombre = "No_autoridad_estado",
               Dato = "Proporción de personas que respondieron \"no\" a P3_16, por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_8, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad_estado", nombre = "No-sabe_autoridad_estado",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_16, por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_9, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad_estado", nombre = "No-responde_autoridad_estado",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_16, por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_1, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad_año", nombre = "Si_autoridad_año",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_16, por autoridad que detiene y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_16_2, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad_año", nombre = "No_autoridad_año",
               Dato = "Proporción de personas que respondieron \"no\" a P3_16, por autoridad que detiene y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_16_8, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad_año", nombre = "No-sabe_autoridad_año",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_16, por autoridad que detiene y año")

tabla_excel_fn(dataset = subset, var_prop = P3_16_9, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Interrogado_autoridad_estado_año", nombre = "No-responde_autoridad_año",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_16, por autoridad que detiene y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_16_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Interrogado_autoridad_estado_año", nombre = "Si_autoridad_estado_año",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_16, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Interrogado_autoridad_estado_año", nombre = "No_autoridad_estado_año",
               Dato = "Proporción de personas que respondieron \"no\" a P3_16, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad_estado_año", nombre = "No-sabe_autoridad_estado_año",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_16, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_16_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Interrogado_autoridad_estado_año", nombre = "No-responde_autoridad_estado_año",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_16, por autoridad que detiene, año y estado de arresto")

#### 3.4.16 Autoridad realizo -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Amenazaron-cargos",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Amenazaron-matarlo",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a)")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Amenazaron-daño",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Amenazaron-daño-familia",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Otro-amenazas",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Presionaron",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Incomunicaron",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Pasearon",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Daño-familia",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Desvistieron",
               Dato = "Proporción de personas que reportaron que Le desvistieron")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo", nombre = "Vendaron",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera")

tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Amenazaron-cargo_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Amenazaron-matarlo_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a), por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Amenazaron-daño_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Amenazaron-daño-familia_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Otro-amenzas_año",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Presionaron_año",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Incomunicaron_año",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Pasearon_año",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Daño-familia_año",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Desvistieron_año",
               Dato = "Proporción de personas que reportaron que Le desvistieron, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año", nombre = "Vendaron_año",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Amenazaron-cargos_año_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Amenazaron-matarlo_año_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a), por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Amenazaron-daño_año_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Amenazaron-daño-familia_año_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Otro-amenazas_año_estado",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Presionaron_año_estado",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Incomunicaron_año_estado",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Pasearon_año_estado",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Daño-familia_año_estado",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Desvistieron_año_estado",
               Dato = "Proporción de personas que reportaron que Le desvistieron, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_estado", nombre = "Vendaron_año_estado",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Amenazaron-cargos_año_sexo",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Amenazaron-matarlo_año_sexo",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a), por sexoy año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Amenazaron-daño_año_sexo",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted, por sexoy año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Amenazaron-daño-familia_año_sexo",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia, por sexoy año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Otro-amenazas_año_sexo",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas, por sexoy año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Presionaron_año_sexo",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien, por sexoy año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Incomunicaron-cargos_año_sexo",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron, por sexoy año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Pasearon_año_sexo",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles, por sexoy año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Daño-familia_año_sexo",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia, por sexoy año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Desvistieron_año_sexo",
               Dato = "Proporción de personas que reportaron que Le desvistieron, por sexoy año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo", nombre = "Vendaron_año_sexo",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera, por sexoy año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Amenazaron-cargos_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Amenazaron-matarlo_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a), por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Amenazaron-daño_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Amenazaron-daño-familia_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Otro-amenazas_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Presionaron_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Incomunicaron_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Pasearon_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Daño-familia_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Desvistieron_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le desvistieron, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Vendaron_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Amenazaron-cargos_autoridad",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Amenazaron-matarlo_autoridad",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a), por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Amenazaron-daño_autoridad",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Amenazaron-daño-familia_autoridad",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Otro-amenazas_autoridad",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Presionaron_autoridad",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Incomunicaron-cargos_autoridad",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Pasearon_autoridad",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Daño-familia_autoridad",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Desvistieron_autoridad",
               Dato = "Proporción de personas que reportaron que Le desvistieron, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad", nombre = "Vendaron_autoridad",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Amenazaron-cargos_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos, por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Amenazaron-matarlo_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a), por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Amenazaron-daño_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted, por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Amenazaron-daño-familia_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia, por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Otro-amenazas_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas, por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Presionaron_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien, por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Incomunicaron_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron, por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Pasearon_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles, por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Daño-familia_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia, por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Desvistieron_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le desvistieron, por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado", nombre = "Vendaron_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera, por estado de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Amenazaron-cargos_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos, por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Amenazaron-matarlo_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a), por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Amenazaron-daño_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted, por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Amenazaron-daño-familia_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia, por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Otro-amenazas_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas, por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Presionaron_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien, por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Incomunicaron_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron, por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Pasearon_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles, por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Daño-familia_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia, por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Desvistieron_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le desvistieron, por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_año", nombre = "Vendaron_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera, por año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Amenazaron-cargos_autoridad_estado_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos, por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Amenazaron-matarlo_autoridad_estado_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a), por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Amenazaron-daño_autoridad_estado_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted, por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Amenazaron-daño-familia_autoridad_estado_año",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia, por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Otro-amenazas_autoridad_estado_año",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas, por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Presionaron_autoridad_estado_año",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien, por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Incomunicaron_estado_año",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron, por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Pasearon_autoridad_estado_año",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles, por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Daño-familia_autoridad_estado_año",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia, por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Desvistieron_autoridad_estado_año",
               Dato = "Proporción de personas que reportaron que Le desvistieron, por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Autoridad-realizo_autoridad_estado_año", nombre = "Vendaron_autoridad_estado_año",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera, por estado y año de arresto y autoridad que detiene")

#### 3.4.17 Agresiones físicas -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Ataron",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Asfixiaron",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Impidieron-respirar",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Patearon",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Golpearon",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Quemaron",
               Dato = "Proporción de personas que reportaron que Le quemaron..")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Descargas",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Aplastaron",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Hirieron",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Agujas",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Dispararon",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Acoso",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Organos-sexuales",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Actividad-sexual",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas", nombre = "Otra",
               Dato = "Proporción de personas que reportaron que Otra agresión física")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Ataron_año",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Asfixiaron_año",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Impidiendo-respirar_año",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Patearon_año",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Golpearon_año",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Quemaron_año",
               Dato = "Proporción de personas que reportaron que Le quemaron..por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Descargas_año",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Aplastaron_año",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Hirieron_año",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Agujas_año",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Dispararon_año",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Acoso_año",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Organos-sexuales_año",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Actividad-sexual_año",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año", nombre = "Otra_año",
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Ataron_año_estado",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Asfixiaron_año_estado",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Impidieron-respirar_año_estado",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Patearon_año_estado",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Golpearon_año_estado",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Quemaron_año_estado",
               Dato = "Proporción de personas que reportaron que Le quemaron..por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Descargas_año_estado",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Aplastaron_año_estado",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Hirieron_año_estado",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Agujas_año_estado",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Dispararon_año_estado",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Acoso-sexual_año_estado",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Organos-sexuales_año_estado",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Actividad-sexual_año_estado",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_estado", nombre = "Otra_año_estado",
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Ataron_año_sexo",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Asfixiaron_año_sexo",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Impidieron-respirar_año_sexo",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Patearon_año_sexo",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Golpearon_año_sexo",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Quemaron_año_sexo",
               Dato = "Proporción de personas que reportaron que Le quemaron..por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Descargas_año_sexo",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Aplastaron_año_sexo",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Hirieron_año_sexo",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Agujas_año_sexo",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Dispararon_año_sexo",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Acoso-sexual_año_sexo",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Organos-sexuales_año_sexo",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Actividad-sexual_año_sexo",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo", nombre = "Otra_año_sexo",
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Ataron_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Asfixiaron_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Impidieron-respirar_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Patearon_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Golpearon_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Quemaron_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le quemaron..por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Descargas_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Aplastaron_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Hirieron_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Agujas_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Dispararon_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Acoso_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Organos-sexuales_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Actividad-sexual_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_año_sexo_estado", nombre = "Otra_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Ataron_autoridad",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Asfixiaron_autoridad",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Impidieron-respirar_autoridad",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Patearon_autoridad",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Golpearon_autoridad",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Quemaron_autoridad",
               Dato = "Proporción de personas que reportaron que Le quemaron..por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Descargas_autoridad",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Aplastaron_autoridad",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Hirieron_autoridad",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Agujas_autoridad",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Dispararon_autoridad",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Acoso_autoridad",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Organos-sexuales_autoridad",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Actividad-sexual_autoridad",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad", nombre = "Otra_autoridad",
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Ataron_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Asfixiaron_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Impidieron-respirar_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Patearon_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Golpearon_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Quemaron_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le quemaron..por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Descargas_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Aplastaron_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Hirieron_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Agujas_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Dispararon_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Acoso_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Organos-sexuales_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Actividad-sexual_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_estado", nombre = "Otra_autoridad_estado",
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Ataron_autoridad_año",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Asfixiandolo_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Impidieron-respirar_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Patearon_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Golpearon_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Quemaron_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le quemaron..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Descargas_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Aplastaron_autoridad_año",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Hirieron_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Agujas_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Dispararon_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Acoso_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Organos-sexuales_autoridad_año",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Actividad-sexual_autoridad_año",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año", nombre = "Otra_autoridad_año",
               Dato = "Proporción de personas que reportaron que Otra agresión física, por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Ataron_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Asfixiaron_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Impidieron-respirar_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Patearon_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Golpearon_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Quemaron_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le quemaron..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Descargas_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Aplastaron_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Hirieron_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Agujas_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Dispararon_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Acoso_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Organos-sexuales_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Actividad-sexual_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Agresiones-fisicas_autoridad_año_estado", nombre = "Otra_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que Otra agresión física, por autoridad que detiene y  estado y año de arresto")

#### 3.4.18 LLevado -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "MP",
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Juez",
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Policia",
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía...")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Arraigo",
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Penitenciario",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Oficina",
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno...")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Casa",
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Establecimiento",
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial...")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Vehiculo",
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Terreno",
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Militar",
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar...")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Migrantes",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Hospital",
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica...")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "Otro",
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "No_sabe",
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado", nombre = "No_responde",
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "MP_año",
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por  año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Juez_año",
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por  año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Policia_año",
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía... año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Arraigo_año",
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Penitenciario_año",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Oficina_año",
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Casa_año",
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Establecimiento_año",
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Vehiculo_año",
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Terreno_año",
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Militar_año",
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Migrantes_año",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Hospital_año",
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "Otro_año",
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "No-sabe_año",
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año", nombre = "No-responde_año",
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "MP_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Juez_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Policia_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Arraigo_año_estado",
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Penitenciario_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Oficina_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Casa_año",
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Establecimiento_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Vehiculo_año",
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Terreno_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Militar_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Migrantes_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Hospital_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "Otro_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "No_sabe_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_estado", nombre = "No_responde_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "MP_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Juez_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Policia_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Arraigo_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Penitenciario_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Oficina_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Casa_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Establecimiento_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Vehiculo_año_sexo", 
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Terreno_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Militar_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Migrantes_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Hospital_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "Otro_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "No_sabe
               _año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo", nombre = "No_resopnde_año_sexo",
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por sexo y año de arresto")




tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "MP_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Juez_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Plocia_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Arraigo_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Penitenciario_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Oficina_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Casa_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Establecimiento_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Vehiculo_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Terreno_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Militar_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Migrantes_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Hospital_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "Otro_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "No_sabe_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_año_sexo_estado", nombre = "No_resonde_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "MP_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Juez_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Policia_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Arraigo_autoridad",
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Penitenciario_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Oficina_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Casa_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Establecimiento_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Vehiculo_autoridad",
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Terreno_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Militar_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Migrantes_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Hospital_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "Otro_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "No_sabe_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad", nombre = "No_responde_autoridad",
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "MP_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Juez_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Policia_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Arraigo_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Penitenciario_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Oficina_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Casa_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Establecimiento_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Vehiculo_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Terreno_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Militar_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Migrantes_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Hospital_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "Otro_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "No_sabe_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_estado", nombre = "No_responde_autoridad_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "MP_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por autoridad que detiene, de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Juez_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Policia_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Arraigo_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Pnitenciario_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Oficina_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Casa_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Establecimiento_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Vehiculo_autoridad_año",
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Terreno_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Militar_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Migrantes_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Hospital_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "Otro_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "No_sabe_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año", nombre = "No_responde_autoridad_año",
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "MP_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Juez_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Policia_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Arraigo_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Penitenciario_autoridad_año_estado", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Oficina_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Casa_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Establecimiento_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Vehículo_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Terreno_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Militar_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Migrantes_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Hospital_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "Otro_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "No_sabe_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Llevado_autoridad_año_estado", nombre = "No_responde_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por autoridad que detiene, año y estado de arresto")

#### 3.4.19 Tiempo -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo", nombre = "30",
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo", nombre = "1",
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo", nombre = "2",
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo", nombre = "4",
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo", nombre = "6",
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo", nombre = "24",
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo", nombre = "48",
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo", nombre = "72",
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo", nombre = "72+",
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo", nombre = "No_sabe",
               Dato = "Proporción de personas que reportaron que pasaron No_sabe")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo", nombre = "No_responde",
               Dato = "Proporción de personas que reportaron que pasaron No_responde")


tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "30_año",
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por  año")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "1_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "2_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "4_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "6_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "24_año",
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "48_año",
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "72_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "72+_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "No_sabe_año",
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año", nombre = "No_responde_año",
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "30_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "1_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "2_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "4_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "6_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "24_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "48_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "72_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "172+_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "No_sabe_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_estado", nombre = "No_responde_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "30_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "1_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "2_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "4_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "6_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "24_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "48_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "72_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "72+_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "No_sabe_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_año_sexo", nombre = "No_responde_año_sexo",
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "30_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "1_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "2_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "4_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "6_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "24_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "48_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "72_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "72+_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "No_sabe_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_año_sexo_estado", nombre = "No_responde_año_sexo_estado",
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "30_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, porautoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "1_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "2_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "4_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "6_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "24_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "48_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "72_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "72+_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "No_sabe_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad", nombre = "No_responde_autoridad",
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "30_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, porautoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "1_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "2_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "4_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "6_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "24_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "48_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "72_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "72+_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "No_sabe_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_estado", nombre = "No_responde_autoridad_estado",
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "30_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "1_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "2_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "4_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "6_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "24_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "48_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "72_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "72+_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "No_sabe_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año", nombre = "No_responde_autoridad_año",
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "30_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "1_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "2_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "4_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "6_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "24_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "48_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "72_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "72+_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "No_sabe_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tiempo_autoridad_año_estado", nombre = "No_responde_autoridad_año_estado",
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por autoridad que detiene, año y estado de arresto")

#### 3.4.20 Cambio -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio", nombre = "Si",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio", nombre = "No",
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio", nombre = "No_sabe",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio", nombre = "No_responde",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año", nombre = "Si_año",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año", nombre = "No_año",
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año", nombre = "No_sabe_año",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año", nombre = "No_responde_año",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_estado", nombre = "Si_año_estado",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_estado", nombre = "No_año_estado",
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_estado", nombre = "No_sabe_año_estado",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_estado", nombre = "No_responde_año_estado",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_sexo", nombre = "Si_año_sexo",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_sexo", nombre = "No_año_sexo",
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_sexo", nombre = "No_sabe_año_sexo",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_sexo", nombre = "No_responde_año_sexo",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_estado_sexo", nombre = "Si_año_estado_sexo",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_estado_sexo", nombre = "No_año_estado_sexo",
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_estado_sexo", nombre = "No_sabe_año_estado_sexo",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_año_estado_sexo", nombre = "No_responde_año_estado_sexo",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad", nombre = "Si_autoridad",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Cambio_autoridad", nombre = "No_autoridad",
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Cambio_autoridad", nombre = "No_sabe_autoridad",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad", nombre = "No_responde_autoridad",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad_estado", nombre = "Si_autoridad_estado",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad_estado", nombre = "No_autoridad_estado",
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Cambio_autoridad_estado", nombre = "No_sabe_autoridad_estado",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad_estado", nombre = "No_responde_autoridad_estado",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad_año", nombre = "Si_autoridad_año",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por autoridad que detiene, año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad_año", nombre = "No_autoridad_año",
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por autoridad que detiene, año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad_año", nombre = "No_sabe_autoridad_año",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por autoridad que detiene, año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad_año", nombre = "No_responde_autoridad_año",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por autoridad que detiene, año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad_año_estado", nombre = "Si_autoridad_año_estado",
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Cambio_autoridad_año_estado", nombre = "No_autoridad_año_estado",
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad_año_estado", nombre = "No_sabe_autoridad_año_estado",
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Cambio_autoridad_año_estado", nombre = "No_responde_autoridad_año_estado",
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por autoridad que detiene, año y estado de arresto")

#### 3.4.21 Dinero -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Dinero", nombre = "Sí",
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Dinero", nombre = "Sí_año",
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por año")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Dinero", nombre = "Sí_año_estado",
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Dinero", nombre = "Sí_año_sexo",
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Dinero", nombre = "Sí_año_estado_sexo",
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Dinero", nombre = "Sí_autoridad",
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Dinero", nombre = "Sí_autoridad_estado",
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Dinero", nombre = "Sí_autoridad_año",
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Dinero", nombre = "Sí_autoridad_año_estado",
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por autoridad que detiene, año y estado de arresto")



#### 3.4.22 Propocionalidad de uso de la fuerza -------------------------------------------------------------

tabla_excel_fn(dataset = subset, var_prop = proporcionalidad_uso_fuerza, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Prop_fuerza", nombre = "Sí",
               Dato = "Proporción de personas clasificadas con uso proporcional de la fuerza")

tabla_excel_fn(dataset = subset, var_prop = proporcionalidad_uso_fuerza, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Prop_fuerza", nombre = "Sí_año",
               Dato = "Proporción de personas clasificadas con uso proporcional de la fuerza, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = proporcionalidad_uso_fuerza, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Prop_fuerza", nombre = "Sí_corporacion",
               Dato = "Proporción de personas clasificadas con uso proporcional de la fuerza, por corporación que realizó la detención")

tabla_excel_fn(dataset = subset, var_prop = proporcionalidad_uso_fuerza, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Prop_fuerza", nombre = "Sí_sexo",
               Dato = "Proporción de personas clasificadas con uso proporcional de la fuerza, por sexo")

tabla_excel_fn(dataset = subset, var_prop = proporcionalidad_uso_fuerza, var1 = "LGBTQ", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Prop_fuerza", nombre = "Sí_LGBTQ",
               Dato = "Proporción de personas clasificadas con uso proporcional de la fuerza, por población LGBT+")

tabla_excel_fn(dataset = subset, var_prop = proporcionalidad_uso_fuerza, var1 = "Edad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Prop_fuerza", nombre = "Sí_edad",
               Dato = "Proporción de personas clasificadas con uso proporcional de la fuerza, por edad")

tabla_excel_fn(dataset = subset, var_prop = proporcionalidad_uso_fuerza, var1 = "Escolaridad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Prop_fuerza", nombre = "Sí_educación",
               Dato = "Proporción de personas clasificadas con uso proporcional de la fuerza, por escolaridad")






### 3.5 Inspecciones + Inspecciones Efectivas + Inspecciones y cumplimiento del debido proceso -------------------------------------------



tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Desvestidas", nombre = "Si",
               Dato = "Proporción de personas que fueron desvestidas durante la inspección")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Inspecciones", seccion = "Desvestidas", nombre = "Si_sexo",
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Desvestidas", nombre = "Si_sexo_estado",
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Desvestidas", nombre = "Si_año",
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por año ")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Desvestidas", nombre = "Si_año_estado",
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Anio_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Desvestidas", nombre = "Si_año_sexo",
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por año y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Desvestidas", nombre = "Si_año_estado_sexo",
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por año y estado de arresto y sexo")


tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "LGBTQ", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Desvestidas", nombre = "Si_LGBTQ",
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por población LGBT+")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Desvestidas", nombre = "Si_corporacion",
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por población LGBT+")


#### 3.5.1 Encontraron -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Encontraron", nombre = "Si",
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección")

tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Inspecciones", seccion = "Encontraron", nombre = "Si_sexo",
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = "Anio_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Encontraron", nombre = "Si_año_sexo",
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por año y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Encontraron", nombre = "Si_año_estado_sexo",
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Encontraron", nombre = "Si_autoridad",
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por autoridad que detiene")


tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Encontraron", nombre = "Si_año",
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Encontraron", nombre = "Si_corporacion",
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por autoridad que detiene")


#### 3.5.1 Encontraron_delito -------------------------------------------------------------


for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = "1", 
                 carpeta = "Inspecciones", seccion = "Encontraron_delito", nombre = paste0("Encontraron_",i),
                 Dato = paste0("Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, entre las acusadas (entre otros) por ",i))
}

#### 3.5.1 Encontraron_delito -------------------------------------------------------------


for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1,
                 carpeta = "Inspecciones", seccion = "Encontraron_delito-grupo", nombre = paste0("Encontraron_",i),
                 Dato = paste0("Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, entre las acusadas (entre otros) por ",i))
}

#### 3.5.1 Sembraron -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Sembraron", nombre = "Si",
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Sembraron", nombre = "Si_sexo",
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Sembraron", nombre = "Si_estado",
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Sembraron", nombre = "Si_año",
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Sembraron", nombre = "Si_año_estado",
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Anio_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Sembraron", nombre = "Si_año_sexo",
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por año y sexo")



tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Sembraron", nombre = "Si_año_estado_sexo",
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por año y estado de arresto")


tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "LGBTQ", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Sembraron", nombre = "Si_LGBTQ",
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por población LGBT+")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Sembraron", nombre = "Si_corporacion",
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por autoridad que realiza el arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Desvistieron", nombre = "Si",
               Dato = "Proporción de personas que reportan que la autoridad lo desvistió durante la inspección")


#### 3.5.1 Videograbo -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_12_5, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Videograbo", nombre = "Si",
               Dato = "Proporción de personas que reportan que la autoridad videograbó la inspección durante la inspección")

tabla_excel_fn(dataset = subset, var_prop = P3_12_5, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Videograbo", nombre = "Si_sexo",
               Dato = "Proporción de personas que reportan que la autoridad videograbó la inspección durante la inspección, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_5, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Videograbo", nombre = "Si_año",
               Dato = "Proporción de personas que reportan que la autoridad videograbó la inspección durante la inspección, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_12_5, var1 = "LGBTQ", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Videograbo", nombre = "Si_LGBTQ",
               Dato = "Proporción de personas que reportan que la autoridad videograbó la inspección durante la inspección, por población LGBT+")

tabla_excel_fn(dataset = subset, var_prop = P3_12_5, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Videograbo", nombre = "Si_corporacion",
               Dato = "Proporción de personas que reportan que la autoridad videograbó la inspección durante la inspección, por autoridad que realizó la dentención")

#### 3.5.1 Objeto_buscaba -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_12_2, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Objeto_buscaba", nombre = "Si",
               Dato = "Proporción de personas que reportan que la autoridad le dijo qué objeto buscaba durante la inspección")

tabla_excel_fn(dataset = subset, var_prop = P3_12_2, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Objeto_buscaba", nombre = "Si_sexo",
               Dato = "Proporción de personas que reportan que la autoridad le dijo qué objeto buscaba durante la inspección")

tabla_excel_fn(dataset = subset, var_prop = P3_12_2, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Objeto_buscaba", nombre = "Si_año",
               Dato = "Proporción de personas que reportan que la autoridad le dijo qué objeto buscaba durante la inspección")

tabla_excel_fn(dataset = subset, var_prop = P3_12_2, var1 = "LGBTQ", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Objeto_buscaba", nombre = "Si_LGBTQ",
               Dato = "Proporción de personas que reportan que la autoridad le dijo qué objeto buscaba durante la inspección")

tabla_excel_fn(dataset = subset, var_prop = P3_12_2, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Objeto_buscaba", nombre = "Si_corporacion",
               Dato = "Proporción de personas que reportan que la autoridad le dijo qué objeto buscaba durante la inspección")

#### 3.5.1 Placa -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Nombre", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad se identificó con nombre o placa")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Nombre", nombre = "Sexo",
               Dato = "Proporción de personas que reportan que la autoridad se identificó con nombre o placa, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Nombre", nombre = "Año",
               Dato = "Proporción de personas que reportan que la autoridad se identificó con nombre o placa, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Nombre", nombre = "Corporacion",
               Dato = "Proporción de personas que reportan que la autoridad se identificó con nombre o placa, por autoridad que realizó la detención")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Nombre", nombre = "Estado",
               Dato = "Proporción de personas que reportan que la autoridad se identificó con nombre o placa, por año de arresto")

#### 3.5.1 Tipo -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Tipo", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad se identificó con tipo de policía")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Tipo", nombre = "Sexo",
               Dato = "Proporción de personas que reportan que la autoridad se identificó con tipo de policía, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Tipo", nombre = "Año",
               Dato = "Proporción de personas que reportan que la autoridad se identificó con tipo de policía, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Inspecciones", seccion = "Tipo", nombre = "Corporacion",
               Dato = "Proporción de personas que reportan que la autoridad se identificó con tipo de policía, por autoridad que realizó la detención")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Tipo", nombre = "Estado",
               Dato = "Proporción de personas que reportan que la autoridad se identificó con tipo de policía, por año de arresto")

#### 3.5.1 Uniformado -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Uniformada", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad estaba uniformada")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Uniformada", nombre = "Sexo",
               Dato = "Proporción de personas que reportan que la autoridad estaba uniformada, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Uniformada", nombre = "Año",
               Dato = "Proporción de personas que reportan que la autoridad estaba uniformada, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Uniformada", nombre = "Corporacion",
               Dato = "Proporción de personas que reportan que la autoridad estaba uniformada, por autoridad que realizó la detención")

tabla_excel_fn(dataset = subset, var_prop = P3_14_3, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Uniformada", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad estaba uniformada, por año de arresto")

#### 3.5.1 Dijo por que -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Dijo-por-que", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le dijo por qué le detuvieron")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Dijo-por-que", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le dijo por qué le detuvieron, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Dijo-por-que", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le dijo por qué le detuvieron, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Dijo-por-que", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le dijo por qué le detuvieron, por autoridad que realizó la detención")

tabla_excel_fn(dataset = subset, var_prop = P3_14_4, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Dijo-por-que", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le dijo por qué le detuvieron, por año de arresto")

#### 3.5.1 Silencio -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Silencio", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le informó sobre guardar silencio y no declarar sin abogado")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Silencio", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le informó sobre guardar silencio y no declarar sin abogado, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Silencio", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le informó sobre guardar silencio y no declarar sin abogado, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Inspecciones", seccion = "Silencio", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le informó sobre guardar silencio y no declarar sin abogado, por autoridad que realizó la detención")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Silencio", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le informó sobre guardar silencio y no declarar sin abogado, por año de arresto")


#### 3.5.1 Dijo llevaría -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Dijo-llevaria", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le dijo a dónde le llevaría")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Dijo-llevaria", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le dijo a dónde le llevaría, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Dijo-llevaria", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le dijo a dónde le llevaría, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Corporacion_grupos", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Dijo-llevaria", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le dijo a dónde le llevaría, por autoridad que realizó la detención")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Inspecciones", seccion = "Dijo-llevaria", nombre = "Nacional",
               Dato = "Proporción de personas que reportan que la autoridad le dijo a dónde le llevaría, por año de arresto")



### 3.6 PPO ---------------------------------------------------------------------



tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Delitos", nombre = "Nacional",
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_vehiculo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Robo_vehiculo",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo de vehículo")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_casa_hab", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Robo_casa_hab",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo a casa habitación")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_negocio", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Robo_negocio",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo a negocio")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_transporte_pub", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Robo_transporte",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo a transporte público")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_transeunte", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Robo_transeunte",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo a transeunte en la via pública")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_autopartes", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Robo_autopartes",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo de autopartes")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_otros", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Robo_otros",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo en forma distinta a las anteriores")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Posesion_drogas", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Robo_drogas",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Posesión ilegal de drogas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Comercio_drogas", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Comercio_drogas",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Comercio ilegal de drogas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Lesiones", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Lesiones",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Lesiones")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Hom_culposo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Hom_culposo",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Homicidio culposo")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Hom_doloso", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Hom_doloso",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Homicidio doloso")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Portacion_armas", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Portación_armas",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Portación ilegal de armas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Incum_asis_fam", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Incumplimiento_fam",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Incumplimiento de obligaciones de asistencia familiar")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Violencia_fam", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "PPO", seccion = "PPO", nombre = "Violencia_fam",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Violencia familiar")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Danio_prop", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Daño_prop",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Daño a la propiedad")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Secuestro", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Secuestro",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Secuestro y secuestro express")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Violacion_sexual", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Violación_sexual",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Violación sexual")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Fraude", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Fraude",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Fraude")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Delincuencia_org", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Delincuencia_org",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Delincuencia organizada")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Otros_sexuales", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "PPO", seccion = "PPO", nombre = "Otros_sexuales",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Otros delitos sexuales")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Extorsion", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Extrosión",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Extorsión")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Privacion_de_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Privación_libertad",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Privación de la libertad")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Abuso_de_conf", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Abuso_confianza",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Abuso de confianza")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Amenazas", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Amenazas",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Amenazas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Otros", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "Otros",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Otros delitos")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "No_sabe", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "No_sabe",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por No_sabe")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "No_responde", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "PPO", nombre = "No_responde",
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por No_responde")

#### 3.5.1 Libertad -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = proceso_en_libertad, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Libertad", nombre = "Si_año",
               Dato = "Proporción de personas que llevaron su proceso en libertad o no, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = proceso_en_libertad, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Libertad", nombre = "Si_año_estado",
               Dato = "Proporción de personas que llevaron su proceso en libertad, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = proceso_en_libertad, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Libertad", nombre = "Si_sexo_estado",
               Dato = "Proporción de personas que llevaron su proceso en libertad, por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Libertad", nombre = "Si_año_PPO",
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO, por año de arresto")

#### 3.5.2 Delitos -------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = "RND_3", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Delitos", nombre = "RND",
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO, antes y después de la implementación del RND_3")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Delitos", nombre = "estado",
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO, por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Delitos", nombre = "sexo",
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO, por sexo")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "PPO", seccion = "Delitos", nombre = "admision",
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO, por admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, 
               carpeta = "PPO", seccion = "No-PPO", nombre = "Nacional",
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, 
               carpeta = "PPO", seccion = "No-PPO", nombre = "año",
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas por año")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "RND_3", var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, 
               carpeta = "PPO", seccion = "No-PPO", nombre = "RND",
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas, antes y después de la implementación del RND_3")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, 
               carpeta = "PPO", seccion = "No-PPO", nombre = "estado",
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas, por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, 
               carpeta = "PPO", seccion = "No-PPO", nombre = "sexo",
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas, por sexo")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "culpabilidad", var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, 
               carpeta = "PPO", seccion = "No-PPO", nombre = "admision",
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas, por admisión de culpabilidad")

#### 3.5.3 Percepción-------------------------------------------------------------


tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez", nombre = "antes-jucio",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez", nombre = "despues-pruebas",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez", nombre = "nunca-vieron",
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "culpabilidad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_admision", nombre = "antes-jucio_admision",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "culpabilidad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_admision", nombre = "despues-pruebas_admision",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "culpabilidad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_admision", nombre = "nunca-vieron_admision",
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO", nombre = "antes-jucio_PPO",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por PPO y no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO", nombre = "despues-pruebas_PPO",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por PPO y no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO", nombre = "nunca-vieron_PPO",
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por PPO y no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "culpabilidad", var2 = "PPO", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_jue_PPOz", nombre = "antes-jucio_admision_PPO",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por admisión de culpabilidad y PPO vs no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "culpabilidad", var2 = "PPO", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_admision_PPO", nombre = "despues-pruebas_admision_PPO",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por admisión de culpabilidad y PPO vs no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "culpabilidad", var2 = "PPO", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_admision_PPO", nombre = "nunca-vieron_admision_PPO",
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por admisión de culpabilidad y PPO vs no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO_estado", nombre = "antes-jucio_PPO_estado",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por PPO y no-PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO_estado", nombre = "despues-pruebas_PPO_estado",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por PPO y no-PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO_estado", nombre = "nunca-vieron_PPO_estado",
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por PPO y no-PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO_sexo", nombre = "antes-jucio_PPO_sexo",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por PPO y no-PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO_sexo", nombre = "despues-pruebas_PPO_sexo",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por PPO y no-PPO y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "PPO", seccion = "Percepcion_juez_PPO_sexo", nombre = "nunca-vieron_PPO_sexo",
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por PPO y no-PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO_admision", nombre = "antes-jucio_PPO_admision",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por PPO y no-PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO_admision", nombre = "despues-pruebas_PPO_admision",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por PPO y no-PPO y estado de arresto y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_PPO_admision", nombre = "nunca-vieron_PPO_admision",
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por PPO y no-PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_libertad", nombre = "inicio-jucio_libertad",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por condición de llevar el proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_libertad", nombre = "despues-pruebas_libertad",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por condición de llevar el proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_libertad", nombre = "nunca-vieron_libertad",
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por condición de llevar el proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_libertad_admision", nombre = "inicio-jucio_libertad_admision",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por condición de llevar el proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_libertad_admision", nombre = "despues-pruebas_libertad_admision",
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por condición de llevar el proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Percepcion_juez_libertad_admision", nombre = "nunca-vieron_libertad_admision",
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por condición de llevar el proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Escuchadas", nombre = "Estado",
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Escuchadas", nombre = "PPO",
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por PPO  vs no PPO")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Escuchadas", nombre = "PPO_estado",
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por PPO  vs no PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Escuchadas", nombre = "PPO_estado_sexo",
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por PPO  vs no PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Escuchadas", nombre = "PPO_admision",
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por PPO  vs no PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Escuchadas", nombre = "Libertad",
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por condición de haber llevado su proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "proceso_en_libertad", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Escuchadas", nombre = "Libertad_estado",
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por condición de haber llevado su proceso en libertad o no y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "proceso_en_libertad", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Escuchadas", nombre = "Libertad_sexo",
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por condición de haber llevado su proceso en libertad o no y sexo")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Escuchadas", nombre = "Libertad_admision",
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por condición de haber llevado su proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_MP", nombre = "PPO",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por PPO  vs no PPO")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_MP", nombre = "PPO_estado",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por PPO  vs no PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_MP", nombre = "PPO_sexo",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por PPO  vs no PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_MP", nombre = "PPO_admision",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por PPO  vs no PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_MP", nombre = "Libertad",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por condición de haber llevado su proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "proceso_en_libertad", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_MP", nombre = "Libertad_estado",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por condición de haber llevado su proceso en libertad o no y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "proceso_en_libertad", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_MP", nombre = "Libertad_sexo",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por condición de haber llevado su proceso en libertad o no y sexo")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_MP", nombre = "Libertad_admision",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por condición de haber llevado su proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_jucio", nombre = "PPO",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por PPO  vs no PPO")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_jucio", nombre = "PPO_estado",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por PPO  vs no PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_jucio", nombre = "PPO_sexo",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por PPO  vs no PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "PPO", seccion = "Satisfecho_abogado_jucio", nombre = "PPO_admision",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por PPO  vs no PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "PPO", seccion = "Satisfecho_abogado_jucio", nombre = "Libertad",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por condición de haber llevado su proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "proceso_en_libertad", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_jucio", nombre = "Libertad_estado",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por condición de haber llevado su proceso en libertad o no y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "proceso_en_libertad", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "PPO", seccion = "Satisfecho_abogado_jucio", nombre = "Libertad_sexo",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por condición de haber llevado su proceso en libertad o no y sexo")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Satisfecho_abogado_jucio", nombre = "Libertad_admision",
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por condición de haber llevado su proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Trato-justo", nombre = "PPO",
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por PPO  vs no PPO")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Trato-justo", nombre = "PPO_estado",
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por PPO  vs no PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Trato-justo", nombre = "PPO_sexo",
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por PPO  vs no PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Trato-justo", nombre = "PPO_admision",
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por PPO  vs no PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Trato-justo", nombre = "Libertad",
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por condición de haber llevado su proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "proceso_en_libertad", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Trato-justo", nombre = "Libertad_estado",
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por condición de haber llevado su proceso en libertad o no y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "proceso_en_libertad", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Trato-justo", nombre = "Libertad_sexo",
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por condición de haber llevado su proceso en libertad o no y sexo")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Trato-justo", nombre = "Libertad_admision",
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por condición de haber llevado su proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Sentencia-justa", nombre = "PPO",
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por PPO  vs no PPO")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Sentencia-justa", nombre = "PPO_estado",
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por PPO  vs no PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Sentencia-justa", nombre = "PPO_sexo",
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por PPO  vs no PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Sentencia-justa", nombre = "PPO_admision",
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por PPO  vs no PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "PPO", seccion = "Sentencia-justa", nombre = "Libertad",
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por condición de haber llevado su proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "proceso_en_libertad", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Sentencia-justa", nombre = "Libertad_estado",
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por condición de haber llevado su proceso en libertad o no y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "proceso_en_libertad", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Sentencia-justa", nombre = "Libertad_sexo",
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por condición de haber llevado su proceso en libertad o no y sexo")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Sentencia-justa", nombre = "Libertad_adimision",
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por condición de haber llevado su proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_f, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Fuero-federal", nombre = "Estado_sexo",
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos del fuero federal, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_c, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Fuero-comun", nombre = "Estado_sexo",
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos del fuero común, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = ambos_fueros, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Fuero-ambos", nombre = "Estado_sexo",
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos de ambos fueros, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_f, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Fuero-federal", nombre = "Estado_año",
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos del fuero federal, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_c, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Fuero-comun", nombre = "Estado_año",
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos del fuero común, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = ambos_fueros, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "PPO", seccion = "Fuero-ambos", nombre = "Estado_año",
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos de ambos fueros, por año y  estado de arresto")
