## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL 2023 - Descriptivos 
##
## Author(s):         F. Marcelo Torres González  (marcelo.torresgo@gmail.com)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##                    Cristina Álvarez Venzor     (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     June 22th, 2023
##
## This version:      September 18th, 2023
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
source("Data_cleaning/Code/settings.R")


# Remove previous files

file.remove("Descriptives/Output/Cruzadas/Detencion_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/Tortura_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/PPO_Parte1_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/PPO_Parte2_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/PPO_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/Inspecciones_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/Flagrancia_constitucional_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/Policia_Parte1_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/Policia_Parte2_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/Policia_Parte3_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/Policia_Parte4_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/Policia_Parte5_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/Policia_Parte4_cruzadas.xlsx")
file.remove("Descriptives/Output/Cruzadas/Policia_Parte5_cruzadas.xlsx")

# Function to create the table and write the excel file

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
             file      = file.path(paste0("Descriptives/Output/Detenciones/", 
                                          carpeta,"/desc_", seccion,".xlsx"),
                                   fsep = "/"),  
             sheetName = paste0(nombre),
             append    = T,
             row.names = F)
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Load Database and prepare data                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load(paste0(path2DB,"/Data_cleaning/Output/Main_database.RData")) 


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
                                      P5_25 == "2" | P5_25 == "3" | P5_25 == "8" | P5_25 == "9" ~ 0 ,
                                      T ~ NA),
         Despues_de_las_pruebas = case_when(P5_25 == "2" ~ 1,
                                            P5_25 == "1" | P5_25 == "3" | P5_25 == "8" | P5_25 == "9" ~ 0 ,
                                            T ~ NA),
         Nunca_vio_al_juez = case_when(P5_25 == "3" ~ 1,
                                       P5_25 == "1" | P5_25 == "2" | P5_25 == "8" | P5_25 == "9" ~ 0 ,
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
         
         # Grupos de P3_19
         P3_19_01 = case_when(P3_19 == "01" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_02 = case_when(P3_19 == "02" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_03 = case_when(P3_19 == "03" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_04 = case_when(P3_19 == "04" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_05 = case_when(P3_19 == "05" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_06 = case_when(P3_19 == "06" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_07 = case_when(P3_19 == "07" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_08 = case_when(P3_19 == "08" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_09 = case_when(P3_19 == "09" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_10 = case_when(P3_19 == "10" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_11 = case_when(P3_19 == "11" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_12 = case_when(P3_19 == "12" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_13 = case_when(P3_19 == "13" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_14 = case_when(P3_19 == "14" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_98 = case_when(P3_19 == "98" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         P3_19_99 = case_when(P3_19 == "99" ~ 1,
                              is.na(P3_19) == TRUE ~ NA,
                              T ~ 0),
         
         # Grupos de P3_20
         P3_20_01 = case_when(P3_20 == "01" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         P3_20_02 = case_when(P3_20 == "02" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         P3_20_03 = case_when(P3_20 == "03" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         P3_20_04 = case_when(P3_20 == "04" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         P3_20_05 = case_when(P3_20 == "05" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         P3_20_06 = case_when(P3_20 == "06" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         P3_20_07 = case_when(P3_20 == "07" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         P3_20_08 = case_when(P3_20 == "08" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         P3_20_09 = case_when(P3_20 == "09" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         P3_20_98 = case_when(P3_20 == "98" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         P3_20_99 = case_when(P3_20 == "99" ~ 1,
                              is.na(P3_20) == TRUE ~ NA,
                              T ~ 0),
         
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
                                                                                         "9" = NA_real_)))  

rm(Main_database)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Create tables                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## Detención



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
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Corporación_grupo",
               Dato = "Proporción de personas detenidas en flagrancia por corporación que detiene")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "detenciones_flagrancia", nombre = "Corporación_grupo_año",
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
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Corporacion",
               Dato = "Proporción de personas detenidas con una orden de detención por corporación que detiene")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Corporacion_grupos", var2 = "Anio_arresto", var3 = NA, 
               varfilter = "Post_2008", filtervalue = 1, 
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Corporacion-grupo_año",
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
               carpeta = "Detenciones", seccion = "orden_aprehension", nombre = "Ingresos",
               Dato = "Proporción de personas detenidas con una orden de detención por nivel de ingresos")

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
               carpeta = "Detenciones", seccion = "despues_inspeccion", nombre = "Ingreso",
               Dato = "Proporción de personas detenidas con una orden de detención por nivel de ingresos")

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
               carpeta = "Detenciones", seccion = "det_ninguna", nombre = "Ingreso",
               Dato = "Proporción de personas detenidas con una orden de detención por nivel de ingresos")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_f, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Fuero", nombre = "Fuerfo-federal",
               Dato = "Proporción de personas detenidas y acusadas únicamente de delitos de fuero federal")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_c, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Fuero", nombre = "Fuerfo-comun",
               Dato = "Proporción de personas detenidas y acusadas únicamente de delitos de fuero común")

tabla_excel_fn(dataset = subset, var_prop = ambos_fueros, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Detenciones", seccion = "Fuero", nombre = "Fuerfo-ambos",
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




# Prevalencia de tortura

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
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "Aprobacion_RND",
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
               carpeta = "Tortura", seccion = "Tortura_traslado", nombre = "Aprobacion_RND_corporacion",
               Dato = "Proporción de personas que reportaron ser torturadas durante el traslado, por RND_3 y corporación que detiene")

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
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Cargos-falsos_genero",
               Dato = "Proporción de personas que reportaron que Le amenazaron con levantarle cargos falsos, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_02, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Amenaza-matarlo_genero",
               Dato = "Proporción de personas que reportaron que Le amenazaron con matarlo(a), por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_03, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Amenaza-daño-usted_genero",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a usted, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_04, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Amenaza-daño-familia_genero",
               Dato = "Proporción de personas que reportaron que Le amenazaron con hacerle daño a su familia, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_05, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Otra-amenaza_genero",
               Dato = "Proporción de personas que reportaron que Le hicieron otro tipo de amenazas, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_06, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Presion-denuncia_genero",
               Dato = "Proporción de personas que reportaron que Le presionaron para denunciar a alguien, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_07, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Incomunicaron_genero",
               Dato = "Proporción de personas que reportaron que Le incomunicaron o aislaron, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_08, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Vueltas_genero",
               Dato = "Proporción de personas que reportaron que Le pasearon en automovil dando vueltas por las calles, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_09, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Hicieron-daño_genero",
               Dato = "Proporción de personas que reportaron que Le hicieron daño a su familia, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_10, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Desvistieron_genero",
               Dato = "Proporción de personas que reportaron que Le desvistieron, por género")

tabla_excel_fn(dataset = subset, var_prop = P3_17_11, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Cburieron-ojos_genero",
               Dato = "Proporción de personas que reportaron que Le vendaron los ojos o cubirieron la cabeza para que no viera , por género")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "RND_3", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "RND",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por RND_3")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "RND_3", var2 = "fuero", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "RND_fuero",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por RND_3 y fuero")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "RND_3", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "RND_estado",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por RND_3 y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "RND_3", var2 = "genero", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "RND_genero",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por RND_3 y genero")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,  
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "estado",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Sexo",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por sexo")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Genero",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por género")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "fuero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "Fuero",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por fuero")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_p, var1 = "RND_3", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_psicologica_traslado", nombre = "RND_corporacion",
               Dato = "Proporción de personas que reportaron ser torturadas psicológicamente durante el traslado, por RND_3 y corporación que detiene")

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
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Asfixiaron_genero",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Impidieron-respirar_genero",
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Patearon_genero",
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Golpearon_genero",
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Quemaron_genero",
               Dato = "Proporción de personas que reportaron que Le quemaron..")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Descargas_genero",
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Aplastaron_genero",
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Hirieron_genero",
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Agujas_genero",
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Dispararon_genero",
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Acoso_genero",
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Organos-sexuales_genero",
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Actividad-sexual_genero",
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada..., por género")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "genero", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Otra_genero",
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
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "Fueron",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado, por fuero")

tabla_excel_fn(dataset = subset, var_prop = tortura_tra_f, var1 = "RND_3", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Tortura", seccion = "Tortura_fisica_traslado", nombre = "RND_corporacion",
               Dato = "Proporción de personas que reportaron ser torturadas físicamente durante el traslado, por RND_3 y corporación que detiene")








# Abuso policial y personas LGBTQ+


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









# Policía


tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Flagrancia_año",
               Dato = "Proporción de personas detenidas por flagrancia, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Orden-det_año",
               Dato = "Proporción de personas detenidas por con orden de detención, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Inspeccion_año",
               Dato = "Proporción de personas detenidas después de una inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Det-ninguna_año",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia/orden de detención/inspección), por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Flagrancia_año_estado",
               Dato = "Proporción de personas detenidas por flagrancia, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA,
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Orden-det_año_estado", 
               Dato = "Proporción de personas detenidas por con orden de detención, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Inspeccion_año_estado",
               Dato = "Proporción de personas detenidas después de una inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Det-ninguna_año_estado",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia/orden de detención/inspección), por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Flagrancia_año_estado_sexo",
               Dato = "Proporción de personas detenidas por flagrancia, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Orden-det_año_estado_sexo",
               Dato = "Proporción de personas detenidas por con orden de detención, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = inspeccion, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Inspeccion_año_estado_sexo",
               Dato = "Proporción de personas detenidas después de una inspección, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Tipo-detencion", nombre = "Det-ninguna_año_estado_sexo",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia/orden de detención/inspección), por año y estado de arresto y sexo")

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
                 carpeta = "Policia", seccion = "Inspeccion_sexo_delito", nombre = paste0("Sexo",i),
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
                 carpeta = "Policia", seccion = "Flagrancia_delito_estado", nombre = paste0("Estado",i),
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

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,  
               carpeta = "Policia", seccion = "Orde-det", nombre = "Autoridad",
               Dato = "Proporción de personas detenidas por orden de detención, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = orden_det, var1 = "Estado_arresto", var2 ="P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Orde-det", nombre = "Estado",
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


tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Det-ninguna", nombre = paste0("Nacional",i),
               Dato = "Proporción de personas detenidas por ninguna de las anteriores")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Edad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte2", nombre = "D42", 
               Dato = "Proporción de personas detenidas por ninguna de las anteriores, por edad")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Escolaridad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte2", nombre = "D43", 
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
               carpeta = "Policia", seccion = "Det-ninguna_autoridad", nombre = paste0("Autoridad",i),
               Dato = "Proporción de personas detenidas por ninguna de las anteriores, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = det_ninguna, var1 = "Estado_arresto", var2 ="P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Det-ninguna_estado", nombre = paste0("Estado",i),
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
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Flagrancia_estado",
               Dato = "Proporción de personas detenidas en flagrancia cuya detención no fue inmediata (P3_9), por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = "orden_det", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Orden-detencion_estado",
               Dato = "Proporción de personas detenidas con orden de detención cuya detención no fue inmediata (P3_9), por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = "inspeccion", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Inspeccion_estado",
               Dato = "Proporción de personas detenidas después de una inspección cuya detención no fue inmediata (P3_9), por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = "det_ninguna", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Ninguna_estado",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia / orden de detención / inspección) cuya detención no fue inmediata (P3_9), por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = "flagrancia", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Flagrancia",
               Dato = "Proporción de personas detenidas en flagrancia cuya detención no fue inmediata (P3_9), por sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = "orden_det", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Orden-detencion_sexo",
               Dato = "Proporción de personas detenidas con orden de detención cuya detención no fue inmediata (P3_9), por sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = "inspeccion", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Inspecion",
               Dato = "Proporción de personas detenidas después de una inspección cuya detención no fue inmediata (P3_9), por sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = "det_ninguna", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Ninguna_sexo",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia / orden de detención / inspección) cuya detención no fue inmediata (P3_9), por sexo")


tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = "flagrancia", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Flagrancia_estado_sexo",
               Dato = "Proporción de personas detenidas en flagrancia cuya detención no fue inmediata (P3_9), por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = "orden_det", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Orden-det_estado_sexo",
               Dato = "Proporción de personas detenidas con orden de detención cuya detención no fue inmediata (P3_9), por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = "inspeccion", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Inspeccion_estado_sexo",
               Dato = "Proporción de personas detenidas después de una inspección cuya detención no fue inmediata (P3_9), por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = detencion_no_inmediata, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = "det_ninguna", filtervalue = 1, 
               carpeta = "Policia", seccion = "Det-no-inmediata", nombre = "Ninguna_estado_sexo",
               Dato = "Proporción de personas detenidas por ninguna (flagrancia / orden de detención / inspección) cuya detención no fue inmediata (P3_9), por estado de arresto y sexo")

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
               carpeta = "Policia", seccion = "Autoridad-accion_nacionl", nombre = "Disparo",
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

#D55


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
             file      = file.path(paste0("Descriptives/Output/Detenciones/Policia/Autoridad-accion_estado_sexo.xlsx"),
                                   fsep = "/"),  
             sheetName = paste0("Estado_sexo",j),
             append    = T,
             row.names = F)
}



#D56


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
             file      = file.path(paste0("Data/descriptives/Policia_Parte3_cruzadas.xlsx"),
                                   fsep = "/"),  
             sheetName = paste0("D56_",j),
             append    = T,
             row.names = F)
}

#D57


for (j in c("P3_13_01", "P3_13_02", "P3_13_03", "P3_13_04", "P3_13_05", "P3_13_06", "P3_13_07", "P3_13_08", "P3_13_09",
            "P3_13_10", "P3_13_11", "P3_13_12")){
  
  df_j = tibble()
  
  for(i in delito_grupos) {
    
    Dato = paste0("Proporción de personas que reportaron \"sí\" a ",j,", acusadas (entre otros) de ",i,", por sexo, y estado de arresto")
    
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
             file      = file.path(paste0("Data/descriptives/Policia_Parte3_cruzadas.xlsx"),
                                   fsep = "/"),  
             sheetName = paste0("D57_",j),
             append    = T,
             row.names = F)
}

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = controles_cooperativos, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, seccion = "Policia_Parte3", nombre = paste0("D58_",i), 
                 Dato = paste0("Proporción de personas que reportaron controles cooperativos, acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = controles_cooperativos, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", seccion = "Policia_Parte3", nombre = paste0("D59_",i), 
                 Dato = paste0("Proporción de personas que reportaron controles cooperativos, acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = control_contacto, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, seccion = "Policia_Parte3", nombre = paste0("D60_",i), 
                 Dato = paste0("Proporción de personas que reportaron controles de contacto, acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = control_contacto, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", seccion = "Policia_Parte3", nombre = paste0("D61_",i), 
                 Dato = paste0("Proporción de personas que reportaron controles de contacto, acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = sometimiento, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, seccion = "Policia_Parte3", nombre = paste0("D62_",i), 
                 Dato = paste0("Proporción de personas que reportaron técnicas de sometimiento, acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = sometimiento, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", seccion = "Policia_Parte3", nombre = paste0("D63_",i), 
                 Dato = paste0("Proporción de personas que reportaron técnicas de sometimiento, acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = tacticas_defensivas, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, seccion = "Policia_Parte3", nombre = paste0("D64_",i), 
                 Dato = paste0("Proporción de personas que reportaron tacticas defensivas, acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = tacticas_defensivas, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", seccion = "Policia_Parte3", nombre = paste0("D65_",i), 
                 Dato = paste0("Proporción de personas que reportaron tacticas defensivas, acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = fuerza_letal, var1 = "Sexo", var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, seccion = "Policia_Parte3", nombre = paste0("D66_",i), 
                 Dato = paste0("Proporción de personas que reportaron fuerza letal, acusadas (entre otros) por ",i," por sexo"))
}

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = fuerza_letal, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
                 varfilter = i, filtervalue = "1", seccion = "Policia_Parte3", nombre = paste0("D67_",i), 
                 Dato = paste0("Proporción de personas que reportaron fuerza letal, acusadas (entre otros) por ",i," por sexo y estado de arresto"))
}

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_nacional", nombre = "Identifico",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó...")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = NA, var2 = NA, var3 = NA, 
               carpeta = "Policia", 
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
               carpeta = "Policia", seccion = "Policia-hizo_año", nombre = "Identifico_año",
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
               carpeta = "Policia", seccion = "Policia-hizo", nombre = "Dijo-detencion_año_estado",
               Dato = "Proporción de personas que reportaron que le dijeron por qué lo(a) detuvieron?, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_5, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo", nombre = "Informo-derechos_año_estado",
               Dato = "Proporción de personas que reportaron la autoridad le informó sobre sus derechos..., por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_6, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_estado", nombre = "Dijo-donde_año_estado",
               Dato = "Proporción de personas que reportaron que la autoridad le dijo a dónde..., por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_1, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_estado", nombre = "Uniformado_año_estado",
               Dato = "Proporción de personas que reportaron que la autoridad se identificó..., por sexo, añode arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_14_2, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Policia-hizo_año_estado", nombre = "Uniformado_año_estado",
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
               carpeta = "Policia", seccion = "Momento-detencion_autoridad", nombre = "Obedecer_autoridad",
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
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte4", nombre = "D84_3", 
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
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Obedecer_autoridad_estado",
               Dato = "Proporción de personas que reportaron obedecer las órdenes..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Arma-punzo_autoridad_estado",
               Dato = "Proporción de personas que portaban algún arma punzo cortante..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_3, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Arma-fuego_autoridad_estado",
               Dato = "Proporción de personas que portaban algún arma de fuego..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_4, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Amenazaron_autoridad_estado",
               Dato = "Proporción de personas que amenazaron a alguien con el arma..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_5, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Dispararon_autoridad_estado",
               Dato = "Proporción de personas que dispararon el arma..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_6, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Manipularon_autoridad_estado",
               Dato = "Proporción de personas que manipularon algún objeto..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_7, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA,
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado", nombre = "Sobornaron_autoridad_estado",
               Dato = "Proporción de personas que trataron de sobornar..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Obedecer_autoridad_estado_año",
               Dato = "Proporción de personas que trataron de defenderse..., por estado y año de arresto y autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_15_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, 
               carpeta = "Policia", seccion = "Momento-detencion_autoridad_estado_año", nombre = "Escapar_autoridad_estado_año",
               Dato = "Proporción de personas que trataron de escapar..., por estado y año de arresto y autoridad que detiene")

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
               carpeta = "Policia", seccion = "Autoridad-realizo_año_sexo_estado", nombre = "Amenazaron-cargos_año_sexo_estado",
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
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte4", nombre = "D103_6", 
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

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_1", 
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_2", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_3", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_4", 
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_5", 
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_6", 
               Dato = "Proporción de personas que reportaron que Le quemaron..")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_7", 
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_8", 
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_9", 
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_10", 
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_11", 
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_12", 
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_13", 
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_14", 
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D104_15", 
               Dato = "Proporción de personas que reportaron que Otra agresión física")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_1", 
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_2", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_3", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_4", 
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_5", 
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_6", 
               Dato = "Proporción de personas que reportaron que Le quemaron..por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_7", 
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_8", 
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_9", 
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_10", 
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_11", 
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_12", 
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_13", 
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_14", 
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D105_15", 
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_1", 
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_2", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_3", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_4", 
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_5", 
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_6", 
               Dato = "Proporción de personas que reportaron que Le quemaron..por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_7", 
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_8", 
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_9", 
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_10", 
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_11", 
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_12", 
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_13", 
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_14", 
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D106_15", 
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_1", 
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_2", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_3", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_4", 
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_5", 
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_6", 
               Dato = "Proporción de personas que reportaron que Le quemaron..por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_7", 
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_8", 
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_9", 
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_10", 
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_11", 
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_12", 
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_13", 
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_14", 
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D107_15", 
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_1", 
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_2", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_3", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_4", 
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_5", 
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_6", 
               Dato = "Proporción de personas que reportaron que Le quemaron..por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_7", 
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_8", 
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_9", 
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_10", 
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_11", 
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_12", 
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_13", 
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_14", 
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D108_15", 
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_1", 
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_2", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_3", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_4", 
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_5", 
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_6", 
               Dato = "Proporción de personas que reportaron que Le quemaron..por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_7", 
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_8", 
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_9", 
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_10", 
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_11", 
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_12", 
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_13", 
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_14", 
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D109_15", 
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor autoridad que detiene ")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_1", 
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_2", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_3", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_4", 
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_5", 
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_6", 
               Dato = "Proporción de personas que reportaron que Le quemaron..por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_7", 
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_8", 
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_9", 
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_10", 
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_11", 
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_12", 
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_13", 
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_14", 
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada...por autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D110_15", 
               Dato = "Proporción de personas que reportaron que Otra agresión físicapor autoridad que detiene y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_1", 
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_2", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_3", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_4", 
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_5", 
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_6", 
               Dato = "Proporción de personas que reportaron que Le quemaron..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_7", 
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_8", 
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_9", 
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_10", 
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_11", 
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_12", 
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_13", 
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_14", 
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada..., por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D111_15", 
               Dato = "Proporción de personas que reportaron que Otra agresión física, por autoridad que detiene  y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_1", 
               Dato = "Proporción de personas que reportaron que Ataron su cuerpo ..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_2", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar asfixiándolo..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_3", 
               Dato = "Proporción de personas que reportaron que Le impidieron respirar o metieron su cabeza en agua..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_4", 
               Dato = "Proporción de personas que reportaron que Le patearon o golpearon con las manos..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_5", 
               Dato = "Proporción de personas que reportaron que Le golpearon con objetos..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_6", 
               Dato = "Proporción de personas que reportaron que Le quemaron..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_7", 
               Dato = "Proporción de personas que reportaron que Le dieron descargas eléctricas..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_8", 
               Dato = "Proporción de personas que reportaron que Aplastaron su cuerpo..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_9", 
               Dato = "Proporción de personas que reportaron que Le hirieron con algún cuchillo, navaja..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_10", 
               Dato = "Proporción de personas que reportaron que Le encajaron agujas en dedos..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_11", 
               Dato = "Proporción de personas que reportaron que Le hirieron por el disparo..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_12", 
               Dato = "Proporción de personas que reportaron que Le agredieron mediante acoso sexual..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_13", 
               Dato = "Proporción de personas que reportaron que Le lastimaron sus órganos sexuales..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_14", 
               Dato = "Proporción de personas que reportaron que Fue obligado ... actividad sexual no deseada..., por autoridad que detiene y estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_18_15, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D112_15", 
               Dato = "Proporción de personas que reportaron que Otra agresión física, por autoridad que detiene y  estado y año de arresto")


tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_1", 
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_2", 
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_3", 
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_4", 
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_5", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_6", 
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_7", 
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_8", 
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_9", 
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_10", 
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_11", 
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_12", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_13", 
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_14", 
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_15", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D113_16", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_1", 
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_2", 
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_3", 
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_4", 
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_5", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_6", 
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_7", 
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_8", 
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_9", 
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_10", 
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_11", 
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_12", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_13", 
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_14", 
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_15", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D114_16", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_1", 
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_2", 
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_3", 
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_4", 
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_5", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_6", 
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_7", 
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_8", 
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_9", 
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_10", 
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_11", 
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_12", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_13", 
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_14", 
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_15", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D115_16", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_1", 
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_2", 
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_3", 
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_4", 
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_5", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_6", 
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_7", 
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_8", 
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_9", 
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_10", 
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_11", 
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_12", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_13", 
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_14", 
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_15", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D116_16", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_1", 
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_2", 
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_3", 
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_4", 
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_5", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_6", 
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_7", 
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_8", 
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_9", 
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_10", 
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_11", 
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_12", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_13", 
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_14", 
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_15", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D117_16", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por sexo, estado y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_1", 
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_2", 
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_3", 
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_4", 
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_5", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_6", 
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_7", 
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_8", 
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_9", 
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_10", 
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_11", 
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_12", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_13", 
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_14", 
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_15", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D118_16", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_1", 
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_2", 
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_3", 
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_4", 
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_5", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_6", 
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_7", 
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_8", 
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_9", 
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_10", 
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_11", 
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_12", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_13", 
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_14", 
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_15", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D119_16", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_1", 
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por autoridad que detiene, de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_2", 
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_3", 
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_4", 
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_5", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_6", 
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_7", 
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_8", 
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_9", 
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_10", 
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_11", 
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_12", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_13", 
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_14", 
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_15", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D120_16", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por autoridad que detiene y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_1", 
               Dato = "Proporción de personas que reportaron que las llevaron a la Agencia del Ministerio Público, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_2", 
               Dato = "Proporción de personas que reportaron que las llevaron con un Juez de lo penal, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_3", 
               Dato = "Proporción de personas que reportaron que las llevaron A una instalación de la policía..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_4", 
               Dato = "Proporción de personas que reportaron que las llevarona un centro de arraigo, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_5", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro penitenciario, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_6", 
               Dato = "Proporción de personas que reportaron que las llevaron a una oficina de gobierno..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_7", 
               Dato = "Proporción de personas que reportaron que las llevaron a una casa particular, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_8", 
               Dato = "Proporción de personas que reportaron que las llevaron a un establecimiento comercial..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_9", 
               Dato = "Proporción de personas que reportaron que las mantuvieron en un vehículo, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_10, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_10", 
               Dato = "Proporción de personas que reportaron que las llevaron a un terreno baldío, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_11, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_11", 
               Dato = "Proporción de personas que reportaron que las llevaron a una zona militar..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_12, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_12", 
               Dato = "Proporción de personas que reportaron que las llevaron a un centro de detención para migrantes, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_13, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_13", 
               Dato = "Proporción de personas que reportaron que las llevaron a un hospital, clínica..., por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_14, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_14", 
               Dato = "Proporción de personas que reportaron que las llevaron a otro lugar, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_15", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_sabe, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_19_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D121_16", 
               Dato = "Proporción de personas que reportaron que las llevaron a No_responde, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_1", 
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_2", 
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_3", 
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_4", 
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_5", 
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_6", 
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_7", 
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_8", 
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_9", 
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_10", 
               Dato = "Proporción de personas que reportaron que pasaron No_sabe")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D122_11", 
               Dato = "Proporción de personas que reportaron que pasaron No_responde")


tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_1", 
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por  año")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_2", 
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_3", 
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_4", 
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_5", 
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_6", 
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_7", 
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_8", 
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_9", 
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_10", 
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D123_11", 
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por  año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_1", 
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_2", 
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_3", 
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_4", 
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_5", 
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_6", 
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_7", 
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_8", 
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_9", 
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_10", 
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D124_11", 
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por  año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_1", 
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_2", 
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_3", 
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_4", 
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_5", 
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_6", 
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_7", 
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_8", 
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_9", 
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_10", 
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D125_11", 
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por sexo y año ")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_1", 
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_2", 
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_3", 
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_4", 
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_5", 
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_6", 
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_7", 
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_8", 
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_9", 
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_10", 
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D126_11", 
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_1", 
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, porautoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_2", 
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_3", 
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_4", 
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_5", 
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_6", 
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_7", 
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_8", 
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_9", 
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_10", 
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D127_11", 
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_1", 
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, porautoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_2", 
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_3", 
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_4", 
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_5", 
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_6", 
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_7", 
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_8", 
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_9", 
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_10", 
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D128_11", 
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por autoridad que detiene,  y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_1", 
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_2", 
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_3", 
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_4", 
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_5", 
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_6", 
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_7", 
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_8", 
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_9", 
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_10", 
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D129_11", 
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_01, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_1", 
               Dato = "Proporción de personas que reportaron que pasaron hasta 30 minutos, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_02, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_2", 
               Dato = "Proporción de personas que reportaron que pasaron más de 30 minutos hasta 1 hora, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_03, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_3", 
               Dato = "Proporción de personas que reportaron que pasaron más de 1 hora hasta 2 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_04, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_4", 
               Dato = "Proporción de personas que reportaron que pasaron más de 2 horas hasta 4 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_05, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_5", 
               Dato = "Proporción de personas que reportaron que pasaron más de 4 horas hasta 6 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_06, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_6", 
               Dato = "Proporción de personas que reportaron que pasaron de 6 horas hasta 24 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_07, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_7", 
               Dato = "Proporción de personas que reportaron que pasaron de 24 horas hasta 48 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_08, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_8", 
               Dato = "Proporción de personas que reportaron que pasaron más de 48 horas hasta 72 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_09, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_9", 
               Dato = "Proporción de personas que reportaron que pasaron más de 72 horas, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_98, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_10", 
               Dato = "Proporción de personas que reportaron que pasaron No_sabe, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_20_99, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D130_11", 
               Dato = "Proporción de personas que reportaron que pasaron No_responde, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D131_1", 
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D131_2", 
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D131_3", 
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D131_4", 
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D132_1", 
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D132_2", 
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D132_3", 
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D132_4", 
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D133_1", 
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D133_2", 
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D133_3", 
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D133_4", 
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D134_1", 
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D134_2", 
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D134_3", 
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D134_4", 
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por sexo, año  de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D135_1", 
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D135_2", 
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D135_3", 
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D135_4", 
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D136_1", 
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D136_2", 
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D136_3", 
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D136_4", 
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D137_1", 
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D137_2", 
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D137_3", 
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D137_4", 
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D138_1", 
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por autoridad que detiene, año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D138_2", 
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por autoridad que detiene, año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D138_3", 
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por autoridad que detiene, año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D138_4", 
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por autoridad que detiene, año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D139_1", 
               Dato = "Proporción de personas que respondieron \"sí\" a P3_21_1, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_2, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D139_2", 
               Dato = "Proporción de personas que respondieron \"no\" a P3_21_1, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_8, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D139_3", 
               Dato = "Proporción de personas que respondieron \"no sabe\" a P3_21_1, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_21_1_9, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D139_4", 
               Dato = "Proporción de personas que respondieron \"no responde\" a P3_21_1, por autoridad que detiene, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D140", 
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D141", 
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D142", 
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Sexo", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D143", 
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por sexo y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D144", 
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por sexo, año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D145", 
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por autoridad que detiene")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Estado_arresto", var2 = "P3_2", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D146", 
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por autoridad que detiene, y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "P3_2", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D147", 
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por autoridad que detiene, y año de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_22_1, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = "P3_2", 
               varfilter = NA, filtervalue = NA, seccion = "Policia_Parte5", nombre = "D148", 
               Dato = "Proporción de personas que respondieron que si les daba dinero lo dejarían ir, por autoridad que detiene, año y estado de arresto")






# Inspecciones + Inspecciones Efectivas + Inspecciones y cumplimiento del debido proceso


tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E1", 
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E2", 
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E3", 
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E4", 
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E5", 
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Anio_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E6", 
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E7", 
               Dato = "Proporción de personas que fueron desvestidas durante la inspección, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E8", 
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E9", 
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = "Anio_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E10", 
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E11", 
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E12", 
               Dato = "Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, por autoridad que detiene")

for (i in delitos) {
  tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = "1", seccion = "Inspecciones", nombre = paste0("E13_",i), 
                 Dato = paste0("Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, entre las acusadas (entre otros) por ",i))
}

for (i in delito_grupos) {
  tabla_excel_fn(dataset = subset, var_prop = P3_12_3, var1 = NA, var2 = NA, var3 = NA, 
                 varfilter = i, filtervalue = 1, seccion = "Inspecciones", nombre = paste0("E14_",i), 
                 Dato = paste0("Proporción de personas que reportan que la autoridad encontró el objeto que buscaba durante la inspección, entre las acusadas (entre otros) por ",i))
}

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E15", 
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E16", 
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E17", 
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E18", 
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E19", 
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Anio_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E20", 
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_4, var1 = "Anio_arresto", var2 = "Estado_arresto", var3 = "Sexo", 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E21", 
               Dato = "Proporción de personas que la autoridad les sembró algún objeto durante la inspección, por año y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = P3_12_1, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E22", 
               Dato = "Proporción de personas que reportan que la autoridad lo desvistió durante la inspección")

tabla_excel_fn(dataset = subset, var_prop = P3_12_5, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E23", 
               Dato = "Proporción de personas que reportan que la autoridad videograbó la inspección durante la inspección")

tabla_excel_fn(dataset = subset, var_prop = P3_12_2, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Inspecciones", nombre = "E24", 
               Dato = "Proporción de personas que reportan que la autoridad le dijo qué objeto buscaba durante la inspección")









# Apego de la flagrancia a la constitucionalidad

tabla_excel_fn(dataset = subset, var_prop = flagrancia_const, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Flagrancia_constitucional", nombre = "F1", 
               Dato = "Proporción de personas detenidas en flagrancia constitucional, por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = flagrancia_const, var1 = "P3_2", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "Flagrancia_constitucional", nombre = "F2", 
               Dato = "Proporción de personas detenidas en flagrancia constitucional, por autoridad que detiene")








# PPO


tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G1", 
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_vehiculo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_1", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo de vehículo")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_casa_hab", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_2", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo a casa habitación")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_negocio", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_3", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo a negocio")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_transporte_pub", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_4", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo a transporte público")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_transeunte", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_5", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo a transeunte en la via pública")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_autopartes", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_6", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo de autopartes")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Robo_otros", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_7", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Robo en forma distinta a las anteriores")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Posesion_drogas", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_8", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Posesión ilegal de drogas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Comercio_drogas", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_9", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Comercio ilegal de drogas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Lesiones", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_10", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Lesiones")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Hom_culposo", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_11", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Homicidio culposo")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Hom_doloso", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_12", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Homicidio doloso")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Portacion_armas", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_13", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Portación ilegal de armas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Incum_asis_fam", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_14", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Incumplimiento de obligaciones de asistencia familiar")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Violencia_fam", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_15", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Violencia familiar")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Danio_prop", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_16", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Daño a la propiedad")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Secuestro", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_17", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Secuestro y secuestro express")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Violacion_sexual", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_18", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Violación sexual")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Fraude", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_19", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Fraude")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Delincuencia_org", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_20", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Delincuencia organizada")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Otros_sexuales", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_21", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Otros delitos sexuales")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Extorsion", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_22", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Extorsión")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Privacion_de_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_23", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Privación de la libertad")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Abuso_de_conf", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_24", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Abuso de confianza")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Amenazas", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_25", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Amenazas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Otros", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_26", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por Otros delitos")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "No_sabe", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_27", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por No_sabe")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "No_responde", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G2_28", 
               Dato = "Proporción de personas que ya fueron sentenciadas entre las acusadas (entre otros) por No_responde")

tabla_excel_fn(dataset = subset, var_prop = proceso_en_libertad, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G3", 
               Dato = "Proporción de personas que llevaron su proceso en libertad o no, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = proceso_en_libertad, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G4", 
               Dato = "Proporción de personas que llevaron su proceso en libertad, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = proceso_en_libertad, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G5", 
               Dato = "Proporción de personas que llevaron su proceso en libertad, por sexo y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G6", 
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO, por año de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = "RND_3", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G7", 
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO, antes y después de la implementación del RND_3")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G8", 
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO, por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G9", 
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO, por sexo")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G10", 
               Dato = "Proporción de personas que ya fueron sentenciadas por delitos de PPO y delitos de no-PPO, por admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, seccion = "PPO_Parte1", nombre ="G11", 
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Anio_arresto", var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, seccion = "PPO_Parte1", nombre ="G12", 
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "RND_3", var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, seccion = "PPO_Parte1", nombre ="G13", 
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas, antes y después de la implementación del RND_3")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, seccion = "PPO_Parte1", nombre ="G14", 
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas, por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "Sexo", var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, seccion = "PPO_Parte1", nombre ="G15", 
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas, por sexo")

tabla_excel_fn(dataset = subset, var_prop = sentenciado, var1 = "culpabilidad", var2 = NA, var3 = NA, 
               varfilter = "PPO", filtervalue = 0, seccion = "PPO_Parte1", nombre ="G16", 
               Dato = "Proporción de personas acusadas de delitos de no-PPO que ya fueron sentenciadas, por admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G17_1", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G17_2", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = NA, var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G17_3", 
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "culpabilidad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G18_1", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "culpabilidad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G18_2", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "culpabilidad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G18_3", 
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G19_1", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por PPO y no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G19_2", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por PPO y no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G19_3", 
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por PPO y no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "culpabilidad", var2 = "PPO", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G20_1", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por admisión de culpabilidad y PPO vs no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "culpabilidad", var2 = "PPO", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G20_2", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por admisión de culpabilidad y PPO vs no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "culpabilidad", var2 = "PPO", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G20_3", 
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por admisión de culpabilidad y PPO vs no-PPO")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G21_1", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por PPO y no-PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G21_2", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por PPO y no-PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G21_3", 
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por PPO y no-PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G22_1", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por PPO y no-PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G22_2", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por PPO y no-PPO y estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G22_3", 
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por PPO y no-PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G23_1", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por PPO y no-PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G23_2", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por PPO y no-PPO y estado de arresto y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G23_3", 
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por PPO y no-PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G24_1", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por condición de llevar el proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G24_2", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por condición de llevar el proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G24_3", 
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por condición de llevar el proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = Antes_del_juicio, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G25_1", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables antes de iniciar el juicio, por condición de llevar el proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Despues_de_las_pruebas, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G25_2", 
               Dato = "Proporción de personas sentenciadas que perciben que el juez los consideraba culpables después de presentadas las pruebas, por condición de llevar el proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = Nunca_vio_al_juez, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte1", nombre ="G25_3", 
               Dato = "Proporción de personas sentenciadas que perciben que nunca vieron al juez, por condición de llevar el proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "Estado_arresto", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G26", 
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G27", 
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por PPO  vs no PPO")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G28", 
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por PPO  vs no PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G29", 
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por PPO  vs no PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G30", 
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por PPO  vs no PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G31", 
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por condición de haber llevado su proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "proceso_en_libertad", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G32", 
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por condición de haber llevado su proceso en libertad o no y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "proceso_en_libertad", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G33", 
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por condición de haber llevado su proceso en libertad o no y sexo")

tabla_excel_fn(dataset = subset, var_prop = escuchado_x_juez, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G34", 
               Dato = "Proporción de personas sentenciadas que se sintieron algo o mucho escuchadas por el juez, por condición de haber llevado su proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G35", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por PPO  vs no PPO")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G36", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por PPO  vs no PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G37", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por PPO  vs no PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G38", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por PPO  vs no PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G39", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por condición de haber llevado su proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "proceso_en_libertad", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G40", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por condición de haber llevado su proceso en libertad o no y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "proceso_en_libertad", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G41", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por condición de haber llevado su proceso en libertad o no y sexo")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_MP, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G42", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados en el MP, por condición de haber llevado su proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G43", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por PPO  vs no PPO")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G44", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por PPO  vs no PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G45", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por PPO  vs no PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G46", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por PPO  vs no PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G47", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por condición de haber llevado su proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "proceso_en_libertad", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G48", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por condición de haber llevado su proceso en libertad o no y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "proceso_en_libertad", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G49", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por condición de haber llevado su proceso en libertad o no y sexo")

tabla_excel_fn(dataset = subset, var_prop = satisfecho_abogado_juicio, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G50", 
               Dato = "Proporción de personas sentenciadas que están muy o algo satisfechas con sus abogados durante el juicio, por condición de haber llevado su proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G51", 
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por PPO  vs no PPO")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G52", 
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por PPO  vs no PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G53", 
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por PPO  vs no PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G54", 
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por PPO  vs no PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G55", 
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por condición de haber llevado su proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "proceso_en_libertad", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G56", 
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por condición de haber llevado su proceso en libertad o no y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "proceso_en_libertad", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G57", 
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por condición de haber llevado su proceso en libertad o no y sexo")

tabla_excel_fn(dataset = subset, var_prop = proceso_justo, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G58", 
               Dato = "Proporción de personas sentenciadas que que fueron tratadas de manera justa durante su proceso, por condición de haber llevado su proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "PPO", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G59", 
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por PPO  vs no PPO")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "PPO", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G60", 
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por PPO  vs no PPO y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "PPO", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G61", 
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por PPO  vs no PPO y sexo")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "PPO", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G62", 
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por PPO  vs no PPO y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "proceso_en_libertad", var2 = NA, var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G63", 
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por condición de haber llevado su proceso en libertad o no")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "proceso_en_libertad", var2 = "Estado_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G64", 
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por condición de haber llevado su proceso en libertad o no y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "proceso_en_libertad", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G65", 
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por condición de haber llevado su proceso en libertad o no y sexo")

tabla_excel_fn(dataset = subset, var_prop = sentencia_justa, var1 = "proceso_en_libertad", var2 = "culpabilidad", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G66", 
               Dato = "Proporción de personas sentenciadas que que consideran que sun sentencia fue muy o algo justa, por condición de haber llevado su proceso en libertad o no y admisión de culpabilidad")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_f, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G68", 
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos del fuero federal, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_c, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G69", 
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos del fuero común, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = ambos_fueros, var1 = "Estado_arresto", var2 = "Sexo", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G70", 
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos de ambos fueros, por estado de arresto y sexo")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_f, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G71", 
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos del fuero federal, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = solo_fuero_c, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G72", 
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos del fuero común, por año y estado de arresto")

tabla_excel_fn(dataset = subset, var_prop = ambos_fueros, var1 = "Estado_arresto", var2 = "Anio_arresto", var3 = NA, 
               varfilter = NA, filtervalue = NA, seccion = "PPO_Parte2", nombre ="G73", 
               Dato = "Proporción de personas que fueron arrestadas sólo por delitos de ambos fueros, por año y  estado de arresto")
