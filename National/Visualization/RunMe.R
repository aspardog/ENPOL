## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            ENPOL - RunMe File
##
## Author(s):         Santiago Pardo   (spardo@worldjusticeproject.org)
##                    Cristina Alvarez (calvarez@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     Abril 2, 2024
##
## This version:      Abril 2, 2024
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 0.  Presettings                                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required Packages, Fonts, ggplot theme, color palettes, comparison countries and other general routines are
# loaded from the following script:
source("Code/settings.R")
source("Code/Parte A/proceso_justo.R")
source("Code/Parte A/proceso_justo_percepcion.R")
source("Code/Parte A/uso_excesivo_fuerza.R")
source("Code/Parte A/corrupcion.R")
source("Code/Parte A/tortura.R")
source("Code/Parte A/detenciones.R")
source("Code/Parte A/prision_preventiva.R")

# Loading plotting functions from GitHub
source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/loading.R")
loadVIZ(set = "ENPOL")

# Loading data
load(paste0(path2SP,
            "/National/Data_cleaning/Output/Main_database.RData"))

mapa <- st_read(paste0(path2SP,"/National/Visualization/Input/shp/México_Estados.shp")) %>%
  mutate(
    ESTADO = 
      case_when(
        ESTADO == "México" ~ "Estado de México",
        T ~ ESTADO
      )
  )

National <- T

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Reporte Nacional                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (National == T) {
  
  Estados <- 'National'
  
} else {
  
  Estados <- Main_database %>%
    select(Estado_arresto) %>%
    distinct() %>%
    drop_na() %>%
    pull() %>%
    as.character()
  
}

for (i in Estados) {
  
  if(National == T){ 
    
    Estados <- "National"
    
    master_data.df <- Main_database %>% 
      filter(Anio_arresto >= as.numeric(2008)) %>% 
      filter(NSJP == 1) 
    
    savePath <- "Nacional"
    
    # Definir la ruta al directorio "nacional"
    nacional_dir <- paste0(
      path2SP, "/National/Visualization", 
      "/Output/Debido proceso/", 
      savePath
    )
    
    # Listar todas las subcarpetas dentro de "nacional"
    subdirs <- list.dirs(nacional_dir, 
                         recursive = FALSE, full.names = TRUE)
    
    # Borrar todas las subcarpetas dentro de "nacional"
    sapply(subdirs, unlink, recursive = TRUE)
    
  } else {
    
    master_data.df <- Main_database %>% 
      filter(Estado_arresto == i) %>%
      filter(Anio_arresto >= as.numeric(2008)) %>% 
      filter(NSJP == 1) 
    
    savePath <- paste0("Estados/", i)
    
    # Definir la ruta al directorio "nacional"
    nacional_dir <- paste0(
      path2SP, "/National/Visualization", 
      "/Output/Debido proceso/", 
      savePath
    )
    
    # Listar todas las subcarpetas dentro de "nacional"
    subdirs <- list.dirs(nacional_dir, 
                         recursive = FALSE, full.names = TRUE)
    
    # Borrar todas las subcarpetas dentro de "nacional"
    sapply(subdirs, unlink, recursive = TRUE)
    
    dir.create(paste0(
      path2SP,
      "/National/Visualization",
      "/Output/Debido proceso/",
      savePath)
    )
  }
  
  print("CAPÍTULO 1: DEBIDO PROCESO")
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ### Capítulo 1: Debido proceso                                                                              ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  print("Generando proceso justo")
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####
  #### Proceso justo                                                                             ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Debido proceso/",
    savePath,"/Proceso justo")
  )
  
  guardar_silencio      <- guardar_silencio.fn()
  informacion_detencion <- informacion_detencion.fn()
  claridad_actores      <- claridad_actores.fn()
  defensa_oportuna      <- defensa_oportuna.fn()
  tipo_defensa          <- tipo_defensa.fn()
  tribunal_transparente <- tribunal_transparente.fn()
  tribunal_imparcial    <- tribunal_imparcial.fn()
  tribunal_presente     <- tribunal_presente.fn()
  tiempo_proceso        <- tiempo_condena.fn()
  
  proceso_justo_lista <- list('Guardar silencio en el tiempo' = guardar_silencio, 
                              'Información de la detencion'   = informacion_detencion,
                              'Claridad de actores'           = claridad_actores,
                              'Defensa oportuna'              = defensa_oportuna,
                              'Tipo de defensa'               = tipo_defensa,
                              'Tribunal transparente'         = tribunal_transparente,
                              'Tribunal imparcial'            = tribunal_imparcial,
                              'Tribunal presente'             = tribunal_presente,
                              'Tiempo proceso'                = tiempo_proceso
                              )
  
  openxlsx::write.xlsx(x = proceso_justo_lista,
    file = paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Debido proceso/",
    savePath,"/Proceso justo",
    "/proceso_justo.xlsx")
  )
  
  print("Proceso justo finalizado")
  print("Generando percepcion de proceso justo")
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####
  #### Percepción proceso justo                                                                             ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Debido proceso/",
    savePath,"/Percepcion proceso justo")
  )
  
  proceso_justo <- proceso_justo.fn()
  percepcion_indicadores <- percepcion_indicadores.fn()
  procedimiento <- procedimiento.fn()
  percepcion_procedimiento <- percepcion_procedimiento.fn()
  culpabilidad <- culpabilidad.fn()
  percepcion_culpabilidad <- percepcion_culpabilidad.fn()
  escucha <- escucha.fn()
  percepcion_escucha <- percepcion_escucha.fn()
  
  proceso_percepcion_lista <- list('Percepcion proceso'       = proceso_justo, 
                                   'Percepcion indicadores'   = percepcion_indicadores,
                                   'Tipo procedimiento'       = procedimiento,
                                   'Percepcion procedimiento' = percepcion_procedimiento,
                                   'Culpabilidad'             = culpabilidad,
                                   'Percepcion culpabilidad'  = percepcion_culpabilidad,
                                   'Sentimiento de escucha'   = escucha,
                                   'Percepcion escucha'       = percepcion_escucha
                                   )
  
  openxlsx::write.xlsx(x = proceso_percepcion_lista,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/",
                         savePath,"/Percepcion proceso justo",
                         "/proceso_justo.xlsx")
  )
  
  print("Percepcion de proceso justo finalizado")
  print("Generando Uso excesivo de la fuerza")
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####
  #### Uso excesivo de la fuerza                                                                           ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Debido proceso/",
    savePath,"/Uso excesivo fuerza")
  )
  
  controles_tipo <- controles_tipo.fn()
  uso_fuerza_tiempo <- uso_fuerza_tiempo.fn()
  uso_fuerza_corporacion <- uso_fuerza_corporacion.fn()
  
  uso_fuerza_lista <- list(
    'Tipo de control'          = controles_tipo,
    'Uso fuerza tiempo'        = uso_fuerza_tiempo, 
    'Uso fuerza corporacion'   = uso_fuerza_corporacion
    
  )
  
  openxlsx::write.xlsx(x = uso_fuerza_lista,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/",
                         savePath,"/Uso excesivo fuerza",
                         "/uso_excesivo_fuerza.xlsx")
  )
  print("Uso excesivo de la fuerza finalizado")
  print("Generando corrupción")
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####
  #### Corrupción                                                                        ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Debido proceso/",
    savePath,"/Corrupción")
  )
  
  corrupcion_tiempo <- corrupcion_tiempo.fn()
  corrupcion_elementos <- corrupcion_elementos.fn()
  
  corrupcion_lista <- list('Corrupción tiempo'      = corrupcion_tiempo, 
                           'Corrupción elementos'   = corrupcion_elementos
  )
  
  openxlsx::write.xlsx(x = corrupcion_lista,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/",
                         savePath,"/Corrupción",
                         "/Corrupción.xlsx")
  )
  print("Corrupción finalizado")
  print("Generando tortura")
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####
  #### Tortura                                                                      ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Debido proceso/",
    savePath,"/Tortura")
  )
  
  tortura_tiempo <- tortura_tiempo.fn()
  tortura_tipo <- tortura_tipo.fn()
  tortura_psicologica <- tortura_psicologica.fn()
  tortura_fisica <- tortura_fisica.fn()
  tortura_RND <- tortura_RND.fn()
  
  tortura_lista <- list('Tortura tiempo'      = tortura_tiempo,
                        'Tortura tipo'        = tortura_tipo,
                        'Tortura psicologica' = tortura_psicologica,
                        'Tortura fisica'      = tortura_fisica,
                        'Tortura RND'         = tortura_RND
                        )
  
  openxlsx::write.xlsx(x = tortura_lista,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/",
                         savePath,"/Tortura",
                         "/Tortura.xlsx")
  )
  print("Tortura finalizado")
  print("Generando detenciones")
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####
  #### Detenciones                                                                   ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Debido proceso/",
    savePath,"/Detenciones")
  )
  
  detenciones_tiempo <- detenciones_tiempo.fn()
  detenciones_traslado <- tiempos_traslado.fn()
  detenciones_lugar <- lugar_traslado.fn()
  
  detenciones_lista <- list('Detenciones tiempo'      = detenciones_tiempo,
                        'Detenciones traslado' = detenciones_traslado,
                        'Detenciones lugar'      = detenciones_lugar
                        )
  
  openxlsx::write.xlsx(x = detenciones_lista,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/",
                         savePath,"/Tortura",
                         "/Detenciones.xlsx")
  )
  print("Detenciones finalizado")
  print("Generando Prisión preventiva")
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####
  #### Prision preventiva                                                                   ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Debido proceso/",
    savePath,"/Prisión preventiva")
  )
  
  pp_proporcion <- pp_proporcion.fn()
  pp_tiempo_total <- pp_tiempo_total.fn()
  pp_tiempo <- pp_tiempo.fn()
  pp_tipo <- pp_tipo.fn()
  
  pp_list <- list(
    
    'PP proporcion' = pp_proporcion,
    'PP duracion' = pp_tiempo_total,
    'PP tiempo' = pp_tiempo,
    'PP tipo' = pp_tipo
    
  )
  openxlsx::write.xlsx(x = pp_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/",
                         savePath,"/Prisión preventiva",
                         "/PP.xlsx")
  )
  print("Prisión Preventiva finalizado")
  
  
  
  print("CAPÍTULO 2: ESTRATEGIAS DE INVESTIGACIÓN")
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ### Capítulo 2: Estrategias de investigación                                                              ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  
}

