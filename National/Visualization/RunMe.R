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
source("Code/Parte A/proceso_justo.R")
source("Code/Parte A/proceso_justo_percepcion.R")
source("Code/Parte A/uso_excesivo_fuerza.R")
source("Code/Parte A/corrupcion.R")
source("Code/Parte A/tortura.R")
source("Code/Parte A/detenciones.R")
source("Code/Parte A/prision_preventiva.R")
source("Code/Parte A/indiceDP.R")


source("Code/Parte B/conclusion_proceso.R")
source("Code/Parte B/delincuencia_prolifica_emergente.R")
source("Code/Parte B/delitos_alto_impacto.R")
source("Code/Parte B/delitos_relevantes_victimas.R")
source("Code/Parte B/detenciones.R")
source("Code/Parte B/distribucion_competencia.R")
source("Code/Parte B/entrevistas_interrogatorios.R")
source("Code/Parte B/estudio_FGR.R")
source("Code/Parte B/inspecciones.R")
source("Code/Parte B/pruebas.R")
source("Code/Parte B/señalamientos.R")


source("Code/Parte C/trato_diferenciado.R")

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
  print("Generando uso excesivo de la fuerza")
  
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
  tiempo_traslado <- tiempo_traslado.fn()
  mapa_tiempo_traslado <- mapa_tiempo_traslado.fn()
  mapa_lugar_traslado <- mapa_lugar_traslado.fn()
  
  detenciones_lista <- list(
    'Detenciones tiempo'          = detenciones_tiempo,
    'Detenciones traslado'        = detenciones_traslado,
    'Tiempo de traslado'          = tiempo_traslado,
    'Mapa tiempo de traslado'     = mapa_tiempo_traslado,
    'Detenciones lugar'           = detenciones_lugar,
    'Mapa lugar de traslado'      = mapa_tiempo_traslado
    )
  
  openxlsx::write.xlsx(x = detenciones_lista,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/",
                         savePath,"/Detenciones",
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
  ppo_defensa <- ppo_defensa.fn()
  sentencias <- sentencias.fn()
  
  pp_list <- list(
    
    'PP proporcion' = pp_proporcion,
    'PP duracion' = pp_tiempo_total,
    'PP tiempo' = pp_tiempo,
    'PP tipo' = pp_tipo,
    'PPO defensa' = ppo_defensa,
    'Sentencias' = sentencias
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
  print("Generando Indicador Debido Proceso")
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####
  #### Indice de criterios minimos                                                                  ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Debido proceso/",
    savePath,"/Indicador DP")
  )
  
  indicador_general        <- indicador_general.fn()
  indicador_logit_positivo <- indicador_logit_positivo.fn()
  indicador_logit_negativo <- indicador_logit_negativo.fn()
  indicador_map            <- indicador_map.fn()
  indicador_proceso_justo  <- indicador_proceso_justo.fn()
  indicador_uso_fuerza     <- indicador_uso_fuerza.fn()
  indicador_tortura        <- indicador_tortura.fn()
  
  indicador_list <- list(
    
    'Indicador general'        = indicador_general,
    'Indicador logit positivo' = indicador_logit_positivo,
    'Indicador logit negativo' = indicador_logit_negativo,
    'Indicador mapa'           = indicador_map,
    'Indicador proceso justo'  = indicador_proceso_justo,
    'Indicador uso fuerza'     = indicador_uso_fuerza,
    'Indicador tortura'        = indicador_tortura
    

  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Debido proceso/",
                         savePath,"/Indicador DP",
                         "/Indicador DP.xlsx")
  )
  
  print("Indicador Debido Proceso finalizado")
  
  print("CAPÍTULO 2: ESTRATEGIAS DE INVESTIGACIÓN")
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ### Capítulo 2: Estrategias de investigación                                                              ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  print("Estrategias Delitos Relevantes")
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Estrategias de atención a Delitos Relevantes                                                                              ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
   dir.create(paste0(
     path2SP,
     "/National/Visualization",
     "/Output/Politica criminal/",
     savePath,"/Delitos victimas")
   )
  
  delitos_ENPOL             <- delitos_ENPOL.fn()
  delitos_ENVIPE            <- delitos_ENVIPE.fn()
  prevalentes_ENVIPE        <- prevalentes_ENVIPE.fn()
  homicidios_ENPOL_ENVIPE   <- homicidios_ENPOL_ENVIPE.fn()
  
  indicador_list <- list(
    
    'Delitos ENPOL'              = delitos_ENPOL,
    'Delitos ENVIPE'             = delitos_ENVIPE,
    'Delitos prevalentes ENVIPE' = prevalentes_ENVIPE,
    'Homicidios ENPOL ENVIPE'    = homicidios_ENPOL_ENVIPE
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Delitos victimas",
                         "/Delitos victimas.xlsx")
  )
  
  print("Delitos Víctimas finalizado")
  
  print("Atención a los delitos de alto impacto")
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Atención a los delitos de alto impacto                                                                             ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Politica criminal/",
    savePath,"/Delitos alto impacto")
  )
  
  control_alto_impacto           <- control_alto_impacto.fn()
  detencion_alto_impacto         <- detencion_alto_impacto.fn()
  terminacion_alto_impacto       <- terminacion_alto_impacto.fn()
  
  indicador_list <- list(
    
    'Control alto impacto'     = control_alto_impacto,
    'Detención alto impacto'   = detencion_alto_impacto,
    'Terminación alto impacto' = terminacion_alto_impacto
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Delitos alto impacto",
                         "/Delitos alto impacto.xlsx")
  )
  
  print("Atención a los delitos de alto impacto finalizado")
  
  print("Delincuencia prolífica y emergente ")
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Atención a fenómenos criminales: delincuencia prolífica y emergente                                                                      ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Politica criminal/",
    savePath,"/Delincuencia prolifica")
  )
  
  reincidentes         <- reincidentes.fn()
  delito_reincidencia  <- delito_reincidencia.fn()
  varios_delitos       <- varios_delitos.fn()
  delito_unico         <- delito_unico.fn()
  
  indicador_list <- list(
    
    'Reincidencia'                         = reincidentes,
    'Tipo delitos reincidencia'            = delito_reincidencia,
    'Varios delitos'                       = varios_delitos,
    'Delitos únicos'                       = delito_unico
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Delincuencia prolifica",
                         "/Delincuencia prolifica.xlsx")
  )
  
  print("Delincuencia prolífica y emergente  finalizado")
  
  
  print("Distribución de competencias ")
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Distribución de competencias                                                                     ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Politica criminal/",
    savePath,"/Distribucion competencias")
  )
  
  delitos_fuero         <- delitos_fuero.fn()
  delitos_federales     <- delitos_federales.fn()
  detenciones_federales <- detenciones_federales.fn()
  detenciones_estatales <- detenciones_estatales.fn()
  
  indicador_list <- list(
    
    'Delitos_fuero'    = delitos_fuero
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Distribucion competencias",
                         "/Distribucion competencias.xlsx")
  )
  
  print("Distribución de competencias finalizado") 
  
  
  print("Estudio FGR")
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Estudio FGR                                                                     ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Politica criminal/",
    savePath,"/Estudio FGR")
  )
  
  detenciones_FGR         <- detenciones_FGR.fn()
  detencion_GN_PF         <- detencion_GN_PF.fn()
  federales_sentenciados  <- federales_sentenciados.fn()
  conclusion_federales    <- conclusion_federales.fn()
  
  indicador_list <- list(
    
    'Detenciones FGR'          = detenciones_FGR,
    'Det_GN/PF_Ministerial'    = detencion_GN_PF,
    'Delitos_fed_senten'       = federales_sentenciados,
    'Conlusión_del_fed'        = conclusion_federales
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Estudio FGR",
                         "/Estudio FGR.xlsx")
  )
  
  print("Estudio FGR finalizado") 
  
  print("Detenciones")
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Estrategias de investigación del delito: Detenciones                                                                     ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Politica criminal/",
    savePath,"/Detenciones")
  )
  
  detenciones_temporal      <- detenciones_temporal.fn()
  detenciones_estado        <- detenciones_estado.fn()
  detencion_proceso_tiempo  <- detencion_proceso_tiempo.fn()
  
  indicador_list <- list(
    
    'Detenciones tiempo'  = detenciones_temporal,
    'Detenciones estado'    = detenciones_estado,
    'Detenciones fuero'     = delitos_fuero
    
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Detenciones",
                         "/Detenciones.xlsx")
  )
  
  print("Detenciones finalizado") 
  
  
  print("Inspecciones")
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Estrategias de investigación del delito: Inspecciones                                                                     ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Politica criminal/",
    savePath,"/Inspecciones")
  )
  
  inspecciones_comportamiento <- inspecciones_comportamiento.fn ()
  inspecciones_objeto         <- inspecciones_objeto.fn ()
  
  indicador_list <- list(
    
    'inspecciones_comportamiento'  = inspecciones_comportamiento,
    'inspecciones_objeto'          = inspecciones_objeto
    
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Inspecciones",
                         "/Inspecciones.xlsx")
  )
  
  print("Inspecciones finalizado") 
  
  
  print("Señalamientos y reconocimiento de personas")
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Estrategias de investigación del delito: eñalamientos y reconocimiento de personas                                                                     ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Politica criminal/",
    savePath,"/Señalamientos")
  )
  
  señalados                  <- señalados.fn()
  señalamientos_condiciones  <- señalamientos_condiciones.fn()
  
  indicador_list <- list(
    
    'Señalados'  = señalados,
    'Señalados condiciones'    = señalamientos_condiciones
    
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Señalamientos",
                         "/Señalamientos.xlsx")
  )
  
  print("Señalamientos y reconocimiento de personas finalizado") 
  
  print("Entrevistas")
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Estrategias de investigación del delito: Entrevistas                                                                    ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Politica criminal/",
    savePath,"/Entrevistas")
  )
  
  interrogatorio_MP                 <- interrogatorio_MP.fn()
  interrogatorio_comportamiento     <- interrogatorio_comportamiento.fn()
  tortura_detencion_MP              <- tortura_detencion_MP.fn()
  tortura_inocencia                 <- tortura_inocencia.fn()
  tortura_culpabilidad              <- tortura_culpabilidad.fn()
  
  indicador_list <- list(
    
    'Interen MP'             = interrogatorio_MP,
    'Inter Comport'          = interrogatorio_comportamiento,
    'Tortura det MP'         = tortura_detencion_MP,
    'Tortura inocencia'      = tortura_inocencia,
    'Tortura culpabilidad'   = tortura_culpabilidad
    
    
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Entrevistas",
                         "/Entrevistas.xlsx")
  )
  
  print("Entrevistas finalizado") 
  
  
  print("Pruebas")
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Estrategias de persecución penal: Pruebas                                                                    ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Politica criminal/",
    savePath,"/Pruebas")
  )
  
  pruebas_pp             <- pruebas_pp.fn()
  pruebas_conclusion     <- pruebas_conclusion.fn()
  
  indicador_list <- list(
    
    'Pruebas PP'  = pruebas_pp,
    'Pruebas conclusión'               = pruebas_conclusion
    
    
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Pruebas",
                         "/Pruebas.xlsx")
  )
  
  print("Pruebas finalizado") 
  
  
  print("Formas de concluir los procesos")
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Formas de concluir los procesos                                                                   ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Politica criminal/",
    savePath,"/Conclusion")
  )
  
  conclusion                 <- conclusion.fn()
  conclusion_presion         <- conclusion_presion.fn()
  conclusion_tiempo_proceso  <- conclusion_tiempo_proceso.fn()
  
  indicador_list <- list(
    
    'Conclusion tipo'            = conclusion,
    'Conclusion presion'         = conclusion_presion,
    'Conclusion proceso'  = conclusion_tiempo_proceso
    
    
    
    
  )
  
  openxlsx::write.xlsx(x = indicador_list,
                       file = paste0(
                         path2SP,
                         "/National/Visualization",
                         "/Output/Politica criminal/",
                         savePath,"/Conclusion",
                         "/Conclusion.xlsx")
  )
  
  print("Formas de concluir los procesos finalizado") 
  
  print("CAPÍTULO 3: TRATO DIFERENCIADO")
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ### Capítulo 3: Trato diferenciado                                                              ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ####  
  #### Formas de concluir los procesos                                                                   ----
  ####
  #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.create(paste0(
    path2SP,
    "/National/Visualization",
    "/Output/Trato diferenciado/",
    savePath, "/")
  )
  
  descripcion           <- descripcion.fn()
  trato_diferenciado    <- trato_diferenciado.fn()

  
  print("TRATO DIFERENCIADO") 
  
  
}

