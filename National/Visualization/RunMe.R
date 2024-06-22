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
source("Code/proceso_justo.R")
source("Code/proceso_justo_percepcion.R")
#source("Code/uso_excesivo_fuerza.R")

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
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ### Capítulo 1                                                                                        ----
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
  
  proceso_justo_lista <- list('Guardar silencio en el tiempo' = guardar_silencio, 
                              'Información de la detencion'   = informacion_detencion,
                              'Claridad de actores'           = claridad_actores,
                              'Defensa oportuna'              = defensa_oportuna,
                              'Tipo de defensa'               = tipo_defensa,
                              'Tribunal transparente'         = tribunal_transparente,
                              'Tribunal imparcial'            = tribunal_imparcial,
                              'Tribunal presente'             = tribunal_presente
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
  
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ### Capítulo 2                                                                                      ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  
}

