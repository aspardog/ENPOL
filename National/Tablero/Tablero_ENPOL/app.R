#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)


source("National/Visualization/Code/settings.R")
source("National/Visualization/Code/Parte A/proceso_justo.R")

# Cargando funciones de graficación desde GitHub
source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/loading.R")
loadVIZ(set = "ENPOL")

# Cargando datos
load(paste0(path2SP, "/National/Data_cleaning/Output/Main_database.RData"))

master_data.df <- Main_database %>% 
  filter(Anio_arresto >= as.numeric(2015)) %>% 
  filter(NSJP == 1)

mapa <- st_read(paste0(path2SP, "/National/Visualization/Input/shp/México_Estados.shp")) %>%
  mutate(
    ESTADO = case_when(
      ESTADO == "México" ~ "Estado de México",
      ESTADO == "Distrito Federal" ~ "Ciudad de México",
      TRUE ~ ESTADO
    )
  )

# Preparando la lista de estados y ordenando alfabéticamente
Estados <- Main_database %>%
  select(Estado_arresto) %>%
  distinct() %>%
  drop_na() %>%
  pull() %>%
  as.character() %>%
  sort() # Ordena alfabéticamente

percepcion_procedimiento.fn <- function(
    
  data.df = master_data.df  
  
) {
  
  data_subset.df <- data.df %>%
    mutate(
      proceso_justo = 
        case_when(
          as.numeric(P5_26A) == 1 ~ 1,
          as.numeric(P5_26A) == 0 ~ 0,
          T ~ NA_real_
        ),
      procedimiento =
        case_when(
          as.numeric(P5_6) == 1 ~ "Juicio",
          as.numeric(P5_6) == 2 ~ "Procedimiento abreviado",
          T ~ NA_character_
        ),
      counter = 1
    ) %>%
    group_by(procedimiento) %>%
    summarise(
      value2plot = mean(proceso_justo, na.rm = T),
      n_obs = sum(counter, na.rm = T)
    ) %>%
    drop_na()
  
  data2plot <- data_subset.df %>%
    mutate(
      value2plot = value2plot*100,
      labels = procedimiento,
      figure = paste0(round(value2plot,0), "%"),
      order_var = case_when(
        labels == "Juicio" ~ 2,
        labels =="Procedimiento abreviado" ~ 1,
        T ~ NA_real_)
    )
  
  colors4plot <-  c("#009AA9","#009AA9")
  
  plot <- barsChart.fn(data.df                    = data2plot,
                       groupVar                   = F,   
                       categories_grouping_var    = labels,
                       colors4plot                = colors4plot, 
                       order                      = T,
                       orientation                = "horizontal")    
  return(plot)
  
}


# Define la UI
ui <- fluidPage(
  # Incluye la fuente Lato desde Google Fonts
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Lato&display=swap")
  ),
  
  # Título centrado
  tags$h1("ENPOL: Análisis estatal", style = "font-family: 'Lato', sans-serif; color: #2a2a9A; text-align: center;"),
  
  # Layout con barra lateral
  sidebarLayout(
    # Panel de búsqueda a la izquierda
    sidebarPanel(
      titlePanel("Buscador"),
      selectInput("estado", "Seleccione un Estado:", choices = Estados),
      selectInput("seccion", "Seleccione una Sección:", choices = c("Sección 1", "Sección 2", "Sección 3")),
      style = "background-color: #f0f0f0;" # Color de fondo gris claro
    ),
    
    # Panel principal donde se mostrará el gráfico
    mainPanel(
      plotOutput("grafico")
    )
  )
)

# Define la lógica del servidor
server <- function(input, output) {
  # Gráfico reactivo
  output$grafico <- renderPlot({
    # Filtrar los datos según el estado seleccionado
    datos_filtrados <- master_data.df %>%
      filter(Estado_arresto == input$estado)
    
    # Llamar a la función `guardar_silencio.fn` con los datos filtrados
    percepcion_procedimiento.fn(datos_filtrados)
  })
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
