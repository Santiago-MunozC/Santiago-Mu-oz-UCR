#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(readxl)

#cargar dataset
dataset <- read_excel("dataset.xlsx")

#ui
ui <- fluidPage(
  
  titlePanel("Music Explorer: Análisis interactivo de canciones y artistas"),
  
  tabsetPanel( 
    tabPanel("Recomendación Personalizada",
             sidebarLayout(
               sidebarPanel(
                 selectInput("rec_genero", "Seleccione un género musical:",
                             choices = NULL),
                 selectInput("rec_artista", "Seleccione un artista (Opcional):",
                             choices = NULL),
                 sliderInput("rec_danceability", "Bailabilidad (Danceability):",
                             min = 0, max = 1, value = c(0, 1), step = 0.05),
                 sliderInput("rec_energy", "Energía (Energy):",
                             min = 0, max = 1, value = c(0, 1), step = 0.05),
                 sliderInput("rec_valence", "Polaridad emocional (Valence):",
                             min = 0, max = 1, value = c(0, 1), step = 0.05),
                 actionButton("btn_recomendar", "Obtener recomendación")
               ),
               mainPanel(
                 h4("Canción recomendada"),
                 uiOutput("resultado_recomendacion"),
                 br(),
                 p("Esta pestaña filtra las canciones según tus preferencias y selecciona una al azar.")
               )
             )
    )
  )
)

#servidor
server <- function(input, output, session){
  
  #cargar los géneros
  observe({
    generos_disponibles <- unique(dataset$Genre)
    generos_disponibles <- generos_disponibles[!is.na(generos_disponibles)]
    
    updateSelectInput(session, "rec_genero",
                      choices = c("Todos", sort(generos_disponibles)))
  })
  
  #filtrar los artistas dependiendo del género seleccionado
  observe({
    req(input$rec_genero)
    
    if (input$rec_genero == "Todos") {
      artistas_disponibles <- unique(dataset$Artist)
    } else {
      artistas_disponibles <- dataset$Artist[dataset$Genre == input$rec_genero]
    }
    artistas_disponibles <- artistas_disponibles[!is.na(artistas_disponibles)]
    
    updateSelectInput(session, "rec_artista",
                      choices = c("Todos", sort(unique(artistas_disponibles))),
                      selected = "Todos") 
  })
  
  #para filtrar y seleccionar una canción aleatoria
  cancion_aleatoria <- eventReactive(input$btn_recomendar, {
    req(dataset)
    datos <- dataset
    
    #para filtrar por género
    if (input$rec_genero != "Todos") {
      datos <- subset(datos, Genre == input$rec_genero)
    }
    
    #para filtrar por artista (solo si se seleccionó uno)
    if (input$rec_artista != "Todos") {
      datos <- subset(datos, Artist == input$rec_artista)
    }
    
    #para filtrat por danceability, energy y valence
    datos <- subset(datos,
                    Danceability >= input$rec_danceability[1] & Danceability <= input$rec_danceability[2] &
                    Energy >= input$rec_energy[1] & Energy <= input$rec_energy[2] &
                    Valence >= input$rec_valence[1] & Valence <= input$rec_valence[2])
    if (nrow(datos) == 0) {
      return(NULL)
    }
    
    #para la selección aleatoria
    fila_aleatoria <- sample(1:nrow(datos), 1)
    return(datos[fila_aleatoria, , drop = FALSE])
  })
  
  #para mostrar la información de la canción recomendada
  output$resultado_recomendacion <- renderUI({
    cancion <- cancion_aleatoria()
    
    if (is.null(cancion)) {
      HTML("<p style='color:red;'>No se encontraron canciones que cumplan con los criterios seleccionados.</p>")
    } else {
      track_nombre   <- cancion$Track[[1]]
      artista_nombre <- cancion$Artist[[1]]
      tipo_album     <- cancion$Album_type[[1]]
      genero_cancion <- cancion$Genre[[1]]
      link_columna   <- cancion$Link[[1]]
      
      url_cancion <- if(!is.na(cancion$Link)) cancion$Link else "#"
      
      tagList(
        p(strong("Nombre (Track): "), track_nombre),
        p(strong("Artista (Artist): "), artista_nombre),
        p(strong("Tipo de lanzamiento (Album_type): "), tipo_album),
        p(strong("Género (Genre): "), genero_cancion),
        p(strong("Enlace: "), a("Escuchar canción en Spotify", href = url_cancion, target = "_blank"))
      )
    }
  })
}

#ejecutar la aplicación
shinyApp(ui = ui, server = server)
