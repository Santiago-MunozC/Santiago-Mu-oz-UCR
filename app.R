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
options(scipen = 999) #quita la notación científica en toda la aplicación

#cargar dataset
dataset <- read_excel("dataset.xlsx")

#ui
ui <- fluidPage(
  
  titlePanel("Music Explorer: Análisis interactivo de canciones y artistas"),
  
  tabsetPanel( 
    tabPanel("Exploración Inicial",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_explorar", "Seleccione una variable para visualizar:",
                             choices = list(
                               "Cuantitativas" = c("Danceability" = "Danceability",
                                                   "Energy" = "Energy",
                                                   "Valence" = "Valence",
                                                   "Instrumentalness" = "Instrumentalness",
                                                   "Duration_min" = "Duration_min",
                                                   "Liveness" = "Liveness"),
                               "Cualitativas" = c("Tipo de Álbum" = "Album_type",
                                                  "Plataforma" = "most_playedon")
                             )
                 )
               ),
               
               mainPanel(
                 h3("Estructura General"),
                 textOutput("estructura_general"), #tamaño del dataset
                 br(),
                 plotOutput("hist_exploracion"),
                 br(),
                 h4("Descripción de la variable"),
                 textOutput("descripcion_variable")
               )
             )
    )
  )
)

#servidor
#descripción de cada variable
server <- function(input, output, session){
  descripcion_variable <- list(
    Artist = "Corresponde al nombre del artista o grupo musical que interpreta la canción.",
    Track = "Corresponde al nombre de la canción.",
    Album = "Corresponde al nombre del álbum al que pertenece la canción.",
    Album_type = "Corresponde al tipo de lanzamiento al que pertenece la canción, ya sea álbum, sencillo o una recopilación.",
    Danceability = "Es la medida que indica qué tan adecuada es una canción para bailar.",
    Energy = "Indica la intensidad y actividad percibida en una canción.",
    Loudness = "Corresponde al nivel promedio del volumen de la canción medido en decibeles.",
    Speechiness = "Corresponde a la proporción de contenido hablado presente en la canción.",
    Acousticness = "Corresponde a la probabilidad de que la canción sea acústica.",
    Instrumentalness = "Corresponde a la probabilidad de que la canción sea principalmente instrumental.",
    Liveness = "Corresponde a la estimación de interpretación en vivo.",
    Valence = "Corresponde al carácter emocional positivo de la canción.",
    Tempo = "Corresponde al ritmo de la canción en pulsaciones por minuto.",
    Duration_min = "Corresponde a la duración de la canción en minutos.",
    most_playedon = "Corresponde a la plataforma donde la canción tiene más reproducciones.",
    Genre = "Corresponde al género musical principal."
  )

  #breve descripción del dataset
  output$estructura_general <- renderText({
    paste("El conjunto de datos a analizar contiene un total de", nrow(dataset), 
          "canciones correspondientes a los artistas más populares (10 canciones por artista).")
  })
  
  #gráfico
  output$hist_exploracion <- renderPlot({
    columna <- dataset[[input$var_explorar]]
    
    if (is.numeric(columna)) {
      
      #filtro para Instrumentalness
      if (input$var_explorar == "Instrumentalness") {
        datos_grafico <- subset(dataset, Instrumentalness <= 0.000025)
        
        #gráfico para Instrumentalness
        p <- ggplot(datos_grafico, aes(x = Instrumentalness * 100000)) +
          geom_histogram(
            bins = 20,
            fill = "limegreen",
            color = "white",
            alpha = 0.85
          ) +
          theme_minimal(base_size = 14) +
          theme(
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(face = "italic", hjust = 0.5),
            axis.title = element_text(face = "bold")
          ) +
          labs(
            title = "Distribución de Instrumentalness",
            subtitle = "Nota: Se omiten valores extremos superiores a 0.000025 para mejorar la visualización",
            x = "Instrumentalness (índice por cada 100 mil)",
            y = "Cantidad de canciones"
          )
        
      } else {
        
        #histograma para variables numéricas
        p <- ggplot(dataset, aes(x = .data[[input$var_explorar]])) +
          geom_histogram(
            bins = 20,
            fill = "limegreen",
            color = "white",
            alpha = 0.85
          ) +
          theme_minimal(base_size = 14) +
          theme(
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            axis.title = element_text(face = "bold")
          ) +
          labs(
            title = paste("Distribución de", input$var_explorar),
            x = input$var_explorar,
            y = "Cantidad de canciones"
          )
      }
      p
      
    } else {
      
      #gráfico para variables cualitativas
      ggplot(dataset, aes(x = .data[[input$var_explorar]])) +
        geom_bar(
          fill = "limegreen",
          color = "white",
          alpha = 0.85,
          na.rm = TRUE
        ) +
        coord_flip() +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid.major.y = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title = element_text(face = "bold")
        ) +
        labs(
          title = paste("Frecuencia de", input$var_explorar),
          x = input$var_explorar,
          y = "Cantidad de canciones"
        )
    }
  })
  
  #añadir la descripción de las variables
  output$descripcion_variable <- renderText({
    descripcion_variable[[input$var_explorar]]
  })
}

#ejecutar la aplicación
shinyApp(ui = ui, server = server)