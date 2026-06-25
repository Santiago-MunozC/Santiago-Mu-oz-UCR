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
                 ),
                 
                 selectInput("genero", "Seleccione un género musical:",
                             choices = NULL)
               ),
               
               mainPanel(
                 plotOutput("hist_exploracion"),
                 br(),
                 h4("Descripción de la variable"),
                 verbatimTextOutput("descripcion_variable"),
                 br(),
                 h4("Descripción del género"),
                 verbatimTextOutput("descripcion_genero"),
                 br(),
                 p("Esta pestaña muestra la distribución general de las características musicales.")
               )
             )
    )
  )
)

#servidor
server <- function(input, output, session){
  
  observe({
    generos_disponibles <- unique(dataset$Genre)
    generos_disponibles <- generos_disponibles[!is.na(generos_disponibles)]
    
    updateSelectInput(session, "genero",
                      choices = sort(generos_disponibles))
  })
  
  #descripción de las variables
  descripcion_variable <- list(
    Artist = "Corresponde al nombre del artista o grupo musical que interpreta la canción.", 
    Track = "Corresponde al nombre de la canción.",
    Album = "Corresponde al nombre del álbum al que pertenece la canción.",
    Album_type = "Corresponde al tipo de lanzamiento al que pertenece la canción, ya sea álbum, sencillo o una recopilación.",
    Danceability = "Es la medida que indica qué tan adecuada es una canción para bailar según factores como el ritmo, la estabilidad del tempo y la fuerza del compás. Entre más cerca esté el valor de 1 más bailable es la canción.",
    Energy = "Indica la intensidad y actividad percibida en una canción, donde los valores altos se relacionan con canciones rápidas y ruidosas.",
    Loudness = "Corresponde al nivel promedio del volumen de la canción medido en decibeles.",
    Speechiness = "Corresponde a la proporción de contenido hablado que está presente en la canción. Entre más alto el valor, mayor la presencia de voz.",
    Acousticness = "Corresponde a la medida de la probabilidad de la canción sea acústica. Toma valores entre 0 y 1 donde entre más cerca estén los valores de 1 más predominan los sonidos acústicos y los valores más cercanos a 0 se relacionan con mayor presencia de instrumentos o sonidos electrónicos.",
    Instrumentalness = "Corresponde a la probabilidad de que en la canción predomine el instrumental y contenga pocas voces (o ninguna).",
    Liveness = "Corresponde a la estimación de la interpretación en vivo dentro de una grabación. Entre más alto el valor, mayor la probabilidad de que la canción haya sido grabada en directo.",
    Valence = "Corresponde a la medida del carácter emocional positivo de la canción. Los valores altos se asocian con canciones alegres, optimistas o energéticas.",
    Tempo = "Corresponde al ritmo de la canción expresado en pulsaciones por minuto.",
    Duration_min = "Corresponde a la duración de la canción en minutos.",
    most_playedon = "Corresponde a la Plataforma en la que la canción registra la mayor cantidad de reproducciones.",
    Genre = "Corresponde al género musical principal asociado a la canción."
  )
  
  #descripción de géneros
  descripcion_genero <- list(
    "Pop" = "Género musical caracterizado por melodías accesibles, estructuras sencillas y una amplia aceptación comercial.",
    "Rock" = "Género que se distingue por el uso predominante de guitarras, baterías y una fuerte expresión instrumental y vocal.",
    "Rap" = "Estilo musical basado en la recitación rítmica de letras, generalmente acompañado por bases instrumentales o electrónicas.",
    "Reggaetón" = "Género urbano de origen latino que combina ritmos caribeños con influencias del hip hop y la música electrónica.",
    "K-pop" = "Género de música popular originario de Corea del Sur, reconocido por su producción moderna y sus elaboradas presentaciones artísticas.",
    "Electrónica" = "Género que utiliza principalmente instrumentos y tecnologías digitales para la creación de sonidos y composiciones musicales.",
    "R&B" = "Género musical que fusiona elementos de soul, jazz y pop, destacándose por su énfasis en la interpretación vocal y la emotividad.",
    "Alternative" = "Género que engloba propuestas musicales que se diferencian de las corrientes comerciales predominantes y exploran sonidos innovadores."
  )
  
  #filtrar por el género seleccionado
  datos_filtrados <- reactive({
    req(input$genero)
    subset(dataset, Genre == input$genero)
  })
  
  #gráfico
  output$hist_exploracion <- renderPlot({
    datos <- datos_filtrados()
    columna <- datos[[input$var_explorar]]
    genero_actual <- input$genero
    
    if (is.numeric(columna)) {
      #gráfico de violín con puntos para variables numéricas
      ggplot(datos, aes(x = "", y = .data[[input$var_explorar]])) +
        geom_violin(
          fill = "limegreen",             
          color = NA, 
          alpha = 0.4
        ) +
        geom_jitter(
          color = "black",          
          width = 0.15,               
          alpha = 0.5,                  
          size = 2                
        ) +
        coord_flip() +            
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
        ) +
        labs(
          title = paste("Distribución de", input$var_explorar, "en", genero_actual),
          y = paste(input$var_explorar, "(Cada punto es una canción)")
        )
      
    } else {
      #gráfico de barras para variables cualitativas
      ggplot(datos, aes(x = .data[[input$var_explorar]])) +
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
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          axis.title = element_text(face = "bold")
        ) +
        labs(
          title = paste("Frecuencia de", input$var_explorar, "en", genero_actual),
          x = input$var_explorar,
          y = "Cantidad de canciones"
        )
    }
  })
  
  #añadir la descripción de las variables
  output$descripcion_variable <- renderText({
    descripcion_variable[[input$var_explorar]]
  })
  
  #añadir la descripción del género (Simplificado sin el "Todos")
  output$descripcion_genero <- renderText({
    req(input$genero)
    if (input$genero %in% names(descripcion_genero)){
      descripcion_genero[[input$genero]]
    } else {
      paste(input$genero, "es uno de los géneros musicales presentes en el conjunto de datos.")
    }
  })
}

#ejecutar la aplicación
shinyApp(ui = ui, server = server)