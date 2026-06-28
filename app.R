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
                               "Cuantitativas" = c("Bailabilidad" = "Danceability",
                                                   "Energía" = "Energy",
                                                   "Cáracter emocional" = "Valence",
                                                   "Probabilidad de ser instrumental" = "Instrumentalness",
                                                   "Duración en minutos" = "Duration_min",
                                                   "Probabilidad de ser una grabación en vivo" = "Liveness",
                                                   "Volumen promedio (dB)" = "Loudness",
                                                   "Contenido hablado" = "Speechiness",
                                                   "Probabilidad de ser acústica" = "Acousticness",
                                                   "Ritmo (BPM)" = "Tempo"),
                               "Cualitativas" = c("Tipo de Álbum" = "Album_type",
                                                  "Plataforma" = "most_playedon",
                                                  "Género musical" = "Genre")
                             )
                 )
               ),
               
               mainPanel(
                 h3("Estructura General"),
                 textOutput("estructura_general"), #tamaño del dataset
                 br(),
                 h4("Vista previa de los datos (Primeras filas)"),
                 div(style = "overflow-x: scroll;", tableOutput("vista_tabla")),
                 br(),
                 uiOutput("nota_instrumentalness"),
                 plotOutput("hist_exploracion"),
                 br(),
                 uiOutput("seccion_frecuencias"),
                 uiOutput("seccion_estadisticas"),
                 br(),
                 h4("Descripción de la variable"),
                 textOutput("descripcion_variable"),
                 br(),
                 h4("Interpretación"),
                 uiOutput("interpretacion_variable")
               )
             )
    )
  )
)

#servidor
server <- function(input, output, session){
  
  #breve descripción de las variables
  descripcion_variable <- list(
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
    Genre = "Corresponde al género musical principal asociado a cada canción."
  )
  
  #interpretación de los gráficos obtenidos
  interpretacion_variable <- list(
    Danceability = "Se concluye que los artistas más populares tienden a producir canciones con alta bailabilidad (factor asociado al éxito de streaming).",
    Energy = "Se concluye que las canciones populares tienen tendencia hacia los altos niveles de energía (asociado a géneros dominantes en streaming como el pop y el reggaetón).",
    Valence = "Se concluye que las canciones populares presentan una distribución equilibrada del carácter emocional, sin una tendencia hacia la positividad o negatividad.",
    Instrumentalness = "Se concluye que la gran mayoría de canciones populares tienen un nivel de instrumentalidad cercano o igual a cero (el éxito en streaming está asociado a canciones con voz y letra).",
    Duration_min = "Se concluye que las canciones populares siguen un formato de duración estándar entre los 2.5 y 4.5 minutos (canciones más cortas para una mayor cantidad de reproducciones).",
    Liveness = "Se concluye que la mayoría de los artistas más populares publican grabaciones de estudio con una alta producción técnica.",
    Loudness = "Se concluye que las canciones populares presentan niveles de volumen consistentemente altos (loudness war), donde los productores maximizan el volumen percibido para destacar en plataformas de streaming.",
    Speechiness = "Se concluye que las canciones populares tienen un contenido hablado muy bajo, por lo que predominan las canciones que tienen una estructura musical tradicional (la melodía sigue siendo el elemento dominante por sobre la palabra).",
    Acousticness = "Se concluye que las canciones populares tienen una baja probabilidad de ser acústicas, por lo que la producción electrónica y el uso de instrumentos procesados digitalmente dominan el mercado.",
    Tempo = "Se concluye que las canciones populares presentan una distribución de tempo con dos zonas de concentración predominantes, lo que refleja la coexistencia de dos grandes corrientes en el mercado del streaming (canciones de ritmo moderado como el pop y canciones de ritmo más animado como el reggaetón).",
    Album_type = "Se concluye que los artistas más populares distribuyen su música a través de álbumes de estudio principalmente (singles representan una estrategia complementaria).",
    most_playedon = "Se concluye que Spotify es la plataforma predominante donde las canciones de los artistas más populares acumulan más reproducciones (plataforma de música líder a nivel global).",
    Genre = "Se concluye que el Pop es el género dominante entre los artistas más populares, seguido de cerca por Rock, Reggaetón y Rap como géneros igualmente relevantes en el streaming. Los géneros minoritarios como K-pop, Electrónica, R&B y Alternative."
  )
  
  #interpretación
  output$interpretacion_variable <- renderUI({
    texto <- interpretacion_variable[[input$var_explorar]]
    HTML(paste("<p style='text-align: justify;'>", texto, "</p>"))
  })
  
  #breve descripción del dataset
  output$estructura_general <- renderText({
    paste("El conjunto de datos a analizar contiene un total de", nrow(dataset), 
          "canciones correspondientes a los artistas más populares (10 canciones por artista).")
  })
  
  #tabla con las primeras 5 filas del dataset
  output$vista_tabla <- renderTable({
    head(dataset, n = 5)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  #nota cuando se selecciona Instrumentalness
  output$nota_instrumentalness <- renderUI({
    if (input$var_explorar == "Instrumentalness") {
      wellPanel(
        style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 10px;",
        HTML("<b> Nota sobre la visualización:</b> Dado que la variable <i>Instrumentalness</i> presenta una fuerte concentración de valores cercanos a cero con algunos valores extremos aislados, el gráfico excluye observaciones superiores a 0.000025 para mejorar la legibilidad. El eje horizontal se expresa en índice por cada 100 mil unidades.")
      )
      
    } else {
      
      NULL
    }
  })
  
  #tabla de frecuencias y porcentajes para variables cualitativas
  output$seccion_frecuencias <- renderUI({
    columna <- dataset[[input$var_explorar]]
    
    if (!is.numeric(columna)) {
      freq_tabla <- as.data.frame(table(columna))
      colnames(freq_tabla) <- c("Categoría", "Frecuencia")
      freq_tabla$Porcentaje <- paste0(round(freq_tabla$Frecuencia / sum(freq_tabla$Frecuencia) * 100, 1), "%")
      freq_tabla <- freq_tabla[order(-freq_tabla$Frecuencia), ]
      
      tagList(
        h4("Distribución de frecuencias"),
        renderTable(freq_tabla, striped = TRUE, hover = TRUE, bordered = TRUE)
      )
      
    } else {
      
      NULL
    }
  })
  
  #estadísticas descriptivas de las variables numéricas
  output$seccion_estadisticas <- renderUI({
    columna <- dataset[[input$var_explorar]]
    
    if (is.numeric(columna)) {
      
      #Instrumentalness
      if (input$var_explorar == "Instrumentalness") {
        sub_datos <- subset(dataset, Instrumentalness <= 0.000025)$Instrumentalness
        media   <- round(mean(sub_datos, na.rm = TRUE), 6)
        mediana <- round(median(sub_datos, na.rm = TRUE), 6)
        desv    <- round(sd(sub_datos, na.rm = TRUE), 6)
        
      } else {
        
        media   <- round(mean(columna, na.rm = TRUE), 2)
        mediana <- round(median(columna, na.rm = TRUE), 2)
        desv    <- round(sd(columna, na.rm = TRUE), 2)
      }
      
      tagList(
        h4("Métricas Descriptivas"),
        HTML(paste0("<ul>",
                    "<li><b>Promedio (Media):</b> ", media, "</li>",
                    "<li><b>Mediana:</b> ", mediana, "</li>",
                    "<li><b>Desviación Estándar:</b> ", desv, "</li>",
                    "</ul>"))
      )
      
    } else {
      
      NULL #variables cualitativas
    }
  })  
  
  #gráfico
  output$hist_exploracion <- renderPlot({
    columna <- dataset[[input$var_explorar]]
    
    if (is.numeric(columna)) {
      
      #filtro para Instrumentalness
      if (input$var_explorar == "Instrumentalness") {
        datos_grafico <- subset(dataset, Instrumentalness <= 0.000025)
        
        #estadísticas descriptivas para Instrumentalness
        media_val <- mean(datos_grafico$Instrumentalness * 100000, na.rm = TRUE)
        mediana_val <- median(datos_grafico$Instrumentalness * 100000, na.rm = TRUE)
        
        #gráfico para Instrumentalness
        p <- ggplot(datos_grafico, aes(x = Instrumentalness * 100000)) +
          geom_histogram(
            bins = 20,
            fill = "limegreen",
            color = "white",
            alpha = 0.85
          ) +
          geom_vline(aes(xintercept = media_val, color = "Media"), linewidth = 1.2, linetype = "dashed") +
          geom_vline(aes(xintercept = mediana_val, color = "Mediana"), linewidth = 1.2, linetype = "dotdash") +
          scale_color_manual(name = "Métricas descriptivas", values = c("Media" = "darkred", "Mediana" = "darkblue")) +
          theme_minimal(base_size = 14) +
          theme(
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            axis.title = element_text(face = "bold"),
            legend.position = "top"
          ) +
          labs(
            title = "Distribución de Instrumentalness",
            x = "Instrumentalness (índice por cada 100 mil)",
            y = "Cantidad de canciones"
          )
        
      } else {
        #estadísticas descriptivas para el resto de variables cuantitativas
        media_val <- mean(columna, na.rm = TRUE)
        mediana_val <- median(columna, na.rm = TRUE)
        
        #histograma para variables numéricas
        p <- ggplot(dataset, aes(x = .data[[input$var_explorar]])) +
          geom_histogram(
            bins = 20,
            fill = "limegreen",
            color = "white",
            alpha = 0.85
          ) +
          geom_vline(aes(xintercept = media_val, color = "Media"), linewidth = 1.2, linetype = "dashed") +
          geom_vline(aes(xintercept = mediana_val, color = "Mediana"), linewidth = 1.2, linetype = "dotdash") +
          scale_color_manual(name = "Métricas Descriptivas", values = c("Media" = "darkred", "Mediana" = "darkblue")) +
          theme_minimal(base_size = 14) +
          theme(
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            axis.title = element_text(face = "bold"),
            legend.position = "top"
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
          x = NULL,
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