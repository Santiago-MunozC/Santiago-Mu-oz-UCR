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
library(dplyr)
library(shinythemes)
options(scipen = 999) #quita la notación científica

#cargar dataset
dataset <- read_excel("dataset.xlsx")

#ui
ui <- navbarPage(
  title = "Music Explorer",
  
  header = tags$head(
    tags$style(HTML("
      body { background-color: #F9F9F9; color: #333; font-family: 'Arial', sans-serif; }
      .well { background-color: #FFFFFF; border: none; box-shadow: 0 4px 6px rgba(0,0,0,0.1); border-radius: 10px; }
      h3, h4 { color: #2C3E50; }
      table { background-color: white !important; border-radius: 10px; }
      thead { background-color: #2C3E50 !important; color: white !important; }
    "))
  ),
  
  #pestaña1
  tabPanel("Exploración Inicial",
    titlePanel("Music Explorer: Análisis interactivo de canciones y artistas"),
    sidebarLayout(
      sidebarPanel(
        selectInput("var_explorar", "Seleccione una variable para visualizar:",
                    choices = list(
                      "Cuantitativas" = c("Danceability" = "Danceability",
                                          "Energy" = "Energy",
                                          "Valence" = "Valence",
                                          "Instrumentalness" = "Instrumentalness",
                                          "Duration_min" = "Duration_min",
                                          "Liveness" = "Liveness",
                                          "Loudness" = "Loudness",
                                          "Speechiness" = "Speechiness",
                                          "Acousticness" = "Acousticness",
                                          "Tempo" = "Tempo"),
                      "Cualitativas" = c("Album_type" = "Album_type",
                                         "most_playedon" = "most_playedon",
                                         "Genre" = "Genre")
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
  ),
  
  #pestaña2
  tabPanel("Comparación por Género",
    titlePanel("¿Cómo varían las características musicales entre los géneros?"),
    sidebarLayout(
      sidebarPanel(
        h4("Filtros de Análisis"),
        selectizeInput("gen_filter", "1. Selecciona Género(s):", 
                       choices = sort(unique(dataset$Genre)), multiple = TRUE),
        
        selectInput("variable", "3. Característica musical:", 
                    choices = c("Danceability", "Energy", "Loudness", "Speechiness", 
                                "Acousticness", "Instrumentalness", "Liveness", "Valence", "Tempo"))
      ),
      mainPanel(
        fluidRow(
          column(6, plotOutput("box_genero", height = "400px")),
          column(6, plotOutput("barra_genero", height = "400px"))
        ),
        hr(),
        h3("Resumen estadístico"),
        tableOutput("tabla_genero")
      )
    )
  ),
  
  #pestaña3
  tabPanel("Energy vs Loudness",
    titlePanel("Relación entre Energy y Loudness"),
    sidebarLayout(
      sidebarPanel(
        selectInput("genero", #escoger entre categorías
                    "Seleccione un género:",
                    choices = unique(dataset$Genre))
      ),
      
      #gráfico de distribución
      mainPanel(
        plotOutput("grafico"),
        br(),
        textOutput("cantidad"),
        br(),
        textOutput("correlacion"),
        br(),
        textOutput("interpretacion")
      )
    )
  ),
  
  #pestaña4
  tabPanel("Bailabilidad y Polaridad emocional",
    titlePanel("Pestaña 4"),
    sidebarLayout(
      sidebarPanel(
        #Menú de géneros
        selectInput("filtro_genero", 
                    "Seleccione un Género Musical:", 
                    choices = unique(dataset$Genre))
      ),
      mainPanel(
        plotOutput("grafico_densidad"),
        
        hr(),
        
        h4("Interpretación del Patrón"),
        textOutput("texto_interpretacion"),
        
        hr(),
        
        h4("Top 3 Canciones con Mayor Bailabilidad y Polaridad emocional"),
        tableOutput("tabla_top_canciones")
      )
    )
  ),
  
  #pestaña5
  tabPanel("Recomendación Personalizada",
    titlePanel("Music Explorer: Análisis interactivo de canciones y artistas"),
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
        uiOutput("resultado_recomendacion")
      )
    )
  )
)

#servidor
server <- function(input, output, session){

  #pestaña1  
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
        todos   <- dataset$Instrumentalness * 100000
        media   <- round(mean(todos, na.rm = TRUE), 4)
        mediana <- round(median(todos, na.rm = TRUE), 4)
        desv    <- round(sd(todos, na.rm = TRUE), 4)
        
      } else {
        
        media   <- round(mean(columna, na.rm = TRUE), 2)
        mediana <- round(median(columna, na.rm = TRUE), 2)
        desv    <- round(sd(columna, na.rm = TRUE), 2)
      }
      
      tagList(
        h4("Métricas descriptivas"),
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
          x = NULL,
          y = "Cantidad de canciones"
        )
    }
  })
  
  #añadir la descripción de las variables
  output$descripcion_variable <- renderText({
    descripcion_variable[[input$var_explorar]]
  })
  
  #pestaña2
  tema_spotify <- theme(
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#E0E0E0"), 
    panel.grid.minor = element_blank(),
    text = element_text(color = "#333333"), 
    axis.text = element_text(color = "#333333"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(t = 15, b=12)),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(t = 15, b=12))
  )
  
  paleta <- c("darkgreen", "forestgreen", "seagreen", "mediumseagreen", "limegreen","yellowgreen", "palegreen", "springgreen4") 
  
  datos_filtrados <- reactive({
    if(is.null(input$gen_filter) || length(input$gen_filter) == 0) return(dataset)
    dataset %>% filter(Genre %in% input$gen_filter)
  })
  
  # boxplot
  output$box_genero <- renderPlot({
    req(nrow(datos_filtrados()) > 0)
    ggplot(datos_filtrados(), aes(x = Genre, y = .data[[input$variable]], fill = Genre)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = colorRampPalette(paleta)(length(unique(datos_filtrados()$Genre)))) +
      tema_spotify +
      coord_flip() + 
      theme(legend.position="none")+
      labs(title = paste("Distribución de", input$variable), x = "", y = "Valor", margin= margin(b=12))
  })
  
  # gráfico de barras
  output$barra_genero <- renderPlot({
    req(nrow(datos_filtrados()) > 0)
    resumen <- datos_filtrados() %>%
      group_by(Genre) %>%
      summarise(Promedio = mean(.data[[input$variable]], na.rm = TRUE))
    
    ggplot(resumen, aes(x = Genre, y = Promedio, fill = Genre)) +
      geom_col(alpha = 0.7) +
      scale_fill_manual(values = colorRampPalette(paleta)(nrow(resumen))) +
      geom_text(aes(label = round(Promedio, 2)), hjust = -0.2, size = 4, fontface = "bold" ) +
      coord_flip() + 
      scale_y_continuous(limits = c(0, max(resumen$Promedio) * 1.3)) + 
      tema_spotify +
      theme(legend.position = "none") +
      labs(title = paste("Promedio de", input$variable), x = "", y = "Valor Promedio")
  })
  
  # tabla resumen
  
  output$tabla_genero <- renderTable({
    datos_filtrados() %>%
      group_by(Genre) %>%
      summarise(Media = round( mean(.data[[input$variable]], na.rm = TRUE), 2),
                Desviacion = round(sd(.data[[input$variable]], na.rm = TRUE),2)
      )
  }, 
  
  striped = TRUE,    
  bordered = TRUE,   
  spacing = 'm',    
  align = 'c'        
  )
  
  #pestaña3
  datos_filtrados_relacion <- reactive({
    subset(dataset, Genre == input$genero)
  })
  
  output$grafico <- renderPlot({
    ggplot(
      datos_filtrados_relacion(),
      aes(x = Loudness, y = Energy)
    ) +
      
      geom_point(
        color = "limegreen",
        size = 3
      ) +
      
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "darkblue",
        linewidth = 1.2
      ) +     
      
      labs(
        title = paste(
          "Relación entre Energy y Loudness:",
          input$genero
        ),
        x = "Loudness",
        y = "Energy"
      ) +
      
      theme_minimal()
  })
  
  output$cantidad <- renderText({
    paste(
      "Cantidad de canciones analizadas:",
      nrow(datos_filtrados_relacion())
    )
    
  })
  
  output$correlacion <- renderText({
    r <- cor(
      datos_filtrados_relacion()$Loudness,
      datos_filtrados_relacion()$Energy
    )
    
    paste(
      "Coeficiente de correlación:",
      round(r,3)
    )
  })
  
  output$interpretacion <- renderText({
    r <- cor(
      datos_filtrados_relacion()$Loudness,
      datos_filtrados_relacion()$Energy
    )
    
    if(r >= 0.7){
      "Interpretación: Existe una relación fuerte entre Loudness y Energy."
      
    } else if(r >= 0.3){
      "Interpretación: Existe una relación moderada entre Loudness y Energy."
      
    } else {
      "Interpretación: La relación entre Loudness y Energy es débil."
      
    }
    
  })
  
  #pestaña4
  output$grafico_densidad <- renderPlot({
    
    #Filtro por el género elegido por el usuario
    datos_filtrados_pest4 <- dataset %>%
      filter(Genre == input$filtro_genero) 
    
    #Gráfico densidad
    ggplot(datos_filtrados_pest4, aes(x = Danceability, y = Valence)) +
      geom_density_2d_filled(alpha = 0.85) +
      geom_point(alpha = 0.3, size = 1.2, color = "black")+
      geom_density2d(color = "white", alpha = 0.4, linewidth = 0.3)+
      scale_x_continuous(limits = c(0,1), expand = c(0,0))+
      scale_y_continuous(limits = c(0,1), expand = c(0,0))+
      scale_fill_manual(
        values = colorRampPalette(c("#111111", "#052e16", "#15803d", "#22c55e", "#4ade80", "#22ff00"))(14)
      ) +
      labs(title = paste("Concentración de canciones para el género:", input$filtro_genero), 
           subtitle = "Las zonas encendidas muestran la mayor concentración de canciones",
           x = "Danceability",
           y = "Valence",
           fill = "Densidad") +
      theme_minimal() +
      theme(text = element_text(size = 14),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
            panel.grid.minor = element_blank(),
            legend.position = "right"
      )
  })
  
  # Texto de Interpretación
  
  output$texto_interpretacion <- renderText({
    genero <- input$filtro_genero
    
    if (genero == "Pop") {
      return("El Pop muestra una altísima concentración en la zona centro-derecha del gráfico. Esto indica que la gran mayoría de sus canciones mantienen niveles de bailabilidad elevados (entre 0.6 y 0.75) acompañados de una polaridad emocional intermedia-alta (Valence cercano a 0.5). En general, consolida el perfil de este género como música predominantemente rítmica y animada.")
    } else if (genero == "Rock") {
      return("El Rock presenta su núcleo de máxima concentración en la región central e inferior-izquierda del gráfico. Esto refleja que la mayoría de sus canciones poseen una bailabilidad moderada (entre 0.40 y 0.55) acompañada de una polaridad emocional intermedia-baja (Valence entre 0.30 y 0.55), consolidando un perfil musical con un ritmo menos marcado y una carga emocional más seria o melancólica.")
    } else if (genero == "Rap") {
      return("El Rap presenta una concentración muy marcada en la sección centro-derecha del gráfico. La zona de máxima densidad revela que la inmensa mayoría de las canciones poseen niveles de bailabilidad altos (entre 0.70 y 0.82), mientras que su polaridad emocional se mantiene en un rango intermedio o neutro (Valence entre 0.40 y 0.55). Esto consolida un perfil rítmicamente muy enérgico pero con una carga emocional balanceada.")
    } else if (genero == "Reggaetón") {
      return("El Reggaetón exhibe una de las concentraciones más compactas y desplazadas hacia la esquina superior derecha de todo el dataset. El núcleo de máxima densidad se ubica en niveles de bailabilidad sobresalientes (entre 0.78 y 0.85) con una polaridad emocional alta (Valence entre 0.60 y 0.70). Este comportamiento define la naturaleza del género: producciones diseñadas específicamente para el baile con vibras intensamente festivas y animadas.")
    } else if (genero == "K-pop") {
      return("El K-pop exhibe una concentración focalizada en el cuadrante superior derecho del gráfico. La zona de máxima densidad revela que las canciones se agrupan en niveles de bailabilidad elevados (entre 0.70 y 0.77) junto con una polaridad emocional notablemente alta (Valence entre 0.65 y 0.72). Estas características reflejan la naturaleza comercial del género: vibra optimista, brillante y altamente coreográfica.")
    } else if (genero == "Electrónica") {
      return("La Electrónica exhibe un comportamiento centrado en el gráfico. Su zona de máxima densidad revela que sus pistas se agrupa en niveles de bailabilidad moderados-altos (entre 0.58 y 0.65) junto con una polaridad emocional intermedia-neutra (Valence entre 0.42 y 0.50). El núcleo principal demuestra un equilibrio sonoro constante y menos polarizado emocionalmente.")
    } else if (genero == "R&B") {
      return("El R&B presenta una distribución dispersa y multimodal muy interesante en la zona centro-derecha del gráfico. Sus núcleos de máxima densidad revelan que el género mantiene una bailabilidad sólida y constante (entre 0.55 y 0.70), pero se divide en dos perfiles emocionales claros: un grupo de canciones con polaridad intermedia-baja (Valence en torno a 0.45) y otro de vibra más alegre y enérgica (Valence cercano a 0.65). Demuestra la versatilidad del género para alternar entre baladas íntimas y pistas rítmicas.")
    } else {
      return("El género Alternative muestra un núcleo central bien definido en la región media-baja del gráfico. Su punto de máxima densidad indica que la mayoría de los temas se sitúan en una bailabilidad moderada (entre 0.55 y 0.62) con una polaridad emocional intermedia o neutra (Valence entre 0.38 y 0.45). Sin embargo, tiene una amplia dispersión hacia el cuadrante inferior izquierdo, reflejando la presencia de subgrupos de canciones más acústicas, lentas y de carácter melancólico.")
    }
  })
  
  # Lógica del Servidor para filtrar y construir la Tabla del Top Canciones
  output$tabla_top_canciones <- renderTable({
    
    top_datos <- dataset %>%
      filter(Genre == input$filtro_genero) %>%
      # Esto prioriza las canciones que están más cerca del extremo superior derecho.
      mutate(Indice_Top = Danceability + Valence) %>%
      arrange(desc(Indice_Top)) %>%
      
      head(3) %>%
      
      # Seleccionamos y renombramos las columnas visibles para el usuario.
      select(
        Canción = Track, 
        Artista = Artist, 
        Bailabilidad = Danceability, 
        Polaridad Emocional = Valence
      )
    
    return(top_datos)
  }, digits = 2, align = 'c') # 2 decimales y centrado
  
  #pestaña5
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
      
      url_cancion <- if(!is.na(link_columna) && link_columna != "") link_columna else "#"
      
      #breve descripción de cada género
      descripcion_genero <- switch(genero_cancion,
                                   "Pop" = "Género musical moderno y comercial, caracterizado por sus melodías pegadizas y su gran popularidad entre un público amplio.",
                                   "Rock" = "Género musical energético y expresivo, caracterizado por el protagonismo de las guitarras, los ritmos intensos y su gran influencia en la cultura popular.",
                                   "Rap" = "Género musical urbano, moderno y expresivo, caracterizado por sus rimas, su ritmo marcado y la transmisión de mensajes y experiencias personales a través de sus letras.",
                                   "Reggaetón" = "Género musical urbano y moderno, caracterizado por sus ritmos contagiosos, su estilo bailable y la fusión de influencias latinas y del hip-hop.",
                                   "K-pop" = "Género musical dinámico y contemporáneo, reconocido por combinar distintos estilos, sus melodías pegadizas, ritmos contagiosos y coreografías llamativas.",
                                   "Electrónica" = "Género musical moderno y versátil, caracterizado por el uso de sonidos sintetizados, ritmos repetitivos y una gran variedad de estilos orientados al baile y la experimentación sonora.",
                                   "R&B" = "Género musical moderno y melódico, caracterizado por la mezcla de soul y pop, sus ritmos suaves y su combinación de ritmos relajados con voces expresivas y letras emocionales.",
                                   "Alternative" = "Género musical moderno y diverso, caracterizado por su sonido poco convencional, la mezcla de distintos estilos y su enfoque en la libertad creativa, la innovación y la expresión personal.")
      
      tagList(
        p(strong("Nombre (Track): "), track_nombre),
        p(strong("Artista (Artist): "), artista_nombre),
        p(strong("Tipo de lanzamiento (Album_type): "), tipo_album),
        p(strong("Género (Genre): "), genero_cancion),
        p(strong("Enlace: "), a("Escuchar canción en Spotify", href = url_cancion, target = "_blank")),
        br(),
        p(strong("Sobre el género: "), descripcion_genero)
      )
    }
  })
}

#ejecutar la aplicación
shinyApp(ui = ui, server = server)