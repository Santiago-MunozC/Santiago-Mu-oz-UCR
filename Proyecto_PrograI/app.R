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

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Music Explorer: Análisis interactivo de canciones y artistas"),
  

  tabsetPanel(
    
    # a. Descripción y exploración inicial
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
                               
                               "Cualitativas" = c("Género Musical" = "Genre",
                                                  "Tipo de Álbum" = "Album_type",
                                                  "Plataforma" = "most_playedon",
                                                  "Artista" = "Artist") 
                             ) 
                 ) 
               ), 
               
               mainPanel(
                 plotOutput("hist_exploracion"),
                 p("Esta pestaña muestra la distribución general de las características musicales.")
               )
               
             ) 
    ) 
  ) 
) 


# 3. Lógica del Servidor
server <- function(input, output){
  
  output$hist_exploracion <- renderPlot({
    columna_seleccionada <- dataset[[input$var_explorar]]
    if(is.numeric(columna_seleccionada)) {
      
      # Si es numérica, hacemos el HISTOGRAMA
      ggplot(dataset, aes_string(x = input$var_explorar)) +
        geom_histogram(fill = "steelblue", color = "white", bins = 20) +
        theme_minimal() +
        labs(title = paste("Distribución de", input$var_explorar), 
             y = "Frecuencia", 
             x = input$var_explorar)
      
    } else {
      
      # Si es categórica/texto, hacemos el GRÁFICO DE BARRAS HORIZONTALES
      ggplot(dataset, aes_string(x = input$var_explorar)) +
        geom_bar(fill = "coral", color = "black") +
        coord_flip() + 
        theme_minimal() +
        labs(title = paste("Frecuencia de", input$var_explorar), 
             y = "Cantidad de Canciones", 
             x = input$var_explorar)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
