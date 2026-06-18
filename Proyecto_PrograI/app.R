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

# Carga de datos
dataset <- read_excel("Proyecto_PrograI/dataset.xlsx")

# 1. Interfaz de Usuario 
ui <- fluidPage(
  titlePanel("Music Explorer: Análisis interactivo de canciones y artistas"),
  
  tabsetPanel(
    tabPanel("Comparación entre Géneros",
             sidebarLayout(
               sidebarPanel(
                 selectInput("gen_filter", "Seleccionar Géneros a comparar:", 
                             choices = unique(dataset$Genre), multiple = TRUE),
                 
                 selectInput("var_comparar", "Métrica musical:",
                             choices = c("Danceability", "Energy", "Loudness", 
                                         "Speechiness", "Instrumentalness", "Tempo")),
                 
                 helpText("Nota: Selecciona varios géneros para ver su comparación estadística.")
               ),
               mainPanel(
                 plotOutput("genBoxplot"),
                 h4("Resumen Estadístico (Promedios)"),
                 tableOutput("tabla_resumen")
               )
             )
    )
  )
)

# 2. Lógica del Servidor 
server <- function(input, output, session) {
  
  output$genBoxplot <- renderPlot({
    df <- dataset
    if(!is.null(input$gen_filter) && length(input$gen_filter) > 0) {
      df <- df[df$Genre %in% input$gen_filter, ]
    }
    
    # boxplot + jitter
    ggplot(df, aes_string(x = "Genre", y = input$var_comparar, fill = "Genre")) +
      geom_jitter(width = 0.2, alpha = 0.3, color = "black") + 
      geom_boxplot(outlier.shape = NA, alpha = 0.7, size = 1) + 
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 15) +
      labs(title = paste("Análisis comparativo de:", input$var_comparar),
           subtitle = "Distribución de valores por género musical",
           x = "Género Musical", y = input$var_comparar) +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold", color = "#2c3e50"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$tabla_resumen <- renderTable({
    df <- dataset
    if(!is.null(input$gen_filter) && length(input$gen_filter) > 0) {
      df <- df[df$Genre %in% input$gen_filter, ]
    }
    aggregate(as.formula(paste(input$var_comparar, "~ Genre")), data = df, mean)
  })
}

# 3. Lanzar la aplicación
shinyApp(ui = ui, server = server)