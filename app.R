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

# Carga de datos
dataset <- read_excel("dataset.xlsx")

# 1. Interfaz de Usuario 
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #F9F9F9; color: #333; font-family: 'Arial', sans-serif; }
      .well { background-color: #FFFFFF; border: none; box-shadow: 0 4px 6px rgba(0,0,0,0.1); border-radius: 10px; }
      h3, h4 { color: #2C3E50; }
      table { background-color: white !important; border-radius: 10px; }
      thead { background-color: #2C3E50 !important; color: white !important; }
    "))
  ),
  

  titlePanel("¿Cómo varían las características musicales entre los géneros?"),

  tabsetPanel(
    tabPanel("Comparación por Género",
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
    )
  )
)


# 2. Lógica del Servidor 
server <- function(input, output, session) {
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
}

# 3. Lanzar la aplicación
shinyApp(ui = ui, server = server)