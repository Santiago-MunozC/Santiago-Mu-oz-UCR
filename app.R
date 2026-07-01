#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readxl)
library(ggplot2)

datos <- read_excel("dataset.xlsx")


ui <- fluidPage(
  
  titlePanel("Relación entre Energy y Loudness"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("genero",                             #escoger entre categorías
                  "Seleccione un género:",
                  choices = unique(datos$Genre))
    ),
    
    # Show a plot of the generated distribution
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
)

# Define server logic required
server <- function(input, output) {
  
  datos_filtrados <- reactive({
    subset(datos, Genre == input$genero)
  })
  
  output$grafico <- renderPlot({
    ggplot(
      datos_filtrados(),
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
      nrow(datos_filtrados())
    )
    
  })
  
  output$correlacion <- renderText({
    r <- cor(
      datos_filtrados()$Loudness,
      datos_filtrados()$Energy
    )
    
    paste(
      "Coeficiente de correlación:",
      round(r,3)
    )
  })
  
  output$interpretacion <- renderText({
    r <- cor(
      datos_filtrados()$Loudness,
      datos_filtrados()$Energy
    )
    
    if(r >= 0.7){
      "Interpretación: Existe una relación fuerte entre Loudness y Energy."
      
    } else if(r >= 0.3){
      "Interpretación: Existe una relación moderada entre Loudness y Energy."
      
    } else {
      "Interpretación: La relación entre Loudness y Energy es débil."
      
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


