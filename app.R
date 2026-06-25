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
library(dplyr)
library(readxl)

dataset <- read_excel("dataset.xlsx")

# Interfaz de usuario
ui <- fluidPage(

    titlePanel("Pestaña 4"),
    
    tabsetPanel(
      tabPanel("Bailabilidad y Positividad Emocional",
               sidebarLayout(
                 sidebarPanel(
                   #Menú de géneros
                   selectInput("filtro_genero", 
                               "Seleccione un Género Musical:", 
                               choices = unique(dataset$Genre))
                 ),
                 mainPanel(
                   plotOutput("grafico_densidad") 
                 )
               )
      )
    )
)

# Servidor
server <- function(input, output) {
  
  output$grafico_densidad <- renderPlot({
    
    #Filtro por el género elegido por el usuario
    datos_filtrados <- dataset %>%
      filter(Genre == input$filtro_genero) 
    
    #Gráfico
    ggplot(datos_filtrados, aes(x = Danceability, y = Valence)) +
      geom_density_2d_filled(alpha = 0.85) +
      geom_point(alpha = 0.3, size = 1.2, color = "black")+
      geom_density2d(color = "white", alpha = 0.4, linewidth = 0.3)+
      scale_x_continuous(limits = c(0,1), expand = c(0,0))+
      scale_y_continuous(limits = c(0,1), expand = c(0,0))+
      scale_fill_viridis_d(option = "plasma")+
      labs(title = paste("Concentración de canciones para el género:", input$filtro_genero), 
           subtitle = "Las zonas encendidas muestran la mayor concentración de canciones",
           x = "Bailabilidad (Danceability)",
           y = "Positividad Emocional (Valence)",
           fill = "Densidad") +
      theme_minimal() +
      theme(text = element_text(size = 14),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
            panel.grid.minor = element_blank(),
            legend.position = "right"
            )
  })
}

shinyApp(ui = ui, server = server)
