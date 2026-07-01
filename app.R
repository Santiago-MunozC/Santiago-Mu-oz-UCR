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
                   plotOutput("grafico_densidad"),
                   
                   hr(),
                   
                   h4("Interpretación del Patrón"),
                   textOutput("texto_interpretacion"),
                   
                   hr(),
                   
                   h4("Top 3 Canciones con Mayor Bailabilidad y Positividad"),
                   tableOutput("tabla_top_canciones")
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
    
    #Gráfico densidad
    ggplot(datos_filtrados, aes(x = Danceability, y = Valence)) +
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
      return("El Pop muestra una altísima concentración en la zona centro-derecha del gráfico. Esto indica que la gran mayoría de sus canciones mantienen niveles de bailabilidad elevados (entre 0.6 y 0.75) acompañados de una positividad emocional intermedia-alta (Valence cercano a 0.5). En general, consolida el perfil de este género como música predominantemente rítmica y animada.")
    } else if (genero == "Rock") {
      return("El Rock presenta su núcleo de máxima concentración en la región central e inferior-izquierda del gráfico. Esto refleja que la mayoría de sus canciones poseen una bailabilidad moderada (entre 0.40 y 0.55) acompañada de una positividad emocional intermedia-baja (Valence entre 0.30 y 0.55), consolidando un perfil musical con un ritmo menos marcado y una carga emocional más seria o melancólica.")
    } else if (genero == "Rap") {
      return("El Rap presenta una concentración muy marcada en la sección centro-derecha del gráfico. La zona de máxima densidad revela que la inmensa mayoría de las canciones poseen niveles de bailabilidad altos (entre 0.70 y 0.82), mientras que su positividad emocional se mantiene en un rango intermedio o neutro (Valence entre 0.40 y 0.55). Esto consolida un perfil rítmicamente muy enérgico pero con una carga emocional balanceada.")
    } else if (genero == "Reggaetón") {
      return("El Reggaetón exhibe una de las concentraciones más compactas y desplazadas hacia la esquina superior derecha de todo el dataset. El núcleo de máxima densidad se ubica en niveles de bailabilidad sobresalientes (entre 0.78 y 0.85) con una positividad emocional alta (Valence entre 0.60 y 0.70). Este comportamiento define la naturaleza del género: producciones diseñadas específicamente para el baile con vibras intensamente festivas y animadas.")
    } else if (genero == "K-pop") {
      return("El K-pop exhibe una concentración focalizada en el cuadrante superior derecho del gráfico. La zona de máxima densidad revela que las canciones se agrupan en niveles de bailabilidad elevados (entre 0.70 y 0.77) junto con una positividad emocional notablemente alta (Valence entre 0.65 y 0.72). Estas características reflejan la naturaleza comercial del género: vibra optimista, brillante y altamente coreográfica.")
    } else if (genero == "Electrónica") {
      return("La Electrónica exhibe un comportamiento centrado en el gráfico. Su zona de máxima densidad revela que sus pistas se agrupa en niveles de bailabilidad moderados-altos (entre 0.58 y 0.65) junto con una positividad emocional intermedia-neutra (Valence entre 0.42 y 0.50). El núcleo principal demuestra un equilibrio sonoro constante y menos polarizado emocionalmente.")
    } else if (genero == "R&B") {
      return("El R&B presenta una distribución dispersa y multimodal muy interesante en la zona centro-derecha del gráfico. Sus núcleos de máxima densidad revelan que el género mantiene una bailabilidad sólida y constante (entre 0.55 y 0.70), pero se divide en dos perfiles emocionales claros: un grupo de canciones con positividad intermedia-baja (Valence en torno a 0.45) y otro de vibra más alegre y enérgica (Valence cercano a 0.65). Demuestra la versatilidad del género para alternar entre baladas íntimas y pistas rítmicas.")
    } else {
      return("El género Alternative muestra un núcleo central bien definido en la región media-baja del gráfico. Su punto de máxima densidad indica que la mayoría de los temas se sitúan en una bailabilidad moderada (entre 0.55 y 0.62) con una positividad emocional intermedia o neutra (Valence entre 0.38 y 0.45). Sin embargo, tiene una amplia dispersión hacia el cuadrante inferior izquierdo, reflejando la presencia de subgrupos de canciones más acústicas, lentas y de carácter melancólico.")
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
        `Canción` = Track, 
        `Artista` = Artist, 
        `Bailabilidad` = Danceability, 
        `Positividad Emocional` = Valence
      )
    
    return(top_datos)
  }, digits = 2, align = 'c') # 2 decimales y centrado
  
}

shinyApp(ui = ui, server = server)
