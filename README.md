# Music Explorer: Análisis interactivo de canciones y artistas
 
Aplicación web desarrollada en **R Shiny** que permite explorar de forma interactiva un conjunto de datos de canciones y artistas, analizando características musicales como bailabilidad, energía, polaridad emocional, tempo, entre otras.
 
## Descripción
 
Este proyecto fue desarrollado como parte del curso **Programación para Estadística I XS-0129** de la Universidad de Costa Rica.
 
La aplicación cuenta con 5 secciones principales:
 
1. **Exploración Inicial**: estadísticas descriptivas, histogramas y tablas de frecuencia para cada variable del dataset.
2. **Comparación por Género**: boxplots y gráficos de barras que comparan características musicales entre distintos géneros.
3. **Energy vs Loudness**: análisis de correlación entre energía y volumen por género.
4. **Bailabilidad y Polaridad Emocional**: mapas de densidad 2D que muestran la relación entre danceability y valence por género, junto con el top 3 de canciones.
5. **Recomendación Personalizada**: un recomendador de canciones filtrable por género, artista y rangos de características musicales.
 
## Tecnologías utilizadas
 
- [R](https://www.r-project.org/)
- [Shiny](https://shiny.posit.co/)
- [ggplot2](https://ggplot2.tidyverse.org/) — visualización de datos
- [dplyr](https://dplyr.tidyverse.org/) — manipulación de datos
- [readxl](https://readxl.tidyverse.org/) — lectura de archivos Excel
- [shinythemes](https://rstudio.github.io/shinythemes/) — temas visuales
 
## Estructura del repositorio
 
```
├── app.R              # Código principal de la aplicación (UI + servidor)
├── dataset.xlsx        # Dataset con la información de canciones y artistas
├── .gitignore          # Archivos/carpetas ignorados por git
└── README.md           # Este archivo
```
 
## Instalación
 
Sigue estos pasos para configurar el proyecto en tu computadora:
 
1. Clona este repositorio:
   ```bash
   git clone https://github.com/Santiago-MunozC/Santiago-Mu-oz-UCR.git
   ```
2. Abre el proyecto en **RStudio**.
3. Instala los paquetes de R necesarios (si no los tienes ya instalados):
   ```r
   install.packages(c("shiny", "ggplot2", "readxl", "dplyr", "shinythemes"))
   ```
4. Verifica que el archivo `dataset.xlsx` esté ubicado en la misma carpeta que `app.R`.
 
## Uso
 
Una vez instalado el proyecto:
 
1. Abre el archivo `app.R` en RStudio.
2. Presiona el botón **"Run App"** ubicado en la parte superior del editor, o ejecuta desde la consola:
   ```r
   shiny::runApp("app.R")
   ```
3. La aplicación se abrirá en una ventana o en tu navegador. Navega entre las 5 pestañas de la barra superior para explorar cada sección:
   - Selecciona variables o géneros musicales en los menús desplegables de la izquierda.
   - Los gráficos y tablas se actualizan automáticamente según tus selecciones.
   - En la pestaña **Recomendación Personalizada**, ajusta los filtros deseados y presiona **"Obtener recomendación"** para recibir una canción sugerida.
 
## Sobre el dataset
 
El dataset contiene canciones de los artistas más populares del año 2023, con las siguientes variables principales:
 
| Variable | Descripción |
|---|---|
| `Track` | Nombre de la canción |
| `Artist` | Artista |
| `Genre` | Género musical |
| `Album_type` | Tipo de lanzamiento (álbum, single, etc.) |
| `Danceability` | Qué tan bailable es la canción |
| `Energy` | Intensidad percibida |
| `Valence` | Positividad emocional |
| `Loudness` | Volumen promedio (dB) |
| `Tempo` | Ritmo (BPM) |
| `most_playedon` | Plataforma con más reproducciones |
| `Album` | Nombre del álbum al que pertenece la canción |
| `Speechiness` | Corresponde a la proporción de contenido hablado que está presente en la canción |
| `Acousticness` | Probabilidad de que la canción sea acústica |
| `Instrumentalness` | Probabilidad de que en la canción predomine el instrumental y contenga pocas voces |
| `Liveness` | Estimación de la interpretación en vivo dentro de una grabación |
| `Duration_min` | Duración de la canción en minutos |
 
## Autores
 
- **Dariana Calderón Sáenz** — Carnet: C5D485
- **Santiago Muñoz Córdoba** — Carnet: C5H708
- **Tanya Rivera Vargas** — Carnet: C5I864
- **María Celeste Salazar Salas** — Carnet: C5J564
