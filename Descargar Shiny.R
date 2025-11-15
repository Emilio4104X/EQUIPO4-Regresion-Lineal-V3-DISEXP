install.packages("shiny")
# Cargar la librería Shiny
library(shiny)

# 1. Definir la Interfaz de Usuario (UI)
ui <- fluidPage(
  # Título de la aplicación
  titlePanel("Histograma Interactivo del Geiser Old Faithful"),
  
  # Diseño con un panel lateral y un panel principal
  sidebarLayout(
    # Panel lateral para los controles de entrada (Input)
    sidebarPanel(
      # Widget de entrada: un deslizador (slider)
      sliderInput(
        inputId = "bins", # ID que se usará en el server
        label = "Número de 'bins' (barras):",
        min = 1,
        max = 50,
        value = 30
      )
    ),
    
    # Panel principal para mostrar los resultados (Output)
    mainPanel(
      # Elemento de salida: un gráfico (plot)
      plotOutput("distPlot") # ID del output que se generará en el server
    )
  )
)

# 2. Definir la lógica del servidor (Server)
server <- function(input, output) {
  
  # Generar el histograma de forma reactiva
  output$distPlot <- renderPlot({
    
    # Acceder al valor del sliderInput usando input$ID
    x    <- faithful[, 2]  # Elige la columna 'waiting' de los datos 'faithful'
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # Crea el histograma
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         main = paste("Intervalos de espera con", input$bins, "bins"))
  })
}

# 3. Ejecutar la aplicación
shinyApp(ui = ui, server = server)