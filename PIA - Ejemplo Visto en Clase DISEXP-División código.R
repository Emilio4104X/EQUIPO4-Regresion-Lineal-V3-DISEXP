require(shiny)
require(ggplot2)
#Ramiro Daniel Aguilar González - 2177783
ui <- fluidPage(
  titlePanel("Análisis de Regresión Lineal"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Configuración de Datos"),
      radioButtons("data_source", "Fuente de datos:",
                   choices = c("Datos de ejemplo (mtcars)" = "example",
                               "Cargar archivo CSV" = "upload")),
      
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file", "Selecciona archivo CSV:",
                  accept = c(".csv"))
      ),
      
      uiOutput("x_var"),
      uiOutput("y_var"),
      
      hr(),
  # Cristopher Alejandro Alvarez Sandoval - 2100063
      h4("Opciones del Modelo"),
      
      checkboxInput("include_intercept", "Incluir intercepto en el modelo", value = TRUE),
      checkboxInput("log_transform", "Aplicar logaritmo a la variable dependiente (Y)", value = FALSE),
      
      sliderInput("conf_level", "Nivel de confianza:",
                  min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      
      hr(),
      
      actionButton("run_regression", "Ejecutar Regresión", 
                   class = "btn-primary"),
      
      hr(),
      
      h4("Resultados del Modelo"),
      verbatimTextOutput("model_summary")
    ),
#Alan Alberto González Mejía - 2103459   
    mainPanel(
      tabsetPanel(
        
        tabPanel("Gráfico de Dispersión",
                 plotOutput("scatter_plot", height = "500px")),
        
        tabPanel("Diagnóstico",
                 h4("Gráficos de diagnóstico del modelo"),
                 plotOutput("diagnostic_plots", height = "500px"),
                 hr(),
                 h4("Tabla de residuos"),
                 tableOutput("residuals_table")),
        
        tabPanel("Datos",
                 h4("Primeras 20 filas del conjunto de datos"),
                 tableOutput("data_table")),
# Mauricio González Bermúdez - 2101442       
        # PREDICCIONES
        tabPanel("Predicciones",
                 h4("Generar predicciones"),
                 uiOutput("prediction_inputs"),
                 actionButton("predict_button", "Predecir"),
                 hr(),
                 verbatimTextOutput("prediction_text"),
                 tableOutput("prediction_table")),
        
        #  RESUMEN & CORRELACIONES 
        tabPanel("Resumen y Correlaciones",
                 h4("Resumen estadístico de las variables numéricas"),
                 tableOutput("data_summary"),
                 hr(),
                 h4("Matriz de correlaciones"),
                 tableOutput("correlation_table")),
        
        # COR.TEST 
        tabPanel("Correlación",
                 h4("Prueba de correlación de Pearson (cor.test)"),
                 verbatimTextOutput("correlation_test"))
      )
    )
  )
)


# SERVIDOR
#Emilio Estrada Villegas - 2097136
server <- function(input, output, session) {
  
  data <- reactive({
    if (input$data_source == "example") mtcars
    else {
      req(input$file)
      read.csv(input$file$datapath)
    }
  })
  
  numeric_data <- reactive({
    df <- data()
    df[sapply(df, is.numeric)]
  })
# 
  # VARIABLES X
  output$x_var <- renderUI({
    df <- numeric_data()
    numeric_vars <- names(df)
    selectInput("x", "Variables independientes (X):", 
                choices = numeric_vars,
                selected = numeric_vars[1],
                multiple = TRUE)
  })
  
  # VARIABLE Y
  output$y_var <- renderUI({
    df <- numeric_data()
    numeric_vars <- names(df)
    default_y <- if (length(numeric_vars) >= 2) numeric_vars[2] else numeric_vars[1]
    selectInput("y", "Variable dependiente (Y):", 
                choices = numeric_vars,
                selected = default_y)
  })
# Rene Alejandro Garza Muñiz - 2099242
  # FORMULA DE REGRESION
  build_formula <- reactive({
    req(input$x, input$y)
    
    y_term <- if (isTRUE(input$log_transform)) paste0("log(", input$y, ")") else input$y
    rhs <- paste(input$x, collapse = " + ")
    
    if (isTRUE(input$include_intercept)) 
      as.formula(paste(y_term, "~", rhs))
    else 
      as.formula(paste(y_term, "~", rhs, "-1"))
  })
  
  # MODELO
  model <- eventReactive(input$run_regression, {
    df <- numeric_data()
    lm(build_formula(), data = df)
  })

  # RESULTADOS DEL MODELO
  output$model_summary <- renderPrint({
    req(model())
    print(summary(model()))
  })
  
  output$scatter_plot <- renderPlot({
    req(model())
  
    if (length(input$x) != 1) {
      plot.new()
      text(0.5, 0.5,
           "📌 La gráfica de dispersión solo puede mostrarse\ncuando seleccionas UNA variable independiente (X).",
           cex = 1.4)
      return()
    }
  # David Alejandro Padilla Cardona - 2094590 
    df <- numeric_data()
    
    ggplot(df, aes_string(x = input$x, y = input$y)) +
      geom_point(color = "steelblue", size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "red",
                  fill = "pink", alpha = 0.2) +
      labs(title = paste("Regresión Lineal:", input$y, "vs", input$x),
           x = input$x, y = input$y) +
      theme_minimal(base_size = 14)
  })

  output$diagnostic_plots <- renderPlot({
    req(model())
    par(mfrow = c(2, 2))
    plot(model())
  })
  
  output$residuals_table <- renderTable({
    req(model())
    m <- model()
    data.frame(Ajustado = fitted(m), Residuo = resid(m))[1:20, ]
  })
  
  output$data_table <- renderTable({
    head(data(), 20)
  })
  
  output$data_summary <- renderTable({
    df <- numeric_data()
    if (ncol(df) == 0) return(NULL)
    as.data.frame(summary(df))
  }, rownames = TRUE)
  
  output$correlation_table <- renderTable({
    df <- numeric_data()
    if (ncol(df) < 2) return(NULL)
    round(cor(df, use = "pairwise.complete.obs"), 3)
  }, rownames = TRUE)
  
  output$prediction_inputs <- renderUI({
    req(input$x)
    
    inputs <- lapply(input$x, function(var) {
      numericInput(paste0("pred_", var),
                   paste("Ingrese valor para", var, ":"),
                   value = NA)
    })
    
    do.call(tagList, inputs)
  })
# Alan Alberto González Mejía - 2103459 
  # PREDICCIÓN 
  
  prediction_result <- eventReactive(input$predict_button, {
    req(model())
    
    newdata <- list()
    
    for (var in input$x) {
      value <- input[[paste0("pred_", var)]]
      if (is.na(value)) return(NULL)
      newdata[[var]] <- value
    }
  #Ramiro Daniel Aguilar González - 2177783
    newdata <- as.data.frame(newdata)
    
    predict(model(), newdata, interval = "confidence", level = input$conf_level)
  })
  
  output$prediction_text <- renderPrint({
    req(prediction_result())
    print(prediction_result())
  })
  
  output$prediction_table <- renderTable({
    req(prediction_result())
    as.data.frame(prediction_result())
  })
 # Cristopher Daniel Alejandro - 2100063
   # COR.TEST 
  
  output$correlation_test <- renderPrint({
    req(input$x, input$y)
    df <- data()
    
    if (length(input$x) == 1) {
      print(cor.test(df[[input$x]], df[[input$y]]))
    } else {
      cat("Seleccione solo 1 X para realizar cor.test().")
    }
  })
}

shinyApp(ui = ui, server = server)





