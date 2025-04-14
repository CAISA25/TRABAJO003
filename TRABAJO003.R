# app.R
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DescTools)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("📊 Plataforma de Análisis Estadístico"),
  
  tabsetPanel(id = "tabs",
              tabPanel("📂 Importar Datos",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("archivo", "Sube tu archivo (.csv o .xlsx):", accept = c(".csv", ".xlsx")),
                           uiOutput("seleccion_vars"),
                           actionButton("analizar", "📈 Iniciar Análisis", class = "btn-primary")
                         ),
                         mainPanel(
                           h4("Vista previa de los datos cargados"),
                           tableOutput("vista_datos")
                         )
                       )
              ),
              
              tabPanel("📊 Estadísticas Descriptivas",
                       h4("Resumen estadístico por variable"),
                       uiOutput("estadisticas_ui"),
                       h4("Gráficos diferenciados por variable"),
                       uiOutput("graficos_ui")
              ),
              
              tabPanel("🧪 Pruebas e Interpretación",
                       h4("Resultado de la prueba estadística"),
                       verbatimTextOutput("resultado_prueba"),
                       h4("📌 Interpretación del resultado"),
                       verbatimTextOutput("texto_interpretacion")
              )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$archivo)
    ext <- tools::file_ext(input$archivo$name)
    df <- if (ext == "csv") {
      read.csv(input$archivo$datapath)
    } else {
      tryCatch(read_excel(input$archivo$datapath), error = function(e) {
        showNotification("⚠️ Error al leer el archivo Excel.", type = "error")
        return(NULL)
      })
    }
    if (!is.null(df)) {
      names(df) <- make.names(names(df))  # Limpiar nombres
    }
    df
  })
  
  output$vista_datos <- renderTable({
    head(datos(), 10)
  })
  
  output$seleccion_vars <- renderUI({
    req(datos())
    selectInput("variables", "Selecciona 2 o más variables:", 
                choices = names(datos()), multiple = TRUE)
  })
  
  observeEvent(input$analizar, {
    updateTabsetPanel(session, "tabs", selected = "📊 Estadísticas Descriptivas")
  })
  
  # ESTADÍSTICAS DESCRIPTIVAS
  output$estadisticas_ui <- renderUI({
    req(input$variables)
    tablas <- lapply(input$variables, function(var) {
      datos_col <- datos()[[var]]
      tipo <- if (is.numeric(datos_col)) "numérica" else "categórica"
      
      tagList(
        h5(paste("Variable:", var, "-", tipo)),
        tableOutput(paste0("tabla_", var))
      )
    })
    do.call(tagList, tablas)
  })
  
  observe({
    req(input$variables)
    for (var in input$variables) {
      local({
        v <- var
        output[[paste0("tabla_", v)]] <- renderTable({
          datos_col <- datos()[[v]]
          if (is.numeric(datos_col)) {
            data.frame(
              Media = mean(datos_col, na.rm = TRUE),
              Mediana = median(datos_col, na.rm = TRUE),
              Moda = unique(DescTools::Mode(datos_col))[1],
              Mínimo = min(datos_col, na.rm = TRUE),
              Máximo = max(datos_col, na.rm = TRUE),
              Rango = max(datos_col, na.rm = TRUE) - min(datos_col, na.rm = TRUE),
              `Desv. Estándar` = sd(datos_col, na.rm = TRUE),
              `Coef. Variación` = sd(datos_col, na.rm = TRUE) / mean(datos_col, na.rm = TRUE)
            )
          } else {
            as.data.frame(table(datos_col))
          }
        })
      })
    }
  })
  
  # GRÁFICOS
  output$graficos_ui <- renderUI({
    req(input$variables)
    plots <- lapply(input$variables, function(var) {
      plotname <- paste0("plot_", var)
      tagList(h5(paste("Gráfico de", var)), plotOutput(plotname))
    })
    do.call(tagList, plots)
  })
  
  observe({
    req(input$variables)
    for (var in input$variables) {
      local({
        v <- var
        output[[paste0("plot_", v)]] <- renderPlot({
          datos_col <- datos()[[v]]
          if (is.numeric(datos_col)) {
            ggplot(data.frame(x = datos_col), aes(x = x)) +
              geom_histogram(bins = 15, fill = "#3498DB", color = "white") +
              labs(title = paste("Histograma de", v), x = v, y = "Frecuencia") +
              theme_minimal()
          } else {
            ggplot(data.frame(x = datos_col), aes(x = x)) +
              geom_bar(fill = "#E67E22") +
              labs(title = paste("Gráfico de barras de", v), x = v, y = "Frecuencia") +
              theme_minimal()
          }
        })
      })
    }
  })
  
  # PRUEBA ESTADÍSTICA
  prueba_resultado <- reactive({
    req(input$variables)
    vars <- input$variables
    df <- datos()
    
    if (length(vars) == 2) {
      v1 <- df[[vars[1]]]
      v2 <- df[[vars[2]]]
      
      if (is.numeric(v1) && is.numeric(v2)) {
        tryCatch(t.test(v1, v2), error = function(e) NULL)
      } else if (!is.numeric(v1) && !is.numeric(v2)) {
        tryCatch(chisq.test(table(v1, v2)), error = function(e) NULL)
      } else {
        NULL
      }
    } else if (length(vars) > 2) {
      num_vars <- vars[sapply(df[, vars], is.numeric)]
      if (length(num_vars) >= 2) {
        formula <- as.formula(paste(num_vars[1], "~", paste(num_vars[-1], collapse = "+")))
        tryCatch(aov(formula, data = df), error = function(e) NULL)
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
  
  output$resultado_prueba <- renderPrint({
    res <- prueba_resultado()
    if (is.null(res)) {
      cat("❌ No se pudo aplicar la prueba. Verifica el tipo y número de variables.")
    } else {
      print(res)
    }
  })
  
  output$texto_interpretacion <- renderPrint({
    res <- prueba_resultado()
    if (is.null(res)) {
      return("Sin resultados para interpretar.")
    }
    
    if (inherits(res, "htest")) {
      p <- res$p.value
      if (p < 0.05) {
        cat("✅ p =", round(p, 4), "- Existe diferencia significativa.")
      } else {
        cat("ℹ️ p =", round(p, 4), "- No hay diferencia significativa.")
      }
    } else if (inherits(res, "aov")) {
      p <- summary(res)[[1]][["Pr(>F)"]][1]
      if (p < 0.05) {
        cat("✅ p =", round(p, 4), "- Al menos un grupo difiere significativamente.")
      } else {
        cat("ℹ️ p =", round(p, 4), "- No hay diferencias significativas entre los grupos.")
      }
    }
  })
}

shinyApp(ui, server)
