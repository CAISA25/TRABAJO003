
library(shiny)
library(shinythemes)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(DescTools)
library(tidyr)

# Función segura para calcular la moda
moda_segura <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  freq <- table(x)
  max_freq <- max(freq)
  if (max_freq == 1) {
    return("No hay moda")
  }
  moda <- names(freq[freq == max_freq])
  if (length(moda) == 1) {
    return(moda)
  } else {
    return(paste(moda, collapse = ", "))
  }
}

ui <- dashboardPage(
  dashboardHeader(title = "Análisis Estadístico"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Importar Datos", tabName = "importar", icon = icon("file-import")),
      menuItem("Estadísticas Descriptivas", tabName = "descriptivas", icon = icon("chart-bar")),
      menuItem("Pruebas e Interpretación", tabName = "pruebas", icon = icon("flask")),
      menuItem("Extras", tabName = "extras", icon = icon("plus-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("importar",
              fluidRow(
                box(title = "Carga de archivo", width = 4, solidHeader = TRUE, status = "primary",
                    fileInput("archivo", "Sube archivo .csv o .xlsx", accept = c(".csv", ".xlsx")),
                    uiOutput("seleccion_vars"),
                    actionButton("analizar", "Iniciar análisis", icon = icon("play"), class = "btn btn-primary")
                ),
                box(title = "Vista previa de datos", width = 8, solidHeader = TRUE, status = "info",
                    tableOutput("vista_datos"))
              )
      ),
      tabItem("descriptivas",
              uiOutput("estadisticas_ui"),
              uiOutput("graficos_ui"),
              downloadButton("descargar_estadisticas", "📥 Descargar estadísticas")
      ),
      tabItem("pruebas",
              box(title = "Resultado de prueba", width = 12, solidHeader = TRUE, status = "success",
                  verbatimTextOutput("resultado_prueba"),
                  h4("📌 Interpretación:"),
                  verbatimTextOutput("texto_interpretacion"))
      ),
      tabItem("extras",
              fluidRow(
                box(title = "Análisis de normalidad (Shapiro-Wilk)", width = 6, solidHeader = TRUE, status = "warning",
                    verbatimTextOutput("normalidad")),
                box(title = "Valores atípicos detectados", width = 6, solidHeader = TRUE, status = "danger",
                    verbatimTextOutput("outliers"))
              ),
              fluidRow(
                box(title = "Boxplot comparativo", width = 12, solidHeader = TRUE, status = "info",
                    plotOutput("boxplot_grupos"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  datos <- reactive({
    req(input$archivo)
    if (grepl(".csv", input$archivo$name)) {
      read.csv(input$archivo$datapath)
    } else {
      read_excel(input$archivo$datapath)
    }
  })
  
  output$vista_datos <- renderTable({ head(datos()) })
  
  output$seleccion_vars <- renderUI({
    req(datos())
    checkboxGroupInput("variables", "Selecciona variables para analizar:",
                       choices = names(datos()), selected = names(datos())[1:2])
  })
  
  output$estadisticas_ui <- renderUI({
    req(input$variables)
    tablas <- lapply(input$variables, function(var) {
      datos_col <- datos()[[var]]
      tipo <- if (is.numeric(datos_col)) "numérica" else "categórica"
      tagList(
        h4(paste("Variable:", var, "-", tipo)),
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
              Moda = moda_segura(datos_col),
              Mínimo = min(datos_col, na.rm = TRUE),
              Máximo = max(datos_col, na.rm = TRUE),
              Rango = max(datos_col, na.rm = TRUE) - min(datos_col, na.rm = TRUE),
              Desv_Estandar = sd(datos_col, na.rm = TRUE),
              Coef_Variacion = round(sd(datos_col, na.rm = TRUE) / mean(datos_col, na.rm = TRUE), 3)
            )
          } else {
            as.data.frame(table(datos_col))
          }
        }, rownames = TRUE)
      })
    }
  })
  
  output$graficos_ui <- renderUI({
    req(input$variables)
    plots <- lapply(input$variables, function(var) {
      tagList(h4(paste("Gráfico:", var)), plotOutput(paste0("plot_", var)))
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
              geom_histogram(bins = 15, fill = "#2E86C1", color = "white") +
              theme_minimal() + labs(title = paste("Histograma de", v), x = v)
          } else {
            ggplot(data.frame(x = datos_col), aes(x = x)) +
              geom_bar(fill = "#E67E22") +
              theme_minimal() + labs(title = paste("Gráfico de barras de", v), x = v)
          }
        })
      })
    }
  })
  
  prueba_resultado <- reactive({
    req(input$variables)
    df <- datos()
    vars <- input$variables
    
    if (length(vars) != 2) return(NULL)
    
    var1 <- df[[vars[1]]]
    var2 <- df[[vars[2]]]
    
    if (is.numeric(var1) && (is.factor(var2) || is.character(var2))) {
      var2 <- as.factor(var2)
      if (length(levels(var2)) == 2) {
        return(t.test(var1 ~ var2))
      } else if (length(levels(var2)) > 2) {
        aov_model <- aov(var1 ~ var2)
        tukey <- TukeyHSD(aov_model)
        return(list(aov_model = aov_model, tukey = tukey))
      }
    } else if (is.numeric(var2) && (is.factor(var1) || is.character(var1))) {
      var1 <- as.factor(var1)
      if (length(levels(var1)) == 2) {
        return(t.test(var2 ~ var1))
      } else if (length(levels(var1)) > 2) {
        aov_model <- aov(var2 ~ var1)
        tukey <- TukeyHSD(aov_model)
        return(list(aov_model = aov_model, tukey = tukey))
      }
    }
    return(NULL)
  })
  
  output$resultado_prueba <- renderPrint({
    res <- prueba_resultado()
    if (is.null(res)) {
      cat("❌ No se pudo aplicar la prueba. Selecciona una variable numérica y otra categórica.")
    } else if (inherits(res, "htest")) {
      print(res)
    } else if (is.list(res)) {
      print(summary(res$aov_model))
      cat("\n📊 Prueba de Tukey:\n")
      print(res$tukey)
    }
  })
  
  output$texto_interpretacion <- renderPrint({
    res <- prueba_resultado()
    if (is.null(res)) {
      cat("No se puede interpretar. Asegúrate de seleccionar una variable numérica y otra categórica.")
    } else if (inherits(res, "htest")) {
      p <- res$p.value
      if (p < 0.05) {
        cat("✅ Hay diferencia significativa entre los grupos. (p =", round(p, 4), ")")
      } else {
        cat("ℹ️ No hay diferencia significativa entre los grupos. (p =", round(p, 4), ")")
      }
    } else if (is.list(res)) {
      p <- summary(res$aov_model)[[1]][["Pr(>F)"]][1]
      if (p < 0.05) {
        cat("✅ ANOVA muestra diferencias significativas entre grupos. (p =", round(p, 4), ")\n✔️ Revisa Tukey para saber qué grupos difieren.")
      } else {
        cat("ℹ️ ANOVA no muestra diferencias significativas entre grupos. (p =", round(p, 4), ")")
      }
    }
  })
  
  # Análisis extra: Normalidad
  output$normalidad <- renderPrint({
    req(input$variables)
    var <- input$variables[1]
    x <- datos()[[var]]
    if (is.numeric(x)) {
      test <- shapiro.test(x)
      print(test)
    } else {
      cat("Selecciona una variable numérica.")
    }
  })
  
  # Detección de valores atípicos
  output$outliers <- renderPrint({
    req(input$variables)
    var <- input$variables[1]
    x <- datos()[[var]]
    if (is.numeric(x)) {
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      outliers <- x[x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)]
      if (length(outliers) > 0) {
        cat("Se encontraron", length(outliers), "valores atípicos:\n")
        print(outliers)
      } else {
        cat("No se detectaron valores atípicos.")
      }
    } else {
      cat("Selecciona una variable numérica.")
    }
  })
  
  # Boxplot por grupos
  output$boxplot_grupos <- renderPlot({
    req(input$variables)
    df <- datos()
    if (length(input$variables) == 2) {
      var1 <- df[[input$variables[1]]]
      var2 <- df[[input$variables[2]]]
      if (is.numeric(var1) && (is.factor(var2) || is.character(var2))) {
        var2 <- as.factor(var2)
        ggplot(df, aes(x = var2, y = var1)) +
          geom_boxplot(fill = "#58D68D") +
          labs(x = input$variables[2], y = input$variables[1]) +
          theme_minimal()
      } else if (is.numeric(var2) && (is.factor(var1) || is.character(var1))) {
        var1 <- as.factor(var1)
        ggplot(df, aes(x = var1, y = var2)) +
          geom_boxplot(fill = "#F5B041") +
          labs(x = input$variables[1], y = input$variables[2]) +
          theme_minimal()
      }
    }
  })
  
  # Exportar tabla
  output$descargar_estadisticas <- downloadHandler(
    filename = function() { "estadisticas_descriptivas.csv" },
    content = function(file) {
      req(input$variables)
      df <- datos()
      resultados <- lapply(input$variables, function(v) {
        col <- df[[v]]
        if (is.numeric(col)) {
          data.frame(
            Variable = v,
            Media = mean(col, na.rm = TRUE),
            Mediana = median(col, na.rm = TRUE),
            Moda = moda_segura(col),
            Min = min(col, na.rm = TRUE),
            Max = max(col, na.rm = TRUE),
            Rango = max(col, na.rm = TRUE) - min(col, na.rm = TRUE),
            Desv_Estandar = sd(col, na.rm = TRUE),
            Coef_Variacion = round(sd(col, na.rm = TRUE) / mean(col, na.rm = TRUE), 3)
          )
        }
      })
      write.csv(do.call(rbind, resultados), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
