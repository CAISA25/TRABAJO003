library(shiny)
library(shinythemes)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(DescTools)
library(tidyr)
library(stats)
library(coin)

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
      menuItem("Extras", tabName = "extras", icon = icon("plus-circle")),
      menuItem("Nuevas Pruebas", tabName = "nuevas_pruebas", icon = icon("plus"))
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
      ),
      tabItem("nuevas_pruebas",
              fluidRow(
                box(title = "Prueba Chi-cuadrada", width = 6, solidHeader = TRUE, status = "warning",
                    verbatimTextOutput("chi_cuadrada")),
                box(title = "Teorema del Límite Central", width = 6, solidHeader = TRUE, status = "info",
                    plotOutput("tlc_plot"),
                    verbatimTextOutput("tlc"))
              ),
              fluidRow(
                box(title = "Prueba de Wilcoxon", width = 6, solidHeader = TRUE, status = "danger",
                    verbatimTextOutput("wilcoxon")),
                box(title = "Correlación de Spearman y Pearson", width = 6, solidHeader = TRUE, status = "success",
                    verbatimTextOutput("correlacion"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  datos <- reactive({
    req(input$archivo)
    tryCatch({
      if (grepl(".csv$", input$archivo$name, ignore.case = TRUE)) {
        read.csv(input$archivo$datapath)
      } else {
        read_excel(input$archivo$datapath)
      }
    }, error = function(e) {
      showNotification("Error al leer el archivo", type = "error")
      NULL
    })
  })
  
  output$vista_datos <- renderTable({
    req(datos())
    head(datos())
  })
  
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
              Estadístico = c("Media", "Mediana", "Moda", "Mínimo", "Máximo", 
                              "Rango", "Desv. Estandar", "Coef. Variación"),
              Valor = c(
                mean(datos_col, na.rm = TRUE),
                median(datos_col, na.rm = TRUE),
                moda_segura(datos_col),
                min(datos_col, na.rm = TRUE),
                max(datos_col, na.rm = TRUE),
                max(datos_col, na.rm = TRUE) - min(datos_col, na.rm = TRUE),
                sd(datos_col, na.rm = TRUE),
                round(sd(datos_col, na.rm = TRUE) / mean(datos_col, na.rm = TRUE), 3)
              )
            )
          } else {
            as.data.frame(table(datos_col)) %>% 
              rename(Categoría = 1, Frecuencia = 2)
          }
        }, rownames = FALSE)
      })
    }
  })
  
  output$graficos_ui <- renderUI({
    req(input$variables)
    plots <- lapply(input$variables, function(var) {
      tagList(
        h4(paste("Gráfico:", var)), 
        plotOutput(paste0("plot_", var)),
        br()
      )
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
              geom_density(aes(y = ..count..), color = "#1F618D", size = 1) +
              theme_minimal() + 
              labs(title = paste("Histograma de", v), x = v, y = "Frecuencia")
          } else {
            ggplot(data.frame(x = datos_col), aes(x = x)) +
              geom_bar(fill = "#E67E22") +
              theme_minimal() + 
              labs(title = paste("Gráfico de barras de", v), x = v) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
    
    if (is.numeric(var1) && is.numeric(var2)) {
      return(cor.test(var1, var2, method = "pearson"))
    } else if (is.factor(var1) && is.factor(var2) && length(levels(var1)) == 2 && length(levels(var2)) == 2) {
      tabla <- table(var1, var2)
      if (all(dim(tabla) == c(2, 2))) {
        return(mcnemar.test(tabla))
      }
    } else if (all(sapply(df[vars], function(x) all(x %in% c(0, 1))))) {
      return(CochranQTest(as.matrix(df[vars])))
    } else if (is.numeric(var1) && (is.factor(var2) || is.character(var2))) {
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
      cat("❌ No se pudo aplicar la prueba. Selecciona variables adecuadas.")
    } else {
      print(res)
    }
  })
  
  output$texto_interpretacion <- renderPrint({
    res <- prueba_resultado()
    if (is.null(res)) {
      cat("No se puede interpretar. Asegúrate de seleccionar variables válidas.")
    } else if (inherits(res, "htest")) {
      p <- res$p.value
      if (p < 0.05) {
        cat("✅ Diferencia o relación significativa encontrada. (p =", round(p, 4), ")")
      } else {
        cat("ℹ️ No hay diferencia o relación significativa. (p =", round(p, 4), ")")
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
  
  # Análisis de normalidad (Shapiro-Wilk)
  output$normalidad <- renderPrint({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.numeric(datos_col) && length(datos_col) >= 3 && length(datos_col) <= 5000) {
      normalidad_test <- shapiro.test(datos_col)
      cat("Resultado de la prueba de normalidad (Shapiro-Wilk):\n")
      cat("Valor p: ", round(normalidad_test$p.value, 4), "\n")
      if (normalidad_test$p.value < 0.05) {
        cat("❌ La variable NO sigue una distribución normal.\n")
      } else {
        cat("✅ La variable sigue una distribución normal.\n")
      }
    } else {
      cat("Selecciona una variable numérica con entre 3 y 5000 observaciones para el análisis de normalidad.")
    }
  })
  
  # Detección de valores atípicos
  output$outliers <- renderPrint({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.numeric(datos_col)) {
      q1 <- quantile(datos_col, 0.25, na.rm = TRUE)
      q3 <- quantile(datos_col, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      outliers <- datos_col[datos_col < (q1 - 1.5 * iqr) | datos_col > (q3 + 1.5 * iqr)]
      
      cat("Detección de valores atípicos:\n")
      cat("Q1:", q1, "| Q3:", q3, "| IQR:", iqr, "\n")
      if (length(outliers) > 0) {
        cat("Se encontraron", length(outliers), "valores atípicos:\n")
        print(sort(outliers))
      } else {
        cat("No se detectaron valores atípicos.\n")
      }
    } else {
      cat("Selecciona una variable numérica para detectar valores atípicos.")
    }
  })
  
  # Boxplot comparativo
  output$boxplot_grupos <- renderPlot({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.numeric(datos_col)) {
      ggplot(datos(), aes(y = datos_col)) +
        geom_boxplot(fill = "#2E86C1", color = "#1F618D") +
        theme_minimal() +
        labs(title = paste("Boxplot de", var), y = var) +
        theme(axis.text.x = element_blank())
    }
  })
  
  # Prueba Chi-cuadrada
  output$chi_cuadrada <- renderPrint({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.factor(datos_col) || is.character(datos_col)) {
      tabla <- table(datos_col)
      if (length(tabla) > 1) {
        chisq_result <- chisq.test(tabla)
        cat("Resultado de la prueba Chi-cuadrada:\n")
        cat("Estadístico Chi-cuadrada: ", round(chisq_result$statistic, 3), "\n")
        cat("Grados de libertad: ", chisq_result$parameter, "\n")
        cat("Valor p: ", round(chisq_result$p.value, 4), "\n")
        cat("\nFrecuencias esperadas:\n")
        print(chisq_result$expected)
        cat("\n")
        if (chisq_result$p.value < 0.05) {
          cat("✅ La distribución NO es uniforme (p < 0.05).\n")
        } else {
          cat("ℹ️ No hay evidencia para rechazar la uniformidad (p >= 0.05).\n")
        }
      } else {
        cat("Se necesitan al menos 2 categorías para la prueba Chi-cuadrada.")
      }
    } else {
      cat("Selecciona una variable categórica para la prueba Chi-cuadrada.")
    }
  })
  
  # Teorema del Límite Central - Corrección del error de paréntesis y llaves
  output$tlc_plot <- renderPlot({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.numeric(datos_col)) {
      set.seed(123)
      muestras <- replicate(1000, mean(sample(datos_col, size = min(30, length(datos_col)), replace = TRUE)))
      
      ggplot(data.frame(x = muestras), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "#3498DB", color = "white") +
        geom_density(color = "#2874A6", size = 1) +
        theme_minimal() +
        labs(title = "Distribución de medias muestrales",
             subtitle = "Teorema del Límite Central",
             x = "Media muestral", y = "Densidad")
    }
  })
  
  # Teorema del Límite Central - Versión corregida
  output$tlc <- renderPrint({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.numeric(datos_col)) {
      set.seed(123)
      muestras <- replicate(1000, mean(sample(datos_col, length(datos_col), replace = TRUE)))
      media_muestral <- mean(muestras)
      sd_muestral <- sd(muestras)
      
      cat("Teorema del Límite Central:\n")
      cat("Media poblacional: ", round(mean(datos_col, na.rm = TRUE), 3), "\n")
      cat("Media de las medias muestrales: ", round(media_muestral, 3), "\n")
      cat("Desviación estándar poblacional: ", round(sd(datos_col, na.rm = TRUE), 3), "\n")
      cat("Desviación estándar de las medias muestrales: ", round(sd_muestral, 3), "\n")
      cat("La distribución de las medias muestrales debería aproximarse a una normal.\n")
    } else {
      cat("Selecciona una variable numérica para realizar el TLC.")
    }
  })
  
  # Prueba de Wilcoxon
  output$wilcoxon <- renderPrint({
    req(input$variables)
    if (length(input$variables) == 2) {
      var1 <- datos()[[input$variables[1]]]
      var2 <- datos()[[input$variables[2]]]
      if (is.numeric(var1) && is.numeric(var2)) {
        wilcox_test <- wilcox.test(var1, var2)
        cat("Prueba de Wilcoxon (Mann-Whitney):\n")
        cat("Estadístico W: ", wilcox_test$statistic, "\n")
        cat("Valor p: ", round(wilcox_test$p.value, 4), "\n")
        if (wilcox_test$p.value < 0.05) {
          cat("✅ Existe diferencia significativa entre las distribuciones.\n")
        } else {
          cat("ℹ️ No hay diferencia significativa entre las distribuciones.\n")
        }
      } else {
        cat("Selecciona dos variables numéricas.")
      }
    } else {
      cat("Selecciona exactamente dos variables.")
    }
  })
  
  # Correlación de Spearman y Pearson
  output$correlacion <- renderPrint({
    req(input$variables)
    if (length(input$variables) == 2) {
      var1 <- datos()[[input$variables[1]]]
      var2 <- datos()[[input$variables[2]]]
      if (is.numeric(var1) && is.numeric(var2)) {
        pearson <- cor.test(var1, var2, method = "pearson")
        spearman <- cor.test(var1, var2, method = "spearman")
        
        cat("Correlación de Pearson:\n")
        cat("Coeficiente: ", round(pearson$estimate, 3), "\n")
        cat("Valor p: ", round(pearson$p.value, 4), "\n")
        cat("Intervalo de confianza (95%): [", 
            round(pearson$conf.int[1], 3), ", ", 
            round(pearson$conf.int[2], 3), "]\n")
        
        cat("\nCorrelación de Spearman:\n")
        cat("Coeficiente: ", round(spearman$estimate, 3), "\n")
        cat("Valor p: ", round(spearman$p.value, 4), "\n")
        
        cat("\nInterpretación:\n")
        if (pearson$p.value < 0.05) {
          cat("✅ Pearson: Correlación significativa.\n")
        } else {
          cat("ℹ️ Pearson: Correlación no significativa.\n")
        }
        
        if (spearman$p.value < 0.05) {
          cat("✅ Spearman: Correlación significativa.\n")
        } else {
          cat("ℹ️ Spearman: Correlación no significativa.\n")
        }
      } else {
        cat("Selecciona dos variables numéricas.")
      }
    } else {
      cat("Selecciona exactamente dos variables.")
    }
  })
  
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
            Tipo = "Numérica",
            Media = mean(col, na.rm = TRUE),
            Mediana = median(col, na.rm = TRUE),
            Moda = moda_segura(col),
            Min = min(col, na.rm = TRUE),
            Max = max(col, na.rm = TRUE),
            Rango = max(col, na.rm = TRUE) - min(col, na.rm = TRUE),
            Desv_Estandar = sd(col, na.rm = TRUE),
            Coef_Variacion = round(sd(col, na.rm = TRUE) / mean(col, na.rm = TRUE), 3)
          )
        } else {
          data.frame(
            Variable = v,
            Tipo = "Categórica",
            Categorías = length(unique(col)),
            Moda = moda_segura(col)
          )
        }
      })
      write.csv(do.call(rbind, resultados), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
