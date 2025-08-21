# === 1. Librerías ===
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

# === 2. Configuración de Rutas ===
MATRICES_DIR <- ("..","Results", "matrices_csv")

# === 3. Carga de Datos Preprocesados (solo lo necesario) ===
decadas_svd_avail <- c()
all_svd_dir <- file.path(MATRICES_DIR, "Todas")
if (dir.exists(all_svd_dir)) {
  svd_files <- list.files(all_svd_dir, pattern = "SVD_\\d{4}s_Todas\\.rds", full.names = FALSE)
  if (length(svd_files) > 0) { 
    decadas_svd_avail <- sort(as.numeric(gsub("SVD_(\\d{4})s_Todas\\.rds", "\\1", svd_files)))
  }
}

# === UI de Shiny ===
ui <- fluidPage(
  titlePanel("Análisis de Componentes Principales (SVD)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("decada", "Década:",
                  choices = as.character(decadas_svd_avail),
                  selected = if(length(decadas_svd_avail) > 0) as.character(max(decadas_svd_avail)) else NULL),
      
      h4("Análisis de Matriz Específica (U, V, D)"),
      selectInput("matriz_seleccionada", "Seleccionar Matriz:",
                  choices = c("U (Modo Espacial)" = "U", "V (Modo Temporal)" = "V", "D (Valor Singular)" = "D"),
                  selected = "U"),
      
      uiOutput("temporal_mode_controls"),
      uiOutput("indice_input"),
      h4("Valor del Punto Seleccionado:"),
      textOutput("valor_punto_seleccionado"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  tabPanel("Scree Plot Log-Log", plotOutput("scree_plot", height = "500px")),
                  tabPanel("Análisis Temporal/Global", plotOutput("temporal_global_plot", height = "600px"))
      )
    )
  )
)

# === Server de Shiny ===
server <- function(input, output, session) {
  
  svd_objeto_reactivo <- reactive({
    req(input$decada)
    nombre_carpeta_zona <- "Todas"
    ruta_svd_completo <- file.path(MATRICES_DIR, nombre_carpeta_zona, paste0("SVD_", input$decada, "s_", nombre_carpeta_zona, ".rds"))
    if (file.exists(ruta_svd_completo)) {
      readRDS(ruta_svd_completo)
    } else {
      NULL
    }
  })
  
  # --- Renderizar el Scree Plot Log-Log ---
  output$scree_plot <- renderPlot({
    svd_data <- svd_objeto_reactivo()
    req(svd_data)
    valores_singulares <- svd_data$d
    df_scree <- data.frame(id = 1:length(valores_singulares), sv = valores_singulares) %>%
      dplyr::filter(sv > 0 & !is.na(sv))
    
    if(nrow(df_scree) == 0) { return(NULL) }
    
    zona_label <- "Global"
    ggplot(df_scree, aes(x = log10(id), y = log10(sv))) +
      geom_point(color = "darkred", size = 3, alpha = 0.7) +
      geom_line(color = "red", linetype = "dashed", alpha = 0.5) +
      labs(title = paste("Scree Plot (Log-Log) para la Década de", input$decada, "en la Zona", zona_label),
           x = "Log10 (Orden del Componente)",
           y = "Log10 (Valor Singular)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # --- Control para seleccionar el modo en el análisis temporal ---
  output$temporal_mode_controls <- renderUI({
    req(input$matriz_seleccionada)
    if (input$matriz_seleccionada == "V") {
      tagList(
        sliderInput("v_mode_selection", "Modo Temporal (columna de V):",
                    min = 1, max = 50, value = 1, step = 1)
      )
    }
  })
  
  # --- Gráfico para Análisis Temporal/Global ---
  output$temporal_global_plot <- renderPlot({
    req(input$matriz_seleccionada, input$decada)
    if (input$matriz_seleccionada == "V") {
      req(input$v_mode_selection)
      modo_v_seleccionado <- as.numeric(input$v_mode_selection)
      df_v_combined <- data.frame(decade = character(), month_index = numeric(), v_value = numeric())
      for (d in decadas_svd_avail) {
        zona_dir_name_all <- "Todas"
        ruta_svd_completo_v <- file.path(MATRICES_DIR, zona_dir_name_all, paste0("SVD_", d, "s_", zona_dir_name_all, ".rds"))
        if (file.exists(ruta_svd_completo_v)) {
          svd_data_decade <- readRDS(ruta_svd_completo_v)
          if (!is.null(svd_data_decade$v) && modo_v_seleccionado <= ncol(svd_data_decade$v)) {
            v_values_for_decade <- svd_data_decade$v[, modo_v_seleccionado]
            df_v_decade <- data.frame(decade = paste0(d, "s"), month_index = 1:length(v_values_for_decade), v_value = v_values_for_decade)
            df_v_combined <- bind_rows(df_v_combined, df_v_decade)
          }
        }
      }
      if (nrow(df_v_combined) == 0) { return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No hay datos de Modo V para la combinación seleccionada.", size = 5)) }
      
      # *** CORRECCIÓN: Usar una paleta de colores dinámicamente generada para todas las décadas
      num_decadas <- length(unique(df_v_combined$decade))
      # Si hay más de 8 décadas, usar una paleta de viridis, sino una de RColorBrewer
      if (num_decadas > 8) {
        color_palette <- viridisLite::viridis(num_decadas)
      } else {
        color_palette <- brewer.pal(max(3, num_decadas), "Paired")
      }
      
      ggplot(df_v_combined, aes(x = month_index, y = v_value, color = decade, group = decade)) +
        geom_line() + geom_point(alpha = 0.7) +
        labs(title = paste("Modo Temporal (V) ", modo_v_seleccionado, " a través de las décadas"),
             x = "Índice de Tiempo (dentro de cada década)",
             y = "Valor del Modo V") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_manual(values = setNames(color_palette, sort(unique(df_v_combined$decade))))
      
    } else if (input$matriz_seleccionada == "D") {
      df_d_combined <- data.frame(decade = character(), mode_id = numeric(), singular_value = numeric())
      for (d in decadas_svd_avail) {
        zona_dir_name_all <- "Todas"
        ruta_svd_completo_d <- file.path(MATRICES_DIR, zona_dir_name_all, paste0("SVD_", d, "s_", zona_dir_name_all, ".rds"))
        if (file.exists(ruta_svd_completo_d)) {
          svd_data_decade <- readRDS(ruta_svd_completo_d)
          if (!is.null(svd_data_decade$d)) {
            df_d_decade <- data.frame(decade = paste0(d, "s"), mode_id = 1:length(svd_data_decade$d), singular_value = svd_data_decade$d)
            df_d_combined <- bind_rows(df_d_combined, df_d_decade)
          }
        }
      }
      if (nrow(df_d_combined) == 0) { return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No hay datos de Valores Singulares (D) para mostrar.", size = 5)) }
      
      num_decadas <- length(unique(df_d_combined$decade))
      if (num_decadas > 8) {
        color_palette <- viridisLite::viridis(num_decadas)
      } else {
        color_palette <- brewer.pal(max(3, num_decadas), "Spectral")
      }
      
      ggplot(df_d_combined, aes(x = mode_id, y = singular_value, color = decade, group = decade)) +
        geom_line() + geom_point(alpha = 0.7) +
        scale_y_log10() +
        labs(title = "Evolución de Valores Singulares (D) por Década",
             x = "Modo", y = "Valor Singular (Log10)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_manual(values = setNames(color_palette, sort(unique(df_d_combined$decade))))
    } else {
      ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Selecciona 'V' o 'D' para ver el análisis.", size = 5)
    }
  })
  
  # Lógica de punto específico (U, V, D)
  output$indice_input <- renderUI({
    svd_data <- svd_objeto_reactivo()
    req(svd_data)
    matriz_sel <- input$matriz_seleccionada
    max_index <- switch(matriz_sel,
                        "U" = nrow(svd_data$u),
                        "V" = nrow(svd_data$v),
                        "D" = length(svd_data$d))
    if (is.null(max_index) || max_index == 0) { return(p("No hay datos disponibles para la matriz seleccionada.")) }
    default_val <- min(1, max_index)
    sliderInput("indice_punto", label = paste("Índice (1 a", max_index, "):"), min = 1, max = max_index, value = default_val, step = 1)
  })
  
  output$valor_punto_seleccionado <- renderText({
    svd_data <- svd_objeto_reactivo()
    req(svd_data, input$indice_punto, input$modo)
    matriz_sel <- input$matriz_seleccionada
    indice <- input$indice_punto
    modo <- as.numeric(input$modo)
    valor <- NULL
    if (matriz_sel %in% c("U", "V")) {
      if (modo > length(svd_data$d)) { return("Modo fuera de rango.") }
    }
    if (matriz_sel == "U") {
      if (indice <= nrow(svd_data$u) && modo <= ncol(svd_data$u)) { valor <- svd_data$u[indice, modo] } else { return("Índice o Modo fuera de rango para U.") }
    } else if (matriz_sel == "V") {
      if (indice <= nrow(svd_data$v) && modo <= ncol(svd_data$v)) { valor <- svd_data$v[indice, modo] } else { return("Índice o Modo fuera de rango para V.") }
    } else if (matriz_sel == "D") {
      if (indice <= length(svd_data$d)) { valor <- svd_data$d[indice] } else { return("Índice fuera de rango para D.") }
    }
    if (!is.null(valor) && is.finite(valor)) { return(paste0(round(valor, 6))) } else { return("N/A") }
  })
}

shinyApp(ui = ui, server = server)
