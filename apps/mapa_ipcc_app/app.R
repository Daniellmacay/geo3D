# === 1. Librerías ===
library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(ggrepel)
library(leaflet)
library(shinyWidgets)
library(grDevices)
library(RColorBrewer) # Nueva librería para generar paletas de colores únicas

# === 2. Configuración de Rutas ===
SOM_FILE_PATH <- ("..","Results","som_classification","som_temp_clusters.rds")
IPCC_REGIONS_FILE <- ("..","Data","IPCC-WGI-reference-regions-v4.geojson")
POINTS_IPCC_ASSIGNMENT_PATH <- ("..","Results","point_ipcc_assignment.csv")
TEMP_DATA_FILE_PATH <- ("..","Data","temp_data_for_som.rds")

# === 3. Cargar Datos y Modelos ===
if (!file.exists(IPCC_REGIONS_FILE)) {
  stop("ERROR: El archivo GeoJSON de regiones IPCC no se encontró en la ruta especificada. Revise la ruta.")
}
if (!file.exists(SOM_FILE_PATH)) {
  stop("ERROR: El archivo .rds de resultados SOM no se encontró. Revise la ruta.")
}
if (!file.exists(TEMP_DATA_FILE_PATH)) {
  stop("ERROR: El archivo de datos de temperatura 'temp_data_for_som.rds' no se encontró. Por favor, ejecute el script de pre-procesamiento del paso 1.")
}

temp_data_from_rds <- readRDS(TEMP_DATA_FILE_PATH)
temp_data_full <- temp_data_from_rds$temp_data
time_var_raw <- temp_data_from_rds$time_var

reference_date_start <- as.Date("1850-01-01")
dates_global <- seq(from = reference_date_start, by = "month", length.out = length(time_var_raw))

som_results_df <- readRDS(SOM_FILE_PATH)
if (is.null(som_results_df) || nrow(som_results_df) == 0) {
  stop("ERROR: Los datos SOM son NULL o están vacíos.")
}
som_results_df$original_idx <- 1:nrow(som_results_df)

ipcc_regions_sf <- tryCatch({
  st_read(IPCC_REGIONS_FILE, quiet = TRUE) %>%
    st_transform(4326)
}, error = function(e) {
  message("Error al cargar o transformar el archivo GeoJSON de regiones IPCC: ", e$message)
  NULL
})

if (!file.exists(POINTS_IPCC_ASSIGNMENT_PATH)) {
  points_ipcc_assignment <- data.frame(
    lon = som_results_df$lon,
    lat = som_results_df$lat,
    original_idx = som_results_df$original_idx,
    ipcc_region_name = NA_character_
  )
  if (!is.null(ipcc_regions_sf) && nrow(ipcc_regions_sf) > 0) {
    global_points_sf <- st_as_sf(points_ipcc_assignment, coords = c("lon", "lat"), crs = 4326)
    intersections <- st_intersects(global_points_sf, ipcc_regions_sf)
    for (i in 1:length(intersections)) {
      if (length(intersections[[i]]) > 0) {
        points_ipcc_assignment$ipcc_region_name[i] <- ipcc_regions_sf$Name[intersections[[i]][1]]
      } else {
        points_ipcc_assignment$ipcc_region_name[i] <- "No Definido"
      }
    }
  } else {
    points_ipcc_assignment$ipcc_region_name <- "No Definido"
  }
  write.csv(points_ipcc_assignment, POINTS_IPCC_ASSIGNMENT_PATH, row.names = FALSE)
} else {
  points_ipcc_assignment <- read.csv(POINTS_IPCC_ASSIGNMENT_PATH)
}

som_data_full_app <- som_results_df %>%
  left_join(points_ipcc_assignment %>% select(original_idx, ipcc_region_name), by = "original_idx")

som_group_ipcc_summary <- som_data_full_app %>%
  filter(!is.na(som_group_id), !is.na(ipcc_region_name), ipcc_region_name != "No Definido") %>%
  group_by(som_group_id, ipcc_region_name) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(som_group_id) %>%
  arrange(desc(count)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(display_name = paste0("Grupo ", som_group_id, " (", ipcc_region_name, ")"))

all_som_groups <- unique(som_results_df$som_group_id) %>% sort()
som_group_ipcc_map <- som_group_ipcc_summary %>% arrange(som_group_id)

# === CORRECCIÓN EN LA PALETA DE COLORES ===
num_clusters <- length(all_som_groups)
if (num_clusters > 0) {
 
  full_palette <- colorRampPalette(brewer.pal(11, "Spectral"))(num_clusters)
  som_color_palette <- setNames(full_palette, as.character(all_som_groups)) 
} else {
  som_color_palette <- setNames(character(), character()) 
}

# === 4. Interfaz de Usuario (UI) ===
ui <- fluidPage(
  titlePanel("Análisis Regional de Clusters SOM y Regiones IPCC"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("som_ts_group_selector"),
      uiOutput("ipcc_region_selector"),
      tags$hr(),
      uiOutput("ipcc_map_options"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  tabPanel("Serie de Tiempo Grupo SOM",
                           plotOutput("som_group_timeseries_plot", height = "500px")),
                  tabPanel("Mapa 2D de Regiones IPCC",
                           tags$p("Este mapa muestra las regiones de referencia del IPCC con interactividad de zoom."),
                           leafletOutput("ipcc_regions_map_leaflet", height = "600px"))
      )
    )
  )
)

# === 5. Servidor (Server) ===
server <- function(input, output, session) {
  
  som_data_reactive <- reactive({
    req(som_results_df)
    som_results_df
  })
  
  output$ipcc_region_selector <- renderUI({
    req(!is.null(points_ipcc_assignment))
    ipcc_choices <- c("Todas las Regiones" = "all", sort(unique(points_ipcc_assignment$ipcc_region_name)))
    selectInput("ipcc_region_filter", "Región IPCC (para SOM TS):",
                choices = ipcc_choices, selected = "all")
  })
  
  output$som_ts_group_selector <- renderUI({
    som_data_local <- som_data_reactive()
    req(som_data_local)
    som_cluster_choices_map <- setNames(as.character(som_group_ipcc_map$som_group_id), som_group_ipcc_map$display_name)
    selectInput("som_ts_group_select", "Grupo SOM:",
                choices = som_cluster_choices_map,
                selected = if(length(som_cluster_choices_map) > 0) som_cluster_choices_map[1] else NULL)
  })
  
  output$ipcc_map_options <- renderUI({
    tagList(
      selectInput("ipcc_map_display_type", "Mostrar en el mapa:",
                  choices = c("Solo Regiones IPCC" = "regions_only",
                              "Regiones IPCC con Puntos SOM" = "regions_som_points"),
                  selected = "regions_only"),
      conditionalPanel(
        condition = "input.ipcc_map_display_type == 'regions_som_points'",
        selectInput("ipcc_map_som_filter", "Resaltar Grupo SOM (en 2D):",
                    choices = c("Todos" = "all",
                                setNames(as.character(som_group_ipcc_map$som_group_id),
                                         som_group_ipcc_map$display_name)),
                    selected = "all")
      )
    )
  })
  
  som_color_palette_proxy <- reactive({
    som_color_palette
  })
  
  output$som_group_timeseries_plot <- renderPlot({
    req(input$main_tabs == "Serie de Tiempo Grupo SOM")
    req(!is.null(som_results_df))
    req(!is.null(input$som_ts_group_select))
    req(!is.null(input$ipcc_region_filter))
    selected_som_group <- as.numeric(input$som_ts_group_select)
    selected_ipcc_region <- input$ipcc_region_filter
    combined_som_ipcc_data <- som_data_full_app %>%
      filter(som_group_id == selected_som_group)
    if (selected_ipcc_region != "all") {
      combined_som_ipcc_data <- combined_som_ipcc_data %>%
        filter(ipcc_region_name == selected_ipcc_region)
    }
    if (nrow(combined_som_ipcc_data) == 0) {
      feedback_message <- paste("No hay puntos para el Grupo SOM", selected_som_group)
      if (selected_ipcc_region != "all") {
        feedback_message <- paste0(feedback_message, " en la región IPCC: ", selected_ipcc_region)
      }
      feedback_message <- paste0(feedback_message, ".\nEsto puede significar que no hay puntos en el grupo SOM que también pertenezcan a la región IPCC seleccionada, o que no hay datos disponibles para esta combinación.")
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = feedback_message, size = 5, color = "darkgrey"))
    }
    valid_original_idx_to_extract <- combined_som_ipcc_data$original_idx[
      combined_som_ipcc_data$original_idx >= 1 & combined_som_ipcc_data$original_idx <= nrow(temp_data_full)
    ]
    if (length(valid_original_idx_to_extract) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No se encontraron datos de temperatura para los puntos seleccionados del grupo SOM y la región IPCC.", size = 5, color = "darkgrey"))
    }
    temp_series_for_group <- temp_data_full[valid_original_idx_to_extract, , drop = FALSE]
    if (nrow(temp_series_for_group) == 0 || all(is.na(temp_series_for_group))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Datos de temperatura no disponibles o solo NA para los puntos seleccionados.", size = 5, color = "darkgrey"))
    }
    avg_temp_series <- apply(temp_series_for_group, 2, mean, na.rm = TRUE)
    if (all(is.na(avg_temp_series)) || all(is.nan(avg_temp_series))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Serie de tiempo promedio vacía o con solo valores faltantes después de promediar.", size = 5, color = "darkgrey"))
    }
    if (length(dates_global) != length(avg_temp_series)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Error en la longitud de las fechas y la serie de tiempo promediada. Revise la indexación de los datos.", size = 5, color = "red"))
    }
    df_timeseries <- data.frame(
      Date = dates_global,
      Temperature = avg_temp_series
    ) %>%
      filter(!is.na(Temperature))
    if (nrow(df_timeseries) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No hay datos de temperatura válidos para este grupo después de promediar y filtrar NA.", size = 5, color = "darkgrey"))
    }
    som_group_display_name <- som_group_ipcc_map$display_name[som_group_ipcc_map$som_group_id == selected_som_group]
    if (length(som_group_display_name) == 0) {
      som_group_display_name <- paste0("Grupo ", selected_som_group, " (Nombre Desconocido)")
    }
    ggplot(df_timeseries, aes(x = Date, y = Temperature)) +
      geom_line(color = "steelblue", size = 1) +
      geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +
      labs(
        title = paste("Serie de Tiempo de Temperatura Promedio para", som_group_display_name,
                      "\n(Filtrado por Región IPCC:", selected_ipcc_region, ")"),
        x = "Fecha",
        y = "Temperatura Promedio (°C)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$ipcc_regions_map_leaflet <- renderLeaflet({
    req(!is.null(ipcc_regions_sf))
    leaflet(ipcc_regions_sf) %>%
      setView(lng = -10, lat = 0, zoom = 2) %>%
      addTiles() %>%
      addPolygons(
        fillColor = "lightblue",
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.5,
        label = ~Name,
        highlightOptions = highlightOptions(color = "red", weight = 3, bringToFront = TRUE)
      )
  })
  
  observe({
    req(input$main_tabs == "Mapa 2D de Regiones IPCC")
    req(input$ipcc_map_display_type == 'regions_som_points',
        !is.null(som_results_df),
        !is.null(points_ipcc_assignment),
        leafletProxy("ipcc_regions_map_leaflet"))
    
    leafletProxy("ipcc_regions_map_leaflet") %>%
      clearMarkers()
    
    points_to_plot <- som_data_full_app %>%
      filter(!is.na(lat) & !is.na(lon))
    
    if (!is.null(input$ipcc_map_som_filter) && input$ipcc_map_som_filter != "all") {
      selected_som_group_map <- as.numeric(input$ipcc_map_som_filter)
      points_to_plot <- points_to_plot %>% filter(som_group_id == selected_som_group_map)
    }
    
    if (nrow(points_to_plot) > 0) {
      points_to_plot$color_val <- som_color_palette_proxy()[as.character(points_to_plot$som_group_id)]
      points_to_plot$color_val[is.na(points_to_plot$color_val)] <- "#D3D3D3"
      
      leafletProxy("ipcc_regions_map_leaflet") %>%
        addCircleMarkers(data = points_to_plot,
                         lng = ~lon,
                         lat = ~lat,
                         radius = 3,
                         color = "black",
                         fillColor = ~color_val,
                         fillOpacity = 0.7,
                         stroke = TRUE,
                         weight = 1,
                         popup = ~paste0("Grupo SOM: ", som_group_id))
    }
  })
}

shinyApp(ui = ui, server = server)
