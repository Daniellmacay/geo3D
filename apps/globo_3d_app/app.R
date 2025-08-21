# === 1. Librerías ===
library(shiny)
library(threejs)
library(dplyr)
library(RColorBrewer)
library(sf) 
library(ggrepel) 

# === 2. Configuración de Rutas y Carga de Datos (compartida) ===
MATRICES_DIR <- "C:/Mi_proyecto/climate-analist/Results/matrices_csv"
SOM_FILE_PATH <- "C:/Mi_proyecto/climate-analist/Results/som_classification/som_temp_clusters.rds"
IPCC_REGIONS_FILE <- "C:/Mi_proyecto/climate-analist/Data/Atlas/IPCC-WGI-reference-regions-v4.geojson"
COORDS_FILE_PATH <- "C:/Mi_proyecto/climate-analist/Data/global_coords.rds"

# Cargar coordenadas desde el archivo .rds para reducir la memoria
if (file.exists(COORDS_FILE_PATH)) {
  global_coords_df <- readRDS(COORDS_FILE_PATH)
  lon_global <- global_coords_df$lon
  lat_global <- global_coords_df$lat
  valid_rows_initial <- global_coords_df$original_idx
} else {
  stop("ERROR: Archivo de coordenadas 'global_coords.rds' no encontrado. Por favor, asegúrate de que exista y esté preprocesado.")
}

# Cargar resultados SOM y corregir la ausencia de original_idx
som_results_df <- NULL
if (file.exists(SOM_FILE_PATH)) {
  som_results_df <- readRDS(SOM_FILE_PATH)
  som_results_df$original_idx <- 1:nrow(som_results_df)
  
} else {
  stop("ERROR: Archivo de resultados SOM 'som_temp_clusters.rds' no encontrado.")
}

# Cargar regiones IPCC y asignar puntos a ellas
ipcc_regions_sf <- NULL
if (file.exists(IPCC_REGIONS_FILE)) {
  ipcc_regions_sf <- tryCatch({
    st_read(IPCC_REGIONS_FILE, quiet = TRUE) %>% st_transform(4326) 
  }, error = function(e) { message("Error loading IPCC GeoJSON: ", e$message); NULL })
}
points_ipcc_assignment <- data.frame(lon = lon_global, lat = lat_global, original_idx = 1:length(lon_global), ipcc_region_name = NA_character_)
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
} else { points_ipcc_assignment$ipcc_region_name <- "No Definido" }

# Mapear grupos SOM a nombres de regiones IPCC y generar paleta de colores fija
som_group_ipcc_map <- data.frame(som_group_id = integer(), ipcc_region_name = character(), display_name = character())
som_color_palette <- character() 
if (!is.null(som_results_df) && nrow(som_results_df) > 0) {
  som_results_with_ipcc <- som_results_df %>% left_join(points_ipcc_assignment %>% select(original_idx, ipcc_region_name), by = "original_idx")
  som_group_ipcc_summary <- som_results_with_ipcc %>%
    filter(!is.na(som_group_id), !is.na(ipcc_region_name), ipcc_region_name != "No Definido") %>%
    group_by(som_group_id, ipcc_region_name) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(som_group_id) %>% arrange(desc(count)) %>% slice(1) %>% ungroup() %>%
    mutate(display_name = paste0("Grupo ", som_group_id, " (", ipcc_region_name, ")"))
  all_som_groups_from_df <- sort(unique(som_results_df$som_group_id))
  for (grp in all_som_groups_from_df) {
    if (!grp %in% som_group_ipcc_summary$som_group_id) {
      som_group_ipcc_summary <- bind_rows(som_group_ipcc_summary, data.frame(som_group_id = grp, ipcc_region_name = "No Definido", display_name = paste0("Grupo ", grp, " (No Definido)"), count = 0))
    }
  }
  som_group_ipcc_map <- som_group_ipcc_summary %>% arrange(som_group_id)
  num_clusters <- length(all_som_groups_from_df)
  if (num_clusters > 0) {
    simple_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "gold", "cyan", "magenta")
    actual_colors_to_use <- rep(simple_colors, length.out = num_clusters)
    som_color_palette <- setNames(actual_colors_to_use, as.character(all_som_groups_from_df))
  }
}

# Almacenamiento de resultados SVD precargados para el globo (Modo U)
decadas_svd_avail <- c()
all_svd_dir <- file.path(MATRICES_DIR, "Todas")
if (dir.exists(all_svd_dir)) {
  svd_files <- list.files(all_svd_dir, pattern = "SVD_\\d{4}s_Todas\\.rds", full.names = FALSE)
  if (length(svd_files) > 0) { 
    decadas_svd_avail <- sort(as.numeric(gsub("SVD_(\\d{4})s_Todas\\.rds", "\\1", svd_files)))
  }
}
resultados_svd_globe <- list()
if(length(decadas_svd_avail) > 0){
  for (d in decadas_svd_avail) {
    ruta_svd_rds <- file.path(MATRICES_DIR, "Todas", paste0("SVD_", d, "s_Todas.rds"))
    if (file.exists(ruta_svd_rds)) {
      svd_result_zona <- readRDS(ruta_svd_rds)
      if (!is.null(svd_result_zona$u) && nrow(svd_result_zona$u) == length(lat_global)) {
        num_modos <- min(50, ncol(svd_result_zona$u))
        for (modo in 1:num_modos) {
          nombre_clave <- paste0(d, "s - Modo ", modo, " - Todas")
          resultados_svd_globe[[nombre_clave]] <- list(values = svd_result_zona$u[, modo], lat = lat_global, lon = lon_global)
        }
      }
    }
  }
}

color_por_cuantiles <- function(x, num_breaks = 10) {
  if (all(is.na(x)) || all(!is.finite(x))) { return(rep(NA_character_, length(x))) }
  x_finite <- x[is.finite(x)]
  if (length(x_finite) == 0) { return(rep(NA_character_, length(x))) }
  x_pos <- x_finite[x_finite > 0]; x_neg <- x_finite[x_finite < 0]
  final_colors <- rep(NA_character_, length(x))
  if (length(x_pos) > 0) {
    breaks_pos <- unique(quantile(x_pos, probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE, type = 7))
    groups_pos <- cut(x_pos, breaks = breaks_pos, include.lowest = TRUE, labels = FALSE, right = TRUE)
    red_palette <- colorRampPalette(c("white", "red4"))(max(1, num_breaks))
    final_colors[x > 0 & is.finite(x)] <- red_palette[groups_pos]
  }
  if (length(x_neg) > 0) {
    breaks_neg <- unique(quantile(abs(x_neg), probs = seq(0, 1, length.out = num_breaks + 1), na.rm = TRUE, type = 7))
    groups_neg <- cut(abs(x_neg), breaks = breaks_neg, include.lowest = TRUE, labels = FALSE, right = TRUE)
    blue_palette <- colorRampPalette(c("white", "blue4"))(max(1, num_breaks))
    final_colors[x < 0 & is.finite(x)] <- blue_palette[groups_neg]
  }
  return(final_colors)
}

# === UI de Shiny ===
ui <- fluidPage(
  titlePanel("Modelo 3D de Modos Climáticos y Análisis SOM"),
  sidebarLayout(
    sidebarPanel(
      selectInput("color_by", "Colorear globo por:", choices = c("Modo SVD" = "svd", "Grupo SOM (Temperatura)" = "som"), selected = "svd"),
      uiOutput("globe_display_options"),
      width = 3
    ),
    mainPanel(
      globeOutput("globo", height = "600px")
    )
  )
)

# === Server de Shiny ===
server <- function(input, output, session) {
  som_data_reactive <- reactive({ req(som_results_df); som_results_df })
  
  output$globe_display_options <- renderUI({
    if (input$color_by == "svd") {
      tagList(
        selectInput("decada", "Década:", choices = as.character(decadas_svd_avail), selected = if(length(decadas_svd_avail) > 0) as.character(max(decadas_svd_avail)) else NULL),
        selectInput("modo", "Modo (1-50):", choices = as.character(1:50), selected = "1"),
        uiOutput("umbral_slider")
      )
    } else if (input$color_by == "som") {
      som_data_local <- som_data_reactive()
      req(som_data_local)
      som_cluster_filter_choices <- c("Todas" = "all", setNames(as.character(som_group_ipcc_map$som_group_id), som_group_ipcc_map$display_name))
      tagList(
        selectInput("som_cluster_filter", "Grupo SOM (para resaltar en Globo):", choices = som_cluster_filter_choices, selected = "all")
      )
    }
  })
  
  modo_u_actual <- reactive({
    req(input$color_by == "svd", input$decada, input$modo)
    nombre_clave_seleccionada <- paste0(input$decada, "s - Modo ", input$modo, " - Todas")
    if (nombre_clave_seleccionada %in% names(resultados_svd_globe)) {
      resultados_svd_globe[[nombre_clave_seleccionada]]$values
    } else { NULL }
  })
  
  output$umbral_slider <- renderUI({
    req(input$color_by == "svd")
    current_mode_values <- modo_u_actual()
    req(current_mode_values)
    finite_values <- current_mode_values[is.finite(current_mode_values)]
    if(length(finite_values) == 0) { return(p("No hay valores finitos para el umbral.")) }
    max_abs_val <- max(abs(finite_values), na.rm = TRUE)
    slider_max <- max_abs_val * 0.25
    if (slider_max < 0.000001) slider_max <- 0.000001
    slider_step <- slider_max / 100
    default_value <- slider_max * 0.01
    sliderInput("umbral", "Eliminar ruido visual:", min = 0, max = slider_max, value = default_value, step = slider_step)
  })
  
  output$globo <- renderGlobe({
    lat_plot <- NULL; lon_plot <- NULL; colors_for_plot <- NULL; point_values <- NULL
    
    if (input$color_by == "som") {
      som_data_local <- som_data_reactive()
      req(som_data_local)
      som_data_for_globe <- som_data_local %>% left_join(points_ipcc_assignment %>% select(original_idx, ipcc_region_name), by = "original_idx")
      
      # Asegurar que lat y lon se obtienen de som_data_for_globe
      lat_plot <- som_data_for_globe$lat
      lon_plot <- som_data_for_globe$lon
      som_group_id_plot <- som_data_for_globe$som_group_id
      
      # Asignación de colores
      colors_for_plot <- som_color_palette[as.character(som_group_id_plot)]
      
      if (!is.null(input$som_cluster_filter) && input$som_cluster_filter != "all") {
        selected_cluster_id <- as.numeric(input$som_cluster_filter)
        filter_mask <- (som_group_id_plot == selected_cluster_id)
        lat_plot <- lat_plot[filter_mask]; lon_plot <- lon_plot[filter_mask]; colors_for_plot <- colors_for_plot[filter_mask];
        if (length(lat_plot) == 0) { return(NULL) }
      }
      point_values <- rep(1, length(lat_plot))
    } else { # input$color_by == "svd"
      req(input$decada, input$modo, input$umbral)
      nombre_clave_seleccionada <- paste0(input$decada, "s - Modo ", input$modo, " - Todas")
      if (!nombre_clave_seleccionada %in% names(resultados_svd_globe)) { return(NULL) }
      sel <- resultados_svd_globe[[nombre_clave_seleccionada]]
      valores_del_modo <- sel$values
      umbral_val <- input$umbral
      if (is.null(valores_del_modo) || length(valores_del_modo) == 0 || all(!is.finite(valores_del_modo))) { return(NULL) }
      if (!is.finite(umbral_val)) umbral_val <- 0
      keep_indices <- which(abs(valores_del_modo) >= umbral_val & is.finite(valores_del_modo))
      if (length(keep_indices) == 0) { return(NULL) }
      valores_a_colorear <- valores_del_modo[keep_indices]
      colors_for_plot <- color_por_cuantiles(valores_a_colorear)
      lat_plot <- sel$lat[keep_indices]
      lon_plot <- sel$lon[keep_indices]
      point_values <- rep(1, length(lat_plot))
    }
    valid_data_points <- !is.na(lat_plot) & !is.na(lon_plot) & !is.na(colors_for_plot)
    if (sum(valid_data_points) == 0) { return(globejs(atmosphere = TRUE, bg = "#000000")) }
    
    globe_obj <- globejs(
      lat = lat_plot[valid_data_points],
      long = lon_plot[valid_data_points],
      value = point_values[valid_data_points],
      color = colors_for_plot[valid_data_points],
      atmosphere = TRUE,
      pointsize = 2,
      bg = "#000000",
      texture = "https://raw.githubusercontent.com/johan/world.geo.json/master/countries/110m/globe.jpg"
    )
    return(globe_obj)
  })
  
}

shinyApp(ui = ui, server = server)
