# === som_temperature_analysis.R ===
# Realiza el análisis SOM sobre los datos de temperatura y guarda los resultados.

# === 1. Librerías Necesarias ===
library(ncdf4)    
library(kohonen)  
library(dplyr)    
library(zoo)     

# === 2. Rutas de Archivos ===
NC_FILE_PATH_1 <- ("..","Data","Land_and_Ocean_Alternate_EqualArea_Cleaned.nc")
OUTPUT_SOM_DIR <- ("..","Results","som_classification")
OUTPUT_SOM_FILE_RDS <- file.path(OUTPUT_SOM_DIR, "som_temp_clusters.rds")

# === 3. Función para Cargar Datos de Temperatura desde un NetCDF Limpio ===
load_nc_data <- function(file_path) {
  if (file.exists(file_path)) {
    nc_data <- nc_open(file_path)
    lon <- ncvar_get(nc_data, "longitude_coords")
    lat <- ncvar_get(nc_data, "latitude_coords")
    temp <- ncvar_get(nc_data, "temperature")
    nc_close(nc_data)
    message(paste("Datos de", basename(file_path), "cargados. Dimensiones:", paste(dim(temp), collapse = "x")))
    return(list(lon = lon, lat = lat, temp = temp))
  } else {
    stop(paste("ERROR: El archivo NetCDF no se encuentra en la ruta:", file_path))
  }
}

# === 4. Cargar el Archivo NetCDF Limpio (¡Líneas Faltantes Añadidas Aquí!) ===
message("Cargando el archivo NetCDF limpio para el análisis SOM...")
data_1_loaded <- load_nc_data(NC_FILE_PATH_1) # Llama a la función para cargar
lon_global_raw <- data_1_loaded$lon
lat_global_raw <- data_1_loaded$lat
tem_global_all_time_raw_1 <- data_1_loaded$temp

message("Datos del NetCDF limpio cargados y listos.")


# === 5. Preparar la Matriz de Datos para el SOM ===
message("Preparando la matriz de datos para el SOM...")
data_for_som_input <- t(apply(tem_global_all_time_raw_1, 1, function(x) {
  if (all(is.na(x))) {
    return(x) 
  } else {
  
    x_filled_ends <- zoo::na.locf(x, na.rm = FALSE)
    x_filled_ends <- zoo::na.locf(x_filled_ends, fromLast = TRUE, na.rm = FALSE)
    
    return(zoo::na.approx(x_filled_ends, na.rm = FALSE, rule = 2)) 
  }
}))

valid_points_mask <- apply(data_for_som_input, 1, function(x) !any(is.na(x)))
data_for_som_input_clean <- data_for_som_input[valid_points_mask, ]


lon_som_filtered <- lon_global_raw[valid_points_mask]
lat_som_filtered <- lat_global_raw[valid_points_mask]

message(paste("Datos para SOM preparados. Puntos válidos después de filtrado:", nrow(data_for_som_input_clean)))

# === 6. Escalar los Datos ===
message("Escalando datos para el SOM...")
data_for_som_scaled <- scale(data_for_som_input_clean)

# === 7. Entrenar el Modelo SOM ===
message("Entrenando el modelo SOM. Esto puede tomar un tiempo...")
set.seed(123) 
som_grid_temp <- somgrid(xdim = 10, ydim = 10, topo = "rectangular") 
som_model_temp <- kohonen::som(data_for_som_scaled,
                               grid = som_grid_temp,
                               rlen = 1000,           
                               alpha = c(0.05, 0.01), 
                               keep.data = TRUE)      

message("Entrenamiento SOM completado.")

# === 8. Extraer las Asignaciones de Clúster ===
som_group_assignments_temp <- som_model_temp$unit.classif

# === 9. Crear el Data Frame de Resultados SOM ===
som_temp_results_df <- data.frame(
  lon = lon_som_filtered,
  lat = lat_som_filtered,
  som_group_id = som_group_assignments_temp
)

message(paste("Data frame de resultados SOM creado. Dimensiones:", paste(dim(som_temp_results_df), collapse = "x")))
print(head(som_temp_results_df)) 

# === 10. Guardar los Resultados del SOM (¡Bloque Unificado y Corregido!) ===
message(paste("Guardando resultados SOM en:", OUTPUT_SOM_FILE_RDS))
dir.create(OUTPUT_SOM_DIR, recursive = TRUE, showWarnings = FALSE) 

saveRDS(som_temp_results_df, file = OUTPUT_SOM_FILE_RDS)

if (file.exists(OUTPUT_SOM_FILE_RDS)) {
  message("ÉXITO: El archivo SOM de clústeres de temperatura fue creado y guardado.")
} else {
  message("ADVERTENCIA: El archivo SOM NO se encontró después de guardar. Revisa si hubo errores o permisos.")
}

message("\n--- Proceso de análisis SOM completado ---")
message(paste0("El archivo de resultados SOM está en: ", OUTPUT_SOM_FILE_RDS))
