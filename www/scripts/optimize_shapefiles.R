# Script to optimize shapefiles for Shiny app
# Run this script once to create optimized versions of your shapefiles

library(sf)
library(dplyr)

# Set your data path
path <- 'data/'

# Function to optimize shapefile
optimize_shapefile <- function(input_path, output_path, tolerance = 100, precision = 6) {
  cat("Processing:", input_path, "\n")
  
  # Read shapefile
  sf_obj <- st_read(input_path)
  cat("Original size:", object.size(sf_obj), "bytes\n")
  
  # 1. Simplify geometry
  sf_obj <- st_simplify(sf_obj, dTolerance = tolerance)
  
  # 2. Reduce coordinate precision
  sf_obj <- st_set_precision(sf_obj, precision)
  
  # 3. Convert to appropriate CRS for web mapping
  if(st_crs(sf_obj)$epsg != 4326) {
    sf_obj <- st_transform(sf_obj, crs = 4326)
  }
  
  # 4. Write optimized shapefile
  st_write(sf_obj, output_path, driver = "ESRI Shapefile", delete_dsn = TRUE)
  
  cat("Optimized size:", object.size(sf_obj), "bytes\n")
  cat("Size reduction:", round((1 - object.size(sf_obj)/object.size(st_read(input_path))) * 100, 1), "%\n")
  
  return(sf_obj)
}

# Function to convert to GeoJSON (even smaller)
convert_to_geojson <- function(input_path, output_path, tolerance = 100) {
  cat("Converting to GeoJSON:", input_path, "\n")
  
  # Read and optimize
  sf_obj <- st_read(input_path)
  sf_obj <- st_simplify(sf_obj, dTolerance = tolerance)
  
  if(st_crs(sf_obj)$epsg != 4326) {
    sf_obj <- st_transform(sf_obj, crs = 4326)
  }
  
  # Write GeoJSON
  st_write(sf_obj, output_path, driver = "GeoJSON", delete_dsn = TRUE)
  
  cat("GeoJSON created:", output_path, "\n")
  return(sf_obj)
}

# Optimize your shapefiles
cat("=== Optimizing Shapefiles ===\n")

# Optimize gemeenten shapefile
gemeenten_optimized <- optimize_shapefile(
  input_path = paste0(path, 'gemeenten_2023_v1.shp'),
  output_path = paste0(path, 'gemeenten_2023_v1_optimized.shp'),
  tolerance = 100  # Adjust based on your needs
)

# Optimize wijken shapefile
wijken_optimized <- optimize_shapefile(
  input_path = paste0(path, 'wijk_2023_v0.shp'),
  output_path = paste0(path, 'wijk_2023_v0_optimized.shp'),
  tolerance = 50  # Smaller tolerance for neighborhoods
)

# Convert to GeoJSON (optional - even smaller)
cat("\n=== Converting to GeoJSON ===\n")
gemeenten_geojson <- convert_to_geojson(
  input_path = paste0(path, 'gemeenten_2023_v1.shp'),
  output_path = paste0(path, 'gemeenten_2023_v1.geojson'),
  tolerance = 100
)

wijken_geojson <- convert_to_geojson(
  input_path = paste0(path, 'wijk_2023_v0.shp'),
  output_path = paste0(path, 'wijk_2023_v0.geojson'),
  tolerance = 50
)

cat("\n=== Optimization Complete ===\n")
cat("You can now use the optimized files in your Shiny app:\n")
cat("- gemeenten_2023_v1_optimized.shp\n")
cat("- wijk_2023_v0_optimized.shp\n")
cat("- gemeenten_2023_v1.geojson\n")
cat("- wijk_2023_v0.geojson\n") 