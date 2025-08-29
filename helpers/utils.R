# Helper function to optimize shapefile size
optimize_shapefile <- function(shapefile_path, tolerance = 100, precision = 6) {
  # Read shapefile
  sf_obj <- st_read(shapefile_path)
  
  # 1. Simplify geometry
  sf_obj <- st_simplify(sf_obj, dTolerance = tolerance)
  
  # 2. Reduce coordinate precision
  sf_obj <- st_set_precision(sf_obj, precision)
  
  # 3. Remove unnecessary columns (keep only essential ones)
  # This will be done in the main code where we know which columns are needed
  
  # 4. Convert to appropriate CRS for web mapping
  if(st_crs(sf_obj)$epsg != 4326) {
    sf_obj <- st_transform(sf_obj, crs = 4326)
  }
  
  return(sf_obj)
}

# Function to convert shapefile to optimized GeoJSON
convert_to_geojson <- function(shapefile_path, output_path = NULL, tolerance = 100) {
  # Read and optimize shapefile
  sf_obj <- optimize_shapefile(shapefile_path, tolerance = tolerance)
  
  # Convert to GeoJSON
  if(is.null(output_path)) {
    output_path <- gsub("\\.shp$", "_optimized.geojson", shapefile_path)
  }
  
  # Write optimized GeoJSON
  st_write(sf_obj, output_path, driver = "GeoJSON", delete_dsn = TRUE)
  
  return(sf_obj)
}

# Function to fix problematic column names
fix_column_names <- function(df) {
  # Create a mapping of old names to new names
  name_mapping <- c(
    # Age columns starting with numbers
    "0to20" = "Age0to20",
    "21to40" = "Age21to40", 
    "41to60" = "Age41to60",
    "61to80" = "Age61to80",
    "Above80" = "AgeAbove80",
    
    # Medication count columns with underscores
    "UniqueMed_Count_5" = "UniqueMed_Count5",
    "UniqueMed_Count_10" = "UniqueMed_Count10",
    
    # Any other problematic names can be added here
    "X0to20" = "Age0to20",
    "X21to40" = "Age21to40",
    "X41to60" = "Age41to60", 
    "X61to80" = "Age61to80"
  )
  
  # Apply the mapping
  for (old_name in names(name_mapping)) {
    if (old_name %in% colnames(df)) {
      colnames(df)[colnames(df) == old_name] <- name_mapping[old_name]
    }
  }
  
  return(df)
}

# Color scales
colorscale <- c("#402580", "#38309F", "#3C50BF", "#4980DF", "#56B7FF", "#6ADDFF", "#7FFCFF", "#95FFF5", "#ABFFE8", "#C2FFE3", "#DAFFE6", "#f2fff6")
colorscale_inverted <- rev(colorscale)

# Define the regions
values_haaglanden <- c("s-Gravenhage", "Delft", "Leidschendam-Voorburg", "Midden-Delfland", 
                       "Pijnacker-Nootdorp", "Rijswijk", "Wassenaar", "Westland", "Zoetermeer")

values_roaz <- c("s-Gravenhage", "Alphen aan den Rijn", "Bodegraven-Reeuwijk", "Delft", "Gouda", 
                 "Hillegom", "Kaag en Braassem", "Katwijk", "Krimpenerwaard", "Leiden", "Leiderdorp", 
                 "Leidschendam-Voorburg", "Lisse", "Midden-Delfland", "Nieuwkoop", "Noordwijk", 
                 "Oegstgeest", "Pijnacker-Nootdorp", "Rijswijk", "Teylingen", "Voorschoten", 
                 "Waddinxveen", "Wassenaar", "Westland", "Zoetermeer", "Zoeterwoude")

values_all_regions <- c(values_haaglanden, values_roaz)

Delft_region <- c('Westland', 'Delft', 'Pijnacker-Nootdorp', 'Midden-Delfland')
Denhaag_region <- c('Leidschendam-Voorburg', "s-Gravenhage", 'Wassenaar', 'Rijswijk')
Leiden_region <- c('Alphen aan den Rijn', 'Leiden', 'Hillegom', 'Lisse', 'Noordwijk', 'Oegstgeest', 'Katwijk', 'Kaag en Braassem', 'Nieuwkoop', 'Teylingen', 'Leiderdorp', 'Voorschoten', 'Zoeterwoude')
ELAN_region <- c("s-Gravenhage", "Leidschendam-Voorburg", "Rijswijk", "Wassenaar", 
                 "Alphen aan den Rijn", "Hillegom", "Kaag en Braassem", "Katwijk", 
                 "Leiden","Leiderdorp", "Lisse", "Nieuwkoop","Noordwijk","Oegstgeest",
                 "Teylingen", "Voorschoten", "Zoeterwoude", "Delft", "Midden-Delfland", 
                 "Pijnacker-Nootdorp", "Westland", "Zoetermeer", "Waddinxveen", "Bodegraven-Reeuwijk"
)

Hadoks_region <- c("s-Gravenhage", "Leidschendam-Voorburg", "Rijswijk", "Wassenaar")

# Define special regions
special_regions <- list("Hadoks' area" = Hadoks_region, 
                        "Delft en omstreken" = Delft_region, 
                        "Leiden en omstreken" = Leiden_region, 
                        "s-gravenhage en omstreken" = Denhaag_region, 
                        "ELAN area" = ELAN_region)

area_dict <- list("s-gravenhage"="s-Gravenhage",
                  'Leiden'='Leiden',
                  "Lisse"="Lisse",
                  'Leidschendam-Voorburg'='Leidschendam-Voorburg',
                  'Wassenaar'='Wassenaar',
                  'Zoetermeer'='Zoetermeer',
                  "s-gravenhage en omstreken"="s-gravenhage en omstreken",
                  "Leiden en omstreken"='Leiden en omstreken',
                  'Delft en omstreken'='Delft en omstreken',
                  'ELAN area'='ELAN area',
                  "Hadoks' area"="Hadoks' area"
)

# Helper function for dictionary processing
remove_prefix <- function(lst) {
  names(lst) <- sub("^%_", "", names(lst))
  return(lst)
}

# Define method to translate dictionary
method_trans_dict <- function(dict_var, trans_label) {
  return(names(dict_var)[sapply(dict_var, function(x) x == trans_label)])
}

# Function to safely create variable dictionary
create_var_dict <- function(var_list, label_dict, category_name) {
  if(length(var_list) == 0) return(NULL)
  
  # Get labels for variables that exist in the label dictionary
  valid_vars <- var_list[var_list %in% names(label_dict)]
  if(length(valid_vars) == 0) return(NULL)
  
  # Create named list
  result <- setNames(valid_vars, unlist(label_dict[valid_vars]))
  
  # Remove any entries with NA or empty values
  result <- result[!is.na(result) & result != ""]
  
  if(length(result) > 0) {
    return(setNames(list(result), category_name))
  } else {
    return(NULL)
  }
}

# Custom theme functions for consistent styling across all charts
custom_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      text = element_text(family = "sans", size = 16, color = "grey20"),
      axis.text = element_text(family = "sans", size = 16),
      axis.title = element_text(family = "sans", size = 18, face = "bold"),
      plot.title = element_text(family = "sans", size = 24, face = "bold"),
      legend.text = element_text(family = "sans", size = 18),
      legend.title = element_text(family = "sans", size = 18, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title.position = 'plot'
    )
}

# Custom theme specifically for line charts with larger fonts
line_chart_theme <- function() {
  theme_minimal(base_size = 18) +
    theme(
      text = element_text(family = "sans", size = 24, color = "grey20"),
      axis.text = element_text(family = "sans", size = 12),
      axis.title = element_text(family = "sans", size = 14, face = "bold"),
      plot.title = element_text(family = "sans", size = 32, face = "bold"),
      legend.text = element_text(family = "sans", size = 26),
      legend.title = element_text(family = "sans", size = 26, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title.position = 'plot'
    )
} 