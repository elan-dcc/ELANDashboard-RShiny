# Global Dashboard Data and Functions
# This file contains all the data loading and processing for the dashboard functionality

# Load required libraries for themes
library(ggplot2)

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

# Data loading
path = 'data/'

# Read shapefile for gemeenten
geo_df_gem <- st_read(paste0(path, 'gemeenten_2023_v1.shp')) 
geo_df_gem <- geo_df_gem[geo_df_gem$H2O == 'NEE', ]
geo_df_gem <- geo_df_gem %>%
  select(GM_CODE, GM_NAAM, geometry, H2O)
geo_df_gem <- st_simplify(geo_df_gem, dTolerance = 100)
geo_df_gem <- st_transform(geo_df_gem, crs = 4326)
geo_df_gem <- geo_df_gem %>%
  rename(GMC = GM_CODE)
geo_df_gem$GMN <- gsub("'", '', geo_df_gem$GM_NAAM)
geo_df_gem <- geo_df_gem %>%
  filter(GMN %in% values_all_regions)

# Read CSV files for gemeenten
df_numeric_gem <- read.csv(paste0(path, 'df_numeric_gem_ver_6.csv'), sep = ',', encoding = 'latin1') %>%
  rename(GMN = gem_name)

df_count_gem <- read.csv(paste0(path, 'df_count_gem_ver_6.csv'), sep = ',', encoding = 'latin1') %>%
  rename(Total_Population = Total_All_Pop)

# Fix problematic column names in gemeente data
df_count_gem <- fix_column_names(df_count_gem)
df_numeric_gem <- fix_column_names(df_numeric_gem)

# Remove any remaining problematic prefixes
colnames(df_count_gem) <- gsub("^X\\._", "", colnames(df_count_gem))

# Merge dataframes for gemeenten
df_gem <- merge(df_count_gem, df_numeric_gem, by = c('GMC', 'GMN', 'YEAR'))
df_gem$YEAR <- as.integer(df_gem$YEAR)

# Change negative values to 0 for gemeenten
numeric_cols_gem <- sapply(df_gem, is.numeric)
df_gem[numeric_cols_gem] <- lapply(df_gem[numeric_cols_gem], function(x) pmax(x, 0))

# End clean-up for gemeenten
headers_gem <- colnames(df_gem)
columns_gem <- setdiff(headers_gem, c('GMC', 'GMN', 'YEAR'))
orig_columns_gem <- columns_gem

df_gem[columns_gem] <- round(df_gem[columns_gem], 4)

df_gem <- df_gem %>%
  mutate(Total_GP_Pop = Total_ICPCPat_Pop *100/ Total_Population)

df_gem <- df_gem %>%
  mutate(Total_GP_Pop = round(Total_GP_Pop, 2))

df_gem$YEAR <- as.integer(df_gem$YEAR)

# Change negative values to 0
numeric_cols <- sapply(df_gem, is.numeric)
df_gem[numeric_cols] <- lapply(df_gem[numeric_cols], function(x) pmax(x, 0))

# End clean-up
headers <- colnames(df_gem)
columns <- setdiff(headers, c('GMC', 'GMN', 'WKN', 'YEAR'))
orig_columns <- columns

df_gem[columns] <- round(df_gem[columns], 4)

# Column names are now fixed by the fix_column_names function

# Read shapefile for wijken
geo_df <- st_read(file.path(path, 'wijk_2023_v0.shp'))
geo_df <- st_simplify(geo_df, dTolerance = 50)
geo_df <- st_transform(geo_df, crs = 4326)
geo_df <- geo_df %>% rename(WKC = WK_CODE)
geo_df$GM_NAAM <- gsub("'", '', geo_df$GM_NAAM)
geo_df <- geo_df %>% filter(GM_NAAM %in% values_all_regions)

# Read and merge numeric data for wijken
df_numeric <- read.csv(file.path(path, 'df_numeric_ver_6.csv'), sep = ',', encoding = 'latin1')
df_numeric <- df_numeric %>% rename(GMN = gem_name)
df_numeric$WKN[(df_numeric$WKN == 'Buitengebied') & (df_numeric$GMN == 'Lisse') ] <- 'Buitengebied Lisse'
df_numeric$WKN[(df_numeric$WKN == 'Buitengebied') & (df_numeric$GMN == 'Hillegom') ] <- 'Buitengebied Hillegom'

df_count <- read.csv(file.path(path, 'df_count_ver_6.csv'), sep = ',', encoding = 'latin1')

# Fix problematic column names in wijken data
df_count <- fix_column_names(df_count)
df_numeric <- fix_column_names(df_numeric)

# Remove any remaining problematic prefixes
colnames(df_count) <- gsub("^X\\._", "", colnames(df_count))

df_count <- df_count %>% rename(Total_Population = Total_All_Pop)
df <- merge(df_count, df_numeric, by = c('WKC', 'WKN', 'GMN', 'YEAR'))
df$YEAR <- as.integer(df$YEAR)

# Change negative values to 0 for wijken
numeric_cols <- sapply(df, is.numeric)
df[numeric_cols] <- lapply(df[numeric_cols], function(x) pmax(x, 0))

# End clean-up for wijken
headers <- colnames(df)
columns <- setdiff(headers, c('WKC', 'GMN', 'WKN', 'YEAR'))
orig_columns <- columns

df[columns] <- round(df[columns], 4)

# Column names are now fixed by the fix_column_names function

# Helper function for dictionary processing
remove_prefix <- function(lst) {
  names(lst) <- sub("^%_", "", names(lst))
  return(lst)
}

# Read variable definitions
var_def_dict <- list()
con <- file(file.path(path, 'Variables_Definition.txt'), open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  s <- strsplit(line, ":")[[1]]
  var_def_dict[[s[1]]] <- s[2]
}
close(con)
var_def_dict <- remove_prefix(var_def_dict)

var_def_NL_dict <- list()
con <- file(file.path(path, 'Variables_Definition_NL.txt'), open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  s <- strsplit(line, ":")[[1]]
  var_def_NL_dict[[s[1]]] <- s[2]
}
close(con)
var_def_NL_dict <- remove_prefix(var_def_NL_dict)

var_def_data_dict <- list()
con <- file(file.path(path, 'Variables_Data_Sources.txt'), open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  s <- strsplit(line, ":")[[1]]
  var_def_data_dict[[s[1]]] <- s[2]
}
close(con)
var_def_data_dict <- remove_prefix(var_def_data_dict)

var_def_data_NL_dict <- list()
con <- file(file.path(path, 'Variables_Data_Sources_NL.txt'), open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  s <- strsplit(line, ":")[[1]]
  var_def_data_NL_dict[[s[1]]] <- s[2]
}
close(con)
var_def_data_NL_dict <- remove_prefix(var_def_data_NL_dict)

var_def_label_dict <- list()
con <- file(file.path(path, 'Variables_Label_R.txt'), open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  s <- strsplit(line, ":")[[1]]
  var_def_label_dict[[s[1]]] <- s[2]
}
close(con)
var_def_label_dict <- remove_prefix(var_def_label_dict)

var_def_label_NL_dict <- list()
con <- file(file.path(path, 'Variables_Label_NL.txt'), open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  s <- strsplit(line, ":")[[1]]
  var_def_label_NL_dict[[s[1]]] <- s[2]
}
close(con)
var_def_label_NL_dict <- remove_prefix(var_def_label_NL_dict)

# Define method to translate dictionary
method_trans_dict <- function(dict_var, trans_label) {
  return(names(dict_var)[sapply(dict_var, function(x) x == trans_label)])
}

# Variable categories - ACTIVE VARIABLES (only these will be shown in the dashboard)
Person_var <- c('AGE_MEAN', 'Gender_Mannen', 'Gender_Vrouwen', 'Age0to20', 'Age21to40', 'Age41to60', 'Age61to80', 'AgeAbove80', 
                'MajorEthnicity_NativeDutch', 'MajorEthnicity_Other')

Huishouden_var <- c('Multiperson_Household', 'HouseholdType_Institutional')

Socioecon_var <- c('Income_MEAN', 'HBOPL_Low', 'HBOPL_Mid', 'HBOPL_High', 'HGOPL_Low', 'HGOPL_Mid', 'HGOPL_High')

Zorgkosten_var <- c('ZVWKOSTENTOTAAL_MEAN', 'ZVWKHUISARTS_MEAN', 'ZVWKHUISARTS_NO_REG_MEAN', 'ZVWKZIEKENHUIS_MEAN', 
                    'ZVWKFARMACIE_MEAN', 'ZVWKOSTENPSYCHO_MEAN', 'ZVWKHUISARTS_user', 'ZVWKFARMACIE_user', 
                    'ZVWKZIEKENHUIS_user', 'ZVWKOSTENPSYCHO_user')

Medicatie_var <- c('UniqueMed_Count5', 'UniqueMed_Count10', 'HVZ_Medication_user', 
                   'DIAB_Medication_user', 'BLOEDDRUKV_Medication_user', 'CHOL_Medication_user', 
                   'DIURETICS_Medication_user', 'DIURETICS_RAAS_BETA_Medication_user', 'Opioid_user_2Years_no_death')

Eerstelijns_var <- c()  # No primary care variables in active list

Secundaire_var <- c()  # No secondary care variables in active list

Eerstelijns_Secundaire_var <- c()  # No combined variables in active list

Ander_var <- c()#c('JGDHULP_user')

Refer_var <- c()  # No referral variables in active list

# COMMENTED OUT VARIABLES (can be easily activated later by uncommenting)
# Person_var_commented <- c('MajorEthnicity_Western', 'MajorEthnicity_NonWestern', 
#                          'MinorEthnicity_Marokko', 'MinorEthnicity_Suriname', 'MinorEthnicity_Turkije', 
#                          'MinorEthnicity_VoormaligeNederlandseAntillenenAruba')

# Huishouden_var_commented <- c('Moving_count_above_1', 'Lifeevents_count_above_2', 'Moving_Count_MEAN', 'Lifeevents_Count_MEAN')

# Socioecon_var_commented <- c('Low_Income', 'Debt_Mortgage', 'Debt_Poor', 'Wanbet', 'Employee', 
#                             'Unemployment_benefit_user', 'Welfare_benefit_user', 'Other_social_benefit_user', 
#                             'Sickness_benefit_user', 'Pension_benefit_user', 'WMO_user', 'WLZ_user')

# Zorgkosten_var_commented <- c()  # All healthcare cost variables are active

# Medicatie_var_commented <- c('UniqueMed_Count_MEAN', 'UniqueMed_Count5', 'UniqueMed_Count10', 
#                            'OPIOID_Medication_user', 'Opioid_user_no_death')

# Eerstelijns_var_commented <- c('Opioid_user_no_death_primary', 'Opioid_user_2Years_no_death_primary', 'ICPC_Hartfalen_patients', 
#                               'Medication_Dependency_patients', 'Medication_Dependency_3Years_patients', 'Medication_Dependency_5Years_patients',
#                               'Alcohol_Dependency_patients', 'Alcohol_Dependency_3Years_patients', 'Alcohol_Dependency_5Years_patients', 
#                               'Loneliness_patients', 'Loneliness_3Years_patients', 'Loneliness_5Years_patients', 
#                               'ICPC_Obesitas_patients', 'BMI_NormalWeight', 'BMI_Obese', 'BMI_OverWeight', 'BMI_UnderWeight')

# Secundaire_var_commented <- c('Hypertensie_patients', 'COPD_patients', 'Diabetes_I_patients', 'Diabetes_II_patients',
#                              'Hartfalen_patients', 'OMA_patients', 'Morbus_Parkinson_patients', 'Heupfractuur_patients', 
#                              'BMIUP45_patients', 'Lung_Cancer_patients', 'Colon_Cancer_patients', 'Back_pain_patients')

# Eerstelijns_Secundaire_var_commented <- c('Opioid_user_no_death_comb', 'Opioid_user_2Years_no_death_comb', 'Hartfalen_PrimarynSecondary_patients', 
#                                          'Primary_care_patients_in_Secondary_care', 'Proxy_Primary_care_refer_to_Secondary_care')

# Ander_var_commented <- c('GEDETINEERDENTAB', 'SHNTAB')

# Refer_var_commented <- c("ICU_Referral", "Ambulance_Referral", "Cardiologist_Referral", "Lab_Referral", "Physiologie_Referral", 
#                         "Primary_care_patients_in_Secondary_care", "Proxy_Primary_care_refer_to_Secondary_care")

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

# Variable dictionary with English labels - ACTIVE VARIABLES ONLY
var_dict_en <- list('Default' = list('Total Population' = "Total_Population", 'Total ICPC Patients Population' = 'Total_ICPCPat_Pop'))

# Add categories safely
person_dict <- create_var_dict(Person_var, var_def_label_dict, 'Person')
if(!is.null(person_dict)) var_dict_en <- c(var_dict_en, person_dict)

household_dict <- create_var_dict(Huishouden_var, var_def_label_dict, 'Household')
if(!is.null(household_dict)) var_dict_en <- c(var_dict_en, household_dict)

socio_dict <- create_var_dict(Socioecon_var, var_def_label_dict, 'Socioeconomic')
if(!is.null(socio_dict)) var_dict_en <- c(var_dict_en, socio_dict)

zorg_dict <- create_var_dict(Zorgkosten_var, var_def_label_dict, 'Healthcare Costs')
if(!is.null(zorg_dict)) var_dict_en <- c(var_dict_en, zorg_dict)

med_dict <- create_var_dict(Medicatie_var, var_def_label_dict, 'Medication')
if(!is.null(med_dict)) var_dict_en <- c(var_dict_en, med_dict)

other_dict <- create_var_dict(Ander_var, var_def_label_dict, 'Other')
if(!is.null(other_dict)) var_dict_en <- c(var_dict_en, other_dict)

# Variable dictionary with Dutch labels - ACTIVE VARIABLES ONLY
var_dict_nl <- list('Standaard' = list('Totale bevolking' = "Total_Population", 'Totaal ELAN-geregistreerde personen' = 'Total_ICPCPat_Pop'))

# Add categories safely
person_dict_nl <- create_var_dict(Person_var, var_def_label_NL_dict, 'Persoon')
if(!is.null(person_dict_nl)) var_dict_nl <- c(var_dict_nl, person_dict_nl)

household_dict_nl <- create_var_dict(Huishouden_var, var_def_label_NL_dict, 'Huishouden')
if(!is.null(household_dict_nl)) var_dict_nl <- c(var_dict_nl, household_dict_nl)

socio_dict_nl <- create_var_dict(Socioecon_var, var_def_label_NL_dict, 'Sociaal-economisch')
if(!is.null(socio_dict_nl)) var_dict_nl <- c(var_dict_nl, socio_dict_nl)

zorg_dict_nl <- create_var_dict(Zorgkosten_var, var_def_label_NL_dict, 'Zorgkosten')
if(!is.null(zorg_dict_nl)) var_dict_nl <- c(var_dict_nl, zorg_dict_nl)

med_dict_nl <- create_var_dict(Medicatie_var, var_def_label_NL_dict, 'Medicatie')
if(!is.null(med_dict_nl)) var_dict_nl <- c(var_dict_nl, med_dict_nl)

other_dict_nl <- create_var_dict(Ander_var, var_def_label_NL_dict, 'Ander')
if(!is.null(other_dict_nl)) var_dict_nl <- c(var_dict_nl, other_dict_nl)

# Default variable dictionary (English)
var_dict <- var_dict_en

# Ensure all dictionaries are properly initialized
if (is.null(var_def_label_dict) || length(var_def_label_dict) == 0) {
  var_def_label_dict <- list()
}
if (is.null(var_def_label_NL_dict) || length(var_def_label_NL_dict) == 0) {
  var_def_label_NL_dict <- list()
}
if (is.null(var_def_dict) || length(var_def_dict) == 0) {
  var_def_dict <- list()
}
if (is.null(var_def_NL_dict) || length(var_def_NL_dict) == 0) {
  var_def_NL_dict <- list()
}
if (is.null(var_def_data_dict) || length(var_def_data_dict) == 0) {
  var_def_data_dict <- list()
}
if (is.null(var_def_data_NL_dict) || length(var_def_data_NL_dict) == 0) {
  var_def_data_NL_dict <- list()
}

# Process data for wijken
df$GMN <- gsub("'", '', df$GMN) 
df$WKN <- gsub("'", '', df$WKN)

GMN_WKN_list <- df %>%
  group_by(GMN) %>%
  summarise(unique_WKN = list(unique(WKN))) %>%
  deframe()

states_wkc <- merge(geo_df, df, by = 'WKC', all.x = F)
states_wkc$WK_CODE <- gsub("WK", 'ID', states_wkc$WKC)

Code_dict_wkn_to_wkc = setNames((states_wkc[states_wkc$YEAR == "2021",]$WKC), (states_wkc[states_wkc$YEAR == "2021",]$WKN))
Code_dict_wkn_to_wk_code = setNames((states_wkc[states_wkc$YEAR == "2021",]$WK_CODE), (states_wkc[states_wkc$YEAR == "2021",]$WKN))
Code_dict_wkc_to_wkn = setNames((states_wkc[states_wkc$YEAR == "2021",]$WKN), (states_wkc[states_wkc$YEAR == "2021",]$WKC))

# Process data for gemeenten
df_gem$GMN <- gsub("'", '', df_gem$GMN) 

states_gmc <- merge(geo_df_gem, df_gem, by = c('GMN','GMC'), all.x = F)
states_gmc$GM_CODE <- gsub("GM", 'ID', states_gmc$GMC)

Code_dict_gmn_to_gmc = setNames((states_gmc[states_gmc$YEAR == "2021",]$GMC), (states_gmc[states_gmc$YEAR == "2021",]$GMN))
Code_dict_gmn_to_gm_code = setNames((states_gmc[states_gmc$YEAR == "2021",]$GM_CODE), (states_gmc[states_gmc$YEAR == "2021",]$GMN))
Code_dict_gmc_to_gmn = setNames((states_gmc[states_gmc$YEAR == "2021",]$GMN), (states_gmc[states_gmc$YEAR == "2021",]$GMC))

# Read Excel files for Data Sources page
df_h <- read_excel(paste0(path, 'Codebook_shorter.xlsx'), sheet = 'Huisartsen')
df_cbs <- read_excel(paste0(path, 'Codebook_shorter.xlsx'), sheet = 'CBS')

# Create Variables Definition data
var_def_label_NL_df <- data.frame(
  columns = names(var_def_label_NL_dict),
  Label = unlist(var_def_label_NL_dict),
  stringsAsFactors = FALSE
)

var_def_NL_df <- data.frame(
  columns = names(var_def_NL_dict),
  Definition = unlist(var_def_NL_dict),
  stringsAsFactors = FALSE
)

# Merge the dataframes
df_label_def_nl <- merge(var_def_label_NL_df, var_def_NL_df, by = "columns")[, c("Label", "Definition")]

# Create fixed color palette for GMN municipalities
all_gmn <- unique(c(states_wkc$GMN, states_gmc$GMN))

gmn_colors <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#1f4e79", "#cc6600", "#1a5f1a", "#a61a1a", "#6b3d8f",
  "#5d3d3d", "#b3478f", "#4d4d4d", "#8b8b1a", "#0f5f5f",
  "#1a4d66", "#b34747", "#b3661a", "#8b5a8b", "#666600",
  "#5d3d1a", "#8b5a1a", "#8b5a8b", "#4d4d4d", "#5a8b5a",
  "#5a5a8b", "#5a8b5a", "#666600", "#4d6666", "#8b5a1a",
  "#5a8b5a", "#8b5a8b", "#4d4d4d", "#5a5a8b", "#5a8b5a"
)

# Create named color vector for GMN
gmn_color_dict <- setNames(gmn_colors[1:length(all_gmn)], all_gmn)

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