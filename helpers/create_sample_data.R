# Script to create smaller sample data files for Shinylive export
# This will create smaller versions of the data files to reduce the app.json size

library(dplyr)
library(sf)

# Set the data path
path <- "data/"

# Create sample data directory
sample_path <- "data_sample/"
if (!dir.exists(sample_path)) {
  dir.create(sample_path)
}

# 1. Create smaller CSV files
cat("Creating smaller CSV files...\n")

# Read the original files
df_numeric <- read.csv(file.path(path, 'df_numeric_ver_6.csv'), sep = ',', encoding = 'latin1')
df_count <- read.csv(file.path(path, 'df_count_ver_6.csv'), sep = ',', encoding = 'latin1')
df_numeric_gem <- read.csv(file.path(path, 'df_numeric_gem_ver_6.csv'), sep = ',', encoding = 'latin1')
df_count_gem <- read.csv(file.path(path, 'df_count_gem_ver_6.csv'), sep = ',', encoding = 'latin1')

# Take only a subset of the data (first 100 rows or unique combinations)
df_numeric_sample <- df_numeric %>%
  group_by(WKC, WKN, GMN, YEAR) %>%
  slice_head(n = 1) %>%
  ungroup()

df_count_sample <- df_count %>%
  group_by(WKC, WKN, GMN, YEAR) %>%
  slice_head(n = 1) %>%
  ungroup()

df_numeric_gem_sample <- df_numeric_gem %>%
  group_by(GMC, GMN, YEAR) %>%
  slice_head(n = 1) %>%
  ungroup()

df_count_gem_sample <- df_count_gem %>%
  group_by(GMC, GMN, YEAR) %>%
  slice_head(n = 1) %>%
  ungroup()

# Write sample files
write.csv(df_numeric_sample, file.path(sample_path, 'df_numeric_ver_6.csv'), row.names = FALSE)
write.csv(df_count_sample, file.path(sample_path, 'df_count_ver_6.csv'), row.names = FALSE)
write.csv(df_numeric_gem_sample, file.path(sample_path, 'df_numeric_gem_ver_6.csv'), row.names = FALSE)
write.csv(df_count_gem_sample, file.path(sample_path, 'df_count_gem_ver_6.csv'), row.names = FALSE)

# 2. Copy the smaller GeoJSON files
cat("Copying GeoJSON files...\n")
file.copy(file.path(path, 'wijk_2023_v0.geojson'), file.path(sample_path, 'wijk_2023_v0.geojson'))
file.copy(file.path(path, 'gemeenten_2023_v1.geojson'), file.path(sample_path, 'gemeenten_2023_v1.geojson'))

# 3. Copy the text files
cat("Copying text files...\n")
text_files <- c(
  'Variables_Label_R.txt',
  'Variables_Label_NL.txt',
  'Variables_Data_Sources_NL.txt',
  'Variables_Data_Sources.txt',
  'Variables_Definition_NL.txt',
  'Variables_Definition.txt'
)

for (file in text_files) {
  if (file.exists(file.path(path, file))) {
    file.copy(file.path(path, file), file.path(sample_path, file))
  }
}

# 4. Copy the Excel file
cat("Copying Excel file...\n")
file.copy(file.path(path, 'Codebook_shorter.xlsx'), file.path(sample_path, 'Codebook_shorter.xlsx'))

cat("Sample data files created in 'data_sample/' directory.\n")
cat("You can now use this directory for Shinylive export.\n") 