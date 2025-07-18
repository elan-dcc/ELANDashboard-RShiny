# Data Directory

This directory contains all the data files required for the ELAN Dashboard.

## File Structure

### Core Data Files (Required by Dashboard)
- **CSV Files:**
  - `df_numeric_gem_ver_6.csv` - Gemeente numeric data
  - `df_count_gem_ver_6.csv` - Gemeente count data
  - `df_numeric_ver_6.csv` - Wijken numeric data
  - `df_count_ver_6.csv` - Wijken count data

- **Excel Files:**
  - `Codebook_shorter.xlsx` - Data dictionary and codebook

- **Shapefiles:**
  - `gemeenten_2023_v1.shp` - Gemeente boundaries (with .shx, .dbf, .prj, .cpg)
  - `wijk_2023_v0.shp` - Wijken boundaries (with .shx, .dbf, .prj)

- **Variable Definition Files:**
  - `Variables_Definition.txt` - Variable definitions (English)
  - `Variables_Definition_NL.txt` - Variable definitions (Dutch)
  - `Variables_Data_Sources.txt` - Data source information (English)
  - `Variables_Data_Sources_NL.txt` - Data source information (Dutch)
  - `Variables_Label_R.txt` - Variable labels (English)
  - `Variables_Label_NL.txt` - Variable labels (Dutch)

### Additional Files (Not used by current dashboard)
- Various version files (ver_3, ver_5)
- Additional Excel files with care data
- JSON and GeoJSON files
- Clustered data files

## Usage

The dashboard automatically loads these files from the `data/` directory. The path is configured in `dashboard/global_dashboard.R`.

## File Sizes

- CSV files: ~5-600KB each
- Shapefiles: ~30-50MB each
- Excel files: ~14KB
- Text files: ~5-12KB each

## Notes

- All files are encoded in UTF-8 or Latin1 as appropriate
- Shapefiles include all necessary components (.shp, .shx, .dbf, .prj, .cpg)
- The dashboard uses version 6 of the data files 