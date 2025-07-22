# ELAN Dashboard - Modular Structure

Link for the current version of the dashboard:

https://elandcc.shinyapps.io/ELANDashboard/

This document explains the new structure of the ELAN Dashboard.

## 📁 New File Structure

```
Project 1/
├── app.R                               # Original app file 
├── server.R                            # Original server file
├── ui.R                                # Updated UI file
├── global.R                            # Original global file
├── dashboard/                          # NEW: Dashboard components
│   ├── README.md                       # Dashboard documentation
│   ├── global_dashboard.R              # Dashboard data and functions
│   ├── ui_dashboard.R                  # Main dashboard UI
│   ├── server_dashboard.R              # Main dashboard server
│   └── components/                     # Individual tab components
│       ├── wijken/
│       │   ├── ui_wijken.R            # Wijken tab UI
│       │   └── server_wijken.R        # Wijken tab server logic
│       └── gemeente/
│           ├── ui_gemeente.R           # Gemeente tab UI
│           └── server_gemeente.R       # Gemeente tab server logic
├── ui_panels/                          # Other nav panels
│   ├── README.md                       # Panel documentation
│   ├── load_panels.R                   # Panel loader
│   ├── variables_definition_panel.R    # Variables Definition panel
│   ├── data_dictionary_panel.R         # Data Dictionary panel
│   ├── change_log_panel.R              # Change Log panel
│   └── about_us_panel.R                # About Us panel
├── www/                                # Static assets
│   ├── style.css                       # Main stylesheet
│   ├── script.js                       # JavaScript functions
│   ├── chart-interactions.js           # Chart interaction functions
│   └── config.js                       # Configuration file
└── [other files...]
```

## 🚀 How to Use the New Structure

### Option 1: Use the New Modular App (Recommended)
```r
# Run the new modular app
source("app.R")
```


## 📋 Component Breakdown

### Dashboard Components (`dashboard/`)

#### `global_dashboard.R`
- All data loading and processing
- Variable definitions and dictionaries
- Color palettes and themes
- Helper functions

#### `ui_dashboard.R`
- Combines Wijken and Gemeente UI components
- Main dashboard navigation structure
- Tab selection logic

#### `server_dashboard.R`
- Combines Wijken and Gemeente server logic
- Custom theme functions
- Main server coordination

#### Wijken Components (`dashboard/components/wijken/`)
- **`ui_wijken.R`**: Complete Wijken tab UI
- **`server_wijken.R`**: Complete Wijken tab server logic

#### Gemeente Components (`dashboard/components/gemeente/`)
- **`ui_gemeente.R`**: Complete Gemeente tab UI
- **`server_gemeente.R`**: Complete Gemeente tab server logic

## 🛠️ Development Workflow

### Adding New Features to Wijken Tab
1. Edit `dashboard/components/wijken/ui_wijken.R` for UI changes
2. Edit `dashboard/components/wijken/server_wijken.R` for server logic
3. Test the specific tab functionality
4. No need to touch Gemeente components

### Adding New Features to Gemeente Tab
1. Edit `dashboard/components/gemeente/ui_gemeente.R` for UI changes
2. Edit `dashboard/components/gemeente/server_gemeente.R` for server logic
3. Test the specific tab functionality
4. No need to touch Wijken components

### Adding Global Features
1. Edit `dashboard/global_dashboard.R` for data changes
2. Edit `dashboard/ui_dashboard.R` for main UI changes
3. Edit `dashboard/server_dashboard.R` for main server changes

## 📊 File Size Comparison

| File Type | Original | New Structure | Improvement |
|-----------|----------|---------------|-------------|
| UI Logic | 1 large file | 3 focused files | Better organization |
| Server Logic | 1 large file | 3 focused files | Easier maintenance |
| Data Loading | Mixed in global.R | Dedicated file | Clear separation |


## 🚀 Future Enhancements

### Potential Improvements

1. **Component Testing Framework**
   - Unit tests for individual components
   - Integration tests for component interactions

2. **Configuration Management**
   - External configuration files
   - Environment-specific settings

3. **Performance Optimization**
   - Lazy loading of components
   - Caching strategies

4. **Documentation Generation**
   - Automatic API documentation
   - Component usage examples


