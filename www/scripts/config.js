// Configuration file for the ELAN Dashboard
// Contains color mappings and other configuration settings

const DashboardConfig = {
  // GMN Color mapping - matches the R color dictionary from global.R
  gmnColors: {
    's-Gravenhage': '#1f77b4',
    'Leiden': '#ff7f0e',
    'Delft': '#2ca02c',
    'Wassenaar': '#d62728',
    'Rijswijk': '#9467bd',
    'Leidschendam-Voorburg': '#8c564b',
    'Zoetermeer': '#e377c2',
    'Westland': '#7f7f7f',
    'Pijnacker-Nootdorp': '#bcbd22',
    'Midden-Delfland': '#17becf',
    'Alphen aan den Rijn': '#a6cee3',
    'Hillegom': '#fb9a99',
    'Lisse': '#fdbf6f',
    'Noordwijk': '#cab2d6',
    'Oegstgeest': '#ffff99',
    'Katwijk': '#b15928',
    'Kaag en Braassem': '#fdb462',
    'Nieuwkoop': '#b3de69',
    'Teylingen': '#fccde5',
    'Leiderdorp': '#d9d9d9',
    'Voorschoten': '#bc80bd',
    'Zoeterwoude': '#ccebc5',
    'Waddinxveen': '#ffed6f',
    'Bodegrave-Reeuwijk': '#80b1d3'
  },

  // Chart interaction settings
  chartSettings: {
    highlightStrokeWidth: '3',
    highlightPointRadius: '4',
    normalStrokeWidth: '0.5',
    normalPointRadius: '1.5',
    dimmedOpacity: '0.3',
    normalOpacity: '1'
  },

  // Default fallback color
  defaultColor: '#000000'
};

// Export for use in other files
if (typeof module !== 'undefined' && module.exports) {
  module.exports = DashboardConfig;
} 