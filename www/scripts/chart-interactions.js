// Chart Interaction Functions
// This file contains JavaScript functions for interactive chart features

const ChartInteractions = {
  // GMN Color mapping - should match the R color dictionary
  colorMap: {
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

  // Get color for a specific GMN
  getGMNColor(gmn) {
    return this.colorMap[gmn] || '#000000';
  },

  // Highlight specific GMN on charts
  highlightGMN(gmn) {
    const color = this.getGMNColor(gmn);
    const svgElements = document.querySelectorAll('svg');
    
    svgElements.forEach(svg => {
      // Find and highlight lines and points with the specific color
      const lines = svg.querySelectorAll(`path[stroke="${color}"]`);
      const points = svg.querySelectorAll(`circle[fill="${color}"]`);
      
      // Highlight matching elements
      lines.forEach(line => {
        line.setAttribute('stroke-width', '3');
        line.style.opacity = '1';
        line.style.strokeWidth = '3px';
      });
      
      points.forEach(point => {
        point.setAttribute('r', '4');
        point.style.opacity = '1';
      });
      
      // Dim other elements
      svg.querySelectorAll('path').forEach(line => {
        const lineColor = line.getAttribute('stroke');
        if (lineColor !== color) {
          line.style.opacity = '0.3';
        }
      });
      
      svg.querySelectorAll('circle').forEach(point => {
        const pointColor = point.getAttribute('fill');
        if (pointColor !== color) {
          point.style.opacity = '0.3';
        }
      });
    });
  },

  // Reset all highlights
  resetHighlight() {
    const svgElements = document.querySelectorAll('svg');
    
    svgElements.forEach(svg => {
      svg.querySelectorAll('path').forEach(line => {
        line.setAttribute('stroke-width', '0.5');
        line.style.opacity = '1';
        line.style.strokeWidth = '0.5px';
      });
      
      svg.querySelectorAll('circle').forEach(point => {
        point.setAttribute('r', '1.5');
        point.style.opacity = '1';
      });
    });
  }
};

// Global functions for backward compatibility
function highlightGMN(gmn) {
  ChartInteractions.highlightGMN(gmn);
}

function resetHighlight() {
  ChartInteractions.resetHighlight();
}

function getGMNColor(gmn) {
  return ChartInteractions.getGMNColor(gmn);
} 