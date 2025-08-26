function highlightGMN(gmn) {
  // Find all SVG elements with the specific GMN color
  const color = getGMNColor(gmn);
  
  // Target all SVG elements in the current chart
  const svgElements = document.querySelectorAll('svg');
  
  svgElements.forEach(svg => {
    // Find lines and points with the specific color
    const lines = svg.querySelectorAll('path[stroke="' + color + '"]');
    const points = svg.querySelectorAll('circle[fill="' + color + '"]');
    
    // Highlight the specific GMN lines and points
    lines.forEach(line => {
      line.setAttribute('stroke-width', '3');
      line.style.opacity = '1';
      line.style.strokeWidth = '3px';
    });
    
    points.forEach(point => {
      point.setAttribute('r', '4');
      point.style.opacity = '1';
    });
    
    // Dim other lines
    svg.querySelectorAll('path').forEach(line => {
      const lineColor = line.getAttribute('stroke');
      if (lineColor !== color) {
        line.style.opacity = '0.3';
      }
    });
    
    // Dim other points
    svg.querySelectorAll('circle').forEach(point => {
      const pointColor = point.getAttribute('fill');
      if (pointColor !== color) {
        point.style.opacity = '0.3';
      }
    });
  });
}

function resetHighlight() {
  // Reset all lines and points to normal
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

function getGMNColor(gmn) {
  // Color mapping - this should match the R color dictionary
  const colorMap = {
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
  };
  return colorMap[gmn] || '#000000';
} 