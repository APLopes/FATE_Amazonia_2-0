// Define the ImageCollection and create a mosaic
var qcn = ee.ImageCollection('projects/mapbiomas-workspace/SEEG/2022/QCN/QCN_30m_BR_v2_0_1').mosaic();
var geometry = ee.Geometry.Rectangle([-74.1, -33.8, -34.9, 5.9]);
print('qcn', qcn);

// Load SEEG Mask Stable in the last year
var mask_stable = ee.ImageCollection('projects/mapbiomas-workspace/SEEG/2023/c10/2_0_Mask_stable').toBands().eq(3).selfMask().slice(-1);
print('mask_stable', mask_stable);
Map.addLayer(mask_stable, {}, 'mask_stable', false);

// Load Amazon biome
var biome = ee.FeatureCollection('users/camilaflorestal/MAPBIOMAS/mb_biomescopy').filter(ee.Filter.eq('Bioma', 'Amazônia'));
var biomeMask = ee.Image().paint(biome).eq(0);
Map.addLayer(biomeMask, {}, 'biomeMask', false);

// Apply the mask
var mask = mask_stable.updateMask(biomeMask);
Map.addLayer(mask, {}, 'mask');

// Create the list of layers to process
var lists = [
  ['AGB stock', qcn.select('cagb')],
  ['AGN stock (CWD + FWD)', qcn.select('cdw').add(qcn.select('clitter')).rename('AGNstock')],
  ['CWD stock (AGN)', qcn.select('cdw')],
  ['FWD stock (AGN)', qcn.select('clitter')],
];

var scale = 30;
var data = [];

// Process each layer
var feats = lists.map(function(list) {
  var name = list[0];
  var img = list[1].updateMask(mask);
  var bandName = img.bandNames().getString(0);
  
  // Calculate min and max values
  var range = img.reduceRegion({
    reducer: ee.Reducer.minMax(),
    geometry: geometry,
    scale: scale,
    maxPixels: 1e13,
  });
  
  var min = ee.Number(range.get(bandName.cat('_min')));
  var max = ee.Number(range.get(bandName.cat('_max')));
  
  // Calculate standard deviation
  var stdDev = img.reduceRegion({
    reducer: ee.Reducer.stdDev(),
    geometry: geometry,
    scale: scale,
    maxPixels: 1e13,
  });
  
  stdDev = ee.Number(stdDev.get(bandName));
  
  // Calculate total area in hectares
  var pixelArea_ha = ee.Image.pixelArea().divide(10000).updateMask(mask);
  var total_area = ee.Number(pixelArea_ha.reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: geometry,
    scale: scale,
    maxPixels: 1e13,
  }).get('area'));
  
  // Calculate total toneladas
  var total_toneladas = ee.Number(pixelArea_ha.multiply(img).reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: geometry,
    scale: scale,
    maxPixels: 1e13,
  }).get('area'));
  
  // Calculate weighted mean
  var weighted_mean = ee.Number(total_toneladas).divide(total_area);
  
  // Collect data as strings
  return ee.Feature(null).set({
    name: name,
    min: min,
    max: max,
    stdDev: stdDev,
    weighted_mean: weighted_mean
  });
});

var featsCollection = ee.FeatureCollection(feats);
print('feats', feats);

// Create a chart from the collected data, it only  applies to 500m scale
var chart = ui.Chart.feature.byFeature(featsCollection, 'name', ['min', 'max', 'stdDev', 'weighted_mean'])
  .setChartType('Table')
  .setOptions({
    title: 'Stock Analysis',
    columns: [
      {label: 'Stock Type'},
      {label: 'Min'},
      {label: 'Max'},
      {label: 'Standard Deviation'},
      {label: 'Weighted Mean'},
    ]
  });

// Print the chart
print(chart);

// Export the results to Google Drive as a CSV file
Export.table.toDrive(
  featsCollection,
  'QCN_metrics',
  'QCN_metrics',
  'QCN_metrics', 
  'CSV'
);
