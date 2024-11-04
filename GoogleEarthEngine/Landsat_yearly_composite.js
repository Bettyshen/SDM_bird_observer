//*****************************************************//
// Goal: export 30-m of Landat raw bands  from May-July, 2006-2023 yearly
//*****************************************************//

// Import 30-m Landsat Imagery //

var Landsat = ee.ImageCollection("LANDSAT/LE07/C02/T1_L2").filterDate('2010-05-01', '2018-05-31');

// Applies scaling factors to Landsat raw bands.
//https://www.usgs.gov/faqs/how-do-i-use-a-scale-factor-landsat-level-2-science-products
function applyScaleFactors(image) {
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  return image.addBands(opticalBands, null, true);
}

var dataset = Landsat.map(applyScaleFactors);

var visualization = {
  bands: ['SR_B3', 'SR_B2', 'SR_B1'],
  min: 0.0,
  max: 0.3,
};



Map.addLayer(dataset, visualization, 'True Color (321)');


//=== Test visulizing 2010 Landsat with updated cloud mask ===//

var vis = {
  bands: ['SR_B1_median', 'SR_B2_median', 'SR_B3_median'],
  min: -0.1,
  max: 0.2,
};


var Landsat_2010_cloud = ee.Image('projects/shenf934044906/assets/Landsat_May_July_2010');
Map.addLayer(Landsat_2010_cloud, vis, 'Landsat_2010_cloud');


//***======Restrict study area to Oregon State=====***//
//Import study area
var geometry = ee.Geometry.Rectangle([-124.5276, 41.99305, -116.6899, 46.23474]);
Map.centerObject(geometry, 6);   // change the zoom factor if you want.

//References: https://gis.stackexchange.com/questions/340433/making-intra-annual-image-composites-for-a-series-of-years-in-google-earth-engin
//References: https://medium.com/@moraesd90/creating-monthly-ndvi-composites-sentinel-2-on-google-earth-engine-a5c2d49bc9ca

//***********Target the year & month we want*******//
// Define start and end years
var startYear = 2006;
var endYear = 2023;

// Define a list of months as integers
var months = ee.List([5, 6, 7]);

print(months);


// Define a reducer
var myReducer = ee.Reducer.median();

// Make a list of years & months to generate composites for
var yearList = ee.List.sequence(startYear, endYear);
print(yearList);

//Define the band we want to use
// We are including QA band
var bands_to_use = [0,1,2,3,4,5,17]; 

// Applies scaling factors.
//https://www.usgs.gov/faqs/how-do-i-use-a-scale-factor-landsat-level-2-science-products



// Import image collection
// clip to Oregon state scale
var col = ee.ImageCollection('LANDSAT/LE07/C02/T1_L2').filterBounds(geometry);
var col_clipped = col.map(function(img){
  return img.clip(geometry);
});

// Get the first image in the collection to extract band names
var firstImage = col.first();
var bandNames = firstImage.bandNames();
print("Available band names:", bandNames);



//*******Cloud masking function *********//

// This example demonstrates the use of the Landsat 4, 5, 7 Collection 2,
// Level 2 QA_PIXEL band (CFMask) to mask unwanted pixels.

function maskL7srClouds(image) {
  // Bit 0 - Fill
  // Bit 1 - Dilated Cloud
  // Bit 2 - Unused
  // Bit 3 - Cloud
  // Bit 4 - Cloud Shadow
  // convert the QA_PIXEL band to an integer image band before performing the bitwiseAnd operation. 
  var qaMask = image.select('QA_PIXEL').toInt()
    .bitwiseAnd(2).neq(0)
    .bitwiseAnd(4).neq(0)
    .bitwiseAnd(8).neq(0)
    .bitwiseAnd(16).neq(0)
    .bitwiseXor(1); // Invert the mask

  var saturationMask = image.select('QA_RADSAT').eq(0);

  // Apply the scaling factors to the appropriate bands.
//https://www.usgs.gov/faqs/how-do-i-use-a-scale-factor-landsat-level-2-science-products

  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBand = image.select('ST_B6').multiply(0.00341802).add(149.0);

  // Replace the original bands with the scaled ones and apply the masks.
  return image.addBands(opticalBands, null, true)
      .addBands(thermalBand, null, true)
      .updateMask(qaMask)
      .updateMask(saturationMask);
}


// Map the function over one year of data to test.
var collection = ee.ImageCollection('LANDSAT/LE07/C02/T1_L2')
                     .filterDate('2013-05-01', '2013-07-31')
                     .map(maskL7srClouds);

var composite = collection.median();

var minMax2010 = composite.reduceRegion({
    reducer: ee.Reducer.minMax(),
    geometry: geometry,
    scale: 30,
    maxPixels: 1e10
});

print('Min & Max SR_B1_2010 band: ', minMax2010);

// Display the results.
Map.setCenter(-114.2579, 38.9275, 8);
Map.addLayer(composite, {bands: ['SR_B3', 'SR_B2', 'SR_B1'], min: -0.1, max: 0.2},'Landsat_2013_cloud2');



//******************************************************************************
// Map over the list of years to generate a composite for each year
//******************************************************************************
var year_monthCompList = yearList.map(function(year) {
  var startMonth = ee.Date.fromYMD(year, 5, 1);
  var endMonth = ee.Date.fromYMD(year, 7, 31);
  var yearCol = col.filterDate(startMonth, endMonth);

  // Define the image list
  var imgList = yearCol.aggregate_array('system:index');

  var scaledCol = yearCol.map(function(image) {
  return maskL7srClouds(image, bandNames);
});

  // Reduce the collection to get the median composite
  var yearComp = scaledCol.reduce(myReducer);

  var nBands = bands_to_use.length;

  // Select the appropriate bands
  var lstDay = yearComp.select(bands_to_use);

  // Add the 'image_list' property to the yearComp object
  var yearCompWithList = lstDay.set({
    'year': year,
    'month': months,
    'image_list': imgList,
    'n_bands': nBands
  });

  return yearCompWithList;
});

print("Landsat", year_monthCompList);

//******Exporting yearly composite********//
// Export the image to an Earth Engine asset
// Iterate over the median Landsat value and export the yearly composites
// Convert yearList to a JavaScript array
var yearArray = yearList.getInfo(); //storing it in the 'yearArray' variable. #getInfo helps to retrieve the value

// Iterate over the yearArray usinng for loop, and export the yearly composites
  // Withinn each iteration, we export the yearly median Landsat composites.
for (var i = 0; i < yearArray.length; i++) {
  var year = yearArray[i];
  
  // Filter the year_monthCompList for the current year
  var yearComposites = year_monthCompList.filter(ee.Filter.eq('year', year));
  
  // Get the first composite for the current year
  var yearComposite = ee.Image(yearComposites.get(0));
  
   // Define the export parameters
  var exportParams = {
    image: yearComposite.float(),
    description: 'Landsat_' + year,
    assetId: 'Landsat_May_July_' + year,
    crs: Landsat.crs,
    scale: 30,
    maxPixels: 1E10,
    region: geometry
  };
  
  // Export the composite
  Export.image.toAsset(exportParams);
}





//******************************************************************************
//Visualization - just make sure everything looks right
//******************************************************************************
var visbands = ['SR_B1_median']; 
// Iterate over the yearArray and add each year's composite to the map
yearArray.forEach(function(year) {
  var yearComposite = ee.Image('projects/shenf934044906/assets/Landsat_May_July_' + year).select(visbands); // Change the asset ID based on your export naming convention
  Map.addLayer(yearComposite, viz, 'Landsat_' + year);
});

//=====Get min & max value from SR_B1_median =====//

var SRB1 = ee.Image('projects/shenf934044906/assets/Landsat_May_July_2010').select(visbands);

var minMax = SRB1.reduceRegion({
    reducer: ee.Reducer.minMax(),
    geometry: geometry,
    scale: 30,
    maxPixels: 1e10
});

print('Min & Max SR_B1 band: ', minMax);



// Specify the band names as a comma-separated string in the visualization parameters
// var viz = {
//   bands: ('SR_B1_median', 'SR_B2_median', 'SR_B3_median'),
//   //min: [-0.002],
//   //max: [0.6],
//   gamma: [5.0], //gamma: Controls the brightness of the image,
//   palette: ['0000FF', '00FF00', 'FF0000'] // Custom RGB color codes for each band
// };

var viz = {
  bands: ('SR_B1_median'),
  min: -0.1,
  max: 0.2,
  gamma: [10], //gamma: Controls the brightness of the image,
  palette: ['0000FF'] // Custom RGB color codes for each band
};

//Map.addLayer(lsCompCol, viz, 'Simple median');
Map.setCenter(-120.92442023761322,44.03046398973987, 7);

