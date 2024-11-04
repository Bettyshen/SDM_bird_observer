//***************Goal********************//
// The goal of this script is to extract pixel value for all species via observer-location approach for fixed radius extraction method

///////////////////////////////////////////////////////////////////////
///Section 1 - Load all Oregon data & Visualize on Map
///////////////////////////////////////////////////////////////////////
var oregondata = ee.FeatureCollection("projects/inspired-alcove-407218/assets/oregon_all_zerofilled");

//====Effective Strip Width for Birds=======//
var effective_sw = ee.FeatureCollection("projects/shenf934044906/assets/Oregon2020-eBird/effective_strip_width_info");

//=== Extract species list ====//
var speciesList = effective_sw.distinct('common_name').aggregate_array('common_name');
print(speciesList);

// === Subset species list (test) ===//
  //=== Set 1: 1-50 species ===//
var speciesSet1 = speciesList.slice(0,50);
print(speciesSet1, "species set 1");
  //=== Set 2: 51-100 species ===//
var speciesSet2 = speciesList.slice(50,100);
print(speciesSet2, "species set 2");
  //=== Set 3: 101-150 species ===//
var speciesSet3 = speciesList.slice(100,150);
print(speciesSet3, "species set 3");
  //=== Set 4: 151-198 species ===//
var speciesSet4 = speciesList.slice(150,199);
print(speciesSet4, "species set 4");

//===== Visualize coordinate points on the map, both presence & absence ====//
Map.addLayer(oregondata, {color:"blue"}, "All Oregon Bird Sampling Sites");

//Create legend for the spots
 var legend = ui.Panel({style: {position: "bottom-left", padding: "8px15px"}});

 legend.add(ui.Label({
  value: "Bird Sampling Locations",
  style: {fontWeight: "bold", fontSize: "18px", margin: "0 0 4px 0", padding: "0px"}
}));

legend.add(ui.Panel(
  [ui.Label({value: "All Bird Sampling Sites", style: {fontWeight: "bold",
    fontSize: "16px", margin: "0 0 4px 0"}}),
    ui.Label({style:{color: "blue", margin: "0 0 0 4px"}, value: "◉"})],
    ui.Panel.Layout.Flow("horizontal")));
    
///////////////////////////////////////////////////////////////////////////
///Section 2 - Develop Buffer & Reduce Image Collection to Point Function
//////////////////////////////////////////////////////////////////////////

  // Create Buffer Point Function
function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}


 // Create reducer to capture raster data (i.e., image collection) from points
 // https://developers.google.com/earth-engine/tutorials/community/extract-raster-values-for-points 
 function zonalStats(ic, fc, params) {
  // Initialize internal params dictionary.
  var _params = {
    reducer: ee.Reducer.median(),
    scale: null,
    crs: null,
    bands: null,
    bandsRename: null,
    imgProps: null,
    imgPropsRename: null,
    datetimeName: 'datetime',
    datetimeFormat: 'YYYY-MM-dd HH:mm:ss'
  };

  // Replace initialized params with provided params.
  if (params) {
    for (var param in params) {
      _params[param] = params[param] || _params[param];
    }
  }

  // Set default parameters based on an image representative.
  var imgRep = ic.first();
  var nonSystemImgProps = ee.Feature(null)
    .copyProperties(imgRep).propertyNames();
  if (!_params.bands) _params.bands = imgRep.bandNames();
  if (!_params.bandsRename) _params.bandsRename = _params.bands;
  if (!_params.imgProps) _params.imgProps = nonSystemImgProps;
  if (!_params.imgPropsRename) _params.imgPropsRename = _params.imgProps;

  // Map the reduceRegions function over the image collection.
  var results = ic.map(function(img) {
    // Select bands (optionally rename), set a datetime & timestamp property.
    img = ee.Image(img.select(_params.bands, _params.bandsRename))
      .set(_params.datetimeName, img.date().format(_params.datetimeFormat))
      .set('timestamp', img.get('system:time_start'));

    // Define final image property dictionary to set in output features.
    var propsFrom = ee.List(_params.imgProps)
      .cat(ee.List([_params.datetimeName, 'timestamp']));
    var propsTo = ee.List(_params.imgPropsRename)
      .cat(ee.List([_params.datetimeName, 'timestamp']));
    var imgProps = img.toDictionary(propsFrom).rename(propsFrom, propsTo);

    // Subset points that intersect the given image.
    var fcSub = fc.filterBounds(img.geometry());

    // Reduce the image by regions.
    return img.reduceRegions({
      collection: fcSub,
      reducer: _params.reducer,
      scale: _params.scale,
      crs: _params.crs
    })
    // Add metadata to each feature.
    .map(function(f) {
      return f.set(imgProps);
    });
  }).flatten().filter(ee.Filter.notNull(_params.bandsRename));

  return results;
}

/////////////////////////////////////////////////////////////////////////////////////////
///Section 3 - Loop over each year and extract pixel value from fixed-radius extraction strategy
/////////////////////////////////////////////////////////////////////////////////////////

// //============== Write a loop to extract 2010-2020 Bird environmental data by year =======================//
// Define start and end years for original Oregon 2020 data (2010-2020)
var startYear = 2010;
var endYear = 2020;
// Make a list of years
var yearList = ee.List.sequence(startYear, endYear);
print(yearList);

//var yearArray = yearList.getInfo(); //storing it in the 'yearArray' variable. # getInfo helps to retrieve the value

// Import 30-m Landsat Imagery for coordinate system alignment (CRS) //
var Landsat = ee.ImageCollection("LANDSAT/LE07/C02/T1_L2").filterDate('2018-01-01', '2018-05-01');

//Define parameters for the zonalStats function.
var params = {
  bands: ["LST_Day_median", "EVI_median", "SR_B1_median", "SR_B2_median", "SR_B3_median", 
  "SR_B4_median", "SR_B5_median", "SR_B7_median", "QA_PIXEL_median", "ppt_median", "tmean_median"],
  bandsRename: ["LST_Day_median", "EVI_median", "SR_B1_median", "SR_B2_median", "SR_B3_median", 
  "SR_B4_median", "SR_B5_median", "SR_B7_median", "QA_PIXEL_median", "ppt_median", "tmean_median"],
  crs: Landsat.crs,
  maxPixels: 2E10,
  scale: 30
};


//========================================================//
// ***** Extraction Method 1: Fixed Radius (100 m) *******
// Divide species list into 50 as a set to run
//=======================================================//

// Function to process each species and year
function processSpeciesAndYear(speciesName, year) {
  //Filter bird data for the current species
    var speciesBird = oregondata.filter(ee.Filter.eq("Common_Name", speciesName));
    
    // Create an empty FeatureCollection to store yearly results for the current species
    var speciesYearsResults = ee.FeatureCollection([]);
      
      // Filter bird data for the current year
      var yearBird = speciesBird.filter(ee.Filter.eq("Year", year));
      
      // Buffer the points
      var ptsEnvi = yearBird.map(bufferPoints(100,false));
      
      // Construct the image path based on the project
        // Landsat
      var Landsatimage =  ee.Image("projects/shenf934044906/assets/EnvirPredict_" + year + "/Landsat_May_July_" + year).set('system:time_start', ee.Date(year + '-05-08').millis());
        // Land Surface Temperature
      var LSTimage = ee.Image("projects/shenf934044906/assets/EnvirPredict_" + year + "/Land_ST_May_July_" + year).set('system:time_start', ee.Date(year + '-05-08').millis());
        // Enhanced Vegetation Index
      var EVIimage = ee.Image("projects/shenf934044906/assets/EnvirPredict_" + year + "/EVI_May_July_" + year).set('system:time_start', ee.Date(year + '-05-08').millis());
        // PRISM (precipitation + temperature)
      var Prismimage = ee.Image("projects/shenf934044906/assets/EnvirPredict_" + year + "/PRISM_May_July_" + year).set('system:time_start', ee.Date(year + '-05-08').millis());
      // Combine the images
      var combinedImageCollection = ee.ImageCollection(ee.Image.cat(Landsatimage, LSTimage, EVIimage, Prismimage));
      
      // Extract zonal statistics per point per image
      var ptsEnviStats_bird_year = zonalStats(combinedImageCollection, ptsEnvi, params);
      
      // Add current year results to speciesYearsResults
      speciesYearsResults = speciesYearsResults.merge(ptsEnviStats_bird_year);
      
      return speciesYearsResults;
    }


// Extract bird points by year and for each species one at a time
  // ==== Species set 1 ====//
  // Create an empty feature collection to store all yearly results for 1-50 species
var allYearsResults = ee.FeatureCollection([]);
speciesSet1.getInfo().forEach(function(speciesName){
  // loop over each year
  yearList.getInfo().forEach(function(year) {
    // Process each species and year
    var speciesYearsResults = processSpeciesAndYear(speciesName, year);
    
    // Merge the results into allYearsResults
    allYearsResults = allYearsResults.merge(speciesYearsResults);
});
});
  
  // ==== Species set 2 ====//  
  // Create an empty feature collection to store all yearly results for 51-100 species
var allYearsResults2 = ee.FeatureCollection([]);
speciesSet2.getInfo().forEach(function(speciesName){
  // loop over each year
  yearList.getInfo().forEach(function(year) {
    // Process each species and year
    var speciesYearsResults = processSpeciesAndYear(speciesName, year);
    
    // Merge the results into allYearsResults
    allYearsResults2 = allYearsResults2.merge(speciesYearsResults);
});
});


  // ==== Species set 3 ====// 
  // Create an empty feature collection to store all yearly results for 101-150 species
var allYearsResults3 = ee.FeatureCollection([]);
speciesSet3.getInfo().forEach(function(speciesName){
  // loop over each year
  yearList.getInfo().forEach(function(year) {
    // Process each species and year
    var speciesYearsResults = processSpeciesAndYear(speciesName, year);
    
    // Merge the results into allYearsResults
    allYearsResults3 = allYearsResults3.merge(speciesYearsResults);
});
});

  // ==== Species set 4 ====//  
  // Create an empty feature collection to store all yearly results for 151-198 species
var allYearsResults4 = ee.FeatureCollection([]);
speciesSet4.getInfo().forEach(function(speciesName){
  // loop over each year
  yearList.getInfo().forEach(function(year) {
    // Process each species and year
    var speciesYearsResults = processSpeciesAndYear(speciesName, year);
    
    // Merge the results into allYearsResults
    allYearsResults4 = allYearsResults4.merge(speciesYearsResults);
});
});

// Try to merge feature collection and export into one CSV file
var allYearsResultsComplete = allYearsResults.merge(allYearsResults2).merge(allYearsResults3).merge(allYearsResults4);
  // Export data table
    // All species for Oregon 2020 data
  Export.table.toDrive({
  collection: allYearsResultsComplete,
  description: 'Median_surveyCentered_100mfixedRadius',
  folder: "GoogleEarthEngine",
  fileFormat: 'CSV'
  });

//================ For special years(not Oregon 2020 data) =====================//
// Define start and end years for original Oregon 2020 data
  // 2006, 2007, 2008, 2009, 2021, 2022, 2023 Environmental Data are stored in other projects. We need to retreive them separately
  var specialYear = ee.List([]).add(2006).add(2007).add(2008).add(2009).add(2021).add(2022).add(2023);
  print(specialYear, 'Special Year');
  //var specialYearArray = specialYear.getInfo(); //storing it in the 'yearArray' variable. # getInfo helps to retrieve the value
  //print(specialYearArray, 'Special Year');
  
  
  // Function to process each species and year
function processSpeciesAndSpecialYear(speciesName, year) {
  //Filter bird data for the current species
    var speciesBird = oregondata.filter(ee.Filter.eq("Common_Name", speciesName));
    
    // Create an empty FeatureCollection to store yearly results for the current species
    var speciesSpecialYearsResults = ee.FeatureCollection([]);
      
      // Filter bird data for the current year
      var yearBird = speciesBird.filter(ee.Filter.eq("Year", year));
      
      // Buffer the points
      var ptsEnvi = yearBird.map(bufferPoints(100,false));
      
      // Construct the image path based on the project
        // Landsat
      var Landsatimage =  ee.Image("projects/inspired-alcove-407218/assets/EnvirPredict_" + year + "/Landsat_May_July_" + year).set('system:time_start', ee.Date(year + '-05-08').millis());
        // Land Surface Temperature
      var LSTimage = ee.Image("projects/inspired-alcove-407218/assets/EnvirPredict_" + year + "/Land_ST_May_July_" + year).set('system:time_start', ee.Date(year + '-05-08').millis());
        // Enhanced Vegetation Index
      var EVIimage = ee.Image("projects/inspired-alcove-407218/assets/EnvirPredict_" + year + "/EVI_May_July_" + year).set('system:time_start', ee.Date(year + '-05-08').millis());
        // PRISM (precipitation + temperature)
      var Prismimage = ee.Image("projects/inspired-alcove-407218/assets/EnvirPredict_" + year + "/PRISM_May_July_" + year).set('system:time_start', ee.Date(year + '-05-08').millis());
      // Combine the images
      var combinedImageCollection = ee.ImageCollection(ee.Image.cat(Landsatimage, LSTimage, EVIimage, Prismimage));
      
      // Extract zonal statistics per point per image
      var ptsEnviStats_bird_year = zonalStats(combinedImageCollection, ptsEnvi, params);
      
      // Add current year results to special year results
      speciesSpecialYearsResults = speciesSpecialYearsResults.merge(ptsEnviStats_bird_year);
      
      return speciesSpecialYearsResults;
    }
    
// Extract bird points by year and for each species one at a time
  // ==== Species set 1 ====//
  // Create an empty feature collection to store all yearly results for 1-50 species
var allSpecialYearsResults = ee.FeatureCollection([]);
speciesSet1.getInfo().forEach(function(speciesName){
  // loop over each year
  specialYear.getInfo().forEach(function(year) {
    // Process each species and year
    var speciesSpecialYearsResults = processSpeciesAndSpecialYear(speciesName, year);
    
    // Merge the results into allYearsResults
    allSpecialYearsResults = allSpecialYearsResults.merge(speciesSpecialYearsResults);
});
});
  
  // ==== Species set 2 ====//  
  // Create an empty feature collection to store all yearly results for 51-100 species
var allSpecialYearsResults2 = ee.FeatureCollection([]);
speciesSet2.getInfo().forEach(function(speciesName){
  // loop over each year
  specialYear.getInfo().forEach(function(year) {
    // Process each species and year
    var speciesSpecialYearsResults = processSpeciesAndSpecialYear(speciesName, year);
    
    // Merge the results into allYearsResults
    allSpecialYearsResults2 = allSpecialYearsResults2.merge(speciesSpecialYearsResults);
});
});


  // ==== Species set 3 ====// 
  // Create an empty feature collection to store all yearly results for 101-150 species
var allSpecialYearsResults3 = ee.FeatureCollection([]);
speciesSet3.getInfo().forEach(function(speciesName){
  // loop over each year
  specialYear.getInfo().forEach(function(year) {
    // Process each species and year
    var speciesSpecialYearsResults = processSpeciesAndSpecialYear(speciesName, year);
    
    // Merge the results into allYearsResults
    allSpecialYearsResults3 = allSpecialYearsResults3.merge(speciesSpecialYearsResults);
});
});

  // ==== Species set 4 ====//  
  // Create an empty feature collection to store all yearly results for 151-198 species
var allSpecialYearsResults4 = ee.FeatureCollection([]);
speciesSet4.getInfo().forEach(function(speciesName){
  // loop over each year
  specialYear.getInfo().forEach(function(year) {
    // Process each species and year
    var speciesSpecialYearsResults = processSpeciesAndSpecialYear(speciesName, year);
    
    // Merge the results into allYearsResults
    allSpecialYearsResults4 = allSpecialYearsResults4.merge(speciesSpecialYearsResults);
});
});


// Try to merge feature collection and export into one CSV file
var AllSpecialYearResults = allYearsResultsComplete.merge(allSpecialYearsResults).merge(allSpecialYearsResults2).merge(allSpecialYearsResults3).merge(allSpecialYearsResults4);
  // Export data table
    // All species for Oregon 2020 data + special year bird data
  Export.table.toDrive({
  collection: AllSpecialYearResults,
  description: 'Median_withSpecialYear_surveyCentered_100mfixedRadius',
  folder: "GoogleEarthEngine",
  fileFormat: 'CSV'
  });


