//V2 Script to check regeneration in the stable mask and calculate deforested forests which were previously burned
//This script generates results for figure 1 of our paper
//Authors: Camila Silva, Wallace Silva, Aline Pontes
//Last edit: 5th Jan 2023




//Load and create auxiliar data_____________________________________________________________________________

var desmat = ee.Image ('projects/ee-seeg-brazil/assets/collection_9/v1/1_1_Temporal_filter_deforestation')
// print (desmat, 'desmat')

var regen = ee.Image('projects/ee-seeg-brazil/assets/collection_9/v1/1_1_Temporal_filter_regeneration')
// print(regen, 'regen')

var ysf = ee.Image('projects/ee-seegfiredyn/assets/mapbiomas-fire-collection1-year-since-fire-v1')
// print(ysf, 'ysf')
// Map.addLayer(ysf, {bands: "classification_2019", palette: 'red'}, "ysf")

var frequence_fire = ee.Image('projects/mapbiomas-workspace/public/collection6/mapbiomas-fire-collection1-fire-frequency-1').divide(100).int();
// print(frequence_fire, 'frequence_fire');
//Map.addLayer(frequence_fire, {bands: 'fire_frequency_1985_2019', palette: ['aaff04','ccff00','fff704','ffbc00','ff5f02','ff1d06']}, 'frequence_fire')


// Load Amazon biome
var biome = ee.FeatureCollection('users/camilaflorestal/MAPBIOMAS/mb_biomescopy').filter(ee.Filter.eq('Bioma', 'Amazônia'));
  
var biomeMask = ee.Image().paint(biome);

var biomesLine = ee.Image().paint(ee.FeatureCollection('users/camilaflorestal/MAPBIOMAS/mb_biomescopy'),'vazio',1);

var statesLine = ee.Image().paint(ee.FeatureCollection('projects/mapbiomas-workspace/AUXILIAR/estados-2017'),'vazio',0.5);

var countriesLine = ee.Image().paint(ee.FeatureCollection('projects/mapbiomas-workspace/AUXILIAR/America_do_Sul'),'vazio',2);
var countriesmask = ee.Image(1).paint(ee.FeatureCollection('projects/mapbiomas-workspace/AUXILIAR/America_do_Sul'));

// Map.addLayer(countriesmask.randomVisualizer())

var biomeBounds = biome.geometry().bounds();
// Map.addLayer(biomeMask, {}, 'biomeMask')
// Map.addLayer(biomeBounds, {}, 'biomeBounds')


//Load SEEG Mask Stable
var mask_stable = ee.ImageCollection('projects/mapbiomas-workspace/SEEG/2021/Col9/mask_stable').toBands().eq(3).selfMask();
// print(mask_stable, 'mask_stable')
// Map.addLayer(mask_stable, {bands: 'SEEG_2021_c6_1989_classification_1989',min:0, max:1, palette: ['black']}, 'mask_stable')

// Non-forest Mask
var non_for = ee.ImageCollection('projects/mapbiomas-workspace/SEEG/2021/Col9/mask_stable').toBands().neq(3).selfMask().multiply(0);
// print(non_for, 'non_for')
// Map.addLayer(non_for, {bands: 'SEEG_2021_c6_1989_classification_1989',min:0, max:0, palette: ['red']}, 'non_for')



//Correction of forest mask by removing regeneration pixels ___________________________________________________

//Create a annual binary mask with forest and non-forest
var blend = mask_stable.blend(non_for)
// print(blend, 'blend')
// Map.addLayer(blend, {bands: 'SEEG_2021_c6_2020_classification_2020', min:0, max:1,palette:['red', 'black'] }, 'blend')


//Isolate initial and final year
var ms_init= blend.slice(0,-1) //initial_year, primeira a penultima banda
// print(ms_init, 'ms_init')
var ms_final = blend.slice(1) //final_year, segunda a ultima banda
// print(ms_final, 'ms_final')


//Subtraction between final and initial to map deforestation and regeneration
var ms_subtr = ms_final.subtract(ms_init)// desmatam=-1; regen=1
// print(ms_subtr, 'ms_subtr')
// Map.addLayer(ms_subtr, {min:-1, max:1}, 'ms_subtr')


// Create a regeneration mask by selecting pixels = 1
var reg = ms_subtr.eq(1).selfMask();
// print(reg, 'reg')
// Map.addLayer(reg, {},'reg')


//Calculate the area of regeneration pixels (inconsistency in the data) and export results in a table
var bioma = 'amazonia';
var biome = ee.FeatureCollection('projects/mapbiomas-workspace/AUXILIAR/biomas_IBGE_250mil')
.filter(ee.Filter.eq('Bioma','Amazônia'))

reg.bandNames().evaluate(function(bandnames){
var newbands = bandnames.map(function(bandname){
  var year = bandname.split('_')[3]
  return "regeneration_"+year
})
var reg_sum = reg.select(bandnames,newbands).aside(print) // Calculates sum of all pixels
.reduceRegion(ee.Reducer.sum(),biome.geometry(),30,null,null,false,1e13);
//Export.table.toDrive(ee.FeatureCollection(ee.Feature(null,reg_sum)),'MaskStable_v2'+ bioma, 'reg_sum'+ bioma);
})


//Delete regeneration pixels in mask stable

// Create a sing mask with all regeneration pixels in the time series 
var regener_total = reg.reduce('sum').gte(1).selfMask();

// Vizualize regeneration pixels with focal mode
print(regener_total, 'regener_total');
// Map.addLayer(regener_total
//     .focalMode({
//     radius:100,
//     kernelType:'square',
//     units:'meters', //meters or pixels
//     // iterations:,
//     // kernel:
//   }), {}, 'regener_total');


//create a binary global mask, regeneration =1
var inver_mask = ee.Image(0).blend(regener_total);
//Map.addLayer(inver_mask.randomVisualizer(), {}, 'inver_mask')

//apply inverse mask to select all primary forest pixels, which are =0 (or non-regeneration) 
var mask_stable_cor = mask_stable.updateMask(inver_mask.eq(0));
print(mask_stable_cor, "mask_stable_cor")
Map.addLayer(mask_stable_cor, {bands:'SEEG_2021_c6_2019_classification_2019', palette:'black'}, 'mask_stable_cor');




//create deforestation mask with corrected forest mask (mask stable)_________________________________________________________________ 
//cria imagem de 35 bandas com floresta/nao-floresta (1,0)
  mask_stable_cor.bandNames().evaluate(function(list){
  var recipe = ee.Image().select()
  list.forEach(function(bandname){
    var blend = ee.Image(0).blend(mask_stable_cor.select(bandname)).rename(bandname)
    recipe = recipe.addBands(blend)
  })
  
  var initial= recipe.slice(0,-1)
  var final = recipe.slice(1)
  var subtr = final.subtract(initial)
  
  var desf = subtr.eq(-1).selfMask() // mascara de desmatamento
  print(desf, 'desf')
  // print(ysf.slice(3, -2), 'ysf') 
  Map.addLayer(desf, {bands:['SEEG_2021_c6_2000_classification_2000'],palette:'008000'},'desf')
  
  
  //Apply correction to ysf dataset - convert fire age at the year of fire to zero
  var annual_fire = ee.Image('projects/mapbiomas-workspace/public/collection6/mapbiomas-fire-collection1-annual-burned-coverage-1')
                    .slice(1)
                    .selfMask()
                    .multiply(0)
  // print(annual_fire, 'annual_fire')
  Map.addLayer(annual_fire, {bands: 'burned_coverage_2000',palette: 'black'}, 'annual_fire')
  
  
  var ysf_cor = ysf.slice(0,-1).blend(annual_fire)
  print(ysf_cor, 'ysf_cor')
  // Map.addLayer(ysf_cor, {bands: 'classification_2020',palette: 'black'}, 'ysf_cor')
  
  
  //recorte temporal do ysf // reduz para 1989-2019
  var ysf_adap = ysf_cor.slice(3, -1)
  print(ysf_adap, 'ysf_adap')
  Map.addLayer(ysf_adap, {bands: 'classification_2000', palette:'black'}, 'ysf_adap')
  
  //Recorte de toda floresta queimada que foi desmatado no ano seguinte
  //A idade representa a idade no ano seguinte
  //Corrigimos a idade pq o desmatamento eh do ano seguinte (add(1))
  //Ver mascara de desmatamento - foi feito recorte de bandas para que tivesse 1 ano a frente do dado de ysf
  //recorte do fogo e de 1989 a 2019, e recorte do desmatamento e de 1990 a 2020
  //com uso desse recorte estamos excluindo fogo de desmatamento
  //desmatamento do ano seguinte ao fogo nao e considerado como fogo de desmatamento
  
  //burned forests deforested in the following year
  //ysf_desf tem a area de toda floresta queimada ate o ano anterior que foi desmatada no ano corrente
  //os pixels do ysf convertidos para 0 (fogo no ano corrente) sao contabilizados no desmatamento do ano seguinte
  //desmatamento ocorre uma unica vez na serie, qd aplicado no pixel de floresta queimada, essa floresta e excluida no resto da serie
  var ysf_desf = ysf_adap.updateMask(desf.eq(1)).updateMask(biomeMask.neq(1)).add(1)
  print(ysf_desf, 'ysf_desf')
  // Map.addLayer(ysf_desf, {bands: 'classification_2000', palette:'red'}, 'ysf_desf')

  
  //Mudando o nome das bandas do ysf_desf
  var oldBands = ysf_desf.bandNames();

  var newNames = oldBands.iterate(function(current, previous){
    current = ee.String(current);
    var year = ee.Number.parse(current.slice(-4)).add(1);
    return ee.List(previous).add(ee.String('deforestation_').cat(year))
  },[])
  // print(oldBands)
  newNames = ee.List(newNames)
  // print(newNames)

  //rename bands
  ysf_desf = ysf_desf.select(oldBands, newNames)
  print(ysf_desf, 'ysf_desf')
  Map.addLayer(ysf_desf, {bands: 'deforestation_2000', palette:'red'}, 'ysf_desf')



  // create area of all forests deforested in following year
  var desf_area = desf.multiply((ee.Image.pixelArea().divide(1e6)))
  .reduceRegions({
    reducer:ee.Reducer.sum(), 
    // collection:ee.FeatureCollection([ee.Feature(biomeBounds)]), 
    collection:ee.FeatureCollection([ee.Feature(geometry)]), 
    scale:30, 
    }) 

  // create area of burned forests deforested in the following year
  // considerar deletar gte(1), 
  var burn_desf_area = ysf_desf.gte(1).multiply((ee.Image.pixelArea().divide(1e6)))
  .reduceRegions({
    reducer:ee.Reducer.sum(), 
    collection:ee.FeatureCollection([ee.Feature(biomeBounds)]), 
    scale:30, 
  }) 
    
  print(burn_desf_area, 'ysf_desf_area')
  // Map.addLayer(burn_desf_area, {bands: 'classification_2019', palette: ['black']}, "burn_desf_area")  
  
  
  function exporting_relational_table(image,index){
    image.bandNames().evaluate(function(bandnames){
        
      bandnames
      .forEach(function(bandname){  
        var table = ee.FeatureCollection(
          ee.List(
            ee.Image.pixelArea().divide(1e6)
            .addBands(image.select(bandname))
            .reduceRegion({
              reducer:ee.Reducer.sum().group(1,bandname),
              geometry:biomeBounds,
              scale:30,
              // geometry:geometry,
              // scale:500,
              maxPixels:1e13,
            }).get('groups')
          )
          .map(function(obj){
            
            obj = ee.Dictionary(obj);
        
            return ee.Feature(null)
              .set('area_km2',obj.get('sum'))
              .set('year',ee.Number.parse(bandname.slice(-4)))
              .set(ee.String(index),obj.get(bandname));
          })
        );
      
      // print(index,bandname.slice(-4),table.limit(10));
      
        // Export.table.toDrive({
        //   collection:  table,
        //   description : 'SEEG-' +index + '_' +  bandname.slice(-4),
        //   folder : 'burned_deforested',
        //   fileNamePrefix: index + '_' +  bandname.slice(-4),
        //   fileFormat:'CSV' ,
        // });
        
      });
    });    
  }
  
  exporting_relational_table(ysf_desf,'ysf_desf');
  
  //create area of all burned forests
  // var ysf_for_area = ysf_adap.updateMask(mask_stable_cor.slice(0,-1)) //atualiza mascara de floresta para 1989-2019
  var burn_std_for_area = ysf_adap.updateMask(mask_stable_cor.select('SEEG_2021_c6_2020_classification_2020')) // ysf_adap e observando somente o que foi estavel a serie inteira
  .gte(1)
  .multiply((ee.Image.pixelArea().divide(1e6)))
  .reduceRegions({
   reducer:ee.Reducer.sum(), 
   collection:ee.FeatureCollection([ee.Feature(biomeBounds)]), 
   scale:30,  
  })
  
  print(burn_std_for_area, 'burn_std_for_area')
  // Map.addLayer(ysf_adap.updateMask(mask_stable_cor.select('SEEG_2021_c6_2020_classification_2020')), {bands: 'classification_2019', palette: ['red']}, "ysf_for_area")
  exporting_relational_table(ysf_adap.updateMask(mask_stable_cor.select('SEEG_2021_c6_2020_classification_2020')),'ysf_std_for_area');
  
  
  // frequencia do fogo em florestas estaveis em 2020
  var freq_std = frequence_fire.slice(5,36).updateMask(mask_stable_cor.select('SEEG_2021_c6_2020_classification_2020')) 
  exporting_relational_table(freq_std,'freq_std');
  
  
  //create area burned forest deforested with frequency information
  var freq_desf = frequence_fire.slice(4,35).updateMask(ysf_desf) 
  // print(freq_desf_area, "freq_desf_area")
  // Map.addLayer(freq_desf_area, {bands: 'fire_frequency_1985_2019', palette:['red']}, "freq_desf_area")
  
  
  //Mudando o nome das bandas para corresponder com o ano do desmatamento
  var oldBandsF = freq_desf.bandNames();

  var newNamesF = oldBandsF.iterate(function(current, previous){
    current = ee.String(current);
    var year = ee.Number.parse(current.slice(-4)).add(1);
    return ee.List(previous).add(ee.String('fire_freq_deforestation_').cat(year))
  },[])
  // print(oldBandsF)
  newNamesF = ee.List(newNamesF)
  // print(newNamesF)

  freq_desf = freq_desf.select(oldBandsF, newNamesF)

  exporting_relational_table(freq_desf,'freq_desf');

  // print(freq_desf_area, 'freq_desf_area')
    
  //Exporting results as table - total annual area 
  
  // Export.table.toDrive({
  //     collection:  burn_desf_area,
  //     description : 'SEEG-burn_desf_area', //all forests deforested in following year
  //     folder : 'burned_deforested',
  //     fileNamePrefix: 'burn_desf_area' ,
  //     fileFormat:'CSV' ,
  //     // selectors, 
  //     // maxVertices
  //     })
    
    
  // Export.table.toDrive({
  //   collection:  burn_std_for_area, 
  //   description : 'SEEG-burn_std_for_area', //Burned forests deforested in the following year
  //   folder : 'burned_deforested',
  //   fileNamePrefix: 'burn_std_for_area' ,
  //   fileFormat:'CSV' ,
  //   // selectors, 
  //   // maxVertices
  // });
    
  //generate grid for visualization 
  var all_ysf_desf = ysf_desf
    .reduce('sum');
    
  var all_ysf_std_for_area = ysf_adap.updateMask(mask_stable_cor.select('SEEG_2021_c6_2020_classification_2020'))
    .reduce('sum');
  
  // elementos cartograficos
  var style = require('users/gena/packages:style')
  
  var textProperties = { fontSize:16, textColor: '000000', outlineColor: 'ffffff', outlineWidth: 2, outlineOpacity: 0.6 }
  var scale = style.ScaleBar.draw(geometryScaleBar, {
    steps:2, palette: ['101010', 'f5f5f5'], multiplier: 1000, format: '%.0f', units: 'km', text: textProperties
  })
  
  Map.addLayer(scale, {}, 'scale bar')

  
  var mapa = ee.Image()
    .blend(countriesmask.visualize({palette:['cccccc','#8AB4F8'],min:0,max:1}))
    .blend(biomeMask.visualize({palette:'ffffff'}))
    .blend(all_ysf_desf.visualize({palette:'ff0000'}).updateMask(biomeMask.eq(0)))
    .blend(all_ysf_std_for_area.visualize({palette:'0000ff'}).updateMask(biomeMask.eq(0)))
    .blend(biomesLine)
    .blend(statesLine)
    .blend(countriesLine)
    .blend(scale);




  Map.addLayer(mapa,{},'mapa');
  
    var thumb = ui.Thumbnail({
    image:mapa,
    params:{
      dimensions:2500,
      region:biomeBounds
    }, 
    // onClick, 
    // style
  });
  
  print(thumb);
    
    
  var export_image = all_ysf_desf.gte(1).multiply(1)
    .blend(all_ysf_std_for_area.gte(1).multiply(2));

  Map.addLayer(export_image,{palette:['ff0000','0000ff'],min:1,max:2},'mapa tiff');

  
  Export.image.toDrive({
    image:export_image,
    description:'mapa-ysf-flo-desf-and-std',
    folder:'burned_deforested',
    fileNamePrefix:'mapa-ysf-flo-desf-and-std',
    // dimensions:,
    region:biomeBounds,
    scale:30,
    // crs:,
    // crsTransform:,
    maxPixels:1e13,
    // shardSize:,
    // fileDimensions:,
    // skipEmptyTiles:,
    fileFormat:'TIFF',
    // formatOptions:
  })
});
 





