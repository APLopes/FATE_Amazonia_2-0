//SEEG_10 emissoes por fogo em FLORESTA

//_____________________________________________________________________________ 
// FATE model for SEEG Fire emissions
// GHG emissions from forest fires not related to deforestation
// Integrated model for the final carbon balance from combustion and mortality/decomposition
// for forestts only
//
// The model estimates forest carbon balance for single and multiple fire events
//
//
// Created by: Camila Silva, Aline Pontes e Wallace Silva
// Last edited: 13 Apr 2023
//_______________________________________________________________________________
// --- --- --- VERSION
var version = 'v2-1-legacy';
// --- --- --- ASSETS
var time_since_fire = ee.Image('projects/mapbiomas-workspace/FOGO_COL2/PRODUTOS_REGIME_DO_FOGO/mapbiomas-fire-collection2-time-after-fire-v1'),
    frequence_fire = ee.Image('projects/mapbiomas-workspace/FOGO_COL2/SUBPRODUTOS/mapbiomas-fire-collection2-fire-frequency-v1').slice(0,38).divide(100).int(),
    annual_fire = ee.Image('projects/mapbiomas-workspace/FOGO_COL2/SUBPRODUTOS/mapbiomas-fire-collection2-annual-burned-coverage-v1').selfMask();
    

// --- SUPPORT DATA LEGACY PROCESS
var postYearsScars = [
  2023,2024,2025,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035,2036,2037,2038
  ];

postYearsScars.forEach(function(year){
  annual_fire = annual_fire.addBands(
    ee.Image().rename('burned_coverage_' + year).uint8()
    );
  
  frequence_fire = frequence_fire.addBands(
    frequence_fire.select(37).divide(100).int().rename('fire_frequency_1985_' + year)
    );
  
  time_since_fire = time_since_fire.addBands(
    time_since_fire.select('classification_'+year).add(1).rename('classification_'+(year+1)).uint8()
    );
  
});
////


var annual_fire_freq_gte2 = annual_fire.updateMask(frequence_fire.gte(2)).gte(1),
    
    mask_stable = ee.Image('projects/ee-seeg-brazil/assets/collection_10/v1/2_1_Mask_stable/SEEG_c10_v1_2020').eq(3).selfMask(),
    
    qcn = ee.ImageCollection('projects/mapbiomas-workspace/SEEG/2022/QCN/QCN_30m_BR_v2_0_1')
      .mosaic(),

    fwd = qcn.select('clitter'),
    cwd = qcn.select('cdw'),
    s_inAGBstock = qcn.select('cagb'), // estamos aguardando o CAGB para os outros biomas além da Amazônia
    s_inAGNstock = fwd.add(cwd),
         
    biomas = ee.FeatureCollection('projects/mapbiomas-workspace/AUXILIAR/biomas_IBGE_250mil');
    
// Map.addLayer(time_since_fire,{},'time_since_fire');
// Map.addLayer(frequence_fire,{},'frequence_fire');
Map.addLayer(annual_fire,{},'annual_fire');
// Map.addLayer(annual_fire_freq_gte2,{},'annual_fire_freq_gte2');
// Map.addLayer(qcn,{},'qcn')
// Map.addLayer(fwd,{},'fwd')
// Map.addLayer(cwd,{},'cwd')
// Map.addLayer(s_inAGBstock,{},'s_inAGBstock')
// Map.addLayer(s_inAGNstock,{},'s_inAGNstock')

// --- --- --- OPTIONS
// This will change to 1986, 2036 when we add the new 16 bands
// var years = ee.List.sequence(1986, 2020, 1).getInfo(); // according to the Mapbiomas fire
var years = [
  1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,
  2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,
  2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030,
  2031,2032,2033,2034,2035,2036,2037,2038
]; // according to the Mapbiomas fire

// fire.years = c(2000,2006) // when the fire events happened -> os anos do fogo podem ser extraidos da camada anual

// --- --- --- LOOKUP TABLES -> auxiliar
var biome_dict = {
  'amazonia':{
    // max:9, // valores limites para frequencia, implementar no futuro um limite adequado para cada bioma // o valor 9 para amazonia é segundo script: https://github.com/cammis/SEEG_firedyn/blob/main/SEEG_Fire-dyn_freq_99perc_20210913.R
  
    AGBloss_lookup: [
      // new vector -> rate relative to the previous year AGB
      // lookup table updated on 2022/03/22
      0,            0.104,        0.080,        0.060,        0.044,        0.032,        0.023,        0.016,        0.011,
      0.008,        0.006,        0.004,        0.003,        0.002,        0.001,        0.001,        0.001,        0,            
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,
    ],
    
    turnover:0.03,
    decomp_rate: 0.19,          //ok,  from Chambers et al. 1999
    fwd_combf: 0.683,           //ok
    cwd_combf_single: 0.643,    //ok
    cwd_combf_multi: 0.824,     //ok
    cwd_part_multi: 0.757 ,     //ok -> !! Verificar se não existe fator para fwd em multiburn
  
    string_list:['Amazônia'] 
    
  },
  'caatinga':{
    // max:9, // segundo script: https://github.com/cammis/SEEG_firedyn/blob/main/SEEG_Fire-dyn_freq_99perc_20210913.R
    
    AGBloss_lookup: [
      // new vector -> rate relative to the previous year AGB
      // lookup table updated on 2022/03/22
      0,            0.104,        0.080,        0.060,        0.044,        0.032,        0.023,        0.016,        0.011,
      0.008,        0.006,        0.004,        0.003,        0.002,        0.001,        0.001,        0.001,        0,            
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,
    ],
    
    turnover:0.03,
    decomp_rate: 0.19,          //ok,  from Chambers et al. 1999
    fwd_combf: 0.983,           //ok
    cwd_combf_single: 0.719,    //ok
    cwd_combf_multi: 0.824,     //ok
    cwd_part_multi: 0.757,     //ok -> !! Verificar se não existe fator para fwd em multiburn
  
    string_list:['Caatinga']
  },
  'cerrado':{
    // max:9, // segundo script: https://github.com/cammis/SEEG_firedyn/blob/main/SEEG_Fire-dyn_freq_99perc_20210913.R
    
    AGBloss_lookup: [
      // new vector -> rate relative to the previous year AGB
      // lookup table updated on 2022/03/22
      0,            0.104,        0.080,        0.060,        0.044,        0.032,        0.023,        0.016,        0.011,
      0.008,        0.006,        0.004,        0.003,        0.002,        0.001,        0.001,        0.001,        0,            
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,
    ],
    
    turnover:0.03,
    decomp_rate: 0.19,          //ok,  from Chambers et al. 1999
    fwd_combf: 0.683,           //ok
    cwd_combf_single: 0.629,    //ok
    cwd_combf_multi: 0.824,     //ok
    cwd_part_multi: 0.757 ,     //ok -> !! Verificar se não existe fator para fwd em multiburn
  
    string_list:['Cerrado'],
  },
  'mata_atlantica':{
    // max:9, // segundo script: https://github.com/cammis/SEEG_firedyn/blob/main/SEEG_Fire-dyn_freq_99perc_20210913.R

    AGBloss_lookup: [
      // new vector -> rate relative to the previous year AGB
      // lookup table updated on 2022/03/22
      0,            0.104,        0.080,        0.060,        0.044,        0.032,        0.023,        0.016,        0.011,
      0.008,        0.006,        0.004,        0.003,        0.002,        0.001,        0.001,        0.001,        0,            
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,
    ],
    
    turnover:0.03,
    decomp_rate: 0.19,          //ok,  from Chambers et al. 1999
    fwd_combf: 0.683,           //ok
    cwd_combf_single: 0.643,    //ok
    cwd_combf_multi: 0.824,     //ok
    cwd_part_multi: 0.757 ,     //ok -> !! Verificar se não existe fator para fwd em multiburn
  
    string_list:['Mata Atlântica'] 
    
  },
  'pampa':{
    // max:9, // segundo script: https://github.com/cammis/SEEG_firedyn/blob/main/SEEG_Fire-dyn_freq_99perc_20210913.R
    
    AGBloss_lookup: [
      // new vector -> rate relative to the previous year AGB
      // lookup table updated on 2022/03/22
      0,            0.104,        0.080,        0.060,        0.044,        0.032,        0.023,        0.016,        0.011,
      0.008,        0.006,        0.004,        0.003,        0.002,        0.001,        0.001,        0.001,        0,            
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,
    ],
    
    turnover:0.03,
    decomp_rate: 0.19,          //ok,  from Chambers et al. 1999
    fwd_combf: 0.683,           //ok
    cwd_combf_single: 0.629,    //ok
    cwd_combf_multi: 0.824,     //ok
    cwd_part_multi: 0.757 ,     //ok -> !! Verificar se não existe fator para fwd em multiburn
  
    string_list:['Pampa'],
  },
  'pantanal':{
    // max:9, // segundo script: https://github.com/cammis/SEEG_firedyn/blob/main/SEEG_Fire-dyn_freq_99perc_20210913.R
    
    AGBloss_lookup: [
      // new vector -> rate relative to the previous year AGB
      // lookup table updated on 2022/03/22
      0,            0.104,        0.080,        0.060,        0.044,        0.032,        0.023,        0.016,        0.011,
      0.008,        0.006,        0.004,        0.003,        0.002,        0.001,        0.001,        0.001,        0,            
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,            0,            0,            0,            0,            0,            0,            0,            0,
      0,
    ],
    
    turnover:0.03,
    decomp_rate: 0.19,          //ok,  from Chambers et al. 1999
    fwd_combf: 0.683,           //ok
    cwd_combf_single: 0.629,    //ok
    cwd_combf_multi: 0.824,     //ok
    cwd_part_multi: 0.757 ,     //ok -> !! Verificar se não existe fator para fwd em multiburn
  
    string_list:['Pantanal'],
  },
};

var list_biomes = [
  'amazonia',
  // 'caatinga',
  // 'cerrado',
  // 'mata_atlantica',
  // 'pampa',
  // 'pantanal',
];


list_biomes.forEach(function(biome){

  // --- geometry for export
  var featureCollection = biomas
    .filter(ee.Filter.inList('Bioma',biome_dict[biome]['string_list']));
  var maskRegion = ee.Image(1).paint(featureCollection).neq(1);
  var region = featureCollection.geometry().bounds();

  var tsfValues = [
      0,            1,            2,            3,            4,            5,            6,            7,          8,
      9,            10,           11,           12,           13,           14,           15,           16,         17,
      18,           19,           20,           21,           22,           23,           24,           25,         26,
      27,           28,           29,           30,           31,           32,           33,           34,         35,
      36,           37,           38,           39,           40,           41,           42,           43,         44,
      45,           46,           47,           48,           49,           50,           51,           52,         53,
      54,
    ];
    
  var AGBloss_lookup = biome_dict[biome]['AGBloss_lookup'];
  
  var turnover = biome_dict[biome]['turnover'];
  
  var aREF_decomposition = biome_dict[biome]['decomp_rate'];
  var aREF_combF_fwd_sF = biome_dict[biome]['fwd_combf'];
  
  var aREF_combF_cwd_sF = biome_dict[biome]['cwd_combf_single'];
  var cwd_combf_multi = biome_dict[biome]['cwd_combf_multi'];
  
  var cwd_part_single = biome_dict[biome]['cwd_part_single'];
  var cwd_part_multi = biome_dict[biome]['cwd_part_multi'];
  
  var bioma = biomas.filter(ee.Filter.inList('Bioma',biome_dict[biome]['string_list']));
  
  //__________________________________________________________________________________________________________
  // --- --- --- --- THE MODEL STARTS HERE
  
  // --- --- sets 0 for first fire 
  var tsf_edit = annual_fire
    .multiply(0)
    .slice(1)
    .blend(time_since_fire.slice(0,-1));
  
  
  //  --- iterate function obs
  
  // - Iterate an algorithm over a list. The algorithm is expected to take
  // two objects, the current list item, and the result from the previous.
  
  // - The function to apply to each element. Must take two arguments: an 
  // element of the collection and the value from the previous iteration.
  
  function model(year, previous_year){
      // --- year prev
      year = ee.Number.parse(year);
      var year_prev  = year.subtract(1);
      
      var tsf_year = tsf_edit.select(ee.String('burned_coverage_').cat(year));
      
      var annual_fire_freq_gte2_year = annual_fire_freq_gte2.select(ee.String('burned_coverage_').cat(year));
      
      // --- --- Creates an image with AGB loss rate as function of TSF
      var aRef_netAGBloss = tsf_year.remap(tsfValues,AGBloss_lookup).multiply(-1)
        .rename(ee.String('aRef_netAGBloss_').cat(year));
        
      var aRef_netAGNgain = tsf_year.remap(tsfValues,AGBloss_lookup)
        .rename(ee.String('aRef_netAGNgain_').cat(year));
      
      
      // --- --- Left AGB after loss by mortality
      var fLeftAGB_prev = ee.Image(previous_year)
        .select(ee.String('fLeftAGB_').cat(year_prev));
      
      var s_inAGBstock_update = s_inAGBstock
        .updateMask(tsf_year.eq(0))
        .blend(fLeftAGB_prev)
        .rename(ee.String('s_inAGBstock_update_').cat(year));
          
      var fNetAGBloss = s_inAGBstock_update.multiply(aRef_netAGBloss)
        .rename(ee.String('fNetAGBloss_').cat(year));
  
      var fLeftAGB = s_inAGBstock_update.add(fNetAGBloss) //->fNetAGBloss have negative values
        .rename(ee.String('fLeftAGB_').cat(year));
  
      // --- --- Necromass inflows
      var fAGNinput_Mortality = s_inAGBstock_update.multiply(aRef_netAGNgain);
      var fAGNinput_TurnOver = s_inAGBstock_update.multiply(turnover);
      var fAGNinput = fAGNinput_Mortality.add(fAGNinput_TurnOver)
        .rename(ee.String('fAGNinput_').cat(year));
      
      // --- --- Necromass outflows
      
      // --- Immediate combustion
      // - combustion when single fire
      
      var fAGNcomb_fwd = fwd.multiply(aREF_combF_fwd_sF)
        .updateMask(tsf_year.eq(0));
        
      fAGNcomb_fwd = ee.Image(0).blend(fAGNcomb_fwd);
        
      var fAGNcomb_cwd = cwd.multiply(aREF_combF_cwd_sF) 
        .updateMask(tsf_year.eq(0));
        
      fAGNcomb_cwd = ee.Image(0).blend(fAGNcomb_cwd);
      
      var fAGNcomb = fAGNcomb_fwd.add(fAGNcomb_cwd)
        .rename(ee.String('fAGNcomb_').cat(year));
        
      // create updtaed AGN stock 
      
      var fLeftAGN_prev = ee.Image(previous_year)
        .select(ee.String('fLeftAGN_').cat(year_prev));
      
      var s_inAGNstock_update = s_inAGNstock
        .updateMask(tsf_year.eq(0))
        .blend(fLeftAGN_prev)
        .rename(ee.String('s_inAGNstock_update_').cat(year));
      
      // - combustion when multi fire
      var fAGNcomb_fwd_M = s_inAGNstock_update.multiply(ee.Image(1).subtract(cwd_part_multi)).multiply(aREF_combF_fwd_sF)
        .updateMask(annual_fire_freq_gte2_year);
        
      var fAGNcomb_cwd_M = s_inAGNstock_update.multiply(cwd_part_multi).multiply(cwd_combf_multi) 
        .updateMask(annual_fire_freq_gte2_year);
                           
      fAGNcomb_cwd_M = ee.Image(0).blend(fAGNcomb_cwd_M);
      
      var fAGNcomb_M = fAGNcomb_fwd_M.add(fAGNcomb_cwd_M);
      
      fAGNcomb = fAGNcomb.blend(fAGNcomb_M)
        .rename(ee.String('fAGNcomb_').cat(year));
        
      
      // Left AGN after loss by combustion and decomposition
      var fLeftAGN = (s_inAGNstock_update.add(fAGNinput).subtract(fAGNcomb)).multiply(1 - aREF_decomposition)
        .rename(ee.String('fLeftAGN_').cat(year));
  
      // Decomposition
      var fAGNdecomp = (s_inAGNstock_update.add(fAGNinput).subtract(fAGNcomb)).multiply(aREF_decomposition)
        .rename(ee.String('fAGNdecomp_').cat(year));
  
      
      // Final Carbon balance
      var fCBalance = (fLeftAGB.subtract(s_inAGBstock_update)).add(fLeftAGN.subtract(s_inAGNstock_update))
        .rename(ee.String('fCBalance_').cat(year));
    
      
      return ee.Image(previous_year)
        .addBands(aRef_netAGBloss)
        .addBands(aRef_netAGNgain)
        .addBands(fNetAGBloss)
        .addBands(fLeftAGB)
        .addBands(fAGNinput)
        .addBands(fAGNcomb)
        .addBands(fAGNdecomp)
        .addBands(fLeftAGN)
        .addBands(fCBalance)
        .addBands(s_inAGBstock_update)
        .addBands(s_inAGNstock_update);
      
  }

  var first = ee.Image().rename('fLeftAGB_1985')
    .addBands(ee.Image().rename('fLeftAGN_1985'));
  /*.rename('fLeftAGB_1985');*/
  
  var data = ee.List(years).iterate(model,first);
  
  data = ee.Image(data)
    .updateMask(mask_stable) // mascarando para area de floresta estavel
    .updateMask(maskRegion); // mascarando para a região do bioma amazonico
  
  // --- --- --- PLOT AND VIS
  var aRef_netAGBloss = data.select('aRef_netAGBloss_(.*)');
  // Map.addLayer(aRef_netAGBloss,{},'aRef_netAGBloss',false);
  
  var aRef_netAGNgain = data.select('aRef_netAGNgain_(.*)');
  // Map.addLayer(aRef_netAGNgain,{},'aRef_netAGNgain',false);
  
  var fNetAGBloss = data.select('fNetAGBloss_(.*)');
  // Map.addLayer(fNetAGBloss,{},'fNetAGBloss',false);
  
  var s_inAGBstock_update = data.select('s_inAGBstock_update_(.*)');
  // Map.addLayer(s_inAGBstock_update,{},'s_inAGBstock_update',false);
  
  var s_inAGNstock_update = data.select('s_inAGNstock_update_(.*)');
  // Map.addLayer(s_inAGNstock_update,{},'s_inAGNstock_update',false);
  
  var fLeftAGB = data.select('fLeftAGB_(.*)');
  // Map.addLayer(fLeftAGB,{},'fLeftAGB',false);
  
  var fAGNinput = data.select('fAGNinput_(.*)');
  // Map.addLayer(fAGNinput,{},'fAGNinput',false);
  
  var fAGNcomb = data.select('fAGNcomb_(.*)');
  // Map.addLayer(fAGNcomb,{},'fAGNcomb',false);
  
  var fAGNdecomp = data.select('fAGNdecomp_(.*)');
  // Map.addLayer(fAGNdecomp,{},'fAGNdecomp',false);
  
  var fLeftAGN = data.select('fLeftAGN_(.*)');
  // Map.addLayer(fLeftAGN,{},'fLeftAGN',false);
  
  var fCBalance = data.select('fCBalance_(.*)');
  // Map.addLayer(fCBalance,{},'fCBalance',false);
  
  // Map.addLayer(s_inAGBstock,{},'s_inAGBstock',false);
  // Map.addLayer(s_inAGNstock,{},'s_inAGNstock',false);
  
  
  // --- --- ponto de referencia para inspecionar o resultado
  var point_inspector = ee.Geometry.Point([-48.76627559256188,-9.04548244826409]);
  Map.centerObject(point_inspector);
  Map.addLayer(point_inspector,{},'point_inspector');
  
  
  // --- --- --- --- EXPORT
  
  // --- --- --- NON_CO2
  //Partitioning of combusted matter - following IPCC GHG emissions factors
  //CO2=1580, CO=104, CH4=6.8 g/kg of dry matter burnt
  
  // OBS: A tabela tem o calculo da proporção do carbono contido nos gases não CO² a ser descontado  
  // OBS: As duas formas de cálcular (fatores de emissão do IPCC e proporção segundo o peso molecular) estão compatíveis, ver planilha no Drive.
  // tabela de proporção dos gases -> https://docs.google.com/spreadsheets/d/1MqdK3bu6YxNTEPVyYgIbyEkWPiYs83j5qsgZgwfqUHE/edit#gid=0
  
  //if C content is 0.5, multiply by 2 to convert in dry matter, and by 1000 to convert from Mg to Kg
  var CO2_g_ha = fAGNcomb.multiply(2000).multiply(1580); // result in g of CO per hectare
  
  var CO_g_ha = fAGNcomb.multiply(2000).multiply(104); // result in g of CO per hectare
  
  var CH4_g_ha = fAGNcomb.multiply(2000).multiply(6.8); // result in g of CH4 per hectare
  
  var N2O_g_ha = fAGNcomb.multiply(2000).multiply(0.2); // resullt in g N2O per hectare
  
  var NOX_g_ha = fAGNcomb.multiply(2000).multiply(1.6); // result in g NOX per hectare
  
  var lists_gases_non_co2 = [
    [CO2_g_ha,'CO2_g_ha_comb_read_warning','ATENÇÃO: Essa é a emissão bruta da combustão e ja foi incluida no balanço (emissão liquida por combustão, decomposição e crescimento). Não somar com a camada de CO2 proveniente do balanço, para não gerar dupla contagem.'], // ATENÇÃO: Não somar com a camada de CO2 proveniente do balanço, pois pode gerar dupla contagem. 
    [CO_g_ha,'CO_g_ha_comb'],
    [CH4_g_ha,'CH4_g_ha_comb'],
    [N2O_g_ha,'N2O_g_ha_comb'],
    [NOX_g_ha,'NOX_g_ha_comb'],
  ];    
  
  lists_gases_non_co2.forEach(function(list){
    var image = list[0];
    var name = list[1];
    var split = name.split('_');

    // CH4_g_ha_comb
    // g CH4 ha-1
    var newProps = {
      date_create:ee.Date(Date.now()).format('y-M-d'),
      source:'FATE_SEEG',
      theme:'Forest fire emissions',
      version: version,
      unit: 'g ' + split[0] + ' ha-1' 
    };
    
    if (list[2] !== undefined){
      newProps.warning = list[2];
    } else {
      delete newProps.warning;
    }
    
    image.bandNames().evaluate(function(oldBands){
      var newBands = oldBands.map(function(band){ return name + band.slice(-5)});
      
      image = image.select(oldBands,newBands)
        .round()
        .int32()
        .set(newProps);
  
      var description = biome+'-'+name + '-' + version;
      var address = 'projects/mapbiomas-workspace/SEEG/2022/FOGO/FLORESTA/';
      
      Map.addLayer(image,{},description,false);
      
      // - export to asset
      // Export.image.toAsset({
      //   image:image,
      //   description:description,
      //   assetId:address + description,
      //   // pyramidingPolicy, dimensions, 
      //   region:region,
      //   scale:30,
      //   // crs, crsTransform, 
      //   maxPixels:1e13,
      //   // shardSize
      // });
      
      // - convert table export to drive 
      var table =  image
        .multiply(ee.Image.pixelArea().divide(10000)) // convert pixel area in meters to hectare
        .reduceRegions({
          collection:featureCollection,
          reducer:ee.Reducer.sum(),
          scale:30, 
          // crs:,
          // crsTransform:,
          // tileScale:
        });
      
      var recipe = ee.FeatureCollection([]);
      // 1000e4 // combustão
      // 1000e2 // balanço
      newBands.forEach(function(newBand){
        var year = newBand.slice(-4);
        
        var newFeat = ee.Feature(null)
          .set({
            Biome:biome,
            GHG:name.split('_')[0],
            Year:year,
            Emissions_Tg:table.first().getNumber(newBand).divide(1e12) // 1e12 transforma grama para teragrama
          });
        
        recipe = recipe.merge(ee.FeatureCollection([newFeat]));
        
      });
      
      // print(table.limit(2),table);
      description = description.replace('g_ha','Tg');
      Export.table.toDrive({
        collection:recipe,
        description:description,
        folder:'fire_dyn_SEEG_IPAM',
        fileNamePrefix:description,
        fileFormat:'csv',
        // selectors:,
        // maxVertices:
      });
        
    });
  });
  
  
  // --- --- --- CO2
  // OBS: A tabela tem o calculo da proporção do carbono contido nos gases não CO² a ser descontado  
  // OBS: As duas formas de cálcular (fatores de emissão do IPCC e proporção segundo o peso molecular) estão compatíveis, ver planilha no Drive.
  // tabela de proporção dos gases -> https://docs.google.com/spreadsheets/d/1MqdK3bu6YxNTEPVyYgIbyEkWPiYs83j5qsgZgwfqUHE/edit#gid=0

  var CO2_Mg_ha = fAGNcomb.multiply(0.099).add(fCBalance) // descontar fração de carbono perdida por gases não CO2 (CO e CH4); OBS: FCBalance é negativo
    .multiply(3.67); // razão entre peso molecular do carbono e do CO2
  // print()
  CO2_Mg_ha.bandNames().evaluate(function(oldBands){
    var newBands = oldBands.map(function(band){ return 'CO2_Mg_ha' + band.slice(-5)});
    
    var name = 'CO2_Mg_ha_balance';
    var description = biome + '-'+ name + '-' + version;
    var address = 'projects/mapbiomas-workspace/SEEG/2022/FOGO/FLORESTA/';
    
    var newProps = {
      date_create:ee.Date(Date.now()).format('y-M-d'),
      source:'FATE_SEEG',
      theme:'Forest fire emissions',
      version: version,
      unit: 'Mg CO2 ha-1'
    };

    
    var image = CO2_Mg_ha.select(oldBands,newBands)
      .float()
      .multiply(-1) // convertendo para emissões
      .set(newProps)
      
      Map.addLayer(image,{},description,false);
      
      // - export to asset
      // Export.image.toAsset({
      //   image:image,
      //   description:description,
      //   assetId:address + description,
      //   // pyramidingPolicy, dimensions, 
      //   region:region,
      //   scale:30,
      //   // crs, crsTransform, 
      //   maxPixels:1e13,
      //   // shardSize
      // });
      
      // - convert table export to drive 
      // - convert table export to drive 
      var table =  image
        .multiply(ee.Image.pixelArea().divide(10000)) // convert pixel area in meters to hectare
        .reduceRegions({
          collection:featureCollection,
          reducer:ee.Reducer.sum(),
          scale:30, 
          // crs:,
          // crsTransform:,
          // tileScale:
        });
      
        
      var recipe = ee.FeatureCollection([]);

      newBands.forEach(function(newBand){
        var year = newBand.slice(-4);
        
        var newFeat = ee.Feature(null)
          .set({
            Biome:biome,
            GHG:name.split('_')[0],
            Year:year,
            Emissions_Tg:table.first().getNumber(newBand).divide(1e6) // 1e6 transforma megagrama para teragrama
          });
        
        recipe = recipe.merge(ee.FeatureCollection([newFeat]));
        
      });
      
      description = description.replace('Mg_ha','Tg');
      Export.table.toDrive({
        collection:recipe,
        description:description,
        folder:'fire_dyn_SEEG_IPAM',
        fileNamePrefix:description,
        fileFormat:'csv',
        // selectors:,
        // maxVertices:
      });
  
  });
  
  // --- --- --- METADATA TABLE
  var metadata = ee.FeatureCollection([
      ee.Feature(null)
        .set({
          METADATA:'FATE Model v2-1 - Integrated model for GHG emissions from forest fires in primary forests',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Emissions from fires not associated with deforestation',
        }),
      ee.Feature(null)
        .set({
          METADATA:'By INPE/IPAM',
        }),
      ee.Feature(null)
        .set({
          METADATA:'',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Headers',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Biome: ' + biome,
        }),
      ee.Feature(null)
        .set({
          METADATA:'GHG:	Greenhouse gas of interest',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Year:	Year of reference',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Emissions_Tg: GHG emissions (in Tg)',
        }),
      ee.Feature(null)
        .set({
          METADATA:'',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Observations',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Input spatial products: Mapbiomas Fire collection 2 (1986-2022), SEEG 10 forest mask stable, and Rectified QCN carbon stocks',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Biophysical processes:  Combustion, tree mortality, tree regeneration and decomposition',
        }),
      ee.Feature(null)
        .set({
          METADATA:'',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Other details',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Forest mask stable is the total standing forest at the end of the time series',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Rectified QCN carbon stocks are maps corrected by Mapbiomas vegetation classes',
        }),
      ee.Feature(null)
        .set({
          METADATA:'',
        }),
      ee.Feature(null)
        .set({
          METADATA:'Output products',
        }),
      ee.Feature(null)
        .set({
          METADATA:biome+'-CO2_Tg_comb_read_warning-v2-1: Gross CO2 emissions from combustion',
        }),
      ee.Feature(null)
        .set({
          METADATA:biome+'-CO_Tg_comb-v2-1: CO emissions from combustion',
        }),
      ee.Feature(null)
        .set({
          METADATA:biome+'-CH4_Tg_comb-v2-1: CH4 emissions from combustion',
        }),
      ee.Feature(null)
        .set({
          METADATA:biome+'-N2O_Tg_comb-v2-1: N2O emissions from combustion',
        }),
      ee.Feature(null)
        .set({
          METADATA:biome+'-NOX_Tg_comb-v2-1: NOX emissions from combustion',
        }),
      ee.Feature(null)
        .set({
          METADATA:biome+'-CO2_Tg_balance-v2-1: Net CO2 emissions from combustion, decomposition and regeneration',
        }),
      ee.Feature(null)
        .set({
          METADATA:'',
        }),
        
      ee.Feature(null)
        .set({
          METADATA:'Author(s):	Dra. Camila Silva, Dra. Aline Pontes Lopes, and Wallace Vieira da Silva',
        }),
      ee.Feature(null)
        .set({
          METADATA:ee.String('Date (y-m-d): ').cat(ee.Date(Date.now()).format('y-M-d')),
        }),
      ee.Feature(null)
        .set({
          METADATA:'For further clarifications, please contact: camila.silva@ipam.org.br; alineplopes@gmail.com',
        }),
    ]);
  
  var description = biome + '-Fate_model_output_metadata-' + version;
  
  Export.table.toDrive({
    collection:metadata,
    description:description,
        folder:'fire_dyn_SEEG_IPAM',
    fileNamePrefix:description,
    fileFormat:'csv',
    // selectors:,
    // maxVertices:
  });
  
});
