// ============================================================================
// FATE-SEEG — Validação de acesso a assets GEE
// Cole este script no Code Editor do Google Earth Engine e execute (Run).
// O output é TSV: copie da aba Console para conferência.
// ============================================================================

var assets = {
  // ========== GRUPO 1: MAPBIOMAS WORKSPACE ==========
  'projects/mapbiomas-workspace/SEEG/2023/c10/2_0_Mask_stable':              {type:'ImageCollection', scripts:'Paper_SM_tab2,Paper_fig1_map'},
  'projects/mapbiomas-workspace/SEEG/2023/c10/2_0_Mask_stable/SEEG_c10_v_0_29_2020': {type:'Image',         scripts:'Todos fate_model*.js'},
  'projects/mapbiomas-workspace/SEEG/2021/Col9/mask_stable':                 {type:'ImageCollection', scripts:'Paper_fig1_data'},
  'projects/mapbiomas-workspace/SEEG/2023/c10/1_1_Temporal_filter_deforestation': {type:'Image',         scripts:'Paper_fig1_map (comentado)'},
  'projects/mapbiomas-workspace/SEEG/2022/QCN/QCN_30m_BR_v2_0_1':           {type:'ImageCollection', scripts:'TODOS os 10 scripts'},
  'projects/mapbiomas-workspace/FOGO_COL2/PRODUTOS_REGIME_DO_FOGO/mapbiomas-fire-collection2-time-after-fire-v1': {type:'Image', scripts:'fate_model, fate_legacy, Paper_fig1_map'},
  'projects/mapbiomas-workspace/FOGO_COL2/SUBPRODUTOS/mapbiomas-fire-collection2-annual-burned-coverage-v1': {type:'Image', scripts:'fate_model, fate_legacy, fate_mb_col2, Paper_fig1_map'},
  'projects/mapbiomas-workspace/FOGO_COL2/SUBPRODUTOS/mapbiomas-fire-collection2-fire-frequency-coverage-v1': {type:'Image', scripts:'fate_model, fate_legacy_AP, Paper_fig1_map'},
  'projects/mapbiomas-workspace/FOGO_COL2/SUBPRODUTOS/mapbiomas-fire-collection2-fire-frequency-v1': {type:'Image', scripts:'fate_model_legacy'},
  'projects/mapbiomas-workspace/AUXILIAR/biomas_IBGE_250mil':                {type:'FeatureCollection', scripts:'8 scripts (fate_model + fig1_data)'},
  'projects/mapbiomas-workspace/AUXILIAR/estados-2017':                      {type:'FeatureCollection', scripts:'Paper_fig1_map'},
  'projects/mapbiomas-workspace/AUXILIAR/America_do_Sul':                    {type:'FeatureCollection', scripts:'Paper_fig1_map'},
  'projects/mapbiomas-workspace/AUXILIAR/areas-protegidas-por-ano-2022/ap2022': {type:'Image',         scripts:'fate_model_legacy_AP'},
  'projects/mapbiomas-workspace/public/collection6/mapbiomas-fire-collection1-fire-frequency-1': {type:'Image', scripts:'Paper_fig1_data (Col1)'},
  'projects/mapbiomas-workspace/public/collection6/mapbiomas-fire-collection1-annual-burned-coverage-1': {type:'Image', scripts:'Paper_fig1_data (Col1)'},

  // ========== GRUPO 2: INPE / SEEG-FireDyn ==========
  'projects/ee-seegfiredyn/assets/internal-version-2023-nasa-mcd64a1-time-after-fire-v1-2002to2023': {type:'Image', scripts:'fate_model_nasa*.js'},
  'projects/ee-seegfiredyn/assets/mapbiomas-fire-collection2-time-after-fire-v1-2002to2023': {type:'Image', scripts:'fate_model_mapbiomas-col2*.js'},
  'projects/ee-seegfiredyn/assets/mapbiomas-fire-collection1-year-since-fire-v1': {type:'Image', scripts:'Paper_fig1_data'},
  'projects/ee-seegfiredyn/assets/10grid':                                   {type:'FeatureCollection', scripts:'Paper_fig1_data'},

  // ========== GRUPO 3: INPE / SEEG-Brazil ==========
  'projects/ee-seeg-brazil/assets/collection_9/v1/1_1_Temporal_filter_deforestation': {type:'Image', scripts:'Paper_fig1_data'},
  'projects/ee-seeg-brazil/assets/collection_9/v1/1_1_Temporal_filter_regeneration': {type:'Image', scripts:'Paper_fig1_data + Paper_fig1_map'},

  // ========== GRUPO 4: USUÁRIO PESSOAL ==========
  'users/camilaflorestal/MAPBIOMAS/mb_biomescopy':                          {type:'FeatureCollection', scripts:'Paper_SM_tab2, Paper_fig1_map, Paper_fig1_data'},

  // ========== GRUPO 5: PÚBLICO ==========
  'MODIS/061/MCD64A1':                                                        {type:'ImageCollection', scripts:'fate_model_nasa*.js'},
};

print('===========================================================');
print('FATE-SEEG — Teste de acesso a assets GEE');
print('===========================================================');
print('');

var results = [];
var total = Object.keys(assets).length;
var tested = 0;

Object.keys(assets).forEach(function(path) {
  var info = assets[path];
  var tipo = info.type;
  var status = '...';
  var detail = '';
  
  try {
    var asset;
    if (tipo === 'Image') {
      asset = ee.Image(path);
    } else if (tipo === 'ImageCollection') {
      asset = ee.ImageCollection(path);
    } else if (tipo === 'FeatureCollection') {
      asset = ee.FeatureCollection(path);
    }
    
    // getInfo() valida acesso e retorna metadados
    var metadata = asset.getInfo();
    status = 'OK';
    
    if (tipo === 'Image') {
      var bands = metadata.bands || [];
      detail = bands.length + ' bandas, escala: ' + ((metadata.properties || {})['system:asset_size'] || '?');
    } else if (tipo === 'ImageCollection') {
      detail = (metadata.id || '') + ' (' + (metadata.properties ? 'ok' : '?') + ')';
    } else if (tipo === 'FeatureCollection') {
      var props = metadata.properties || {};
      var features = metadata.features ? metadata.features.length : '?';
      detail = 'features: ' + features;
    }
  } catch(e) {
    var msg = e.message || String(e);
    if (msg.indexOf('not found') >= 0 || msg.indexOf('does not exist') >= 0) {
      status = 'NOT_FOUND';
    } else if (msg.indexOf('denied') >= 0 || msg.indexOf('access') >= 0 || msg.indexOf('permission') >= 0) {
      status = 'ACCESS_DENIED';
    } else if (msg.indexOf('timeout') >= 0 || msg.indexOf('timed out') >= 0) {
      status = 'TIMEOUT';
    } else {
      status = 'ERRO';
    }
    detail = msg.substring(0, 120);
  }
  
  tested++;
  var line = [status, path, tipo, info.scripts, detail].join('\t');
  print(line);
  results.push(line);
});

print('');
print('Total testado: ' + tested + ' / ' + total + ' assets');
print('');
print('=== FIM ===');
print('Copie as linhas acima (formato TSV) para a tabela de resultados.');
